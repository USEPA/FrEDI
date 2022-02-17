###### Documentation ######
#' Calculates climate change impacts on socially vulnerable populations throughout the 21st century for available sectors
#'
#' @description
#' This function allows users to project annual average climate change impacts throughout the 21st century (2010-2090) for available sectors (see [FrEDI::get_sectorInfo()]). Users may specify an optional list of custom scenarios. The output is an R data frame object containing annual average impacts, by year, for each sector, adaptation, impact type, model (GCM or SLR scenario), and region.
#'
#' @param inputsList A list of named elements named elements (`names(inputsList)= c("tempInput", "slrInput", "gdpInput", "popInput")`), each containing dataframes of custom temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and/or population scenarios, respectively, over the period 2010 to 2090. Note: temperature and sea level rise inputs should start in 2000 or earlier.
#' @param sectorList A character vector indicating a selection of sectors for which to calculate results (see [FrEDI::get_sectorInfo()]). If `NULL`, all sectors are included.
#' @param aggLevels Levels of aggregation at which to summarize data: one or more of `c("national", "modelaverage", "impactyear", "impacttype", "all")`. Defaults to all levels (i.e., `aggLevels="all"`). Uses the same aggregation levels as [FrEDI::aggregate_impacts()].
#' @param pv A `TRUE/FALSE` value indicating Whether to calculate present values for the annual impacts. Defaults to `pv=TRUE`. Present values (i.e., discounted impacts) are calculated as `discounted_impacts=annual_impacts/(1+rate)^(year-baseYear)`. Set an annual discounting rate and a base year using `baseYear` and `rate`.
#' @param baseYear Base year used for calculating present values of annual impacts (i.e., discounting). Defaults to `baseYear=2010`.
#' @param rate Annual discount rate used in calculating present values of annual impacts (i.e., discounting). Defaults to `rate=0.03` (i.e., 3% per year).
# @param primaryTypes = F whether to filter to primary impacts
#' @param elasticity=NULL A numeric value indicating an elasticity to use for adjusting economic values.
#### ADD MORE INFO ABOUT ELASTICITY
#' @param silent A `TRUE/FALSE` value indicating the level of messaging desired by the user (default=`TRUE`).
#'
#' @details This function allows users to project annual average climate change impacts throughout the 21st century (2010-2090) for available sectors. [FrEDI::run_fredi()] is the main function in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information).
#'
#' Users can run [FrEDI::run_fredi()] for all the sectors (default) or run FrEDI for specific sectors specified as a character vector to the `sectorsList` argument (run [FrEDI::get_sectorInfo()] to see a list of available sectors).
#'
#' #' Users can run FrEDI with the default scenario or have the option to specify custom inputs as a list of scenarios. The output of [FrEDI::run_fredi()] is an R data frame object containing annual average impacts, by year, for each sector, adaptation, impact type, model (GCM or SLR scenario), and region.
#'
#' Users can specify an optional list of custom scenarios with `inputsList` (for more information on the format of inputs, see [FrEDI::import_inputs()]). [FrEDI::run_fredi()] uses default scenarios for temperature, population, and GDP when no inputs are specified (i.e., `inputsList=NULL`) or for empty elements of the inputs list. If the user does not specify an input scenario for GMSL (i.e., `inputsList=list(slrInput=NULL)`, [FrEDI::run_fredi()] first converts the CONUS temperature scenario to global temperatures and then converts the global temperatures to a global mean sea level rise (GMSL) height in centimeters. For more information on the conversion of CONUS temperatures to global temperatures, see [FrEDI::convertTemps()]. For more information on the conversion of global temperatures to GMSL, see [FrEDI::temps2slr()].
#'
#' Values for input scenarios must be within reasonable ranges. If a user inputs a custom scenario with values outside the allowable ranges, [FrEDI::run_fredi()] will not run the scenarios and will instead stop and return an error message. For more information, see [FrEDI::import_inputs()]. Temperature and GMSL inputs must begin in 2000 or earlier. Values for population and GDP scenarios can start in 2010 or earlier.
#'
#' * The input temperature scenario (passed to [FrEDI::run_fredi()] via the `inputsList` argument) requires temperatures for the contiguous U.S. (CONUS) in degrees Celsius relative to 1995 (degrees of warming relative to the baseline). Temperature values must be greater than or equal to zero and less than or equal to 10 degrees Celsius. Users can convert global temperatures to CONUS temperatures using [FrEDI::convertTemps(from="global")] or by specifying [FrEDI::import_inputs(temptype="global")] when importing a temperature scenario from a CSV file.
#' * Values for the sea level rise (SLR) scenario are for global mean sea level rise (GMSL) must be in centimeters (cm) and values must be greater than or equal to zero and less than or equal to 250 cm.
#' * Population and gross domestic product (GDP) values must be greater than or equal to zero.
#'
#' If `inputsList=NULL`, [FrEDI::run_fredi()] uses defaults for all scenarios. Otherwise, [FrEDI::run_fredi()] looks for a list object passed to the argument `inputsList`. Within that list, [FrEDI::run_fredi()] looks for list elements `tempInput`, `slrInput`, `gdpInput`, and `popInput` containing dataframes with custom scenarios for temperature, GMSL, GDP, and regional population, respectively. [FrEDI::run_fredi()] will default back to the default scenarios for any list elements that are `NULL` or missing. In other words, running `run_fredi(inputsList=list())` returns the same outputs as running [FrEDI::run_fredi()]. For help importing custom scenarios from CSV files, refer to the pre-processing function [FrEDI::import_inputs()].
#'
#' [FrEDI::run_fredi()] linearly interpolates missing annual values for all input scenarios using non-missing values (requires at least two non-missing values). Temperatures are interpolated using 1995 as the baseline year (i.e., the central year of the 1986-2005 baseline). In other words, the temperature (in degrees Celsius) is set to zero for the year 1995 and GMSL is set to zero for the year 2000. The interpolated temperature and GMSL scenarios are combined into a column called `driverValue`, along with additional columns for year, the driver unit (column `"driverUnit"`, with `driverUnit="degrees Celsius"` and `driverUnit="cm"` for temperature- and SLR-driven sectors, respectively), and the associated model type (column `"model_type"`, with `model_type="GCM"` and `model_type="SLR"` for temperature- and SLR-driven sectors, respectively).
#'
#' The population scenario must provide annual regional values for population, with national totals calculated from regional values. FrEDI uses the national population scenario and the GDP scenario to calculate GDP per capita. Values for regional population, national population, national GDP (in 2015$), and national per capita GDP (in 2015$/capita) are provided in the results dataframe in columns `"reg_pop"`, `"national_pop"`, `"gdp_usd"`, and `"gdp_percap"`, respectively.
#'
#' Annual impacts for each sector, adaptation, impact type, and impact year combination included in the model are calculated by multiplying scaled climate impacts by a physical scalar and economic scalars and multipliers.
#'
#' [FrEDI::run_fredi()] aggregates or summarizes results to levels of aggregation specified by the user (passed to `aggLevels`) using the post-processing helper function [FrEDI::aggregate_impacts()]. Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels="national"`), averaging across model types and models (`aggLevels="modelaverage"`), summing over all impact types (`aggLevels="impacttype"`), and interpolate between impact year estimates (`aggLevels="impactYear"`). Users can specify all aggregation levels at once by specifying `aggLevels="all"` (default) or no aggregation levels (`aggLevels="none"`).
#'
#' For each of the `aggLevels`, [FrEDI::run_fredi()] performs the following summarization (using [FrEDI::aggregate_impacts()]):
#'
#' \tabular{ll}{
#' \strong{Aggregation Level} \tab \strong{Description} \cr
#' `national` \tab Annual values are summed across all regions present in the data. I.e., data is grouped by columns `"sector"`, `"adaptation"`, `"impactType"`,  `"impactYear"`, `"model_type"`, `"model"`, and `"year"`) and summed across regions. Years which have missing column data for all regions return as `NA`. The rows of the dataframe of national values (with column `region="National Total"`) are then added as rows to the results. \cr
#' `modelaverage` \tab For temperature-driven sectors, annual results are averaged across all GCM models present in the data. I.e., data is grouped by columns `"sector"`, `"adaptation"`, `"impactType"`, `"impactYear"`, `"model_type"`, `"region"`, and `"year"` and averaged across models (SLR impacts are estimated as an interpolation between SLR scenarios). Averages exclude missing values. Years which have missing column data for all models return as `NA`. The rows of model averages (with column `model="Average"` are then added as rows to the results dataframe. \cr
#' `impactType` \tab Annual results are summed across all impact types by sector present in the data. I.e., data is grouped by columns `"sector"`, `"adaptation"`, `"impactType"`, `"impactYear"`,`"model_type"`, `"model"`, `"region"`, and `"year"` and summed across impact types. Mutates column `impactType="all"` for all values. Years which have missing column data for all impact types return as `NA`. If results are aggregated across impact types, information about physical impacts (columns `"physicalmeasure"` and `"physical_impacts"`) are dropped.\cr
#' `impactYear` \tab Annual results for sectors with only one impact year estimate (i.e., `impactYear == "N/A"`) are separated from those with multiple impact year estimates. For sectors with multiple impact years (i.e. 2010 and 2090 socioeconomic runs), annual results are interpolated between impact year estimates for applicable sectors  i.e., data is grouped by columns `"sector", "adaptation", "impactType, "model_type", "model", "region", "year"` and interpolated across years with the 2010 run assigned to year 2010 and the 2090 run assigned to year 2090. The interpolated values are bound back to the results for sectors with a single impact year estimate, and column `impactYear` set to `impactYear="Interpolation"` for all values. \cr
#' }
#'
#' Users can choose to calculate present values of annual impacts (i.e., discounted impacts), by setting `pv=TRUE` (defauts to `pv=FALSE`). If `pv=TRUE`, discounted impacts are calculated using a base year and annual discount rate as `discounted_impacts=annual_impacts/(1+rate)^(year-baseYear)`. Set base year and annual discount rate using `baseYear` (defaults to `baseYear=2010`) and `rate` (defaults to 3% i.e., `rate=0.03`), respectively.
#'
#' @return
#' The output of [FrEDI::run_fredi()] is an R data frame object containing annual average impacts, by year (2010-2090), for each sector, adaptation, model (GCM or SLR scenario), and region.
#'
#' @examples
#' ### Run function with defaults (same as `defaultResults` dataset)
#' df_defaults <- run_fredi()
#'
#' ### Path to example scenarios
#' scenariosPath <- system.file(package="FrEDI") %>% file.path("extdata","scenarios")
#' ### View example scenario names
#' scenariosPath %>% list.files
#' ### Temperature Scenario File Name
#' tempInputFile <- scenariosPath %>% file.path("GCAM_scenario.csv")
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath %>% file.path("slr_from_GCAM.csv")
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath %>% file.path("pop_scenario.csv")
#' ### Import inputs
#' example_inputsList <- import_inputs(
#'   tempfile = tempInputFile,
#'   slrfile  = slrInputFile,
#'   popfile  = popInputFile
#' )
#'
#' ### Run custom temperature scenario and output impacts without aggregation and with present values (default base year and discount rate)
#' df_tempExOut <- run_fredi(inputsList= tempBin_inputs, aggLevels="none", pv=TRUE, silent=TRUE)
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'
#' @export
#' @md
#'
###### run_fredi ######
### This function creates a dataframe of sector impacts for default values or scenario inputs.
### run_fredi relies on the following helper functions: "interpolate_annual", "match_scalarValues","get_econAdjValues" , "calcScalars", "interpolate_tempBin"
run_fredi_sv <- function(
  inputsList = list(tempInput=NULL, slrInput, popInput=NULL), ### List of inputs
  sectorList = NULL, ### Vector of sectors to get results for
  return     = T,
  output2xl  = F,
  outpath    = "~",
  silent     = TRUE  ### Whether to message the user
){

  ###### Set up the environment ######
  ### Level of messaging (default is to message the user)
  silent  <- ifelse(is.null(silent), T, silent)
  msgUser <- !silent

  ###### Sectors List ######
  ### Sector names
  sector_names  <- co_sectors$sector_id
  sector_labels <- co_sectors$sector_label
  ### Initialize sector list if the sectors list is null
  if(is.null(sectorList)){
    sectorList       <- sector_names
  } else{
    ### Compare inputs to names and labels in the data
    ### Subset to sector list in sector names
    which_sectors    <- which(
      (tolower(sector_names) %in% tolower(sectorList)) |
        (tolower(sector_labels) %in% tolower(sectorList))
    )
    sectorList       <- sector_names[which_sectors]
    ### Message users about missing sectors
    which_notSectors <- which(!(
      (tolower(sectorList) %in% tolower(sector_names)) |
        (tolower(sectorList) %in% tolower(sector_labels))
    ) )
    missing_sectors  <- sectorList[which_notSectors]
    ### Message the user
    if(length(missing_sectors)>=1){
      message(
        "Warning! Error in `sectorList`.",
        "\n\t", "Impacts are not available for the following: '",
        "\n\t\t", paste(missing_sectors, collapse= "', '"), "'...",
        "\n\t", "Available sectors: '",
        "\n\t\t", paste(sector_names, collapse= "', '"), "'"
      )
    }
  }
  ### Number of sectors
  num_sectors  <- sectorList %>% length

  ###### Load Inputs ######
  ### Create logicals and initialize inputs list
  list_inputs     <- co_inputScenarioInfo$inputName
  num_inputNames  <- co_inputScenarioInfo %>% nrow

  if( is.null(inputsList)){
    inputsList <- list()
  } else{
    message("Checking input values...")
  }
  ### Iterate over the input list
  # if(!is.null(inputsList)){
  ### Assign inputs to objects
  for(i in 1:num_inputNames){
    inputInfo_i <- co_inputScenarioInfo[i,]
    ### Input name and label
    input_i     <- inputInfo_i$inputName %>% unique
    msgName_i   <- inputInfo_i$inputType %>% unique
    ### Input run_fredi argument
    inputName_i <- inputInfo_i$tempBinListName %>% unique
    ### Min and Max Values
    min_i       <- inputInfo_i$inputMin %>% unique
    max_i       <- inputInfo_i$inputMax %>% unique
    ###### Column Info ######
    region_i    <- inputInfo_i$region %>% unique
    valueCol_i  <- inputInfo_i$valueCol %>% unique
    ### Initialize column names
    numCols_i   <- colNames_i <- c("year", valueCol_i) #; print(colNames_i)
    ### Add region column
    if(region_i == 1){
      colNames_i  <- c(colNames_i[1], "region", colNames_i[2])
    }
    has_i        <- paste0("has_", input_i, "Update")
    # has_update_i <- is.null(inputsList[[inputName_i]])
    df_input_i   <- inputsList[[inputName_i]]
    has_update_i <- !is.null(df_input_i)
    ###### Assign inputs to objects ######
    assign(has_i,    has_update_i)
    assign(inputName_i, df_input_i)

    ### Iterate over the input list and check flags for inputs that are not null
    if(has_update_i){
      message("\t", "Checking input values for ", msgName_i, "...")
      ### Values
      values_i       <- df_input_i[,valueCol_i]
      ### Substitute NULL for missing values for min and max
      if(is.na(min_i)) min_i <- NULL; if(is.na(max_i)) max_i <- NULL
      ### Check the status
      flag_i         <- values_i %>% check_inputs(xmin = min_i, xmax = max_i)
      ### Return and message the user if there is a flag:
      flagStatus_i   <- flag_i$flagged
      flagRows_i     <- flag_i$rows
      ### If flag, message user and return flagStatus_i
      if(flagStatus_i){
        ### Message labels
        numrows_i    <- flagRows_i %>% length
        years_i      <- df_input_i$year[flagRows_i]; yearsLabel_i <- paste(years_i, collapse=",")
        rangeLabel_i <- paste0("c(", min_i , ",", max_i, ")")
        ### Create message and message user
        msg1_i       <- "Error in importing inputs for" %>% paste(msgName_i) %>% paste0(":")
        msg2_i       <- inputName_i %>% paste("has", numrows_i,  "values outside of defined range", rangeLabel_i)
        msg3_i       <- "Please correct values" %>% paste(msgName_i, "values for years", yearsLabel_i)

        message("\t", "\t", msg1_i); message("\t", "\t", "\t", msg2_i); message("\t", "\t","\t",  msg3_i, "..."); message("Exiting...", "\n")
        ### Return list with error and flagged rows
        returnList <- list(
          error_msg    = paste0("Error in ", inputName_i, ". Values outside range."),
          flagged_rows = flagRows_i
        )

        ### Return list and not an inputs list if an error occurred
        return(returnList)
      } ### End if flagged_i
    } ### End if !is.null(df_i)
  } ### End iterate over inputs
  # }

  ###### Check Scenario Values ######
  ### Get unique temperature scenarios and unique SLR scenarios
  c_temp_scenarios <- tempInput$scenario %>% unique
  c_slr_scenarios  <- slrInput$scenario %>% unique

  ###### Temperature Scenario ######
  ### User inputs: temperatures have already been converted to CONUS temperatures. Filter to desired range.
  ### Name the reference year temperature
  ### Add the point where impacts are zero
  refYear_temp <- (co_modelTypes %>% filter(modelUnitType=="temperature"))$modelRefYear %>% unique
  ### If no user input (default): Use temperature scenario for one region
  if(has_tempUpdate){
    message("Creating temperature scenario from user inputs...")

    tempInput <- tempInput %>%
      ### Select appropriate columns
      select(c("year", "temp_C")) %>%
      ### Remove missing values of years, temperatures
      filter(!is.na(temp_C) & !(is.na(year))) %>%
      ### Filter to appropriate years
      filter( year >  refYear_temp & year <= maxYear) %>%
      ### Zero out series at the temperature reference year
      (function(x){
        data.frame(year= refYear_temp, temp_C = 0) %>% rbind(x)
      })

    ### Create a copy
    temp_df  <- tempInput %>%
      ### Add a dummy value for National Total
      mutate(region="National Total") %>%
      ### Interpolate annual values
      (function(x){
        minYear_x <- x$year %>% min

        interpYrs <- refYear_temp:maxYear

        # newInterpYrs <- c(seq(refYear_temp, minYear_x-1), interpYrs)
        # newInterpYrs %>% print

        x_interp  <- x %>% interpolate_annual(
          years  = interpYrs,
          column = "temp_C",
          rule   = 1:2
        ) %>%
          select(-c("region"))
        return(x_interp)
      }) %>%
      rename(temp_C_conus = temp_C) %>%
      mutate(temp_C_global = temp_C_conus %>% convertTemps(from="conus"))
  }
  else{
    ### No need to interpolate these values since they're already interpolated
    message("Creating temperature scenario from defaults...")
    # tempInput <- co_defaultScenario %>% filter(region==co_regions$region_dot[1])

    # co_defaultTemps %>% nrow %>% print
    temp_df <- co_defaultTemps %>%
      ### Filter to appropriate years
      filter( year >  refYear_temp & year <= maxYear) %>%
      ### Convert to CONUS
      mutate(temp_C_conus = temp_C_global %>% convertTemps(from="global")) %>%
      select(c("year", "temp_C_conus")) %>%
      ### Filter to appropriate years
      filter( year >  refYear_temp & year <= maxYear) %>%
      ### Zero out series at the temperature reference year
      (function(x){
        data.frame(year= refYear_temp, temp_C_conus = 0) %>% rbind(x)
      }) %>%
      ### Recalculate global temps
      mutate(temp_C_global=temp_C_conus %>% convertTemps(from="conus"))
  }
  # temp_df %>% nrow %>% print

  ###### SLR Scenario ######
  ### Year where SLR impacts are zero
  refYear_slr <- (co_modelTypes %>% filter(modelUnitType=="slr"))$modelRefYear %>% unique
  ### Follow similar procedure to temperatures
  if(has_slrUpdate){
    message("Creating SLR scenario from user inputs...")

    slrInput  <-
      slrInput %>%
      ### Select appropriate columns
      select(c("year", "slr_cm")) %>%
      ### Filter values
      filter(!is.na(slr_cm) & !is.na(year)) %>%
      filter( year >  refYear_slr, year <= maxYear) %>%
      ### Zero out series at the temperature reference year
      (function(x){
        data.frame(year= refYear_slr, slr_cm = 0) %>% rbind(x)
      })

    ### Add dummy region and interpolate values
    # slr_df_years <-
    slr_df <- slrInput %>%
      mutate(region="National Total") %>%
      ### Interpolate annual values
      (function(x){
        minYear_x <- x$year %>% min
        interpYrs <- refYear_slr:maxYear

        # newInterpYrs <- c(seq(refYear_slr, minYear_x-1), interpYrs)
        # newInterpYrs %>% print

        x_interp  <- x %>% interpolate_annual(
          # years  = newInterpYrs,
          years  = interpYrs,
          column = "slr_cm",
          rule   = 1:2
        ) %>%
          select(-c("region"))
        return(x_interp)
      })

  }
  ### Else if doesn't have SLR update
  else{
    ### If there is no SLR scenario, calculate from temperatures
    ### First convert temperatures to global temperatures
    ### Then convert global temps to SLR
    message("Creating SLR scenario from temperature scenario...")

    slr_df <- temp_df %>%
      (function(x){
        temps2slr(temps = x$temp_C_global, years = x$year)
      })
  }
  driverScenario <- temp_df
  # slr_df %>% nrow %>% print
  # slr_df %>% head %>% print


  ###### Socioeconomic Scenario ######
  ### Update the socioeconomic scenario with any GDP or Population inputs and message the user
  ### Reformat GDP inputs if provided, or use defaults
  ### Population inputs
  if(has_popUpdate){
    message("Creating Population scenario from user inputs...")

  } else{
    message("Creating Population scenario from defaults...")
    pop_df        <- pop_default %>% select("year", "reg_pop", "region") %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
    national_pop  <- national_pop_default %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
  }

  if(!exists("popProjList")){load(file.path(dataPath, popProj_file))}
  df_popProj <-
    calc_countyPop(
      regPop  = region_pop,
      funList = popProjList,
      years   = c_scenYears
    ); rm("popProjList")

  ###### Scaled Impacts ######
  ### Filter initial results to specified sectors
  ### Join to the updated base scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars




  ###### Other Impacts  ######
  ### Physical impacts = physScalar * scaled_impacts
  ### Annual impacts = phys-econ scalar value by the scaled impacts
  df_impacts <- df_impacts %>%
    filter(year>=minYear) %>%
    mutate(physical_impacts = physScalar     * scaled_impacts * !is.na(physicalmeasure)) %>%
    mutate(annual_impacts   = physEconScalar * scaled_impacts) %>%
    as.data.frame

  ###### Format Results ######
  ### SV Path, Excel path info
  pathSV          <- file.path(dataPath, svData_file)
  # excel_wb_path   <- resultsPath   %>% file.path(paste(excelTemplateFile, "xlsx", sep="."))
  excel_wb_path   <- resultsPath   %>% file.path(paste("FrEDI SV Graphics Template Unformatted", "xlsx", sep="."))
  # excel_wb_path   <- resultsPath   %>% file.path(paste("test_formatting", "xlsx", sep="."))
  excel_wb_exists <- excel_wb_path %>% file.exists; excel_wb_exists %>% print
  excel_wb_sheets <- c("FrEDI Outputs 1", "FrEDI Outputs 2")
  ### Initialize list
  df_regImpacts <- list()
  sysTime3      <- Sys.time()
  # df_formatInfo2 <- df_formatInfo[1:12,]
  # df_formatInfo2 <- df_formatInfo[14:23,]
  df_formatInfo2 <- df_formatInfo
  # for(i in 2:nrow(df_sectorInfo)){
  for(i in 4:4){
    sector_i       <- c_sectorList[i]
    df_info_i      <- df_sectorInfo %>% filter(sector == sector_i)
    adapt_i        <- df_info_i$adapt_abbr
    adaptLabel_i   <- df_info_i$adapt_label
    adapt_i   %>% print

    ###### Outfile Info ######
    outFile_i    <- df_info_i$outputsFile[1]; outFile_i %>% print
    outPath_i    <- resultsPath %>% file.path(paste(outFile_i, "xlsx", sep="."))
    outPath_dir_exists_i <- outPath_i %>% dirname %>% dir.exists
    outPath_exists_i     <- outPath_i %>% file.exists
    overwrite_i          <- ifelse(outPath_exists_i, T, F)

    ###### Workbook Info ######
    df_readme1_i <- data.frame(x=c(sector_i, as.character(Sys.Date())))
    if(length(adaptLabel_i)==1){
      adaptLabel_i <- paste(sector_i, "All Impacts", sep=", ")
      df_readme2_i <- data.frame(x=c(adaptLabel_i, "N/A"))
    } else{
      df_readme2_i <- data.frame(x=adaptLabel_i)
    }
    ### Open the workbook and write  ReadMe info
    if(excel_wb_exists){
      excel_wb      <- excel_wb_path %>% loadWorkbook()

      ### Write sector, date/time, and adaptation info to workbook
      ### sector & date/time info
      excel_wb %>%
        writeData(
          x = df_readme1_i, sheet = "ReadMe", startCol = 3, startRow = 3, colNames = F
        )

      ####### Write adaptation info
      excel_wb %>%
        writeData(
          x = df_readme2_i, sheet = "ReadMe", startCol = 3, startRow = 7, colNames = F
        )

      ###### Add Styles ######
      # https://rdrr.io/cran/openxlsx/man/addStyle.html
      for(k in 1:nrow(df_formatInfo2)){
        # k %>% print

        df_info_k <- df_formatInfo2[k,] %>% as.data.frame
        format_k  <- df_info_k$styleName[1]
        sheet_k   <- df_info_k$worksheet[1] #; sheet_k %>% print
        style_k   <- list_styles[[format_k]]


        rows_k    <- (df_info_k$first_row[1]):(df_info_k$end_row[1])
        cols_k    <- (df_info_k$first_col[1]):(df_info_k$end_col[1])

        excel_wb %>% addStyle(
          style = style_k,
          sheet = sheet_k, rows = rows_k, cols = cols_k,
          gridExpand = T, stack = T
        )
      }; rm("df_info_k", "style_k", "sheet_k", "rows_k", "cols_k")
    } ### End if excel_wb_exists

    ###### Save Data ######
    if(excel_wb_exists){
      ### Save data
      if(outPath_dir_exists_i){
        "Saving workbook..." %>% message
        excel_wb %>% saveWorkbook(file=outPath_i, overwrite = overwrite_i)
      } ### End if outPath_dir_exists
    } ### End if excel_wb_exists
  } ### end i
  ### Remove iteration objects
  # rm("sector_i", "df_info_i", "adapt_i", "adaptLabel_i", "df_readme1_i", "df_readme2_i")
  # rm("outFile_i", "outPath_i", "outPath_dir_exists_i", "outPath_exists_i", "overwrite_i", "excel_wb")
  ### System time
  sysTime4 <- Sys.time()
  sysTime4 - sysTime3

  ###### Convert to Dataframe ######
  df_results   <- df_results %>% ungroup %>% as.data.frame
  ###### Return Object ######
  message("\n", "Finished", ".")
  return(df_results)

} ### End function








