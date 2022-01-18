###### Documentation ######
#' Project annual average climate change impacts throughout the 21st century for available sectors
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
run_fredi <- function(
  inputsList = list(tempInput=NULL, slrInput=NULL, gdpInput=NULL, popInput=NULL), ### List of inputs
  sectorList = NULL, ### Vector of sectors to get results for
  aggLevels  = c("national", "modelaverage", "impactyear", "impacttype"), ### Aggregation levels
  pv         = FALSE, ### T/F value indicating Whether to calculate net present value
  baseYear   = 2010, ### Default = 2010
  rate       = 0.03, ### Ratio, defaults to 0.03
  elasticity = NULL, ### Override value for elasticity for economic values
  silent     = TRUE  ### Whether to message the user
){

  ###### Set up the environment ######
  ### Level of messaging (default is to message the user)
  silent  <- ifelse(is.null(silent), T, silent)
  msgUser <- !silent
  ### Uncomment for testing
  testing <- FALSE  ### Whether to include scaled impact values
  primaryTypes <- F ### whether to filter to primary impacts

  ###### Create file paths ######
  ### Assign data objects to objects in this namespace
  ### Configuration and data list
  for(i in 1:length(tempBin_config)) assign(names(tempBin_config)[i], tempBin_config[[i]])
  for(i in 1:length(rDataList     )) assign(names(rDataList)[i], rDataList[[i]])

  ###### Present values ######
  ### Default base year and rate defined in config
  pv       <- ifelse(is.null(pv), FALSE, pv)
  baseYear <- ifelse(is.null(baseYear), baseYear0, baseYear)
  rate     <- ifelse(is.null(rate), rate0, rate)

  ###### Aggregation level  ######
  ### Types of summarization to do: default
  primaryTypes <- ifelse(is.null(primaryTypes), FALSE, primaryTypes)
  aggList0 <- c("national", "modelaverage", "impactyear", "impacttype")
  if(!is.null(aggLevels)){
    ### Aggregation levels
    aggLevels    <- aggLevels %>% tolower()
    aggLevels    <- aggLevels[which(aggLevels %in% c(aggList0, "all", "none"))]
    ### If none specified, no aggregation (only SLR interpolation)
    ### Otherwise, aggregation depends on length of agg levels
    if("none" %in% aggLevels){
      aggLevels   <- c()
      requiresAgg <- F
    } else if("all"  %in% aggLevels){
      aggLevels   <- aggList0
      requiresAgg <- T
    } else{
      requiresAgg    <- length(aggLevels) > 0
    }
  } else{
    aggLevels    <- aggList0
    requiresAgg  <- F
  }

  # ### Aggregate to impact years or national...reduces the columns in output
  # impactYearsAgg <- ifelse("impactyear"   %in% aggLevels, T, F)
  # nationalAgg    <- ifelse("national"     %in% aggLevels, T, F)
  # impactTypesAgg <- ifelse("impacttype"   %in% aggLevels, T, F)
  # modelAveAgg    <- ifelse("modelaverage" %in% aggLevels, T, F)

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
  # slr_df %>% nrow %>% print
  # slr_df %>% head %>% print

  ###### Driver Scenario  ######
  ###### Combine Scenarios and bind with the model type info
  df_drivers <-
    ### Format the temperature values
    temp_df %>%
    select(c("year", "temp_C_conus")) %>%
    rename(modelUnitValue = temp_C_conus) %>%
    mutate(modelType="gcm") %>%
    ### R Bind the SLR values
    rbind(
      slr_df %>%
        select(c("year", "slr_cm")) %>%
        rename(modelUnitValue=slr_cm) %>%
        mutate(modelType="slr")
    ) %>%
    ### Join with info about models
    left_join(
      co_modelTypes %>%
        rename(modelType = modelType_id) %>%
        select(modelType), by = "modelType"
    ) %>%

    ### Filter to the years used by the R tool
    filter( year >= minYear) %>%
    filter( year <= maxYear)
  # df_drivers %>% nrow %>% print; return()

  ###### Socioeconomic Scenario ######
  ### Update the socioeconomic scenario with any GDP or Population inputs and message the user
  ### Reformat GDP inputs if provided, or use defaults
  if(has_gdpUpdate){
    message("Creating GDP scenario from user inputs...")
    gdp_df        <- gdpInput %>%
      filter(!is.na(gdp_usd)) %>%
      mutate(region="National.Total") %>%
      interpolate_annual(years= c(list_years), column = "gdp_usd", rule = 2:2) %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
    rm("gdpInput")
  } else{
    message("Creating GDP scenario from defaults...")
    gdp_df          <- gdp_default %>% select("year", "gdp_usd", "region") %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
  }
  ### Population inputs
  if(has_popUpdate){
    message("Creating Population scenario from user inputs...")
    pop_df         <- popInput %>%
      interpolate_annual(years= c(list_years), column = "reg_pop", rule = 2:2) %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
    ### Calculate national population
    national_pop <- pop_df %>% group_by(year) %>%
      summarize_at(.vars=c("reg_pop"), sum, na.rm=T) %>%
      rename(national_pop = reg_pop) %>%
      mutate(region="National.Total")
    rm("popInput")
  } else{
    message("Creating Population scenario from defaults...")
    pop_df        <- pop_default %>% select("year", "reg_pop", "region") %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
    national_pop  <- national_pop_default %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
  }
  ### Message user
  if(has_gdpUpdate|has_popUpdate){if(msgUser){messages_tempBin[["updatePopGDP"]]}}
  ### National scenario
  national_scenario <- gdp_df %>%
    left_join(national_pop, by=c("year", "region")) %>%
    mutate(gdp_percap = gdp_usd/national_pop)
  ### Updated scenario
  updatedScenario <- national_scenario %>%
    select(-region) %>%
    left_join(pop_df, by = "year")

  ###### Update Scalars ######
  if(msgUser) message("", messages_tempBin[["updateScalars"]]$try, "")
  ### update regional population physical scalar from population scenario and bind it to the main scalars
  df_regPopScalar <- pop_df %>%
    select(c("year", "region", "reg_pop")) %>%
    rename(value=reg_pop) %>%
    mutate(
      scalarName = "reg_pop",
      scalarType = "physScalar",
      national_or_regional ="regional"
    )
  df_mainScalars <- df_regPopScalar %>%
    rbind(
      df_mainScalars %>% filter(scalarName!="reg_pop")
    ); rm(df_regPopScalar)

  ###### Initialize Results ######
  ### Filter initial results to specified sectors
  ### Join to the updated base scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars
  if(!is.null(elasticity)){if(!is.numeric(elasticity)){
    message("\t", "Incorrect value type provided for argument 'elasticity'...")
    message("\t\t", "Using default elasticity values.")
  }}

  initialResults <- df_results0 %>%
    filter(sector %in% sectorList) %>%
    left_join(updatedScenario, by = c("year", "region")) %>%
    match_scalarValues(df_mainScalars, scalarType="physScalar") %>%
    get_econAdjValues(scenario = updatedScenario, multipliers=co_econMultipliers[,1]) %>%
    calcScalars(elasticity = elasticity)
  rm("df_mainScalars") ### df_mainScalars no longer needed

  ### Message the user
  if(msgUser) message("\t", messages_tempBin[["updateScalars"]]$success)

  ###### Join Initial Results & Model Info  ######
  ### - Join Initial Results with Model info
  ### - Also add a scenario identifier and determine whether the scenario has an impact function (scenarios with all missing values have no functions)
  ### - Check if there is an impact function
  if(msgUser) message(messages_tempBin[["scaledImpacts"]]$try)

  initialResults <- initialResults %>%
    rename(model_type=modelType) %>%
    left_join(co_models %>% rename(model_type=modelType), by="model_type") %>%
    mutate(scenario_id = paste(sector, adaptation, impactYear, impactType, model_type, model_dot, region, sep="_"))

  ###### Scaled Impacts  ######
  if(msgUser) message("Calculating scaled impacts...")
  ### Initialize and empty dataframe df_scenarioResults
  df_scenarioResults  <- data.frame()
  ### List of impact functions for GCM sectors
  impactFunctionNames <- list_impactFunctions %>% names %>% unique
  ### Initial results
  ### Iterate over model types:
  for(modelType_i in co_modelTypes$modelType_id){
    ###### GCM Impacts ######
    if(tolower(modelType_i)=="gcm"){
      ### Drivers
      df_xVar_i   <- df_drivers %>% filter(modelType == modelType_i) %>% as.data.frame
      ### Functions list
      allFunctions_i <- (initialResults %>% filter(model_type==modelType_i))$scenario_id %>% unique#; length(allFunctions_i) %>% print
      df_functions_i <- data.frame(scenario_id = allFunctions_i) %>%
        mutate(
          hasScenario    = (scenario_id %in% impactFunctionNames)*1
        ) #; length(which(allFunctions_i)) %>% print
      which_hasFunc  <- which(df_functions_i$hasScenario==1)#; length(which_hasFunc) %>% print
      hasFunctions_i <-   length(which_hasFunc)>=1
      hasNonFunc_i   <- !(length(allFunctions_i) == length(which_hasFunc))

      ### Get impacts for scenario_ids that have functions
      if(hasFunctions_i){
        namesFunctions_i <- df_functions_i[which_hasFunc, "scenario_id"] %>% unique
        listFunctions_i  <- list_impactFunctions[which(impactFunctionNames %in% namesFunctions_i)]
        df_hasfun_i      <- listFunctions_i %>%
          interpolate_impacts(xVar = df_xVar_i$modelUnitValue, years = df_xVar_i$year) %>%
          rename(modelUnitValue = xVar) %>%
          filter(year>=minYear)
        df_scenarioResults <- df_scenarioResults %>% rbind(df_hasfun_i)
        # df_scenarioResults %>% names %>% print
      } #; return(df_i)
      if(hasNonFunc_i){
        df_nonfun_i    <- df_functions_i[-which_hasFunc,] %>%
          mutate(scaled_impacts = NA, joinCol = 1) %>%
          left_join(df_xVar_i %>% mutate(joinCol = 1), by=c("joinCol")) %>%
          select(-c(joinCol, hasScenario, modelType))
        # df_nonfun_i %>% names %>% print
        df_scenarioResults <- df_scenarioResults %>% rbind(df_nonfun_i)
      }
    }
    ###### SLR Impacts ######
    else{
      df_slrImpacts <- slrImpacts %>%
        ### Empty column for modelUnitValue
        mutate(modelUnitValue=NA) %>%
        mutate(
          scenario_id = paste(sector, adaptation, impactYear, impactType, model_type, model_dot, region, sep="_")
          ) %>%
        select(c("scenario_id", "year", "modelUnitValue", "scaled_impacts"))
      # df_scenarioResults %>% names %>% print
      df_scenarioResults <- df_scenarioResults %>% rbind(df_slrImpacts)
    }
  }

  ### Join results with initialized results and update missing observations with NA
  ### Remove intermediate values
  df_impacts   <- initialResults %>% left_join(df_scenarioResults, by=c("scenario_id", "year")); rm("initialResults")

  if(msgUser) message("\t", messages_tempBin[["scaledImpacts"]]$success)

  ###### Calculate Impacts  ######
  ### Physical impacts = physScalar * scaled_impacts
  ### Annual impacts = phys-econ scalar value by the scaled impacts
  df_impacts <- df_impacts %>%
    filter(year>=minYear) %>%
    mutate(physical_impacts = physScalar     * scaled_impacts * !is.na(physicalmeasure)) %>%
    mutate(annual_impacts   = physEconScalar * scaled_impacts) %>%
    as.data.frame

  ###### Add Scenario Information ######
  message("Formatting results", "...")
  df_results <- df_impacts %>% select(-modelUnitValue) %>%
    left_join(
      df_drivers %>% filter(year>=minYear) %>% rename(model_type = modelType),
      by=c("year", "model_type")
    )
  rm("df_impacts")

  ###### Format Outputs ######
  ### Refactor sectors, adaptation levels, impactTypes
  co_adaptations <- co_adaptations %>% mutate(sector_adaptation = paste(sector_id, adaptation_id, sep="_"))
  co_impactTypes <- co_impactTypes %>% mutate(sector_impactType = paste(sector_id, impactType_id, sep="_"))

  #### Rename Sector Columns
  df_results <- df_results %>% rename(sector_id = sector, sector = sector_label)

  #### Regions
  df_results <- df_results %>%
    mutate(
      region_id = region,
      region    = region_id %>% factor(levels=co_regions$region_dot, labels=co_regions$region_label)
    ) %>% select(-c("region_id"))

  ### Model types and models
  df_results <- df_results %>%
    select(-model_type) %>%
    rename(
      model       = model_label,
      model_type  = modelType_label,
      driverValue = modelUnitValue,
      driverUnit  = modelUnit_label,
      driverType  = modelUnitDesc
    ) %>%
    select(-c("model_id", "model_dot", "model_underscore", "modelUnit_id")) %>%
    select(-c("modelRefYear", "modelMaxOutput", "modelUnitScale", "modelMaxExtrap"))
  ### Adaptation labels
  df_results <- df_results %>%
    mutate(
      adaptation_id     = adaptation,
      sector_adaptation = sector_id %>% paste(adaptation_id, sep="_"),
      adaptation        = sector_adaptation %>% factor(levels=co_adaptations$sector_adaptation, labels=co_adaptations$adaptation_label)
    ) %>% select(-c("adaptation_id", "sector_adaptation"))
  ### Impact types
  df_results <- df_results %>%
    mutate(
      impactType_id     = impactType,
      sector_impactType = sector_id %>% paste(impactType_id, sep="_"),
      impactType        = sector_impactType %>% factor(levels=co_impactTypes$sector_impactType, labels=co_impactTypes$impactType_label)
    ) %>% select(-c("impactType_id", "sector_impactType"))
  ### Convert to character and drop sector id
  df_results <- df_results %>%
    mutate_at(vars(sector, adaptation, impactYear, impactType, model_type, model, region), as.character) %>%
    select(-c("sector_id")) %>% filter(!is.na(sector))

  ###### Simplify the Dataframe ######
  ### Scalar column names
  cScalarNames <- c(
    "physScalarName", "physAdjName", "damageAdjName", "physScalar",
    "physScalarValue","physAdjValue", "damageAdjValue",
    "econScalarName", "econMultiplierName", "econScalar",
    "econScalarValue", "econMultiplierValue", "econAdjValue", "econMultiplier",
    "physEconScalar", "scaled_impacts"
  )
  ### Sector info names
  cSectorInfoNames <- c("modelUnitType", "c0", "c1", "exp0", "year0")
  ### Scenario names
  cScenarioNames   <- c("scenario_id")
  ### Filter some columns
  if(!testing){
    df_results <- df_results %>%
      select(-c(
        all_of(cScenarioNames),
        all_of(cScalarNames),
        all_of(cSectorInfoNames)
      ))
  }

  ###### SLR Interpolation ######
  c_modelTypes <- df_results$model_type %>% unique
  if("slr" %in% tolower(c_modelTypes)){
    df_results_nonSLR <- df_results %>% filter(tolower(model_type)!="slr")
    df_results_SLR    <- df_results %>% filter(tolower(model_type)=="slr")
    slrGroupByCols    <- c("sector", "adaptation", "impactYear", "impactType", "model_type", "model", "region", "sectorprimary", "includeaggregate")
    slr_results_names <- df_results_SLR %>% names
    slrGroupByCols    <- slrGroupByCols[which(slrGroupByCols %in% slr_results_names)]

    df_results_SLR    <- df_results_SLR %>%
      aggregate_impacts(aggLevels = "modelaverage", groupByCols = slrGroupByCols, mode="slrinterpolation")

    df_results        <- df_results_nonSLR %>% rbind(df_results_SLR)

    rm("df_results_nonSLR", "df_results_SLR", "slrGroupByCols", "slr_results_names")
  }

  ###### Aggregation ######
  ### For regular use (i.e., not impactYears), simplify the data:
  if(requiresAgg){

    aggGroupByCols <- c("sector", "adaptation", "impactYear", "impactType", "model_type", "model", "region")
    includeAggCol  <- "includeaggregate"

    ### If the user specifies primary type, filter to primary types and adaptations and drop that column
    if(primaryTypes){
      df_results    <- df_results %>% filter(includeaggregate==1) %>% select(-c(all_of(includeAggCol)))
    } else{
      aggGroupByCols <- aggGroupByCols %>% c(includeAggCol)
    }
    agg_results_names <- df_results %>% names

    df_results <- df_results %>% aggregate_impacts(aggLevels = aggLevels, groupByCols = aggGroupByCols)

    rm("aggGroupByCols", "agg_results_names")
  }


  ###### Order the Output ######
  ### Convert levels to character
  ### Order the rows, then order the columns
  resultNames   <- df_results %>% names
  groupByCols   <- c("sector",  "adaptation", "impactYear", "impactType", "region", "model_type", "model", "year")
  driverCols    <- c("driverValue", "driverUnit", "driverType")
  nonGroupCols  <- resultNames[which(!(resultNames %in% c(groupByCols, driverCols)))]
  orderColIndex <- which(names(data) %in% groupByCols)
  df_results    <- df_results %>%
    select(c(all_of(groupByCols), all_of(driverCols), all_of(nonGroupCols)))

  # c_aggColumns <- c("sectorprimary", "includeaggregate") %>% (function(y){y[which(y %in% names(df_results))]})
  # if(length(c_aggColumns)>0){
  #   df_results     <- df_results %>% mutate_at(.vars=c(all_of(c_aggColumns)), as.numeric)
  # }

  ###### Present Values ######
  if(pv){
    ### Discount rate info
    df_rates <- data.frame(
      year = list_years,
      rate = rate,
      baseYear = baseYear
    ) %>%
      mutate(
        discountFactor = 1 / (1 + rate)^(year - baseYear)
      )
    ### Discounted impacts
    df_results <- df_results %>%
      left_join(df_rates, by = c("year")) %>%
      mutate(discounted_impacts = annual_impacts * discountFactor)
  }

  ###### Convert to Dataframe ######
  df_results   <- df_results %>% ungroup %>% as.data.frame
  ###### Return Object ######
  message("\n", "Finished", ".")
  return(df_results)

} ### End function








