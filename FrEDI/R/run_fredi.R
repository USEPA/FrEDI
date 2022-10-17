###### Documentation ######
#' Project annual average climate change impacts throughout the 21st century for available sectors
#'
#' @description
#' This function allows users to project annual average climate change impacts throughout the 21st century (2010-2090) for available sectors (see [FrEDI::get_sectorInfo()]). Users may specify an optional list of custom scenarios. The output is an R data frame object containing annual average impacts, by year, for each sector, adaptation (or variant), impact type, model (GCM or SLR scenario), and region.
#'
#' @param inputsList A list of named elements named elements (`names(inputsList)= c("tempInput", "slrInput", "gdpInput", "popInput")`), each containing dataframes of custom temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and/or population scenarios, respectively, over the period 2010 to 2090. Note: temperature and sea level rise inputs should start in 2000 or earlier.
#' @param sectorList A character vector indicating a selection of sectors for which to calculate results (see [FrEDI::get_sectorInfo()]). If `NULL`, all sectors are included.
#' @param aggLevels Levels of aggregation at which to summarize data: one or more of `c("national"`, `"modelaverage"`, `"impactyear"`, `"impacttype"`, `"all")`. Defaults to all levels (i.e., `aggLevels="all"`). Uses the same aggregation levels as [FrEDI::aggregate_impacts()].
#' @param pv A `TRUE/FALSE` value indicating Whether to calculate present values for the annual impacts. Defaults to `pv=TRUE`. Present values (i.e., discounted impacts) are calculated as `discounted_impacts=annual_impacts/(1+rate)^(year-baseYear)`. Set an annual discounting rate and a base year using `baseYear` and `rate`.
#' @param baseYear Base year used for calculating present values of annual impacts (i.e., discounting). Defaults to `baseYear=2010`.
#' @param rate Annual discount rate used in calculating present values of annual impacts (i.e., discounting). Defaults to `rate=0.03` (i.e., 3% per year).
# @param primaryTypes = F whether to filter to primary impacts
#' @param elasticity=NULL A numeric value indicating an elasticity to use for adjusting VSL for applicable sectors and impacts. Applicable sectors and impacts are: Air Quality (all impact types), CIL Extreme Temperature (all impact types), Extreme Temperature (all impact types), Southwest Dust (All Mortality), Valley Fever (Mortality), and Wildfire (Mortality). If `elasticity=NULL` (default), [FrEDI::run_fredi()] uses default elasticities.
#' @param silent A `TRUE/FALSE` value indicating the level of messaging desired by the user (default=`TRUE`).
#'
#' @details This function allows users to project annual average climate change impacts throughout the 21st century (2010-2090) for available sectors. [FrEDI::run_fredi()] is the main function in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information).
#'
#' Users can run [FrEDI::run_fredi()] for all the sectors (default) or run FrEDI for specific sectors specified as a character vector to the `sectorsList` argument (run [FrEDI::get_sectorInfo()] to see a list of available sectors).
#'
#' #' Users can run FrEDI with the default scenario or have the option to specify custom inputs as a list of scenarios. The output of [FrEDI::run_fredi()] is an R data frame object containing annual average impacts, by year, for each sector, variant, impact type, model (GCM or SLR scenario), and region.
#'
#' Users can specify an optional list of custom scenarios with `inputsList` (for more information on the format of inputs, see [FrEDI::import_inputs()]). [FrEDI::run_fredi()] uses default scenarios for temperature, population, and GDP when no inputs are specified (i.e., `inputsList=NULL`) or for empty elements of the inputs list. If the user does not specify an input scenario for GMSL (i.e., `inputsList=list(slrInput=NULL)`, [FrEDI::run_fredi()] first converts the CONUS temperature scenario to global temperatures and then converts the global temperatures to a global mean sea level rise (GMSL) height in centimeters. For more information on the conversion of CONUS temperatures to global temperatures, see [FrEDI::convertTemps()]. For more information on the conversion of global temperatures to GMSL, see [FrEDI::temps2slr()].
#'
#' Values for input scenarios must be within reasonable ranges. If a user inputs a custom scenario with values outside the allowable ranges, [FrEDI::run_fredi()] will not run the scenarios and will instead stop and return an error message. For more information, see [FrEDI::import_inputs()]. Temperature and GMSL inputs must begin in 2000 or earlier. Values for population and GDP scenarios can start in 2010 or earlier.
#'
#' * The input temperature scenario (passed to [FrEDI::run_fredi()] via the `inputsList` argument) requires temperatures for the contiguous U.S. (CONUS) in degrees Celsius relative to 1995 (degrees of warming relative to the baseline). Temperature values must be greater than or equal to zero and less than or equal to 10 degrees Celsius. Users can convert global temperatures to CONUS temperatures using `FrEDI::convertTemps(from="global")` or by specifying `FrEDI::import_inputs(temptype="global")` when importing a temperature scenario from a CSV file.
#' * Values for the sea level rise (SLR) scenario are for global mean sea level rise (GMSL) must be in centimeters (cm) and values must be greater than or equal to zero and less than or equal to 250 cm.
#' * Population and gross domestic product (GDP) values must be greater than or equal to zero.
#'
#' If `inputsList=NULL`, [FrEDI::run_fredi()] uses defaults for all scenarios. Otherwise, [FrEDI::run_fredi()] looks for a list object passed to the argument `inputsList`. Within that list, [FrEDI::run_fredi()] looks for list elements `tempInput`, `slrInput`, `gdpInput`, and `popInput` containing dataframes with custom scenarios for temperature, GMSL, GDP, and regional population, respectively. [FrEDI::run_fredi()] will default back to the default scenarios for any list elements that are `NULL` or missing. In other words, running `run_fredi(inputsList=list())` returns the same outputs as running [FrEDI::run_fredi()]. For help importing custom scenarios from CSV files, refer to the pre-processing function [FrEDI::import_inputs()].
#'
#' [FrEDI::run_fredi()] linearly interpolates missing annual values for all input scenarios using non-missing values (requires at least two non-missing values). Temperatures are interpolated using 1995 as the baseline year (i.e., the central year of the 1986-2005 baseline). In other words, the temperature (in degrees Celsius) is set to zero for the year 1995 and GMSL is set to zero for the year 2000. The interpolated temperature and GMSL scenarios are combined into a column called `driverValue`, along with additional columns for year, the driver unit (column `"driverUnit"`, with `driverUnit="degrees Celsius"` and `driverUnit="cm"` for temperature- and SLR-driven sectors, respectively), and the associated model type (column `"model_type"`, with `model_type="GCM"` and `model_type="SLR"` for temperature- and SLR-driven sectors, respectively).
#'
#' The population scenario must provide annual regional values for population, with national totals calculated from regional values. FrEDI uses the national population scenario and the GDP scenario to calculate GDP per capita. Values for regional population, national population, national GDP (in 2015$), and national per capita GDP (in 2015$/capita) are provided in the results dataframe in columns `"reg_pop"`, `"national_pop"`, `"gdp_usd"`, and `"gdp_percap"`, respectively.
#'
#' Annual impacts for each sector, variant, impact type, and impact year combination included in the model are calculated by multiplying scaled climate impacts by a physical scalar and economic scalars and multipliers.
#'
#' [FrEDI::run_fredi()] aggregates or summarizes results to levels of aggregation specified by the user (passed to `aggLevels`) using the post-processing helper function [FrEDI::aggregate_impacts()]. Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels="national"`), averaging across model types and models (`aggLevels="modelaverage"`), summing over all impact types (`aggLevels="impacttype"`), and interpolate between impact year estimates (`aggLevels="impactYear"`). Users can specify all aggregation levels at once by specifying `aggLevels="all"` (default) or no aggregation levels (`aggLevels="none"`).
#'
#' For each of the `aggLevels`, [FrEDI::run_fredi()] performs the following summarization using [FrEDI::aggregate_impacts()] (note that the `"variant"` column referred to below contains information about the adaptation or variant name or `“N/A”`, as applicable):
#'
#' \tabular{ll}{
#' \strong{Aggregation Level} \tab \strong{Description} \cr
#' `national` \tab Annual values are summed across all regions present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`,  `"impactYear"`, `"model_type"`, `"model"`, and `"year"`) and summed across regions. Years which have missing column data for all regions return as `NA`. The rows of the dataframe of national values (with column `region="National Total"`) are then added as rows to the results. \cr
#' `modelaverage` \tab For temperature-driven sectors, annual results are averaged across all GCM models present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"model_type"`, `"region"`, and `"year"` and averaged across models (SLR impacts are estimated as an interpolation between SLR scenarios). Averages exclude missing values. Years which have missing column data for all models return as `NA`. The rows of model averages (with column `model="Average"` are then added as rows to the results dataframe. \cr
#' `impactType` \tab Annual results are summed across all impact types by sector present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`,`"model_type"`, `"model"`, `"region"`, and `"year"` and summed across impact types. Mutates column `impactType="all"` for all values. Years which have missing column data for all impact types return as `NA`. If results are aggregated across impact types, information about physical impacts (columns `"physicalmeasure"` and `"physical_impacts"`) are dropped.\cr
#' `impactYear` \tab Annual results for sectors with only one impact year estimate (i.e., `impactYear == "N/A"`) are separated from those with multiple impact year estimates. For sectors with multiple impact years (i.e. 2010 and 2090 socioeconomic runs), annual results are interpolated between impact year estimates for applicable sectors  i.e., data is grouped by columns `"sector", "variant", "impactType, "model_type", "model", "region", "year"` and interpolated across years with the 2010 run assigned to year 2010 and the 2090 run assigned to year 2090. The interpolated values are bound back to the results for sectors with a single impact year estimate, and column `impactYear` set to `impactYear="Interpolation"` for all values. \cr
#' }
#'
#' Users can choose to calculate present values of annual impacts (i.e., discounted impacts), by setting `pv=TRUE` (defauts to `pv=FALSE`). If `pv=TRUE`, discounted impacts are calculated using a base year and annual discount rate as `discounted_impacts=annual_impacts/(1+rate)^(year-baseYear)`. Set base year and annual discount rate using `baseYear` (defaults to `baseYear=2010`) and `rate` (defaults to 3% i.e., `rate=0.03`), respectively.
#'
#' @return
#' The output of [FrEDI::run_fredi()] is an R data frame object containing annual average impacts, by year (2010-2090), for each sector, variant, model (GCM or SLR scenario), and region.
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
    elasticity = NULL, ### Override value for elasticity for economic values
    maxYear    = 2090,
    thru2300   = FALSE,
    pv         = FALSE, ### T/F value indicating Whether to calculate net present value
    baseYear   = 2010, ### Default = 2010
    rate       = 0.03, ### Ratio, defaults to 0.03
    silent     = TRUE  ### Whether to message the user
){
  
  sectorList = c_sectorsList
  aggLevels="none"
  maxYear=2200
   inputsList = testInputs$ramp
   elasticity = NULL
  thru2300   = FALSE
   pv         = FALSE
   rate       = 0.03
   baseYear   = 2010
   rate       = 0.03
   silent     = TRUE
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user)
  silent   <- ifelse(is.null(silent), T, silent)
  msgUser  <- !silent
  ### Uncomment for testing
  testing  <- FALSE  ### Whether to include scaled impact values
  primaryTypes <- F ### whether to filter to primary impacts
  ### Model years
  maxYear0 <- ifelse(thru2300, 2300, maxYear)

  ###### Create file paths ######
  ### Assign data objects to objects in this namespace
  ### Configuration and data list
  for(i in 1:length(fredi_config)) assign(names(fredi_config)[i], fredi_config[[i]])
  for(i in 1:length(rDataList   )) assign(names(rDataList)[i], rDataList[[i]])

  ### Years
  maxYear    <- maxYear0
  list_years <- minYear:maxYear
  # maxYear    %>% print; list_years %>% max %>% print

  ###### Present values ######
  ### Default base year and rate defined in config
  pv         <- ifelse(is.null(pv), FALSE, pv)
  baseYear   <- ifelse(is.null(baseYear), baseYear0, baseYear)
  rate       <- ifelse(is.null(rate), rate0, rate)

  ###### Aggregation level  ######
  ### Types of summarization to do: default
  primaryTypes <- ifelse(is.null(primaryTypes), FALSE, primaryTypes)
  aggList0     <- c("national", "modelaverage", "impactyear", "impacttype")
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
      requiresAgg <- length(aggLevels) > 0
    }
  } else{aggLevels <- aggList0; requiresAgg  <- F}

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

  if(is.null(inputsList)) {inputsList <- list()}else{message("Checking input values...")}
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
  } ### End iterate over inputs
  # }

  ###### Temperature Scenario ######
  ### User inputs: temperatures have already been converted to CONUS temperatures. Filter to desired range.
  ### Name the reference year temperature
  ### Add the point where impacts are zero
  refYear_temp <- (co_modelTypes %>% filter(modelUnitType=="temperature"))$modelRefYear %>% unique
  # co_modelTypes %>% names %>% print

  ### If no user input (default): Use temperature scenario for one region
  if(has_tempUpdate){
    message("Creating temperature scenario from user inputs...")
    ### Select appropriate columns
    ### Remove missing values of years, temperatures
    ### Filter to appropriate years
    tempInput <- tempInput %>%
      select(c("year", "temp_C")) %>%
      filter(!is.na(temp_C) & !(is.na(year))) %>%
      filter( year >  refYear_temp & year <= maxYear)
    ### Zero out series at the temperature reference year
    tempInput <- data.frame(year= refYear_temp, temp_C = 0) %>% rbind(tempInput)

    ### Add a dummy value for National Total
    ### Interpolate annual values
    temp_df  <- tempInput %>% mutate(region="National Total")
    temp_df  <- temp_df   %>% (function(x){
      minYear_x <- x$year %>% min
      interpYrs <- refYear_temp:maxYear
      ### Interpolate
      x_interp  <- x %>% interpolate_annual(
        years  = interpYrs,
        column = "temp_C",
        rule   = 1:2
      ) %>% select(-c("region"))
      return(x_interp)
    })
    temp_df  <- temp_df  %>% rename(temp_C_conus = temp_C)
    temp_df  <- temp_df  %>% mutate(temp_C_global = temp_C_conus %>% convertTemps(from="conus"))
    rm("tempInput")
  } else{
    ### No need to interpolate these values since they're already interpolated
    message("Creating temperature scenario from defaults...")
    # tempInput <- co_defaultScenario %>% filter(region==co_regions$region_dot[1])
    # co_defaultTemps %>% nrow %>% print
    ### Filter to appropriate years
    ### Convert to CONUS
    ### Filter to appropriate years
    temp_df <- co_defaultTemps %>%
      filter( year >  refYear_temp & year <= maxYear) %>%
      mutate(temp_C_conus = temp_C_global %>% convertTemps(from="global")) %>%
      select(c("year", "temp_C_conus")) %>%
      filter( year >  refYear_temp & year <= maxYear)
    ### Zero out series at the temperature reference year
    ### Recalculate global temps
    temp_df <- data.frame(year= refYear_temp, temp_C_conus = 0) %>% rbind(temp_df)
    temp_df <- temp_df %>% mutate(temp_C_global=temp_C_conus %>% convertTemps(from="conus"))
  } ### End else(has_tempUpdate)
  # temp_df %>% nrow %>% print

  ###### SLR Scenario ######
  ### Year where SLR impacts are zero
  refYear_slr <- (co_modelTypes %>% filter(modelUnitType=="slr"))$modelRefYear %>% unique
  # co_modelTypes %>% names %>% print

  ### Follow similar procedure to temperatures
  ### Select appropriate columns
  ### Select out NA values and filter to appropriate years
  if(has_slrUpdate){
    message("Creating SLR scenario from user inputs...")
    slrInput  <- slrInput %>%
      select(c("year", "slr_cm")) %>%
      filter(!is.na(slr_cm) & !is.na(year)) %>%
      filter( year >  refYear_slr, year <= maxYear)
    ### Zero out series at the temperature reference year
    slrInput  <- data.frame(year= refYear_slr, slr_cm = 0) %>% rbind(slrInput)
    ### Add dummy region and interpolate values
    slr_df <- slrInput %>% mutate(region="National Total")
    slr_df <- slr_df   %>% (function(x){
      minYear_x <- x$year %>% min
      interpYrs <- refYear_slr:maxYear
      ### Interpolate annual values
      x_interp  <- x %>% interpolate_annual(
        years  = interpYrs,
        column = "slr_cm",
        rule   = 1:2
      ) %>% select(-c("region"))
      return(x_interp)
    })
    rm("slrInput")
  }  else{
    ### If there is no SLR scenario, calculate from temperatures
    ### First convert temperatures to global temperatures
    ### Then convert global temps to SLR
    message("Creating SLR scenario from temperature scenario...")
    slr_df <- temp_df %>% (function(x){temps2slr(temps = x$temp_C_global, years = x$year)})
  } ### End else(has_slrUpdate)
  # slr_df %>% nrow %>% print
  # slr_df %>% head %>% print
  # slr_df$year %>% range %>% print

  ###### Driver Scenario  ######
  ### Format the temperature and SLR values
  temp_df <- temp_df %>% select(c("year", "temp_C_conus")) %>%
    rename(modelUnitValue = temp_C_conus) %>% mutate(modelType="gcm")
  slr_df  <- slr_df  %>% select(c("year", "slr_cm")) %>%
    rename(modelUnitValue=slr_cm) %>% mutate(modelType="slr")
  ###### Combine Scenarios and bind with the model type info
  ### R Bind the SLR values
  ### Join with info about models
  ### Filter to the years used by the R tool
  co_modelTypes <- co_modelTypes %>% rename(modelType = modelType_id)
  co_modelType0 <- co_modelTypes %>% select(c("modelType"))
  df_drivers    <- temp_df    %>% rbind(slr_df)
  df_drivers    <- df_drivers %>% filter( year >= minYear) %>% filter( year <= maxYear)
  # df_drivers %>% names %>% print; co_modelType0 %>% names %>% print
  df_drivers    <- df_drivers %>% left_join(co_modelType0, by = "modelType")
  rm("co_modelType0")

  ###### Socioeconomic Scenario ######
  ### Update the socioeconomic scenario with any GDP or Population inputs and message the user
  ### Reformat GDP inputs if provided, or use defaults
  if(has_gdpUpdate){
    message("Creating GDP scenario from user inputs...")
    gdp_df <- gdpInput %>%
      filter(!is.na(gdp_usd)) %>%
      mutate(region="National.Total") %>%
      interpolate_annual(years= c(list_years), column = "gdp_usd", rule = 2:2) %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
    rm("gdpInput")
  } else{
    message("Creating GDP scenario from defaults...")
    gdp_df <- gdp_default %>% select("year", "gdp_usd", "region")
    gdp_df <- gdp_df      %>% filter( year >= minYear) %>% filter( year <= maxYear)
  }
  ### Population inputs
  if(has_popUpdate){
    message("Creating Population scenario from user inputs...")
    ### Standardize region and then interpolate
    pop_df         <- popInput %>%
      mutate(region = gsub(" ", ".", region)) %>%
      interpolate_annual(years= c(list_years), column = "reg_pop", rule = 2:2) %>%
      filter( year >= minYear) %>% filter( year <= maxYear) #%>% mutate_at(.vars=c("reg_pop"), round, 0)
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
  # df_regPopScalar$year %>% print
  df_mainScalars <- df_mainScalars %>% filter(year >= minYear) %>% filter(year <= maxYear)
  df_mainScalars <- df_regPopScalar %>% rbind(df_mainScalars %>% filter(scalarName!="reg_pop"));
  rm("df_regPopScalar")

  ###### NPD Scalars ######
  ### First set of scalars
  # national_scenario %>% names %>% print
  # slr_sectors    <- c("CoastalProperties", "HTF")
  co_npdScalars <- data.frame(
    sector          = c("CoastalProperties", "HTF"),
    npd_scalarType  = c("gdp_percap", "gdp_usd"),
    c1   = c(1,  0.16425),
    exp0 = c(ifelse(is.null(elasticity), 0.45, elasticity), 1),
    c2   = c(0, 0.8375)
  )
  c_npdRefYear <- 2090
  ### Calculate scalars
  npdScalars <- national_scenario %>%
    filter(year >= c_npdRefYear) %>%
    select(c("year", "gdp_usd", "gdp_percap")) %>%
    gather(
      key = "npd_scalarType", value="npd_scalarValue",
      c("gdp_usd", "gdp_percap")
    )
  ### Get 2090 values and then bind them
  npdScalars <- npdScalars %>% (function(y){
    y2 <- y %>%
      filter(year == c_npdRefYear) %>% select(-c("year")) %>%
      rename(npd_scalarValueRef = npd_scalarValue)
    y  <- y %>%
      left_join(y2, by = c("npd_scalarType")) %>%
      left_join(co_npdScalars, by = c("npd_scalarType"))
    return(y)
  })
  ### Calculate scalar value
  npdScalars <- npdScalars %>% mutate(npd_scalarValue = c1 * (npd_scalarValue / npd_scalarValueRef)**exp0)
  npdScalars <- npdScalars %>% select(-c("c1", "exp0", "npd_scalarValueRef"))

  # (npdScalars %>% filter(year > 2090))$year %>% head %>% print
  ### Join with regional population
  npdScalars <- npdScalars %>% left_join(
    pop_df %>% filter(year >= c_npdRefYear) %>% select(c("year", "region", "reg_pop")),
    by = c("year")
  )
  npdScalars <- npdScalars %>% left_join(
    pop_df %>% filter(year == c_npdRefYear) %>% select(-c("year")) %>%
      select(c("region", "reg_pop")) %>% rename(reg_popRef = reg_pop),
    by = c("region")
  )
  ### Calculate value and rename values
  npdScalars <- npdScalars %>% mutate(
    npd_scalarValue = npd_scalarValue + c2 * (reg_pop / reg_popRef)
  ) %>% select(-c("c2", "reg_pop", "reg_popRef"))
  ### Rename values
  npdScalars <- npdScalars %>% mutate(
    econScalar     = npd_scalarValue,
    physEconScalar = npd_scalarValue
  )
  npdScalars <- npdScalars %>% filter(year > c_npdRefYear)
  npdScalars <- npdScalars %>% select(-c("npd_scalarValue", "npd_scalarType"))
  # rm("co_npdScalars")


  ###### Initialize Results ######
  ### Filter initial results to specified sectors
  ### Join to the updated base scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars
  if(!is.null(elasticity)){if(!is.numeric(elasticity)){
    message("\t", "Incorrect value type provided for argument 'elasticity'...")
    message("\t\t", "Using default elasticity values.")
  }}
  df_results0    <- df_results0 %>% filter(year >= minYear) %>% filter(year <= maxYear)
  initialResults <- df_results0 %>%
    filter(sector %in% sectorList) %>%
    left_join(updatedScenario, by = c("year", "region")) %>%
    match_scalarValues(df_mainScalars, scalarType="physScalar") %>%
    get_econAdjValues(scenario = updatedScenario, multipliers=co_econMultipliers[,1]) %>%
    calcScalars(elasticity = elasticity)
  rm("df_mainScalars") ### df_mainScalars no longer needed
  ### Get initial results for NPD
  initialResults_npd <- initialResults %>%
    filter( (sector %in% co_npdScalars$sector & year > c_npdRefYear)) %>%
    select(-c("econScalar", "physEconScalar")) %>%
    left_join(npdScalars, by = c("sector", "year", "region"));
  # ### Adjust NPD scalars
  initialResults    <- initialResults %>%
    filter(!(sector %in% co_npdScalars$sector & year > c_npdRefYear)) %>%
    rbind(initialResults_npd)
  rm("initialResults_npd", "co_npdScalars", "npdScalars")
  ### Message the user
  if(msgUser) message("\t", messages_tempBin[["updateScalars"]]$success)


  ###### Scenario ID  ######
  ### Create scenario ID and separate by model type
  initialResults     <- initialResults %>% mutate(model_type=modelType)
  initialResults_slr <- initialResults %>% filter(modelType=="slr")
  initialResults     <- initialResults %>% filter(modelType!="slr")

  nrow_gcm           <- initialResults     %>% nrow
  nrow_slr           <- initialResults_slr %>% nrow

  ###### Scaled Impacts  ######
  ### Initialize and empty dataframe df_scenarioResults
  if(msgUser) message(messages_tempBin[["scaledImpacts"]]$try)
  if(msgUser) message("Calculating scaled impacts...")
  df_scenarioResults  <- data.frame()
  impactSelectCols    <- c("year", "scaled_impacts", "scenario_id")


  ###### GCM Scaled Impacts ######
  if(nrow_gcm){
    ### Drivers
    df_drivers_gcm <- df_drivers %>% filter(modelType == "gcm")
    ### Get scenario id
    # initialResults_slr <- initialResults_slr %>% get_scenario_id(include=c())
    initialResults <- initialResults %>% left_join(co_models, by="modelType")
    initialResults <- initialResults %>% get_scenario_id(include=c("model_dot", "region"))
    initialResults <- initialResults %>% select(-c("model_type"))
    ### Get list of unique impact functions
    impFunNames    <- list_impactFunctions %>% names %>% unique
    ### Check whether the scenario has an impact function (scenarios with all missing values have no functions)
    gcmAllFuncs    <- initialResults$scenario_id %>% unique
    df_gcm_i       <- data.frame(scenario_id = gcmAllFuncs)
    df_gcm_i       <- df_gcm_i %>% mutate(hasScenario = (scenario_id %in% impFunNames)*1)
    ### Figure out which have functions
    which_hasFunc  <- which(df_gcm_i$hasScenario==1)
    gcmHasFuns     <- length(which_hasFunc)>=1
    gcmNoFuns      <- !(length(gcmAllFuncs) == length(which_hasFunc))
    # impFunNames[1:5] %>% print; gcmAllFuncs[1:5] %>% print; which_hasFunc %>% head %>% print

    ### Get impacts for scenario_ids that have functions
    if(gcmHasFuns){
      hasFunNames <- df_gcm_i[which_hasFunc, "scenario_id"] %>% unique
      hasFunsList <- list_impactFunctions[which(impFunNames %in% hasFunNames)]
      ### Get impacts
      imp_hasFuns <- hasFunsList %>% interpolate_impacts(xVar = df_drivers_gcm$modelUnitValue, years = df_drivers_gcm$year)
      imp_hasFuns <- imp_hasFuns  %>% rename(modelUnitValue = xVar) %>% filter(year>=minYear)
      imp_hasFuns <- imp_hasFuns  %>% select(c(all_of(impactSelectCols)))
      # df_scenarioResults %>% names %>% print
      df_scenarioResults <- df_scenarioResults %>% rbind(imp_hasFuns)
      rm("hasFunNames", "hasFunsList", "imp_hasFuns")
    } #; return(df_i)
    if(gcmNoFuns){
      imp_noFuns  <- df_gcm_i[-which_hasFunc,]
      imp_noFuns  <- imp_noFuns %>% mutate(scaled_impacts = NA, joinCol = 1)
      imp_noFuns  <- imp_noFuns %>% left_join(df_drivers_gcm %>% mutate(joinCol = 1), by=c("joinCol"))
      imp_noFuns  <- imp_noFuns %>% select(c(all_of(impactSelectCols)))
      # df_scenarioResults %>% names %>% print; imp_noFuns %>% names %>% print
      df_scenarioResults <- df_scenarioResults %>% rbind(imp_noFuns)
      rm("imp_noFuns")
    }
    rm("df_drivers_gcm", "gcmAllFuncs", "which_hasFunc", "gcmHasFuns", "gcmNoFuns")
  }

  ###### SLR Scaled Impacts ######
  if(nrow_slr){
    ### Filter to appropriate number of years
    slrImpacts     <- slrImpacts  %>% filter(year <= maxYear)
    slrExtremes    <- slrExtremes %>% filter(year <= maxYear)
    slrDrivers     <- df_drivers  %>% filter(modelType=="slr") #%>% rename(model_type=modelType)

    ###### ** SLR Scaled Impacts Above Max #######
    ### Examine driver values: combine with extremeSs
    slrMax         <- (co_modelTypes %>% filter(modelType=="slr"))$modelMaxOutput[1]
    ### Combine SLR with extremes and filter to appropriate years
    df_slrMax      <- slrDrivers
    df_slrMax      <- df_slrMax   %>% left_join(slrExtremes, by=c("year"))
    df_slrMax      <- df_slrMax   %>% mutate(deltaDriver = driverValue_ref - modelUnitValue)
    df_slrMax      <- df_slrMax   %>% filter(deltaDriver >= 0)
    # df_slrMax      <- df_slrMax   %>% filter(modelUnitValue > driverValue_ref)
    slrMaxYears    <- df_slrMax$year %>% unique %>% sort
    #rm("slrMax")
    ### Calculate scaled impacts for values > slrMax and adjust the model value
    df_slrMax      <- df_slrMax   %>% mutate(scaled_impacts = impacts_intercept + impacts_slope * deltaDriver)
    df_slrMax      <- df_slrMax   %>% mutate(model_dot = "Interpolation")
    # df_slrMax %>% names %>% print
    df_slrMax      <- df_slrMax   %>% get_scenario_id(.,include=c("model_dot", "region"))
    # check_max_ids <- df_slrMax$scenario_id; # check_max_ids %>% head %>% print; rm("check_max_ids)

    ###### ** SLR Other Scaled Impacts #######
    ### Get impacts and create scenario ID for values <= slrMax
    df_slrImpacts  <- slrDrivers    %>% filter((modelUnitValue <= slrMax))
    df_slrImpacts  <- df_slrImpacts %>% left_join(slrImpacts, by=c("year"))
    df_slrImpacts  <- df_slrImpacts %>% get_scenario_id(include=c("model_dot", "region"))

    nrow_oth       <- df_slrImpacts %>% nrow
    nrow_oth %>% print
    ###### ** SLR Interpolation ######
    # df_results   <- df_results %>% filter(year >= minYear) %>% filter(year <= maxYear)
    # df_results   <- df_results %>% filter(year > 2090)
  
    if(nrow_oth){
      ### Group by cols
      cols0          <- c("modelType",  "modelUnitValue")
      cols1          <- c("model_type", "driverValue")
      cols2          <- c("lower_model", "upper_model")
      ### Group by cols
      slr_names      <- df_slrImpacts %>% names
      slrGroupByCols <- c("sector", "variant", "impactYear", "impactType", "model", "model_dot", "region", "scenario_id")
      slrGroupByCols <- slrGroupByCols[which(slrGroupByCols %in% slr_names)] %>% c(cols1)
      # rm("slrGroupByCols", "slr_names")
      #### Interpolate driver values
      slrDrivers     <- slrDrivers %>% rename_at(.vars=c(all_of(cols0)), ~cols1)
      slrScenario    <- slrDrivers %>% filter(tolower(model_type)=="slr") %>% select(-c("model_type"))
      slrScenario    <- slrScenario %>% slr_Interp_byYear
      slrScenario    <- slrScenario %>% mutate_at(.vars=c(all_of(cols2)), function(y){gsub(" ", "", y)})
      # df_slrImpacts$model %>% unique %>% print
      # return(slrScenario)
      # df_slrImpacts %>% names %>% print; slrScenario %>% names %>% print
      ### Interpolate
      # df_slrImpacts %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print
      df_slrImpacts  <- df_slrImpacts %>% rename_at(.vars=c(all_of(cols0[2])), ~cols1[2])
      df_slrImpacts  <- df_slrImpacts %>% fredi_slrInterp(slr_x = slrScenario, groupByCols=slrGroupByCols)
      df_slrImpacts %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print
      # "got here" %>% print
      # df_slrImpacts %>% names %>% print
      df_slrImpacts  <- df_slrImpacts %>% rename_at(.vars=c(all_of(cols1[2])), ~cols0[2])
      df_slrImpacts %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print
      df_slrImpacts %>% names %>% print
      #df_slrImpacts %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr.csv")), row.names=F, na="")
      rm("slrGroupByCols", "slr_names", "slrScenario")
      rm("cols0", "cols1")
    }
    rm("nrow_oth")
    df_slrImpacts %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print
    #df_slrImpacts %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr1.csv")), row.names=F, na="")
  

    ###### ** SLR Join Scaled Impacts ######
    ### Add other results back in
    df_slrMax      <- df_slrMax     %>% select(c(all_of(impactSelectCols)))
    df_slrImpacts  <- df_slrImpacts %>% select(c(all_of(impactSelectCols)))
    slrMaxYears   %>% length %>% print
    df_slrMax     %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print
    df_slrImpacts %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print

    df_slrImpacts  <- df_slrImpacts %>% rbind(df_slrMax)
    df_slrImpacts  <- df_slrImpacts %>% arrange_at(.vars=c("scenario_id", "year"))
    df_slrImpacts  <- df_slrImpacts %>% 
      filter(!is.na(scaled_impacts)) %>% 
      mutate(across('scenario_id',str_replace, '(\\d)[0-9]{1,3}cm', '\\Interpolation'))
    df_slrImpacts %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr3.csv")), row.names=F, na="")
    rm("impactSelectCols", "df_slrMax")
    # df_slrImpacts %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print

    ### Bind with other results
    # df_scenarioResults %>% names %>% print; df_slrImpacts$scenario_id %>% head %>% print
    df_scenarioResults  <- df_scenarioResults %>% rbind(df_slrImpacts)
    df_scenarioResults %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr4.csv")), row.names=F, na="")
    rm("df_slrImpacts")

    ###### ** SLR Initial Results #######
    ### Separate initial results out
    # initialResults_slr  <- initialResults_slr %>% mutate(scenario_id = scenario_id)
    initialResults_max  <- initialResults_slr %>% filter( (year %in% slrMaxYears))
    initialResults_max %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr5.csv")), row.names=F, na="")
    initialResults_slr  <- initialResults_slr %>% filter(!(year %in% slrMaxYears))
    initialResults_slr %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr6.csv")), row.names=F, na="")
    ### Add additional columns
    addMaxCols          <- c("model_id", "model_dot", "model_label", "model_underscore")
    initialResults_max[,addMaxCols]  <- "Interpolation"
    rm("addMaxCols")

    ### Add scenarios
    initialResults_max  <- initialResults_max %>% get_scenario_id(include=c("model_dot", "region"))
    initialResults_max %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr7.csv")), row.names=F, na="")
    initialResults_slr  <- initialResults_slr %>% get_scenario_id(include=c("model_dot", "region")) %>%
                                                  mutate(across('scenario_id',str_replace,'slr','slr_Interpolation'))
    initialResults_slr %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr8.csv")), row.names=F, na="")
    # check_inMax_ids <- initialResults_max$scenario_id; check_inSlr_ids <- initialResults_slr$scenario_id;
    # check_inMax_ids %>% head %>% print; check_inSlr_ids %>% head %>% print

    ### Join with model
    initialResults_max  <- initialResults_max %>% left_join(co_modelTypes, by="modelType")
    initialResults_max %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr9.csv")), row.names=F, na="")
    initialResults_slr  <- initialResults_slr %>% left_join(co_models, by="modelType") %>%
                                                  mutate(model_id =	"Interpolation",
                                                         model_dot =	"Interpolation",
                                                         model_underscore =	"Interpolation",
                                                         model_label = "Interpolation") %>% distinct()
                                                           
    
    initialResults_slr %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr10.csv")), row.names=F, na="")
    ### Add scenarios and bind results
    initialResults_slr  <- initialResults_slr %>% rbind(initialResults_max)
    initialResults_slr %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr11.csv")), row.names=F, na="")
    # # df_impacts %>% names %>% print
    # # initialResults_slr$scenario_id %>% head %>% print
    # initialResults_slr %>% filter(!is.na(econScalarValue)) %>% nrow %>% print
    rm("initialResults_max")

    ###### ** SLR Bind Initial Results #######
    ### Arrange and bind with other results
    initialResults_slr  <- initialResults_slr %>% select(-c("model_type"))
    initialResults_slr  <- initialResults_slr %>% arrange_at(.vars=c("scenario_id", "year"))
    initialResults_slr %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr12.csv")), row.names=F, na="")
    # names1 <- initialResults_slr %>% names; names2 <- initialResults %>% names; # names1[!(names1 %in% names2)] %>% print
    # initialResults_slr %>% names %>% print; initialResults %>% names %>% print
    initialResults      <- initialResults %>% rbind(initialResults_slr)
    initialResults_slr %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr13.csv")), row.names=F, na="")
    rm("initialResults_slr")
  }

  ###### Calculate Impacts  ######
  ### Join results with initialized results and update missing observations with NA
  ### Remove intermediate values
  # initialResults %>% names %>% print; df_scenarioResults %>% names %>% print
  df_impacts <- initialResults %>% left_join(df_scenarioResults, by=c("scenario_id", "year"));
  rm("initialResults")
  if(msgUser) message("\t", messages_tempBin[["scaledImpacts"]]$success)
  
  df_impacts %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr14.csv")), row.names=F, na="")
  # df_impacts %>% names %>% print
  # initialResults$modelUnit_label %>% unique %>% print
  # df_impacts %>% filter(modelUnit_label=="cm") %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print
  # (df_impacts %>% filter(modelUnit_label=="cm"))$year %>% range %>% print

  ### Physical impacts = physScalar * scaled_impacts
  ### Annual impacts = phys-econ scalar value by the scaled impacts
  df_impacts <- df_impacts %>% mutate(hasPhysImpacts   = 1 * !is.na(physicalmeasure))
  df_impacts <- df_impacts %>% mutate(physical_impacts = hasPhysImpacts * scaled_impacts * physScalar)
  df_impacts <- df_impacts %>% mutate(annual_impacts   = hasPhysImpacts * scaled_impacts * physEconScalar)
  df_impacts <- df_impacts %>% select(-c("hasPhysImpacts")) #%>% as.data.frame

  ###### Add Scenario Information ######
  ### Add in model info
  message("Formatting results", "...")
  df_impacts <- df_impacts %>% filter(year>=minYear) %>% rename(model_type = modelType)
  df_drivers <- df_drivers %>% filter(year>=minYear) %>% rename(model_type = modelType)
  df_results <- df_impacts %>% left_join(df_drivers, by=c("year", "model_type"))
  df_impacts %>% write.csv(file = file.path(saveOutputsPath, paste0(today, "_qcslr15.csv")), row.names=F, na="")
  rm("df_impacts")
  # (df_results %>% filter(modelUnit_label=="cm"))$year %>% range %>% print
  # df_results %>% filter(modelUnit_label=="cm") %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print

  ###### Format Outputs ######
  ### Refactor sectors, variants, impactTypes
  co_variants    <- co_variants    %>% mutate(sector_variant    = paste(sector_id, variant_id, sep="_"))
  co_impactTypes <- co_impactTypes %>% mutate(sector_impactType = paste(sector_id, impactType_id, sep="_"))

  #### Rename Sector Columns
  df_results <- df_results %>% rename(sector_id = sector, sector = sector_label)
  #### Regions
  # df_results %>% names %>% print
  reg_lvls   <- co_regions$region_dot
  reg_lbls   <- co_regions$region_label
  df_results <- df_results %>% rename(region_id = region)
  df_results <- df_results %>% mutate(region    = region_id %>% factor(reg_lvls, reg_lbls))
  rm("reg_lvls", "reg_lbls")

  ### Model types and models
  modelCols0 <- c("model_label", "modelType_label", "modelUnitValue", "modelUnit_label", "modelUnitDesc")
  modelCols1 <- c("model"      , "model_type"     , "driverValue"   , "driverUnit",      "driverType")
  modelCols2 <- c("model_id", "model_dot", "model_underscore", "modelUnit_id")
  modelCols3 <- c("modelRefYear", "modelMaxOutput", "modelUnitScale", "modelMaxExtrap")
  df_results <- df_results %>% select(-c("model_type"))
  df_results <- df_results %>% rename_at(.vars=c(all_of(modelCols0)), ~modelCols1)
  df_results <- df_results %>% select(-c(all_of(modelCols2), all_of(modelCols3)))
  rm("modelCols0", "modelCols1", "modelCols2", "modelCols3")
  # df_results %>% names %>% print
  # (df_results %>% filter(driverUnit=="cm"))$year %>% range %>% print

  ### Variant labels
  var_lvls   <- co_variants$sector_variant
  var_lbls   <- co_variants$variant_label
  df_results <- df_results %>% mutate(sect_var = sector_id %>% paste(variant, sep="_"))
  df_results <- df_results %>% mutate(variant  = sect_var %>% factor(var_lvls, var_lbls))
  df_results <- df_results %>% select(-c("sect_var"))
  rm("var_lvls", "var_lbls")
  # (df_results %>% filter(driverUnit=="cm"))$year %>% range %>% print

  ### Impact types
  imp_lvls   <- co_impactTypes$sector_impactType
  imp_lbls   <- co_impactTypes$impactType_label
  df_results <- df_results %>% mutate(sect_imp   = sector_id %>% paste(impactType, sep="_"))
  df_results <- df_results %>% mutate(impactType = sect_imp %>% factor(imp_lvls, imp_lbls))
  df_results <- df_results %>% select(-c("sect_imp", "sector_id"))
  rm("imp_lvls", "imp_lbls")
  # (df_results %>% filter(driverUnit=="cm"))$year %>% range %>% print

  ###### Columns ######
  ### Scalar column names, Sector info names, Scenario names
  cGroupByCols0    <- c("sector", "variant", "impactYear", "impactType", "model_type", "model", "region")
  cAddCols0        <- c("sectorprimary", "includeaggregate")
  cSectorInfoNames <- c("modelUnitType", "c0", "c1", "exp0", "year0")
  cScenarioNames   <- c("scenario_id")
  cScalarNames     <- c("physScalarName", "physAdjName", "damageAdjName", "physScalar") %>%
    c("physScalarValue", "physAdjValue"       , "damageAdjValue") %>%
    c("econScalarName" , "econMultiplierName" , "econScalar") %>%
    c("econScalarValue", "econMultiplierValue", "econAdjValue", "econMultiplier") %>%
    c("physEconScalar" , "scaled_impacts")
  selectCols0      <- c(cScenarioNames, cScalarNames, cSectorInfoNames)

  ### Convert to character and drop sector id
  df_results <- df_results %>% mutate_at(.vars = c(all_of(cGroupByCols0)), as.character)
  df_results <- df_results %>% filter(!is.na(sector))
  # (df_results %>% filter(driverUnit=="cm"))$year %>% range %>% print

  ###### Testing ######
  if(!testing) {df_results <- df_results %>% select(-c(all_of(selectCols0)))}


  ###### Aggregation ######
  ### For regular use (i.e., not impactYears), simplify the data:
  if(requiresAgg){
    ### Aggregation types
    aggGroupByCols <- cGroupByCols0
    includeAggCol  <- c("includeaggregate")
    if( primaryTypes){df_results     <- df_results %>% filter(includeaggregate==1) %>% select(-c(all_of(includeAggCol)))}
    if(!primaryTypes){aggGroupByCols <- aggGroupByCols %>% c(includeAggCol)}
    ### If the user specifies primary type, filter to primary types and variants and drop that column
    df_results <- df_results %>% as.data.frame %>% aggregate_impacts(aggLevels = aggLevels, groupByCols = aggGroupByCols)
    rm("aggGroupByCols")
  }
  # df_results %>% names %>% print

  ###### Order the Output ######
  ### Convert levels to character
  ### Order the rows, then order the columns
  resultNames   <- df_results %>% names
  groupByCols   <- c("sector",  "variant", "impactYear", "impactType", "region", "model_type", "model", "year")
  driverCols    <- c("driverValue", "driverUnit", "driverType")
  nonGroupCols  <- resultNames[which(!(resultNames %in% c(groupByCols, driverCols)))]
  orderColIndex <- which(names(data) %in% groupByCols)
  selectCols    <- c(groupByCols, driverCols, nonGroupCols) %>% (function(x){x[x!="annual_impacts"] %>% c("annual_impacts")})

  df_results    <- df_results %>% select(c(all_of(selectCols))) %>% arrange_at(.vars=c(all_of(groupByCols)))

  # c_aggColumns <- c("sectorprimary", "includeaggregate") %>% (function(y){y[which(y %in% names(df_results))]})
  # if(length(c_aggColumns)>0){df_results <- df_results %>% mutate_at(.vars=c(all_of(c_aggColumns)), as.numeric)}

  ###### Present Values ######
  if(pv){
    ### Discount rate info
    df_rates   <- data.frame(year = list_years, rate = rate, baseYear = baseYear)
    df_rates   <- df_rates %>% mutate(discountFactor = 1 / (1 + rate)^(year - baseYear))
    ### Discounted impacts
    df_results <- df_results %>% left_join(df_rates, by = c("year"))
    df_results <- df_results %>% mutate(discounted_impacts = annual_impacts * discountFactor)
  }

  ###### Format Dataframe ######
  df_results   <- df_results %>% ungroup %>% as.data.frame
  # df_results %>% filter(driverUnit=="cm") %>% filter(!is.na(annual_impacts)) %>% nrow %>% print
  ###### Return Object ######
  message("\n", "Finished", ".")
  return(df_results)

} ### End function








