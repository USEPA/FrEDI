###### Documentation ######
#' Project annual average climate change impacts throughout the 21st century for available sectors
#'
#'
#'
#' @description
#' This function allows users to project annual average climate change impacts through 2090 (2010-2090) for available sectors (see [FrEDI::get_sectorInfo()]), with the option to extend results to 2300 (2010-2300). Users may specify custom temperature, U.S. population, and GDP scenarios. The output is an R data frame object containing annual average impacts, by year, for each sector, variant, impact type, model (GCM or SLR scenario), and region.
#'
#'
#'
#' @param inputsList A list of named elements named elements (`names(inputsList)= c("tempInput", "slrInput", "gdpInput", "popInput")`), each containing data frames of custom temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and/or population scenarios, respectively, over a continuous period in the range 2010 to 2300. Temperature and sea level rise inputs should start in 2000 or earlier. Values for population and GDP scenarios can start in 2010 or earlier. Values for each scenario type must be within reasonable ranges. For more information, see [FrEDI::import_inputs()].
#' @param sectorList A character vector indicating a selection of sectors for which to calculate results (see [FrEDI::get_sectorInfo()]). If `NULL`, all sectors are included.
#' @param aggLevels Levels of aggregation at which to summarize data: one or more of `c("national"`, `"modelaverage"`, `"impactyear"`, `"impacttype"`, `"all"`, `"none")`. Defaults to all levels (i.e., `aggLevels="all"`). Uses the same aggregation levels as [FrEDI::aggregate_impacts()].
#' @param elasticity=0.4 A numeric value indicating an elasticity to use for adjusting VSL for applicable sectors and impacts (defaults to `elasticity=0.4`). Applicable sectors and impacts are Air Quality (all impact types), ATS Extreme Temperature, CIL Extreme Temperature, Extreme Temperature (all impact types), Mental Health, Southwest Dust (All Mortality), Valley Fever (Mortality), Vibrio, and Wildfire (Mortality).
#' @param maxYear=2090 A numeric value indicating the maximum year for the analysis.
#' @param thru2300 A ` TRUE/FALSE` shortcut that overrides the maxYear argument to run the model to 2300.
#' @param outputList A ` TRUE/FALSE` value indicating whether to output results as a data frame object (`outputList=FALSE`, default) or to return a list of objects (`outputList=TRUE`) that includes information about model provenance (including input arguments and input scenarios) along with the data frame of results.
#' @param byState A `TRUE/FALSE` value indicating whether to run at the state level for available sectors (default=`FALSE`).
#' @param silent A `TRUE/FALSE` value indicating the level of messaging desired by the user (default=`TRUE`).
#'
#'
#'
#' @details This function allows users to project annual average climate change impacts through 2300 (2010-2300) for available sectors. [FrEDI::run_fredi()] is the main function in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information).
#'
#' Users can specify an optional list of custom scenarios with `inputsList` (for more information on the format of inputs, see [FrEDI::import_inputs()]). [FrEDI::run_fredi()] uses default scenarios for temperature, population, and GDP when no inputs are specified (i.e., `inputsList=NULL`) or for empty elements of the inputs list. If the user does not specify an input scenario for GMSL (i.e., `inputsList=list(slrInput=NULL)`, [FrEDI::run_fredi()] first converts the CONUS temperature scenario to global temperatures and then converts the global temperatures to a global mean sea level rise (GMSL) height in centimeters. For more information on the conversion of CONUS temperatures to global temperatures, see [FrEDI::convertTemps()]. For more information on the conversion of global temperatures to GMSL, see [FrEDI::temps2slr()].
#'
#' Temperature and GMSL inputs must begin in 2000 or earlier, whereas values for population and GDP scenarios can start in 2010 or earlier. Values for input scenarios must be within reasonable ranges (for instance, negative values for population and GDP are non-sensical). If a user inputs a custom scenario with values outside the allowable ranges, [FrEDI::run_fredi()] will not run the scenarios and will instead stop and return an error message. For more information, see [FrEDI::import_inputs()].
#'
#' * The input temperature scenario (passed to [FrEDI::run_fredi()] via the `inputsList` argument) requires temperatures for CONUS in degrees Celsius relative to 1995 (degrees of warming relative to the baseline). Temperature values must be greater than or equal to zero degrees Celsius (CONUS temperatures). Users can convert global temperatures to CONUS temperatures using `FrEDI::convertTemps(from="global")` or by specifying `FrEDI::import_inputs(temptype="global")` when importing a temperature scenario from a CSV file.
#' * Values for the sea level rise (SLR) scenario are for global mean sea level rise (GMSL) must be in centimeters (cm) and values must be greater than or equal to zero and less than or equal to 250 cm.
#' * Population and gross domestic product (GDP) values must be greater than or equal to zero.
#'
#' If `inputsList=NULL`, [FrEDI::run_fredi()] uses defaults for all scenarios. Otherwise, [FrEDI::run_fredi()] looks for a list object passed to the argument `inputsList`. Within that list, [FrEDI::run_fredi()] looks for list elements `tempInput`, `slrInput`, `gdpInput`, and `popInput` containing data frames with custom scenarios for temperature, GMSL, GDP, and regional population, respectively. [FrEDI::run_fredi()] will default back to the default scenarios for any list elements that are `NULL` or missing. In other words, running `run_fredi(inputsList=list())` returns the same outputs as running [FrEDI::run_fredi()]. For help importing custom scenarios from CSV files, refer to the pre-processing function [FrEDI::import_inputs()].
#'
#' [FrEDI::run_fredi()] linearly interpolates missing annual values for all input scenarios using non-missing values (requires at least two non-missing values). Temperatures are interpolated using 1995 as the baseline year (i.e., the central year of the 1986-2005 baseline). In other words, the temperature (in degrees Celsius) is set to zero for the year 1995 and GMSL is set to zero for the year 2000. The interpolated temperature and GMSL scenarios are combined into a column called `driverValue`, along with additional columns for year, the driver unit (column `"driverUnit"`, with `driverUnit="degrees Celsius"` and `driverUnit="cm"` for temperature- and SLR-driven sectors, respectively), and the associated model type (column `"model_type"`, with `model_type="GCM"` and `model_type="SLR"` for temperature- and SLR-driven sectors, respectively).
#'
#' The population scenario must provide annual regional values for population, with national totals calculated from regional values. [FrEDI] uses the national population scenario and the GDP scenario to calculate GDP per capita. Values for regional population, national population, national GDP (in 2015$), and national per capita GDP (in 2015$/capita) are provided in the results data frame in columns `"reg_pop"`, `"national_pop"`, `"gdp_usd"`, and `"gdp_percap"`, respectively.
#'
#' By default, [FrEDI::run_fredi()] will calculate impacts for all sectors included in the tool. Alternatively, users can pass a character vector specifying a single sector or a subset of sectors using the `sectorList` argument. To see a list of sectors included within [FrEDI], run [FrEDI::get_sectorInfo()]. If `sectorList=NULL` (default), all sectors are included.
#'
#' [FrEDI::run_fredi()] aggregates or summarizes results to level(s) of aggregation specified by the user (passed to `aggLevels`) using the post-processing helper function [FrEDI::aggregate_impacts()]. Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels="national"`), averaging across model types and models (`aggLevels="modelaverage"`), summing over all impact types (`aggLevels="impacttype"`), and interpolating between impact year estimates (`aggLevels="impactYear"`). Users can specify all aggregation levels at once by specifying `aggLevels="all"` (default) or no aggregation levels (`aggLevels="none"`).
#'
#' For each of the `aggLevels`, [FrEDI::run_fredi()] performs the following calculations using [FrEDI::aggregate_impacts()] (note that the `"variant"` column referred to below contains information about the variant (or adaptation) name or `“N/A”`, as applicable):
#'
#' \tabular{ll}{
#' \strong{Aggregation Level} \tab \strong{Description} \cr
#' `national` \tab Annual values are summed across all regions present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`,  `"impactYear"`, `"model_type"`, `"model"`, and `"year"`) and summed across regions. Years which have missing column data for all regions return as `NA`. The rows of the data frame of national values (with column `region="National Total"`) are then added as rows to the results. \cr
#' `modelaverage` \tab For temperature-driven sectors, annual results are averaged across all GCM models present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"model_type"`, `"region"`, and `"year"` and averaged across models (SLR impacts are estimated as an interpolation between SLR scenarios). Averages exclude missing values. Years which have missing column data for all models return as `NA`. The rows of model averages (with column `model="Average"` are then added as rows to the results data frame. \cr
#' `impacttype` \tab Annual results are summed across all impact types by sector present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`,`"model_type"`, `"model"`, `"region"`, and `"year"` and summed across impact types. Mutates column `impactType="all"` for all values. Years which have missing column data for all impact types return as `NA`. If results are aggregated across impact types, information about physical impacts (columns `"physicalmeasure"` and `"physical_impacts"`) are dropped.\cr
#' `impactyear` \tab Annual results for sectors with only one impact year estimate (i.e., `impactYear = "N/A"`) are separated from those with multiple impact year estimates. Sectors with multiple impact years have separate results for impact years 2010 and 2090. For these sectors, annual results are linearly interpolated between impact year estimates. For any model run years above 2090, annual results for sectors with multiple impact years return the 2090 estimate. The interpolated values are bound back to the results for sectors with a single impact year estimate, and column `impactYear` set to `impactYear="Interpolation"` for all values. \cr
#' }
#'
#' Annual impacts for each sector, variant, impact type, and impact year combination included in the model are calculated by multiplying scaled climate impacts by a physical scalar and economic scalars and multipliers. Some sectors use Value of a Statistical Life (VSL) to adjust the value non-linearly over time. [FrEDI::run_fredi()] uses a default value of `elasticity=0.4`to adjust VSL for applicable sectors and impacts. Applicable sectors and impacts are Air Quality (all impact types), ATS Extreme Temperature, CIL Extreme Temperature, Extreme Temperature (all impact types), Mental Health, Southwest Dust (All Mortality), Valley Fever (Mortality), Vibrio, and Wildfire (Mortality). A custom elasticity can be passed to the `elasticity` argument; to keep VSL constant over time, specify `elasticity=1`.
#'
#' By default, [FrEDI::run_fredi()] calculates impacts starting in the year 2010 and ending in 2090. Specify an alternative end year for the analysis using the `maxYear` argument (defaults to `maxYear=2090`). The minimum and maximum valid values for `maxYear` are `maxYear=2011` and `maxYear=2300`, respectively. Alternatively, run the model through the year 2300 by specifying `thru2300=TRUE` (this will override the `maxYear` argument and set `maxYear=2300`). Note that the default scenarios included within [FrEDI] stop in the year 2090; to get non-zero/non-missing values for years after 2090, users must specify a `maxYear` after 2090 and also provide custom input scenarios out to the desired end year.
#'
#'
#' [FrEDI::run_fredi()] defaults to returning a data frame of annual average impacts over the analysis period, for each sector, variant, model (GCM or SLR scenario), impact type, impact year, and region (`outputList=FALSE`). If `outputList=TRUE`, [FrEDI::run_fredi()] returns a list object containing information about values for function arguments and driver scenarios in addition to the data frame of impacts
#'
#'
#'
#' @return
#' If `outputList=FALSE`, the output of [FrEDI::run_fredi()] is a data frame object containing annual average impacts over the analysis period, for each sector, variant, model (GCM or SLR scenario), impact type, impact year, and region. If `outputList=TRUE`, [FrEDI::run_fredi()] returns a list object containing the following:
#'
#' * `arguments`, containing a list with values for the arguments passed to [FrEDI::run_fredi()] (with the exception of scenarios passed to the `inputsList` argument, which are provided in the `driverScenarios` list element)
#' * `driverScenarios`, a list object containing elements with the driver scenarios for temperature, SLR, population, and GDP used in the model
#' * `results`, containing a data frame of annual average impacts
#'
#'
#'
#' @examples
#' ### Load FrEDI
#' require(FrEDI)
#'
#' ### Run function with defaults (same as `defaultResults` dataset)
#' run1 <- run_fredi()
#'
#' ### Load climate scenarios and glimpse data
#' data("gcamScenarios")
#' gcamScenarios |> glimpse()
#'
#' ### Load population scenario and glimpse data
#' data(popScenario)
#' popScenario |> glimpse()
#'
#' ### Subset climate scenario
#' temps1 <- gcamScenarios |> filter(scenario=="ECS_3.0_ref_0") |> select(year, temp_C)
#'
#' ### Run custom scenario
#' run2 <- run_fredi(inputsList=list(tempInput=temps1, popInput=popScenario))
#'
#' ### Load scenarios from file:
#' scenariosPath <- system.file(package="FrEDI") |> file.path("extdata","scenarios")
#' scenariosPath |> list.files()
#'
#' ### Temperature Scenario File Name
#' tempInputFile <- scenariosPath |> file.path("GCAM_scenario.csv")
#'
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath |> file.path("slr_from_GCAM.csv")
#'
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath |> file.path("pop_scenario.csv")
#'
#' ### Import inputs
#' x_inputs <- import_inputs(
#'   tempfile = tempInputFile,
#'   slrfile  = slrInputFile,
#'   popfile  = popInputFile
#' )
#'
#' ### Run custom scenarios
#' run3 <- run_fredi(inputsList=x_inputs)
#'
#' ### Get information on sectors:
#' get_sectorInfo()
#'
#' ### Run for a single sector, with default inputs, no aggregation, and elasticity=1:
#' run4 <- run_fredi(sectorList="ATS Extreme Temperature", aggLevels="none", elasticity=1)
#'
#' ### Set end year for analysis to 2100 using default scenarios (values after 2090 will all be missing, since default scenarios only have values out to 2090)
#' run5 <- run_fredi(maxYear=2100)
#'
#' ### Set end year for analysis to 2300 using default scenarios (values after 2090 will all be missing, since default scenarios only have values out to 2090)
#' run6 <- run_fredi(thru2300=TRUE)
#'
#'
#'
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'
#' @export
#' @md
#'
#'
#'
#'
###### run_fredi ######
### This function creates a data frame of sector impacts for default values or scenario inputs.
### run_fredi relies on the following helper functions: "interpolate_annual", "match_scalarValues","get_econAdjValues" , "calcScalars", "interpolate_tempBin"
run_fredi <- function(
    inputsList = list(tempInput=NULL, slrInput=NULL, gdpInput=NULL, popInput=NULL), ### List of inputs
    sectorList = NULL, ### Vector of sectors to get results for
    aggLevels  = c("national", "modelaverage", "impactyear", "impacttype"), ### Aggregation levels
    # elasticity = 0.4, ### Override value for elasticity for economic values
    elasticity = NULL, ### Override value for elasticity for economic values
    maxYear    = 2090,
    thru2300   = FALSE,
    outputList = FALSE, ### Whether to return input arguments as well as results. [If TRUE], returns a list instead of a data frame
    # byState    = FALSE, ### Whether to run at state level for available sectors
    allCols    = FALSE, ### Whether to include additional columns in output
    silent     = TRUE   ### Whether to message the user
){
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user)
  silent    <- silent |> is.null() |> ifelse(T, silent)
  msgUser   <- !silent
  ### Uncomment for allCols
  doPrimary <- F  ### whether to filter to primary impacts
  ### Model years and NPD (FrEDI past 2090)
  refYear   <- 2090
  npdYear   <- 2300
  maxYear   <- maxYear  |> is.null() |> ifelse(refYear, maxYear)
  maxYear0  <- thru2300 |> ifelse(npdYear, maxYear)
  do_npd    <- maxYear0 > refYear

  ###### Return List ######
  ### Initialize list to return
  returnList <- list()
  argsList   <- list()

  ###### Create file paths ######
  ### Assign data objects to objects in this namespace
  for(list_i in rDataList){
    ### Assign list elements to name space
    name_i <- list_i[["name"]]
    data_i <- list_i[["data"]]
    ### Assign list elements to name space
    name_i |> assign(data_i)
    ### Assign elements of list i to name space
    names_i <- data_i |> names()
    for(name_j in names_i){name_j |> assign(data_i[[name_j]]); rm(name_j)}
    rm(list_i, name_i, data_i)
  } ### End for(i in rDataList)
  ### Assign FrEDI config
  for(name_i in fredi_config |> names()){assign(name_i, fredi_config[[name_i]]); rm(name_i)}

  ### Years
  maxYear    <- maxYear0
  list_years <- minYear:maxYear
  # maxYear    |> print(); list_years |> max() |> print()

  ###### Aggregation level  ######
  ### Types of summarization to do: default
  doPrimary <- doPrimary |> is.null() |> ifelse(FALSE, doPrimary)
  aggList0  <- c("national", "modelaverage", "impactyear", "impacttype")
  if(aggLevels |> is.null()){
    aggLevels <- aggList0
  } else{
    ### Aggregation levels
    aggLevels    <- aggLevels |> tolower()
    aggLevels    <- aggLevels[which(aggLevels %in% c(aggList0, "all", "none"))]
    doAgg        <- "none" %in% aggLevels
    ### If none specified, no aggregation (only SLR interpolation)
    ### Otherwise, aggregation depends on length of agg levels
    if     ("none" %in% aggLevels) {aggLevels   <- c()}
    else if("all"  %in% aggLevels) {aggLevels   <- aggList0}
    else                           {requiresAgg <- aggLevels |> length() > 0}
  } ### End else(!is.null(aggLevels))
  requiresAgg <- aggLevels |> length() > 0

  ###### Sectors List ######
  ### Sector names & labels
  sectorIds    <- co_sectors[["sector_id"   ]]
  sectorLbls   <- co_sectors[["sector_label"]]
  ### Initialize sector list if the sectors list is null
  if(sectorList |> is.null()){
    sectorList    <- sectorIds
  } else{
    # sectorIds |> print()
    ### Get lower case versions
    sectors0   <- sectorIds  |> tolower()
    sectors1   <- sectorLbls |> tolower()
    sectors2   <- sectorList |> tolower()
    # sectors0 |> print(); sectors1 |> print(); sectors2 |> print()
    ### Compare inputs to names and labels in the data
    ### Subset to sector list in sector names
    which0     <- (sectors0 %in% sectors2) | (sectors1 %in% sectors2)
    sectorIds  <- sectorIds [which0]
    sectorList <- sectorLbls[which0]
    ### Message users about missing sectors
    which1     <- (sectors2 %in% sectors0) | (sectors2 %in% sectors1)
    na_sectors <- sectorList[!which1]
    rm(sectors0, sectors1, sectors2, which0, which1)
    ### Message the user
    if(na_sectors |> length() >= 1){
      paste0("Warning! Error in `sectorList`:") |> message()
      "\n\t" |> paste0(
        "Impacts are not available for the following sectors: '",
        na_sectors |> paste(collapse= "', '"),
        "'..."
      ) |> message()
      "\n\t" |> paste0(
        "Available sectors: '",
        sectorLbls |> paste(collapse= "', '"),
        "'."
      ) |> message() ### End message
    } ### End if(length(missing_sectors)>=1)
  } ### End else(is.null(sectorList))
  ### Number of sectors
  # sectorList |> print()
  num_sectors  <- sectorList |> length()

  ### Filter to sectors
  co_slrScalars  <- co_slrScalars  |> filter(sector %in% sectorIds)
  df_sectorsInfo <- df_sectorsInfo |> filter(sector %in% sectorIds)

  ###### By State ######
  byState <- TRUE
  popCol0 <- byState |> ifelse("state_pop", "reg_pop")
  if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}

  ###### Inputs ######
  ###### ** Load Inputs ######
  ### Create logicals and initialize inputs list
  list_inputs     <- co_inputScenarioInfo[["inputName"]]
  num_inputNames  <- co_inputScenarioInfo |> nrow()
  if(inputsList |> is.null()) {inputsList <- list()} else{"Checking input values..." |> message()}
  ### Iterate over the input list
  # if(!is.null(inputsList)){
  ### Assign inputs to objects
  for(i in 1:num_inputNames){
    inputInfo_i <- co_inputScenarioInfo[i,]
    ### Input name and label
    input_i     <- inputInfo_i$inputName |> unique()
    msgName_i   <- inputInfo_i$inputType |> unique()
    ### Input run_fredi argument
    inputName_i <- inputInfo_i$tempBinListName |> unique()
    ### Min and Max Values
    min_i       <- inputInfo_i$inputMin |> unique()
    max_i       <- inputInfo_i$inputMax |> unique()
    ### Column Info
    region_i    <- inputInfo_i$region |> unique()
    valueCol_i  <- inputInfo_i$valueCol |> unique()
    if(region_i == 1){regCol_i <- c("region")} else{regCol_i <- c()}
    ### Initialize column names
    colNames_i  <- "year" |> c(regCol_i) |> c(valueCol_i) #; print(colNames_i)
    numCols_i   <- colNames_i |> length()
    has_i        <- paste0("has_", input_i, "Update")
    # has_update_i <- is.null(inputsList[[inputName_i]])
    df_input_i   <- inputsList[[inputName_i]]
    has_update_i <- !(df_input_i |> is.null())
    ### Assign inputs to objects
    has_i       |> assign(has_update_i)
    inputName_i |> assign(df_input_i  )
  } ### End iterate over inputs
  # }

  ### Message user about elasticity
  if(!(elasticity |> is.null()) & !(elasticity |> is.numeric())){
    paste0("\t", "Incorrect value type provided for argument 'elasticity'...") |> message()
    paste0("\t\t", "Using default elasticity values.") |> message()
  } ### End if


  ### Update arguments list
  argsList[["sectorList"]] <- sectorList
  argsList[["aggLevels" ]] <- aggLevels
  argsList[["elasticity"]] <- elasticity
  argsList[["maxYear"   ]] <- maxYear
  argsList[["thru2300"  ]] <- thru2300
  ### Add to return list and remove intermediate arguments
  returnList[["arguments"]] <- argsList
  rm("argsList")

  ###### ** Temperature Scenario ######
  ### User inputs: temperatures have already been converted to CONUS temperatures. Filter to desired range.
  ### Name the reference year temperature
  ### Add the point where impacts are zero
  refYear_temp <- (co_modelTypes |> filter(modelUnitType=="temperature"))$modelRefYear |> unique()
  # co_modelTypes |> names() |> print()

  ### If no user input (default): Use temperature scenario for one region
  if(has_tempUpdate){
    message("Creating temperature scenario from user inputs...")
    ### Select appropriate columns
    ### Remove missing values of years, temperatures
    ### Zero out series at the temperature reference year
    tempInput <- tempInput |> select(c("year", "temp_C"))
    tempInput <- tempInput |> filter(!(temp_C |> is.na()) & !(year |> is.na()))
    tempInput <- tempInput |> filter(year > refYear_temp)
    tempInput <- tibble(year= refYear_temp, temp_C = 0) |> rbind(tempInput)

    ### Interpolate annual values and then drop region
    temp_df   <- tempInput |> (function(x){
      minYear_x <- x$year |> min()
      interpYrs <- refYear_temp:maxYear
      ### Interpolate
      x_interp  <- x |> interpolate_annual(
        years  = interpYrs,
        column = "temp_C",
        rule   = 1:2
      ) |> select(-c("region"))
      return(x_interp)
    })()
    temp_df  <- temp_df |> rename(temp_C_conus  = temp_C)
    temp_df  <- temp_df |> mutate(temp_C_global = temp_C_conus |> convertTemps(from="conus"))
    # rm("tempInput")
  } ### End if(has_tempUpdate)
  ### Load default temperature scenario
  else{
    message("No temperature scenario provided...using default temperature scenario...")
    tempInput <- co_defaultTemps
    temp_df   <- tempInput
  } ### End else(has_tempUpdate)
  # temp_df |> nrow() |> print()

  ###### ** SLR Scenario ######
  ### Year where SLR impacts are zero
  refYear_slr <- (co_modelTypes |> filter(modelUnitType=="slr"))$modelRefYear |> unique()
  # co_modelTypes |> names() |> print()

  ### Follow similar procedure to temperatures
  ### Select appropriate columns
  ### Select out NA values and filter to appropriate years
  ### Zero out series at the temperature reference year
  if(has_slrUpdate){
    message("Creating SLR scenario from user inputs...")
    slrInput  <- slrInput |> select(c("year", "slr_cm"))
    slrInput  <- slrInput |> filter(!(slr_cm |> is.na()) & !(year |> is.na()))
    slrInput  <- slrInput |> filter(year >  refYear_slr)
    slrInput  <- tibble(year= refYear_slr, slr_cm = 0) |> rbind(slrInput)
    ### Interpolate values
    slr_df    <- slrInput |> (function(x){
      minYear_x <- x$year |> min()
      interpYrs <- refYear_slr:maxYear
      ### Interpolate annual values
      x_interp  <- x |> interpolate_annual(#wm same as temps above, I think fine to leave
        years  = interpYrs,
        column = "slr_cm",
        rule   = 1:2
      ) |> select(-c("region"))
      return(x_interp)
    })()
    # rm("slrInput")
  }  ### else(has_slrUpdate)
  ### If there is no SLR scenario, calculate from temperatures
  ### First convert temperatures to global temperatures
  ### Then convert global temps to SLR
  else{
    message("Creating SLR scenario from temperature scenario...")
    # slr_df <- temp_df %>% (function(x){temps2slr(temps = x$temp_C_global, years = x$year)})
    slr_df <- temps2slr(temps = temp_df$temp_C_global, years = temp_df$year)
  } ### End else(has_slrUpdate)
  # slr_df |> nrow() |> print()
  # slr_df |> head() |> print()
  # slr_df$year |> range() |> print()

  ###### ** Driver Scenario  ######
  ### Select columns
  temp_df <- temp_df |> select(c("year", "temp_C_conus"))
  slr_df  <- slr_df  |> select(c("year", "slr_cm"))
  ### Rename columns
  temp_df <- temp_df |> rename(modelUnitValue = temp_C_conus)
  slr_df  <- slr_df  |> rename(modelUnitValue = slr_cm      )
  ### Add model type
  temp_df <- temp_df |> mutate(modelType="gcm")
  slr_df  <- slr_df  |> mutate(modelType="slr")
  ###### Combine Scenarios and bind with the model type info
  ### R Bind the SLR values
  ### Join with info about models
  ### Filter to the years used by the R tool
  co_modelTypes <- co_modelTypes |> rename(modelType = modelType_id)
  co_modelType0 <- co_modelTypes |> select(c("modelType"))
  df_drivers    <- temp_df       |> rbind(slr_df)
  df_drivers    <- df_drivers    |> filter(year >= minYear)
  df_drivers    <- df_drivers    |> filter(year <= maxYear)
  # df_drivers |> names() |> print(); co_modelType0 |> names() |> print()
  df_drivers    <- df_drivers    |> left_join(co_modelType0, by="modelType")
  ### Update inputs in outputs list
  returnList[["driverScenarios"]][["temp"]] <- temp_df
  returnList[["driverScenarios"]][["slr" ]] <- slr_df
  ### Remove intermediate values
  rm("co_modelType0", "temp_df", "slr_df")

  ###### ** Socioeconomic Scenario ######
  ### Update the socioeconomic scenario with any GDP or Population inputs and message the user
  ### Reformat GDP inputs if provided, or use defaults
  gdpCols0 <- c("year", "gdp_usd")
  popCols0 <- c("year", "region") |> c(stateCols0, popCol0)
  if(has_gdpUpdate){
    message("Creating GDP scenario from user inputs...")
    gdp_df  <- gdpInput |> filter(!(is.na(gdp_usd)) & !(year |> is.na()))
    gdp_df  <- gdp_df   |> interpolate_annual(years=list_years, column="gdp_usd", rule = 2) |> select(-c("region"))
    rm("gdpInput")
  } ### End if(has_gdpUpdate)
  else{
    message("No GDP scenario provided...Using default GDP scenario...")
    gdp_df  <- gdp_default |> select(all_of(gdpCols0))
    rm("gdp_default")
  } ### End else(has_gdpUpdate)

  ### Population inputs
  if(has_popUpdate){
    message("Creating Population scenario from user inputs...")
    ### Standardize region and then interpolate
    pop_df       <- popInput |> mutate(region = gsub(" ", ".", region))
    pop_df       <- pop_df   |> interpolate_annual(years=list_years, column=popCol0, rule=2, byState=byState) |> ungroup()
    # pop_df |> glimpse()
    ### Calculate national population
    national_pop <- pop_df |> group_by_at(.vars=c("year")) |> summarize_at(.vars=c(popCol0), sum, na.rm=T) |> ungroup()
    national_pop <- national_pop |> rename_at(vars(popCol0), ~"national_pop")
    # national_pop |> glimpse()
    rm("popInput")
  } ### if(has_popUpdate)
  else{
    message("Creating Population scenario from defaults...")
    ### Select columns and filter
    pop_df        <- pop_default          |> select(all_of(popCols0))
    national_pop  <- national_pop_default |> select("year", "national_pop")
    rm("pop_default", "national_pop_default")
  } ### End else(has_popUpdate)
  ### Message user
  if(has_gdpUpdate|has_popUpdate){if(msgUser){messages_data[["updatePopGDP"]]}}
  ### Filter to correct years
  gdp_df            <- gdp_df            |> filter(year >= minYear & year <= maxYear)
  pop_df            <- pop_df            |> filter(year >= minYear & year <= maxYear)
  national_pop      <- national_pop      |> filter(year >= minYear & year <= maxYear)
  ### National scenario
  # gdp_df |> glimpse(); national_pop |> glimpse();
  national_scenario <- gdp_df            |> left_join(national_pop, by=c("year"))
  national_scenario <- national_scenario |> mutate(gdp_percap = gdp_usd / national_pop)
  ### Update inputs in outputs list
  returnList[["driverScenarios"]][["gdp"]] <- gdp_df
  returnList[["driverScenarios"]][["pop"]] <- pop_df
  # gdp_df |> nrow() |> print(); national_pop |> nrow() |> print(); national_scenario |> nrow() |> print()
  rm("gdp_df", "national_pop")

  ### Updated scenario
  join0             <- "year"
  arrange0          <- "region" |> c(stateCols0) |> c(join0)
  updatedScenario   <- national_scenario |> left_join(pop_df, by=join0)
  updatedScenario   <- updatedScenario   |> arrange_at(c(arrange0))
  rm(join0, arrange0)

  ###### Update Scalars ######
  if(msgUser) message("", list_messages[["updateScalars"]]$try, "")
  ### Filter main scalars to correct years and filter out regional population
  df_mainScalars <- df_mainScalars |> filter(year >= minYear) |> filter(year <= maxYear)
  # df_mainScalars |> glimpse();
  df_mainScalars <- df_mainScalars |> update_popScalars(updatedScenario)
  # df_mainScalars1 |> glimpse();

  # ### Message the user
  # if(msgUser) {paste0("\t", messages_data[["calcScalars"]]$success) |> message()}
  # # ### Message the user
  # # if(msgUser) message("\t", list_messages[["updateScalars"]]$success)
  # "got here" |> print()

  ###### Initialize Results ######
  ### Initialized results: Join sector info and default scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars
  # df_sectorsInfo |> glimpse(); #df_mainScalars |> glimpse(); df_mainScalars1 |> glimpse()
  df_results0    <- updatedScenario |> initialize_resultsDf(
    df_info    = df_sectorsInfo,
    df_scalars = df_mainScalars,
    elasticity = elasticity
  )
  # df_mainScalars1 |> glimpse();
  ### Filter to years
  df_results0    <- df_results0 |> filter(year >= minYear & year <= maxYear)
  # df_results0    <- df_results0 |> mutate(model_type=modelType)

  ##### Scenario ID  ######
  ### Mutate model for SLR sectors
  # df_results0 |> glimpse()
  modelCols0     <- c("model_id", "model_dot", "model_underscore", "model_label")
  co_models0     <- "co_models" |> get_frediDataObj("frediData")
  ### Change model to interpolation for SLR models
  group0         <- co_models0 |> names()
  which_slr      <- (co_models0[["modelType"]] |> tolower() == "slr") |> which()
  co_models0[which_slr,modelCols0] <- "Interpolation"
  co_models0     <- co_models0 |>
    group_by_at(c(group0)) |>
    summarize(n=n(), .groups="keep") |> ungroup() |>
    select(-c("n"))
  rm(group0, which_slr, modelCols0)
  # "got here" |> print()
  ### Join with initial results
  join0          <- c("modelType")
  df_results0    <- df_results0 |> left_join(co_models0, by=c(join0))
  # df_results0 |> glimpse()
  rm(join0, co_models0)
  # return(df_results0)

  ### Create scenario ID and separate by model type
  include0        <- c("region") |> c(stateCols0) |> c("model_label")
  df_results0     <- df_results0 |> get_scenario_id(include = include0)

  ###### Scaled Impacts  ######
  ### Initialize and empty data frame df_scenarioResults
  if(msgUser) message(list_messages[["scaledImpacts"]]$try)
  if(msgUser) message("Calculating scaled impacts...")
  df_scenarioResults  <- tibble()
  # df_results0    <- df_results0 |> mutate(model_type=modelType)
  df_results0_gcm <- df_results0 |> filter(modelType!="slr")
  df_results0_slr <- df_results0 |> filter(modelType=="slr")
  rm(df_results0)

  ### Number of GCM and SLR rows
  nrow_gcm        <- df_results0_gcm |> nrow()
  nrow_slr        <- df_results0_slr |> nrow()
  # nrow_gcm |> c(nrow_slr) |> print()

  ###### ** GCM Scaled Impacts ######
  if(nrow_gcm){
    # df_results0_gcm |> glimpse()
    df_gcm0            <- df_results0_gcm |> get_gcmScaledImpacts(df1=df_drivers)
    df_scenarioResults <- df_scenarioResults |> rbind(df_gcm0)
    # # df_scenarioResults |> glimpse()
    # df_results0_gcm$scenario_id |> unique() |> head() |> print()
    # (df_gcm0 |> filter(!is.na(scaled_impacts)))$scenario_id |> unique() |> head() |> print()
    # df_scenarioResults$scenario_id |> unique() |> head() |> print()
    rm(df_gcm0)
  } ### End if(nrow_gcm)
  # df_scenarioResults |> filter(!is.na(scaled_impacts)) |> nrow() |> print()
  ###### ** SLR Scaled Impacts ######
  if(nrow_slr){
    # "got here1" |> print()
    df_slr0            <- df_results0_slr |> get_slrScaledImpacts(df1=df_drivers)
    df_scenarioResults <- df_scenarioResults |> rbind(df_slr0)
    rm(df_slr0)
  } ### End if(nrow_slr)

  ###### ** Format Scaled Impacts ######
  ### Drop columns
  # df_results0 |> glimpse() |> print()
  drop0              <- c("modelUnitValue")
  df_scenarioResults <- df_scenarioResults |> select(-all_of(drop0))
  # df_scenarioResults |> names() |> print()
  rm(drop0)

  ### Message user
  if(msgUser) message("\t", list_messages[["scaledImpacts"]]$success)
  ###### Calculate Impacts  ######
  ### Join results with initialized results and update missing observations with NA
  ### Drop columns, then join with scenario results
  # df_results0_gcm |> names() |> print(); df_results0_slr |> names() |> print()
  join0       <- c("scenario_id", "year")
  df_results0 <- df_results0_gcm |> rbind(df_results0_slr)
  # df_results0 |> names() |> print()
  # return(df_results0)
  # df_results0 |> names() |> print(); df_scenarioResults |> names() |> print()
  # df_results0 |> glimpse(); df_scenarioResults |> glimpse()
  # df_results0$scenario_id |> unique() |> head() |> print();
  # df_scenarioResults$scenario_id |> unique() |> head() |> print()
  df_impacts  <- df_results0 |> left_join(df_scenarioResults, by=c(join0));
  # df_impacts |> glimpse()
  rm(df_results0, df_scenarioResults); rm(join0)


  ### Physical impacts = physScalar * scaled_impacts
  ### Annual impacts = phys-econ scalar value by the scaled impacts
  df_impacts <- df_impacts |> mutate(physical_impacts = scaled_impacts * physScalar)
  df_impacts <- df_impacts |> mutate(annual_impacts   = scaled_impacts * physEconScalar)

  ###### Add Scenario Information ######
  ### Add in model info
  paste0("Formatting results", "...") |> message()

  # ###### ** Model Types ######
  # ### Model types and models
  rename0    <- c("model_label", "modelType_label", "modelUnitValue", "modelUnit_label", "modelUnitDesc")
  rename1    <- c("model"      , "model_type"     , "driverValue"   , "driverUnit",      "driverType")
  select0    <- rename0 |> c("modelType")
  join0      <- c("modelType", "year")
  drop0      <- c("model_type")
  drop1      <- c("modelType")
  # drop1      <- drop0 |> c("modelUnitValue")
  ### Drop columns
  # df_drivers |> glimpse(); df_impacts |> glimpse()
  # df_drivers <- df_drivers |> select(all_of(select0))
  df_impacts <- df_impacts |> select(-any_of(drop0))
  ### Join values
  df_results <- df_impacts |> left_join(df_drivers, by=c(join0))
  rm(df_drivers, df_impacts)
  ### Drop join column & rename columns
  df_results <- df_results |> select(-any_of(drop1))
  df_results <- df_results |> rename_at(c(rename0), ~rename1)
  rm(rename0, rename1, select0, join0, drop0, drop1)
  # df_results |> glimpse()



  ### Update inputs in outputs list
  returnList[["results"]] <- df_results

  ###### Format Outputs ######
  ### Refactor sectors, variants, impactTypes
  co_variants    <- co_variants    |> mutate(sector_variant    = sector_id |> paste0("_", variant_id))
  co_impactTypes <- co_impactTypes |> mutate(sector_impactType = sector_id |> paste0("_", impactType_id))

  #### Rename Sector Columns
  df_results <- df_results |> rename(sector_id = sector, sector = sector_label)
  #### Regions #wm should we do something similar for state?
  # df_results |> names() |> print()
  reg_lvls   <- co_regions[["region_dot"]]
  reg_lbls   <- co_regions[["region_label"]]
  df_results <- df_results |> rename(region_id = region)
  df_results <- df_results |> mutate(region    = region_id |> factor(reg_lvls, reg_lbls))
  rm(reg_lvls, reg_lbls)



  ### Variant labels
  var_lvls   <- co_variants[["sector_variant"]]
  var_lbls   <- co_variants[["variant_label"]]
  df_results <- df_results |> mutate(sect_var = sector_id |> paste0("_", variant))
  df_results <- df_results |> mutate(variant  = sect_var  |> factor(var_lvls, var_lbls))
  df_results <- df_results |> select(-c("sect_var"))
  rm("var_lvls", "var_lbls")
  # (df_results |> filter(driverUnit=="cm"))$year |> range() |> print()

  ### Impact types
  imp_lvls   <- co_impactTypes[["sector_impactType"]]
  imp_lbls   <- co_impactTypes[["impactType_label"]]
  df_results <- df_results |> mutate(sect_imp   = sector_id |> paste0("_", impactType))
  df_results <- df_results |> mutate(impactType = sect_imp  |> factor(imp_lvls, imp_lbls))
  df_results <- df_results |> select(-c("sect_imp", "sector_id"))
  rm("imp_lvls", "imp_lbls")
  # (df_results |> filter(driverUnit=="cm"))$year |> range() |> print()

  ###### Columns ######
  ### Grouping columns
  groupCols0  <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0)
  groupCols0  <- groupCols0 |> c("model_type", "model")
  groupCols0  <- groupCols0 |> c("modelUnitType")
  groupCols0  <- groupCols0 |> c("sectorprimary", "includeaggregate")
  groupCols0  <- groupCols0 |> c("physicalmeasure")
  groupCols0  <- groupCols0 |> (function(x){x[!(x %in% (x |> names()))]})()
  groupCols0  <- groupCols0 |> unique()
  ### Driver columns
  driverCols0 <- c("driverType", "driverUnit", "driverValue")
  ### National & regional scenario columns
  scenCols0   <- c("gdp_usd", "national_pop", "gdp_percap") |> c(popCol0)
  ### Impact columns
  impactCols0 <- c("physical_impacts", "annual_impacts")
  ### Columns to select
  select0     <- groupCols0 |> c(driverCols0, scenCols0) |> c("year") #|> c(impactCols0)
  ### Relocate columns
  # df_results |> names() |> print(); groupCols0 |> print(); select0 |> print()
  df_results  <- df_results |> relocate(all_of(select0))
  # scenarioCol0     <- c("scenario_id")

  ### Scalar columns
  scalarCols0 <- c("physScalar", "physAdj", "damageAdj", "econScalar", "econAdj", "econMultiplier")
  infoCols0   <- c("c0", "c1", "exp0", "year0")
  suffix0     <- c("Name", "Value")
  scalarCols0 <- scalarCols0 |> map(~.x |> paste0(c(suffix0))) |> unlist()
  scalarCols0 <- scalarCols0 |> c("physScalar", "econScalar", "physEconScalar")
  scalarCols0 <- scalarCols0 |> c("scaled_impacts")
  scalarCols0 <- infoCols0   |> c(scalarCols0)
  rm(suffix0, infoCols0)
  ### Rearrange or drop scalar columns
  # df_results |> names() |> print()
  if(allCols){
    df_results  <- df_results |> relocate(any_of(scalarCols0), .after=all_of(select0))
  } else{
    df_results  <- df_results |> select(-any_of(scalarCols0))
  } ### End if(allCols)
  # df_results |> names() |> print()
  ### Other columns
  # otherCols0  <- df_results |> names() |> (function(x){x[!(x %in% c(select0, scalarCols0))]})()
  # df_results  <- df_results |> relocate(all_of(otherCols0), .before=all_of(impactCols0))
  otherCols0  <- df_results |> names() |> (function(x){x[!(x %in% c(select0, scalarCols0, impactCols0))]})()
  df_results  <- df_results |> select(-all_of(otherCols0))

  ### Convert to character and drop sector id
  df_results <- df_results |> mutate_at(c(groupCols0), as.character)
  # df_results <- df_results |> filter(!(sector |> is.na()))
  # (df_results |> filter(driverUnit=="cm"))$year |> range() |> print()

  ###### Testing ######
  # if(allCols) {
  #   drop1      <- select0
  #   df_results <- df_results |> select(-all_of(drop1))
  # } ### End if(!allCols)
  # # df_results |> glimpse()
  # # return(df_results)

  ###### Primary Columns ######
  if(doPrimary){
    df_results   <- df_results |> filter(sectorprimary   ==1)
    df_results   <- df_results |> filter(includeaggregate==1)
  } ### End if(doPrimary)
  df_results    <- df_results |> mutate_at(c("sectorprimary", "includeaggregate"), as.numeric)

  ###### Aggregation ######
  ### For regular use (i.e., not impactYears), simplify the data: groupCols0
  if(requiresAgg){
    # df_results <- df_results |> aggregate_impacts(aggLevels=aggLevels, groupByCols=groupCols0)
    group0     <- groupCols0
    # group0     <- select0 |> (function(x){x[!(x %in% driverCols0)]})()
    # select0 |> print(); df_results |> names() |> print()
    # group0     <- select0
    df_results <- df_results |> aggregate_impacts(
      aggLevels   = aggLevels,
      groupByCols = group0,
      columns     = impactCols0
    ) ### End aggregate_impacts
  } ### End if(requiresAgg)
  # df_results |> names() |> print()

  ###### Order the Output ######
  ### Convert levels to character
  ### Order the rows, then order the columns
  # if(!allCols){
  arrange0   <- groupCols0 |> c("year")
  arrange0   <- arrange0 |> (function(x){x[x %in% names(df_results)]})()
  ### Select columns
  df_results <- df_results |> arrange_at(c(arrange0))
  df_results <- df_results |> relocate(any_of(select0))
  # df_results <- df_results |> relocate(any_of(otherCols0), .after=any_of(select0))
  rm(arrange0)
  # } ### End if(!allCols)

  ###### Format as Data Frame ######
  ### Update results in list
  df_results   <- df_results |> ungroup()
  returnList[["results"]] <- df_results
  ### Which object to return
  if(outputList) {returnObj <- returnList}
  else           {returnObj <- df_results}
  ###### Return Object ######
  message("\n", "Finished", ".")
  return(returnObj)

} ### End function








