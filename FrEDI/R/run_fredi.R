###### Documentation ######
#' Project annual average climate change impacts throughout the 21st century for available sectors
#'
#'
#'
#' @description
#' This function allows users to project annual average climate change impacts through 2100 (2010-2100) for available sectors, with the option to extend results to 2300 (2010-2300). Users may specify custom temperature, U.S. population, and GDP scenarios. The output is an R data frame object containing annual average impacts, by year, for each sector, variant, impact type, region, state, and model.
#'
#' As of FrEDI Version 4.1.0, [FrEDI::run_fredi()] calculates impacts at the state-level for all available sectors.
#'
#' @param inputsList=NULL A list of named elements named elements (`names(inputsList) = c("tempInput", "slrInput", "gdpInput", "popInput")`), each containing data frames of custom temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and/or state-level population trajectories, respectively, over a continuous period in the range 2010 to 2300. Temperature and sea level rise inputs should start in 2000 or earlier. Values for population and GDP scenarios can start in 2010 or earlier. Values for each scenario type must be within reasonable ranges. For more information, see [FrEDI::import_inputs()].
#'
#' @param sectorList=NULL A character vector indicating a selection of sectors for which to calculate results (see [FrEDI::get_sectorInfo()]). If `NULL`, all sectors are included (i.e., `sectorList = get_sectorInfo()`).
#'
#' @param aggLevels="all" Levels of aggregation at which to summarize data: one or more of `c("national"`, `"modelaverage"`, `"impactyear"`, `"impacttype"`, `"all"`, `"none")`. Defaults to all levels (i.e., `aggLevels = "all"`). Uses the same aggregation levels as [FrEDI::aggregate_impacts()]. Note that, if `"impacttype"` is in `aggLevels` (e.g., `aggLevels = "all"`), columns `"physical_measure"` and `"physical_impacts"` will be dropped from the results data frame. This is because aggregating over impact types for some sectors requires summing costs over different types of physical impacts, so reporting the physical impacts would be nonsensical.
#'
#' @param elasticity=1 A numeric value indicating an elasticity to use for adjusting VSL for applicable sectors and impacts (defaults to `elasticity = 1`). Applicable sectors and impacts are: **Climate-Driven Changes in Air Quality** (all impact types), **ATS Temperature-Related Mortality** (`impactType = "N/A"`; i.e., all impact types), **CIL Temperature-Related Mortality**, **Extreme Temperature** (all impact types), **Suicide** (`impactType = "N/A"`; i.e., all impact types), **Southwest Dust** (`impactType = "All Mortality"`), **Valley Fever** (`impactType = "Mortality"`), **Vibriosis** (`impactType = "N/A"`; i.e., all impact types), and **Wildfire** (`impactType = "Mortality"`).
#'
#' @param maxYear=2100 A numeric value indicating the maximum year for the analysis. The range for `maxYear` is `[2011, 2300]`. Defaults to `maxYear = 2100`.
#'
#' @param thru2300=FALSE A ` TRUE/FALSE` shortcut that overrides the `maxYear` argument to run the model to 2300. Defaults to `thru2300 = FALSE`.
#'
#' @param outputList=FALSE A ` TRUE/FALSE` value indicating whether to output results as a data frame object (`outputList = FALSE`, default) or to return a list of objects (`outputList = TRUE`) that includes information about model provenance (including input arguments and input scenarios) along with the data frame of results.
#'
#' @param allCols=FALSE A `TRUE/FALSE` value indicating whether to include intermediate column values in results (e.g., physical and economic multipliers). Used in testing. Note that aggregation levels must be set to `aggLevels = "none"` to properly return the intermediate columns. Defaults to `allCols = FALSE`).
#'
#' @param silent=TRUE A `TRUE/FALSE` value indicating the level of messaging desired by the user (default=`TRUE`).
#'
#'
#'
#' @details This function allows users to project annual average climate change impacts through 2300 (2010-2300) for available sectors. [FrEDI::run_fredi()] is the main function in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information).
#'
#' Users can specify an optional list of custom scenarios with `inputsList` (for more information on the format of inputs, see [FrEDI::import_inputs()]). The function [FrEDI::import_inputs()] can be used to importing custom scenarios from CSV files. [FrEDI::import_inputs()] returns a list with elements `tempInput`, `slrInput`, `gdpInput`, and `popInput`, with each containing a data frame with a custom scenario for temperature, GMSL, GDP, and state-level population, respectively. If a user imports scenarios using [FrEDI::import_inputs()], they can pass the outputs of [FrEDI::import_inputs()] directly to the [FrEDI::run_fredi()] argument `inputsList`. Note that the documentation for [FrEDI::import_inputs()] can also provide additional guidance and specification on the formats for each scenario type.
#'
#' If `inputsList = NULL`, [FrEDI::run_fredi()] uses defaults for temperature, SLR, GDP, and population. Otherwise, [FrEDI::run_fredi()] looks for a list object passed to the argument `inputsList`. Within that list, [FrEDI::run_fredi()] looks for list elements `tempInput`, `slrInput`, `gdpInput`, and `popInput` containing data frames with custom scenarios for temperature, GMSL, GDP, and regional population, respectively. [FrEDI::run_fredi()] will default back to the default scenarios for any list elements that empty or `NULL` (in other words, running `run_fredi(inputsList = list())` returns the same outputs as running [FrEDI::run_fredi()]).
#'
#' * __Temperature Inputs.__ The input temperature scenario requires CONUS temperatures in degrees Celsius relative to 1995 (degrees of warming relative to the baseline year--i.e., the central year of the 1986-2005 baseline). CONUS temperature values must be greater than or equal to zero degrees Celsius.
#'    * Users can convert global temperatures to CONUS temperatures using [FrEDI::convertTemps]`(from = "global")` (or by specifying [FrEDI::import_inputs]`(temptype = "global")` when using [FrEDI::import_inputs()] to import a temperature scenario from a CSV file).
#'    * `tempInput` requires a data frame object with two columns with names `"year"`, and `"temp_C"` containing the year and CONUS temperatures in degrees Celsius, respectively.
#'    * Temperature inputs must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for temperature (i.e., `inputsList = list(tempInput = NULL)`, [FrEDI::run_fredi()] uses a default temperature scenario.
#' * __SLR Inputs.__ The input SLR scenario requires values for changes in global mean sea level rise (GMSL) heights in centimeters (cm). GMSL heights must be greater than or equal to zero.
#'    * `slrInput` requires a data frame object with two columns with names `"year"`, `"slr_cm"` containing the year and global mean sea level rise (GMSL) in centimeters, respectively.
#'    * SLR inputs must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for SLR (i.e., `inputsList = list(slrInput = NULL)`, [FrEDI::run_fredi()] first converts the input or default CONUS temperature scenario to global temperatures (using [FrEDI::convertTemps()]) and then converts the global temperatures to a global mean sea level rise (GMSL) height in centimeters (using [FrEDI::temps2slr()]).
#' * __GDP Inputs.__ The input scenario for gross domestic product (GDP) requires national GDP values in 2015$. GDP values must be greater than or equal to zero.
#'    * `gdpInput` requires a data frame object with two columns with names `"year"`, and `"gdp_usd"` containing the year and the national GDP, respectively. GDP values must be greater than or equal to zero.
#'    * GDP inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for GDP (i.e., `inputsList = list(gdpInput = NULL)`, [FrEDI::run_fredi()] uses a default GDP scenario.
#' * __Population Inputs.__ The input population scenario requires state-level population values. Population values must be greater than or equal to zero.
#'    * `popInput` requires a data frame object with five columns with names `"year"`, `"region"`, `"state"`, `"postal"`, and `"state_pop"` containing the year, the NCA region name, the state name, the postal code abbreviation for the state, and the state population, respectively.
#'    * Population inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for population (i.e., `inputsList = list(popInput = NULL)`, [FrEDI::run_fredi()] uses a default population scenario.
#'
#'
#' [FrEDI::run_fredi()] linearly interpolates missing annual values for all input scenarios using non-missing values (each scenario requires at least two non-missing values as detailed above for each scenario type). After interpolation of the input scenarios, [FrEDI::run_fredi()] subsets the input scenarios to values within the analysis period.
#'
#' * Temperatures are interpolated using 1995 as the baseline year (i.e., the central year of the 1986-2005 baseline) and GMSL is interpolated using 2000 as the baseline year. In other words, temperature (in degrees Celsius) is set to zero for the year 1995, whereas GMSL is set to zero for the year 2000. The interpolated temperature and GMSL scenarios are combined into a column called `driverValue`, along with additional columns for year, the driver unit (column `"driverUnit"`, with `driverUnit = "degrees Celsius"` and `driverUnit = "cm"` for temperature- and SLR-driven sectors, respectively), and the associated model type (column `"model_type"`, with `model_type = "GCM"` and `model_type = "SLR"` for temperature- and SLR-driven sectors, respectively
#' * [FrEDI::run_fredi()] calculations national population from state-level values and then calculates GDP per capita from values for GDP and national population. Values for state population, national population, national GDP (in 2015$), and national per capita GDP (in 2015$/capita) are provided in the results data frame in columns `"state_pop"`, `"national_pop"`, `"gdp_usd"`, and `"gdp_percap"`, respectively.
#'
#' By default, [FrEDI::run_fredi()] will calculate impacts for all sectors included in the tool. Alternatively, users can pass a character vector specifying a single sector or a subset of sectors using the `sectorList` argument. To see a list of sectors included within [FrEDI], run [FrEDI::get_sectorInfo()]. If `sectorList = NULL` (default), all sectors are included.
#'
#' By default, [FrEDI::run_fredi()] calculates impacts starting in the year 2010 and ending in 2100. Specify an alternative end year for the analysis using the `maxYear` argument. `maxYear` has a default value of `2100` and minimum and maximum values of `2011` and `2300`, respectively. Alternatively, users can set argument `thru2300 = TRUE` to override the `maxYear` argument and set `maxYear = 2300`. Note that the default scenarios included within [FrEDI] stop in the year 2100; users must provide custom input scenarios out to the desired end year **and** specify a `maxYear >= 2100` (and `maxYear <= 2300`) in order to return non-missing values for years after 2100.
#'
#' Annual impacts for each sector, variant, impact type, and impact year combination included in the model are calculated by multiplying scaled climate impacts by a physical scalar and economic scalars and multipliers. Some sectors use Value of a Statistical Life (VSL) to adjust the value non-linearly over time. [FrEDI::run_fredi()] uses a default value of `elasticity = 1`to adjust VSL for applicable sectors and impacts (the default value of `elasticity = 1` keeps VSL constant over time). A custom elasticity can be passed to the `elasticity` argument. Applicable sectors and impacts are ***Climate-Driven Changes in Air Quality** (all impact types), **ATS Temperature-Related Mortality**  (`impactType = "N/A"`; i.e., all impact types), **CIL Temperature-Related Mortality**, **Extreme Temperature** (all impact types), **Suicide** (`impactType = "N/A"`; i.e., all impact types), **Southwest Dust** (`impactType = "All Mortality"`), **Valley Fever** (`impactType = "Mortality"`), **Vibriosis** (`impactType = "N/A"`; i.e., all impact types), and **Wildfire** (`impactType = "Mortality"`).
#'
#' [FrEDI::run_fredi()] aggregates or summarizes results to level(s) of aggregation specified by the user (passed to `aggLevels`) using the post-processing helper function [FrEDI::aggregate_impacts()]. Users can specify all aggregation levels at once by specifying `aggLevels = "all"` (default) or no aggregation levels (`aggLevels = "none"`). Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels = "national"`), averaging across model types and models (`aggLevels = "modelaverage"`), summing over all impact types (`aggLevels = "impacttype"`), and interpolating between impact year estimates (`aggLevels = "impactYear"`).
#'
#' If the user specifies `aggLevels = "none"`, [FrEDI::run_fredi()] returns a data frame with columns: `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, `"model"`, `"sectorprimary"`, `"includeaggregate"`, `"physicalmeasure"`, `"driverType"`, `"driverUnit"`, `"driverValue"`, `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, `"state_pop"`, `"year"`, `"physical_impacts"`, and `"annual_impacts"`.
#'
#'
#' * Columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, and `"model"` all contain observation identifiers (sector name, variant (i.e., sector variant or adaptation name), impact type, impact year, region, state, state postal code abbreviation, model type, and model, respectively).
#' * Column `"sectorprimary"` contains values indicating which variant (i.e., sector variant or adaptation name) is the primary one for the sector (`sectorprimary = 1`for primary variants and `sectorprimary = 0` for non-primary variants). This column can be used to filter the outputs of [FrEDI::run_fredi()] (e.g., as might be done before aggregating impacts over sectors).
#' * Column `"includeaggregate"` contains values indicating which sectors should be included when aggregating over sectors (`includeaggregate = 1`for primary sectors and `includeaggregate = 0` for non-primary sectors). For instance, sectors __ATS Temperature-Related Mortality__, __CIL Temperature-Related Mortality__, and __Extreme Temperature__ have values for temperature-related mortality. To avoid double counting, outputs of [FrEDI::run_fredi()] should be filtered to values for which `sectorprimary = 1` and `includeaggregate = 1`.
#' * Columns `"driverType"`, `"driverUnit"`, and `"driverValue"` contain information about the temperature and SLR scenarios.
#' * Columns `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, and `"state_pop"` contain information about the GDP and population scenarios.
#' * Columns `"physicalmeasure"` and `"physical_impacts"` contain information about physical impacts.
#' * Column `"annual_impacts"` contains information on the economic value associated with annual impacts.
#'
#'
#' If the user specifies `aggLevels = "all"` or other combinations of aggregation levels, [FrEDI::run_fredi()] passes the results data frame and the `aggLevels` argument to the [FrEDI::aggregate_impacts()] function. [FrEDI::aggregate_impacts()] then performs the following calculations, using the default grouping columns for the [FrEDI::aggregate_impacts()]: `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, `"model"`, `"sectorprimary"`, `"includeaggregate"`, `"physicalmeasure"`, and `"year"` (note that the `"variant"` column referred to below contains information about the variant name (or `“N/A”`), as applicable).
#'
#' \tabular{ll}{
#' \strong{Aggregation Level} \tab \strong{Description} \cr
#' *`impactyear`* \tab To aggregate over impact years, [FrEDI::aggregate_impacts()] first separates results for sectors with only one impact year estimate (i.e., `impactYear = "N/A"`) from from observations with multiple impact year estimates (i.e., sectors with results for both `impactYear = "2010"` and `impactYear = "2090"`). For these sectors with multiple impact years, physical impacts and annual costs (columns `"physical_impacts"` and `"annual_impacts"`) are linearly interpolated between impact year estimates. For any model run years above 2090, annual results for sectors with multiple impact years return the 2090 estimate. The interpolated values are then row-bound to the results for sectors with a single impact year estimate, and column `impactYear` set to `impactYear = "Interpolation"` for all values. If `"impactyear"` is included in `aggLevels` (e.g., `aggLevels = "all"`), [FrEDI::aggregate_impacts()] aggregates over impact years before performing other types of aggregation. \cr
#'
#' *`modelaverage`* \tab To aggregate over models for temperature-driven sectors, [FrEDI::aggregate_impacts()] averages physical impacts and annual costs (columns `"physical_impacts"` and `"annual_impacts"`, respectively) across all GCM models present in the data. [FrEDI::aggregate_impacts()] drops the column `"model"` from the grouping columns when averaging over models. Averages exclude observations with missing values. However, If all values within a grouping are missing, the model average is set to `NA`. The values in column `"model"` are set to `"Average"` for model averages and the model averages data frame is then row-bound to the main results data frame. For SLR-driven sectors, there is no need for additional model aggregation; these values already have `model = "Interpolation"`. If `"modelaverage"` is included in `aggLevels` (e.g., `aggLevels = "all"`), [FrEDI::aggregate_impacts()] first aggregates over impact years  (if `"impactyear"` present in `aggLevels` or if `aggLevels = "all"`) before aggregating over models.\cr
#'
#' *`national`* \tab To aggregate values to the national level, [FrEDI::aggregate_impacts()] sums physical impacts and annual costs (columns `"physical_impacts"` and `"annual_impacts"`, respectively) across all states present in the data. [FrEDI::aggregate_impacts()] drops the columns `"region"`, `"state"`, and `"postal"` when summing over states and regions. Years which have missing column data for all regions return as `NA`. Values for columns `"region"`, `"state"`, and `"postal"` are set to `"National Total"`, `All`, and `US`, respectively. The data frame with national totals is then row-bound to the main results data frame. If `"national"` is included in `aggLevels` (e.g., `aggLevels = "all"`), [FrEDI::aggregate_impacts()] first aggregates over impact years and/or models (if `"impactyear"` and/or `"modelaverage"` are present in `aggLevels` or if `aggLevels = "all"`) before aggregating over models.\cr
#'
#' *`impacttype`* \tab To aggregate values over impact types, [FrEDI::aggregate_impacts()] sums annual impacts (column `"annual_impacts"`) across all impact types for each sector. [FrEDI::aggregate_impacts()] drops the column `"impactType"` and `"physicalmeasure"` from the grouping columns when summing over impact types. Years which have missing column data for all impact types return as `NA`. All values in column `"impactType"` are set to `"all"`. Aggregating over impact types, drops columns related to physical impacts (i.e., columns `"physicalmeasure"` and `"physical_impacts"`). These columns are dropped since aggregating over impact types for some sectors requires summing costs over different types of physical impacts, so reporting the physical impacts would be nonsensical.\cr
#' }
#'
#' After aggregating values, [FrEDI::aggregate_impacts()] joins the data frame of impacts with information about `"driverType"`, `"driverUnit"`, `"driverValue"`, `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, and `"state_pop"`.
#'
#' If `outputList = FALSE` (default), [FrEDI::run_fredi()] returns a data frame of annual average impacts over the analysis period, for each sector, variant, impact type, impact year, region, state, model type (`"GCM"` or `"SLR"`), and model. If `outputList = TRUE`, in addition to the data frame of impacts, [FrEDI::run_fredi()] returns a list object containing information about values for function arguments, driver scenarios, and population and GDP scenarios.
#'
#'
#'
#' @return
#' If `outputList=FALSE`, the output of [FrEDI::run_fredi()] is a data frame object (described above) containing annual average impacts over the analysis period, for each sector, variant, impact type, impact year, region, state, and model (GCM name for temperature-driven sectors and `"Interpolation"` for SLR-driven sectors).
#'
#' If `outputList=TRUE`, [FrEDI::run_fredi()] returns a list object containing the following:
#'
#' * __`statusList`__. A list with values for the arguments passed to [FrEDI::run_fredi()] (including defaults if unspecified).
#' * __`argsList`__. A list with elements named after [FrEDI::run_fredi()] arguments, containing the values of the arguments passed to [FrEDI::run_fredi()] (or default values if unspecified).
#' * __`scenarios`__. A list with named elements `"temp"`, `"slr"`, `"gdp"`, and `"pop"` -- each containing the scenarios for temperature, SLR, GDP, and population as used by the model in calculating impacts.
#' * __`results`__. Containing a data frame of annual impacts (i.e., the same data frame returned if `outputList = FALSE`).
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
#' temps1 <- gcamScenarios |> filter(scenario=="Hector_GCAM_v5.3_ECS_3.0_ref")
#' temps1 <- temps1 |> mutate(temp_C = temp_C_global |> convertTemps(from="global"))
#' temps1 <- temps1 |> select(year, temp_C)
#'
#' ### Run custom scenario
#' run2 <- run_fredi(inputsList=list(tempInput=temps1, popInput=popScenario))
#'
#' ### Load scenarios from file:
#' scenariosPath <- system.file(package="FrEDI") |> file.path("extdata","scenarios")
#' scenariosPath |> list.files()
#'
#'
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath |> file.path("slr_from_GCAM.csv")
#'
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath |> file.path("State ICLUS Population.csv")
#'
#' ### Import inputs
#' x_inputs <- import_inputs(slrfile=slrInputFile, popfile=popInputFile, popArea="state")
#'
#' ### Run custom scenarios
#' run3 <- run_fredi(inputsList=x_inputs)
#'
#' ### Get information on sectors:
#' get_sectorInfo()
#'
#' ### Run for a single sector, with default inputs, no aggregation, and elasticity=1:
#' run4 <- run_fredi(sectorList="ATS Temperature-Related Mortality", aggLevels="none", elasticity=1)
#'
#' ### Set end year for analysis to 2110 -- messages user and returns a null value since default scenarios only have values out to 2100
#' run5 <- run_fredi(maxYear=2110)
#'
#' ### Set end year for analysis to 2300 -- messages user and returns a null value since default scenarios only have values out to 2100)
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
#'list(tempInput=NULL, slrInput=NULL, gdpInput=NULL, popInput=NULL) |> (function(x){x |> set_names(x |> names())})
#'
#'
#'
###### run_fredi_modular ######
### This function creates a data frame of sector impacts for default values or scenario inputs.
### run_fredi relies on the following helper functions: "interpolate_annual", "match_scalarValues","get_econAdjValues" , "calcScalars", "interpolate_tempBin"
run_fredi <- function(
    inputsList = list(tempInput=NULL, slrInput=NULL, gdpInput=NULL, popInput=NULL), ### List of inputs
    sectorList = NULL, ### Vector of sectors to get results for
    aggLevels  = c("national", "modelaverage", "impactyear", "impacttype"), ### Aggregation levels
    elasticity = 1,    ### Override value for elasticity for economic values
    maxYear    = 2100,
    thru2300   = FALSE,
    outputList = FALSE, ### Whether to return input arguments as well as results. [If TRUE], returns a list instead of a data frame
    allCols    = FALSE, ### Whether to include additional columns in output
    silent     = TRUE   ### Whether to message the user
){
  ###### Load Objects ######
  ### Assign data objects to objects in this namespace
  ### Assign FrEDI config
  # for(list_i in rDataList) list_i[["data"]] |> list2env(envir = environment())
  # fredi_config <- "fredi_config" |> get_frediDataObj("fredi_config")
  fredi_config <- rDataList[["fredi_config"]]
  # fredi_config |> names() |> print()
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}
  # fredi_config |> names() |> print()
  # "got here" |> print()
  # fredi_config |> list2env(envir = environment())


  ###### Set up the environment ######
  ### Level of messaging (default is to message the user)
  msgUser   <- !silent
  ### Uncomment for allCols
  # doPrimary <- F  ### whether to filter to primary impacts
  ### Model years and NPD (FrEDI past 2100)
  minYear   <- minYear0
  maxYear   <- thru2300 |> ifelse(npdYear0, maxYear)
  do_npd    <- maxYear > maxYear0
  # "got here2" |> print()

  ###### ** Return List ######
  ### Initialize list to return
  returnList <- list() ### List to return args, scenarios, and statuses
  argsList   <- list() ### List of arguments
  statusList <- list() ### List to return custom or default

  ### Initialize return list: add scenarios
  if(outputList) {returnList[["scenarios"]] <- list()}


  ### Initialize status list
  ### Add statuses:
  ### - inputsList items and aggLevels assessed further below
  ### Add to list
  if(outputList){
    statusList[["inputsList"]] <- inputsList
    statusList[["sectorList"]] <- (!(sectorList |> is.null())) |> get_returnListStatus()
    statusList[["aggLevels" ]] <- "placeholder"
    statusList[["elasticity"]] <- (elasticity == 1) |> get_returnListStatus()
    statusList[["maxYear"   ]] <- (maxYear == maxYear0 & !thru2300) |> get_returnListStatus()
    statusList[["thru2300"  ]] <- (!thru2300) |> get_returnListStatus()
    statusList[["allCols"   ]] <- (!allCols ) |> get_returnListStatus()
    statusList[["silent"    ]] <- ( silent  ) |> get_returnListStatus()
  } ### End if(outputList)


  ### Initialize arguments list
  ### - inputsList items, sectorList, and aggLevels assessed further below
  if(outputList){
    argsList[["inputsList"]] <- inputsList
    argsList[["sectorList"]] <- "placeholder"
    argsList[["aggLevels" ]] <- "placeholder"
    argsList[["elasticity"]] <- elasticity
    argsList[["maxYear"   ]] <- maxYear
    argsList[["thru2300"  ]] <- thru2300
    argsList[["allCols"   ]] <- allCols
    argsList[["silent"    ]] <- silent
  } ### End if(outputList)




  ###### ** Years   ######
  # cYears0 <- minYear:maxYear

  ###### ** Elasticity ######
  ### Message user about elasticity
  has_elasticity <- elasticity     |> is.numeric()
  elasticity     <- has_elasticity |> ifelse(elasticity, elasticity0)
  if(!has_elasticity){
    paste0("\t", "Incorrect value type provided for argument 'elasticity'...") |> message()
    paste0("\t\t", "Using default elasticity values.") |> message()
  } ### End if
  rm(has_elasticity, elasticity0)

  ###### ** State Info ######
  byState        <- TRUE
  # popCol0        <- "state_pop"
  popCol0        <- "pop"
  stateCols0     <- c("state", "postal")

  ###### ** Agg Levels  ######
  ### Types of summarization to do: default
  ### Aggregation levels
  aggList0   <- aggList0  |> tolower()
  aggLevels  <- aggLevels |> tolower()
  ### Update status list
  statusList[["aggLevels" ]] <- ("all" %in% aggLevels | (aggLevels %in% aggList0) |> all()) |> get_returnListStatus()
  aggList1   <- aggList0  |> c("all", "none")
  aggLevels  <- aggLevels |> (function(y, z=aggList1){y[y %in% z]})()
  ### If none specified, no aggregation (only SLR interpolation)
  ### Otherwise, aggregation depends on length of agg levels
  if     ("none" %in% aggLevels) {aggLevels <- c()}
  else if("all"  %in% aggLevels) {aggLevels <- aggList0}
  doAgg      <- (aggLevels |> length()) > 0
  ### Add to list
  if(outputList) {argsList[["aggLevels"]] <- aggLevels}
  rm(aggList0, aggList1)


  ###### ** Sectors List ######
  ### Initialize sector list if the sectors list is null
  co_sectors   <- "co_sectors" |> get_frediDataObj("frediData")
  if(sectorList |> is.null()) sectorList <- co_sectors |> pull(sector_label)
  ### Sector names & labels
  dfSectors    <- co_sectors |> filter((sector_label |> tolower()) %in% (sectorList |> tolower()))
  sectorIds    <- dfSectors  |> pull(sector_id)
  sectorLbls   <- dfSectors  |> pull(sector_label)

  ### Check sectors
  # sectorList |> print(); sectorLbls |> print()
  naSectors0   <- sectorList |> get_matches(y=sectorLbls, matches=F, type="values")
  ### Message the user
  if(naSectors0 |> length()){
    naSectors0  <- "\"" |> paste0(naSectors0 |> paste(collapse= "\", \""), "\"")
    msgSectors0 <- "\"" |> paste0(sectorLbls |> paste(collapse= "\", \""), "\"")
    1 |> get_msgPrefix(newline=T) |> paste0("Warning! Error in `sectorList`:") |> message()
    2 |> get_msgPrefix() |> paste0("Impacts are not available for sectors: ", naSectors0) |> message()
    2 |> get_msgPrefix(newline=T) |> paste0("Available sectors: ", msgSectors0) |> message()
    return()
  } ### End if(length(missing_sectors)>=1)
  ### Update in list
  if(outputList) {argsList[["sectorList"]] <- sectorLbls}



  ###### Scenarios ######
  ###### ** Check Inputs ######
  paste0("Checking scenarios...") |> message()
  ### Add info to data
  df_inputInfo <- "co_inputInfo" |> get_frediDataObj("frediData")
  df_inputInfo <- df_inputInfo |> mutate(ref_year = c(1995, 2000, 2010, 2010))
  df_inputInfo <- df_inputInfo |> mutate(min_year = c(2000, 2000, 2010, 2010))
  df_inputInfo <- df_inputInfo |> mutate(max_year = maxYear)
  # df_inputInfo <- df_inputInfo |> mutate(max_year = maxYear |> rep(df_inputInfo |> nrow()))

  ### Input defaults
  inputDefs    <- list()
  gcamDefault  <- "gcam_default" |> get_frediDataObj("scenarioData")
  inputDefs[["temp"]] <- gcamDefault |> select(c("year", "temp_C_conus")) |> rename_at(c("temp_C_conus"), ~"temp_C")
  inputDefs[["slr" ]] <- gcamDefault |> select(c("year", "slr_cm"))
  inputDefs[["gdp" ]] <- "gdp_default" |> get_frediDataObj("scenarioData")
  inputDefs[["pop" ]] <- "pop_default" |> get_frediDataObj("scenarioData")
  rm(gcamDefault)

  ### Input info
  inNames      <- df_inputInfo |> pull(inputName)
  inIDCols     <- get_import_inputs_idCols(popArea="state") |> (function(list0){
    list0[["pop"]] <- c("region", "state", "postal")
    return(list0)
  })()
  # inValCols    <- df_inputInfo |> pull(valueCol) |> str_replace("pop", "state_pop")
  inValCols    <- df_inputInfo |> pull(valueCol)
  inMinYears   <- df_inputInfo |> pull(min_year)
  inMaxYears   <- df_inputInfo |> pull(max_year)

  ### Rename inputs
  inputsList   <- inputsList |> set_names(inNames)

  # ### Rename pop column if present
  # inputsList   <- inputsList |> (function(list0){
  #   df0    <- list0[["pop"]]
  #   hasPop <- !(df0 |> is.null())
  #   cond0  <- hasPop |> ifelse("pop" %in% (df0 |> names()), FALSE)
  #   if(cond0){df0 <- df0 |> rename_at(c("pop"), ~"state_pop")}
  #   return(df0)
  # })()

  ### Make sure all inputs are present
  ### Check whether inputs are present
  inputsList   <- inNames     |> map(function(name_i){  inputsList[[name_i]]}) |> set_names(inNames)
  hasInputs    <- inNames     |> map(function(name_i){!(inputsList[[name_i]] |> is.null())}) |> set_names(inNames)
  whichInputs  <- hasInputs   |> unlist()
  hasAnyInputs <- whichInputs |> length()
  # whichInputs |> print()

  ### Create logicals and initialize inputs list
  if(hasAnyInputs) {
    inputsList <- list(
      inputName = inNames,
      inputDf   = inputsList,
      idCol     = inIDCols,
      valCol    = inValCols,
      yearMin   = inMinYears,
      yearMax   = inMaxYears
    ) |>
      pmap(check_input_data) |>
      set_names(inNames)
  } ### if(hasAnyInputs)

  ### Check again for inputs
  hasInputs    <- inNames |> map(function(name_i){!(inputsList[[name_i]] |> is.null())}) |> set_names(inNames)

  ### Update inputs with defaults if values are missing
  for(name_i in inNames) { if(inputsList[[name_i]] |> is.null()) {
    if("slr" %in% name_i) inputsList[[name_i]] <- inputsList[["temp"]]
    else                  inputsList[[name_i]] <- inputDefs[[name_i]]
    rm(name_i)
  } } ### End if, end for

  ### Iterate over list and calculate values
  inputsList   <- list(
    name0     = inNames,
    df0       = inputsList,
    hasInput0 = hasInputs,
    idCols0   = inIDCols,
    valCols0  = inValCols
  ) |> pmap(function(df0, name0, hasInput0, idCols0, valCols0){
    df0 |> format_inputScenarios(
      name0     = name0,
      hasInput0 = hasInput0,
      idCols0   = idCols0,
      valCols0  = valCols0,
      minYear   = minYear,
      maxYear   = maxYear,
      info0     = df_inputInfo
    )
  }) |> set_names(inNames)


  ### For each input:
  ### - Make sure values are at correct range
  ### - Update in status list
  if(outputList){    for(name_i in inNames) {
    name_i1  <- name_i |> paste0("Input")
    inputs_i <- inputsList[[name_i]]
    inputs_i <- inputs_i |> filter(year >= minYear, year <= maxYear)
    inputsList[[name_i]] <- inputs_i
    ### Add lists
    statusList[["inputsList"]][[name_i1]] <- hasInputs[[name_i]] |> get_returnListStatus()
    argsList  [["inputsList"]][[name_i1]] <- inputs_i
    returnList[["scenarios" ]][[name_i ]] <- inputs_i
    rm(name_i)
  } } ### End if(outputList)



  ###### ** Physical Driver Scenario  ######
  ### Select columns
  select0    <- c("inputName")
  filter0    <- c("temp", "slr")
  df_drivers <- inputsList[filter0] |> combine_driverScenarios(info0 = df_inputInfo)
  df_drivers <- df_drivers |> filter(year >= minYear, year <= maxYear)
  # return(df_drivers)

  ###### ** Socioeconomic Driver Scenario ######
  ### Update values
  gdp_df       <- inputsList[["gdp"]]
  pop_df       <- inputsList[["pop"]] |> mutate(region = region |> str_replace(" ", ""))
  pop_df       <- pop_df |> mutate(region = region |> str_replace("\\.", ""))
  ### ### Subset to desired range
  pop_df       <- pop_df |> filter(year >= minYear, year <= maxYear)
  gdp_df       <- gdp_df |> filter(year >= minYear, year <= maxYear)
  ### Calculate national population and update national scenario
  seScenario   <- gdp_df |> create_nationalScenario(pop0 = pop_df)
  rm(gdp_df, pop_df)
  # seScenario |> glimpse()


  ### Update scalars
  ### Filter main scalars to correct years and filter out regional population
  paste0("Updating scalars...") |> message()
  df_scalars   <- "df_scalars" |> get_frediDataObj("stateData")
  df_scalars   <- df_scalars |> filter(year >= minYear, year <= maxYear)
  df_scalars   <- df_scalars |> update_popScalars(seScenario, popCol = "pop")


  ###### Calculate Impacts ######
  ###### ** Initialize Impacts Data frame ######
  ### Initialized results: Join sector info and default scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars
  paste0("Calculating impacts...") |> message()
  df_results   <- seScenario |> initialize_resultsDf(sectors=sectorIds, elasticity=elasticity)
  # df_results |> glimpse()
  # return(df_results)

  ### Get scaled impacts
  # df_impacts   <- sectorIds |> calc_scaled_impacts_fredi(drivers0 = df_drivers)
  df_impacts   <- df_results |> calc_scaled_impacts_fredi(drivers0 = df_drivers)
  # df_impacts |> glimpse()
  # return(df_impacts)
  # return(list(df0=df_results, df1=df_impacts))

  ### Get impacts
  # df_results     <- df_results |> calc_impacts_fredi(df1=df_impacts)
  df_results   <- df_impacts |> calc_impacts_fredi()
  # return(list(df0=df_results, df1=df_impacts))
  rm(df_impacts)
  # df_results |> glimpse()
  return(df_results)


  ###### Format Results ######
  ### Add in model info
  paste0("Formatting results", "...") |> message()

  ###### ** Get Labels ######
  ### Rename sector
  drop0       <- c("sector", "variant", "impactType", "impactYear", "modelType", "model")
  renameAt0   <- drop0 |> paste0("_label") |> c("modelUnitDesc", "modelUnit_label", "modelUnitValue")
  renameTo0   <- drop0 |> c("driverType", "driverUnit", "driverValue")
  df_results  <- df_results |> select(-any_of(drop0))
  df_results  <- df_results |> rename_at(c(renameAt0), ~renameTo0)
  rm(drop0, renameAt0, renameTo0)

  ### Format region
  select0     <- c("region_label", "region_id")
  join0       <- c("region_id")
  # renameAt0   <- c("region_label")
  co_regions  <- "co_regions" |> get_frediDataObj("frediData") |> select(all_of(select0))
  # co_regions |> pull(region) |> unique() |> print()
  # df_results |> pull(region) |> unique() |> print()
  df_results  <- df_results |> rename(region_id = region) |> left_join(co_regions, by=c(join0))
  df_results  <- df_results |> select(-c(join0))
  df_results  <- df_results |> rename(region = region_label)
  rm(select0, join0, co_regions)

  ### Rename model type
  renameAt0   <- c("modelType")
  renameTo0   <- c("model_type")
  df_results  <- df_results |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)


  ###### ** Columns List ######
  ### Grouping columns
  groupCols0  <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0)
  groupCols0  <- groupCols0 |> c("model_type", "model")
  groupCols0  <- groupCols0 |> c("sectorprimary", "includeaggregate")
  groupCols0  <- groupCols0 |> c("physicalmeasure")
  groupCols0  <- groupCols0 |> get_matches(y=df_results |> names(), matches=TRUE)
  groupCols0  <- groupCols0 |> unique()
  # groupCols0 |> print()
  ### Driver columns
  driverCols0 <- c("driverType", "driverUnit", "driverValue")
  ### National & regional scenario columns
  scenCols0   <- c("gdp_usd", "national_pop", "gdp_percap") |> c(popCol0)
  ### Impact columns
  impactCols0 <- c("physical_impacts", "annual_impacts")
  ### Columns to select
  select0     <- groupCols0 |> c(driverCols0, scenCols0) |> c("year")
  ### Relocate columns
  # df_results |> glimpse()
  df_results  <- df_results |> relocate(all_of(select0))
  # df_results  <- df_results |> relocate(any_of(select0))

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
  if(allCols){
    df_results  <- df_results |> relocate(any_of(scalarCols0), .after=all_of(select0))
  } else{
    df_results  <- df_results |> select(-any_of(scalarCols0))
  } ### End if(allCols)

  ### Other columns
  # excludeCols
  # otherCols0  <- df_results |> names() |> (function(x){x[!(x %in% c(select0, scalarCols0, impactCols0))]})()
  otherCols0  <- df_results |> names() |> get_matches(y=c(select0, scalarCols0, impactCols0), matches=FALSE)
  df_results  <- df_results |> select(-all_of(otherCols0))

  ### Convert to character and drop sector id
  df_results  <- df_results |> mutate_at(c(groupCols0), as.character)

  ###### ** Primary Columns ######
  mutate0     <- c("sectorprimary", "includeaggregate")
  df_results  <- df_results |> mutate_at(c(mutate0), as.numeric)
  rm(mutate0)
  # if(doPrimary){
  #   df_results   <- df_results |> filter(sectorprimary   ==1)
  #   df_results   <- df_results |> filter(includeaggregate==1)
  # } ### End if(doPrimary)


  ###### ** Aggregation ######
  ### For regular use (i.e., not impactYears), simplify the data: groupCols0
  if(doAgg) {
    # doAgg |> print()
    group0     <- groupCols0
    df_results <- df_results |> aggregate_impacts(
      aggLevels   = aggLevels,
      groupByCols = group0,
      columns     = impactCols0
    ) ### End aggregate_impacts
  } ### End if(doAgg)

  ###### Arrange Outputs ######
  ### Convert levels to character
  ### Order the rows, then order the columns
  # arrange0   <- groupCols0 |> c("year")
  arrange0   <- groupCols0 |> c("year") |> get_matches(y = df_results |> names())
  # arrange0 |> print()
  ### Select columns
  df_results <- df_results |> arrange_at(c(arrange0))
  df_results <- df_results |> relocate(any_of(select0))
  rm(arrange0)

  ###### Format as Tibble ######
  ### Update results in list
  df_results   <- df_results |> as_tibble()
  df_results   <- df_results |> ungroup()

  ###### Return Object ######
  ### Which object to return
  if(outputList) {
    ### Add items to list/reorganize list
    list_scenarios <- returnList[["scenarios" ]]
    returnList     <- list()
    returnList[["statusList"]] <- statusList
    returnList[["argsList"  ]] <- argsList
    returnList[["scenarios" ]] <- list_scenarios
    returnList[["results"   ]] <- df_results
    returnObj <- returnList
  } else {
    returnObj <- df_results
  } ### End if(outputList)


  ###### Return ######
  ### Message
  message("\n", "Finished", ".")
  return(returnObj)

} ### End function








