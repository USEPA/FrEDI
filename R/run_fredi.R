## Documentation ----------------
#' Project annual average impacts from temperature and sea level change throughout the 21st century for available sectors
#'
#'
#'
#' @description
#' This function allows users to project annual average impacts from temperature and sea level change through 2100 (2010-2100) for available sectors, with the option to extend results to 2300 (2010-2300). Users may specify custom temperature, sea level, U.S. population, and GDP scenarios. The output is an R data frame object containing annual average impacts, by year, for each sector, variant, impact type, region, state, and model.
#'
#' As of FrEDI Version 4.1.0, [FrEDI::run_fredi()] calculates impacts at the state-level for all available sectors.
#'
#' @param inputsList=list(gdp=NULL,pop=NULL,temp=NULL,slr=NULL) A list with named elements (`gdp`, `pop`, `temp`, and/or `slr`), each containing data frames of custom scenarios for gross domestic product (GDP), state-level population, temperature, and/or global mean sea level rise (GMSL) trajectories, respectively, over a continuous period. Temperature and sea level rise inputs should start in 2000 or earlier. Values for population and GDP scenarios can start in 2010 or earlier. Values for each scenario type must be within reasonable ranges. For more information, see [FrEDI::import_inputs()].
#'
#' @param sectorList=NULL A character vector indicating a selection of sectors for which to calculate results (see [FrEDI::get_sectorInfo()]). If `NULL`, all sectors are included (i.e., `sectorList = get_sectorInfo()`).
#'
#' @param aggLevels="all" Levels of aggregation at which to summarize data: one or more of `c("national"`, `"modelaverage"`, `"impactyear"`, `"impacttype"`, `"all"`, `"none")`. Defaults to all levels (i.e., `aggLevels = "all"`). Uses the same aggregation levels as [FrEDI::aggregate_impacts()]. Note that, if `"impacttype"` is in `aggLevels` (e.g., `aggLevels = "all"`), columns `"physical_measure"` and `"physical_impacts"` will be dropped from the results data frame. This is because aggregating over impact types for some sectors requires summing costs over different types of physical impacts, so reporting the physical impacts would be nonsensical.
#'
#' @param elasticity=1 A numeric value indicating an elasticity to use for adjusting VSL for applicable sectors and impacts (defaults to `elasticity = 1`). Applicable sectors and impacts are: **Air Quality** (all impact types), **ATS Temperature-Related Mortality** (`impactType = "N/A"`; i.e., all impact types), **CIL Temperature-Related Mortality**, **Extreme Temperature** (all impact types), **Suicide** (`impactType = "N/A"`; i.e., all impact types), **Southwest Dust** (`impactType = "All Mortality"`), **Valley Fever** (`impactType = "Mortality"`), **Vibriosis** (`impactType = "N/A"`; i.e., all impact types), and **Wildfire** (`impactType = "Mortality"`).
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
#' @details This function allows users to project annual average impacts from temperature and sea level change through 2300 (2010-2300) for available sectors. [FrEDI::run_fredi()] is the main function in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information).
#'
#' Users can specify an optional list of custom scenarios with `inputsList` (for more information on the format of inputs, see [FrEDI::import_inputs()]). The function [FrEDI::import_inputs()] can be used to importing custom scenarios from CSV files. [FrEDI::import_inputs()] returns a list with named elements `gdp`, `pop`, `temp`, and `slr`, with each containing a data frame with a custom scenario for GDP, state-level population, temperature, and/or GMSL respectively. If a user imports scenarios using [FrEDI::import_inputs()], they can pass the outputs of [FrEDI::import_inputs()] directly to the [FrEDI::run_fredi()] argument `inputsList`. Note that the documentation for [FrEDI::import_inputs()] can also provide additional guidance and specification on the formats for each scenario type.
#'
#' Otherwise, [FrEDI::run_fredi()] looks for a list object passed to the argument `inputsList`. Within that list, [FrEDI::run_fredi()] looks for named list elements -- `gdp`, `pop`, `temp`, and/or `slr` -- each containing a data frame with a custom scenario for GDP, state population, temperature, and/or GMSL, respectively. If `inputsList = NULL` or `inputsList = list()` (default), [FrEDI::run_fredi()] uses default trajectories for GDP, population, temperature, and SLR (see [FrEDI::gdpDefault], [FrEDI::popDefault], and [FrEDI::gcamScenarios] for more information). [FrEDI::run_fredi()] will default back to the default scenarios for any list elements that empty or `NULL` (in other words, running `run_fredi(inputsList = list())` returns the same outputs as running [FrEDI::run_fredi()]).
#'
#' * __GDP Inputs.__ The input scenario for gross domestic product (GDP) requires national GDP values in 2015$. GDP values must be greater than or equal to zero.
#'    * `gdp` requires a data frame object with two columns with names `"year"`, and `"gdp_usd"` containing the year and the national GDP, respectively. GDP values must be greater than or equal to zero.
#'    * GDP inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for GDP (i.e., `inputsList = list(gdp = NULL)`, [FrEDI::run_fredi()] uses a default GDP scenario.
#' * __Population Inputs.__ The input population scenario requires state-level population values. Population values must be greater than or equal to zero.
#'    * `pop` requires a data frame object with five columns with names `"region"`, `"state"`, `"postal"`, `"year"`, and `"pop"` containing the year, the NCA region name (one of `"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"`), the state name, the postal code abbreviation for the state, and the state population, respectively.
#'    * Population inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for population (i.e., `inputsList = list(pop = NULL)`, [FrEDI::run_fredi()] uses a default population scenario.
#' * __Temperature Inputs.__ The input temperature scenario requires CONUS temperatures in degrees Celsius relative to 1995 (degrees of warming relative to the baseline year--i.e., the central year of the 1986-2005 baseline). CONUS temperature values must be greater than or equal to zero degrees Celsius.
#'    * Users can convert global temperatures to CONUS temperatures using [FrEDI::convertTemps]`(from = "global")` (or by specifying [FrEDI::import_inputs]`(temptype = "global")` when using [FrEDI::import_inputs()] to import a temperature scenario from a CSV file).
#'    * `temp` requires a data frame object with two columns with names `"year"`, and `"temp_C"` containing the year and CONUS temperatures in degrees Celsius, respectively.
#'    * Temperature inputs must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for temperature (i.e., `inputsList = list(temp = NULL)`, [FrEDI::run_fredi()] uses a default temperature scenario.
#' * __SLR Inputs.__ The input SLR scenario requires values for changes in global mean sea level rise (GMSL) heights in centimeters (cm). GMSL heights must be greater than or equal to zero.
#'    * `slr` requires a data frame object with two columns with names `"year"`, `"slr_cm"` containing the year and global mean sea level rise (GMSL) in centimeters, respectively.
#'    * SLR inputs must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for SLR (i.e., `inputsList = list(slr = NULL)`, [FrEDI::run_fredi()] first converts the input or default CONUS temperature scenario to global temperatures (using [FrEDI::convertTemps()]) and then converts the global temperatures to a global mean sea level rise (GMSL) height in centimeters (using [FrEDI::temps2slr()]).
#'
#'
#' [FrEDI::run_fredi()] linearly interpolates missing annual values for all input scenarios using non-missing values (each scenario requires at least two non-missing values as detailed above for each scenario type). After interpolation of the input scenarios, [FrEDI::run_fredi()] subsets the input scenarios to values within the analysis period.
#'
#' * Temperatures are interpolated using 1995 as the baseline year (i.e., the central year of the 1986-2005 baseline) and GMSL is interpolated using 2000 as the baseline year. In other words, temperature (in degrees Celsius) is set to zero for the year 1995, whereas GMSL is set to zero for the year 2000. The interpolated temperature and GMSL scenarios are combined into a column called `driverValue`, along with additional columns for year, the driver unit (column `"driverUnit"`, with `driverUnit = "degrees Celsius"` and `driverUnit = "cm"` for temperature- and SLR-driven sectors, respectively), and the associated model type (column `"model_type"`, with `model_type = "GCM"` and `model_type = "SLR"` for temperature- and SLR-driven sectors, respectively.
#' * [FrEDI::run_fredi()] calculates national population from state-level values and then calculates GDP per capita from values for GDP and national population. Values for state population, national population, national GDP (in 2015$), and national per capita GDP (in 2015$/capita) are provided in the results data frame in columns `"pop"`, `"national_pop"`, `"gdp_usd"`, and `"gdp_percap"`, respectively.
#'
#' By default, [FrEDI::run_fredi()] will calculate impacts for all sectors included in the tool. Alternatively, users can pass a character vector specifying a single sector or a subset of sectors using the `sectorList` argument. To see a list of sectors included within [FrEDI], run [FrEDI::get_sectorInfo()]. If `sectorList = NULL` (default), all sectors are included.
#'
#' By default, [FrEDI::run_fredi()] calculates impacts starting in the year 2010 and ending in 2100. Specify an alternative end year for the analysis using the `maxYear` argument. `maxYear` has a default value of `2100` and minimum and maximum values of `2011` and `2300`, respectively. Alternatively, users can set argument `thru2300 = TRUE` to override the `maxYear` argument and set `maxYear = 2300`. Note that the default scenarios included within [FrEDI] stop in the year 2100; users must provide custom input scenarios out to the desired end year **and** specify a `maxYear >= 2100` (and `maxYear <= 2300`) in order to return non-missing values for years after 2100.
#'
#' Annual impacts for each sector, variant, impact type, and impact year combination included in the model are calculated by multiplying scaled impacts by a physical scalar and economic scalars and multipliers. Some sectors use Value of a Statistical Life (VSL) to adjust the value non-linearly over time. [FrEDI::run_fredi()] uses a default value of `elasticity = 1`to adjust VSL for applicable sectors and impacts (the default value of `elasticity = 1` keeps VSL constant over time). A custom elasticity can be passed to the `elasticity` argument. Applicable sectors and impacts are ***Air Quality** (all impact types), **ATS Temperature-Related Mortality**  (`impactType = "N/A"`; i.e., all impact types), **CIL Temperature-Related Mortality**, **Extreme Temperature** (all impact types), **Suicide** (`impactType = "N/A"`; i.e., all impact types), **Southwest Dust** (`impactType = "All Mortality"`), **Valley Fever** (`impactType = "Mortality"`), **Vibriosis** (`impactType = "N/A"`; i.e., all impact types), and **Wildfire** (`impactType = "Mortality"`).
#'
#' [FrEDI::run_fredi()] aggregates or summarizes results to level(s) of aggregation specified by the user (passed to `aggLevels`) using the post-processing helper function [FrEDI::aggregate_impacts()]. Users can specify all aggregation levels at once by specifying `aggLevels = "all"` (default) or no aggregation levels (`aggLevels = "none"`). Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels = "national"`), averaging across model types and models (`aggLevels = "modelaverage"`), summing over all impact types (`aggLevels = "impacttype"`), and interpolating between impact year estimates (`aggLevels = "impactYear"`).
#'
#' If the user specifies `aggLevels = "none"`, [FrEDI::run_fredi()] returns a data frame with columns: `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, `"model"`, `"sectorprimary"`, `"includeaggregate"`, `"physicalmeasure"`, `"driverType"`, `"driverUnit"`, `"driverValue"`, `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, `"pop"`, `"year"`, `"physical_impacts"`, and `"annual_impacts"`.
#'
#'
#' * Columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, and `"model"` all contain observation identifiers (sector name, variant, impact type, impact year, region, state, state postal code abbreviation, model type, and model, respectively).
#' * Columns `"sectorprimary"` and `"includeaggregate"` contain values that provide information about how to treat sectors and when aggregating over sectors (e.g., summing impacts across sectors). Note that [FrEDI::FrEDI] does not currently provide functionality to aggregate over sectors; this information is provided for user convenience.
#'     * Column `"sectorprimary"` contains values indicating which variant to use as the primary one for each sector: `sectorprimary = 1`for primary variants and `sectorprimary = 0` for non-primary variants. When aggregating impacts over sectors, users should filter the outputs of `run_fredi()` to variants with `sectorprimary == 1`.
#'     * Column `"includeaggregate"` contains values that provide information about how to treat sectors when aggregating over sectors (e.g., summing impacts across sectors). Sectors that have a value of `includeaggregate == 0` should be dropped when aggregating results over sectors, to avoid potential double-counting of impacts for similar sectors. For instance, sectors __ATS Temperature-Related Mortality__, __CIL Temperature-Related Mortality__, and __Extreme Temperature__ have values for temperature-related mortality. To avoid double counting, outputs of [FrEDI::run_fredi()] should be filtered to values for which `includeaggregate > 0`. Sectors with a value of `includeaggregate > 0` can be included when aggregating over sectors; most sectors with a value of `includeaggregate > 0` will have a value of `includeaggregate = 1`. Values of `includeaggregate > 1` flag additional information about sectors; currently, only the __Suicide__ sector has a value of `includeaggregate > 1`, with a value of `includeaggregate = 2`. This flag indicates that the impacts from __Suicide__ can be included when summing values across sectors, but may have some overlap with impacts from __ATS Temperature-Related Mortality__. For more information about the potential overlap between impacts for __ATS Temperature-Related Mortality__ and __Suicide__, visit the technical documentation at <https://epa.gov/cira/FrEDI/>,
#' * Columns `"driverType"`, `"driverUnit"`, and `"driverValue"` contain information about the temperature and SLR scenarios.
#' * Columns `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, and `"pop"` contain information about the GDP and population scenarios.
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
#' After aggregating values, [FrEDI::aggregate_impacts()] joins the data frame of impacts with information about `"driverType"`, `"driverUnit"`, `"driverValue"`, `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, and `"pop"`.
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
#' ### Run function with defaults
#' run1 <- run_fredi()
#'
#' ### Load temperature scenarios and glimpse data
#' data("gcamScenarios")
#' gcamScenarios |> glimpse()
#'
#' ### Load population scenario and glimpse data
#' data("popDefault")
#' popDefault |> glimpse()
#'
#' ### Subset temperature scenario
#' temps1 <- gcamScenarios |> filter(scenario=="ECS_3.0_ref")
#' temps1 <- temps1 |> select(year, temp_C_conus)
#'
#' ### Run custom scenario
#' run2 <- run_fredi(inputsList=list(temp=temps1, pop=popDefault))
#'
#' ### Load scenarios from file:
#' scenariosPath <- system.file(package="FrEDI") |> file.path("extdata","scenarios")
#' scenariosPath |> list.files()
#'
#'
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath |> file.path("gcamDefault.csv")
#'
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath |> file.path("popDefault.csv")
#'
#' ### Import inputs
#' x_inputs <- import_inputs(inputsList=list(slr=slrInputFile, pop=popInputFile), popArea="state")
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
#'
#'
#'
#'
## run_fredi ----------------
### This function creates a data frame of sector impacts for default values or scenario inputs.
run_fredi <- function(
    inputsList = list(temp=NULL, slr=NULL, gdp=NULL, pop=NULL), ### List of inputs
    sectorList = NULL, ### Vector of sectors to get results for
    aggLevels  = c("national", "modelaverage", "impactyear", "impacttype"), ### Aggregation levels
    elasticity = 1,     ### Override value for elasticity for economic values
    maxYear    = 2100,  ### Maximum year for the analysis period
    thru2300   = FALSE, ### Whether to run FrEDI through 2300
    outputList = FALSE, ### Whether to return input arguments as well as results. [If TRUE], returns a list instead of a data frame
    allCols    = FALSE, ### Whether to include additional columns in output
    silent     = TRUE   ### Whether to message the user
){
  ### Load Objects ######
  ### Assign data objects to objects in this namespace
  ### Assign FrEDI config
  fredi_config <- rDataList[["fredi_config"]]
  # fredi_config |> names() |> print()
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}

  ### Other values
  co_sectors   <- "co_sectors"    |> get_frediDataObj("frediData")
  co_modTypes  <- "co_modelTypes" |> get_frediDataObj("frediData")


  ###### Set up the environment ######
  ### Level of messaging (default is to message the user)
  msgUser      <- !silent
  msgN         <- "\n"
  msg0         <- ""
  msg1         <- msg0 |> paste0("\t")

  ### Model years and NPD (FrEDI past 2100)
  minYear      <- minYear0
  maxYear      <- thru2300 |> ifelse(npdYear0, maxYear)
  do_npd       <- maxYear > maxYear0

  ###### ** Return List ######
  ### Initialize list to return
  returnList   <- list() ### List to return args, scenarios, and statuses
  argsList     <- list() ### List of arguments
  statusList   <- list() ### List to return custom or default

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
  aggLevels  <- aggLevels |> get_matches(y=aggList1)
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
  if(sectorList |> is.null()) sectorList <- co_sectors |> pull(sector_label)
  ### Sector names & labels
  dfSectors    <- co_sectors |> filter((sector_label |> tolower()) %in% (sectorList |> tolower()))
  sectorIds    <- dfSectors  |> pull(sector_id)
  sectorLbls   <- dfSectors  |> pull(sector_label)
  sectors0     <- co_sectors |> pull(sector_label) |> unique()

  ### Check sectors
  # sectorList |> print(); sectorLbls |> print()
  naSectors0   <- sectorList |> get_matches(y=sectorLbls, matches=F, type="values")
  ### Message the user
  if(naSectors0 |> length()){
    naSectors0  <- "'" |> paste0(naSectors0 |> paste(collapse="', '")) |> paste0("'")
    msgSectors0 <- "'" |> paste0(sectors0   |> paste(collapse="', '")) |> paste0("'")
    1 |> get_msgPrefix(newline=T) |> paste0("Warning! Error in `sectorList`:") |> message()
    2 |> get_msgPrefix() |> paste0("Impacts are not available for sectors: ") |> message()
    3 |> get_msgPrefix() |> paste0(naSectors0) |> message()
    2 |> get_msgPrefix(newline=T) |> paste0("Availabler sectors: ") |> message()
    3 |> get_msgPrefix()|> paste0(msgSectors0) |> message()
    return()
  } ### End if(length(missing_sectors)>=1)
  ### Update in list
  if(outputList) {argsList[["sectorList"]] <- sectorLbls}


  ###### ** Model Types List ######
  ### Which model types are in play based on sector selection
  modTypes0    <- co_sectors  |> filter(sector_label %in% sectorLbls) |> pull(modelType) |> unique()
  # sectorLbls |> print(); modTypes0 |> print(); co_modTypes |> glimpse()
  modTypesIn0  <- co_modTypes |> filter(modelType_id %in% modTypes0 ) |> pull(inputName) |> unique()
  doSlr        <- ("slr" %in% modTypes0)
  doGcm        <- ("gcm" %in% modTypes0)
  if(doSlr) modTypesIn <- c("temp") |> c(modTypesIn0)
  else      modTypesIn <- modTypesIn0
  modInputs0   <- c("gdp", "pop") |> c(modTypesIn)

  ###### Inputs List ######
  ###### ** Input Info ######
  paste0("Checking scenarios...") |> message()
  ### Add info to data
  co_inputInfo <- "co_inputInfo" |> get_frediDataObj("frediData")
  co_inputInfo <- co_inputInfo |> mutate(ref_year = c(1995, 2000, 2010, 2010))
  co_inputInfo <- co_inputInfo |> mutate(min_year = c(2000, 2000, 2010, 2010))
  co_inputInfo <- co_inputInfo |> mutate(max_year = maxYear)
  co_inputInfo <- co_inputInfo |> filter(inputName %in% modInputs0)

  ### Initialize subset
  df_inputInfo <- co_inputInfo

  ### Input info
  inNames0     <- co_inputInfo |> pull(inputName)
  # inNames0 |> print()

  ###### ** Input Defaults ######
  inputDefs    <- inNames0 |> map(function(name0){
    ### Objects
    doTemp0  <- "temp" %in% name0
    doSlr0   <- "slr"  %in% name0
    defName0 <- (doTemp0 | doSlr0) |> ifelse("gcam", name0) |> paste0("_default")
    df0      <- defName0 |> get_frediDataObj("scenarioData")
    ### Format data
    if(doTemp0) df0 <- df0 |> select(c("year", "temp_C_conus")) |> rename_at(c("temp_C_conus"), ~"temp_C")
    if(doSlr0 ) df0 <- df0 |> select(c("year", "slr_cm"      ))
    ### Return
    return(df0)
  }) |> set_names(inNames0)


  ###### ** Input Columns ######
  ### Get list with expected name of columns used for unique ids
  ### Get list with expected name of column containing values
  valCols0     <- co_inputInfo |> pull(valueCol) |> as.list() |> set_names(inNames0)
  idCols0      <- list(valCols0=valCols0, df0=inputDefs[inNames0]) |> pmap(function(valCols0, df0){
    df0 |> names() |> get_matches(y=valCols0, matches=F)
  }) |> set_names(inNames0)


  ###### ** Valid Inputs & Input Info ######
  ### Figure out which inputs are not null, and filter to that list
  ### inputsList Names
  inNames      <- inputsList |> names()
  inLength     <- inputsList |> length()
  hasNames     <- inNames    |> length()
  if(hasNames) {
    # inNames      <- inputsList |> names()
    # inNames |> print()
    # inputsList |> map(glimpse)
    # inWhich      <- inNames    |> map(function(name0, list0=inputsList){(!(list0[[name0]] |> is.null())) |> which()}) |> unlist() |> unique()
    inWhich      <- inNames    |> map(function(name0, list0=inputsList){!(list0[[name0]] |> is.null())}) |> unlist() |> which()
    ### Filter to values that are not NULL
    inputsList   <- inputsList[inWhich]
    inNames      <- inputsList |> names()
    rm(inWhich)
    ### Check which input names are in the user-provided list
    inWhich      <- inNames %in% inNames0
    inNames      <- inNames[inWhich]
    inputsList   <- inputsList[inNames]
    hasAnyInputs <- inNames |> length()
    rm(inWhich)
    # inNames |> print()
  } else if (inLength) {
    paste0(msg1) |> paste0("Error! `inputsList` argument requires a list with named elements.") |> message()
    msgN |> paste0(msg1) |> paste0("Exiting...") |> message()
    return()
  } else {
    hasAnyInputs <- FALSE
  } ### End if(!hasInputs)


  ###### ** Check Inputs ######
  ### Filter to valid inputs & get info
  ### Reorganize inputs list
  df_inputInfo <- df_inputInfo |> filter(inputName %in% inNames)
  inNames      <- df_inputInfo |> pull(inputName)

  ### Create logicals and initialize inputs list
  if(hasAnyInputs) {
    ### Min ad max years
    minYrs0    <- inNames |> map(function(name0, df0=df_inputInfo){df0 |> filter(inputName %in% name0) |> pull(min_year) |> unique()}) |> set_names(inNames)
    maxYrs0    <- inNames |> map(function(name0, df0=df_inputInfo){df0 |> filter(inputName %in% name0) |> pull(max_year) |> unique()}) |> set_names(inNames)

    ### Check inputs
    inputsList <- list(
      inputName = inNames,
      inputDf   = inputsList[inNames],
      idCol     = idCols0   [inNames],
      valCol    = valCols0  [inNames],
      yearMin   = minYrs0,
      yearMax   = maxYrs0,
      module    = "fredi" |> rep(inNames |> length())
    ) |>
      pmap(check_input_data) |>
      set_names(inNames)
    rm(minYrs0, maxYrs0)

    ### Check again for inputs
    ### Filter to values that are not NULL
    # inWhich      <- inNames |> map(function(name0, list0=inputsList){(!(list0[[name0]] |> is.null())) |> which()}) |> unlist() |> unique()
    inWhich      <- inNames    |> map(function(name0, list0=inputsList){!(list0[[name0]] |> is.null())}) |> unlist() |> which()
    inputsList   <- inputsList[inWhich]
    inNames      <- inputsList |> names()
    rm(inWhich)
  } ### if(hasAnyInputs)


  ### Update list
  ### For each input:
  ### - Make sure values are at correct range
  ### - Update in status list
  if(outputList){
    statusList[["inputsList"]] <- inputsList |> map(function(df0){
      df0 |> length() |> as.logical() |> get_returnListStatus()
    }) |> set_names(inNames)
    argsList  [["inputsList"]] <- inputsList
  } ### End if(outputList)


  ### If SLR is missing but user provided a temperature scenario, update with new temperature scenario
  ### If there are no GCM sectors, drop temperature
  if(doSlr) {
    if(!("slr" %in% inNames)) {
      if("temp" %in% inNames) {
        inputsList <- inputsList |> (function(list0, y="slr"){list0[!((list0 |> names() %in% y))]})()
        inputsList[["slr"]] <- inputsList[["temp"]]
        inNames    <- inNames |> c("slr") |> unique()
      } ### End if("temp" %in% inNames)
    } ### End if(!("slr" %in% inNames))
  } ### End if(doSlr0)
  # inNames |> print()

  ### If !doGcm, drop temperatures if present
  if(!doGcm) {
    inputsList <- inputsList |> (function(list0, y="temp"){list0[!((list0 |> names() %in% y))]})()
    inputDefs  <- inputDefs  |> (function(list0, y="temp"){list0[!((list0 |> names() %in% y))]})()
    inNames0   <- inNames0   |> get_matches(y="temp", matches=F)
    inNames    <- inNames    |> get_matches(y="temp", matches=F)
  } ### End if(!doGcm)

  ### Update values
  # inNames |> print()
  hasInputs    <- inNames      |> length()
  # df_inputInfo <- co_inputInfo |> filter(inputName %in% inNames )

  ### Iterate over list and format values
  if(hasInputs) {
    inputsList   <- list(
      name0     = inNames,
      df0       = inputsList,
      hasInput0 = TRUE |> rep(inNames |> length()),
      idCols0   = idCols0 [inNames],
      valCols0  = valCols0[inNames]
    ) |> pmap(function(df0, name0, hasInput0, idCols0, valCols0){
      df0 |> format_inputScenarios(
        name0     = name0,
        hasInput0 = hasInput0,
        idCols0   = idCols0,
        valCols0  = valCols0,
        minYear   = minYear,
        maxYear   = maxYear,
        info0     = co_inputInfo
      ) ### End format_inputScenarios
    }) |> set_names(inNames)
  } ### End if(hasInputs)

  ### Update inputs with defaults if values are missing
  inputsList   <- inNames0 |> (function(names0, list0=inputDefs, list1=inputsList){
    ### Filter to list
    list0    <- list0[names0]
    ### List names
    names0   <- list0 |> names()
    ### If user provided a scenario, update defaults list
    for(name_i in names0) {
      df_i     <- list1[[name_i]]
      has_i    <- df_i |> length()
      if(has_i) list0[[name_i]] <- df_i
      rm(name_i, df_i, has_i)
    } ### End for(name_i in names0)
    ### Return
    return(list0)
  })()
  # inputsList |> names() |> print()
  ### Update names
  inNames      <- inputsList |> names()
  df_inputInfo <- co_inputInfo |> filter(inputName %in% inNames)

  ### Filter to lists
  inputsList   <- inputsList |> map(function(df0, minYr0=minYear, maxYr0=maxYear){
    df0 <- df0 |> filter(year >= minYear, year <= maxYear)
    return(df0)
  }) |> set_names(inNames)

  ### Update returnList with Scenario Input Data
  if(outputList){
    returnList[["scenarios"]] <- inputsList |> set_names(inNames)
  } ### End if(outputList)

  ###### Scenarios ######
  ###### ** Physical Driver Scenario  ######
  ### Select columns
  filter0    <- c("temp", "slr") |> get_matches(y=inNames)
  df_drivers <- inputsList[filter0] |> combine_driverScenarios(info0 = df_inputInfo)
  mTypes0    <- df_drivers |> pull(modelType) |> unique()
  # df_drivers <- df_drivers |> filter(year >= minYear, year <= maxYear)
  rm(filter0)
  # return(df_drivers)



  ###### ** Socioeconomic Driver Scenario ######
  ### Get GDP and Population scenarios
  gdp_df       <- inputsList[["gdp"]]
  pop_df       <- inputsList[["pop"]]
  ### Convert region to region IDs
  pop_df       <- pop_df |> mutate(region = region |> str_replace_all("\\.|_|-| ", ""))
  ### Calculate national population and update national scenario
  seScenario   <- gdp_df |> create_nationalScenario(pop0 = pop_df)
  # return(seScenario)
  # seScenario |> pull(region) |> unique() |> print()
  rm(gdp_df, pop_df)
  # seScenario |> glimpse()


  ### Calculate Impacts ----------------
  #### Select/Filter Scenario Info and Scalars ----------------
  #### Get Scalar Values ----------------
  ### Calculate physical scalars and economic multipliers then calculate scalars
  paste0("Calculating impacts...") |> message()
  df_results   <- seScenario |> initialize_resultsDf(
    sectors    = sectorIds,
    mTypes     = mTypes0,
    minYr0     = minYear,
    maxYr0     = maxYear,
    elasticity = elasticity
  ) ### End initialize_resultsDf

  #### Calculate Scaled Impacts ----------------
  ### Get scaled impacts
  df_impacts   <- df_results |> calc_scaled_impacts_fredi(drivers0=df_drivers, minYr0=minYear, maxYr0=maxYear)

  #### Calculate Total Impacts ----------------
  ### Get impacts
  df_results   <- df_results |> calc_impacts_fredi(df1=df_impacts, minYr0=minYear, maxYr0=maxYear) |> ungroup()



  ###### Refactor Data ######
  ### Add in model info
  paste0("Formatting results", "...") |> message()

  ###### ** Get Labels ######
  ### Rename sector
  drop0       <- c("sector", "variant", "impactType", "impactYear", "region", "modelType", "model")
  renameAt0   <- drop0 |> paste0("_label") |> c("modelUnitDesc", "modelUnit_label", "modelUnitValue")
  renameTo0   <- drop0 |> c("driverType", "driverUnit", "driverValue")
  df_results  <- df_results |> select(-any_of(drop0))
  df_results  <- df_results |> rename_at(c(renameAt0), ~renameTo0)
  rm(drop0, renameAt0, renameTo0)
  # df_results |> glimpse()

  ### Rename model type
  renameAt0   <- c("modelType")
  renameTo0   <- c("model_type")
  df_results  <- df_results |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  # df_results |> pull(region) |> unique() |> print()
  # return(df_results)

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
  # df_results |> glimpse()
  # df_results |> pull(region) |> unique() |> print()
  # return(df_results)


  ###### Aggregation ######
  ### For regular use (i.e., not impactYears), simplify the data: groupCols0
  if(doAgg) {
    paste0("Aggregating impacts", "...") |> message()
    # aggLevels |> length(); doAgg |> print()
    group0     <- groupCols0
    df_results <- df_results |> aggregate_impacts(
      aggLevels   = aggLevels,
      groupByCols = group0,
      columns     = impactCols0
    ) |> ungroup() ### End aggregate_impacts
  } ### End if(doAgg)


  ###### Format Results ######
  ###### ** Arrange Columns ######
  ### Convert levels to character
  ### Order the rows, then order the columns
  arrange0   <- groupCols0 |> get_matches(y = df_results |> names()) |> c("year") |> unique()
  # arrange0 |> print()
  ### Select columns
  df_results <- df_results |> arrange_at(c(arrange0))
  df_results <- df_results |> relocate(any_of(select0))
  rm(arrange0)



  ###### ** Format as Tibble ######
  ### Update results in list
  df_results   <- df_results |> as_tibble()



  ###### ** Format Return Object ######
  ### Add items to list/reorganize list
  if(outputList) {
    ### Initialize list
    returnObj <- list()
    ### Add status list
    returnObj[["statusList"]] <- statusList
    rm(statusList)
    ### Add arguments
    returnObj[["argsList"  ]] <- argsList
    rm(argsList)
    ### Add scenarios
    returnObj[["scenarios" ]] <- returnList[["scenarios"]]
    rm(returnList)
    ### Add results
    returnObj[["results"   ]] <- df_results
    rm(df_results)
  } else {
    returnObj <- df_results
  } ### End if(outputList)



  ###### Return ######
  ### Message, clear unused memory, return
  message("\n", "Finished", ".")
  gc()
  return(returnObj)

} ### End function








