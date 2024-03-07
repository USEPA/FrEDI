###### Documentation ######
#' Project annual average climate change impacts throughout the 21st century for available sectors
#'
#'
#'
#' @description
#' This function allows users to project annual average climate change impacts through 2090 (2010-2090) for available sectors (see [FrEDI::get_sectorInfo()]), with the option to extend results to 2300 (2010-2300). Users may specify custom temperature, U.S. population, and GDP scenarios. The output is an R data frame object containing annual average impacts, by year, for each sector, variant, impact type, region, state, and model.
#'
#' As of FrEDI Version 4.0.1, [FrEDI::run_fredi()] calculates impacts at the state-level for the following sectors: **ATS Temperature-Related Mortality**, **Asphalt Roads**, **Climate-Driven Changes in Air Quality**, **Electricity Transmission and Distribution**, **Labor**, **Suicide**, **Transportation Impacts from High Tide Flooding**, **Urban Drainage**, **Wildfire**, **Wind Damage**. Eventually, all sectors will be converted to state-level. Sectors that have only region-level impacts will have values in the `"state"` and `"postal"` columns of the outputs data frame set to `"N/A"`.
#'
#' @param inputsList=NULL A list of named elements named elements (`names(inputsList) = c( "tempInput", "slrInput", "gdpInput", "popInput" )`), each containing data frames of custom temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and/or population scenarios, respectively, over a continuous period in the range 2010 to 2300. Temperature and sea level rise inputs should start in 2000 or earlier. Values for population and GDP scenarios can start in 2010 or earlier. Values for each scenario type must be within reasonable ranges. For more information, see [FrEDI::import_inputs()].
#'
#' @param sectorList=NULL A character vector indicating a selection of sectors for which to calculate results (see [FrEDI::get_sectorInfo()]). If `NULL`, all sectors are included (i.e., `sectorList=get_sectorInfo()`).
#'
#' @param aggLevels="all" Levels of aggregation at which to summarize data: one or more of `c("national"`, `"modelaverage"`, `"impactyear"`, `"impacttype"`, `"all"`, `"none")`. Defaults to all levels (i.e., `aggLevels= "all"`). Uses the same aggregation levels as [FrEDI::aggregate_impacts()]. Note that, if `"impacttype"` is in `aggLevels` (e.g., `aggLevels= "all"`), columns `"physical_measure"` and `"physical_impacts"` will be dropped from the results data frame. This is because aggregating over impact types for some sectors requires summing costs over different types of physical impacts, so reporting the physical impacts would be nonsensical.
#'
#' @param elasticity=1 A numeric value indicating an elasticity to use for adjusting VSL for applicable sectors and impacts (defaults to `elasticity=1`). Applicable sectors and impacts are: **Climate-Driven Changes in Air Quality** (all impact types), **ATS Temperature-Related Mortality**  (`impactType="N/A"`; i.e., all impact types), **CIL Temperature-Related Mortality**, **Extreme Temperature** (all impact types), **Suicide** (`impactType = "N/A"`; i.e., all impact types), **Southwest Dust** (`impactType= "All Mortality"`), **Valley Fever** (`impactType= "Mortality"`), **Vibriosis** (`impactType="N/A"`; i.e., all impact types), and **Wildfire** (`impactType = "Mortality"`).
#'
#' @param maxYear=2090 A numeric value indicating the maximum year for the analysis. The range for `maxYear` is `[2011, 2300]. `Defaults to `maxYear=2090`.
#'
#' @param thru2300=FALSE A ` TRUE/FALSE` shortcut that overrides the maxYear argument to run the model to 2300. Defaults to `thru2300=FALSE`.
#'
#' @param outputList=FALSE A ` TRUE/FALSE` value indicating whether to output results as a data frame object (`outputList = FALSE`, default) or to return a list of objects (`outputList = TRUE`) that includes information about model provenance (including input arguments and input scenarios) along with the data frame of results.
#'
#' @param allCols=FALSE A `TRUE/FALSE` value indicating whether to include intermediate column values in results (e.g., physical and economic multipliers). Used in testing. Note that aggregation levels must be set to `aggLevels="none"` to properly return the intermediate columns. Defaults to `allCols=FALSE`).
#'
#' @param silent=TRUE A `TRUE/FALSE` value indicating the level of messaging desired by the user (default=`TRUE`).
#'
#'
#'
#' @details This function allows users to project annual average climate change impacts through 2300 (2010-2300) for available sectors. [FrEDI::run_fredi()] is the main function in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information).
#'
#'
#' Users can specify an optional list of custom scenarios with `inputsList` (for more information on the format of inputs, see [FrEDI::import_inputs()]). The function [FrEDI::import_inputs()] can be used to importing custom scenarios from CSV files. [FrEDI::import_inputs()] returns a list with elements `tempInput`, `slrInput`, `gdpInput`, and `popInput`, with each containing a data frame with a custom scenario for temperature, GMSL, GDP, and state-level population, respectively. If a user imports scenarios using [FrEDI::import_inputs()], they can pass the outputs of [FrEDI::import_inputs()] directly to the [FrEDI::run_fredi()] argument `inputsList`. Note that the documentation for [FrEDI::import_inputs()] can also provide additional guidance and specification on the formats for each scenario type.
#'
#' If `inputsList=NULL`, [FrEDI::run_fredi()] uses defaults for temperature, SLR, GDP, and population. Otherwise, [FrEDI::run_fredi()] looks for a list object passed to the argument `inputsList`. Within that list, [FrEDI::run_fredi()] looks for list elements `tempInput`, `slrInput`, `gdpInput`, and `popInput` containing data frames with custom scenarios for temperature, GMSL, GDP, and regional population, respectively. [FrEDI::run_fredi()] will default back to the default scenarios for any list elements that empty or `NULL` (in other words, running `run_fredi( inputsList = list() )` returns the same outputs as running [FrEDI::run_fredi()]).
#'
#' * __Temperature Inputs.__ The input temperature scenario requires CONUS temperatures in degrees Celsius relative to 1995 (degrees of warming relative to the baseline year--i.e., the central year of the 1986-2005 baseline). CONUS temperature values must be greater than or equal to zero degrees Celsius.
#'    * Users can convert global temperatures to CONUS temperatures using [FrEDI::convertTemps]`(from="global")` (or by specifying [FrEDI::import_inputs]`( temptype = "global" )` when using [FrEDI::import_inputs()] to import a temperature scenario from a CSV file).
#'    * `tempInput` requires a data frame object with two columns with names `"year"`, and `"temp_C"` containing the year and CONUS temperatures in degrees Celsius, respectively.
#'    * Temperature inputs must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for temperature (i.e., `inputsList=list(tempInput=NULL)`, [FrEDI::run_fredi()] uses a default temperature scenario.
#' * __SLR Inputs.__ The input SLR scenario requires values for changes in global mean sea level rise (GMSL) heights in centimeters (cm). GMSL heights must be greater than or equal to zero.
#'    * `slrInput` requires a data frame object with two columns with names `"year"`, `"slr_cm"` containing the year and global mean sea level rise (GMSL) in centimeters, respectively.
#'    * SLR inputs must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for SLR (i.e., `inputsList=list(slrInput=NULL)`, [FrEDI::run_fredi()] first converts the input or default CONUS temperature scenario to global temperatures (using [FrEDI::convertTemps()]) and then converts the global temperatures to a global mean sea level rise (GMSL) height in centimeters (using [FrEDI::temps2slr()]).
#' * __GDP Inputs.__ The input scenario for gross domestic product (GDP) requires national GDP values in 2015$. GDP values must be greater than or equal to zero.
#'    * `gdpInput` requires a data frame object with five columns with names `"year"`, and `"gdp_usd"` containing the year and the national GDP, respectively. GDP values must be greater than or equal to zero.
#'    * GDP inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for GDP (i.e., `inputsList=list(gdpInput=NULL)`, [FrEDI::run_fredi()] uses a default GDP scenario.
#' * __Population Inputs.__ The input population scenario requires state-level population values. Population values must be greater than or equal to zero.
#'    * `popInput` requires a data frame object with five columns with names `"year"`, `"region"`, `"state"`, `"postal"`, and `"state_pop"` containing the year, the NCA region name, and the state, the postal code abbreviation, and the state population, respectively.
#'    * Population inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If the user does not specify an input scenario for population (i.e., `inputsList=list(popInput=NULL)`, [FrEDI::run_fredi()] uses a default population scenario.
#'
#'
#' [FrEDI::run_fredi()] linearly interpolates missing annual values for all input scenarios using non-missing values (each scenario requires at least two non-missing values as detailed above for each scenario type). After interpolation of the input scenarios, [FrEDI::run_fredi()] subsets the input scenarios to values within the analysis period.
#'
#' * Temperatures are interpolated using 1995 as the baseline year (i.e., the central year of the 1986-2005 baseline) and GMSL is interpolated using 2000 as the baseline year. In other words, temperature (in degrees Celsius) is set to zero for the year 1995, whereas GMSL is set to zero for the year 2000. The interpolated temperature and GMSL scenarios are combined into a column called `driverValue`, along with additional columns for year, the driver unit (column `"driverUnit"`, with `driverUnit= "degrees Celsius"` and `driverUnit= "cm"` for temperature- and SLR-driven sectors, respectively), and the associated model type (column `"model_type"`, with `model_type="GCM"` and `model_type="SLR"` for temperature- and SLR-driven sectors, respectively
#' * [FrEDI::run_fredi()] calculations national population from state-level values and then calculates GDP per capita from values for GDP and national population. Values for state population, national population, national GDP (in 2015$), and national per capita GDP (in 2015$/capita) are provided in the results data frame in columns `"state_pop"`, `"national_pop"`, `"gdp_usd"`, and `"gdp_percap"`, respectively.
#'
#' By default, [FrEDI::run_fredi()] will calculate impacts for all sectors included in the tool. Alternatively, users can pass a character vector specifying a single sector or a subset of sectors using the `sectorList` argument. To see a list of sectors included within [FrEDI], run [FrEDI::get_sectorInfo()]. If `sectorList= NULL` (default), all sectors are included.
#'
#' By default, [FrEDI::run_fredi()] calculates impacts starting in the year 2010 and ending in 2090. Specify an alternative end year for the analysis using the `maxYear` argument. `maxYear` has a default value of `2090` and minimum and maximum values of `2011` and `2300`, respectively. Alternatively, users can set argument `thru2300=TRUE` to override the `maxYear` argument and set `maxYear=2300`. Note that the default scenarios included within [FrEDI] stop in the year 2090; users must provide custom input scenarios out to the desired end year **and** specify a `maxYear>=2090` (and `maxYear<=2300`) in order to return non-missing values for years after 2090.
#'
#' Annual impacts for each sector, variant, impact type, and impact year combination included in the model are calculated by multiplying scaled climate impacts by a physical scalar and economic scalars and multipliers. Some sectors use Value of a Statistical Life (VSL) to adjust the value non-linearly over time. [FrEDI::run_fredi()] uses a default value of `elasticity=1`to adjust VSL for applicable sectors and impacts (the default value of `elasticity=1` keeps VSL constant over time). A custom elasticity can be passed to the `elasticity` argument.Applicable sectors and impacts are ***Climate-Driven Changes in Air Quality** (all impact types), **ATS Temperature-Related Mortality**  (`impactType="N/A"`; i.e., all impact types), **CIL Temperature-Related Mortality**, **Extreme Temperature** (all impact types), **Suicide** (`impactType = "N/A"`; i.e., all impact types), **Southwest Dust** (`impactType= "All Mortality"`), **Valley Fever** (`impactType= "Mortality"`), **Vibriosis** (`impactType="N/A"`; i.e., all impact types), and **Wildfire** (`impactType = "Mortality"`).
#'
#' [FrEDI::run_fredi()] aggregates or summarizes results to level(s) of aggregation specified by the user (passed to `aggLevels`) using the post-processing helper function [FrEDI::aggregate_impacts()]. Users can specify all aggregation levels at once by specifying `aggLevels= "all"` (default) or no aggregation levels (`aggLevels= "none"`). Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels= "national"`), averaging across model types and models (`aggLevels= "modelaverage"`), summing over all impact types (`aggLevels= "impacttype"`), and interpolating between impact year estimates (`aggLevels= "impactYear"`).
#'
#' If the user specifies `aggLevels= "none"`, [FrEDI::run_fredi()] returns a data frame with columns: `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, `"model"`, `"sectorprimary"`, `"includeaggregate"`, `"physicalmeasure"`, `"driverType"`, `"driverUnit"`, `"driverValue"`, `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, `"state_pop"`, `"year"`, `"physical_impacts"`, and `"annual_impacts"`.
#'
#'
#' * Columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, and `"model"` all contain observation identifiers (sector name, variant (i.e., sector variant or adaptation name), impact type, impact year, region, state, state postal code, model type, and model, respectively).
#' * Column `"sectorprimary"` contains values indicating which variant (i.e., sector variant or adaptation name) is the primary one for the sector (`sectorprimary=1`for primary variants and `sectorprimary=0` for non-primary variants). This column can be used to filter the outputs of [FrEDI::run_fredi()] (e.g., as might be done before aggregating impacts over sectors).
#' * Column `"includeaggregate"` contains values indicating which sectors should be included when aggregating over sectors (`includeaggregate=1`for primary sectors and `includeaggregate=0` for non-primary sectors). For instance, sectors __ATS Temperature-Related Mortality__, __CIL Temperature-Related Mortality__, and __Extreme Temperature__ have values for temperature-related mortality. To avoid double counting, outputs of [FrEDI::run_fredi()] should be filtered to values for which `sectorprimary==1` and `includeaggregate=1`.
#' * Columns `"driverType"`, `"driverUnit"`, and `"driverValue"` contain information about the temperature and SLR scenarios.
#' * Columns `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, and `"state_pop"` contain information about the GDP and population scenarios.
#' * Columns `"physicalmeasure"` and `"physical_impacts"` contain information about physical impacts.
#' * Column `"annual_impacts"` contains information on the economic value associated with annual impacts.
#'
#'
#' If the user specifies `aggLevels= "all"` or other combinations of aggregation levels, [FrEDI::run_fredi()] passes the results data frame and the `aggLevels` argument to the [FrEDI::aggregate_impacts()] function. [FrEDI::aggregate_impacts()] then performs the following calculations, using the default grouping columns for the [FrEDI::aggregate_impacts()]: `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, `"model"`, `"sectorprimary"`, `"includeaggregate"`, `"physicalmeasure"`, and `"year"` (note that the `"variant"` column referred to below contains information about the variant name (or `“N/A”`), as applicable).
#'
#' \tabular{ll}{
#' \strong{Aggregation Level} \tab \strong{Description} \cr
#' *`impactyear`* \tab To aggregate over impact years, [FrEDI::aggregate_impacts()] first separates results for sectors with only one impact year estimate (i.e., `impactYear= "N/A"`) from from observations with multiple impact year estimates (i.e., sectors with results for both `impactYear= "2010"` and `impactYear= "2090"`). For these sectors with multiple impact years, physical impacts and annual costs (columns `"physical_impacts"` and `"annual_impacts"`) are linearly interpolated between impact year estimates. For any model run years above 2090, annual results for sectors with multiple impact years return the 2090 estimate. The interpolated values are then row-bound to the results for sectors with a single impact year estimate, and column `impactYear` set to `impactYear= "Interpolation"` for all values. If `"impactyear"` is included in `aggLevels` (e.g., `aggLevels= "all"`), [FrEDI::aggregate_impacts()] aggregates over impact years before performing other types of aggregation. \cr
#'
#' *`modelaverage`* \tab To aggregate over models for temperature-driven sectors, [FrEDI::aggregate_impacts()] averages physical impacts and annual costs (columns `"physical_impacts"` and `"annual_impacts"`, respectively) across all GCM models present in the data. [FrEDI::aggregate_impacts()] drops the column `"model"` from the grouping columns when averaging over models. Averages exclude observations with missing values. However, If all values within a grouping are missing, the model average is set to `NA`. The values in column `"model"` are set to `"Average"` for model averages and the model averages data frame is then row-bound to the main results data frame. For SLR-driven sectors, there is no need for additional model aggregation; these values already have `model="Interpolation"`. If `"modelaverage"` is included in `aggLevels` (e.g., `aggLevels= "all"`), [FrEDI::aggregate_impacts()] first aggregates over impact years  (if `"impactyear"` present in `aggLevels` or if `aggLevels="all"`) before aggregating over models.\cr
#'
#' *`national`* \tab To aggregate values to the national level, [FrEDI::aggregate_impacts()] sums physical impacts and annual costs (columns `"physical_impacts"` and `"annual_impacts"`, respectively) across all regions present in the data. [FrEDI::aggregate_impacts()] drops the columns `"region"`, `"state"`, and `"postal"` when summing over states and regions. Years which have missing column data for all regions return as `NA`. Values for column `"region"` are set to `"National Total"`; values for column `"state"` are set to `All`, and values for column `"postal"` are set to `US`. The data frame with national totals is then row-bound to the main results data frame. If `"national"` is included in `aggLevels` (e.g., `aggLevels= "all"`), [FrEDI::aggregate_impacts()] first aggregates over impact years and/or models (if `"impactyear"` and/or `"modelaverage"` are present in `aggLevels` or if `aggLevels= "all"`) before aggregating over models.\cr
#'
#' *`impacttype`* \tab To aggregate values over impact types, [FrEDI::aggregate_impacts()] sums annual impacts (column `"annual_impacts"`) across all impact types for each sector. [FrEDI::aggregate_impacts()] drops the column `"impactType"` and `"physicalmeasure"` from the grouping columns when summing over impact types. Years which have missing column data for all impact types return as `NA`. All values in column `"impactType"` are set to `"all"`. Aggregating over impact types, drops columns related to physical impacts (i.e., columns `"physicalmeasure"` and `"physical_impacts"`). These columns are dropped since aggregating over impact types for some sectors requires summing costs over different types of physical impacts, so reporting the physical impacts would be nonsensical.\cr
#' }
#'
#' After aggregating values, [FrEDI::aggregate_impacts()] joins the data frame of impacts with information about `"driverType"`, `"driverUnit"`, `"driverValue"`, `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, and `"state_pop"`.
#'
#' If `outputList=FALSE` (default), [FrEDI::run_fredi()] returns a data frame of annual average impacts over the analysis period, for each sector, variant, impact type, impact year, region, state, model type (`"GCM"` or `"SLR"`), and model. If `outputList=TRUE`, in addition to the data frame of impacts, [FrEDI::run_fredi()] returns a list object containing information about values for function arguments, driver scenarios, and population and GDP scenarios.
#'
#'
#'
#' @return
#' If `outputList=FALSE`, the output of [FrEDI::run_fredi()] is a dataframe object (described above) containing annual average impacts over the analysis period, for each sector, variant, impact type, impact year, region, state, and model (GCM name for temperature-driven sectors and "Interpolation" for SLR-driven sectors).
#'
#' If `outputList=TRUE`, [FrEDI::run_fredi()] returns a list object containing the following:
#'
#' * __`statusList`__. A list with values for the arguments passed to [FrEDI::run_fredi()] (including defaults if unspecified).
#' * __`argsList`__. A list with elements named after [FrEDI::run_fredi()] arguments, containing the values of the arguments passed to [FrEDI::run_fredi()] (or default values if unspecified).
#' * __`scenarios`__. A list with named elements `"temp"`, `"slr"`, `"gdp"`, and `"pop"` -- each containing the scenarios for temperature, SLR, GDP, and population as used by the model in calculating impacts.
#' * __`results`__. Containing a data frame of annual impacts (i.e., the same data frame returned if `outputList=FALSE`).
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
#'
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath |> file.path("slr_from_GCAM.csv")
#'
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath |> file.path("State ICLUS Population.csv")
#'
#' ### Import inputs
#' x_inputs <- import_inputs(
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
#' run4 <- run_fredi(sectorList="ATS Temperature-Related Mortality", aggLevels="none", elasticity=1)
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
    elasticity = 1, ### Override value for elasticity for economic values
    maxYear    = 2090,
    thru2300   = FALSE,
    outputList = FALSE, ### Whether to return input arguments as well as results. [If TRUE], returns a list instead of a data frame
    allCols    = FALSE, ### Whether to include additional columns in output
    silent     = TRUE   ### Whether to message the user
){
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user)
  msgUser   <- !silent
  ### Uncomment for allCols
  doPrimary <- F  ### whether to filter to primary impacts
  ### Model years and NPD (FrEDI past 2090)
  refYear   <- 2090
  npdYear   <- 2300
  maxYear0  <- thru2300 |> ifelse(npdYear, maxYear)
  do_npd    <- maxYear0 > refYear


  ###### ** Return List ######
  ### Initialize list to return
  returnList <- list() ### List to return args, scenarios, and statuses
  argsList   <- list() ### List of arguments
  statusList <- list() ### List to return custom or default


  ### Initialize return list
  if(outputList) {returnList[["scenarios"]] <- list()}


  ### Initialize status list
  ### Add statuses:
  ### - inputsList items and aggLevels assessed further below
  ### Add to list
  if(outputList){
    statusList[["inputsList"]] <- inputsList
    statusList[["sectorList"]] <- (!(sectorList |> is.null())) |> ifelse("Default", "Custom")
    statusList[["aggLevels" ]] <- "placeholder"
    statusList[["elasticity"]] <- (elasticity == 1) |> ifelse("Default", "Custom")
    statusList[["maxYear"   ]] <- (maxYear == refYear & !thru2300) |> ifelse("Default", "Custom")
    statusList[["thru2300"  ]] <- (!thru2300  ) |> ifelse("Default", "Custom")
    # statusList[["outputList"]] <- (!outputList) |> ifelse("Default", "Custom")
    statusList[["allCols"   ]] <- (!allCols   ) |> ifelse("Default", "Custom")
    statusList[["silent"    ]] <- ( silent    ) |> ifelse("Default", "Custom")
  } ### End if(outputList)


  ### Initialize arguments list
  ### - inputsList items, sectorList, and aggLevels assessed further below
  if(outputList){
    argsList[["inputsList"]] <- inputsList
    argsList[["sectorList"]] <- "placeholder"
    argsList[["aggLevels" ]] <- "placeholder"
    argsList[["elasticity"]] <- elasticity
    argsList[["maxYear"   ]] <- maxYear0
    argsList[["thru2300"  ]] <- thru2300
    # argsList[["outputList"]] <- outputList
    argsList[["allCols"   ]] <- allCols
    argsList[["silent"    ]] <- silent
  } ### End if(outputList)


  ###### ** Load Objects ######
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


  ###### ** Aggregation Levels  ######
  ### Types of summarization to do: default
  ### Aggregation levels
  aggList0   <- aggList0  |> tolower()
  aggLevels  <- aggLevels |> tolower()
  ### Update status list
  statusList[["aggLevels" ]] <- ("all" %in% aggLevels | (aggLevels %in% aggList0) |> all()) |> ifelse("Default", "Custom")
  # aggList0   <- c("national", "modelaverage", "impactyear", "impacttype")
  aggList1   <- aggList0  |> c("all", "none")
  aggLevels  <- aggLevels |> (function(y, z=aggList1){y[y %in% z]})()
  ### If none specified, no aggregation (only SLR interpolation)
  ### Otherwise, aggregation depends on length of agg levels
  if     ("none" %in% aggLevels) {aggLevels <- c()}
  else if("all"  %in% aggLevels) {aggLevels <- aggList0}
  doAgg <- (aggLevels |> length()) > 0
  # doAgg |> print(); aggLevels |> print(); aggList1 |> print()
  ### Add to list
  if(outputList) {argsList[["aggLevels" ]] <- aggLevels}
  rm(aggList0, aggList1)

  ###### ** Elasticity ######
  ### Message user about elasticity
  has_elasticity <- elasticity |> is.numeric()
  elasticity     <- has_elasticity |> ifelse(elasticity, elasticity0)
  if(!has_elasticity){
    paste0("\t", "Incorrect value type provided for argument 'elasticity'...") |> message()
    paste0("\t\t", "Using default elasticity values.") |> message()
  } ### End if
  rm(has_elasticity, elasticity0)


  ###### ** Sectors List ######
  ### Sector names & labels
  sectorIds    <- co_sectors[["sector_id"   ]]
  sectorLbls   <- co_sectors[["sector_label"]]
  ### Initialize sector list if the sectors list is null
  if(sectorList |> is.null()){
    sectorList    <- sectorIds
  } else{
    ### Get lower case versions
    sectors0   <- sectorIds  |> tolower()
    sectors1   <- sectorLbls |> tolower()
    sectors2   <- sectorList |> tolower()
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
    if(na_sectors |> length()){
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
  ### Update in list
  if(outputList) {argsList[["sectorList"]] <- sectorLbls}
  ### Number of sectors
  num_sectors  <- sectorList |> length()

  ### Filter to sectors
  co_slrScalars  <- co_slrScalars  |> filter(sector %in% sectorIds)
  df_sectorsInfo <- df_sectorsInfo |> filter(sector %in% sectorIds)

  ###### ** By State ######
  byState    <- TRUE
  popCol0    <- "state_pop"
  stateCols0 <- c("state", "postal")

  ###### ** Inputs ######
  ###### ** Load Inputs ######
  ### Create logicals and initialize inputs list
  list_inputs    <- co_inputScenarioInfo[["inputName"]]
  num_inputNames <- co_inputScenarioInfo |> nrow()
  "Checking input values..." |> message()
  ### Iterate over the input list
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
    rm(inputInfo_i, input_i, msgName_i, min_i, max_i, region_i, valueCol_i) |> try(silent=T)
    rm(colNames_i, numCols_i, has_i, df_input_i, has_update_i) |> try(silent=T)
  } ### End iterate over inputs
  rm(list_inputs, num_inputNames)

  ###### ** Temperature Scenario ######
  ### User inputs: temperatures have already been converted to CONUS temperatures. Filter to desired range.
  ### Name the reference year temperature
  ### Add the point where impacts are zero
  refYear_temp <- (co_modelTypes |> filter(modelUnitType=="temperature"))$modelRefYear |> unique()
  # co_modelTypes |> names() |> print()

  ### If no user input (default): Use temperature scenario for one region
  if(has_tempUpdate){
    "\t" |> message("Creating temperature scenario from user inputs...")
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
  } else{
    ### Load default temperature scenario
    "\t" |> message("No temperature scenario provided...using default temperature scenario...")
    # co_defaultTemps |> glimpse()
    tempInput <- co_defaultTemps |> rename_at(c("temp_C_conus"), ~"temp_C")
    tempInput <- tempInput       |> select(c("year", "temp_C"))
    temp_df   <- co_defaultTemps |> as_tibble()
  } ### End else(has_tempUpdate)
  ### Filter to appropriate years
  temp_df <- temp_df |> filter(year >= refYear_temp) |> filter(year <= maxYear)
  ### Add to list
  if(outputList){
    statusList[["inputsList"]][["tempInput"]] <- has_tempUpdate |> ifelse("Custom", "Default")
    # argsList  [["inputsList"]][["tempInput"]] <- inputsList[["tempInput"]]
    argsList  [["inputsList"]][["tempInput"]] <- tempInput
  } ### End if(outputList)
  rm(tempInput, co_defaultTemps, has_tempUpdate)
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
    "\t" |> message("Creating SLR scenario from user inputs...")
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
  } else{
    ### If there is no SLR scenario, calculate from temperatures
    ### First convert temperatures to global temperatures
    ### Then convert global temps to SLR
    "\t" |> message("Creating SLR scenario from temperature scenario...")
    slrInput <- temps2slr(temps = temp_df$temp_C_global, years = temp_df$year)
    slr_df   <- slrInput
  } ### End else(has_slrUpdate)
  ### Filter to appropriate years
  slr_df  <- slr_df |> filter(year >= refYear_slr) |> filter(year <= maxYear)
  ### Add to list
  if(outputList){
    statusList[["inputsList"]][["slrInput"]] <- has_slrUpdate |> ifelse("Custom", "Default")
    # argsList  [["inputsList"]][["slrInput"]] <- inputsList[["slrInput"]]
    argsList  [["inputsList"]][["slrInput"]] <- slrInput
  } ### End if(outputList)
  rm(slrInput, has_slrUpdate)

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
  if(outputList){
    returnList[["scenarios"]][["temp"]] <- temp_df
    returnList[["scenarios"]][["slr" ]] <- slr_df
  } ### End if(outputList)
  ### Remove intermediate values
  rm(co_modelType0, temp_df, slr_df)

  ###### ** Socioeconomic Scenario ######
  ### Update the socioeconomic scenario with any GDP or Population inputs and message the user
  ### Reformat GDP inputs if provided, or use defaults
  gdpCols0 <- c("year", "gdp_usd")
  popCols0 <- c("year", "region") |> c(stateCols0, popCol0)
  if(has_gdpUpdate){
    "\t" |> message("Creating GDP scenario from user inputs...")
    gdpInput <- gdpInput |> filter(!(gdp_usd |> is.na()) & !(year |> is.na()))
    gdpInput <- gdpInput |> filter(gdp_usd >= 0)
    gdp_df   <- gdpInput |> interpolate_annual(years=list_years, column="gdp_usd", rule = 2) |> select(-c("region"))
  } else{
    "\t" |> message("No GDP scenario provided...Using default GDP scenario...")
    gdpInput <- gdp_default |> select(all_of(gdpCols0))
    gdp_df   <- gdpInput
  } ### End else(has_gdpUpdate)
  ### Add to list
  if(outputList){
    statusList[["inputsList"]][["gdpInput"]] <- has_gdpUpdate |> ifelse("Custom", "Default")
    # argsList  [["inputsList"]][["gdpInput"]] <- inputsList[["gdpInput"]]
    argsList  [["inputsList"]][["gdpInput"]] <- gdpInput
  } ### End if(outputList)
  rm(gdpInput, gdp_default, has_gdpUpdate)

  ### Population inputs
  if(has_popUpdate){
    "\t" |> message("Creating population scenario from user inputs...")
    ### Standardize region and then interpolate
    popInput     <- popInput |> filter(!(year |> is.na()))
    popInput     <- popInput |> (function(y, col0=popCol0){y[!(y[[popCol0]] |> is.na()),]})()
    popInput     <- popInput |> (function(y, col0=popCol0){y[!(y[[popCol0]] <= 0),]})()
    pop_df       <- popInput |> mutate(region = gsub(" ", ".", region))
    pop_df       <- pop_df   |> interpolate_annual(years=list_years, column=popCol0, rule=2, byState=byState) |> ungroup()
    # pop_df |> glimpse()
    ### Calculate national population
    national_pop <- pop_df |> group_by_at(.vars=c("year")) |> summarize_at(.vars=c(popCol0), sum, na.rm=T) |> ungroup()
    national_pop <- national_pop |> rename_at(vars(popCol0), ~"national_pop")
    # national_pop |> glimpse()
  } else{
    "\t" |> message("Creating population scenario from defaults...")
    ### Select columns and filter
    popInput     <- pop_default |> select(all_of(popCols0))
    pop_df       <- popInput
    national_pop <- national_pop_default |> select("year", "national_pop")
  } ### End else(has_popUpdate)
  ### Add to list
  if(outputList){
    statusList[["inputsList"]][["popInput"]] <- has_popUpdate |> ifelse("Custom", "Default")
    argsList  [["inputsList"]][["popInput"]] <- popInput
    # argsList  [["inputsList"]][["popInput"]] <- inputsList[["popInput"]]
  } ### End if(outputList)
  rm(popInput, pop_default, national_pop_default, has_popUpdate)

  ### Filter to correct years
  gdp_df       <- gdp_df       |> filter(year >= minYear & year <= maxYear)
  pop_df       <- pop_df       |> filter(year >= minYear & year <= maxYear)
  national_pop <- national_pop |> filter(year >= minYear & year <= maxYear)

  ### National scenario
  # gdp_df |> glimpse(); national_pop |> glimpse();
  national_scenario <- gdp_df  |> left_join(national_pop, by=c("year"))
  national_scenario <- national_scenario |> mutate(gdp_percap = gdp_usd / national_pop)
  ### Update inputs in outputs list
  if(outputList){
    returnList[["scenarios"]][["gdp"]] <- gdp_df
    returnList[["scenarios"]][["pop"]] <- pop_df
  } ### End if(outputList)
  # gdp_df |> nrow() |> print(); national_pop |> nrow() |> print(); national_scenario |> nrow() |> print()
  rm(gdp_df, national_pop)

  ### Updated scenario
  join0             <- "year"
  arrange0          <- "region" |> c(stateCols0) |> c(join0)
  updatedScenario   <- national_scenario |> left_join(pop_df, by=join0)
  updatedScenario   <- updatedScenario   |> arrange_at(c(arrange0))
  rm(join0, arrange0)

  ###### Update Scalars ######
  message("Updating scalars...")

  ### Filter main scalars to correct years and filter out regional population
  df_mainScalars <- df_mainScalars |> filter(year >= minYear) |> filter(year <= maxYear)
  df_mainScalars <- df_mainScalars |> update_popScalars(updatedScenario)
  # df_mainScalars |> glimpse()

  ### Message the user
  message("Calculating impacts...")

  ###### Initialize Results ######
  ### Initialized results: Join sector info and default scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars
  # df_sectorsInfo |> glimpse(); #df_mainScalars |> glimpse(); df_mainScalars1 |> glimpse()
  df_mainScalars <- df_mainScalars |> filter(year >= minYear & year <= maxYear)
  df_results0    <- updatedScenario |> initialize_resultsDf(
    df_info    = df_sectorsInfo,
    df_scalars = df_mainScalars,
    elasticity = elasticity
  )
  # df_mainScalars |> glimpse();
  ### Filter to years
  df_results0    <- df_results0 |> filter(year >= minYear & year <= maxYear)

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
  ### Join with initial results
  join0          <- c("modelType")
  df_results0    <- df_results0 |> left_join(co_models0, by=c(join0))
  # df_results0 |> glimpse()
  rm(join0, co_models0)

  ### Create scenario ID and separate by model type
  include0        <- c("region") |> c(stateCols0) |> c("model_label")
  df_results0     <- df_results0 |> get_scenario_id(include = include0)

  ###### Scaled Impacts  ######
  # ### Initialize and empty data frame df_scenarioResults
  # if(msgUser) message(list_messages[["scaledImpacts"]]$try)
  # if(msgUser) message("Calculating scaled impacts...")
  df_scenarioResults  <- tibble()
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
  # if(msgUser) message("\t", list_messages[["scaledImpacts"]]$success)
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
  rm(var_lvls, var_lbls)
  # (df_results |> filter(driverUnit=="cm"))$year |> range() |> print()

  ### Impact types
  imp_lvls   <- co_impactTypes[["sector_impactType"]]
  imp_lbls   <- co_impactTypes[["impactType_label"]]
  df_results <- df_results |> mutate(sect_imp   = sector_id |> paste0("_", impactType))
  df_results <- df_results |> mutate(impactType = sect_imp  |> factor(imp_lvls, imp_lbls))
  df_results <- df_results |> select(-c("sect_imp", "sector_id"))
  rm(imp_lvls, imp_lbls)
  # (df_results |> filter(driverUnit=="cm"))$year |> range() |> print()

  ###### Columns ######
  ### Grouping columns
  groupCols0  <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0)
  groupCols0  <- groupCols0 |> c("model_type", "model")
  # groupCols0  <- groupCols0 |> c("modelUnitType")
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
  if(doAgg){
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
  } ### End if(doAgg)
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

  ### Message
  message("\n", "Finished", ".")

  ### Return
  return(returnObj)

} ### End function








