###### Documentation ######
#' Project annual average impacts from methane, NOx, and ozone.
#'
#'
#'
#' @description
#' This function allows users to estimate impacts from changes to concentrations of ozone, methane, and/or NOx in the atmosphere.
#'
#'
#'
#' @param inputsList=list(gdp=NULL,pop=NULL,ch4=NULL,nox=NULL,o3=NULL) A list with named elements (`gdp`, `pop`, `ch4`, `nox`, and/or `o3`), each containing data frames of custom scenarios for gross domestic product (GDP), state-level population, temperature, and/or global mean sea level rise (GMSL) trajectories, respectively, over a continuous period. Temperature and sea level rise inputs should start in 2000 or earlier. Values for population and GDP scenarios can start in 2010 or earlier. Values for each scenario type must be within reasonable ranges. For more information, see [FrEDI::import_inputs()].
#'
#' @param elasticity=1 A numeric value indicating an elasticity to use for adjusting VSL (defaults to `elasticity = 1`)
#'
#' @param maxYear=2100 A numeric value indicating the maximum year for the analysis. The range for `maxYear` is `[2011, 2300]`. Defaults to `maxYear = 2100`.
#'
#' @param thru2300=FALSE A ` TRUE/FALSE` shortcut that overrides the `maxYear` argument to run the model to 2300. Defaults to `thru2300 = FALSE`.
#'
#' @param outputList=FALSE A ` TRUE/FALSE` value indicating whether to output results as a data frame object (`outputList = FALSE`, default) or to return a list of objects (`outputList = TRUE`) that includes information about model provenance (including input arguments and input scenarios) along with the data frame of results.
#'
#' @param allCols=FALSE A `TRUE/FALSE` value indicating whether to include intermediate column values in results (e.g., physical and economic multipliers). Used in testing. Defaults to `allCols = FALSE`).
#'
#'
#'
#' @details This function allows users to estimate impacts from changes to concentrations of ozone, methane, and/or NOx in the atmosphere.
#'
#' Users can specify an optional list of custom scenarios with `inputsList` (for more information on the format of inputs, see [FrEDI::import_inputs()]). The function [FrEDI::import_inputs()] can be used to importing custom scenarios from CSV files. [FrEDI::import_inputs()] returns a list with named elements `gdp`, `pop`, `o3`, `ch4`, and/or `nox`, with each respectively containing a data frame with a custom scenario for GDP, state-level population, ozone concentration, methane concentration, and/or NOx concentration. If a user imports scenarios using [FrEDI::import_inputs()], they can pass the outputs of [FrEDI::import_inputs()] directly to the `run_fred_methane()` argument `inputsList`. Note that the documentation for [FrEDI::import_inputs()] can also provide additional guidance and specification on the formats for each scenario type.
#'
#' Otherwise, `run_fred_methane()` looks for a list object passed to the argument `inputsList`. Within that list, `run_fred_methane()` looks for named list elements -- `gdp`, `pop`, `o3`, `ch4`, and/or `nox` -- with each respectively containing a data frame with a custom scenario for GDP, state-level population, ozone concentration, methane concentration, and/or NOx concentration. If `inputsList = NULL` or `inputsList = list()` (default), `run_fred_methane()` uses default trajectories for GDP, population, ozone concentration, methane concentration, and/or NOx concentration. `run_fred_methane()` will default back to the default scenarios for any list elements that empty or `NULL` (in other words, running `run_fredi(inputsList = list())` returns the same outputs as running `run_fred_methane()`). See [FrEDI::gdpDefault], [FrEDI::popDefault] for more information about the default GDP and population scenarios. Default scenarios for ozone, methane, and NOx are described in more detail below.
#'
#' * __GDP Inputs.__ The input scenario for gross domestic product (GDP) requires national GDP values in 2015$. GDP values must be greater than or equal to zero.
#'    * `gdp` requires a data frame object with two columns with names `"year"`, and `"gdp_usd"` containing the year and the national GDP, respectively. GDP values must be greater than or equal to zero.
#'    * GDP inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`). Note that the minimum year for the GDP scenario is different from that for [FrEDI::run_fredi()], because `run_fredi_methane()` is only available starting in 2020.
#'    * If the user does not specify an input scenario for GDP (i.e., `inputsList = list(gdp = NULL)`, `run_fred_methane()` uses a default GDP scenario.
#' * __Population Inputs.__ The input population scenario requires state-level population values. Population values must be greater than or equal to zero.
#'    * `pop` requires a data frame object with five columns with names `"region"`, `"state"`, `"postal"`, `"year"`, and `"pop"` containing the region name (one of `"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"` for CONUS states, or `"Alaska"` and `"Hawaii"` for Alaska and Hawaii, respectively), the state name, the two-character postal code abbreviation for the state, the year, and the state population, respectively.
#'    * Population inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`). Note that the minimum year for the GDP scenario is different from that for [FrEDI::run_fredi()], because `run_fredi_methane()` is only available starting in 2020.
#'    * If the user does not specify an input scenario for population (i.e., `inputsList = list(pop = NULL)`, `run_fred_methane()` uses a default population scenario.
#' * __Ozone Inputs.__ The input ozone scenario requires changes in annual state-level ozone concentrations, by GCM model, in parts per trillion by volume (pptv) relative to a 2020 baseline. In other words, the input ozone scenario requires ozone concentrations specific to the state, GCM model, and year of the analysis.
#'    * `o3` requires a data frame object with five columns with names `"region"`, `"state"`, `"postal"`, `"model"`, `"year"`, and `"O3_pptv"`  containing the region name (`"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"` for CONUS states, or `"Alaska"` and `"Hawaii"` for Alaska and Hawaii, respectively), the state name, the two-character postal code abbreviation for the state, the GCM model name (`"CanESM2"`, `"GFDL-CM3"`, `"GISS-E2-R"`, `"HadGEM2-ES"`, and/or `"MIROC5"`), the year, and the change in ozone concentration (in pptv) relative to a 2020 baseline.
#'    * Ozone inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If inputs are specified for ozone _and_ methane or Nox (i.e., `!is.null(inputsList$o3) & (!is.null(inputsList$ch4) | !is.null(inputsList$nox))`), `run_fredi_methane()` will use the ozone scenario in preference of the methane and NOx scenario.
#' * __Methane Inputs.__ The input methane scenario requires changes in annual methane concentrations, at the national level, in parts per billion by volume (ppbv) relative to a 2020 baseline.
#'    * `ch4` requires a data frame object with two columns with names `"year"` and `"CH4_ppbv"`  containing the year and the change in methane concentration (in ppbv) relative to a 2020 baseline.
#'    * Methane inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * `run_fredi_methane()` will override a user-supplied methane scenario with a user-supplied ozone scenario; in other words, `run_fredi_methane()` will use the ozone scenario in preference of the methane and NOx scenario.
#' * __NOx Inputs.__ The input NOx scenario requires changes in annual NOx concentrations, at the national level, in Megatons (MT) relative to a 2020 baseline.
#'    * `nox` requires a data frame object with two columns with names `"year"` and `"NOx_Mt"`  containing the year and the change in NOx concentration (in Mt) relative to a 2020 baseline.
#'    * NOx inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * `run_fredi_methane()` will override a user-supplied methane scenario with a user-supplied ozone scenario; in other words, `run_fredi_methane()` will use the ozone scenario in preference of the methane and NOx scenario.
#'
#' If inputs are specified for ozone _and_ methane or Nox (i.e., `!is.null(inputsList$o3) & (!is.null(inputsList$ch4) | !is.null(inputsList$nox))`), `run_fredi_methane()` will use the ozone scenario in preference of the methane and NOx scenario. If no ozone, methane, or NOx scenario are provided (i.e., `inputsList$o3`, `inputsList$ch4`, and `inputsList$nox` are all `NULL`), `run_fredi_methane()` will use the default ozone scenario to calculate impacts. However, if a user provides an input scenario for methane or NOx (i.e., either `inputsList$ch4` or `inputsList$nox` are not `NULL`) but no ozone scenario is provided (i.e., `inputsList$o3` is `NULL`), then `run_fredi_methane()` will use the methane and NOx scenarios (if either of those inputs is missing, `run_fredi_methane()` will use the corresponding default scenario).
#'
#' To calculate the change in ozone concentrations when using methane and NOx scenarios, `run_fredi_methane()` follows the approach described in EPA/IEc (XXXX):
#'
#' 1. First, `run_fredi_methane` calculates values for the change in ozone concentration (in pptv) by multiplying values for a given change in methane concentrations (in ppbv) by a state- and model-specific ozone response matrix (with values in units of concentrations of ozone in pptv relative to concentrations of methane in ppbv).
#' 2. Second, `run_fredi_methane` calculates values for NOx factor (`NOxFactor`) from the NOx concentrations in Mt (`NOX_Mt`), using the equation `NOxFactor = (log(NOX_Mt) \* k1 + k0) \* 1e3/556` (where `k0` and `k1` are coefficients with values of `-1.12` and `-0.49`, respectively).
#' 3. Third, `run_fredi_methane` calculates a NOx ratio (`NOxRatio = NOxFactor / NOxFactor0`) by dividing the NOx factor values (`NOxFactor`) from Step 2 by a reference NOx factor (`NOxFactor0=-4.088991`), where the value for `NOxFactor0` was calculated for a reference NOx concentration (`NOX_Mt0=10.528`) using the equation from Step 2.
#' 4. Fourth, `run_fredi_methane` adjusts the values for change in ozone concentration from Step 1 by the NOx ratio from Step 3.
#'
#' `run_fredi_methane` uses the following default scenarios:
#'
#' * __Methane__. The methane default scenario, `ch4Default`, uses a constant value of `CH4_ppbv=100` for change in methane concentration (in ppbv) for the years 2020 through 2100.
#' * __NOx__. The NOx default scenario, `noxDefault`, uses a constant value of `NOx_Mt=10.528` for change in NOx concentration (in Mt) for the years 2020 through 2100.
#' * __Ozone__. The ozone default scenario, `oeDefault`, uses state- and model-specific constant values for change in ozone concentration (`O3_pptv` in pptv) for the years 2020 through 2100, as calculated from the default methane and NOx scenarios using the approach described above.
#'
#' `run_fredi_methane()` linearly interpolates missing annual values for all input scenarios using non-missing values (each scenario requires at least two non-missing values as detailed above for each scenario type). After interpolation of the input scenarios, `run_fredi_methane()` subsets the input scenarios to values within the analysis period (years above 2020 and ending in the year specified by `maxYear`).
#'
#' By default, `run_fredi_methane()` calculates impacts starting in the year 2020 and ending in 2100. Specify an alternative end year for the analysis using the `maxYear` argument. `maxYear` has a default value of `2100` and minimum and maximum values of `2020` and `2300`, respectively. Alternatively, users can set argument `thru2300 = TRUE` to override the `maxYear` argument and set `maxYear = 2300`. Note that the default scenarios included within [FrEDI] stop in the year 2100; users must provide custom input scenarios out to the desired end year **and** specify a `maxYear >= 2100` (and `maxYear <= 2300`) in order to return non-missing values for years after 2100.
#'
#' `run_fredi_methane()` calculates national population from state population values and then calculates GDP per capita from values for GDP and national population. Values for state population, national population, national GDP (in 2015$), and national per capita GDP (in 2015$/capita) are provided in the results data frame in columns `"pop"`, `"national_pop"`, `"gdp_usd"`, and `"gdp_percap"`, respectively. `run_fredi_methane()` converts the physical impacts (excess deaths) to an economic impact using a Value of Statistical Life (VSL) approach. VSL values are adjusted over time by scaling GDP per capita (relative to CONUS population) relative to a reference GDP per capita. For more information, refer to EPA (2021).
#'
#' The process used by the methane module to calculate physical impacts (excess respiratory deaths) from ozone is as follows:
#'
#' 1. `run_fredi_methane()` estimates a time-dependent national respiratory mortality rate (in deaths per capita) from national population values relative to a reference population.
#' 2. State-level respiratory mortality (deaths) is then calculated by the national respiratory mortality rate by state population.
#' 3. `run_fredi_methane()` then calculates a state-level respiratory mortality ratio by dividing the state-level respiratory mortality by a reference respiratory mortality.
#' 4. `run_fredi_methane()` also calculates a state- and model-specific ozone ratio by dividing the change in ozone concentration values in pptv by reference values.
#' 5. To calculate the number of excess respiratory mortality due to ozone, `run_fredi_methane()` multiplies the state- and model-specific baseline values for excess respiratory mortality by the state-level respiratory mortality ratio and the state- and model-specific ozone ratio. These
#'
#' To calculate the economic impacts of excess respiratory deaths from ozone, `run_fredi_methane()` multiplies the physical impacts by VSL adjusted for GDP and population, as described above.
#'
#'
#'
#'
#' If `outputList = FALSE` (default), `run_fredi_methane()` returns a data frame of annual physical and economic impacts over the analysis period, for each region, state, and model. If `outputList = TRUE`, in addition to the data frame of impacts, `run_fred_methane()` returns a list object containing information about values for function arguments and scenarios for GDP, population, and ozone or methane and NOx.
#'
#'
#'
#'
#' @return
#' If `outputList=FALSE`, the output of `run_fredi_methane()` is a data frame object (described above) containing annual physical and economic impacts over the analysis period, for each region, state, and model.
#'
#' If `outputList=TRUE`, `run_fredi_methane()` returns a list object containing the following:
#'
#' * __`statusList`__. A list with values for the arguments passed to `run_fredi_methane()` (including defaults if unspecified).
#' * __`argsList`__. A list with elements named after `run_fredi_methane()` arguments, containing the values of the arguments passed to `run_fred_methane()` (or default values if unspecified).
#' * __`scenarios`__. A list with named elements `gdp` and `pop` and `o3` or `ch4` and `nox` -- each containing the scenarios for GDP, population, and ozone or methane and NOx as used by the model in calculating impacts.
#' * __`results`__. Containing a data frame of annual physical and economic impacts (i.e., the same data frame returned if `outputList = FALSE`).
#'
#'
#'
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#' EPA/IEc. XXXX. Methane paper.
#'
#'
#' @examples
#' ### Load FrEDI
#' require(FrEDI)
#'
#' ### Load population and GDP scenarios and glimpse data
#' data("popDefault"); popDefault |> glimpse()
#' data("gdpDefault"); gdpDefault |> glimpse()
#'
#'
#' ### Run FrEDI methane with O3 inputs
#' example1 <- run_fredi_methane(inputsList=list(gdp=gdpDefault, pop=popDefault, o3=o3Default))
#'
#' ### Run FrEDI methane with methane inputs
#' example1 <- run_fredi_methane(inputsList=list(gdp=gdpDefault, pop=popDefault, ch4=ch4Default))
#'
#' ### Run FrEDI methane with methane and NOx inputs
#' example1 <- run_fredi_methane(inputsList=list(gdp=gdpDefault, pop=popDefault, ch4=ch4Default, nox=noxDefault))
#'
#'
#' @export
#' @md
#'
#'
#'
#'

###### run_fredi_methane ######
### This function creates a data frame of sector impacts for default values or scenario inputs.
run_fredi_methane <- function(
    inputsList = list(ch4=NULL, nox=NULL, gdp=NULL, pop=NULL), ### List of inputs
    elasticity = 1,     ### Override value for elasticity for economic values
    maxYear    = 2100,  ### Maximum year for the analysis period
    thru2300   = FALSE, ### Whether to run FrEDI methane through 2300
    outputList = FALSE, ### Whether to return input arguments as well as results. [If TRUE], returns a list instead of a data frame
    allCols    = FALSE  ### Whether to include additional columns in output
){
  ###### Set up the environment ######
  ###### ** Messaging ######
  # ### Level of messaging (default is to message the user)
  # silent     = TRUE   ### Whether to message the user
  # msgUser   <- !silent


  ###### ** Load Data ######
  ### Assign data objects to objects in this namespace
  ### Assign FrEDI config
  fredi_config <- rDataList[["fredi_config"]]
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}

  ### Coefficients
  minYear0  <- listMethane[["package"]][["coefficients"]][["minYear0"]]
  maxYear0  <- listMethane[["package"]][["coefficients"]][["maxYear0"]]

  ### Model years and NPD (FrEDI past 2100)
  minYear   <- minYear0
  maxYear   <- thru2300 |> ifelse(npdYear0, maxYear)
  do_npd    <- maxYear > maxYear0



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
    argsList[["elasticity"]] <- elasticity
    argsList[["maxYear"   ]] <- maxYear
    argsList[["thru2300"  ]] <- thru2300
    argsList[["allCols"   ]] <- allCols
    argsList[["silent"    ]] <- silent
  } ### End if(outputList)



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
  popCol0        <- c("pop")
  stateCols0     <- c("state", "postal")


  # aggLevels  = c("national", "modelaverage"), ### Aggregation levels
  # ###### ** Agg Levels  ######
  # ### Types of summarization to do: default
  # ### Aggregation levels
  # aggList0   <- aggList0  |> tolower() |> get_matches(y=c("impactyear", "impacttype"), matches=F)
  # aggLevels  <- aggLevels |> tolower()
  # ### Update status list
  # statusList[["aggLevels" ]] <- ("all" %in% aggLevels | (aggLevels %in% aggList0) |> all()) |> get_returnListStatus()
  # aggList1   <- aggList0  |> c("all", "none")
  # aggLevels  <- aggLevels |> get_matches(y=aggList1)
  # ### If none specified, no aggregation (only SLR interpolation)
  # ### Otherwise, aggregation depends on length of agg levels
  # if     ("none" %in% aggLevels) {aggLevels <- c()}
  # else if("all"  %in% aggLevels) {aggLevels <- aggList0}
  # doAgg      <- (aggLevels |> length()) > 0
  # ### Add to list
  # if(outputList) {argsList[["aggLevels"]] <- aggLevels}
  # rm(aggList0, aggList1)



  ###### Input Scenarios ######
  ###### ** Input Info ######
  paste0("Checking scenarios...") |> message()
  ### Add info to data
  co_inputInfo <- "co_inputInfo" |> get_frediDataObj(listSub="package", listName="listMethane")
  # co_inputInfo <- co_inputInfo |> filter(!inputName %in% "o3")
  co_inputInfo <- co_inputInfo |> mutate(ref_year = 2020)
  co_inputInfo <- co_inputInfo |> mutate(min_year = 2020)
  co_inputInfo <- co_inputInfo |> mutate(max_year = maxYear)

  ### Initialize subset
  df_inputInfo <- co_inputInfo

  ### Input info
  inNames0     <- co_inputInfo |> pull(inputName)
  # inNames0 |> print()


  ###### ** Input Columns ######
  ### Get list with expected name of columns used for unique ids
  ### Get list with expected name of column containing values
  valCols0     <- co_inputInfo |> pull(valueCol) |> as.list() |> set_names(inNames0)
  idCols0      <- inNames0     |> map(get_import_inputs_idCols) |> set_names(inNames0)
  # idCols0      <- list(valCols0=valCols0, df0=inputDefs[inNames0]) |> pmap(function(valCols0, df0){
  #   df0 |> names() |> get_matches(y=valCols0, matches=F)
  # }) |> set_names(inNames0)



  ###### ** Input Defaults ######
  inputDefs    <- inNames0 |> map(function(name0){
    ### Get defaults
    defName0 <- name0    |> paste0("_default")
    df0      <- defName0 |> get_frediDataObj(listSub="scenarioData", listName="listMethane")
    ### Format defaults
    do_o3_0  <- "o3"  %in% name0
    if(do_o3_0 ) {
      df0 <- df0 |> select(c(idCols0[["o3"]], valCols0[["o3"]]))
    } ### End if(do_o3_0 )
    ### Return
    return(df0)
  }) |> set_names(inNames0)
  # inputDefs$o3 |> glimpse()



  ###### ** Valid Inputs & Input Info ######
  ### Figure out which inputs are not null, and filter to that list
  ### inputsList Names
  inNames      <- inputsList |> names()
  # inNames |> print(); inputsList |> map(glimpse)
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


  ### Need scenario for CH4 & NOX or O3:
  ### If has O3, use O3. Otherwise, use CH4
  has_o3     <- inputsList[["o3" ]] |> nrow() |> length()
  has_ch4    <- inputsList[["ch4"]] |> nrow() |> length()
  if(has_o3) {
    drop0      <- c("ch4", "nox")
    inputsList <- inputsList |> (function(list0, y=drop0){list0[!((list0 |> names() %in% y))]})()
    inputDefs  <- inputDefs  |> (function(list0, y=drop0){list0[!((list0 |> names() %in% y))]})()
    inNames0   <- inNames0   |> get_matches(y=drop0, matches=F)
    inNames    <- inNames    |> get_matches(y=drop0, matches=F)
    rm(drop0)
  } else if(has_ch4) {
    drop0      <- c("o3")
    inputsList <- inputsList |> (function(list0, y=drop0){list0[!((list0 |> names() %in% y))]})()
    inputDefs  <- inputDefs  |> (function(list0, y=drop0){list0[!((list0 |> names() %in% y))]})()
    inNames0   <- inNames0   |> get_matches(y=drop0, matches=F)
    inNames    <- inNames    |> get_matches(y=drop0, matches=F)
    rm(drop0)
  } ### End if(has_o3)
  # inNames |> print()

  ###### ** Check Inputs ######
  ### Filter to valid inputs & get info
  ### Reorganize inputs list
  # inputDefs |> names() |> print()
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
      module    = "methane" |> rep(inNames |> length())
    ) |>
      pmap(check_input_data) |>
      set_names(inNames)
    rm(minYrs0, maxYrs0)

    ### Check again for inputs
    ### Filter to values that are not NULL
    inWhich      <- inNames    |> map(function(name0, list0=inputsList){!(list0[[name0]] |> is.null())}) |> unlist() |> which()
    inputsList   <- inputsList[inWhich]
    inNames      <- inputsList |> names()
    rm(inWhich)
  } ### if(hasAnyInputs)
  # inNames |> print()

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


  ### Update values
  # inNames |> print()
  hasInputs    <- inNames |> length()
  # return(inputsList)

  ### Iterate over list and format values
  if(hasInputs) {
    ### Update idCols for population, o3 if present in outputs
    doPop0       <- "pop" %in% inNames
    doO3_0       <- "o3"  %in% inNames
    if(doPop0) idCols0[["pop"]] <- c("region", "state", "postal") |> c(idCols0[["pop"]]) |> unique()
    if(doO3_0) idCols0[["o3" ]] <- c("region", "state", "postal", "model") |> c(idCols0[["o3" ]]) |> unique()
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
  # return(inputsList)

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




  ###### Physical Driver Scenario  ######
  ### Need scenario for CH4 & NOX or O3
  has_o3     <- inputsList[["o3" ]] |> nrow() |> length()
  has_ch4    <- inputsList[["ch4"]] |> nrow() |> length()
  has_driver <- has_o3 | has_ch4
  if(has_o3) {
    df_drivers <- inputsList[["o3"]]
    df_drivers <- df_drivers |> mutate(region = region |> str_replace_all("\\.|_|-| ", ""))
    df_drivers <- df_drivers |> mutate(model  = model  |> str_replace_all("\\.|_|-| ", ""))
  } else{
    join0      <- c("year")
    df_drivers <- inputsList[["ch4"]] |> left_join(inputsList[["nox"]], by=join0)
    rm(join0)
  } ### End if(has_o3)

  ### Get RR scalar and ozone response data
  df_drivers <- df_drivers |> format_methane_drivers()
  # df_drivers$model |> unique() |> print()
  # return(df_drivers)
  # df_drivers |> glimpse()

  ###### Socioeconomic Driver Scenario ######
  ### Update values
  gdp_df       <- inputsList[["gdp"]]
  pop_df       <- inputsList[["pop"]]
  pop_df       <- pop_df |> mutate(region = region |> str_replace_all("\\.|_|-| ", ""))
  # return(pop_df)
  # gdp_df |> group_by(year) |> summarize(n=n(), .groups="drop") |> filter(n>1) |> glimpse()
  # pop_df |> group_by(state, year) |> summarize(n=n(), .groups="drop") |> filter(n>1) |> glimpse()
  ### Calculate national population and update national scenario
  natScenario  <- gdp_df |> create_nationalScenario(pop0 = pop_df)
  rm(gdp_df, pop_df)
  # natScenario |> glimpse()
  # natScenario |> group_by(state, year) |> summarize(n=n(),.groups="drop") |> filter(n>1) |> glimpse()

  ### Calculate for CONUS values
  seScenario   <- natScenario |> calc_conus_scenario()
  rm(natScenario)
  # return(seScenario)
  # seScenario |> pull(region) |> unique() |> print()
  # seScenario |> glimpse()
  # seScenario |> group_by(state, year) |> summarize(n=n(),.groups="drop") |> filter(n>1) |> glimpse()

  ###### Calculate Impacts ######
  ###### ** Calculate Scalars ######
  ### Initialized results: Join sector info and default scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars
  paste0("Calculating impacts...") |> message()
  df_results   <- seScenario |> calc_methane_scalars(elasticity = elasticity)
  # return(df_results)
  # df_results |> select(c("year", "gdp_usd", "national_pop", "gdp_percap")) |> unique() |> nrow() |> print()
  # df_results |> glimpse()
  # df_results |> group_by(state, year) |> summarize(n=n(),.groups="drop") |> filter(n>1) |> glimpse()

  ###### ** Calculate Mortality Rate ######
  df_results <- df_results |> calc_methane_mortality()
  # df_results |> group_by(state, year) |> summarize(n=n(),.groups="drop") |> filter(n>1) |> glimpse()
  # df_results |> glimpse()

  ###### ** Calculate Excess Mortality ######
  # df_drivers |> glimpse()
  df_results <- df_results |> calc_methane_impacts(df1=df_drivers)
  # df_results |> group_by(state, model, year) |> summarize(n=n(),.groups="drop") |> filter(n>1) |> glimpse()
  # df_results$model |> unique() |> print()
  # df_results |> glimpse()



  ###### Format Results ######
  ### Add in model info
  paste0("Formatting results", "...") |> message()

  ### Add values
  move0      <- c("physicalmeasure")
  after0     <- c("econScalarName")
  df_results <- df_results |> mutate(module = "Methane")
  df_results <- df_results |> mutate(physicalmeasure = "Excess Mortality")
  df_results <- df_results |> relocate(all_of(move0), .after=all_of(after0))
  rm(move0, after0)

  ### Adjust region
  join0      <- c("region")
  renameAt0  <- c("region_label")
  renameTo0  <- c(join0)
  select0    <- c(join0) |> c(renameAt0)
  me_regions <- listMethane$package$co_regions |> select(all_of(select0))
  # me_models |> glimpse()
  df_results <- df_results |> left_join(me_regions, by=join0)
  df_results <- df_results |> select(-any_of(join0))
  df_results <- df_results |> rename_at(c(renameAt0), ~join0)
  rm(join0, select0, renameAt0)

  # ### Adjust model
  join0      <- c("model")
  renameAt0  <- join0 |> paste0("_label")
  select0    <- c(join0) |> c(renameAt0)
  me_models  <- listMethane$package$co_models |> select(all_of(select0))
  # me_models |> glimpse()
  df_results <- df_results |> left_join(me_models, by=join0)
  df_results <- df_results |> select(-any_of(join0))
  df_results <- df_results |> rename_at(c(renameAt0), ~join0)
  rm(join0, select0, renameAt0)

  ### Format driver values
  df_results <- df_results |> mutate(driverType  = "Ozone Concentration")
  df_results <- df_results |> mutate(driverUnit  = "pptv")
  df_results <- df_results |> mutate(driverValue = O3_pptv)

  ### Columns
  idCols0    <- c("module", "region", "state", "postal", "model") |> c("year")
  modCols0   <- c("driver") |> paste0(c("Type", "Unit", "Value"))
  natCols0   <- c("pop", "gdp_usd", "national_pop", "gdp_percap")
  valCols0   <- c("physicalmeasure")
  sumCols0   <- c("physical_impacts", "annual_impacts")
  select0    <- idCols0    |> c(modCols0) |> c(natCols0) |> c(valCols0) |> c(sumCols0) |> unique()
  arrange0   <- idCols0    |> unique()
  groupCols0 <- c(idCols0) |> c(valCols0) |> unique()

  ### Select columns
  if(!allCols) df_results <- df_results |> select(all_of(select0))

  ### Arrange data
  df_results <- df_results |> arrange_at(c(arrange0))



  # ###### ** Aggregation ######
  # if(doAgg) {
  #   # doAgg |> print()
  #   df_results <- df_results |> aggregate_impacts(
  #     aggLevels   = aggLevels,
  #     groupByCols = groupCols0,
  #     columns     = sumCols0
  #   ) ### End aggregate_impacts
  # } ### End if(doAgg)
  #
  # ###### ** Arrange Columns ######
  # ### Convert levels to character
  # ### Order the rows, then order the columns
  # arrange0   <- arrange0 |> get_matches(y = df_results |> names())
  # # arrange0 |> print()
  # ### Select columns
  # df_results <- df_results |> arrange_at(c(arrange0))
  # df_results <- df_results |> relocate(any_of(idCols0))
  # rm(arrange0)



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
  ### Message, clear unused memory, return
  paste0("\n", "Finished", ".") |> message()
  gc()
  return(returnObj)

} ### End function








