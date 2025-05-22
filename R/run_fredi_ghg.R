## Documentation ----------------
#' Project annual average impacts from methane, NOx, and ozone.
#'
#'
#'
#' @description
#' This function allows users to estimate impacts from changes in atmospheric concentrations of greenhouse gases (GHG) in the atmosphere.
#'
#'
#'
#' @param inputsList=list(gdp=NULL,pop=NULL,ch4=NULL,nox=NULL,o3=NULL) A list with named elements (`gdp`, `pop`, `ch4`, `nox`, and/or `o3`), each containing data frames of custom scenarios for gross domestic product (GDP), state-level population, ozone concentration, methane concentration, and NOx emissions, respectively, over a continuous period. Values should start in 2020 or earlier. Values for each scenario type must be within reasonable ranges. For more information, see [FrEDI::import_inputs()].
#'
#' @param elasticity=1 A numeric value indicating an elasticity to use for adjusting VSL (defaults to `elasticity = 1`).
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
#' @details This function allows users to estimate impacts from changes in atmospheric concentrations of greenhouse gases (GHG) in the atmosphere. This module (also referred to as the **concentration-driven module** or **FrEDI-GHG**. **FrEDI-GHG** is an optional supplement to the main FrEDI function [FrEDI::run_fredi()], and is driven by changes in GHG concentrations, rather than by changes in temperature or sea-level rise. The outputs of **FrEDI-GHG** complement the damages output from main FrEDI and allow users to explore how these additional GHG-related impacts will be distributed across CONUS regions.
#'
#' The **FrEDI-GHG** module currently assesses the health impacts associated with changes in exposure to the ozone produced from atmospheric methane. `run_fredi_ghg()` calculates these impacts by adapting the reduced form damage function developed by McDuffie et al., 2023. As described in McDuffie et al. 2023, increases in atmospheric methane concentrations lead to higher levels of global background ozone in the troposphere, and result in increased ozone attributable respiratory-related mortality. The efficiency of ozone production from methane (i.e., the ozone response) is sensitive to (or modified by) the presence of other ozone precursors, such as nitrogen oxides (NOx), CO, or volatile organic compounds (VOCs). GCM simulations conducted by the UNEP/CCAC Methane Assessment Report (which formed the basis of the McDuffie at al., 2023 results) previously showed that the ozone response efficiency was more sensitive to change in NOx emissions than to changes in VOCs and derived a NOx-specific modification factor. This relationship shows that as NOx emissions are reduced, ozone production will become more NOx limited (or VOC saturated) and methane will have a smaller impact on ozone production. Users have the option to supply annual NOx concentrations to modify the amount of methane-ozone production in each state by the change in NOx emissions relative to the reference values (the reference scenario assumes constant annual NOx emissions of 10.53Mt/year in the US). If the user does not provide a NOx emissions trajectory with a methane concentration trajectory, we assume NOx levels stay constant at the reference values, and with no adjustment to ozone response. Alternatively, users can supply a custom ozone concentration scenario. Users also have the option to supply a custom state-level population trajectory and/or a national Gross Domestic Product (GDP) scenario.
#'
#' Users can specify any optional custom scenarios to use with `run_fredi_ghg()` via the `inputsList` argument (for more information on the format of inputs, refer to the format of outputs described in [FrEDI::import_inputs()]). `run_fredi_ghg()` looks for a list object passed to the argument `inputsList`. Within that list, `run_fredi_ghg()` looks for named list elements -- `gdp`, `pop`, `o3`, `ch4`, and/or `nox` -- with each respectively containing a data frame with a custom scenario for GDP, state-level population, ozone concentration, methane concentration, and/or NOx concentration. If `inputsList = NULL` or `inputsList = list()` (default), `run_fredi_ghg()` uses default trajectories for GDP, population, ozone concentration, methane concentration, and/or NOx concentration. `run_fredi_ghg()` will default back to the default scenarios for any list elements that empty or `NULL` (in other words, running `run_fredi(inputsList = list())` returns the same outputs as running `run_fredi_ghg()`). See [FrEDI::gdpDefault], [FrEDI::popDefault] for more information about the default GDP and population scenarios. Default scenarios for ozone, methane, and NOx are described in more detail below.
#'
#' * __GDP Inputs.__ The input scenario for gross domestic product (GDP) requires national GDP values in 2015$. GDP values must be greater than or equal to zero.
#'    * `gdp` requires a data frame object with two columns with names `"year"`, and `"gdp_usd"` containing the year and the national GDP, respectively. GDP values must be greater than or equal to zero.
#'    * GDP inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`). Note that the minimum year for the GDP scenario is different from that for [FrEDI::run_fredi()], because `run_fredi_ghg()` is only available starting in 2020.
#'    * If the user does not specify an input scenario for GDP (i.e., `inputsList = list(gdp = NULL)`, `run_fredi_ghg()` uses a default GDP scenario.
#'
#' * __Population Inputs.__ The input population scenario requires state-level population values(national-, CONUS-, or region-level population values can be converted to state-level population by immporting inputs using the [FrEDI::import_inputs()] function). Population values must be greater than or equal to zero.
#'    * `pop` requires a data frame object with five columns with names `"region"`, `"state"`, `"postal"`, `"year"`, and `"pop"` containing the region name (one of `"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"` for CONUS states, or `"Alaska"` and `"Hawaii"` for Alaska and Hawaii, respectively), the state name, the two-character postal code abbreviation for the state, the year, and the state population, respectively.
#'    * Population inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`). Note that the minimum year for the GDP scenario is different from that for [FrEDI::run_fredi()], because `run_fredi_ghg()` is only available starting in 2020.
#'    * If the user does not specify an input scenario for population (i.e., `inputsList = list(pop = NULL)`, `run_fredi_ghg()` uses a default population scenario.
#'
#' * __Ozone Inputs.__ The input ozone scenario requires changes in annual state-level ozone concentrations, by GCM model, in parts per trillion by volume (pptv) relative to a 1986-2005 baseline era. In other words, the input ozone scenario requires ozone concentrations specific to the state, GCM model, and year of the analysis.
#'    * `o3` requires a data frame object with six columns with names `"region"`, `"state"`, `"postal"`, `"model"`, `"year"`, and `"O3_pptv"`  containing the region name (`"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"` for CONUS states, or `"Alaska"` and `"Hawaii"` for Alaska and Hawaii, respectively), the state name, the two-character postal code abbreviation for the state, the GCM model name (`"CanESM2"`, `"GFDL-CM3"`, `"GISS-E2-R"`, `"HadGEM2-ES"`, and/or `"MIROC5"`), the year, and the change in ozone concentration (in pptv) relative to a 1986-2005 baseline era.
#'    * Ozone inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If inputs are specified for ozone _and_ methane or NOx (i.e., `!is.null(inputsList$o3) & (!is.null(inputsList$ch4) | !is.null(inputsList$nox))`), `run_fredi_ghg()` will use the ozone scenario in preference of the methane and NOx scenario.
#'
#' * __Methane Inputs.__ The input methane scenario requires changes in annual methane concentrations, at the national level, in parts per billion by volume (ppbv) relative to a 1986-2005 baseline era.
#'    * `ch4` requires a data frame object with two columns with names `"year"` and `"CH4_ppbv"`  containing the year and the change in methane concentration (in ppbv) relative to a 1986-2005 baseline era.
#'    * Methane inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * `run_fredi_ghg()` will override a user-supplied methane scenario with a user-supplied ozone scenario; in other words, `run_fredi_ghg()` will use the ozone scenario in preference of the methane and NOx scenario.
#'
#' * __NOx Inputs.__ The input NOx scenario requires annual NOx emissions in the US, at the national level, in Megatons (MT) relative to a 1986-2005 baseline.
#'    * `nox` requires a data frame object with two columns with names `"year"` and `"NOx_Mt"`  containing the year and the change in NOx concentration (in Mt) relative to a 1986-2005 baseline era.
#'    * NOx inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * `run_fredi_ghg()` will override a user-supplied methane scenario with a user-supplied ozone scenario; in other words, `run_fredi_ghg()` will use the ozone scenario in preference of the methane and NOx scenario.
#'
#' The function [FrEDI::import_inputs()] can be used to importing custom scenarios from CSV files. [FrEDI::import_inputs()] returns a list with named elements `gdp`, `pop`, `o3`, `ch4`, and/or `nox`, with each respectively containing a data frame with a custom scenario for GDP, state-level population, change in ozone concentration, change in methane concentration, and NOx emissions. If a user imports scenarios using [FrEDI::import_inputs()], they can pass the outputs of [FrEDI::import_inputs()] directly to the `run_fredi_ghg()` argument `inputsList`. Note that the documentation for [FrEDI::import_inputs()] can also provide additional guidance and specification on the formats for each scenario type.
#'
#' If inputs are specified for ozone _and_ methane or NOx (i.e., `!is.null(inputsList$o3) & (!is.null(inputsList$ch4) | !is.null(inputsList$nox))`), `run_fredi_ghg()` will use the ozone scenario in preference of the methane and NOx scenario. If no ozone, methane, or NOx scenario are provided (i.e., `inputsList$o3`, `inputsList$ch4`, and `inputsList$nox` are all `NULL`), `run_fredi_ghg()` will use the default ozone scenario to calculate impacts. However, if a user provides an input scenario for methane or NOx (i.e., either `inputsList$ch4` or `inputsList$nox` are not `NULL`) but no ozone scenario is provided (i.e., `inputsList$o3` is `NULL`), then `run_fredi_ghg()` will use the methane and NOx scenarios (if either of those inputs is missing, `run_fredi_ghg()` will use the corresponding default scenario).
#'
#' To calculate the change in ozone concentrations when using methane and NOx scenarios, `run_fredi_ghg()` follows the approach described in EPA (Forthcoming):
#'
#' 1. First, `run_fredi_ghg` calculates values for the change in ozone concentration (in pptv) by multiplying values for a given change in methane concentrations (in ppbv) by a state- and model-specific ozone response matrix (with values in units of concentrations of ozone in pptv relative to concentrations of methane in ppbv).
#' 2. Second, `run_fredi_ghg` calculates values for NOx factor (`NOxFactor`) from the NOx concentrations in Mt (`NOX_Mt`), using the equation `NOxFactor = (log(NOX_Mt) \* k1 + k0) \* 1e3/556` (where `k0` and `k1` are coefficients with values of `-1.12` and `-0.49`, respectively). Note that methane module currently uses the GCM average values for the US, though GCM-specific values are available and could be added in future revisions.
#' 3. Third, `run_fredi_ghg` calculates a NOx ratio (`NOxRatio = NOxFactor / NOxFactor0`) by dividing the NOx factor values (`NOxFactor`) from Step 2 by a reference NOx factor (`NOxFactor0=-4.088991`), where the value for `NOxFactor0` was calculated for a reference NOx concentration (`NOX_Mt0=10.528`) using the equation from Step 2.
#' 4. Fourth, `run_fredi_ghg` adjusts the values for change in ozone concentration from Step 1 by the NOx ratio from Step 3.
#'
#' `run_fredi_ghg` uses the following default scenarios:
#'
#' * __Methane__. The methane default scenario, `ch4Default`, uses a constant value of `CH4_ppbv=100` for change in methane concentration (in ppbv) for the years 2020 through 2100. See [FrEDI::ch4Default] for more information on the default scenario. Note that the temperature scenario used to produce this default methane scenario differs from the default temperature scenario used in main `FrEDI` ([FrEDI::run_fredi()]) and the `FrEDI` SV module ([FrEDI::run_fredi_sv()]).
#' * __NOx__. The NOx default scenario, `noxDefault`, uses a constant value of `NOx_Mt=10.528` for change in NOx concentration (in Mt) for the years 2020 through 2100.
#' * __Ozone__. The ozone default scenario, `o3Default`, uses state- and GCM-specific constant values for change in ozone concentration (`O3_pptv` in pptv) for the years 2020 through 2100, as calculated from the default methane and NOx scenarios using the approach described above.
#'
#' `run_fredi_ghg()` linearly interpolates missing annual values for all input scenarios using non-missing values (each scenario requires at least two non-missing values as detailed above for each scenario type). After interpolation of the input scenarios, `run_fredi_ghg()` subsets the input scenarios to values within the analysis period (years above 2020 and ending in the year specified by `maxYear`).
#'
#' By default, `run_fredi_ghg()` calculates impacts starting in the year 2020 and ending in 2100. Specify an alternative end year for the analysis using the `maxYear` argument. `maxYear` has a default value of `2100` and minimum and maximum values of `2020` and `2300`, respectively. Alternatively, users can set argument `thru2300 = TRUE` to override the `maxYear` argument and set `maxYear = 2300`. Note that the default scenarios included within [FrEDI] stop in the year 2100; users must provide custom input scenarios out to the desired end year **and** specify a `maxYear >= 2100` (and `maxYear <= 2300`) in order to return non-missing values for years after 2100.
#'
#' `run_fredi_ghg()` calculates national population from state population values and then calculates GDP per capita from values for GDP and national population. Values for state population, national population, national GDP (in 2015$), and national per capita GDP (in 2015$/capita) are provided in the results data frame in columns `"pop"`, `"national_pop"`, `"gdp_usd"`, and `"gdp_percap"`, respectively. `run_fredi_ghg()` converts the physical impacts (excess deaths) to an economic impact using a Value of Statistical Life (VSL) approach. VSL values are adjusted over time by scaling GDP per capita (relative to CONUS population) relative to a reference GDP per capita. For more information, refer to EPA (2021).
#'
#' The process used by the methane module to calculate physical impacts (excess respiratory deaths) from ozone is as follows:
#'
#' 1. `run_fredi_ghg()` estimates a time-dependent national respiratory mortality rate (in deaths per capita) from national population values relative to a reference population.
#' 2. State-level respiratory mortality (deaths) is then calculated by the national respiratory mortality rate by state population.
#' 3. `run_fredi_ghg()` then calculates a state-level respiratory mortality ratio by dividing the state-level respiratory mortality by a reference respiratory mortality.
#' 4. `run_fredi_ghg()` also calculates a state- and model-specific ozone ratio by dividing the change in ozone concentration values in pptv by reference values.
#' 5. To calculate the number of excess respiratory mortality due to ozone, `run_fredi_ghg()` multiplies the state- and model-specific baseline values for excess respiratory mortality by the state-level respiratory mortality ratio and the state- and model-specific ozone ratio. These
#'
#' To calculate the economic impacts of excess respiratory deaths from ozone, `run_fredi_ghg()` multiplies the physical impacts by VSL adjusted for GDP and population, as described above.
#'
#'
#'
#'
#' If `outputList = FALSE` (default), `run_fredi_ghg()` returns a data frame of annual physical and economic impacts over the analysis period, for each region, state, and model. If `outputList = TRUE`, in addition to the data frame of impacts, `run_fredi_ghg()` returns a list object containing information about values for function arguments and scenarios for GDP, population, and ozone or methane and NOx.
#'
#'
#'
#'
#' @return
#' If `outputList=FALSE`, the output of `run_fredi_ghg()` is a data frame object (described above) containing annual physical and economic impacts over the analysis period, for each region, state, and model.
#'
#' If `outputList=TRUE`, `run_fredi_ghg()` returns a list object containing the following:
#'
#' * __`statusList`__. A list with values for the arguments passed to `run_fredi_ghg()` (including defaults if unspecified).
#' * __`argsList`__. A list with elements named after `run_fredi_ghg()` arguments, containing the values of the arguments passed to `run_fredi_ghg()` (or default values if unspecified).
#' * __`scenarios`__. A list with named elements `gdp` and `pop` and `o3` or `ch4` and `nox` -- each containing the scenarios for GDP, population, and ozone or methane and NOx as used by the model in calculating impacts.
#' * __`results`__. Containing a data frame of annual physical and economic impacts (i.e., the same data frame returned if `outputList = FALSE`).
#'
#'
#'
#'
#' @references Environmental Protection Agency (EPA). (Forthcoming). Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#' McDuffie, E. E., Sarofim, M. C., Raich, W., Jackson, M., Roman, H., Seltzer, K., Henderson, B. H., Shindell, D. T., Collins, M., Anderton, J., Barr, S., & Fann, N. (2023). The Social Cost of Ozone-Related Mortality Impacts From Methane Emissions. Earth’s Future, 11(9), e2023EF003853.
#'
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
#' example1 <- run_fredi_ghg(inputsList=list(gdp=gdpDefault, pop=popDefault, o3=o3Default))
#'
#' ### Run FrEDI methane with methane inputs
#' example1 <- run_fredi_ghg(inputsList=list(gdp=gdpDefault, pop=popDefault, ch4=ch4Default))
#'
#' ### Run FrEDI methane with methane and NOx inputs
#' example1 <- run_fredi_ghg(inputsList=list(gdp=gdpDefault, pop=popDefault, ch4=ch4Default, nox=noxDefault))
#'
#'
#' @export
#' @md
#'
#'
#'
#'

## run_fredi_ghg ----------------
### This function creates a data frame of sector impacts for default values or scenario inputs.
run_fredi_ghg <- function(
    inputsList = list(gdp=NULL, pop=NULL, ch4=NULL, nox=NULL, o3=NULL), ### List of inputs
    elasticity = 1,     ### Override value for elasticity for economic values
    maxYear    = 2100,  ### Maximum year for the analysis period
    # thru2300   = FALSE, ### Whether to run FrEDI methane through 2300
    outputList = FALSE, ### Whether to return input arguments as well as results. [If TRUE], returns a list instead of a data frame
    allCols    = FALSE  ### Whether to include additional columns in output
){
  ### Set up the environment ----------------
  #### Messaging ----------------
  # ### Level of messaging (default is to message the user)
  # silent     = TRUE   ### Whether to message the user
  # msgUser   <- !silent

  ### Load Database
  conn <-  load_frediDB()

  #### Load Data ----------------
  ### Assign data objects to objects in this namespace
  ### Assign FrEDI config
  #fredi_config <- rDataList[["fredi_config"]]

  fredi_config    <- DBI::dbReadTable(conn,"fredi_config")
  fredi_config    <- unserialize(fredi_config$value |> unlist())
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}

  ### Get GHG Data from database
  ghgData    <- DBI::dbReadTable(conn,"ghgData")
  ghgData    <- unserialize(ghgData$value |> unlist())

  ### Coefficients
  minYear0  <- ghgData[["ghgData"]][["coefficients"]][["minYear0"]]
  maxYear0  <- ghgData[["ghgData"]][["coefficients"]][["maxYear0"]]

  ### Model years and NPD (FrEDI past 2100)
  minYear   <- minYear0
  # maxYear   <- thru2300 |> ifelse(npdYear0, maxYear)
  # do_npd    <- maxYear > maxYear0



  #### Return List ----------------
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



  #### Elasticity ----------------
  ### Message user about elasticity
  has_elasticity <- elasticity     |> is.numeric()
  elasticity     <- has_elasticity |> ifelse(elasticity, elasticity0)
  if(!has_elasticity){
    paste0("\t", "Incorrect value type provided for argument 'elasticity'...") |> message()
    paste0("\t\t", "Using default elasticity values.") |> message()
  } ### End if
  rm(has_elasticity, elasticity0)



  ### Input Scenarios ----------------
  #### Input Info ----------------
  paste0("Checking scenarios...") |> message()
  ### Add info to data
  ghgData    <- DBI::dbReadTable(con,"ghgData")
  ghgData    <- unserialize(ghgData$value |> unlist())

  co_inputInfo <- ghgData$ghgData$co_inputInfo

  # co_inputInfo <- co_inputInfo |> filter(!inputName %in% "o3")
  co_inputInfo <- co_inputInfo |> mutate(ref_year = 2020)
  co_inputInfo <- co_inputInfo |> mutate(min_year = 2020)
  co_inputInfo <- co_inputInfo |> mutate(max_year = maxYear)

  ### Initialize subset
  df_inputInfo <- co_inputInfo

  ### Input info
  inNames0     <- co_inputInfo |> pull(inputName)
  # inNames0 |> print()


  #### Input Columns ----------------
  ### Get list with expected name of columns used for unique ids
  ### Get list with expected name of column containing values
  valCols0     <- co_inputInfo |> pull(valueCol) |> as.list() |> set_names(inNames0)
  idCols0      <- inNames0     |> map(get_import_inputs_idCols) |> set_names(inNames0)
  # idCols0      <- list(valCols0=valCols0, df0=inputDefs[inNames0]) |> pmap(function(valCols0, df0){
  #   df0 |> names() |> get_matches(y=valCols0, matches=F)
  # }) |> set_names(inNames0)



  #### Input Defaults ----------------
  inputDefs    <- inNames0 |> map(function(name0){
    ### Get defaults
    defName0 <- name0    |> paste0("_default")
    df0      <- defName0 |> get_frediDataObj(listSub="scenarioData", listName="ghgData")
    ### Format defaults
    do_o3_0  <- "o3"  %in% name0
    if(do_o3_0 ) {
      df0 <- df0 |> select(c(idCols0[["o3"]], valCols0[["o3"]]))
    } ### End if(do_o3_0 )
    ### Return
    return(df0)
  }) |> set_names(inNames0)
  # inputDefs$o3 |> glimpse()



  #### Valid Inputs & Input Info ----------------
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

  #### Check Inputs ----------------
  ### Filter to valid inputs & get info
  ### Reorganize inputs list
  # inputDefs |> names() |> print()
  df_inputInfo <- df_inputInfo |> filter(inputName %in% inNames)
  inNames      <- df_inputInfo |> pull(inputName)

  ### Create logicals and initialize inputs list
  if(hasAnyInputs) {
    ### Min and max years
    ### - Min years
    minYrs0    <- inNames |> map(function(name0, df0=df_inputInfo){
      df0 |> filter(inputName %in% name0) |> pull(min_year) |> unique()
    }) |> set_names(inNames)
    ### - Max years
    maxYrs0    <- inNames |> map(function(name0, df0=df_inputInfo){
      df0 |> filter(inputName %in% name0) |> pull(max_year) |> unique()
    }) |> set_names(inNames)

    ### Check inputs
    inputsList <- list(
      inputName = inNames,
      inputDf   = inputsList[inNames],
      idCol     = idCols0   [inNames],
      # valCol    = valCols0  [inNames]
      valCol    = valCols0  [inNames],
      yearMin   = minYrs0,
      yearMax   = maxYrs0
      # yearMax   = maxYrs0,
      # module    = "methane" |> rep(inNames |> length())
      # module    = "ghg" |> rep(inNames |> length())
    ) |>
      pmap(check_input_data, popArea="state", module="ghg") |>
      set_names(inNames)
    rm(minYrs0, maxYrs0)

    ### Check again for inputs
    ### Filter to values that are not NULL
    inWhich      <- inNames    |> map(function(name0, list0=inputsList){
      !(list0[[name0]] |> is.null())
    }) |> unlist() |> which()
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
  rm(valCols0)
  # return(inputsList)

  ## Add an object to track names of User Inputs
  inNames1 <- if(hasInputs){inputsList[!(inputsList |> map(is.null) |> unlist())] |> names()}

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



  ### Driver Scenarios  ----------------
  #### Physical Driver Scenario ----------------
  ### Need scenario for CH4 & NOX or O3
  #has_o3     <- inputsList[["o3" ]] |> nrow() |> length()
  #has_ch4    <- inputsList[["ch4"]] |> nrow() |> length()
  has_o3     <-  "o3"  %in% inNames1
  has_ch4    <-  "ch4" %in% inNames1
  has_driver <- has_o3 | has_ch4
  if(has_o3) {
    df_drivers <- inputsList[["o3"]]
    driveNames <- driveNames
    ### Check Region
    doReg0 <- "region" %in% driveNames
    if(doReg0) {
      df_drivers <- df_drivers |> mutate(region = region |> str_replace_all("\\.|_|-| ", ""))
    } ### End if(doReg0)
    ### Format model
    # df_drivers <- df_drivers |> mutate(state  = state |> str_replace_all("\\.|_|-| ", ""))
    df_drivers <- df_drivers |> mutate(model  = model |> str_replace_all("\\.|_|-| ", ""))
  } else{
    join0      <- c("year")
    df_drivers <- inputsList[["ch4"]] |> left_join(inputsList[["nox"]], by=join0)
    rm(join0)
  } ### End if(has_o3)

  ### Get RR scalar and ozone response data
  # df_drivers |> glimpse()
  df_drivers <- df_drivers |> format_ghg_drivers()
  # df_drivers$model |> unique() |> print()
  # df_drivers |> glimpse()
  # return(df_drivers)

  #### Socioeconomic Drivers & Scalars ----------------
  ##### Socioeconomic Driver Scenario
  ### Update values
  gdp_df       <- inputsList[["gdp"]]
  pop_df       <- inputsList[["pop"]]
  pop_df       <- pop_df |> mutate(region = region |> str_replace_all("\\.|_|-| ", ""))
  # pop_df$region |> unique() |> sort() |> print()

  ### Calculate national population and update national scenario
  natScenario  <- gdp_df |> create_nationalScenario(pop0 = pop_df)
  rm(gdp_df, pop_df)
  # natScenario$region |> unique() |> print()
  # natScenario |> glimpse()
  # return(natScenario)

  ### Calculate for CONUS values
  seScenario   <- natScenario |> calc_conus_scenario()
  rm(natScenario)
  # seScenario$region |> unique() |> print()
  # seScenario$state |> unique() |> print()
  # seScenario |> glimpse()
  # return(seScenario)

  ### Calculate Scalars ----------------
  ### Initialized results: Join sector info and default scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars
  paste0("Calculating scalars...") |> message()
  df_scalars   <- seScenario |> calc_ghg_scalars(elasticity = elasticity)
  # df_scalars |> glimpse()
  # return(df_scalars)
  # df_scalars |> glimpse()

  ### Calculate Impacts ----------------
  paste0("Calculating impacts...") |> message()
  #### Mortality ----------------
  #### - Calculate Mortality Rate
  #### - Calculate Excess Mortality
  dfMort0      <- df_scalars |> calc_ghg_mortality()
  dfMort0      <- "mort" |> calc_ghg_impacts(df0=dfMort0, df1=df_drivers)
  # dfMort0 |> filter(sector |> is.na()) |> glimpse()
  # dfMort0 |> glimpse()
  # return(dfMort0)

  #### Morbidity ----------------
  #### Calculate Mortality Rate
  dfMorb0      <- df_scalars |> calc_ghg_morbidity()
  dfMorb0      <- "morb" |> calc_ghg_impacts(df0=dfMorb0, df1=df_drivers)
  # dfMorb0 |> filter(sector |> is.na()) |> glimpse()
  # dfMorb0 |> glimpse()
  # return(dfMorb0)

  ### Format Results ----------------
  ### Add in model info
  paste0("Formatting results", "...") |> message()

  ### Select common names
  namesMort0   <- dfMort0 |> names()
  namesMorb0   <- dfMorb0 |> names()
  namesBoth0   <- namesMort0 |> get_matches(namesMorb0)
  ### Add NAs to missing columns
  naMort0      <- namesMort0 |> get_matches(namesBoth0, matches=F)
  naMorb0      <- namesMorb0 |> get_matches(namesBoth0, matches=F)
  # naMort0 |> print(); naMorb0 |> print()
  dfMort0[,naMorb0] <- NA
  dfMorb0[,naMort0] <- NA
  # namesBoth0 |> print()
  namesMort0   <- dfMort0 |> names()
  namesMorb0   <- dfMorb0 |> names()
  namesBoth0   <- namesMort0 |> get_matches(namesMorb0)
  dfMort0      <- dfMort0 |> select(all_of(namesBoth0))
  dfMorb0      <- dfMorb0 |> select(all_of(namesBoth0))
  df_results   <- dfMort0 |> bind_rows(dfMorb0)
  # df_results |> glimpse();
  # return()
  # return(df_results)
  rm(dfMort0, dfMorb0)

  ### Add module
  df_results   <- df_results |> mutate(module="GHG", .before="sector")

  ### Add module sector label
  join0      <- c("sector")
  select0    <- join0 |> c("sector_label")
  dfSects0   <- ghgData$ghgData$co_sectors |> select(all_of(select0))
  df_results <- df_results |> left_join(dfSects0, by=join0)
  rm(join0, select0, dfSects0)
  # "got here1" |> print()
  # return(df_results)
  # df_results |> glimpse();

  ### Add region label
  join0      <- c("region")
  select0    <- join0 |> c("region_label")
  dfRegions0 <- ghgData$ghgData$co_regions |> select(all_of(select0))
  # df0 |> glimpse()
  df_results <- df_results |> left_join(dfRegions0, by=join0)
  # df_results |> glimpse()
  # return(df_results)
  rm(join0, select0, dfRegions0)
  # "got here2" |> print()

  ### Drop and rename
  from0      <- c("sector", "impactType", "region", "model")
  to0        <- c(from0) |> paste0("_label")
  df_results <- df_results |>
    select(-any_of(from0)) |>
    rename_at(c(to0), ~from0)
  rm(from0, to0)
  # return(df_results)
  # "got here3" |> print()

  ### Format driver values and add module
  from0      <- c("O3_pptv")
  to0        <- c("driverValue")
  move0      <- "driver" |> paste0(c("Type", "Unit", "Value"))
  df_results <- df_results |>
    rename_at(c(from0), ~to0) |>
    mutate(driverType  = "Ozone Concentration") |>
    mutate(driverUnit  = "pptv") |>
    relocate(any_of(move0), .before="year")
  # return(df_results)
  # df_results <- df_results |> mutate(module = "GHG") |> relocate(c("module"))
  # df_results <- df_results |> mutate(physicalmeasure = "Excess Mortality")

  ### Columns
  idCols0    <- c("module", "sector", "impactType", "endpoint", "ageType", "ageRange") |>
    c("region", "state", "postal", "model", "year")
  modCols0   <- c("driver") |> paste0(c("Type", "Unit", "Value"))
  natCols0   <- c("pop", "gdp_usd", "national_pop", "gdp_percap")
  # valCols0   <- c("physicalmeasure")
  valCols0   <- c()
  sumCols0   <- c("physical_impacts", "annual_impacts")
  # idCols0 |> print(); modCols0 |> print(); natCols0 |> print(); valCols0 |> print(); sumCols0 |> print()
  select0    <- idCols0 |> c(modCols0, natCols0, valCols0, sumCols0) |> unique()
  arrange0   <- idCols0 |> unique()

  ### Select columns
  # select0 |> glimpse()
  if(!allCols) {df_results <- df_results |> select(any_of(select0))}

  ### Arrange data
  df_results <- df_results |> relocate(any_of(select0))
  # df_results <- df_results |> arrange_at(c(arrange0))


  ### Return Object ----------------
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

  ### Return ----------------
  ### Message, clear unused memory, return
  paste0("\n", "Finished", ".") |> message()
  dbDisconnect(conn)
  gc()
  return(returnObj)

} ### End function








