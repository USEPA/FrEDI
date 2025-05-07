## Documentation  ----------------
#' Calculate annual average impacts from temperature and sea level change with distribution among different populations throughout the 21st century for available sectors
#'
#' @description
#' `run_fredi_sv` allows users to project annual average impacts from temperature and sea level change throughout the 21st century (2010-2100) with distribution among different populations for available sectors. Users can run [FrEDI::run_fredi_sv()] for individual sectors to generate annual physical impacts for SV populations. [FrEDI::run_fredi_sv()] can be run with default population, temperature, and sea level trajectories or using custom trajectories. The output of [FrEDI::run_fredi_sv()] is an R data frame object containing annual average physical impacts at five-year increments for the period 2010 to 2100. The basic structure, specific methodology, and underlying data supporting FrEDI-SV are derived from EPA’s independently peer-reviewed September [2021 report](https://epa.gov/cira/social-vulnerability-report).
#'
#'
#'
#' @param sector A character string indicating the sector for which the FrEDI SV module should calculate impacts (see [FrEDI::get_sv_sectorInfo()] for a list of available sectors).
#'
#' @param inputsList=list(pop=NULL,temp=NULL,slr=NULL) A list with named elements (`pop`, `temp`, and/or `slr`), each containing data frames of custom scenarios for state-level population, temperature, and/or global mean sea level rise (GMSL) trajectories, respectively, over a continuous period. Temperature and sea level rise inputs should start in 2000 or earlier. Values for population scenarios can start in 2010 or earlier. Values for each scenario type must be within reasonable ranges. For more information, see [FrEDI::import_inputs()].
#'    * __pop__. The input population scenario requires a data frame object with a single scenario of population values for each of the 48 U.S. states and the District of Columbia comprising the contiguous U.S. (CONUS).
#'        * The population scenario must have five columns with names `"region"`, `"state"`, `"postal"`, `"year"`, and `"_pop"` containing the NCA region name (`"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"`), the state name, the two-letter postal code abbreviation for the state (e.g., `"ME"` for Maine), the year, and the state population, respectively.
#'        * The input population scenario can only contain a single scenario, in contrast to values for temperature or SLR inputs. In other words, [FrEDI::run_fredi_sv()] uses the same population scenario when running any and all of the temperature or SLR scenarios passed to `run_fredi_sv()`.
#'        * If the user does not specify an input scenario for population (i.e., `popInput = NULL`, [FrEDI::run_fredi_sv()] uses a default population scenario (see documentation for [FrEDI::popScenario]).
#'        * Population inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (2100).
#'        * Population values must be greater than or equal to zero.
#'    * __temp__ or __slr__. The input temperature or SLR scenario should be a data frame containing one or more custom scenarios. These inputs should be formulated similarly to those for [FrEDI::run_fredi()] and [FrEDI::import_inputs()], with an additional column (`scenario`) indicating the unique scenario identifier. Temperature and/or SLR input scenarios must have at least one non-missing value in the year 2000 or earlier and at least one non-missing value in or after the final analysis year (2100).
#'        * __temp__. Temperature inputs are used by `run_fredi_sv()` with temperature-driven sectors; run `get_sv_sectorInfo(gcmOnly=TRUE)` to get a list of the temperature-driven sectors available for the SV module. Temperature inputs require a data frame with columns of `year`, `temp_C`, and `scenario`, respectively containing the year associated with an observation, temperature values for CONUS in degrees Celsius of warming relative to a 1995 baseline (where 1995 is the central year of a 1986-2005 baseline period -- i.e., values should start at zero in the year 1995), and a unique scenario identifier. If no temperature scenario is specified (i.e., `inputsList$temp` is `NULL`) when running a temperature-driven sector, `run_fredi_sv()` will use a default temperature scenario (see [FrEDI:gcamScenarios]).
#'        * __slr__. SLR inputs are used by `run_fredi_sv()` with sea level rise-driven sectors; run `get_sv_sectorInfo(slrOnly=TRUE)` to get a list of the SLR-driven sectors available for the SV module. SLR inputs require a data frame with columns of `year`, `slr_cm`, and `scenario`, respectively containing the global mean sea level rise in centimeters relative to a 2000 baseline (i.e., values should start at zero in the year 2000), and a unique scenario identifier. If no SLR scenario is specified (i.e., `inputsList$slr` is `NULL`) when running a temperature-driven sector: if a user has supplied a temperature scenario (i.e., `inputsList$temp` is not `NULL`), `run_fredi_sv()` will calculate sea level rise values from the temperature inputs using the [FrEDI::temps2slr()] function; if no temperature scenario is provided, `run_fredi_sv` will use a default SLR scenario (see [FrEDI:gcamScenarios]).
#'
#' @param silent A logical (`TRUE/FALSE`) value indicating the level of messaging desired by the user (defaults to `silent=TRUE`).
#'
#'
#'
#' @details [FrEDI::run_fredi_sv()] projects annual average impacts from temperature and sea level change throughout the 21st century (2010-2100) for available sectors, with distribution among different populations, using default or user-specified population, temperature, and sea level rise (SLR) trajectories. [FrEDI::run_fredi_sv()] is the main function for the FrEDI SV module in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information). The SV module extends the [FrEDI] framework to different populations using data underlying a [2021 U.S. Environmental Protection Agency (EPA) report](https://www.epa.gov/cira/social-vulnerability-report/).
#'
#' Users can run [FrEDI::run_fredi_sv()] to generate annual physical impacts for SV groups for individual sectors. When running [FrEDI::run_fredi_sv()], users must specify one of the sectors in the SV module; use [FrEDI::get_sv_sectorInfo()] for a list of available sectors.
#'
#' [FrEDI::run_fredi_sv()] can be run with default population, temperature, and sea level trajectories or use [FrEDI::run_fredi_sv()] to run custom scenarios. Running [FrEDI::run_fredi_sv()] with custom temperature and sea level scenarios requires passing a data frame of scenarios to the `driverInput` argument. [FrEDI::run_fredi_sv()] can also be run with a custom population scenario by passing a data frame of state population trajectories to the `popInput` argument; unlike the temperature and sea level scenarios, [FrEDI::run_fredi_sv()] will only run a single population scenario at a time.
#'
#'
#' The output of [FrEDI::run_fredi_sv()] is an R data frame object containing average annual physical impacts, with distribution among different populations, at the NCA region level and five-year increments.
#'
#'
#'
#' @return
#' The output of [FrEDI::run_fredi_sv()] is an R data frame object containing average annual physical impacts, with distribution among different populations, at the NCA region level and five-year increments.
#'
#' @examples
#' ### Run SV Module with defaults without specifying sector
#' # df_sv <- run_fredi_sv()
#'
#' ### Return a character vector with the names of all of the sectors in the FrEDI SV Module:
#' get_sv_sectorInfo()
#'
#' ### Return a data frame of all of the sectors in the FrEDI SV Module (sector names and additional information)
#' get_sv_sectorInfo(description=T)
#'
#' ### Run SV Module with defaults for "Coastal Properties" without saving
#' df_sv <- run_fredi_sv(sector="Coastal Properties")
#'
#' ### Run SV Module with defaults for "Extreme Temperature" without saving
#' df_sv <- run_fredi_sv(sector="Extreme Temperature")
#'
#' ### Load temperature scenarios
#' data(gcamScenarios)
#'
#' ### Load population scenario
#' data(popDefault)
#'
#' ### Run SV Module for "Extreme Temperature" with custom population and temperature scenarios
#' df_sv <- run_fredi_sv(sector = "Extreme Temperature", inputsList=list(pop=popDefault, temp=gcamScenarios)
#'
#' ### Run SV Module for "Coastal Properties" with custom population and SLR scenarios
#' df_sv <- run_fredi_sv(sector = "Coastal Properties", inputsList=list(pop=popDefault, slr=gcamScenarios)
#'
#' ### Run SV Module for "Coastal Properties" with custom population and temperature scenarios
#' df_sv <- run_fredi_sv(sector = "Coastal Properties", inputsList=list(pop=popDefault, temp=gcamScenarios)
#'
#'
#'
#' @references
#' Bierwagen, B., D. M. Theobald, C. R. Pyke, A. Choate, P. Groth, J. V. Thomas, and P. Morefield. 2010. https://doi.org/10.1073/pnas.1002096107.
#'
#' EPA. 2017. https://doi.org/10.13140/RG.2.2.14466.79045
#'
#' EPA. 2021. Technical Documentation on the Framework for Evaluating Damages and Impacts (FrEDI). U.S. Environmental Protection Agency, EPA 430-R-21-004. Available at <https://epa.gov/cira/FrEDI/>.
#'
#' EPA. 2021. <https://www.epa.gov/cira/social-vulnerability-report/>.
#'
#' United Nations. 2015. World population prospects: The 2015 revision. New York: United Nations, Department of Economic and Social Affairs, Population Division.
#'
#' U.S. Global Change Research Program. 2015. <https://scenarios.globalchange.gov/regions_nca4>.
#'
#'
#' @export
#' @md
#'
## run_fredi_sv  ----------------
### This function creates a data frame of annual average impacts over the years 2010-2100, from default values or scenario inputs, for a subset of FrEDI sectors as a function of SV group, sector, and region.
run_fredi_sv <- function(
    sector      = NULL, ### Vector of sectors to get results for
    inputsList  = list(pop=NULL, temp=NULL, slr=NULL),
    silent      = TRUE, ### Whether to message the user
    .testing    = FALSE
){
  ### Set up the environment ----------------
  #### Messaging ----------------
  ### Level of messaging (default is to message the user)
  msgUser  <- !silent
  msgN     <- "\n"
  msg0     <- 0
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  msg3     <- msg0 + 3

  #### Module Info ----------------
  ### Assign data objects to objects in this namespace
  ### Assign FrEDI config
  fredi0        <- "fredi"
  module0       <- "sv"
  modData0      <- module0 |> fun_moduleDataStr()
  svDataList0   <- module0 |> paste0("DataList")
  frediData0    <- fredi0  |> paste0("Data")
  ctrlDataStr0  <- "controlData"
  fConfigStr0   <- "fredi_config"
  configLStr0   <- "configData"
  stateLStr0    <- "stateData"


  #### Paths ----------------
  pkgPath       <- NULL
  pkgPath       <- (pkgPath |> is.null()) |> ifelse(system.file(package="FrEDI"), pkgPath);
  rDataType     <- "rds"
  impactsPath   <- pkgPath |> file.path("extdata", "sv", "impactLists")

  #### Load Data Objects ----------------
  ### Get FrEDI data objects
  # fredi_config  <- "frediData"  |> get_frediDataObj("fredi_config")
  ### Assign config files
  # fredi_config |> list2env(envir = environment())
  # for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}

  ### Values & Columns ----------------
  ### Values
  ### Columns
  yrCol0       <- "year"
  gcmStr0      <- "gcm"
  slrStr0      <- "slr"
  tempStr0     <- "temp"
  natPost0     <- "US"

  ### Group types
  c_svGroupTypes <- svDataList0 |> get_frediDataObj("c_svGroupTypes")
  ### Model years and NPD (FrEDI past 2100)
  # maxYear |> print()
  minYear      <- frediData0 |> get_frediDataObj(fConfigStr0, "minYear0")
  maxYear      <- frediData0 |> get_frediDataObj(fConfigStr0, "maxYear0")
  maxYear      <- 2020
  # minYear      <- minYear0
  yearsBy5     <- minYear |> seq(maxYear, by=5)

  # ### Testing
  # save0        <- save; rm(save)
  # save0        <- .testing |> ifelse(FALSE, save0)


  #### State Columns ----------------
  byState    <- TRUE
  popCol0    <- "pop"
  stateCols0 <- c("state", "postal")


  #### Sector Info ----------------
  sectorInfo    <- svDataList0 |> get_frediDataObj("sectorInfo")
  svSectorInfo  <- svDataList0 |> get_frediDataObj("svSectorInfo")
  svDemoInfo    <- svDataList0 |> get_frediDataObj("svDemoInfo")
  svValidTypes  <- svDataList0 |> get_frediDataObj("svValidTypes")
  co_formatting <- svDataList0 |> get_frediDataObj("co_formatting")

  ### Sector names
  hasSector <- !(sector |> is.null())
  if(!hasSector){
    sector_msg1   <- paste0("Please select a sector: ") |> print()
    sector_msg2   <- sectorInfo |> nrow() |> seq_len() |> map(function(i){
      sector_i    <- sectorInfo$sector[i]
      msg_i       <- paste0(i, ". ", sector_i)
      msg_i |> print()
    }) ### End map(function(i)
    sector_msg3   <- "Enter a number:"
    sector_input  <- readline(prompt = sector_msg3) |> as.numeric()
    sector        <- sectorInfo$sector[sector_input]
    rm(sector_msg1, sector_msg2, sector_msg3, sector_input)
  } ### End if(hasSector)
  c_sector        <- sector
  paste0("Running FrEDI SV for sector '", c_sector, "':") |> message()

  ### Sector info
  df_sectorInfo   <- svSectorInfo  |> filter(sector == c_sector)
  c_variants      <- df_sectorInfo |> pull(variant_abbr)
  c_variantLabels <- df_sectorInfo |> pull(variant_label)
  c_popWtCol      <- sectorInfo |> filter(sector == c_sector) |> pull(popWeightCol) |> tolower()
  c_modelType     <- sectorInfo |> filter(sector == c_sector) |> pull(modelType) |> tolower()
  # df_validGroups  <- svDemoInfo |> get_validGroups(df1 = svValidTypes, col0 = c_popWtCol)
  df_validGroups  <- c_popWtCol |> get_validGroups()


  #### Model Types List ----------------
  ### Which model types are in play based on sector selection
  slrStr0      <- "slr"
  gcmStr0      <- "gcm"
  tempStr0     <- "temp"
  modTypes0    <- df_sectorInfo |> pull(modelType) |> unique()
  modInTypes0  <- modTypes0 |> str_replace(gcmStr0, tempStr0)
  doSlr0       <- slrStr0 %in% modTypes0
  doGcm0       <- gcmStr0 %in% modTypes0
  # if(doSlr0) modTypesIn <- c("temp") |> c(modTypesIn0)
  # else       modTypesIn <- modTypesIn0
  # modInputs0   <- c("pop") |> c(modTypesIn)

  ### Format Input Scenarios ----------------
  #### Initialize Lists ----------------
  msg1 |> get_msgPrefix() |> paste0("Checking input scenarios...") |> message()
  ### Get input info
  inputInfo0   <- module0 |> get_dfInputInfo(modTypes0) |> mutate(maxYear = maxYear)
  inNames0     <- inputInfo0 |> pull(inputName)
  ### Get defaults
  inputDefs    <- inputInfo0 |> get_defaultScenarios(
    mTypes0 = modTypes0,
    minYr0  = minYear,
    maxYr0  = maxYear,
    module0 = module0
  ) ### End get_defaultScenarios
  # inputDefs |> glimpse()

  ### Format inputs list
  inputsList   <- format_inputsList(
    dfInfo     = inputInfo0,
    inputsList = inputsList,
    tempType   = "conus",
    popArea    = "state",
    module0    = module0,
    msg0       = 1
  ) ### End format_inputsList
  ### Exit if inputs aren't valid
  validInputs  <- inputsList |> is.list()
  if(!validInputs) {return()}

  #### Interpolate Values ----------------
  ### Check if there are inputs
  ### If there are, iterate over list and format values
  inNames      <- inputsList |> names()
  hasInputs    <- inNames    |> length()
  # hasInputs |> print()
  if(hasInputs) {
    inputsList   <- list(
      df0     = inputsList,
      name0   = inNames
    ) |> pmap(
      format_inputScenarios,
      minYear = minYear,
      maxYear = maxYear,
      info0   = inputInfo0,
      msg0    = msg1
    ) |> set_names(inNames)
  } ### End if(hasInputs)
  # inputsList |> glimpse()

  #### Update Defaults ----------------
  ### Update inputs with defaults if values are missing
  # inputDefs |> glimpse()
  # defNames     <- inNames0 |> get_matches(inNames
  # msg1 |> get_msgPrefix() |> paste0("Using default scenarios for ", , "inputs...") |> message()
  inputsList   <- inNames0 |> map(
    update_inputDefault,
    dfInfo     = inputInfo0,
    defaults   = inputDefs,
    inputsList = inputsList,
    minYear    = minYear,
    maxYear    = maxYear,
    module0    = module0
  ) |> set_names(inNames0)
  rm(inputDefs)
  ### Drop any Null scenarios
  # inputsList |> glimpse()
  # return(inputsList)
  inputsList   <- inputsList |> drop_nullListElements(matches=FALSE)
  inNames      <- inputsList |> names()
  # inputsList |> glimpse()


  ### Format Scenarios  ----------------
  #### Format Physical Driver Scenario ----------------
  ### Get unique scenarios
  physDrivers  <- modInTypes0
  df_drivers   <- inputsList[physDrivers] |>
    combine_physDrivers(info0=inputInfo0, module0=module0) |>
    filter(year %in% yearsBy5)
  c_scenarios  <- df_drivers  |> pull(scenario) |> unique()
  # "gothere0" |> print(); df_drivers |> pull(year) |> range() |> print()


  #### Population Scenario  ----------------
  # ### Standardize population data
  ### Get areas
  areas0    <- "controlData" |>
    get_frediDataObj("co_moduleAreas") |>
    filter(module %in% module0) |>
    filter(!(area %in% "US")) |>
    pull(area)
  # areas0 |> print()
  ### Get states
  dfRegions <- "controlData" |>
    get_frediDataObj("co_states") |>
    filter(area %in% areas0) |>
    select(region_label, state, postal) |>
    rename_at(c("region_label"), ~"region")
  # dfRegions |> glimpse(); inputsList[["pop"]] |> glimpse()
  ### Standardize pop
  joinPop      <- "postal"
  pop_df       <- inputsList[["pop"]] |>
    left_join(dfRegions, by=joinPop) |>
    # mutate(region = region |> str_replace_all("\\.", " ")) |>
    filter(year %in% yearsBy5)
  rm(inputsList)
  # pop_df <- pop_df |> mutate(region = region |> str_replace_all(" ", " "))

  ### Calculate county population
  msg1 |> get_msgPrefix() |> paste0("Calculating county population from state population...") |> message()
  pop_df       <- pop_df |> get_countyPop(
    years   = yearsBy5,
    xCol0   = "year",     ### X column in df0
    yCol0   = "pop" ### Y column in df0
  ) ### End get_countyPop
  # return(df_popProj)

  ### Calculate Impacts ----------------
  ### Iterate over adaptations/variants
  listResults <- list()
  # c_tracts    <- pop_df$geoid0 |> unique()
  cRows       <- df_sectorInfo |> nrow() |> seq_len()
  # msgSector   <- "Calculating impacts for sector='" |> paste0(c_sector, "'")
  df_impacts  <- cRows |> map_svVariant(
    info_i      = df_sectorInfo, ### df_sectorInfo
    sector_i    = c_sector,
    # tracts_i    = c_tracts,
    scenarios_i = c_scenarios, ### List of scenarios
    df0         = df_drivers, ### Dataframe of driver values for one scenario with columns driverValue, driverUnit, year
    funPath     = impactsPath,
    xCol        = "driverValue",
    popData     = pop_df,
    svGroups    = c_svGroupTypes,
    dfGroups    = df_validGroups,
    dataExt = rDataType,
    sleep   = 1e-7,
    silent  = FALSE,
    .msg0   = msg1
  ) |> bind_rows() ### End map_svVariant

  ### Iterate over variants


  ### Format Results  ----------------
  ### Bind results and relocate columns
  move0       <- c("sector", "variant", "scenario")
  listResults <- df_impacts |>
    mutate(sector = c_sector) |>
    relocate(any_of(move0))

  # if(.testing) {listResults <- list(results = listResults, county_pop = df_popProj)}
  # else         {listResults <- listResults}



  ### Return  ----------------
  ### Message, clear unused memory, return
  msg1 |> get_msgPrefix(newline=T) |> paste0("Finished.") |> message()
  gc()
  return(listResults)
}








