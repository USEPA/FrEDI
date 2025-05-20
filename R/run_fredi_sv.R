###### Documentation ######
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
###### run_fredi_sv ######
### This function creates a data frame of annual average impacts over the years 2010-2100, from default values or scenario inputs, for a subset of FrEDI sectors as a function of SV group, sector, and region.
run_fredi_sv <- function(
    sector      = NULL, ### Vector of sectors to get results for
    inputsList  = list(pop=NULL, temp=NULL, slr=NULL),
    silent      = TRUE, ### Whether to message the user
    .testing    = FALSE
){
  ###### Set up the environment ######
  pkgPath       <- NULL
  pkgPath       <- (pkgPath |> is.null()) |> ifelse(system.file(package="FrEDI"), pkgPath);
  rDataType     <- "rds"
  impactsPath   <- pkgPath |> file.path("extdata", "sv", "impactLists")


  ### Load Database
  conn <-  load_frediDB()

  ###### ** Load Data Objects ######
  ### Get FrEDI data objects
  # fredi_config  <- "fredi_config"  |> get_frediDataObj("frediData")
  #fredi_config  <- rDataList[["fredi_config"]]
  fredi_config    <- DBI::dbReadTable(conn,"fredi_config")
  fredi_config    <- unserialize(fredi_config$value |> unlist())
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}

  # co_sectors    <- svDataList[["sectorInfo"]] |> select(c("sector", "modelType"))
  #co_inputInfo  <- "co_inputInfo"  |> get_frediDataObj("frediData")
  #co_modTypes   <- "co_modelTypes" |> get_frediDataObj("frediData")
  #co_states     <- "co_states"     |> get_frediDataObj("frediData")
  #temp_default  <- "temp_default"  |> get_frediDataObj("frediData")
  #pop_default   <- "pop_default"   |> get_frediDataObj("stateData")

  co_inputInfo  <- DBI::dbReadTable(conn,"co_inputInfo")
  co_modTypes   <- DBI::dbReadTable(conn,"co_modelTypes")
  co_states     <- DBI::dbReadTable(conn, "co_states")

  scenarioData    <- DBI::dbReadTable(conn,"scenarioData")
  scenarioData    <- unserialize(scenarioData$value |> unlist())
  temp_default  <- scenarioData[["gcam_default"]]
  pop_default   <- scenarioData[["pop_default"]]

  ### SVData Lists
  svDataList    <- DBI::dbReadTable(conn,"svDataList")
  svDataList    <- unserialize(svDataList$value |> unlist())

  svPopList    <- DBI::dbReadTable(conn,"svPopList")
  svPopList    <- unserialize(svPopList$value |> unlist())

  format_styles    <- DBI::dbReadTable(conn,"format_styles")
  format_styles    <- unserialize(format_styles$value |> unlist())
  ### Group types
  c_svGroupTypes <- svDataList$c_svGroupTypes
  minYear    <- minYear0
  maxYear    <- maxYear0
  yearsBy5   <- minYear |> seq(maxYear, by=5)

  ### Testing
  save       <- .testing |> ifelse(FALSE, save)

  ### Level of messaging (default is to message the user)
  silent     <- (silent |> is.null()) |> ifelse(T, silent)
  msgUser    <- !silent
  msg0       <- ""
  msg1       <- msg0 |> paste0("\t")
  msg2       <- msg1 |> paste0("\t")
  msg3       <- msg2 |> paste0("\t")

  ###### ** State Columns ######
  byState    <- TRUE
  popCol0    <- "pop"
  stateCols0 <- c("state", "postal")


  ###### ** Sector Info ######
  sectorInfo    <- svDataList$sectorInfo
  svSectorInfo  <- svDataList$svSectorInfo
  svDemoInfo    <- svDataList$svDemoInfo
  svValidTypes  <- svDataList$svValidTypes
  co_formatting <- svDataList$co_formatting
  co_formatting <- svDataList$co_formatting

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
  df_validGroups  <- c_popWtCol |> get_validGroups(df0  = svDataList[["svDemoInfo"  ]], ### svDemoInfo
                                                   df1  = svDataList[["svValidTypes"]])


  ###### ** Model Types List ######
  ### Which model types are in play based on sector selection
  modTypes0    <- df_sectorInfo |> pull(modelType) |> unique()
  modTypesIn0  <- co_modTypes   |> filter(modelType_id %in% modTypes0 ) |> pull(inputName) |> unique()
  doSlr        <- ("slr" %in% modTypes0)
  doGcm        <- ("gcm" %in% modTypes0)
  if(doSlr) modTypesIn <- c("temp") |> c(modTypesIn0) else      modTypesIn <- modTypesIn0
  modInputs0   <- c("pop") |> c(modTypesIn)

  ###### Inputs List ######
  ###### ** Input Info ######
  paste0(msg1, "Checking scenarios...") |> message()
  ### Add info to data
  #co_inputInfo <- "co_inputInfo" |> get_frediDataObj("frediData")
  co_inputInfo <- co_inputInfo |> mutate(ref_year = c(1995, 2000, 2010, 2010))
  co_inputInfo <- co_inputInfo |> mutate(min_year = c(2000, 2000, 2010, 2010))
  co_inputInfo <- co_inputInfo |> mutate(max_year = maxYear)
  co_inputInfo <- co_inputInfo |> filter(inputName %in% modInputs0)
  # co_inputInfo |> glimpse()

  ### Initialize subset
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
    scenarioData   <- DBI::dbReadTable(conn,"scenarioData")
    scenarioData   <- unserialize(scenarioData$value |> unlist())
    df0      <- scenarioData[[defName0]]
    ### Format data
    if(doTemp0) df0 <- df0 |> select(c("year", "temp_C_conus")) |> rename_at(c("temp_C_conus"), ~"temp_C")
    if(doSlr0 ) df0 <- df0 |> select(c("year", "slr_cm"      ))
    ### Add scenario column
    df0      <- df0 |> mutate(scenario = "FrEDI Default")
    ### Return
    return(df0)
  }) |> set_names(inNames0)


  ###### ** Input Columns ######
  ### Get list with minimum, maximum years associated with inputs
  ### Get lists with expected name of columns used for unique ids
  ### Get list with expected name of column containing values
  minYrs0      <- inNames0 |> map(function(name0, df0=co_inputInfo){df0 |> filter(inputName==name0) |> pull(min_year) |> unique()})
  maxYrs0      <- inNames0 |> map(function(name0, df0=co_inputInfo){df0 |> filter(inputName==name0) |> pull(max_year) |> unique()})
  valCols0     <- co_inputInfo |> pull(valueCol) |> as.list() |> set_names(inNames0)
  idCols0      <- list(valCols0=valCols0, df0=inputDefs[inNames0]) |> pmap(function(valCols0, df0){
    df0 |> names() |> get_matches(y=valCols0, matches=F)
  }) |> set_names(inNames0)
  # valCols0 |> print(); idCols0 |> print()

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
    ### Divide temperature or SLR inputs into multiple scenarios
    inputsList   <- list(name0=inputsList |> names(), df0=inputsList) |> pmap(function(name0, df0){
      names0  <- df0 |> names()
      doScen0 <- name0 %in% c("temp", "slr")
      doScen1 <- "scenario" %in% names0
      doList0 <- doScen0 & doScen1
      # name0 |> print(); names0 |> print(); c(doScen0, doScen1, doList0) |> print()
      if(doList0) {
        scen0  <- df0 |> pull(scenario) |> unique()
        nScen0 <- scen0 |> length()
        names1 <- name0 |> rep(nScen0)
        list1  <- scen0 |> map(function(scen_i, df_i=df0){df0 |> filter(scenario == scen_i)}) |> set_names(names1)
      } else{
        list1  <- list()
        list1[[name0]] <- df0
      } ### End if(doList0)
      ### Return
      return(list1)
      # name0 |> print()
      # return(df0)
    }) |> set_names(inputsList |> names())

    ### Unlist one level and format names
    inputsList <- inputsList |> unlist(recursive=FALSE)
    inputsList <- inputsList |> (function(list0, names0=inNames){
      lNames0 <- list0   |> names()
      str0    <- names0  |> paste0("\\.") |> paste(collapse="|")
      lNames1 <- lNames0 |> str_replace(pattern=str0, "")
      list0   <- list0   |> set_names(lNames1)
      return(list0)
    })()
    ### Update names
    inNames    <- inputsList |> names()

    ### Check input data
    inputsList <- list(
      inputName = inNames,
      inputDf   = inputsList,
      idCol     = idCols0 [inNames],
      valCol    = valCols0[inNames],
      yearMin   = minYrs0 [inNames],
      yearMax   = maxYrs0 [inNames],
      module    = "sv" |> rep(inNames |> length())
    ) |>
      pmap(check_input_data) |>
      set_names(inNames)

    ### Check again for inputs
    ### Filter to values that are not NULL
    inWhich      <- inNames    |> map(function(name0, list0=inputsList){!(list0[[name0]] |> is.null())}) |> unlist() |> which()
    inputsList   <- inputsList[inWhich]
    inNames      <- inputsList |> names()
    rm(inWhich)
  } ### if(hasAnyInputs)


  ### If SLR is missing but user provided a temperature scenario, update with new temperature scenario
  ### If there are no GCM sectors, drop temperature
  if(doSlr) {
    if(!("slr" %in% inNames)) {
      if("temp" %in% inNames) {
        inputsList <- inputsList |> (function(list0, y="slr"){list0[!((list0 |> names() %in% y))]})()
        inputsList <- inputsList |> set_names(inNames |> str_replace("temp", "slr"))
        inNames    <- inputsList |> names()
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

  ### Iterate over list and format values
  if(hasInputs) {
    inputsList   <- list(
      name0     = inNames,
      df0       = inputsList,
      hasInput0 = TRUE |> rep(inNames |> length()),
      idCols0   = idCols0 [inNames],
      valCols0  = valCols0[inNames]
    ) |> pmap(function(df0, name0, hasInput0, idCols0, valCols0){
      ### If scenario present, get unique scenario and drop column from data
      # scenCol <- "scenario"
      # doScen0 <- scenCol %in% (df0 |> names())
      # if(doScen0) {
      #   scen0 <- df0 |> pull(all_of(scenCol)) |> unique()
      #   df0   <- df0 |> select(-any_of(scenCol))
      # } ### End if(doScen0)
      ### Format input scenario
      df0     <- df0 |> format_inputScenarios(
        name0     = name0,
        hasInput0 = hasInput0,
        idCols0   = idCols0,
        valCols0  = valCols0,
        minYear   = minYear,
        maxYear   = maxYear,
        info0     = co_inputInfo
      ) ### End format_inputScenarios
      # ### If scenario present, add unique scenario to data
      # if(doScen0) df0[[scenCol]] <- scen0
      ### Return
      return(df0)
    }) |> set_names(inNames)

    ### Iterate over types of names and row bind similar scenarios
    inNames      <- inputsList |> names() |> unique()
    inputsList   <- inNames |> map(function(name0, list0=inputsList){
      which0 <- (inputsList |> names()) %in% name0
      list0  <- list0[which0]
      list0  <- list0 |> bind_rows()
      return(list0)
    }) |> set_names(inNames)
  } ### End if(hasInputs)
  ### Free memory
  gc()

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
  ### Free memory
  gc()
  # inputsList |> names() |> print()
  ### Update names
  inNames      <- inputsList |> names()
  df_inputInfo <- co_inputInfo |> filter(inputName %in% inNames)

  ### Filter to lists
  inputsList   <- inputsList |> map(function(df0, minYr0=minYear, maxYr0=maxYear){
    df0 <- df0 |> filter(year >= minYear, year <= maxYear)
    df0 <- df0 |> filter(year %in% yearsBy5)
    return(df0)
  }) |> set_names(inNames)



  ###### Format Scenarios ######
  ###### ** Driver Scenarios ######
  ### Subset to driver scenario
  drivers_df  <- inputsList[[modTypesIn0]]

  ### Get unique scenarios
  c_scenarios <- drivers_df  |> pull(scenario) |> unique()

  ### Rename column
  modValCol0  <- df_inputInfo |> filter(inputName %in% modTypesIn0) |> pull(valueCol) |> unique()
  renameAt0   <- c(modValCol0)
  renameTo0   <- c("driverValue")
  drivers_df  <- drivers_df  |> rename_at(c(renameAt0), ~renameTo0)

  ### Add driver unit
  df_modTypes <- co_modTypes |> filter(inputName %in% modTypesIn0)
  modUnit0    <- df_modTypes |> pull(modelUnit_label) |> unique()
  drivers_df  <- drivers_df  |> mutate(driverUnit = modUnit0)



  ###### ** Population Scenario ######
  pop_df     <- inputsList[["pop"]]

  ### Standardize population data
  # c(minYear, maxYear) |> print(); yearsBy5 |> range() |> print()
  pop_df     <- pop_df |> mutate(region = region |> str_replace_all("\\.", " "))
  # pop_df <- pop_df |> mutate(region = region |> str_replace_all(" ", " "))

  ### Calculate county population
  message(msg1, "Calculating county population from state population...")
  df_popProj <- pop_df |> get_countyPop(
    years   = yearsBy5,
    xCol0   = "year",     ### X column in df0
    yCol0   = "state_pop", ### Y column in df0
    funList = svPopList[["popProjList"]]
  ) ### End get_countyPop


  ###### Calculate Impacts ######
  ### Iterate over adaptations/variants
  listResults <- list()
  cRows       <- df_sectorInfo |> nrow() |> seq_len()
  msgSector   <- "Calculating impacts for sector=\"" |> paste0(c_sector, "\"")

  for(row_i in cRows) {
    ### Which SV data to use
    svName_i     <- (c_sector=="Coastal Properties") |> ifelse("svDataCoastal", "svData")
    svInfo_i     <- svDataList[[svName_i]]

    # scenarios_x |> print(); # svName_i |> print()
    sectorAbbr_i <- df_sectorInfo[["impactList_fileExt"]][row_i]
    varLabel_i   <- df_sectorInfo[["variant_label"     ]][row_i]
    varAbbr_i    <- df_sectorInfo[["variant_abbr"      ]][row_i]
    weightsCol_i <- df_sectorInfo[["popWeightCol"      ]][row_i]

    ### Which impacts list to use
    # varAbbr_i     <- varAbbr_i |> (function(y){y |> is.na() |> ifelse(NULL, y)})()
    if(varAbbr_i |> is.na()){varAbbr_i <- NULL}
    msgVar_i      <- "variant=\"" |> paste0(varLabel_i, "\"")

    ### Read in the file
    impactsName_i <- "impactsList" |> c(sectorAbbr_i, varAbbr_i) |> paste(collapse="_")
    impactsPath_i <- impactsPath   |> file.path(impactsName_i) |> paste0(".", rDataType)
    impactsList   <- impactsPath_i |> readRDS()
    exists_i      <- "impactsList" |> exists()


    ### Iterate over scenarios, and calculate tract impacts
    impacts_i     <- list()
    for(scenario_j in c_scenarios) {
      ### Message user
      msgScen_j <- "scenario=\"" |> paste0(scenario_j, "\"")
      "\n" |> paste0(msg1, msgSector) |> paste(msgVar_i, msgScen_j, sep=", ") |> paste0("...") |> message()
      ### Filter drivers
      drivers_j <- drivers_df |> filter(scenario == scenario_j)
      ### Calculate scaled impacts
      impacts_j <- calc_tractScaledImpacts(
        funList      = impactsList,
        driverValues = drivers_j,
        silent       = silent,
        .msg0        = msg2
      ) ### End calc_tractScaledImpacts
      impacts_j <- impacts_j |> ungroup()
      ### Add to list and drop values
      impacts_i[[scenario_j]] <- impacts_j
      rm(scenario_j, drivers_j, impacts_j)
    } ### End for(scenario_j in c_scenarios)
    ### Add list names
    impacts_i     <- impacts_i |> set_names(c_scenarios)
    if(exists_i){remove(list=c("impactsList"), inherits=T)}
    exists_i      <- "impactsList" |> exists()
    if(exists_i){rm(impactsList)}

    ### Iterate over scenarios, calculate tract impacts
    for(scenario_j in c_scenarios) {
      ### Message user
      # msgScen_j <- "scenario=\"" |> paste0(scenario_j, "\"")
      # "\n" |> paste0(msg1, msgSector) |> paste(msgVar_i, msgScen_j, sep=", ") |> paste0("...")
      ### Confirm year is numeric and filter out missing impacts
      impacts_j <- impacts_i[[scenario_j]]
      impacts_j <- impacts_j |> mutate(year = year |> as.character() |> as.numeric())

      ### Calculate impacts by tract
      impacts_j <- impacts_j |> calc_tractImpacts(
        sector    = c_sector,
        popData   = df_popProj,
        svInfo    = svInfo_i,
        svGroups  = c_svGroupTypes,
        weightCol = weightsCol_i,
        years     = yearsBy5,
        silent    = silent,
        .msg0     = msg2,
        .testing  = .testing
      ) ### End calc_tractImpacts
      impacts_j <- impacts_j |> ungroup()
      ### Add to list and drop values
      impacts_i[[scenario_j]] <- impacts_j
      rm(scenario_j, impacts_j)
    } ### End for(scenario_j in c_scenarios)

    ###### Bind Results
    ### Bind results and add variant level
    impacts_i <- impacts_i |> bind_rows(.id="scenario")
    impacts_i <- impacts_i |> mutate(variant = varLabel_i)
    impacts_i <- impacts_i |> relocate(c("scenario"))

    ###### Adjust SV Group Values
    if(!.testing){
      valSuff0  <- c("ref", "sv")
      ### Join and adjust results valueAdj
      valCols0  <- c("impPop", "impact", "national_highRiskPop", "regional_highRiskPop", "aveRate")
      valCols1  <- valCols0  |> map(function(col_j){col_j |> paste(valSuff0, sep="_")}) |> unlist()
      drop0     <- c("validGroups", "weightCol", "validType", "valueAdj")
      ### Adjust results
      impacts_i <- impacts_i |> left_join(df_validGroups, by = c("svGroupType"))
      impacts_i <- impacts_i |> mutate_at(vars(valCols1), function(col_j){col_j * impacts_i$valueAdj})
      impacts_i <- impacts_i |> select(-all_of(drop0))
      rm(drop0)
    } ### End if(!.testing)
    ### Add impacts_i to list
    listResults[[row_i]] <- impacts_i
    rm(row_i, impacts_i)
  } ### End for(row_i in cRows)



  ###### Format Results ######
  ### Bind results and relocate columns
  move0       <- c("sector", "variant", "scenario")
  listResults <- listResults |> bind_rows()
  listResults <- listResults |> mutate(sector = c_sector)
  listResults <- listResults |> relocate(all_of(move0))

  # if(.testing) {listResults <- list(results = listResults, county_pop = df_popProj)}
  # else         {listResults <- listResults}



  ###### Return Object ######
  ### Message, clear unused memory, return
  msg1 |> paste0("Finished.") |> message()
  dbDisconnect(conn)
  gc()
  return(listResults)
}








