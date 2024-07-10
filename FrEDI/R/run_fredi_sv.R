###### Documentation ######
#' Calculate climate change impacts on socially vulnerable (SV) populations throughout the 21st century for available sectors
#'
#' @description
#' `run_fredi_sv` allows users to project annual average climate change impacts throughout the 21st century (2010-2100) for socially vulnerable (SV) populations for available sectors. Users can run [FrEDI::run_fredi_sv()] for individual sectors to generate annual physical impacts for SV populations. [FrEDI::run_fredi_sv()] can be run with default population and climate (temperature and sea level rise trajectories) or using custom trajectories. The output of [FrEDI::run_fredi_sv()] is an R data frame object containing annual average physical impacts at five-year increments for the period 2010 to 2100. The basic structure, specific methodology, and underlying data supporting FrEDI-SV are derived from EPA’s independently peer-reviewed September 2021 report, [Climate Change and Social Vulnerability in the United States: A Focus on Six Impacts](https://epa.gov/cira/social-vulnerability-report)
#'
#'
#'
#' @param sector A character string indicating the sector for which the FrEDI SV module should calculate impacts (see [FrEDI::get_sv_sectorInfo()] for a list of available sectors).
#'
#' @param driverInput A data frame of up to four custom scenarios for drivers (temperature or global mean sea level rise). `driverInput` requires a data frame with columns of `"year"` and `"scenario"`. The data frame must also include a third column: `"temp_C"` for temperature-driven sectors (containing temperature values in degrees Celsius of warming for the contiguous U.S.) or `"slr_cm"` for sea level rise (SLR)-driven sectors (containing values for global mean sea level rise in centimeters). Run `get_sv_sectorInfo(gcmOnly=TRUE)` to see temperature-driven sectors in the SV module and `get_sv_sectorInfo(slrOnly=TRUE)` to see SLR-driven scenarios. Users can also pass a data frame with all four columns (`"year"`, `"scenario"`, `"temp_C"`, and `"slr_cm"`), in which case [FrEDI::run_fredi_sv()] determines whether to use the `"temp_C"` or `"slr_cm"` column as the driver trajectory based on the specified sector. Driver inputs for all scenarios should start in the year 2000 or earlier. All scenarios must include at least two non-missing values  (especially values before or at 2000 and at or after 2100). If any required columns are missing, [FrEDI::run_fredi_sv()] will use the default temperature or sea level rise scenario from [FrEDI::run_fredi()]. If the data frame passed to `driverInput` has more than four unique scenarios, [FrEDI::run_fredi_sv()] will only run the first four scenarios.
#'
#' @param popInput The input population scenario requires a data frame object with a single scenario of state-level population values.
#'    * The population scenario must have five columns with names `"year"`, `"region"`, `"state"`, `"postal"`, and `"state_pop"` containing the year, the NCA region name, the state name, the postal code abbreviation (e.g., "ME" for "Maine") for the state, and the state population, respectively.
#'    * `popInput` only accepts a a single scenario, in contrast to `driverInput`. In other words, [FrEDI::run_fredi_sv()] uses the same population scenario for any and all driver scenarios passed to `driverInput`.
#'    * If the user does not specify an input scenario for population (i.e., `popInput = NULL`, [FrEDI::run_fredi_sv()] uses a default population scenario.
#'    * Population inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (2100).
#'    * Population values must be greater than or equal to zero.
#'
#' @param silent A logical (`TRUE/FALSE`) value indicating the level of messaging desired by the user (defaults to `silent=TRUE`).
# @param return=TRUE A `TRUE/FALSE` value indicating whether to return the results as a data frame (default=`TRUE`).
#'
#'
#'
#' @details [FrEDI::run_fredi_sv()] projects annual climate change impacts for socially vulnerable (SV) populations throughout the 21st century (2010-2100) for available sectors, using default or user-specified population, temperature, and sea level rise (SLR) trajectories. [FrEDI::run_fredi_sv()] is the main function for the FrEDI Social Vulnerability (SV) module in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information). The SV module extends the [FrEDI] framework to socially vulnerable populations using data underlying a 2021 U.S. Environmental Protection Agency (EPA) report on [Climate Change and Social Vulnerability in the United States](https://www.epa.gov/cira/social-vulnerability-report/).
#'
#' Users can run [FrEDI::run_fredi_sv()] to generate annual physical impacts for SV groups for individual sectors. When running [FrEDI::run_fredi_sv()], users must specify one of the sectors in the SV module; use [FrEDI::get_sv_sectorInfo()] for a list of available sectors.
#'
#' [FrEDI::run_fredi_sv()] can be run with default population and climate (temperature and SLR) trajectories or use [FrEDI::run_fredi_sv()] to run custom scenarios. Running [FrEDI::run_fredi_sv()] with custom climate scenarios requires passing a data frame of scenarios to the `driverInput` argument. [FrEDI::run_fredi_sv()] can also be run with a custom population scenario by passing a data frame of regional population trajectories to the `popInput` argument; unlike climate scenarios, [FrEDI::run_fredi_sv()] will only run a single scenario at a time.
#'
#' * `driverInput` can take a data frame containing up to four custom scenarios for drivers (temperature or global mean sea level rise). `driverInput` requires a data frame with columns of `"year"` and `"scenario"`. The data frame must also include a third column: `"temp_C"` for temperature-driven sectors (containing temperature values in degrees Celsius of warming for the contiguous U.S.) or `"slr_cm"` for sea level rise (SLR)-driven sectors (containing values for global mean sea level rise in centimeters). Run `get_sv_sectorInfo(gcmOnly = TRUE)` to see temperature-driven sectors in the SV module and `get_sv_sectorInfo(slrOnly = TRUE)` to see SLR-driven scenarios. Users can also pass a data frame with all four columns (`"year"`, `"scenario"`, `"temp_C"`, and `"slr_cm"`), in which case [FrEDI::run_fredi_sv()] determines whether to use the `"temp_C"` or `"slr_cm"` column as the driver trajectory based on the specified sector. If any required columns are missing, [FrEDI::run_fredi_sv()] will use the default temperature or sea level rise scenario from [FrEDI::run_fredi()]. If the data frame passed to `driverInput` has more than four unique scenarios, [FrEDI::run_fredi_sv()] will only run the first four scenarios.
#'     * Temperature inputs must be temperature change in degrees Celsius for the contiguous U.S. (use [FrEDI::convertTemps()] to convert global temperatures to CONUS temperatures before passing to `driverInput`) relative to a 1995 baseline (where 1995 is the central year of a 1986-2005 baseline period; values should start at zero in the year 1995).
#'     * Sea level rise inputs must be in centimeters relative to a 2000 baseline (i.e., values should start at zero in the year 2000). Driver inputs for all scenarios should start in the year 2000 or earlier. All scenarios must include at least two non-missing values  (especially values before or at 2000 and at or after 2100).
#' * The input population scenario requires a data frame object with a single scenario of state-level population values.
#'    * The population scenario must have five columns with names `"year"`, `"region"`, `"state"`, `"postal"`, and `"state_pop"` containing the year, the NCA region name, the state name, the postal code abbreviation (e.g., "ME" for "Maine") for the state, and the state population, respectively.
#'    * `popInput` only accepts a a single scenario, in contrast to `driverInput`. In other words, [FrEDI::run_fredi_sv()] uses the same population scenario for any and all driver scenarios passed to `driverInput`.
#'    * If the user does not specify an input scenario for population (i.e., `popInput = NULL`, [FrEDI::run_fredi_sv()] uses a default population scenario.
#'    * Population inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (2100).
#'    * Population values must be greater than or equal to zero.
#'    The default regional population scenario is drawn from the Integrated Climate and Land Use Scenarios version 2 (ICLUSv2) model (Bierwagen et al, 2010; EPA 2017) under the Median variant projection of United Nations (United Nations, 2015). Note that the FrEDI SV default population scenario differs from the default population scenario used by [FrEDI::run_fredi()].
#'
#' The output of [FrEDI::run_fredi_sv()] is an R data frame object containing average annual physical impacts for socially vulnerable groups, at the NCA region level and five-year increments.
#'
#'
#'
#' @return
#' The output of [FrEDI::run_fredi_sv()] is an R data frame object containing average annual physical impacts for socially vulnerable groups, at the NCA region level and five-year increments.
#'
#' @examples
#' ### Run SV Module with defaults without specifying sector
#' df_sv <- run_fredi_sv()
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
#' load(gcamScenarios)
#'
#' ### Load population scenario
#' load(popScenario)
#'
#' ### Run SV Module for "Extreme Temperature" with custom population and temperature scenarios
#' df_sv <- run_fredi_sv(sector = "Extreme Temperature", driverInput = gcamScenarios, popInput = popScenario)
#'
#' @references
#' Bierwagen, B., D. M. Theobald, C. R. Pyke, A. Choate, P. Groth, J. V. Thomas, and P. Morefield. 2010. “National housing and impervious surface scenarios for integrated climate impact assessments.” Proc. Natl. Acad. Sci. 107 (49): 20887–20892. https://doi.org/10.1073/pnas.1002096107.
#'
#' EPA. 2017. Multi-Model Framework for Quantitative Sectoral Impacts Analysis: A technical report for the Fourth National Climate Assessment. U.S. Environmental Protection Agency, EPA 430-R-17-001.
#'
#' EPA. 2021. Technical Documentation on the Framework for Evaluating Damages and Impacts (FrEDI). U.S. Environmental Protection Agency, EPA 430-R-21-004. Available at <https://epa.gov/cira/FrEDI/>.
#'
#' EPA. 2021. Climate Change and Social Vulnerability in the United States: A Focus on Six Impacts. U.S. Environmental Protection Agency, EPA 430-R-21-003. Available at <https://www.epa.gov/cira/social-vulnerability-report/>.
#'
#' United Nations. 2015. World population prospects: The 2015 revision. New York: United Nations, Department of Economic and Social Affairs, Population Division.
#'
#' U.S. Global Change Research Program. 2015. Scenarios for the National Climate Assessment. Available at <https://scenarios.globalchange.gov/regions_nca4>.
#'
#'
#' @export
#' @md
#'
###### run_fredi_sv ######
### This function creates a data frame of annual average impacts over the years 2010-2100, from default values or scenario inputs, for a subset of FrEDI sectors as a function of SV group, sector, and region.
### run_fredi_sv relies on the following helper functions: "interpolate_annual", "match_scalarValues","get_econAdjValues" , "calcScalars", "interpolate_tempBin"
run_fredi_sv <- function(
    sector      = NULL, ### Vector of sectors to get results for
    driverInput = NULL,
    popInput    = NULL,
    silent      = TRUE,  ### Whether to message the user
    .testing    = FALSE
){
  ###### Set up the environment ######
  pkgPath     <- NULL
  pkgPath     <- (pkgPath |> is.null()) |> ifelse(system.file(package="FrEDI"), pkgPath);
  rDataType   <- "rds"
  impactsPath <- pkgPath |> file.path("extdata", "sv", "impactLists")

  ### Get FrEDI data objects
  fredi_config  <- "fredi_config"  |> get_frediDataObj("frediData")
  co_modelTypes <- "co_modelTypes" |> get_frediDataObj("frediData")
  co_states     <- "co_states"     |> get_frediDataObj("frediData")
  temp_default  <- "temp_default"  |> get_frediDataObj("frediData")
  pop_default   <- "pop_default"   |> get_frediDataObj("stateData")

  ### Assign config files
  fredi_config |> list2env(envir = environment())

  ### Group types
  c_svGroupTypes <- svDataList$c_svGroupTypes
  minYear  <- 2010
  maxYear  <- 2100
  yearsBy5 <- minYear |> seq(maxYear, by=5)

  ### Testing
  save    <- .testing |> ifelse(FALSE, save)

  ### Level of messaging (default is to message the user)
  silent  <- (silent |> is.null()) |> ifelse(T, silent)
  msgUser <- !silent
  msg0    <- ""
  msg1    <- msg0 |> paste0("\t")
  msg2    <- msg1 |> paste0("\t")
  msg3    <- msg2 |> paste0("\t")

  ###### By State ######
  byState    <- TRUE
  popCol0    <- "state_pop"
  stateCols0 <- c("state", "postal")


  ###### Sector Info ######
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
  df_validGroups  <- c_popWtCol |> get_validGroups()

  ###### Check Driver Inputs ######
  ### Initialize whether to check for inputs
  check_slrInput  <- c_modelType == "slr"
  # check_tempInput <- TRUE
  check_tempInput <- c_modelType == "gcm"
  check_popInput  <- !(popInput |> is.null())
  ### Initialize whether inputs exist
  has_driverInput <- !(driverInput |> is.null())
  has_slrInput    <- FALSE
  has_tempInput   <- FALSE
  has_popInput    <- FALSE
  ### Scenario columns
  tempCols        <- c("scenario", "year", "temp_C")
  slrCols         <- c("scenario", "year", "slr_cm")
  popCols         <- c("region"  , "state", "postal", "year") |> c(popCol0)
  ### Scenario ranges
  driverMax       <- co_modelTypes |> filter(modelType_id == c_modelType) |> pull(modelMaxOutput)
  driverRange     <- 0 |> c(driverMax)


  ### Check inputs
  if(has_driverInput){
    msg1 |> message("Checking `driverInput` values...")
    ### Check that the input is a data frame
    class_driverInput <- driverInput |> class()
    if(!("data.frame" %in% class_driverInput)){
      msg2 |> message("Error: `driverInput` must have `class='data.frame'`!", "\n")
      msg2 |> message("Exiting...")
      return()
    } ### End if(!("data.frame" %in% class_driverInput))

    ### Info about driverInputs
    driverInputCols <- driverInput |> names()
    has_scenarioCol <- "scenario" %in% driverInputCols
    has_yearCol     <- "year"     %in% driverInputCols
    # driverInputCols |> print(); # has_scenarioCol |> print(); # has_yearCol |> print()

    ### Check scenarios
    if(has_scenarioCol){
      msg1 |> message("Checking scenarios in `driverInput`...")
      ### If scenarios are present, check the number of scenarios
      c_scenarios <- driverInput$scenario |> unique()
      n_scenarios <- c_scenarios |> length()
      if(n_scenarios > 4){
        msg2 |> message("Warning: `driverInput` has more than four distinct scenarios!", "")
        msg3 |> message("Only the first four scenarios will be used...", "\n")
        c_scenarios <- c_scenarios[1:4]; n_scenarios <- c_scenarios |> length()
        driverInput <- driverInput |> filter(scenario %in% c_scenarios)
      } ### End if(n_scenarios > 4)
    } else{ ### End if(has_scenarioCol)
      msg2 |> message("Error: `driverInput` must have column='scenario' present`!", "\n")
      msg2 |> message("Exiting...")
      return()
    } ### End else(has_scenarioCol)
    rm(has_scenarioCol)

    ### Check input years
    if(!has_yearCol){
      msg2 |> message("Error: `driverInput` must have column='year' present`!", "\n")
      msg2 |> message("Exiting...")
      return()
    } ### if(!has_yearCol)
    rm(has_yearCol)

    ### Check for SLR inputs
    if(has_driverInput & check_slrInput){
      msg1 |> message("Checking `driverInput` values for SLR scenario...")
      ### Check for SLR columns
      slrCols_inInput <- slrCols %in% driverInputCols
      if(slrCols_inInput |> all()){
        msg2 |> message("All SLR scenario columns present in `driverInput`...")
        ### Filter to non-missing data
        driverInput   <- driverInput |> filter_all(all_vars(!(. |> is.na())))
        ### Check non-missing values
        if(driverInput |> nrow()){
          df_nonNA    <- driverInput |>
            group_by_at(c("scenario")) |>
            summarize(n=n(), .groups="drop")
          naIssues    <- !all(df_nonNA$n >= 2)
          rm(df_nonNA)
        } else{ ### End if(driverInput |> nrow())
          naIssues    <- TRUE
        } ### End else(driverInput |> nrow())

        ### If missing values are an issue:
        if(naIssues){
          msg2 |> message("Error: each scenario must have at least two non-missing values for \'slr_cm\'!", "\n")
          msg2 |> message("Exiting...")
          return()
        } ### End if(naIssues)
        has_slrInput    <- TRUE
        has_tempInput   <- FALSE
        check_tempInput <- FALSE
        rm(naIssues)
      } ### End if(all(slrCols_inInput))
      ### Message user about missing columns
      else{
        msg1 |> message("Warning: `driverInput` is missing the following SLR scenario input columns:")
        msg2 |> message("\'", paste(slrCols[!slrCols_inInput], collapse="\', \'"),"'...", "\n")
        msg1 |> message("Looking for temperature scenario instead", "...", "\n")
        has_slrInput    <- FALSE
        has_tempInput   <- FALSE
        check_tempInput <- TRUE
      } ### End else(all(slrCols_inInput))
      rm(slrCols_inInput)
    } ### End if(has_driverInput & check_slrInput)

    ### Otherwise, check temperature inputs
    if(has_driverInput & check_tempInput){
      check_slrInput |> ifelse(msg2, msg1) |> message("Checking `driverInput` values for temperature scenario...")
      ### Check for temperature columns
      tempCols_inInput <- (tempCols %in% driverInputCols)
      if(tempCols_inInput |> all()){
        check_slrInput |> ifelse(msg3, msg2) |> message("All temperature scenario columns present...")
        ### Filter to non-missing data
        driverInput <- driverInput |> filter_all(all_vars(!(. |> is.na())))
        ### Check non-missing values
        if(driverInput |> nrow()){
          df_nonNA <- driverInput |> filter_all(all_vars(!(. |> is.na())))
          df_nonNA <- df_nonNA |>
            group_by_at(c("scenario")) |>
            summarize(n=n(), .groups="drop")
          naIssues <- !all(df_nonNA$n >= 2)
          rm(df_nonNA)
        } ### End if(driverInput |> nrow())
        else{
          naIssues <- TRUE
        } ### End else(driverInput |> nrow())

        ### If naIssues
        if(naIssues){
          msg2 |> message("Error: each scenario must have at least two non-missing values for \'temp_C\'!", "\n")
          msg2 |> message("Exiting...")
          return()
        }  ### End if(naIssues)
        has_tempInput   <- TRUE
        check_tempInput <- TRUE
        rm(naIssues)
      } ### End if(all(tempCols_inInput))
      else{
        msg2 |> message("Warning: `driverInput` is missing the following temperature scenario input columns...")
        msg2 |> message("\'", paste(tempCols[!tempCols_inInput], collapse="\', \'"),"'...", "\n")
        msg2 |> message("Exiting...")
        return()
      } ### End else(all(tempCols_inInput))
      rm(tempCols_inInput)
    } ### if(has_driverInput & check_tempInput)
    else{
      has_driverInput <- FALSE
    } ### end else(has_driverInput & check_tempInput)
    rm(class_driverInput, driverInputCols)
  } ### End if(has_driverInput)


  ###### Check Population Inputs ######
  ### Check that the input is a data frame
  if(check_popInput){
    msg1 |> message("Checking `popInput` values...")
    class_popInput <- popInput |> class()
    if(!("data.frame" %in% class_popInput)){
      msg2 |> message("Error: `popInput` must have `class='data.frame'`!", "\n")
      msg2 |> message("Exiting...")
      return()
    } ### End if(!("data.frame" %in% class_popInput))

    ### Check for popInput columns
    ### Info about popInputs
    popInputCols    <-  popInput |> names()
    popCols_inInput <- (popCols %in% popInputCols)
    allPopCols      <- popCols_inInput |> all()
    if(!allPopCols) {
      ### Exit and message the user
      # msg1 |> message("Using default regional population scenario", "...", "\n")
      # has_popInput    <- FALSE
      msg2 |> message("Error: `popInput` is missing the following input columns:")
      msg3 |> message("'", paste(popCols[!popCols_inInput], collapse="', '"),"'...", "\n")
      msg2 |> message("Exiting...")
      return()
    } ### End if(!allPopCols)

    ### Otherwise message user
    msg2 |> message("All population scenario columns present in `popInput`...")
    ### Filter to non-missing data
    popInput     <- popInput |> filter_all(all_vars(!(. |> is.na())))
    popInput     <- popInput |> unique()

    ### Check input Population values: no repeating rows
    if(popInput |> nrow()){
      df_dups       <- popInput |>
        group_by_at(c("year", "region", "state")) |>
        summarize(n=n(), .groups="drop")
      checkIssues  <- (df_dups$n > 1) |> any()
      rm(df_dups)
    } else{ ### if(popInput |> nrow())
      checkIssues <- FALSE
    } ### End else(popInput |> nrow())

    ### If there are issues with years:
    if(checkIssues){
      msg2 |> message("Error: duplicate rows present in `popInput`!")
      msg2 |> message("Exiting...")
      return()
    } ### End if(checkIssues)
    rm(checkIssues)

    ### Check input Population values: population >= 0
    checkIssues <- (popInput |> pull(state_pop)) < 0
    anyIssues   <- checkIssues |> any()
    if(anyIssues){
      msg2 |> message("Error: Values for 'reg_pop' in `popInput` must be greater than zero!")
      msg2 |> message("Exiting...")
      return()
    } ### End if(checkIssues)
    has_popInput <- TRUE
    rm(checkIssues, anyIssues)
    rm(class_popInput, popInputCols, popCols_inInput)
  } ### End if(check_popInput)



  ###### Driver Scenario ######
  paste0("\n", msg1) |> message("Preparing driver scenario...")

  ###### ** Temperature Scenario ######
  ### User inputs: temperatures have already been converted to CONUS temperatures. Filter to desired range.
  ### Add the point where impacts are zero (the reference year temperature)
  ### For user inputs:
  ### - Select appropriate columns
  ### - Remove missing values of years, temperatures
  ### - Filter to appropriate years
  checkTemp0   <- c_modelType == "gcm" & has_tempInput
  checkTemp1   <- c_modelType == "slr" & has_tempInput & !has_slrInput
  checkTemp2   <- c_modelType == "gcm" & !has_tempInput
  checkTemp3   <- c_modelType == "slr" & !has_tempInput & !has_slrInput
  if(checkTemp0 | checkTemp1){
    ### Message user
    # if(checkTemp1){msg1 |> message("No SLR inputs provided...")}
    msg2 |> message("Using temperature scenario from user inputs...")
    ### Format inputs
    driverInput    <- driverInput |> select(all_of(tempCols)) |> rename(driverValue = temp_C)
  } ### End if(checkTemp0 | checkTemp1)
  else if(checkTemp2 | checkTemp3){
    ### Otherwise use default scenario and add scenario column
    msg2 |> message("Using default temperature scenario...")
    driverInput <- temp_default
    driverInput <- driverInput |> mutate(temp_C = temp_C_conus)
    driverInput <- driverInput |> mutate(scenario = "FrEDI Default")
    ### Select columns
    driverInput <- driverInput |> select(all_of(tempCols)) |> rename(driverValue = temp_C)
  } ### End else if(checkTemp2 | checkTemp3)

  ### Interpolate temperatures over scenarios:
  if(checkTemp0 | checkTemp1 | checkTemp2 | checkTemp3){
    ### Scenarios
    c_scenarios <- driverInput |> pull(scenario) |> unique()
    n_scenarios <- c_scenarios |> length()
    ### Ref year
    refYearTemp <- co_modelTypes |> filter(modelUnitType=="temperature") |> pull(modelRefYear)
    ### Drivers
    drivers_df  <- c_scenarios |> map(function(
    scenario_i,
    data_x     = driverInput,
    refYear_x  = refYearTemp,
    refValue_x = 0,
    maxYear_x  = maxYear
    ){
      ### - Filter to scenario i and drop scenario column
      ### - Zero out series at the temperature reference year
      # tempInput |> names() |> print()
      input_i <- data_x  |> filter(scenario==scenario_i) |> select(-c("scenario"))
      input_i <- input_i |> filter(year > refYear_x) |> filter(year <= maxYear_x)
      input_i <- tibble(year= refYear_x, driverValue = refValue_x) |> rbind(input_i)

      ### Then, interpolate
      ### - Use minimum series year to determine interpolation years
      ### - Add a dummy region for National Total for interpolate_annual
      ### - Interpolate, drop dummy region, and add scenario back in
      years_i <- refYear_x:maxYear_x
      input_i <- input_i |> mutate(region="National Total")
      input_i <- input_i |> interpolate_annual(years=years_i, column="driverValue", rule=1:1)
      input_i <- input_i |> select(-c("region"))
      input_i <- input_i |> mutate(scenario = scenario_i)
      ### Return
      return(input_i)
    }) |> bind_rows()
    ### Add driver unit
    drivers_df <- drivers_df |> mutate(driverUnit = "degrees Celsius")
    ### Remove values
    rm(driverInput, refYearTemp)
  } ### End if(checkTemp0 | checkTemp1 | checkTemp2 | checkTemp3)
  ### Remove intermediate objects
  rm(checkTemp0, checkTemp1, checkTemp2)



  ###### ** SLR Scenario ######
  ### Year where SLR impacts are zero
  ### Follow similar procedure to temperatures:
  ### - Select appropriate columns
  ### - Remove missing values of years, slr
  ### - Filter to appropriate years
  checkSLR0   <- c_modelType == "slr" &  has_slrInput
  checkSLR1   <- c_modelType == "slr" & !has_slrInput

  ### If there is no SLR scenario, calculate from temperatures
  ### First convert temperatures to global temperatures
  ### Then convert global temps to SLR
  if(checkSLR0){
    msg2 |> message("Using SLR scenario from user inputs...")
    driverInput <- driverInput |> select(all_of(slrCols))
    ### Scenarios
    c_scenarios <- driverInput |> pull(scenario) |> unique()
    n_scenarios <- c_scenarios |> length()
    ### Ref year
    refYearSLR  <- co_modelTypes |> filter(modelUnitType=="slr") |> pull(modelRefYear)
    ### Drivers
    drivers_df  <- c_scenarios |> map(function(
    scenario_i,
    data_x     = driverInput,
    refYear_x  = refYearSLR,
    refValue_x = 0,
    maxYear_x  = maxYear
    ){
      ### - Filter to scenario i and drop scenario column
      ### - Zero out series at the temperature reference year
      # tempInput |> names() |> print()
      input_i <- data_x  |> filter(scenario==scenario_i) |> select(-c("scenario"))
      input_i <- input_i |> filter(year > refYear_x) |> filter(year <= maxYear_x) |> rename(driverValue = slr_cm)
      input_i <- tibble(year= refYear_x, driverValue = refValue_x) |> rbind(input_i)

      ### Then, interpolate
      ### - Use minimum series year to determine interpolation years
      ### - Add a dummy region for National Total for interpolate_annual
      ### - Interpolate, drop dummy region, and add scenario back in
      years_i <- refYear_x:maxYear_x
      input_i <- input_i |> mutate(region="National Total")
      input_i <- input_i |> interpolate_annual(years=years_i, column="driverValue", rule=1:1)
      input_i <- input_i |> select(-c("region"))
      input_i <- input_i |> mutate(scenario = scenario_i)
      ### Return
      return(input_i)
    }) |> bind_rows()
    ### Add driver unit
    drivers_df <- drivers_df |> mutate(driverUnit  = "cm")
    ### Remove values
    rm(driverInput, refYearSLR)
  } else if(checkSLR1){
    msg2 |> message("Creating SLR scenario from temperature scenario...")
    drivers_df <- c_scenarios |> map(function(
    scenario_i,
    data_x = drivers_df
    ){
      data_i <- data_x |> filter(scenario==scenario_i)
      data_i <- data_i |> mutate(temp_C = driverValue |> convertTemps(from="conus"))
      data_i <- temps2slr(temps = data_i$temp_C, years = data_i$year)
      data_i <- data_i |> rename(driverValue=slr_cm)
      data_i <- data_i |> mutate(scenario=scenario_i)
      return(data_i)
    }) |> bind_rows()
    ### Add driver unit
    drivers_df <- drivers_df |> mutate(driverUnit = "cm")
  } ### End else if(checkSLR0)
  # drivers_df |> names() |> print()
  ### Remove intermediate objects
  rm(checkSLR0, checkSLR1)

  ### Standardize years
  drivers_df <- drivers_df |> filter(year >= minYear) |> filter(year <= maxYear)
  drivers_df <- drivers_df |> filter_all(all_vars(!(. |> is.na())))

  ### Check if there are any years after the max year
  msgInputs1 <- " scenario must have at least one non-missing value in or after the year " |> paste0(maxYear, "!")
  msgInputs2 <- "\n" |> paste0("Exiting...")
  msg_driver <- "Driver" |> paste0(msgInputs1)
  checkDrive <- drivers_df |> filter(year == maxYear) |> nrow()
  if(!checkDrive) {
    "\n" |> paste0(msg1, "Warning! ", msg_driver) |> message()
    msgInputs2 |> message()
    return()
  } ### if(!check_temp)
  rm(msg_driver, checkDrive)


  ###### ** Standardize Driver Scenarios ######
  ### Subset to desired years
  drivers_df <- drivers_df |> filter(year %in% yearsBy5)

  ###### Population Scenario ######
  paste0("\n", msg1) |> message("Preparing population scenario...")

  ###### ** Region Population Scenario ######
  ### Population inputs
  if(has_popInput) {
    msg2 |> message("Creating population scenario from user inputs...")
    ### Join with region info
    drop0     <- c("fips")
    join0     <- popInput  |> names() |> (function(y, z=co_states |> names()){y[y %in% z]})()
    popInput  <- co_states |> left_join(popInput, by=c(join0), relationship="many-to-many")
    popInput  <- popInput  |> filter_all(all_vars(!(. |> is.na())))
    popInput  <- popInput  |> select(all_of(popCols))
    rm(join0, drop0)

    ### Mutate region
    pop_df    <- popInput |> mutate(region = gsub(" ", ".", region))
    rm(popInput)

    ### Interpolate annual
    pop_df    <- pop_df   |> interpolate_annual(years=yearsBy5, column=popCol0, rule=1:1, byState=byState) |> ungroup()
  } else {
    # msg1 |> message("No population scenario provided...")
    msg2 |> message("Using default population scenario...")
    pop_df <- pop_default
  } ### End if(has_popInput)


  ### Standardize population data
  # c(minYear, maxYear) |> print(); yearsBy5 |> range() |> print()
  pop_df <- pop_df |> mutate(region = gsub("\\.", " ", region))

  ### Check if there are any years after the max year
  pop_df <- pop_df |> filter(year >= minYear) |> filter(year <= maxYear)
  pop_df <- pop_df |> filter_all(all_vars(!(. |> is.na())))
  msg_pop    <- "Population" |> paste0(msgInputs1)
  check_pop  <- pop_df |> filter(year == maxYear) |> nrow()
  if(!check_pop) {
    "\n" |> paste0(msg1, "Warning! ", msg_pop) |> message()
    msgInputs2 |> message()
    return()
  } ### if(!check_pop)
  rm(msg_pop, check_pop)


  ###### ** County Population Scenario ######
  message(msg2, "Calculating county population from state population...")
  df_popProj <- pop_df |> get_countyPop(
    years   = yearsBy5,
    xCol0   = "year",     ### X column in df0
    yCol0   = "state_pop" ### Y column in df0
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

  ###### Return Object ######
  msg1 |> paste0("Finished.") |> message()
  # if(.testing) {listResults <- list(results = listResults, county_pop = df_popProj)}
  # else         {listResults <- listResults}
  return(listResults)
}








