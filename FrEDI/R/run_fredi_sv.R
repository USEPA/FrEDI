###### Documentation ######
#' Calculate climate change impacts on socially vulnerable (SV) populations throughout the 21st century for available sectors
#'
#' @description
#' `run_fredi_sv` allows users to project annual average climate change impacts throughout the 21st century (2010-2090) for socially vulnerable (SV) populations for available sectors. Users can run [FrEDI::run_fredi_sv()] for individual sectors to generate annual physical impacts for SV populations. [FrEDI::run_fredi_sv()] can be run with default population and climate (temperature and sea level rise trajectories) or provide custom trajectories. The output of [FrEDI::run_fredi_sv()] is an R data frame object containing annual average physical impacts at five-year increments for the period 2010 to 2090. Users have the option to write outputs to Excel files that provide additional visualization of SV outputs.
#'
#' @param sector A character string indicating the sector for which the FrEDI SV module should calculate impacts (see [FrEDI::get_sv_sectorInfo()] for a list of available sectors).
#'
#' @param driverInput A data frame of up to four custom scenarios for drivers (temperature or global mean sea level rise). `driverInput` requires a data frame with columns of `"year"` and `"scenario"`. The data frame must also include a third column: `"temp_C"` for temperature-driven sectors (containing temperature values in degrees Celsius of warming for the contiguous U.S.) or `"slr_cm"` for sea level rise (SLR)-driven sectors (containing values for global mean sea level rise in centimeters). Run `get_sv_sectorInfo(gcmOnly=TRUE)` to see temperature-driven sectors in the SV module and `get_sv_sectorInfo(slrOnly=TRUE)` to see SLR-driven scenarios. Users can also pass a data frame with all four columns (`"year"`, `"scenario"`, `"temp_C"`, and `"slr_cm"`), in which case [FrEDI::run_fredi_sv()] determines whether to use the `"temp_C"` or `"slr_cm"` column as the driver trajectory based on the specified sector. Driver inputs for all scenarios should start in the year 2000 or earlier. All scenarios must include at least two non-missing values  (especially values before or at 2000 and at or after 2090). If any required columns are missing, [FrEDI::run_fredi_sv()] will use the default temperature or sea level rise scenario from [FrEDI::run_fredi()]. If the data frame passed to `driverInput` has more than four unique scenarios, [FrEDI::run_fredi_sv()] will only run the first four scenarios.
#'
#' @param popInput A data frame containing regional population trajectories for each of the seven regions for the contiguous U.S. (`"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, `"Southwest"`) as defined by the [National Climate Assessment (NCA)](https://scenarios.globalchange.gov/regions_nca4). The data frame passed to `popInput` should have columns `"year"`, `"region"`, and `"reg_pop"`, which respectively contain values for year, NCA region name, and regional population. `popInput` only accepts a data frame with a single scenario; [FrEDI::run_fredi_sv()] uses the same population scenario for any and all driver scenarios in the data frame passed to `driverInput`. If `popInput=NULL` (default), [FrEDI::run_fredi_sv()] will use the default regional population trajectories.
#' @param silent A logical (`TRUE/FALSE`) value indicating the level of messaging desired by the user (defaults to `silent=TRUE`).
# @param return=TRUE A `TRUE/FALSE` value indicating whether to return the results as a data frame (default=`TRUE`).

#' @param save A logical (`TRUE/FALSE`) value indicating whether to save the results to an Excel file (defaults to `save=FALSE`).
#'
#' @param outpath A character string indicating a file directory to save the Excel file. Defaults to the working directory, i.e. `outpath=getwd()`. If the directory specified by `outpath` does not exist, [FrEDI::run_fredi_sv()] will attempt to create the specified directory.
#'
#' @param overwrite A logical (`TRUE/FALSE`) value indicating whether to overwrite an existing Excel file if `save=TRUE` (defaults to `overwrite=FALSE`). If `overwrite=FALSE`, [FrEDI::run_fredi_sv()] will not automatically overwrite an existing Excel file; however, if a file exists and `overwrite=FALSE`, [FrEDI::run_fredi_sv()] will message the user and the user will have the option to overwrite the existing file. If `overwrite=TRUE` and the Excel file exists in the output directory, [FrEDI::run_fredi_sv()] will overwrite the existing file without messaging the user.
#'
#' @param addDate A logical (`TRUE/FALSE`) value indicating whether to add the date to the name of the output Excel file if `save=TRUE` (defaults to `addDate=FALSE`). If `save=TRUE` and `addDate=TRUE`, [FrEDI::run_fredi_sv()] will append the system date to the beginning of the name of the outputs Excel file using the format `"%Y%m%d"` (see [base::format()] and [base::Sys.Date()] for additional information).
# @param libPath=.libPaths()[1] Path to R library containing the FrEDI package files. Defaults to the first path in `.libPaths()`.
#'
#'
#' @details [FrEDI::run_fredi_sv()] projects annual climate change impacts for socially vulnerable (SV) populations throughout the 21st century (2010-2090) for available sectors, using default or user-specified population, temperature, and sea level rise (SLR) trajectories. [FrEDI::run_fredi_sv()] is the main function for the FrEDI Social Vulnerability (SV) module in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information). The SV module extends the [FrEDI] framework to socially vulnerable populations using data underlying a 2021 U.S. Environmental Protection Agency (EPA) report on [Climate Change and Social Vulnerability in the United States](https://www.epa.gov/cira/social-vulnerability-report/).
#'
#' Users can run [FrEDI::run_fredi_sv()] to generate annual physical impacts for SV groups for individual sectors. When running [FrEDI::run_fredi_sv()], users must specify one of the sectors in the SV module; use [FrEDI::get_sv_sectorInfo()] for a list of available sectors.
#'
#' [FrEDI::run_fredi_sv()] can be run with default population and climate (temperature and SLR) trajectories or use [FrEDI::run_fredi_sv()] to run custom scenarios. Running [FrEDI::run_fredi_sv()] with custom climate scenarios requires passing a data frame of scenarios to the `driverInput` argument. [FrEDI::run_fredi_sv()] can also be run with a custom population scenario by passing a data frame of regional population trajectories to the `popInput` argument; unlike climate scenarios, [FrEDI::run_fredi_sv()] will only run a single scenario at a time.
#'
#' * `driverInput` can take a data frame containing up to four custom scenarios for drivers (temperature or global mean sea level rise). `driverInput` requires a data frame with columns of `"year"` and `"scenario"`. The data frame must also include a third column: `"temp_C"` for temperature-driven sectors (containing temperature values in degrees Celsius of warming for the contiguous U.S.) or `"slr_cm"` for sea level rise (SLR)-driven sectors (containing values for global mean sea level rise in centimeters). Run `get_sv_sectorInfo(gcmOnly=TRUE)` to see temperature-driven sectors in the SV module and `get_sv_sectorInfo(slrOnly=TRUE)` to see SLR-driven scenarios. Users can also pass a data frame with all four columns (`"year"`, `"scenario"`, `"temp_C"`, and `"slr_cm"`), in which case [FrEDI::run_fredi_sv()] determines whether to use the `"temp_C"` or `"slr_cm"` column as the driver trajectory based on the specified sector. If any required columns are missing, [FrEDI::run_fredi_sv()] will use the default temperature or sea level rise scenario from [FrEDI::run_fredi()]. If the data frame passed to `driverInput` has more than four unique scenarios, [FrEDI::run_fredi_sv()] will only run the first four scenarios.
#'     * Temperature inputs must be temperature change in degrees Celsius for the contiguous U.S. (use [FrEDI::convertTemps()] to convert global temperatures to CONUS temperatures before passing to `driverInput`) relative to a 1995 baseline (where 1995 is the central year of a 1986-2005 baseline period; values should start at zero in the year 1995).
#'     * Sea level rise inputs must be in centimeters relative to a 2000 baseline (i.e., values should start at zero in the year 2000). Driver inputs for all scenarios should start in the year 2000 or earlier. All scenarios must include at least two non-missing values  (especially values before or at 2000 and at or after 2090).
#' * `popInput` can take a data frame containing a single scenario with regional population trajectories for each of the seven regions for the contiguous U.S. (`"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, `"Southwest"`) as defined by the [National Climate Assessment (NCA)](https://scenarios.globalchange.gov/regions_nca4). The data frame passed to `popInput` should have columns `"year"`, `"region"`, and `"reg_pop"`, which respectively contain values for year, NCA region name, and regional population. `popInput` only accepts a data frame with a single scenario; [FrEDI::run_fredi_sv()] uses the same population scenario for any and all driver scenarios in the data frame passed to `driverInput`. If `popInput=NULL` (default), [FrEDI::run_fredi_sv()] will use the default regional population trajectories. The default regional population scenario is drawn from the Integrated Climate and Land Use Scenarios version 2 (ICLUSv2) model (Bierwagen et al, 2010; EPA 2017) under the Median variant projection of United Nations (United Nations, 2015). Note that the FrEDI SV default population scenario differs from the default population scenario used by [FrEDI::run_fredi()].
#'
#' The output of [FrEDI::run_fredi_sv()] is an R data frame object containing NCA region-specific annual average physical impacts for socially vulnerable groups at five-year increments between 2010 and 2090. Users have the additional option to write results to an Excel file by setting `save=TRUE`; output Excel files provide basic visualizations of output data. Additional arguments provide more control over how the outputs are saved if `save=TRUE`:
#'
#' * `outpath` can be used to specify the directory in which to save an Excel output file. Defaults to `outpath=getwd()` (i.e., the working directory). If the directory specified by `outpath` does not exist, [FrEDI::run_fredi_sv()] will attempt to create the specified directory.
#' * `overwrite` can be used to force [FrEDI::run_fredi_sv()] to overwrite an existing Excel file in the output directory.  If `overwrite=FALSE`, [FrEDI::run_fredi_sv()] will not automatically overwrite an existing Excel file; however, if a file exists and `overwrite=FALSE`, [FrEDI::run_fredi_sv()] will message the user and the user will have the option to overwrite the existing file. If `overwrite=TRUE` and the Excel file exists in the output directory, [FrEDI::run_fredi_sv()] will overwrite the existing file without messaging the user.
#' * `addDate` can be used to append the date to the output Excel file. If `save=TRUE` and `addDate=TRUE`, [FrEDI::run_fredi_sv()] will append the system date to the beginning of the name of the outputs Excel file using the format `"%Y%m%d"` (see [base::format()] and [base::Sys.Date()] for additional information).
#'
#'
#'
#' @return
#' The output of [FrEDI::run_fredi_sv()] is an R data frame object containing NCA region-specific annual average physical impacts for socially vulnerable groups at five-year increments between 2010 and 2090. An optional output of [FrEDI::run_fredi_sv()] is an Excel file containing the output data frame with basic visualizations of sector outputs.
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
#' ### Run SV Module with defaults for "Extreme Temperature" with saving and add date to file name
#' df_sv <- run_fredi_sv(sector="Extreme Temperature", save=T, addDate=T)
#'
#' ### Load temperature scenarios
#' load(gcamScenarios)
#'
#' ### Load population scenario
#' load(popScenario)
#'
#' ### Run SV Module for "Extreme Temperature" with custom population and temperature scenarios. Save and overwrite previous results
#' df_sv <- run_fredi_sv(sector="Extreme Temperature", driverInput = gcamScenarios, popInput = popScenario, save=T, addDate=T, overwrite = T)
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
### This function creates a data frame of annual average impacts over the years 2010-2090, from default values or scenario inputs, for a subset of FrEDI sectors as a function of SV group, sector, and region.
### run_fredi_sv relies on the following helper functions: "interpolate_annual", "match_scalarValues","get_econAdjValues" , "calcScalars", "interpolate_tempBin"
run_fredi_sv <- function(
    sector      = NULL, ### Vector of sectors to get results for
    driverInput = NULL,
    popInput    = NULL,
    silent      = TRUE,  ### Whether to message the user
    save        = FALSE,
    outpath     = getwd(),
    overwrite   = FALSE,
    addDate     = FALSE,
    .testing    = FALSE
    # addDate     = FALSE, ### Whether to add the date to the file name
    # libPath     = .libPaths()[1]
){
  ###### Set up the environment ######
  pkgPath     <- NULL
  pkgPath     <- ifelse(is.null(pkgPath), system.file(package="FrEDI"), pkgPath);
  rDataType   <- "rds"
  #pkgPath |> print()
  impactsPath <- pkgPath |> file.path("extdata", "sv", "impactLists")
  # impactsPath <- libPath |> file.path("FrEDI", "extdata", "sv", "impactLists")

  ### Assign previous configuration objects
  for(i in 1:length(fredi_config)) assign(names(fredi_config)[i], fredi_config[[i]])
  ### Group types
  c_svGroupTypes <- svDataList$c_svGroupTypes
  ### Update years,
  minYear <- 2010; maxYear <- 2090; list_years_by5 <- seq(minYear, maxYear, by=5)

  ### Testing
  save    <- ifelse(.testing, FALSE, save)

  ### Level of messaging (default is to message the user)
  silent  <- ifelse(is.null(silent), T, silent)
  msgUser <- !silent
  msg0    <- ""
  msg1    <- msg0 |> paste0("\t")
  msg2    <- msg1 |> paste0("\t")
  msg3    <- msg2 |> paste0("\t")

  ###### Sector Info ######
  ### Objects <-
  sectorInfo    <- svDataList$sectorInfo
  svSectorInfo  <- svDataList$svSectorInfo
  svDemoInfo    <- svDataList$svDemoInfo
  svValidTypes  <- svDataList$svValidTypes
  co_formatting <- svDataList$co_formatting
  co_formatting <- svDataList$co_formatting
  # co_modelTypes <- rDataList$co_modelTypes
  ### Sector names
  if(is.null(sector)){
    sector_msg1   <- paste0("Please select a sector: ") |> print()
    sector_msg2   <- 1:nrow(sectorInfo) |> lapply(function(i){
      sector_i    <- sectorInfo$sector[i]
      msg_i       <- paste0(i, ". ", sector_i)
      msg_i |> print()
      # return(msg_i)
    }) #|> unlist() |> paste(collapse="")
    # sector_msg3  <- sector_msg1 |> paste0(sector_msg2)
    sector_msg3   <- "Enter a number:"
    sector_input  <- readline(prompt = sector_msg3) |> as.numeric()
    sector        <- sectorInfo$sector[sector_input]
    rm("sector_msg1", "sector_msg2", "sector_msg3", "sector_input")
  }
  c_sector        <- sector
  paste0("Running FrEDI SV for sector '", c_sector, "':") |> message()
  ### Sector info
  which_sector    <- (svSectorInfo$sector == c_sector) |> which() #; which_sector |> print()
  df_sectorInfo   <- svSectorInfo[which_sector,]
  c_variants      <- df_sectorInfo[["variant_abbr" ]][which_sector]
  c_variantLabels <- df_sectorInfo[["variant_label"]][which_sector]
  rm("which_sector")
  ###### Invalid Sectors ######
  which_sector    <- (sectorInfo$sector == c_sector) |> which() #; which_sector |> print()
  c_popWtCol      <- sectorInfo[["popWeightCol"]][which_sector] |> tolower()
  c_modelType     <- sectorInfo[["modelType"   ]][which_sector] |> tolower()
  rm("which_sector")
  df_validGroups  <- svDemoInfo |> get_validGroups(df1 = svValidTypes, col0 = c_popWtCol)
  # return(df_validGroups)

  ###### Check Driver Inputs ######
  ### Initialize whether to check for inputs
  check_slrInput  <- ifelse(c_modelType=="slr", T, F)
  # check_tempInput <- TRUE
  check_tempInput <- ifelse(c_modelType=="gcm", T, F)
  check_popInput  <- ifelse(is.null(popInput), F, T)
  ### Initialize whether inputs exist
  has_driverInput <- ifelse(is.null(driverInput), F, T)
  has_slrInput    <- FALSE
  has_tempInput   <- FALSE
  has_popInput    <- FALSE
  ### Scenario columns
  tempCols        <- c("year", "temp_C", "scenario")
  slrCols         <- c("year", "slr_cm", "scenario")
  popCols         <- c("year", "reg_pop", "region")
  ### Scenario ranges
  tempRange       <- c(0, 6)
  slrRange        <- c(0, 200)
  driverRange     <- c(0) |> c(ifelse(c_modelType=="slr", slrRange[2], tempRange[2]))

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
    driverInputCols <- driverInput |> names(); # driverInputCols |> print()
    has_scenarioCol <- "scenario" %in% driverInputCols; #has_scenarioCol |> print()
    has_yearCol     <- "year"     %in% driverInputCols; #has_scenarioCol |> print()

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
    } ### End if(has_scenarioCol)
    else{
      msg2 |> message("Error: `driverInput` must have column='scenario' present`!", "\n")
      msg2 |> message("Exiting...")
      return()
    } ### End else(has_scenarioCol)
    rm("has_scenarioCol")

    ### Check input years
    if(!has_yearCol){
      msg2 |> message("Error: `driverInput` must have column='year' present`!", "\n")
      msg2 |> message("Exiting...")
      return()
    } ### if(!has_yearCol)
    rm("has_yearCol")

    ### Check for SLR inputs
    if(has_driverInput & check_slrInput){
      msg1 |> message("Checking `driverInput` values for SLR scenario...")
      ### Check for SLR columns
      slrCols_inInput <- slrCols %in% driverInputCols
      if(all(slrCols_inInput)){
        msg2 |> message("All SLR scenario columns present in `driverInput`...")
        ### Filter to non-missing data
        driverInput     <- driverInput |> filter(!is.na(year) & !is.na(slr_cm) & !is.na(scenario))
        ### Check non-missing values
        if(driverInput |> nrow()){
          df_nonNA      <- driverInput |>
            group_by_at(.vars=c("scenario")) |>
            summarize(n=n(), .groups="keep") |> ungroup()
          naIssues    <- !all(df_nonNA$n >= 2)
          rm("df_nonNA")
        } ### End if(driverInput |> nrow())
        else{
          naIssues <- TRUE
        } ### End else(driverInput |> nrow())

        ### If missing values are an issue:
        if(naIssues){
          msg2 |> message("Error: each scenario must have at least two non-missing values for \'slr_cm\'!", "\n")
          msg2 |> message("Exiting...")
          return()
        } ### End if(naIssues)
        has_slrInput <- TRUE ; has_tempInput <- FALSE; check_tempInput <- FALSE
        rm("naIssues")
      } ### End if(all(slrCols_inInput))
      ### Message user about missing columns
      else{
        msg1 |> message("Warning: `driverInput` is missing the following SLR scenario input columns:")
        msg2 |> message("\'", paste(slrCols[!slrCols_inInput], collapse="\', \'"),"'...", "\n")
        msg1 |> message("Looking for temperature scenario instead", "...", "\n")
        has_slrInput <- FALSE; has_tempInput <- FALSE; check_tempInput <- TRUE
      } ### End else(all(slrCols_inInput))
      rm("slrCols_inInput")
    } ### End if(has_driverInput & check_slrInput)

    ### Otherwise, check temperature inputs
    if(has_driverInput & check_tempInput){
      ifelse(check_slrInput, msg2, msg1) |> message("Checking `driverInput` values for temperature scenario...")
      ### Check for temperature columns
      tempCols_inInput <- (tempCols %in% driverInputCols)
      if(all(tempCols_inInput)){
        ifelse(check_slrInput, msg3, msg2) |> message("All temperature scenario columns present...")
        ### Filter to non-missing data
        driverInput     <- driverInput |> filter(!is.na(year) & !is.na(temp_C))
        ### Check non-missing values
        if(driverInput |> nrow()){
          df_nonNA       <- driverInput |>
            filter(!is.na(year) & !is.na(temp_C)) |>
            group_by_at(.vars=c("scenario")) |>
            summarize(n=n(), .groups="keep") |> ungroup()
          naIssues    <- !all(df_nonNA$n >= 2)
          rm("df_nonNA")
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
        has_tempInput <- TRUE; check_tempInput <- TRUE
        rm("naIssues")
      } ### End if(all(tempCols_inInput))
      else{
        msg2 |> message("Warning: `driverInput` is missing the following temperature scenario input columns...")
        msg2 |> message("\'", paste(tempCols[!tempCols_inInput], collapse="\', \'"),"'...", "\n")
        msg2 |> message("Exiting...")
        return()
      } ### End else(all(tempCols_inInput))
      rm("tempCols_inInput")
    } ### if(has_driverInput & check_tempInput)
    else{
      has_driverInput <- FALSE
    }
    rm("class_driverInput", "driverInputCols")
  } ### End if(has_driverInput)
  # ### Check input SLR heights
  # checkIssues <- (driverInput$slr_cm < slrRange[1]) | (driverInput$slr_cm > slrRange[2])
  # ### Check input temperatures
  # checkIssues <- (driverInput$temp_C < tempRange[1]) | (driverInput$temp_C > tempRange[2])

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
    if(all(popCols_inInput)){
      msg2 |> message("All population scenario columns present in `popInput`...")
      ### Filter to non-missing data
      popInput     <- popInput |> filter(!is.na(year) & !is.na(region) & !is.na(reg_pop))

      ### Check input Population values: no repeating years
      if(popInput |> nrow()){
        df_dups       <- popInput |>
          group_by_at(.vars=c("year", "region")) |>
          summarize(n=n(), .groups="keep") |> ungroup()
        checkIssues  <- any(df_dups$n > 1)
        rm("df_dups")
      } ### End if(popInput |> nrow())
      else{
        checkIssues <- TRUE
      }
      ### If there are issues with years:
      if(checkIssues){
        msg2 |> message("Error: duplicate years present in `popInput`!")
        msg2 |> message("Exiting...")
        return()
      } ### End if(checkIssues)
      rm("checkIssues")

      ### Check input Population values: population >= 0
      checkIssues <- (popInput$reg_pop < 0)
      anyIssues   <- checkIssues |> any()
      if(anyIssues){
        msg2 |> message("Error: Values for 'reg_pop' in `popInput` must be greater than zero!")
        msg2 |> message("Exiting...")
        return()
      } ### End if(checkIssues)
      has_popInput <- TRUE
      rm("checkIssues", "anyIssues")
    } ### End if(all(popCols_inInput))
    else{
      ### Exit and message the user
      # msg1 |> message("Using default regional population scenario", "...", "\n")
      # has_popInput    <- FALSE
      msg2 |> message("Error: `popInput` is missing the following input columns:")
      msg3 |> message("'", paste(popCols[!popCols_inInput], collapse="', '"),"'...", "\n")
      msg2 |> message("Exiting...")
      return()
    } ### End else(all(popCols_inInput))
    rm("class_popInput", "popInputCols", "popCols_inInput")
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
    driverInput    <- driverInput |> select(c(all_of(tempCols))) |> rename(driverValue = temp_C)
  } ### End if(checkTemp0 | checkTemp1)
  else if(checkTemp2 | checkTemp3){
    ### Otherwise use default scenario and add scenario column
    msg2 |> message("Using default temperature scenario...")
    # rDataList$co_defaultTemps |> names() |> print()
    driverInput <- rDataList$co_defaultTemps |>
      mutate(temp_C = temp_C_global |> convertTemps(from="global")) |>
      mutate(scenario="FrEDI Default")
    ### Select columns
    driverInput    <- driverInput |> select(c(all_of(tempCols))) |> rename(driverValue = temp_C)
  } ### End else if(checkTemp2 | checkTemp3)


  ### Interpolate temperatures over scenarios:
  if(checkTemp0 | checkTemp1 | checkTemp2 | checkTemp3){
    ### Scenarios
    c_scenarios <- driverInput$scenario |> unique()
    n_scenarios <- c_scenarios |> length()
    ### Ref year
    refYearTemp <- (rDataList$co_modelTypes |> filter(modelUnitType=="temperature"))$modelRefYear[1]
    ### Drivers
    drivers_df  <- c_scenarios |> lapply(function(
    scenario_i, data_x = driverInput,
    refYear_x = refYearTemp, refValue_x = 0,
    maxYear_x = maxYear
    ){
      ### - Filter to scenario i and drop scenario column
      ### - Zero out series at the temperature reference year
      # tempInput |> names() |> print()
      input_i <- data_x  |> filter(scenario==scenario_i) |> select(-c("scenario"))
      input_i <- input_i |> filter(year > refYear_x) |> filter(year <= maxYear_x)
      input_i <- data.frame(year= refYear_x, driverValue = refValue_x) |> rbind(input_i)

      ### Then, interpolate
      ### - Use minimum series year to determine interpolation years
      ### - Add a dummy region for National Total for interpolate_annual
      ### - Interpolate, drop dummy region, and add scenario back in
      years_i <- refYear_x:maxYear_x
      input_i <- input_i |>
        mutate(region="National Total") |>
        interpolate_annual(years = years_i, column = "driverValue", rule = 1:2) |>
        select(-c("region"))
      ### Add scenario
      input_i <- input_i |> mutate(scenario = scenario_i)
      ### Return
      return(input_i)
    }) |> (function(x){do.call(rbind, x)})()
    ### Add driver unit
    drivers_df <- drivers_df |> mutate(driverUnit  = "degrees Celsius")
    ### Remove values
    rm("driverInput", "refYearTemp")
  } ### End if(checkTemp0 | checkTemp1 | checkTemp2 | checkTemp3)
  ### Remove intermediate objects
  rm("checkTemp0", "checkTemp1", "checkTemp2")


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
    driverInput  <- driverInput |> select(c(all_of(slrCols)))
    ### Scenarios
    c_scenarios <- driverInput$scenario |> unique()
    n_scenarios <- c_scenarios |> length()
    ### Ref year
    refYearSLR  <- (rDataList$co_modelTypes |> filter(modelUnitType=="slr"))$modelRefYear[1]
    ### Drivers
    drivers_df  <- c_scenarios |> lapply(function(
    scenario_i, data_x = driverInput,
    refYear_x = refYearSLR, refValue_x = 0,
    maxYear_x = maxYear
    ){
      ### - Filter to scenario i and drop scenario column
      ### - Zero out series at the temperature reference year
      # tempInput |> names() |> print()
      input_i <- data_x  |> filter(scenario==scenario_i) |> select(-c("scenario"))
      input_i <- input_i |> filter(year > refYear_x) |> filter(year <= maxYear_x) |> rename(driverValue = slr_cm)
      input_i <- data.frame(year= refYear_x, driverValue = refValue_x) |> rbind(input_i)

      ### Then, interpolate
      ### - Use minimum series year to determine interpolation years
      ### - Add a dummy region for National Total for interpolate_annual
      ### - Interpolate, drop dummy region, and add scenario back in
      years_i <- refYear_x:maxYear_x
      input_i <- input_i |> mutate(region="National Total")
      input_i <- input_i |> interpolate_annual(years = years_i, column = "driverValue", rule = 1:2)
      input_i <- input_i |> select(-c("region"))
      ### Add scenario
      input_i <- input_i |> mutate(scenario = scenario_i)
      ### Return
      return(input_i)
    }) |> (function(x){do.call(rbind, x)})()
    ### Add driver unit
    drivers_df <- drivers_df |> mutate(driverUnit  = "cm")
    ### Remove values
    rm("driverInput", "refYearSLR")
  } ### End if(checkSLR0)
  else if(checkSLR1){
    msg2 |> message("Creating SLR scenario from temperature scenario...")
    drivers_df <- c_scenarios |> lapply(function(
    scenario_i, data_x = drivers_df
    ){
      data_i <- data_x |> filter(scenario==scenario_i)
      data_i <- data_i |> mutate(temp_C = driverValue |> convertTemps(from="conus"))
      data_i <- temps2slr(temps = data_i$temp_C, years = data_i$year)
      data_i <- data_i |> rename(driverValue=slr_cm)
      data_i <- data_i |> mutate(scenario=scenario_i)
      return(data_i)
    }) |> (function(scenarios_i){do.call(rbind, scenarios_i)})()
    ### Add driver unit
    drivers_df <- drivers_df |> mutate(driverUnit  = "cm")
  } ### End else if(checkSLR0)
  # drivers_df |> names() |> print()
  ### Remove intermediate objects
  rm("checkSLR0", "checkSLR1")

  ###### ** Standardize Driver Scenarios ######
  ### Subset to desired years
  drivers_df <- drivers_df |> filter(year %in% list_years_by5)

  ###### Population Scenario ######
  paste0("\n", msg1) |> message("Preparing population scenario...")

  ###### Region Population Scenario ######
  ### Population inputs
  if(has_popInput) {
    msg2 |> message("Creating population scenario from user inputs...")
    pop_df    <- popInput |> select(c(all_of(popCols)))
    pop_df    <- pop_df   |> interpolate_annual(years= list_years_by5, column = "reg_pop", rule = 2:2)
    pop_df    <- pop_df   |> rename(region_pop = reg_pop)
    rm("popInput")
  } ### End if(has_popInput)
  else              {
    # msg1 |> message("No population scenario provided...")
    msg2 |> message("Using default population scenario...")
    pop_df <- svPopList$iclus_region_pop
  } ### End else(has_popInput)
  ### Standardize population data
  pop_df <- pop_df |> filter(year >= minYear) |> filter(year <= maxYear)
  pop_df <- pop_df |> mutate(region = gsub("\\.", " ", region))

  ###### County Population Scenario ######
  msg2 |> message("Calculating county population from regional population...")
  df_popProj <- calc_countyPop(
    regPop  = pop_df,
    funList = svPopList$popProjList,
    years   = list_years_by5
  ) #rm("popProjList")


  ###### Calculate Impacts ######
  ### Iterate over adaptations/variants
  df_results  <- 1:nrow(df_sectorInfo) |> lapply(function(
    row_i, info_x = df_sectorInfo, scenarios_x = c_scenarios
  ){
    # scenarios_x |> print()
    ### Which SV data to use
    svName_i       <- ifelse(c_sector=="Coastal Properties", "svDataCoastal", "svData"); # svName_i |> print()
    # svDataList[[svName_i]] |> names() |> print(); # return()
    ### Sector info
    info_i         <- info_x[row_i,]
    sectorAbbr_i   <- info_i$impactList_fileExt[1]
    variantLabel_i <- info_i$variant_label[1]
    variantAbbr_i  <- info_i$variant_abbr[1]
    weightsCol_i   <- info_i$popWeightCol[1]
    # info_i |> print()

    ### Which impacts list to use
    impactsName_i  <- "impactsList" |>
      paste(sectorAbbr_i, sep="_") |>
      paste0(ifelse(is.na(variantAbbr_i), "", "_")) |>
      paste0(ifelse(is.na(variantAbbr_i), "", variantAbbr_i))
    impactsPath_i  <- impactsPath |> file.path(impactsName_i) |> paste0(".", rDataType)

    ###### Iterate Over Scenarios ######
    results_i <- scenarios_x |> lapply(function(scenario_j){
      paste0("\n", msg1) |> message("Calculating impacts for sector='", c_sector, "', variant='",
                                     variantLabel_i, "', scenario='", scenario_j, "'...")
      ###### Scaled Impacts ######
      drivers_j <- drivers_df |> filter(scenario == scenario_j) |> select(-c("scenario"))
      # drivers_j |> glimpse()
      ### Get impact list, calculate scaled impacts, remove impact list
      if(!exists("impactsList_j")){impactsList_j <- impactsPath_i |> readRDS()}
      # impactsList_j[[as.character(29031880500)]](1.667535543) |> print(); return()
      impacts_j <- calc_tractScaledImpacts(
        funList      = impactsList_j,
        driverValues = drivers_j,
        silent       = silent,
        .msg0        = msg2
      )
      if(exists("impactsList_j")){remove(list=c("impactsList_j"), inherits = T)}

      ###### Total Impacts ######
      ### Confirm year is numeric and filter out missing impacts
      impacts_j <- impacts_j |> mutate(year = year |> as.character() |> as.numeric())

      ### Calculate impacts by tract
      impacts_j <- impacts_j |> calc_tractImpacts(
        sector    = c_sector,
        popData   = df_popProj,
        svInfo    = svDataList[[svName_i]],
        svGroups  = c_svGroupTypes,
        weightCol = weightsCol_i,
        years     = list_years_by5,
        silent    = silent,
        .msg0     = msg2,
        .testing  = .testing
      )
      impacts_j <- impacts_j |> mutate(scenario = scenario_j)

      # (impacts_j$impPop_ref != 0) |> which() |> length() |> print()
      ###### Return Impacts ######
      return(impacts_j)
    })
    ###### Bind Results ######
    ### Bind results and add variant level
    results_i <- results_i|> (function(y){do.call(rbind, y)})()
    results_i <- results_i |> mutate(variant = variantLabel_i)

    ###### Adjust SV Group Values ######
    if(!.testing){
      valSuff0  <- c("ref", "sv")
      ### Join and adjust results valueAdj
      valCols0  <- c("impPop", "impact", "national_highRiskPop", "regional_highRiskPop", "aveRate")
      valCols1  <- valCols0  |> lapply(function(col_j){col_j |> paste(valSuff0, sep="_")}) |> unlist()
      drop0     <- c("validGroups", "weightCol", "validType", "valueAdj")
      ### Adjust results
      # df_validGroups |> glimpse(); results_i |> glimpse()
      results_i <- results_i |> left_join(df_validGroups, by = c("svGroupType"))
      results_i <- results_i |> mutate_at(.vars=c(all_of(valCols1)), function(col_j){col_j * results_i$valueAdj})
      results_i <- results_i |> select(-c(all_of(drop0))); rm("drop0")
      # (results_i$impPop_ref != 0) |> which() |> length() |> print()
      # results_i |> names() |> print()
      rm("valCols1")
      ###### Replace Driver Values ######
      valCols0  <- valCols0[!(valCols0 %in% c("impPop"))]
      valCols1  <- valCols0  |> lapply(function(col_j){col_j |> paste(valSuff0, sep="_")}) |> unlist()
      # valCols1 |> print()
      # driverRange |> print(); results_i$driverValue |> range() |> print()
      which0_i  <- (results_i$driverValue < driverRange[1]) | (results_i$driverValue > driverRange[2])
      # results_i |> glimpse()
      results_i[which0_i, valCols1] <- NA
      rm("valCols0", "valSuff0")
    }
    ### Return
    return(results_i)
  })
  ###### Format Results ######
  ### Bind results and ungroup
  df_results <- df_results  (function(x){do.call(rbind, x)})()
  df_results <- df_results |> ungroup() |> as.data.frame()

  ###### Save Results ######
  if(save){
    msg1 |> paste0("Saving results to Excel...") |> message()
    ###### File Info ######
    ###### Template Info
    inFilePath      <- system.file(package="FrEDI") |> file.path("extdata", "sv")
    inFileName      <- "FrEDI SV Graphics Template.xlsx"
    ###### Workbook Info
    excel_wb_path   <- inFilePath   |> file.path(inFileName)
    excel_wb_exists <- excel_wb_path |> file.exists(); #excel_wb_exists |> print()
    excel_wb_sheets <- c("FrEDI Outputs 1", "FrEDI Outputs 2")
    ###### Outfile Info and add date if specified
    outFileBase     <- "FrEDI" |> paste("SV", "Outputs", c_sector, sep="_") #; outFileBase |> print()
    outFileName     <- outFileBase |> paste0(".xlsx"); #outFileName |> print()
    if(addDate){
      today         <- Sys.Date() |> format("%Y%m%d")
      outFileName   <- paste(today, outFileName, sep="_")
    }

    outFilePath     <- outpath |> file.path(outFileName)
    ###### Workbook Info
    df_readme1      <- data.frame(x=c(c_sector, as.character(Sys.Date())))

    if(nrow(df_sectorInfo)==1){
      # c_variantLabels <- paste(c_sector, "All Impacts", sep=", ")
      c_variantLabels <- c("All Impacts", "--")
      # df_readme2    <- data.frame(x=c(c_variantLabels, "--"))
    } else{
      c_variantLabels <- df_sectorInfo$variant_label
      # df_readme2    <- data.frame(x=c_variantLabels)
    }
    df_readme2    <- data.frame(x=c_variantLabels)

    ###### Check Directory and File ######
    outDirExists    <- outFilePath |> dirname() |> dir.exists()
    outFileExists   <- outFilePath |> file.exists()
    if(!excel_wb_exists){
      msg2 |> paste0("Warning: Excel template '", inFileName, "' not found in '", inFilePath, "'...") |> message()
      msg2 |> paste0("Exiting without saving...") |> message()
    }
    ### What to do if the directory doesn't exist
    if(!outDirExists){
      msg2 |> paste0("Warning: `outpath='", outpath, "' does not exist...", "\n") |> message()
      msg2 |> paste0("Exiting without saving...") |> message()
    }
    ### What to do if the directory exists
    if(outFileExists & !overwrite){
      msg2 |> paste0("Warning: Excel file '", outFileName,"' already exists!") |> message()
      overwritePrompt <- paste0("Overwrite existing file (y/n)?")
      overwriteInput  <- readline(prompt = overwritePrompt) |> tolower(); #rm("overwritePrompt")
      overwrite       <- ifelse(overwriteInput == "y", T, overwrite)
    }

    ###### If not overwrite, then return and exit
    writeFile <- (outFileExists & overwrite) | (!outFileExists)
    if(!writeFile){
      msg2 |> paste0("Exiting without saving...") |> message()
    } ### End if(!writeFile)
    else{
      ### Open the workbook and write  ReadMe info
      if(msgUser){ msg2 |> paste0("Formatting workbook...") |> message()}
      excel_wb      <- excel_wb_path |> loadWorkbook()
      ### Write sector, date/time, and variant info to workbook
      ### sector & date/time info
      excel_wb |> writeData(
        x = df_readme1, sheet = "ReadMe", startCol = 3, startRow = 3, colNames = F
      )
      ####### Write variant info
      excel_wb |> writeData(
        x = df_readme2, sheet = "ReadMe", startCol = 3, startRow = 7, colNames = F
      )
      ###### Add Styles
      # https://rdrr.io/cran/openxlsx/man/addStyle.html
      for(i in 1:nrow(co_formatting)){
        df_info_i <-  co_formatting[i,] |> as.data.frame()
        format_i  <-  df_info_i$styleName[1]
        sheet_i   <-  df_info_i$worksheet[1] #; sheet_i |> print()
        rows_i    <- (df_info_i$first_row[1]):(df_info_i$end_row[1])
        cols_i    <- (df_info_i$first_col[1]):(df_info_i$end_col[1])
        ### Style
        style_i   <- format_styles[[format_i]]
        ### Add the style to the workbook
        excel_wb |> addStyle(
          style = style_i, sheet = sheet_i,
          rows  = rows_i, cols = cols_i,
          gridExpand = T, stack = T
        )
        ### Remove styles
        rm("df_info_i", "format_i", "sheet_i", "rows_i", "cols_i", "style_i")
      }

      ###### Write results
      if(msgUser){ msg2 |> paste0("Writing results...") |> message()}
      for(i in 1:nrow(df_sectorInfo)){
        variant_i     <- df_sectorInfo$variant_label[i]
        sheet_i       <- excel_wb_sheets[i]
        label_i       <- c_variantLabels[i]
        ### Filter results and rename
        results_i     <- df_results |> filter(variant == variant_i)
        results_i     <- results_i  |> mutate(variant = label_i)
        ### Save results
        excel_wb |> writeData(
          x        = results_i, sheet = sheet_i,
          startCol = 1, startRow = 2, colNames = F, na.string = ""
        )
        rm("i", "variant_i", "sheet_i", "label_i", "results_i")
      }
      ### Save object
      excel_wb |> saveWorkbook(file=outFilePath, overwrite = overwrite)
      rm("excel_wb")
    } ### End if overwrite
    ### System time
    # sysTime4 <- Sys.time(); (sysTime4 - sysTime3) |> print()
  } ### End if save
  ###### Return Object ######
  msg1 |> paste0("Finished.") |> message()
  df_results   <- df_results |> ungroup() |> as.data.frame()

  returnList <- df_results
  # if(.testing) {returnList <- list(results = df_results, county_pop = df_popProj)}
  # else         {returnList <- df_results}
  return(returnList)
}








