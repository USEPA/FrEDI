###### Documentation ######
#' FrEDI Social Vulnerability (SV) Module: calculates climate change impacts on socially vulnerable populations throughout the 21st century for available sectors
#'
#' @description
#' This function allows users to project annual average climate change impacts throughout the 21st century (2010-2090) for socially vulnerable populations for available sectors (see [FrEDI::get_sv_sectorInfo()]). Users must pass a data frame of custom driver scenarios to [FrEDI::run_fredi_sv()] using the `driverInput` argument (required) and have the option to pass a population scenario to [FrEDI::run_fredi_sv()] via the `popInput` argument (optional). The output is an R data frame object containing annual average impacts at five-year increments between 2010 and 2090. Users have the option to write results to an Excel file by setting `save=TRUE`. Additional arguments provide more control over how the outputs are saved if `save=TRUE`.
#'
#' @param sector=NULL A character vector indicating for which sector(s) to run the FrEDI SV module.
#' @param driverInput=NULL A data frame of up to four custom scenarios for drivers (temperature or global mean sea level rise). `driverInput` requires a data frame with columns of `"year"` and `"scenario"`. The data frame must also include at least one of the following columns: `"temp_C"` or `"slr_cm"` (depending on whether the sector impacts are driven primarily by temperature or sea level rise, respectively...use [FrEDI::get_sv_sectorInfo()] to get information on driver types). If users include all four columns (`c(``"year",` `"scenario",` `"temp_C",` `"slr_cm"``)`) then [FrEDI::run_fredi_sv()] will determine which driver column (`"temp_C"` or `"slr_cm"`) to use based on the specified sector. Driver inputs (temperature and sea level rise) should start in the year 2000 or earlier. Temperature inputs must be temperature change in degrees Celsius for the CONUS region (if starting from global temperature change, use [FrEDI::convertTemps()] to convert global temperatures to CONUS temperatures before passing to `driverInput`). Sea level rise inputs must be change in sea level in centimeters. All scenarios must include at least two non-missing values. If any required columns are missing, [FrEDI::run_fredi_sv()] will use the default temperature or sea level rise scenario from [FrEDI::run_fredi()]. If the data frame passed to `driverInput` has more than four unique scenarios, [FrEDI::run_fredi_sv()] will only run the first four scenarios.
#' @param popInput=NULL A data frame containing a custom scenario for regional population, with columns `c(``"year",` `"region",` `"reg_pop"``)` (containing the year, region, and regional population, respectively). The data frame passed to `popInput` can be imported using [FrEDI::import_inputs()] (for more information, see [FrEDI::import_inputs()]). Note that, in contrast to the data frame passed to `driverInput`, the `popInput` data frame must be a single scenario (i.e., [FrEDI::run_fredi_sv()] uses the same population scenario for all driver scenarios in `driverInput`). Region names in the `"region"` column must match those in `c(``"Midwest",` `"Northeast",` `"Northwest",` `"Northern Plains",` `"Southeast",` `"Southwest",` `"Southern Plains"``)` or `c(``"Midwest",` `"Northeast",` `"Northwest",` `"Northern.Plains",` `"Southeast",` `"Southwest",` `"Southern.Plains"``)`.
#' @param silent=TRUE A `TRUE/FALSE` value indicating the level of messaging desired by the user (default=`TRUE`).
# @param return=TRUE A `TRUE/FALSE` value indicating whether to return the results as a data frame (default=`TRUE`).
#' @param save=FALSE A `TRUE/FALSE` value indicating whether to save the results to an Excel file (default=`FALSE`).
#' @param outpath=getwd() A character string indicating a file directory to save the Excel file (created if `save=TRUE`). By default, if `save=TRUE`, the Excel file will be saved to the working directory (with a file name determined by the sector).
#' @param overwrite=FALSE A `TRUE/FALSE` value indicating whether to overwrite an existing Excel file if `save=TRUE` (default=`FALSE`). By default, if `save=TRUE`, `overwrite=FALSE`. If `overwrite=FALSE`, [FrEDI::run_fredi_sv()] will not write over an existing file. If `overwrite=FALSE`, and the file already exists, [FrEDI::run_fredi_sv()] will provide the user with the option to write over the existing file: in this case, when [FrEDI::run_fredi_sv()] is ready to save the file, it will message the user and prompt them if they would like to write over the existing file.
#' @param addDate=FALSE A `TRUE/FALSE` value indicating whether to add the date to the name of the output Excel file if `save=TRUE` (default=`FALSE`). By default, if `save=TRUE`, [FrEDI::run_fredi_sv()] will not add the date to the Excel file name (`addDate=FALSE`). If `addDate=TRUE` (and `save=TRUE`), [FrEDI::run_fredi_sv()] will append the system date to the beginning of the name of the outputs Excel file using the format `"%Y%m%d"` (see [base::format()] and [base::Sys.Date()] for additional information).
#'
#' @details This function allows users to project annual average climate change impacts throughout the 21st century (2010-2090) for socially vulnerable populations for available sectors (see [FrEDI::get_sv_sectorInfo()]). [FrEDI::run_fredi_sv()] is the main function for the FrEDI Social Vulnerability (SV) Module in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information). Users have the option to pass a data frame of custom driver scenarios to [FrEDI::run_fredi_sv()] using the `driverInput` argument (required) and have the option to pass a population scenario to [FrEDI::run_fredi_sv()] via the `popInput` argument (optional). The output is an R data frame object containing annual average impacts at five-year increments between 2010 and 2090. Users have the option to write results to an Excel file by setting `save=TRUE`. Additional arguments provide more control over how the outputs are saved if `save=TRUE`.
#'
#' `driverInput` requires a data frame of up to four custom scenarios for drivers (temperature or global mean sea level rise). `driverInput` requires a data frame with columns of `"year"` and `"scenario"`. The data frame must also include at least one of the following columns: `"temp_C"` or `"slr_cm"` (depending on whether the sector impacts are driven primarily by temperature or sea level rise, respectively). If users include all four columns (`c(``"year",``"scenario",``"temp_C",``"slr_cm"``)`) then [FrEDI::run_fredi_sv()] will determine which driver column (`"temp_C"` or `"slr_cm"`) to use based on the specified sector. Driver inputs (temperature and sea level rise) should start in the year 2000 or earlier. Temperature inputs must be temperature change in degrees Celsius for the CONUS region (if starting from global temperature change, use [FrEDI::convertTemps()] to convert global temperatures to CONUS temperatures before passing to `driverInput`). Sea level rise inputs must be change in sea level in centimeters. All scenarios must include at least two non-missing values. If any required columns are missing, [FrEDI::run_fredi_sv()] will use the default temperature or sea level rise scenario from [FrEDI::run_fredi()]. If the data frame passed to `driverInput` has more than four unique scenarios, [FrEDI::run_fredi_sv()] will only run the first four scenarios.
#'
#' `popInput` is an optional input that takes a data frame containing a custom scenario for regional population, with columns `c(``"year",` `"region",` `"reg_pop"``)` (containing the year, region, and regional population, respectively). If `popInput=NULL` (default), [FrEDI::run_fredi_sv()] will use the default regional population scenario from the Integrated Climate and Land Use Scenarios version 2 (ICLUSv2) model (Bierwagen et al, 2010; EPA 2017) under the Median variant projection of United Nations (United Nations, 2015). The data frame passed to `popInput` can be imported using [FrEDI::import_inputs()] (for more information, see [FrEDI::import_inputs()]). Note that, in contrast to the data frame passed to `driverInput`, the `popInput` data frame must be a single scenario (i.e., [FrEDI::run_fredi_sv()] uses the same population scenario for all driver scenarios in `driverInput`). Region names in the `"region"` column must match those in `c(``"Midwest",` `"Northeast",` `"Northwest",` `"Northern Plains",` `"Southeast",` `"Southwest",` `"Southern Plains"``)` or `c(``"Midwest",` `"Northeast",` `"Northwest",` `"Northern.Plains",` `"Southeast",` `"Southwest",` `"Southern.Plains"``)`.
#'
#' @return
#' The output of [FrEDI::run_fredi_sv()] is an R data frame object containing annual average impacts, by year (2010-2090), for each sector, variant, model (GCM or SLR scenario), and region.
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
#'
#' @export
#' @md
#'
###### run_fredi_sv ######
### This function creates a data frame of sector impacts for default values or scenario inputs.
### run_fredi_sv relies on the following helper functions: "interpolate_annual", "match_scalarValues","get_econAdjValues" , "calcScalars", "interpolate_tempBin"
run_fredi_sv <- function(
  sector      = NULL, ### Vector of sectors to get results for
  driverInput = NULL,
  popInput    = NULL,
  silent      = TRUE,  ### Whether to message the user
  save        = FALSE,
  outpath     = getwd(),
  overwrite   = FALSE,
  addDate     = FALSE ### Whether to add the date to the file name
){
  ###### Set up the environment ######
  pkgPath <- NULL
  ### Assign previous configuration objects
  for(i in 1:length(fredi_config)) assign(names(fredi_config)[i], fredi_config[[i]])
  c_svGroupTypes <- svDataList$c_svGroupTypes

  ### Level of messaging (default is to message the user)
  silent  <- ifelse(is.null(silent), T, silent)
  msgUser <- !silent
  msg0    <- ""
  msg1    <- msg0 %>% paste0("\t")
  msg2    <- msg1 %>% paste0("\t")
  msg3    <- msg2 %>% paste0("\t")

  ###### Sector Info ######
  ### Objects <-
  sectorInfo    <- svDataList$sectorInfo
  svSectorInfo  <- svDataList$svSectorInfo
  co_formatting <- svDataList$co_formatting
  co_formatting <- svDataList$co_formatting
  # co_modelTypes <- rDataList$co_modelTypes
  ### Sector names
  if(is.null(sector)){
    sector_msg1   <- paste0("Please select a sector: ") %>% print
    sector_msg2   <- 1:nrow(sectorInfo) %>% lapply(function(i){
      sector_i    <- sectorInfo$sector[i]
      msg_i       <- paste0(i, ". ", sector_i)
      msg_i %>% print
      # return(msg_i)
    }) #%>% unlist %>% paste(collapse="")
    # sector_msg3  <- sector_msg1 %>% paste0(sector_msg2)
    sector_msg3   <- "Enter a number:"
    sector_input  <- readline(prompt = sector_msg3) %>% as.numeric
    sector        <- sectorInfo$sector[sector_input]
    rm("sector_msg1", "sector_msg2", "sector_msg3", "sector_input")
  }
  c_sector        <- sector
  paste0("Running FrEDI SV for sector '", c_sector, "':") %>% message
  ### Sector info
  which_sector    <- (sectorInfo$sector == c_sector) %>% which
  c_modelType     <- sectorInfo$modelType[which_sector] %>% tolower
  # paste0("Running FrEDI SV for sector ", c_sector) %>% message

  # df_sectorInfo <- svSectorInfo %>% filter(sector==sector)
  which_sector    <- (svSectorInfo$sector == c_sector) %>% which #; which_sector %>% print
  df_sectorInfo   <- svSectorInfo[which_sector,]
  c_variants      <- df_sectorInfo$variant_abbr
  c_variantLabels <- df_sectorInfo$variant_label

  ###### Check Driver Inputs ######
  ### Initialize whether to check for inputs
  check_slrInput  <- ifelse(c_modelType=="slr", T, F)
  check_tempInput <- TRUE
  ### Initialize whether inputs exist
  has_driverInput <- ifelse(is.null(driverInput), F, T)
  has_slrInput    <- FALSE
  has_tempInput   <- FALSE
  ### Scenario columns
  tempCols        <- c("year", "temp_C", "scenario")
  slrCols         <- c("year", "slr_cm", "scenario")
  ### Scenario ranges
  tempRange       <- c(0, 10)
  slrRange        <- c(0, 250)

  ### Check inputs
  if(has_driverInput){
    msg1 %>% message("Checking `driverInput` values...")
    ### Check that the input is a dataframe
    class_driverInput <- driverInput %>% class
    if(!("data.frame" %in% class_driverInput)){
      msg2 %>% message("Error: `driverInput` must have `class='data.frame'`!", "\n")
      msg2 %>% message("Exiting...")
      return()
    }

    ### Info about driverInputs
    driverInputCols <- driverInput %>% names; # driverInputCols %>% print
    has_scenarioCol <- "scenario" %in% driverInputCols; #has_scenarioCol %>% print
    ### Check scenarios
    if(has_scenarioCol){
      msg1 %>% message("Checking scenarios in `driverInput`...")
      ### If scenarios are present, check the number of scenarios
      c_scenarios <- driverInput$scenario %>% unique
      n_scenarios <- c_scenarios %>% length
      if(n_scenarios > 4){
        msg2 %>% message("Warning: `driverInput` has more than four distinct scenarios!", "")
        msg3 %>% message("Only the first four scenarios will be used...", "\n")
        c_scenarios <- c_scenarios[1:4]; n_scenarios <- c_scenarios %>% length
        driverInput <- driverInput %>% filter(scenario %in% c_scenarios)
      }
    } else{
      msg2 %>% message("Error: `driverInput` must have column='scenario' present`!", "\n")
      msg2 %>% message("Exiting...")
      return()
    }

    ### Check input years
    if(!("year" %in% driverInputCols)){
      msg2 %>% message("Error: `driverInput` must have column='year' present`!", "\n")
      msg2 %>% message("Exiting...")
      return()
    }

    ### Check for SLR inputs
    if(check_slrInput){
      msg1 %>% message("Checking `driverInput` values for SLR scenario...")
      ### Check for SLR columns
      slrCols_inInput <- (slrCols %in% driverInputCols)
      if(all(slrCols_inInput)){
        msg2 %>% message("All SLR scenario columns present...")
        has_slrInput    <- TRUE
        has_tempInput   <- FALSE
        check_tempInput <- FALSE

        ### Check input SLR heights
        checkIssues <- (driverInput$slr_cm < slrRange[1]) | (driverInput$slr_cm > slrRange[2])
        anyIssues   <- checkIssues %>% any
        ### Check non-missing values
        n_nonNA     <- (!is.na(driverInput$slr_cm)) %>% length
        naIssues    <- n_nonNA < n_scenarios/2
        if(anyIssues){
          msg2 %>% message("Error: values for 'slr_cm' must be in the allowable range of [", slrRange[1], ",", slrRange[2], "]!", "\n")
          msg2 %>% message("Exiting...")
          return()
        } else if(naIssues){
          msg2 %>% message("Error: each scenario must have at least two non-missing values for 'slr_cm'!", "\n")
          msg2 %>% message("Exiting...")
          return()
        }

      } else{
        ### Instead of exiting, check for temperature
        msg1 %>% message("Warning: `driverInput` is missing the following SLR scenario input columns:")
        msg2 %>% message("'", paste(slrCols[!slrCols_inInput], collapse="', '"),"'...", "\n")
        msg1 %>% message("Looking for temperature scenario instead", "...", "\n")
      }
    }
    ### Otherwise, check temperature inputs
    if(check_tempInput){
      msg1 %>% message("Checking `driverInput` values for temperature scenario...")
      ### Check for temperature columns
      tempCols_inInput <- (tempCols %in% driverInputCols)
      if(all(tempCols_inInput)){
        msg1 %>% message("All temperature scenario columns present...")
        has_tempInput    <- TRUE

        ### Check input temperatures
        checkIssues <- (driverInput$temp_C < tempRange[1]) | (driverInput$temp_C > tempRange[2])
        anyIssues   <- checkIssues %>% any
        ### Check non-missing values
        n_nonNA     <- (!is.na(driverInput$temp_C)) %>% length
        naIssues    <- n_nonNA < n_scenarios/2
        if(anyIssues){
          msg2 %>% message("Error: values for 'temp_C' must be in the allowable range of [", tempRange[1], ",", tempRange[2], "]!", "\n")
          msg2 %>% message("Exiting...")
          return()
        } else if(naIssues){
          msg2 %>% message("Error: each scenario must have at least two non-missing values for 'temp_C'!", "\n")
          msg2 %>% message("Exiting...")
          return()
        }

      } else{
        msg2 %>% message("Error in temperature scenario input...")
        msg2 %>% message("`driverInput` is missing columns: '", paste(tempCols[!tempCols_inInput], collapse=", '"),"'...", "\n")
        msg2 %>% message("Exiting...")
        return()
      }
    }
  }

  ###### Check Population Inputs ######
  has_popInput <- ifelse(is.null(popInput), F, T)
  popCols      <- c("year", "reg_pop", "region")
  if(has_popInput){
    msg1 %>% message("Checking `popInput` values...")
    ### Check that the input is a dataframe
    class_popInput <- popInput %>% class
    if(!("data.frame" %in% class_popInput)){
      msg2 %>% message("Error: `popInput` must have `class='data.frame'`!", "\n")
      msg2 %>% message("Exiting...")
      return()
    }
    ### Info about popInputs
    popInputCols <- popInput %>% names
    ### Check for popInput columns
    popCols_inInput <- (popCols %in% popInputCols)
    if(all(popCols_inInput)){
      msg2 %>% message("All population scenario columns present...")
      has_popInput    <- TRUE

      ### Check input Population values: population >= 0
      checkIssues <- (popInput$reg_pop < 0)
      anyIssues   <- checkIssues %>% any
      if(anyIssues){
        msg2 %>% message("Error: Values for 'reg_pop' in `popInput` must be greater than zero!")
        msg2 %>% message("Exiting...")
        return()
      }

      ### Check input Population values: no repeating years
      checkYears   <- (popInput$year %>% unique)
      checkRegions <- (popInput$region %>% unique)
      anyIssues    <- (checkYears %>% length)*(checkRegions %>% length) < (popInput %>% nrow)
      if(anyIssues){
        msg2 %>% message("Error: duplicate years present in `popInput`!")
        msg2 %>% message("Exiting...")
        return()
      }

    } else{
      ### Exit and message the user
      msg2 %>% message("Error: `popInput` is missing the following input columns:")
      msg3 %>% message("'", paste(popCols[!popCols_inInput], collapse="', '"),"'...", "\n")
      # msg1 %>% message("Using default regional population scenario", "...", "\n")
      # has_popInput    <- FALSE
      msg2 %>% message("Exiting...")
      return()
    }
  }


  ###### Temperature Scenario ######
  ### User inputs: temperatures have already been converted to CONUS temperatures. Filter to desired range.
  ### Add the point where impacts are zero (the reference year temperature)
  ### For user inputs:
  ### - Select appropriate columns
  ### - Remove missing values of years, temperatures
  ### - Filter to appropriate years
  refYear_temp <- (rDataList$co_modelTypes %>% filter(modelUnitType=="temperature"))$modelRefYear[1]
  if(has_tempInput){
    msg1 %>% message("Creating temperature scenario from user inputs...")
    tempInput    <- driverInput %>%
      select(c("year", "temp_C", "scenario")) %>%
      filter( year >  refYear_temp & year <= maxYear) %>%
      filter(!is.na(temp_C) & !(is.na(year)))
  }
  else{
    if(!has_slrInput){
    ### Otherwise use default scenario and add scenario column
    msg1 %>% message("No temperature scenario provided...")
    msg2 %>% message("Using default temperature scenario...")
    # rDataList$co_defaultTemps %>% names %>% print
    }
    tempInput <- rDataList$co_defaultTemps %>%
      mutate(temp_C = temp_C_global %>% convertTemps(from="global")) %>%
      select(c("year", "temp_C")) %>%
      mutate(scenario="FrEDI Default")
  }
  ### Interpolate over scenarios:
  temp_df <- tempInput$scenario %>% unique %>%
    lapply(function(scenario_i){
      ### - Filter to scenario i and drop scenario column
      ### - Zero out series at the temperature reference year
      # tempInput %>% names %>% print
      input_i <- data.frame(year= refYear_temp, temp_C = 0) %>%
        rbind(
          tempInput %>%
            filter(scenario==scenario_i) %>%
            select(-c("scenario"))
        )

      ### Then, interpolate
      ### - Use minimum series year to determine interpolation years
      ### - Add a dummy region for National Total for interpolate_annual
      ### - Interpolate, drop dummy region, and add scenario back in
      df_i <-     input_i %>%
        (function(x){
          minYear_x <- x$year %>% min
          interpYrs <- refYear_temp:maxYear
          x_interp  <- x %>%
            mutate(region="National Total") %>%
            interpolate_annual(years = interpYrs, column = "temp_C", rule = 1:2) %>%
            select(-c("region"))
          return(x_interp)
        }) %>%
        mutate(scenario = scenario_i)
      return(df_i)
    }) %>%
    (function(x){do.call(rbind, x)})
  ### Remove intermediate objects
  rm("tempInput", "refYear_temp")

  ###### SLR Scenario ######
  ### Year where SLR impacts are zero
  ### Follow similar procedure to temperatures:
  ### - Select appropriate columns
  ### - Remove missing values of years, slr
  ### - Filter to appropriate years
  refYear_slr <- (rDataList$co_modelTypes %>% filter(modelUnitType=="slr"))$modelRefYear %>% unique
  if(has_slrInput){
    msg1 %>% message("Creating SLR scenario from user inputs...")
    slrInput  <- driverInput %>%
      select(c("year", "slr_cm", "scenario")) %>%
      filter(!is.na(slr_cm) & !is.na(year)) %>%
      filter( year >  refYear_slr, year <= maxYear)

    ### Interpolate over scenarios
    slr_df <- slrInput$scenario %>% unique %>%
      lapply(function(scenario_i){
        ### - Filter to scenario i and drop scenario column
        ### - Zero out series at the slr reference year
        input_i <- data.frame(year= refYear_slr, slr_cm = 0) %>%
          rbind(
            slrInput %>%
              filter(scenario==scenario_i) %>%
              select(-c("scenario"))
          )

        ### Then, interpolate
        ### - Use minimum series year to determine interpolation years
        ### - Add a dummy region for National Total for interpolate_annual
        ### - Interpolate, drop dummy region, and add scenario back in
        df_i <-     input_i %>%
          (function(x){
            minYear_x <- x$year %>% min
            interpYrs <- refYear_slr:maxYear
            x_interp  <- x %>%
              mutate(region="National Total") %>%
              interpolate_annual(years = interpYrs, column = "slr_cm", rule = 1:2) %>%
              select(-c("region"))
            return(x_interp)
          }) %>%
          mutate(scenario = scenario_i)
        return(df_i)
      }) %>%
      (function(x){do.call(rbind, x)})
    ### Remove intermediate objects
    rm("slrInput")
  }
  ### If there is no SLR scenario, calculate from temperatures
  ### First convert temperatures to global temperatures
  ### Then convert global temps to SLR
  else{
    msg1 %>% message("No SLR scenario provided...")
    msg2 %>% message("Creating SLR scenario from temperature scenario...")
    slr_df <- temp_df$scenario %>% unique %>%
      lapply(function(scenario_i){
        df_i <- temp_df %>% filter(scenario==scenario_i) %>%
          mutate(temp_C_global = temp_C %>% convertTemps(from="conus")) %>%
          (function(x){
            temps2slr(temps = x$temp_C_global, years = x$year)
          }) %>%
          mutate(scenario=scenario_i)
      }) %>%
      (function(x){do.call(rbind, x)})
  }
  ### Remove intermediate objects
  rm("refYear_slr")


  ###### Standardize Driver Scenarios ######
  ### Get unique temperature scenarios and unique SLR scenarios
  ### Subset to desired years
  if(c_modelType=="gcm"){
    drivers_df  <- temp_df %>% filter(year %in% list_years_by5) %>%
      rename(driverValue = "temp_C") %>%
      mutate(driverUnit  = "degrees Celsius")
    rm("temp_df")
  } else{
    drivers_df   <- slr_df %>% filter(year %in% list_years_by5) %>%
      rename(driverValue = "slr_cm") %>%
      mutate(driverUnit  = "cm")
    ### Remove intermediate objects
    rm("slr_df")
    if(has_tempInput){rm("temp_df")}
  }
  c_scenarios <- drivers_df$scenario %>% unique

  ###### Region Population Scenario ######
  ### Population inputs
  if(has_popInput){
    msg1 %>% message("Creating population scenario from user inputs...")
    popInput  <- popInput %>% select(c("year", "reg_pop", "region")) %>%
      mutate(region = gsub("\\.", " ", region))
    pop_df    <- popInput %>%
      interpolate_annual(years= list_years_by5, column = "reg_pop", rule = 2:2) %>%
      rename(region_pop = reg_pop) %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
    ### Remove intermediate object
    rm("popInput")
  } else{
    msg1 %>% message("No population scenario provided...")
    msg2 %>% message("Using default population scenario...")
    pop_df <- svPopList$iclus_region_pop %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
  }

  ###### County Population Scenario ######
  msg1 %>% message("Calculating county population from regional population...")
  df_popProj <-
    calc_countyPop(
      regPop  = pop_df,
      funList = svPopList$popProjList,
      years   = list_years_by5
    ); #rm("popProjList")

  ###### Calculate Impacts ######
  ### Iterate over adaptations/variants
  pkgPath     <- ifelse(is.null(pkgPath), system.file(package="FrEDI"), pkgPath)
  impactsPath <- pkgPath %>% file.path("extdata", "sv", "impactLists")
  df_results  <-
    1:nrow(df_sectorInfo) %>%
    # 1:1 %>%
    lapply(function(i){
      sectorAbbr_i   <- df_sectorInfo$impactList_fileExt[i]
      variantLabel_i <- df_sectorInfo$variant_label[i]
      variantAbbr_i  <- df_sectorInfo$variant_abbr[i]
      weightsCol_i   <- df_sectorInfo$popWeightCol[i]

      ### Which impacts list to use
      impactsName_i     <- "impactsList" %>%
        paste(sectorAbbr_i, sep="_") %>%
        paste0(ifelse(is.na(variantAbbr_i), "", "_")) %>%
        paste0(ifelse(is.na(variantAbbr_i), "", variantAbbr_i))
      impactsPath_i     <- impactsPath %>% file.path(impactsName_i) %>% paste0(".rds")

      ### Which SV data to use
      svName_i     <- ifelse(c_sector=="Coastal Properties", "svDataCoastal", "svData"); # svName_i %>% print

      ###### Iterate Over Scenarios ######
      results_i <- c_scenarios %>% lapply(function(scenario_j){
         paste0("\n", msg1, "Calculating impacts for sector='", c_sector, "', variant='", variantLabel_i, "', scenario='", scenario_j, "'...") %>% message
        ###### Scaled Impacts ######
        drivers_j <- drivers_df %>% filter(scenario == scenario_j) %>% select(-c("scenario"))
        ### Get impact list, calculate scaled impacts, remove impact list

        if(!exists("impactsList_j")){impactsList_j <- impactsPath_i %>% readRDS}
        impacts_j <- calc_tractScaledImpacts(
          funList      = impactsList_j,
          driverValues = drivers_j,
          silent       = silent,
          .msg0        = msg2
        ) %>%
          mutate(year  = year %>% as.numeric)
        if(exists("impactsList_j")){remove(list=c("impactsList_j"), inherits = T)}
        # impacts_j %>% nrow %>% print

        impacts_j <- impacts_j %>% filter(!is.na(sv_impact))
        # impacts_j %>% nrow %>% print

        ###### Total Impacts ######
        ### Load the SV data if not loaded; remove after running
        if(!exists("svInfo")){svInfo <- svDataList[[svName_i]]}

        impacts_j      <- impacts_j %>%
          calc_tractImpacts(
            popData   = df_popProj,
            svInfo    = svInfo,
            weightCol = weightsCol_i,
            sector    = c_sector,
            svGroups  = c_svGroupTypes,
            silent    = silent,
            .msg0     = msg2
          ); if(exists("svInfo")){rm("svInfo")}

        ###### Return Impacts ######
        impacts_j <- impacts_j %>% mutate(scenario = scenario_j)
        return(impacts_j)

      }) %>%
        (function(y){do.call(rbind, y)}) %>%
        mutate(variant = variantLabel_i)

      return(results_i)
    }) %>%
    (function(x){do.call(rbind, x)})

  ###### Format Results ######
  df_results   <- df_results %>% ungroup %>% as.data.frame

  ###### Save Results ######
  if(save){
    msg1 %>% paste0("Saving results to Excel...") %>% message
    ###### File Info ######
    ###### Template Info
    inFilePath      <- system.file(package="FrEDI") %>% file.path("extdata", "sv")
    inFileName      <- "FrEDI SV Graphics Template.xlsx"
    ###### Workbook Info
    excel_wb_path   <- inFilePath   %>% file.path(inFileName)
    excel_wb_exists <- excel_wb_path %>% file.exists; #excel_wb_exists %>% print
    excel_wb_sheets <- c("FrEDI Outputs 1", "FrEDI Outputs 2")
    ###### Outfile Info and add date if specified
    outFileBase     <- "FrEDI" %>% paste("SV", "Outputs", c_sector, sep="_") #; outFileBase %>% print
    outFileName     <- outFileBase %>% paste0(".xlsx"); #outFileName %>% print
    if(addDate){
      today         <- Sys.Date() %>% format("%Y%m%d")
      outFileName   <- paste(today, outFileName, sep="_")
    }

    outFilePath     <- outpath %>% file.path(outFileName)
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
    outDirExists    <- outFilePath %>% dirname %>% dir.exists
    outFileExists   <- outFilePath %>% file.exists
    if(!excel_wb_exists){
      msg2 %>% paste0("Warning: Excel template '", inFileName, "' not found in '", inFilePath, "'...") %>% message
      msg2 %>% paste0("Exiting without saving...") %>% message
      msg1 %>% paste0("Finished.") %>% message
    }
    ### What to do if the directory doesn't exist
    if(!outDirExists){
      msg2 %>% paste0("Warning: `outpath='", outpath, "' does not exist...", "\n") %>% message
      msg2 %>% paste0("Exiting without saving...") %>% message
      msg1 %>% paste0("Finished.") %>% message
    }
    ### What to do if the directory exists
    if(outFileExists & !overwrite){
      msg2 %>% paste0("Warning: Excel file '", outFileName,"' already exists!") %>% message
      overwritePrompt <- paste0("Overwrite existing file (y/n)?")
      overwriteInput  <- readline(prompt = overwritePrompt) %>% tolower; #rm("overwritePrompt")
      overwrite       <- ifelse(overwriteInput == "y", T, overwrite)
    }

    ###### If not overwrite, then return and exit
    writeFile <- (outFileExists & overwrite) | (!outFileExists)
    if(!writeFile){
      msg2 %>% paste0("Exiting without saving...") %>% message
      msg1 %>% paste0("Finished.") %>% message
    } else{
      ### Open the workbook and write  ReadMe info
      if(msgUser){ msg2 %>% paste0("Formatting workbook...") %>% message}
      excel_wb      <- excel_wb_path %>% loadWorkbook()

      ### Write sector, date/time, and variant info to workbook
      ### sector & date/time info
      excel_wb %>%
        writeData(
          x = df_readme1, sheet = "ReadMe", startCol = 3, startRow = 3, colNames = F
        )
      ####### Write variant info
      excel_wb %>%
        writeData(
          x = df_readme2, sheet = "ReadMe", startCol = 3, startRow = 7, colNames = F
        )
      ###### Add Styles
      # https://rdrr.io/cran/openxlsx/man/addStyle.html
      for(i in 1:nrow(co_formatting)){
        df_info_i <-  co_formatting[i,] %>% as.data.frame
        format_i  <-  df_info_i$styleName[1]
        sheet_i   <-  df_info_i$worksheet[1] #; sheet_i %>% print
        rows_i    <- (df_info_i$first_row[1]):(df_info_i$end_row[1])
        cols_i    <- (df_info_i$first_col[1]):(df_info_i$end_col[1])
        ### Style
        style_i   <- format_styles[[format_i]]
        ### Add the style to the workbook
        excel_wb %>% addStyle(
          style = style_i,
          sheet = sheet_i,
          rows  = rows_i, cols = cols_i,
          gridExpand = T, stack = T
        )
      }
      ### Remove styles
      rm("df_info_i", "style_i", "sheet_i", "rows_i", "cols_i")

      ###### Write results
      if(msgUser){ msg2 %>% paste0("Writing results...") %>% message}
      for(i in 1:nrow(df_sectorInfo)){
        variant_i     <- df_sectorInfo$variant_label[i]
        sheet_i       <- excel_wb_sheets[i]
        label_i       <- c_variantLabels[i]

        ### Filter results
        # results_i   <- df_results %>% filter(variant == variant_i)
        results_i   <- df_results %>%
          filter(variant == variant_i) %>%
          # mutate(variant = variant %>% as.character) %>%
          mutate(variant = label_i)

        ### Save results
        excel_wb %>%
          writeData(
            x        = results_i,
            sheet    = sheet_i,
            startCol = 1,
            startRow = 2,
            colNames = F
          )
      }
      ### Remove objects
      rm("variant_i", "sheet_i", "results_i")
      excel_wb %>% saveWorkbook(file=outFilePath, overwrite = overwrite)
      rm("excel_wb")
    } ### End if overwrite
    ### System time
    # sysTime4 <- Sys.time(); (sysTime4 - sysTime3) %>% print
  } ### End if save
  ###### Return Object ######
  message("\n", "Finished", ".")
  df_results   <- df_results %>% ungroup %>% as.data.frame
}








