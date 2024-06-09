#' Import custom scenarios for temperature, global mean sea level rise (GMSL), population, and GDP from user-specified file names
#'
#' @description
#' This function enables users to import data on custom scenarios for use with [FrEDI::FREDI] (supplied as inputs to [FrEDI::run_fredi()]). Users specify path names to CSV files containing temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and population scenarios. [FrEDI::import_inputs()] reads in and format any specified files as data frames and returns a list of data frames for imported scenarios.
#'
#' @param tempfile A character string indicating the location of a CSV file containing a custom temperature scenario (first column contains years; second column contains temperatures, in degrees Celsius, above the 1995 baseline year). The temperature scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param slrfile A character string indicating the location of a CSV file containing a custom sea level rise scenario (first column contains years; second column contains values for global mean sea level rise (GMSL), in centimeters, above the 2000 baseline). The SLR scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param popfile A character string indicating the location of a CSV file containing a custom population scenario for states and NCA regions. The first column contains years in the interval 2010 to 2300. The second column should contain the NCA Region label associated with the state. The third column should contain state names. The fourth column should contain the state postal code abbreviation. The fifth column should contain the population values. The population scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param gdpfile A character string indicating the location of a CSV file containing a custom scenario for gross domestic product (GDP) (first column contains years; second column contains values for GDP, in total 2015$). The GDP scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param temptype A character string indicating whether the temperature values in the temperature input file (specified by `tempfile` represent global temperature change (`temptype="global"`) or temperature change for the contiguous U.S. (`temptype="conus"`) in degrees Celsius. By default, the model assumes temperatures are CONUS temperatures (i.e., `temptype="conus"`).
#'
#'
#'
#' @details
#' This function enables users to import data on custom scenarios for use with temperature binning. Users specify path names to CSV files containing temperature, global mean sea level rise (GMSL), population, and gross domestic product (GDP) scenarios (`tempfile`, `slrfile`, `gdpfile`, and `popfile`, respectively). [FrEDI::import_inputs()] reads in and formats any specified files as data frames and returns a list of data frames for imported scenarios. Users can specify whether the temperature input is for the contiguous U.S. (CONUS) or global using `temptype`.
#'
#'
#'
#' * __Temperature Inputs.__ The input temperature scenario requires CONUS temperatures in degrees Celsius relative to 1995 (degrees of warming relative to the baseline year--i.e., the central year of the 1986-2005 baseline). CONUS temperature values must be greater than or equal to zero degrees Celsius.
#'    * Users can convert global temperatures to CONUS temperatures using [FrEDI::convertTemps]`(from="global")` (or by specifying [FrEDI::import_inputs]`(temptype="global")` when using [FrEDI::import_inputs()] to import a temperature scenario from a CSV file).
#'    * Temperature inputs must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __SLR Inputs.__ The input SLR scenario requires values for changes in global mean sea level rise (GMSL) heights in centimeters (cm). GMSL heights must be greater than or equal to zero.
#'    * `slrInput` requires a data frame object with two columns containing the year and global mean sea level rise (GMSL) in centimeters, respectively.
#'    * SLR inputs must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __GDP Inputs.__ The input scenario for gross domestic product (GDP) requires national GDP values in 2015$. GDP values must be greater than or equal to zero.
#'    * `gdpInput` requires a data frame object with five columns with names `"year"`, and `"gdp_usd"`, containing the year and the national GDP, respectively. GDP values must be greater than or equal to zero.
#'    * GDP inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __Population Inputs.__ The input population scenario requires state-level population values. Population values must be greater than or equal to zero.
#'    * `popInput` requires a data frame object with five columns with names `"year"`, `"region"`, `"state"`, `"postal"`, and `"reg_pop"`, containing the year, the NCA region name, and the state, the postal code abbreviation, and the state population, respectively.
#'    * Population inputs must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#'
#'
#'
#'
#' [FrEDI::import_inputs()] outputs a list of data frames that can be passed to the main FREDI function [FrEDI::run_fredi()] using the `inputList` argument. For example, specify `run_fredi(inputsList=x)` to generate impacts for a custom scenario `x` (where `x` is a list of data frames such as that output from [FrEDI::import_inputs()]) (see [FrEDI::run_fredi()]).
#'
#' All inputs to [FrEDI::import_inputs()] are optional. If the user does not specify a file path for `tempfile`, `slrfile`, `gdpfile`, or `popfile` (or if there is an error reading in inputs from those file paths), [FrEDI::import_inputs()] outputs a list with a `NULL` value for the associated list element. [FrEDI::run_fredi()] defaults back to the default scenarios for any elements of the inputs list that are `NULL` or missing. In other words, running `run_fredi(inputsList=list())` returns the same outputs as running [FrEDI::run_fredi()] (see [FrEDI::run_fredi()]).
#'
#'
#' @return
#' [FrEDI::import_inputs()] returns a list of named elements containing data frames with custom scenarios for temperature, GMSL, GDP, and regional population, respectively:
#'
#' \tabular{ll}{
#' \strong{List Index} \tab \strong{Description} \cr
#' `tempInput` \tab Data frame containing a custom temperature scenario imported from the CSV file specified by `tempfile`, with missing values removed. `tempInput` has two columns with names `"year"` and `"temp_C"`, containing the year and CONUS temperatures in degrees Celsius, respectively. \cr
#' `slrInput` \tab Data frame containing a custom GMSL scenario imported from the CSV file specified by `slrfile`, with missing values removed. `slrInput` has two columns with names `"year"`, and `"slr_cm"`, containing the year and global mean sea level rise (GMSL) in centimeters, respectively. \cr
#' `gdpInput` \tab Data frame containing a custom GDP scenario imported from the CSV file specified by `gdpfile`, with missing values removed. `gdpInput` has two columns with names `"year"`, and `"gdp_usd"`, containing the year and the national GDP, respectively. \cr
#' `popInput` \tab Data frame containing a custom temperature scenario imported from the CSV file specified by `popfile`, with missing values removed. `popInput` has three columns with names `"year"`, `"region"`, `"state"`, `"postal"`, and `"reg_pop"`, containing the year, the NCA region name, and the state, the postal code abbreviation, and the state population, respectively.. \cr
#' }
#'
#'
#' @examples
#' ### Path to example scenarios
#' scenariosPath <- system.file(package="FrEDI") |> file.path("extdata","scenarios")
#'
#' ### View example scenario names
#' scenariosPath |> list.files()
#'
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath |> file.path("slr_from_gcam.csv")
#'
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath |> file.path("State ICLUS Population.csv")
#'
#' ### Import inputs
#' example_inputsList <- import_inputs(
#'   slrfile  = slrInputFile,
#'   popfile  = popInputFile,
#'   temptype = "global"
#' )
#'
#' ### Use imports with FREDI:
#' df_x <- run_fredi(inputsList=example_inputsList)
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'
#' @export
#' @md
#'
#'

###### import_inputs ######
### Created 2021.02.08. Last updated 2021.02.08
### This function imports data from user-specified file names.
import_inputs <- function(
  tempfile = NULL, ### File path of CSV with temperature inputs
  slrfile  = NULL,
  popfile  = NULL,
  gdpfile  = NULL,
  temptype = "conus", ### "global", or "conus" (default)
  popArea  = "state"  ### "national", "conus", "regional", or "state" (default)
){
  ###### Messaging ######
  namesInputs  <- c("tempfile", "slrfile", "popfile", "gdpfile")
  hasAnyInputs <- list(tempfile, slrfile, popfile, gdpfile)
  hasAnyInputs <- namesInputs  |> map(function(x, list0=hasAnyInputs){!(list0[[x]] |> is.null())})
  hasAnyInputs <- hasAnyInputs |> unlist() |> any()
  silent  <- TRUE
  msgUser <- !silent
  msg0    <- ""
  msg1    <- msg0 |> paste0("\t")
  msg2    <- msg1 |> paste0("\t")
  msg3    <- msg2 |> paste0("\t")
  if(hasAnyInputs) {
    "\n" |> paste0(msg0) |> paste0("In import_inputs():") |> message()
  } ### End if(hasAnyInputs)

  ###### Defaults ######
  geo_msg <- " state..."

  ### Set temperature type default and set temperature type to default if none
  ### is declared. Check whether inputs temperatures are already in CONUS degrees
  temptype_default <- "conus"
  temptype         <- (temptype |> is.null()) |> ifelse(temptype_default, temptype)
  conus            <- ((temptype |> tolower()) == "conus"); #conus |> print()
  
  popArea <- tolower(popArea)

  ###### Initialize Inputs List ######
  ### Get input scenario info: co_inputScenarioInfo
  name_dfScenarioInfo <- "co_inputScenarioInfo"
  assign(name_dfScenarioInfo, rDataList[["frediData"]][["data"]][[name_dfScenarioInfo]])
  name_stateInfo <- "co_states"
  assign(name_stateInfo, rDataList[["frediData"]][["data"]][[name_stateInfo]])
  input_names_vector  <- co_inputScenarioInfo$inputName
  num_inputNames      <- co_inputScenarioInfo |> nrow()

  ###### Initialize Results List ######
  inputsList <- list()

  ###### Iterate Over Inputs List ######
  for(i in 1:num_inputNames){
    ###### Input Info ######
    inputInfo_i <- co_inputScenarioInfo[i,]

    ### Input name and label
    input_i     <- inputInfo_i$inputName |> unique()
    msgName_i   <- inputInfo_i$inputType |> unique()
    ### Input argument and run_fredi argument
    inputArg_i  <- inputInfo_i$importArgName |> unique()
    inputName_i <- inputInfo_i$tempBinListName |> unique()
    ### Min and Max Values
    min_i       <- inputInfo_i$inputMin |> unique()
    max_i       <- inputInfo_i$inputMax |> unique()
    ###### Column Info ######
    region_i    <- inputInfo_i$region |> unique()
    valueCol_i  <- inputInfo_i$valueCol |> unique()
    #valueCol_i  <- (input_i == "pop") |> ifelse("state_pop", valueCol_i)
    ### Initialize column names
    numCols_i   <- colNames_i <- c("year") |> c(valueCol_i)
    # cols0       <- c("region")
    #statecols0  <- c("region", "state", "postal")
    if(popArea == "regional"){
      popcols0 <- c("region")
    }
    if(popArea == "state"){
      popcols0 <- c("state", "postal")
    }
    if(input_i == "pop" & (popArea == "regional"|popArea == "state")) {colNames_i <- popcols0 |> c(colNames_i)}
    
    # if(byState){
    #   cols0      <- statecols0
    #   colNames_i <- numCols_i <- c("year", "state_pop")
    # }
    # ### Add state/region columns as needed
    # if(region_i == 1){
    #   colNames_i  <- c(colNames_i[1], cols0, colNames_i[2])
    # }

    ###### Initialize Results List Element ######
    ### Initialize input in list
    inputsList[[inputName_i]] <- NULL

    ###### Parse File ######
    ### Parse inputArg_i and add to the list, then check if it is null
    inputFile_i  <- parse(text=inputArg_i) |> eval()
    isNullFile_i <- inputFile_i |> is.null()
    # list_i[["inputFile"]] <- inputFile_i
    # isNullFile_i <- list_i[["inputFile"]] |> is.null()

    ###### Format Data Frame ######
    if(!isNullFile_i){
      msg1 |> paste0("User supplied ", msgName_i, " inputs...") |> message()
      msg2 |> paste0("Importing data from ", inputFile_i, "...") |> message()
      ### Try to import the file and initialize the list value
      fileInput_i   <- inputFile_i |> fun_tryInput(silent=T)
      fileStatus_i  <- fileInput_i[["fileStatus"]]
      df_input_i    <- fileInput_i[["fileInput"]]

      ### Message the user
      if(msgUser){ msg2 |> paste0(fileInput_i[["fileMsg"]]) |> message() }

      ######## For loaded data ######
      ### If the load is a success, add results to the input list
      if(fileStatus_i=="loaded"){
        msg2 |> paste0("Formatting ", msgName_i, " inputs...") |> message()


        ###### Standardize All Columns ######
        ### Rename Inputs and Convert all columns to numeric
        ### Rename Inputs and Convert all columns to numeric
        # if(byState){
        #   df_input_i <- df_input_i |>
        #     left_join(co_states, by = c("state" = "state"), suffix = c("", ".y")) |>
        #     select(colNames_i)
        # }

        df_input_i  <- df_input_i |>
          rename_inputs(colNames_i) |>
          mutate_all(as.character) |>
          mutate_at(.vars=c(numCols_i), as.numeric)

        ###### Convert Global Temps to CONUS ######
        ### Convert Global Temps to CONUS if there are temperature inputs and they
        ### aren't already in CONUS degrees
        if(input_i=="temp" & !conus){
          ### Message user
          msg3 |> paste0("User specified `temptype='global'`...") |> message()
          msg3 |> paste0("Converting global temperatures to CONUS temperatures...") |> message()
          ### Convert temps
          df_input_i <- df_input_i |> mutate(temp_C = temp_C |> convertTemps(from="global"))
        } ### End if(input_i=="temp" & !conus)

        # ###### Check Input ######
        # msg2 |> paste0("Checking values...") |> message()
        # ### Values
        # values_i <- df_input_i[,valueCol_i]
        # ### Substitute NULL for missing values for min and max
        # if(is.na(min_i)) min_i <- NULL; if(is.na(max_i)) max_i <- NULL
        # ### Check the status
        # flag_i <- values_i |> check_inputs(xmin = min_i, xmax = max_i)
        # ### Return and message the user if there is a flag:
        # flagStatus_i <- flag_i$flagged
        # flagRows_i   <- flag_i$rows
        # ### If flag, message user and return flagStatus_i
        # if(flagStatus_i){
        #   ### Message labels
        #   numrows_i    <- flagRows_i |> length()
        #   years_i      <- df_input_i$year[flagRows_i]; yearsLabel_i <- paste(years_i, collapse=",")
        #   rangeLabel_i <- paste0("c(", min_i , ",", max_i, ")")
        #   ### Create message and message user
        #   msg1_i       <- msg2 |> paste("Error in importing inputs for", msgName_i) |> paste0("!")
        #   msg2_i       <- msg3 |> paste(inputName_i, "has", numrows_i,  "values outside of defined range", rangeLabel_i)
        #   msg3_i       <- msg3 |> paste("Please correct values", msgName_i, "values for years", yearsLabel_i) |> paste0("...")
        #   ### Message user
        #   "\n" |> paste0(msg0) |> paste0("Warning:") |> message()
        #   msg1_i |> message(); msg2_i |> message(); msg3_i |> message()
        #   "\n" |> paste0(msg0) |> paste0("Exiting...") |> message()
        #
        #   ### Return list with error and flagged rows
        #   returnList <- list(
        #     error_msg    = paste0("Error in ", inputName_i, ". Values outside range."),
        #     flagged_rows = flagRows_i
        #     )
        #
        #   ### Return list and not an inputs list if an error occurred
        #   return(returnList)
        # } ### End if flagged
        
        ##### Calculate State Populations #####
        if(input_i == "pop" & popArea != "state"){
          ### Message user
          msg3 |> paste0("User specified `popArea = '", popArea, "'`...") |> message()
          msg3 |> paste0("Converting ", popArea, " populations to state populations") |> message()

          ### Convert populations
          df_popRatios    <- rDataList[["stateData"]][["data"]][["df_popRatios"]]
          state_ratios    <- df_popRatios |> select(state, postal, region, year, region_to_state) |> distinct()
          reg_ratios      <- df_popRatios |> select(region, year, conus_to_region) |> distinct()
          conus_ratios    <- rDataList[["stateData"]][["data"]][["df_conusRatios"]] ## set to appropriate name

          if(popArea == "national"){# input file should have cols year, pop
            df_input_i <- df_input_i |>
              group_by(year) |>
              summarize(pop = sum(pop)) |>
              left_join(conus_ratios, by = "year") |>
              left_join(reg_ratios, by = "year") |>
              left_join(state_ratios, by = c("year", "region")) |>
              mutate(state_pop = pop * nation_to_conus * conus_to_region * region_to_state)
          }
          if(popArea == "conus"){# input file should have cols year, pop
            df_input_i <- df_input_i |>
              group_by(year) |>
              summarize(pop = sum(pop)) |>
              left_join(reg_ratios, by = c("year")) |>
              left_join(state_ratios, by = c("year", "region")) |>
              mutate(state_pop = pop * conus_to_region * region_to_state)
          }
          if(popArea == "regional"){# input file should have cols region, year, pop, region must be in dot form (e.g. Southern.Plains)
            df_input_i <- df_input_i |>
              group_by(year, region) |>
              summarize(pop = sum(pop)) |>
              left_join(state_ratios, by = c("year", "region")) |>
              mutate(state_pop = pop * region_to_state)
            print(head(df_input_i))
          }
          df_input_i <- df_input_i |>
            select(state, postal, year, state_pop) # do we also need region here?
        } else if(input_i == "pop" & popArea == "state"){
          df_input_i <- df_input_i |>
            rename(state_pop = pop) |>
            select(state, postal, year, state_pop) # need region?
        }

        ###### Update Results List Element ######
        ### Add results to the file
        inputsList[[inputName_i]] <- df_input_i
      } ### End if status == loaded
    } ### End if !isNullFile
  }  ### End iterate on i

  ###### Return input list ######
  msg0 |> paste0("Finished.") |> message()
  return(inputsList)
}

