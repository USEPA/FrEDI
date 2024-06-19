#' Import custom scenarios for temperature, global mean sea level rise (GMSL), population, and GDP from user-specified file names
#'
#' @description
#' This function enables users to import data on custom scenarios for use with [FrEDI::run_fredi()]. Users specify path names to CSV files containing temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and state population scenarios. [FrEDI::import_inputs()] reads in and format any specified files as data frames and returns a list of data frames for imported scenarios.
#'
#' @param tempfile=NULL A character string indicating the location of a CSV file containing a custom temperature scenario (first column contains years; second column contains temperatures, in degrees Celsius, above the 1995 baseline year). The temperature scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param slrfile=NULL A character string indicating the location of a CSV file containing a custom sea level rise scenario (first column contains years; second column contains values for global mean sea level rise (GMSL), in centimeters, above the 2000 baseline). The SLR scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param popfile=NULL A character string indicating the location of a CSV file containing a custom population scenario for states and NCA regions. The first column contains years in the interval 2010 to 2300. The second column should contain the NCA Region label associated with the state. The third column should contain state names. The fourth column should contain the state postal code abbreviation (e.g., `postal = "ME"` for `state = "Maine"`. The fifth column should contain the state population values. The population scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param gdpfile=NULL A character string indicating the location of a CSV file containing a custom scenario for gross domestic product (GDP) (first column contains years; second column contains values for GDP, in total 2015$). The GDP scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param temptype="conus" A character string indicating whether the temperature values in the temperature input file (as specified by `tempfile`) represent temperature changes, in degrees Celsius, at the global level (`temptype = "global"`) or for the contiguous U.S. (CONUS) only (`temptype = "conus"`, default).
#' @param popArea="state" A character string indicating the geographical scale of population inputs (as specified by `popfile`). Specify one of: `"national"`, for national totals (including Alaska and Hawaii); `"conus"`, for the contiguous U.S. (CONUS) only (i.e., national totals, but excluding Alaska and Hawaii); `"regional"`, for totals at the levels of NCA regions (i.e., ); or `"state"`, for state-level population (default).
#'
#'
#'
#' @details
#' This function enables users to import data on custom scenarios for use with temperature binning. Users specify path names to CSV files containing temperature, global mean sea level rise (GMSL), population, and gross domestic product (GDP) scenarios (`tempfile`, `slrfile`, `gdpfile`, and `popfile`, respectively). [FrEDI::import_inputs()] reads in and formats any specified files as data frames and returns a list of data frames for imported scenarios. Users can specify whether the temperature input is for the contiguous U.S. (CONUS) or global using `temptype`.
#'
#'
#'
#' * __Temperature Inputs.__ The input temperature scenario requires CONUS temperatures in degrees Celsius relative to 1995 (degrees of warming relative to the baseline year--i.e., the central year of the 1986-2005 baseline). CONUS temperature values must be greater than or equal to zero degrees Celsius.
#'    * Users can convert global temperatures to CONUS temperatures using [FrEDI::convertTemps]`(from = "global")` (or by specifying [FrEDI::import_inputs]`(temptype = "global")` when using [FrEDI::import_inputs()] to import a temperature scenario from a CSV file).
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
#' [FrEDI::import_inputs()] outputs a list of data frames that can be passed to the main FREDI function [FrEDI::run_fredi()] using the `inputList` argument. For example, specify `run_fredi(inputsList = x)` to generate impacts for a custom scenario `x` (where `x` is a list of data frames such as that output from [FrEDI::import_inputs()]) (see [FrEDI::run_fredi()]).
#'
#' All inputs to [FrEDI::import_inputs()] are optional. If the user does not specify a file path for `tempfile`, `slrfile`, `gdpfile`, or `popfile` (or if there is an error reading in inputs from those file paths), [FrEDI::import_inputs()] outputs a list with a `NULL` value for the associated list element. When using [FrEDI::import_inputs()] with [FrEDI::run_fredi()], [FrEDI::run_fredi()] defaults back to the default scenarios for any elements of the inputs list that are `NULL` or missing. In other words, running `run_fredi(inputsList = list())` returns the same outputs as running [FrEDI::run_fredi()] (see [FrEDI::run_fredi()]).
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
#' `popInput` \tab Data frame containing a custom temperature scenario imported from the CSV file specified by `popfile`, with missing values removed. `popInput` has three columns with names `"year"`, `"region"`, `"state"`, `"postal"`, and `"reg_pop"`, containing the year, the NCA region name, and the state, the postal code abbreviation, and the state population, respectively. \cr
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
  silent  <- TRUE
  msgUser <- !silent
  msgN    <- "\n"
  msg0    <- ""
  msg1    <- msg0 |> paste0("\t")
  msg2    <- msg1 |> paste0("\t")
  msg3    <- msg2 |> paste0("\t")
  geo_msg <- " state..."

  ### Message the user
  msgN |> paste0(msg0) |> paste0("In import_inputs():") |> message()


  ###### Load Data from FrEDI ######
  ### Get objects from FrEDI name space
  ### Get input scenario info: co_inputScenarioInfo
  ### Get state info: co_states
  fListNames <- c("co_inputScenarioInfo", "co_states")
  sListNames <- c("df_popRatios")
  for(name_i in fListNames){name_i |> assign(rDataList[["frediData"]][["data"]][[name_i]]); rm(name_i)}
  for(name_i in sListNames){name_i |> assign(rDataList[["stateData"]][["data"]][[name_i]]); rm(name_i)}


  ###### Defaults ######
  ### Input names, output names
  inNames    <- c("temp", "slr", "gdp", "pop")
  outNames   <- inNames |> paste0("Input")
  ### Valid values
  tempTypes  <- c("global", "conus")
  popAreas   <- c("national", "conus", "regional", "state")
  ### Convert string inputs to lower case
  tempType   <- temptype |> tolower()
  popArea    <- popArea  |> tolower()
  ### Check whether inputs temperatures are already in CONUS degrees
  conus      <- "conus" %in% tempType
  # conus |> print()
  ### Check for validity
  tempValid  <- tempType %in% tempTypes
  popValid   <- popArea %in% popAreas
  ### Error messages
  msgTemp    <- paste0("`tempType` must be either \"global\" or \"conus\", not \"", tempType, "\"!")
  msgPop     <- paste0("`popArea` must be in \"", popAreas |> paste(collapse="\", \""), ", not \"", popArea, "\", !")
  ### If not valid, message and drop those from outputs
  if(!tempValid) {
    fListNames[["temp"]] <- NULL
    msg1 |> paste0(msgTemp) |> message()
    msg2 |> paste0("Dropping temp inputs from outputs.") |> message()
  } else if(!popValid){
    fListNames[["pop"]] <- NULL
    msg1 |> paste0(msgPop)
    msg2 |> paste0("Dropping pop inputs from outputs.") |> message()
  } ### End if(!tempValid)


  ###### Other Values ######
  ### Get list with expected name of column containing values
  ### Get list with expected name of columns used for unique ids
  valCols    <- get_import_inputs_valCols(popArea=popArea)
  idCols     <- get_import_inputs_idCols (popArea=popArea)
  # valCols |> unlist() |> print(); idCols  |> unlist() |> print()

  ###### Initialize Input File Names List ######
  ### Initialize list of file names and
  fList      <- inNames |> map(function(x){parse(text=x |> paste0("file")) |> eval()}) |> set_names(inNames)

  ###### Initialize Inputs List ######
  ### Initialize a list for loaded data
  ### Check if any file names supplied
  inputsList <- fList
  hasInputs  <- fList |> map(function(item_i){!(item_i |> is.null())}) |> unlist() |> any()

  ### Message user
  if(hasInputs) {msg1 |> paste0("Loading data...", "\n") |> message()}
  else          {msg1 |> paste0("No inputs specified! Returning an empty list.") |> message()}

  ### If some file names supplied, iterate over list, trying to read in file names
  inputsList <- list(
    inputName = inNames,
    fileName  = inputsList
  ) |>
    pmap(run_fun_tryInput) |>
    set_names(inNames)
  # inputsList |> print()

  ###### Check the Inputs ######
  ### Message the user
  ### Use the input info to check the values make sense
  if(hasInputs) msgN |> paste0(msg1, "Checking input values...") |> message()
  inputsList <- list(
    inputName = inNames,
    inputDf   = inputsList,
    valCol    = valCols,
    idCol     = idCols
  ) |>
    pmap(check_input_data) |>
    set_names(inNames)


  ###### Calculate State Population ######
  ### Calculate state population if pop input present and popArea != "state"
  ### Then check values
  # if(hasInputs) msgN |> paste0(msg1, "Checking population values...") |> message()
  df_pop     <- inputsList[["pop"]]
  hasPop     <- df_pop |> length()
  if(hasPop) df_pop <- df_pop |> calc_import_pop(popArea=popArea)
  inputsList[["pop"]] <- df_pop

  ###### Rename List ######
  inputsList <- inputsList |> set_names(outNames)


  ###### Return ######
  ### Return input list
  msgN |> paste0(msg0, "Finished.") |> message()
  return(inputsList)
}

