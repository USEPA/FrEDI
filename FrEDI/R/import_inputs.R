#' Import custom scenarios for temperature, global mean sea level rise (GMSL), population, and GDP from user-specified file names
#'
#' @description
#' This function enables users to import data on custom scenarios for use with [FrEDI::run_fredi()]. Users specify path names to CSV files containing temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and state population scenarios. [FrEDI::import_inputs()] reads in and format any specified files as data frames and returns a list of data frames for imported scenarios.
#'
#'
#'
#' @param tempfile=NULL A character string indicating the location of a CSV file containing a custom temperature scenario. The first column in the CSV should be named `"year"` and contain years associated with the temperature estimates; the second column (named `"temp_C"`) should contain values for temperatures (i.e., degrees of warming relative to a baseline year of 1995), in degrees Celsius. The temperature scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param slrfile=NULL A character string indicating the location of a CSV file containing a custom sea level rise scenario. The first column in the CSV should be named `"year"` and contain years associated with the sea level rise estimates; second column should be named "`slr_cm"` and contain values for global mean sea level rise (GMSL), in centimeters, above a baseline year of 2000. The SLR scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param gdpfile=NULL A character string indicating the location of a CSV file containing a custom scenario for U.S. gross domestic product (GDP). The first column in the CSV should be named `"year"` and contains years associated with the GDP estimates; the second column (named `"gdp_usd"`) should contain values for U.S. GDP in 2015 U.S. dollars  (2015$ USD). The GDP scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]). Values for GDP in column `"gdp_usd"` must be greater than or equal to zero.
#' @param popfile=NULL A character string indicating the location of a CSV file containing a custom population scenario for states and NCA regions. The first column should be named `"year"` and contain years in the interval 2010 to 2300. The second column should be called `"pop"` and contain population values. Whether a third column is required depends on the geographical scale of the data, as specified by the `popArea` argument (for more information, see the `popArea` argument, below.
#'    * If `popArea = "state"`, the third column should be called `"state"` and contain the name of the state associated with each population estimate in a given year. If `popArea = "regional"`, the third column should be called `"region"` and contain the NCA Region label associated with each population estimate in a given year. If `popArea = "national"`, only the `year` and `pop` columns are required.
#'    * If `popArea = "state"`, the file must contain estimates for all CONUS states -- i.e., all states except Alaska and Hawaii must be present in the input file (estimates for Alaska and Hawaii are optional). If `popArea = "regional"`, the file must contain estimates for all seven NCA regions.
#'    * The population scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]). Values for population in column `"pop"` must be greater than or equal to zero.
#' @param temptype="conus" A character string indicating whether the temperature values in the temperature input file (as specified by `tempfile`) represent temperature changes, in degrees Celsius, at the global level (`temptype = "global"`) or for the contiguous U.S. (CONUS) only (`temptype = "conus"`, default).
#' @param popArea="state" A character string indicating the geographical scale of population inputs (as specified by `popfile`). Specify one of: `"state"`, for state-level population (default); `"conus"`, for the contiguous U.S. (CONUS) only (i.e., national totals, but excluding Alaska and Hawaii); or `"national"`, for national totals (CONUS population, including Alaska and Hawaii).
#'
#'
#'
#' @details
#' This function enables users to import data on custom scenarios for use with temperature binning. Users specify path names to CSV files containing temperature, global mean sea level rise (GMSL), population, and gross domestic product (GDP) scenarios (`tempfile`, `slrfile`, `gdpfile`, and `popfile`, respectively). [FrEDI::import_inputs()] reads in and formats any specified files as data frames and returns a list of data frames for imported scenarios.
#'
#' The CSV files should contain estimates aligned with the requirements of the `run_fredi()` or ``
#'
#' * __Temperature Scenario.__ The file specified by `tempfile` must have two columns -- `"year"` and `"temp_C"` -- respectively containing the years associated with the temperature estimates and the temperatures (i.e., degrees of warming) in degrees Celsius relative to a baseline year of 1995 (i.e., the central year of a 1986-2005 reference period).
#'    * If values in `"temp_C"` have global rather than CONUS-specific temperatures, users should specify `temptype = "global"` when running `import_inputs()`, and `import_inputs()` will convert the temperatures to CONUS using [FrEDI::convertTemps] (with argument `from = "global"`).
#'    * Temperature inputs to [FrEDI::run_fredi()] must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __SLR Scenario.__ The file specified by `slrfile` must have two columns -- `"year"` and `"slr_cm"` -- respectively containing the years associated with the GMSL estimates and GMSL estimates, in centimeters, above a 2000 baseline year.
#'    * GMSL heights must be greater than or equal to zero.
#'    * The SLR scenario must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __GDP Scenario.__ The file specified by `gdpfile` must have two columns -- `"year"` and `"gdp_usd"` -- respectively containing the years associated with the GDP estimates and the estimates for U.S. GDP, in 2015$.
#'    * GDP values must be greater than or equal to zero.
#'    * The GDP scenario must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __Population Scenario.__ The file specified by `popfile` population must have at least two columns -- `"year"` and `"pop"` -- respectively containing the years associated with the population estimates and the population estimates. requires state-level population values. Whether a third column is required depends on the geographical scale of the data, as specified by the `popArea` argument.
#'    * If `popArea = "state"`, the third column should be called `"state"` and contain the name of the state associated with each population estimate in a given year. If `popArea = "regional"`, the third column should be called `"region"` and contain the NCA Region label associated with each population estimate in a given year. If `popArea = "national"`, only the `year` and `pop` columns are required.
#'    * If `popArea = "state"`, the file must contain estimates for all CONUS states -- i.e., all states except Alaska and Hawaii must be present in the input file (estimates for Alaska and Hawaii are optional). If `popArea = "regional"`, the file must contain estimates for all seven NCA regions.
#'    * The population scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#'    * Population values must be greater than or equal to zero.
#'    * Population estimates must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#'
#' [FrEDI::run_fredi()] requires a population scenario at the state level. If the population scenario is supplied to [FrEDI::import_inputs()] at a geographical scale above the state level (i.e., if `popArea = "national"`, `popArea = "area"`, or `popArea = "regional"`), [FrEDI::import_inputs()] will calculate state-level estimates from the provided data:
#'
#' * If `popArea = "national"`, [FrEDI::import_inputs()] will use historical U.S. Census data for the period 2010--2023 to allocate total national population to CONUS and non-CONUS regions (i.e., Alaska and Hawaii); U.S. Census values from 2023 are applied in allocations for years after 2023. [FrEDI::import_inputs()] then uses ICLUS data and projections for the period 2010--2100 to allocate CONUS population to specific NCA regions; ICLUS values from 2100 are applied in allocations for years after 2100.
#' * If `popArea = "national"` or `popArea = "regional"`, [FrEDI::import_inputs()] uses ICLUS data and projections for the period 2010--2100 to allocate population for each NCA region to the associated states; ICLUS values from 2100 are applied in allocations for years after 2100.
#'
#'
#' [FrEDI::import_inputs()] outputs a list of data frame objects that can be passed to the main FREDI function [FrEDI::run_fredi()] using the `inputList` argument. For example, specify `run_fredi(inputsList = x)` to generate impacts for a custom scenario `x` (where `x` is a list of data frames such as that output from [FrEDI::import_inputs()]) (see [FrEDI::run_fredi()]).
#'
#' All inputs to [FrEDI::import_inputs()] are optional. If the user does not specify a file path for `tempfile`, `slrfile`, `gdpfile`, or `popfile` (or if there is are any errors reading in inputs from a file path), [FrEDI::import_inputs()] outputs a list with a `NULL` value for the associated list element. After reading in data from the specified CSV files, [FrEDI::import_inputs()] performs basic checks and will also return a `NULL` value for a particular list element if any of the checks fail.
#'
#' When using [FrEDI::import_inputs()] with [FrEDI::run_fredi()], [FrEDI::run_fredi()] defaults back to the default scenarios for any elements of the inputs list that are `NULL` or missing. In other words, running `run_fredi(inputsList = list())` returns the same outputs as running [FrEDI::run_fredi()] (see [FrEDI::run_fredi()]).
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
#' `popInput` \tab Data frame containing a custom temperature scenario imported from the CSV file specified by `popfile`, with missing values removed. `popInput` has three columns with names `"year"`, `"region"`, `"state"`, `"postal"`, and `"state_pop"`, containing the year, the NCA region name, and the state, the postal code abbreviation (e.g., `postal = "ME"` for `state = "Maine"`), and the state population, respectively. \cr
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
#' example_inputsList <- import_inputs(slrfile=slrInputFile, popfile=popInputFile, popArea="state")
#'
#' ### Use imports with FREDI:
#' df_x <- run_fredi(inputsList=example_inputsList)
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#' United Nations. 2015. World population prospects: The 2015 revision. New York: United Nations, Department of Economic and Social Affairs, Population Division.
#'
#' U.S. Census Bureau. 2021. State Population Totals and Components of Change: 2010--2019. Available at <https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html>.
#'
#' U.S. Census Bureau. 2023. State Population Totals and Components of Change: 2020--2023. Available at <https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html>.
#'
#' U.S. Census Bureau. 2023. 2023 National Population Projections Tables: Main Series. Available at <https://www.census.gov/data/tables/2023/demo/popproj/2023-summary-tables.html>.
#'
#' @export
#' @md
#'
#'

###### import_inputs ######
### This function imports data from user-specified file names.
import_inputs <- function(
  tempfile = NULL, ### File path of CSV with temperature inputs
  slrfile  = NULL,
  gdpfile  = NULL,
  popfile  = NULL,
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
  if(!(df_pop |> is.null())) {
    ### Rename state_pop columne
    doRename <- "state_pop" %in% (data1 |> names())
    if(doRename) {
      rename0  <- c("state_pop")
      renameTo <- c("pop")
      df_pop   <- df_pop |> rename_at(c(rename0), ~renameTo)
    } ### End if(doRename)
    ### Update in output
    inputsList[["pop"]] <- df_pop
  } else {
    inputsList[["pop"]] <- list(NULL)
  } ### End if(!(df_pop |> is.null()))

  ###### Rename List ######
  inputsList <- inputsList |> set_names(outNames)


  ###### Return ######
  ### Return input list
  msgN |> paste0(msg0, "Finished.") |> message()
  return(inputsList)
}

