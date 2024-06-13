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
  fListNames <- c("co_inputScenarioInfo", "co_states", "df_popRatios")
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
  ### Create a list with expected name of column containing values
  valCols    <- list()
  valCols[["temp"]] <- c("temp_C")
  valCols[["slr" ]] <- c("slr_cm")
  valCols[["gdp" ]] <- c("gdp_usd")
  valCols[["pop" ]] <- list()
  valCols[["pop" ]][["national"]] <- c("national_pop")
  valCols[["pop" ]][["conus"   ]] <- c("area_pop")
  valCols[["pop" ]][["regional"]] <- c("reg_pop")
  valCols[["pop" ]][["state"   ]] <- c("state_pop")
  valCols[["pop" ]] <- valCols[["pop"]][[popArea]]

  ### Create a list with expected name of columns used for unique ids
  idCols     <- list()
  idCols[["temp"]] <- c()
  idCols[["slr" ]] <- c()
  idCols[["gdp" ]] <- c()
  idCols[["pop" ]] <- list()
  idCols[["pop" ]][["national"]] <- c()
  idCols[["pop" ]][["conus"   ]] <- c()
  idCols[["pop" ]][["regional"]] <- c("region")
  idCols[["pop" ]][["state"   ]] <- c("state")
  idCols[["pop" ]] <- idCols[["pop"]][[popArea]]

  ###### Initialize Input File Names List ######
  ### Initialize list of file names and
  fList      <- list()
  for(name_i in inNames){fList[[name_i]] <- parse(text=name_i |> paste0("file")) |> eval(); rm(name_i)}

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
    name_i = inputNames,
    file_i = inputsList
  ) |> pmap(function(name_i, file_i){
    ### Messages
    msg_i1 <- paste0("User specified ", name_i |> paste0("file"), "...")
    msg_i2 <- paste0("File does not exist! Returning a null data frame for", name_i |> paste0("Input", "."))
    msg_i3 <- paste0("Importing data from ", file_i, "...")

    ### Initialize data frame as a null value
    df_i   <- NULL

    ### Check if a file name was supplied
    ### - If no file supplied, return NULL
    ### - Otherwise, continue
    has_i  <- file_i |> length()
    if(!has_i) return(df_i)

    ### If a file name was supplied, message user
    ### Get info about the input
    ### Then check if the file exists
    msg1 |> paste0(msg_i1) |> message()
    has_i  <- file_i |> file.exists()

    ### If file does not exist, message user and return NULL
    ### If the file exists, try to load the file
    if(!has_i) {
      msg2 |> paste0(msg_i2) |> message()
      return(df_i)
    } else {
      msg2 |> paste0(msg_i3) |> message()
      try_i  <- file_i |> fun_tryInput(silent=T)
      msg_i4 <- try_i[["fileMsg"]]
      has_i  <- "loaded" %in% data_i[["fileStatus"]]
    } ### End if(!has_i)

    ### Message user, then check if file import was successful
    msg3 |> paste0(msg_i4) |> message()
    if(!has_i) {
      return(df_i)
    } else{
      df_i   <- try_i[["fileInput"]]
    } ### End if(!has_i)

    ### Return df_i
    return(df_i)
  }) |> set_names(inputNames)


  ###### Check the Inputs ######
  ### Message the user
  if(hasInputs) msg1 |> paste0("Checking input values...") |> message()
  ### Use the input info to check the values make sense
  inputsList <- list(
    name_i   = inputNames,
    df_i     = inputsList,
    valCol_i = valCols,
    idCol_i  = idCols
  ) |> pmap(function(
    name_i, df_i, valCol_i, idCol_i,
    df0      = co_inputScenarioInfo
  ){
    ### Messages
    msg_i1    <- paste0("\n", "Checking input values for ", name_i, " inputs...")
    msg_i2    <- paste0("Dropping ", name_i, " inputs from outputs.")
    msg_i3    <- paste0("Data is missing the following required columns: " )
    msg_i4    <- paste0("Data for column ", valCol_i, " is not numeric!")
    msg_i5    <- paste0("Some values for column ", valCol_i, " are outside the range!")

    ### Get info for input i
    info_i    <- co_inputScenarioInfo |> filter(inputName == name_i)
    ### Min and Max Values
    min_i     <- info_i |> pull(inputMin) |> unique()
    max_i     <- info_i |> pull(inputMax) |> unique()
    hasMin_i  <- !(min_i |> is.na())
    hasMax_i  <- !(max_i |> is.na())
    msgRange  <- case_when(hasMin_i ~ ">=" |> paste(min_i), .default="") |>
      paste0(case_when(hasMin_i & hasMax_i ~ " and "), .default="") |>
      paste0(case_when(hasMax_i ~ "<=" |> paste(max_i), .default=""))
    ### Column names
    yearCol_i <- "year"
    # idCol_i   <- case_when("pop" %in% name_i ~ idCol_i[[popArea]], .default=idCol_i)
    cols_i    <- idCol_i |> c(valCol_i) |> c(yearCol_i) |> unique()

    ### Check that data exists
    ### If it does, message user; otherwise, return NULL
    has_i     <- NULL |> nrow() |> length()
    if(has_i) {msg2 |> paste0(msg_i1) |> message()} else {return(NULL)}

    ### If data exists, check for required columns
    dNames_i  <- df_i |> names()
    whichCols <- cols_i %in% dNames_i
    checkCols <- whichCols |> all()
    ### If columns don't pass, message user and return NULL
    ### Otherwise, continue
    if(!checkCols) {
      msg3 |> paste0(msg_i3) |> paste0(dNames_i[whichCols] |> paste(collapse=", "), "!") |> message()
      msg3 |> paste0(msg_i2) |> message()
      return(NULL)
    } ### End if(!checkCols)

    ### If data has required columns, check the value column:
    ### - Check that it is numeric
    ### - If it is numeric, check min and max values
    dataVals  <- df_i |> pull(all_of(valCol_i))

    checkNum  <- dataVals |> is.numeric()
    if(!checkNum) {
      msg3 |> paste0(msg_i4) |> message()
      msg3 |> paste0(msg_i2) |> message()
      return(NULL)
    } ### End if(!checkNum)

    checkYear  <- df_i |> pull(all_of(yearCol_i)) |> is.numeric()
    if(!checkYear) {
      msg3 |> paste0("Data for column year is not numeric!") |> message()
      msg3 |> paste0(msg_i2) |> message()
      return(NULL)
    } ### End if(!checkNum)

    rangeVals <- dataVals |> range(na.rm=T)
    checkVals <- case_when(hasMin_i ~ (dataVals >= min_i) |> all(), .default = T)  &
      case_when(hasMax_i ~ (dataVals >= max_i) |> all(), .default = T)
    if(!checkVals) {
      msg3 |> paste0(msg_i5) |> message()
      msg3 |> paste0("Values for column ", valCol_i, " must be ", msgRange, ".") |> message()
      msg3 |> paste0(msg_i2) |> message()
      return(NULL)
    } ### End if(!checkNum)

    ### If checks pass, convert all id columns to character
    mutate0   <- idCol_i |> (function(x, y="year"){x[!(x %in% y)]})()
    df_i      <- df_i |> mutate_at(c(idCol_i), as.character)

    ### Check regions, states, postal for correct values if pop input present
    doPop      <- "pop" %in% name_i
    if(doPop) {
      df_pop     <- inputsList[["pop"]] |> filter_at(c(valCols), function(x){!(x |> is.na())})
      namesPop   <- df_pop |> names()
      namesState <- co_states |> names()
      checkNames <- namesPop |> (function(x, y=namesState){x[x %in% y]})()
      ### Message user
      msgPop     <- paste0("Checking pop inputs for columns: ") |> paste0(checkNames |> paste(collapse=", "))
      msg1 |> paste0(msgN, msgPop) |> message()
      ### Which levels to check
      doRegions  <- "region" %in% checkNames
      doStates   <- "state"  %in% checkNames
      ### Unique values
      refRegions <- co_states |> pull(region) |> unique()
      refStates  <- co_states |> pull(state ) |> unique()
      ### Select columns
      popData    <- df_pop    |> select(all_of(checkNames)) |> unique()
      popData    <- popData   |> mutate(test = 1)
      ### Join values to check
      co_states  <- co_states |> left_join(popData, by=c(checkNames), relationship="many-to-many")
      newRegions <- co_states |> filter(region |> is.na()) |> pull(region) |> unique()
      newStates  <- co_states |> filter(state  |> is.na()) |> pull(state ) |> unique()
      ### Figure out which regions and states are missing
      naRegions  <- refRegions |> (function(x, y=newRegions){x[!(x %in% y)]})()
      naStates   <- refStates  |> (function(x, y=newStates ){x[!(x %in% y)]})()
      ### Messages
      msgRegions <- paste0("Missing population data for the following regions: ") |> paste0(naRegions |> paste(collapse=", "))
      msgStates  <- paste0("Missing population data for the following states: ") |> paste0(naStates |> paste(collapse=", "))
      ### Check values
      checkReg   <- !(naRegions |> length())
      checkState <- !(naStates  |> length())
      if(!checkReg){
        msg2 |> paste0(msgN, msgRegions) |> message()
        msg2 |> paste0("Dropping pop inputs from outputs.") |> message()
        df_i <- NULL
      } else if(!checkState){
        msg2 |> paste0(msgN, msgStates) |> message()
        msg2 |> paste0("Dropping pop inputs from outputs.") |> message()
        df_i <- NULL
      } ### End if(!checkReg)
    } ### End if(hasPop)

    ### Return
    return(df_i)
  }) |> set_names(inputNames)


  ###### Calculate State Population ######
  ### Calculate state population if pop input present and popArea != "state"
  hasPop     <- inputsList[["pop"]] |> length()
  calcPop    <- hasPop & !("state" %in% popArea)
  if(calcPop) {
    df_pop   <- inputsList[["pop"]]
    namesPop <- df_pop |> names()
    ### Info on popArea
    doArea   <- (namesPop %in% c("national_pop")) |> any()
    doReg    <- (namesPop %in% c("national_pop", "area_pop")) |> any()
    ### Join data with pop ratios
    join0    <- namesPop |> (function(x, y=df_popRatios |> names()){x[x %in% y]})()
    df_pop   <- df_pop   |> left_join(df_popRatios, by=c(join0))
    ### If do area: Calculate area pop from national
    ### If do reg: Calculate region pop from national
    if(doArea){df_pop <- df_pop |> mutate(area_pop = national_pop * area2nat)}
    if(doReg ){df_pop <- df_pop |> mutate(reg_pop  = area_pop     * reg2area)}
    ### Calculate state population
    df_pop   <- df_pop |> mutate(reg_pop  = reg_pop * state2reg)

    ### Select state columns
    select0  <- c("state", "year", "state_pop")
    df_pop   <- df_pop |> select(all_of(select0))
  } ### End if(calcPop)



  ###### Return input list ######
  msg0 |> paste0("Finished.") |> message()
  return(inputsList)
}

