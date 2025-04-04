###### get_msg_prefix ######
### Function to get message prefix
get_msg_prefix <- function(
    level  = 0,
    prefix = ""
){
  msgP    <- "\t"   |> rep(level) |> paste(collapse="")
  msgP    <- prefix |> paste0(msgP)
  return(msgP)
}


## Initial Functions ----------------
### Info about which column contains info on region
# get_import_inputs_idCols
get_inputsIDCols <- function(
    type0   = "temp", ### c("gdp", "pop", "temp", "slr", "ch4", "nox", "o3"),
    doReg0  = FALSE,
    doPop0  = FALSE,
    doO3    = FALSE,
    popArea = "state" ### One of: c("state", "regional", "area", "national")
){
  ### Initialize list
  cols0 <- c()
  ### If doReg
  if(doReg0) {
    ### If doPop0, get popArea
    if(doPop0) {
      doState <- popArea |> str_detect("state")
      cols0   <- doState |> ifelse("state", "region")
    } else {
      cols0   <- "state"
    } ### End if(doPop0)

    ### If o3, add model
    if(doO3) {
      cols0   <- cols0 |> c("model")
    } ### End if(doO3)
  } ### End if(doReg0)

  ### Add year
  cols0 <- cols0 |> c("year")

  ### Return
  return(cols0)
}


### Get default scenarios
get_dfInputInfo <- function(
    module0 = "fredi",
    mTypes0 = c("gcm", "slr")

){
  ###
  dfInfo <- controlData[["co_moduleScenarios"]] |>
    filter(module %in% module0) |>
    rename(inputName = inputType) |>
    left_join(controlData[["co_inputInfo"]], by="inputName")
  ### Get list of input names
  doSlr0       <- mTypes0 |> tolower() |> str_detect("slr")
  if(!doSlr0) dfInfo <- dfInfo |> filter(!(inputName %in% "slr"))
  return(dfInfo)
}

### Add info to data
get_defaultScenarios <- function(
    dfInfo,
    mTypes0 = c("gcm", "slr"),
    minYr0  = 2010,
    maxYr0  = 2100,
    yrCol0  = "year"
){
  ### Get list of input names
  inputNames0  <- dfInfo  |> pull(inputName)

  ### Drop SLR if that is not present
  if(!doSlr0) inputNames0 |> get_matches("slr", matches=F)
  doSlr0       <- mTypes0 |> tolower() |> str_detect("slr")

  ### Get list of defaults
  inputDefs    <- inputNames0 |> map(function(name0){
    ### Conditionals
    doTemp  <- name0 |> str_detect("temp")
    doSlr   <- name0 |> str_detect("slr")
    ### Info
    info0   <- co_inputInfo |> filter(inputName %in% name0)
    idCol0  <- info0 |> pull(idCol0)
    scen0   <- info0 |> pull(scenarioName)
    ### Filter scenario
    df0     <- get_frediDataObj(name0, "scenarioData") |> filter_at(c(idCol0), function(x, y=scen0){x %in% y})
    # if(doTemp) df0 <- df0 |> filter(scenario %in% "ECS_3.0_REF")
    reg0    <- info0 |> pull(regional)
    yCol0   <- info0 |> pull(valueCol)
    idCols0 <- yrCol0
    if(doTemp   ) yCol0   <- yCol0 |> paste0("_", c("conus", "global"))
    if(reg0 == 1) idCols0 <- c("region", "state", "postal") |> c(idCols0)
    cols0   <- idCols0 |> c(yrCol0, yCol0)
    ### Select columns
    df0     <- df0 |>
      select(all_of(cols0)) |>
      filter(year <= maxYr0)
    ### If hasMin0, filter min
    hasMin0 <- !(minYr0 |> is.null())
    if(hasMin0) df0 <- df0 |> filter(year >= minYr0)
    ### Return
    return(df0)
  }) |> set_names(inputNames0)

  ### Return
  return(inputDefs)
}


### Check and format inputs list
format_inputsList <- function(
    dfInfo0, ### Outputs of get_dfInputInfo
    inputsList = list(temp=NULL, slr=NULL, pop=NULL, gdp=NULL),
    # maxYr0     = 2100,
    tempType   = "conus",
    popArea    = "state",
    msg0       = 0
){
  ### Messaging
  msgN       <- "\n"
  ### Figure out which inputs are not null, and filter to that list
  ### Get input names that match inNames0 and filter to values that are not null
  inNames0   <- dfInfo     |> pull(inputName)
  inNames    <- inNames0   |> get_matches(y=inputsList |> names())
  notNull    <- inNames    |> map(function(name0){!(inputsList[[name0]] |> is.null())}) |> unlist() |> which()
  ### Filter values and update inNames
  inputsList <- inputsList[inNames][notNull]
  inNames    <- inputsList |> names()
  dfInfo     <- dfInfo     |> filter(inputName %in% inNames)
  inNames    <- dfInfo     |> pull(inputName)
  inputsList <- inputsList[inNames]
  ### Check if there are values
  nInputs    <- inputsList |> length()
  nNames     <- inNames    |> length()
  if (!nNames) {
    paste0(msg0) |> paste0("Error! `inputsList` argument requires a list with named elements.") |> message()
    msgN |> paste0(msg0) |> paste0("Exiting...") |> message()
    return()
  } else if(!nInputs){
    return(inputsList)
  } ### End if(!hasInputs)


  ### Check Inputs
  ### Filter to valid inputs & get info
  ### Reorganize inputs list
  old0       <- c("inputName", "minYear", "maxYear", "inputMin", "inputMax", "regional", "valueCol")
  new0       <- c("type0", "minYr0", "maxYr0", "inputMin0", "inputMax0", "doReg0", "valCol0")
  cols0      <- new0 |> c("doPop0", "doO3", "popArea")
  dfInfo0    <- dfInfo0 |>
    select(all_of(cols0)) |>
    mutate(regional = regional == 1) |>
    mutate(doTemp0  = inputName |> str_detect("temp")) |>
    mutate(doPop0   = inputName |> str_detect("pop")) |>
    mutate(doO3     = inputName |> str_detect("o3")) |>
    mutate(tempType = case_when(doTemp0 ~ tempType, .default = NA)) |>
    mutate(popArea  = case_when(doPop0  ~ popArea , .default = NA)) |>
    rename_at(c(old0), ~c(new0))

  ### Create logicals and initialize inputs list
  idCols0    <- dfInfo |>
    select(c("type0", "doReg0", "doPop0", "doO3", "popArea")) |>
    pmap(get_inputsIDCols) |>
    set_names(inNames0)

  ### Check inputs
  # old0       <- c("valueCol", "minYear", "maxYear")
  # new0       <- c("valCol", "yearMin", "yearMax")
  inputsList <- dfInfo |>
    # select(-c("doReg0")) |>
    as.list() |>
    c(list(idCols  = idCols0   )) |>
    c(list(inputDf = inputsList)) |>
    pmap(check_inputData, module=module0) |>
    set_names(inNames)

  ### Check which are not null
  notNull    <- inputsList |> map(function(obj0){!(obj0 |> is.null())}) |> unlist() |> which()
  inputsList <- inputsList[notNull]

  ### Return inputs list
  return(inputsList)
}




## Functions to Load Inputs ----------------
### This function attempts to load a user-specified input file
fun_tryInput <- function(
    inputName = "temp",
    filename  = NULL,
    silent    = FALSE, ### Whether to message
    msg0      = 0     ### Message prefix
){
  ### Set Up Environment
  #### Messaging
  msgUser  <- !silent
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  msg3     <- msg0 + 3

  #### Columns and values
  nullName <- filename |> is.null()

  #### Initialize results
  list0    <- list()
  df0      <- NULL
  exists0  <- FALSE
  isDf0    <- FALSE

  ### Check if the file exists and try to load the file
  ### Set input to null if it doesn't exist
  if(nullName) {
    msg0 |> get_msg_prefix() |> paste0("No filename provided. Exiting...") |> message()
    return()
  } ### End if(nullName)

  ### If file name exists, check file
  msg0 |> get_msg_prefix() |> paste0("User specified ", inputName |> paste0("file"), "...")
  exists0  <- filename |> file.exists()
  ### If the file exists, try loading the file and then check the class of the result
  if(exists0) {
    df0    <- filename |> read.csv(check.names=F) |> try(silent=T)
    class0 <- df0 |> class()
    isDf0  <- "data.frame" %in% class0

    ### If loading the inputs was successful
    if(isDf0){
      list0[["fileInput" ]] <- df0
      list0[["fileStatus"]] <- "loaded"
      list0[["fileMsg"   ]] <- "Data loaded successfully."
    } else{
      list0[["fileInput" ]] <- NULL
      list0[["fileStatus"]] <- "other-error"
      list0[["fileMsg"   ]] <- "Not able to read CSV data. Returning null value..."
    } ### End if(inputExists)
  }  else{
    list0[["fileInput" ]] <- NULL
    list0[["fileStatus"]] <- "no-file"
    list0[["fileMsg"   ]] <- "Input file does not exist. Returning null value..."
  } ### End if(fileExists)

  ### Message the user
  # message("\t", list0[["fileMsg"]])
  # msgLast <- list0[["fileMsg"]]
  # if(msgUser)
  msg1 |> get_msg_prefix() |> paste0(list0[["fileMsg"]]) |> message()
  df0      <- list0[["fileInput" ]]

  ### Return
  # return(list0)
  return(df0)
}


###### Deprecated: run_fun_tryInput ######
# ### Function to iterate over a list of file names
# run_fun_tryInput <- function(
#     inputName = "temp",      ### Type of input; one of: c("temp", "slr", "gdp", "pop")
#     fileName  = "temps.csv", ### File path and name of CSV to be imported
#     msgLevel  = 1
# ){
#   ### Set Up Environment
#   #### Messaging
#   msgUser  <- !silent
#   msgN     <- "\n"
#   msg0     <- msgLevel
#   msg1     <- msg0 + 1
#   msg2     <- msg0 + 2
#   msg3     <- msg0 + 3
#
#   #### Columns and values
#   msgN   <- "\n"
#   msg0_i <- get_msg_prefix(level=msgLevel)
#   msg1_i <- get_msg_prefix(level=msgLevel + 1)
#   msg2_i <- get_msg_prefix(level=msgLevel + 2)
#   msg_i1 <- paste0("User specified ", inputName |> paste0("file"), "...")
#   msg_i2 <- paste0("File does not exist! Returning a null data frame for ", inputName |> paste0(" input", "."))
#   msg_i3 <- paste0("Importing data from ", fileName, "...")
#
#   ### Initialize data frame as a null value
#   df0      <- NULL
#
#   # ### Check if a file name was supplied
#   # ### - If no file supplied, return NULL
#   # ### - Otherwise, continue
#   # hasName0 <- fileName |> length()
#   # if(!has_i) return(df_i)
#
#   ### If a file name was supplied, message user
#   ### Get info about the input
#   ### Then check if the file exists
#   msg0_i |> paste0(msg_i1) |> message()
#   has_i  <- fileName |> file.exists()
#
#   ### If file does not exist, message user and return NULL
#   ### If the file exists, try to load the file
#   if(!has_i) {
#     msg1_i |> paste0(msg_i2) |> message()
#     return(df_i)
#   } else {
#     msg1_i |> paste0(msg_i3) |> message()
#     try_i  <- fileName |> fun_tryInput(silent=T)
#     msg_i4 <- try_i[["fileMsg"]]
#     has_i  <- "loaded" %in% try_i[["fileStatus"]]
#   } ### End if(!has_i)
#
#   ### Message user, then check if file import was successful
#   msg1_i |> paste0(msg_i4) |> message()
#   if(!has_i) return(df_i)
#   else       df_i <- try_i[["fileInput"]]
#
#   ### Return df_i
#   return(df_i)
# }



## check_inputs
### Check Input Ranges
### If input range is outside the range, return "flag" and row numbers of flagged values
### If input range is all inside the range, return "allgood"
check_inputs <- function(
    x,           ### Vector of data
    xmin  = NULL, ### Minimum of range
    xmax  = NULL, ### Maximum of range
    in0   = FALSE ### Whether we want values that match the conditions or not
){
  ### Initialize flagged rows
  maxFlags <- minFlags <- c()
  flagList <- list()
  msgList0 <- list()

  ### Check whether xmin/xmax present
  ### - xmin
  nullMin  <- xmin |> is.null()
  naMin    <- nullMin |> ifelse(T, xmin |> is.na())
  hasMin   <- nullMin & naMin & xmin |> length()
  minFlags <- c()
  checkMin <- T
  ### - xmax
  nullMax  <- xmax |> is.null()
  naMax    <- nullMax |> ifelse(T, xmax |> is.na())
  hasMax   <- nullMax & naMax & xmax |> length()
  maxFlags <- c()
  checkMax <- T

  ### Messages
  ### - When in0 is FALSE
  msgList0[["min"]] <- case_when(hasMin ~ in0 |> ifelse(">=", "<=") |> paste(xmin), .default="")
  msgList0[["and"]] <- case_when(hasMin & hasMax ~ " and ", .default="")
  msgList0[["max"]] <- case_when(hasMax ~ in0 |> ifelse("<=", ">=") |> paste(xmax), .default="")
  flagList[["message"]] <- msgList0

  ### If all are null
  hasAny   <- hasMin | hasMax
  if(!hasAny) {
    flagList[["check"]] <- T
    return(flagList)
  } ### End if(!hasAny)

  ### Check which observations are below the minimum
  ### Get range
  xRange   <- x |> range(na.rm=T)
  if(hasMin) {
    if(in0) minFlags <- (x >= xmin) |> which()
    else    minFlags <- (x <= xmin) |> which()
    checkMin <- minFlags |> length()
  } ### End if(hasMin)

  ### Check which observations are above the maximum
  if(hasMax) {
    if(in0) maxFlags <- (x <= xmax) |> which()
    else    maxFlags <- (x >= xmax) |> which()
  } ### End if(hasMax)

  ### Combine Any Flagged Rows
  ### Summarize Flag Status
  ### Update in list
  check0   <- checkMin & checkMax
  flagRows <- minFlags |> c(maxFlags)
  hasFlags <- flagRows |> length()
  flagList[["check"  ]] <- check0
  flagList[["flagged"]] <- hasFlags
  flagList[["rows"   ]] <- flagRows

  ### Return List
  # flagList   <- list(flagged=hasFlags, rows=flagRows)
  return(flagList)
}


## check_pop_regions
### Check that all regions, states, present in data
check_regions <- function(
    df0,                ### Tibble with population info
    type0  = "pop"  , ### Label for type of input
    module = "fredi", #### "fredi", "sv", or "methane"
    msg0   = 0        ### Level of messaging
){
  ### Set Up the Environment ----------------
  #### Messaging ----------------
  sep0       <- ", "
  msgN       <- "\n"
  # msg0       <- msgLevel
  msg1       <- msg0 + 1
  msg2       <- msg0 + 2
  msg3       <- msg0 + 3
  msg0 |> get_msgPrefix(newline=T) |> paste0("Checking regions, states in ", type0 , "file data...") |> message()

  #### Columns & Values ----------------
  #### Columns
  yrCol0     <- "year"
  areaCol0   <- "area"
  regCol0    <- "region"
  stateCol0  <- "state"
  postCol0   <- "postal"
  orderCol0  <- "state_order"
  lblStr0    <- "_label"
  colIds0    <- c(areaCol0, regCol0)
  regCols0   <- c(colIds0, stateCol0, postCol0, orderCol0)
  regIdCols0 <- colIds0 |> paste0(lblStr0) |> c(regCols0)

  ### Values
  names0     <- df0 |> names()
  checkCols0 <- c()
  doReg0     <- names0 |> str_detect("region")
  doState0   <- names0 |> str_detect("state" )
  ### Standardize region, if present
  if(doRegion) df0 <- df0 |> mutate(region = region |> str_replace_all("_|\\.", " "))
  if(doReg0  ) checkCols0 <- checkCols0 |> c(regCol0  )
  if(doState0) checkCols0 <- checkCols0 |> c(stateCol0)


  #### Module Options ----------------
  module0    <- module |> tolower()
  areas0     <- get_frediDataObj("co_moduleAreas", "controlData") |>
    filter_at(c("module"), function(x, y=module){x %in% y}) |>
    pull(all_of(areaCol0))
  co_states  <- get_frediDataObj("co_states", "controlData") |>
    filter_at(c(areaCol0), function(x, y=areas0){x %in% y}) |>
    # select(-any_of(area))
    select(any_of(regIdCols0))

  ### Format Data ----------------
  ### Drop missing values and rename column
  join0      <- names0  |> get_matches(co_states)
  old0       <- colIds0 |> get_matches(join0)
  df0        <- df0 |>
    filter_all(all_vars(!(. |> is.na()))) |>
    rename_at(c(old0), ~old0 |> paste0(lblStr0))

  ### Join data into states
  df0        <- co_states |>
    left_join(df0, by=join0) |>
    arrange_at(c(orderCol0, yrCol0)) |>
    select(-any_of(colIds0)) |>
    rename_at(c(old0 |> paste0(lblStr0)), ~old0)

  ### Check Data ----------------
  ### Get any NA values
  dfNA       <- df0 |>
    filter_all(all_vars(. |> is.na())) |>
    select(any_of(regIdCols0)) |>
    distinct()
  ### Missing states, regions
  naRegions0 <- dfNA |> pull(all_of(regCol0  )) |> unique()
  naStates0  <- dfNA |> pull(all_of(stateCol0)) |> unique()
  ### Conditionals
  allRegs0   <- !(naRegions0 |> length())
  # allStates0 <- !(naStates0  |> length())
  if(!allRegs0) {
    msgRegs0   <- paste0("Missing data for the following regions ", naRegions0 |> paste(collapse=sep0), "...")
    msgStates0 <- paste0("Missing data for the following states ", naStates0 |> paste(collapse=sep0), "...")
    msg1 |> get_msgPrefix(newline=T) |> paste0(msgRegs0  ) |> message()
    msg1 |> get_msgPrefix(newline=T) |> paste0(msgStates0) |> message()
    msg2 |> get_msgPrefix(newline=T) |> paste0("Returning an empty dataset...") |> message()
    return()
  } else{
    msg1 |> get_msgPrefix(newline=T) |> paste0("All states and regions present and have some non-missing values...") |> message()
  } ### End if(!allRegs0)

  ### Filter to non-NA values
  df0        <- df0 |>
    filter_all(all_vars(!(. |> is.na()))) |>
    select(-any_of(colIds0)) |>
    rename_at(c(old0 |> paste0(lblStr0)), ~old0)

  ### Return ----------------
  return(df0)
}



## check_valCols
### Check value columns
check_valCols <- function(
    # inputDf,   ### Data frame of values
    inputName = "temp", ### Input name c("temp", "slr", "gdp", "pop")
    valCol0   = "temp_C", ### Value column
    doTemp0   = TRUE,
    doPop0    = FALSE,
    # argVal0 = NA,
    tempType  = "conus", ### One of: c("conus", "global")
    popArea   = "state", ### One of: c("state", "regional", "conus", "national")
    inputDf,   ### Data frame of values
    msg0      = 0
    # msgLevel  = 2
) {
  ### Set Up the Environment ----------------
  #### Messaging ----------------
  sep0       <- ", "
  msgN       <- "\n"
  # msg0       <- msgLevel
  msg1       <- msg0 + 1
  msg2       <- msg0 + 2
  msg3       <- msg0 + 3
  msg0 |> get_msgPrefix(newline=F) |> paste0("Checking for column '", valCol0, "' in ", inputName, " data...") |> message()

  #### Columns & Values ----------------
  #### Conditionals
  doTemp0    <- "temp" %in% inputName
  doPop0     <- "pop"  %in% inputName

  ### Initial names and values
  valColRef <- valCol0
  inNameRef <- inputName
  namesRef  <- inputDf |> names()
  ### Convert to lowercase
  valColLC0 <- valCol0   |> tolower()
  inNameLC0 <- inputName |> tolower()
  namesLC0  <- namesRef  |> tolower()


  ### Get matches ----------------
  ### Match values
  colMatch0 <- valColRef |> get_matches(namesRef, type="matches")
  lcMatch0  <- valColLC0 |> get_matches(namesLC0, type="matches")
  strMatch0 <- namesLC0  |> str_detect(inNameLC0, type="matches")
  if     (doTemp0) argMatch0 <- namesLC0  |> str_detect(tempType)
  else if(doPop0 ) argMatch0 <- namesLC0  |> str_detect(popArea )
  else             argMatch0 <- T
  # inputDf   <- inputDf   |> set_names(namesDf)

  ### Check if there is a match
  colMatchN <- colMatch0 |> which() |> length()
  lcMatchN  <- lcMatch0  |> which() |> length()
  strMatchN <- strMatch0 |> which() |> length()
  argMatchN <- argMatch0 |> which() |> length()

  ### Which match ----------------
  ### Check if there are any string matches, and get number of matches
  msgStrCol <- "Column '"
  msgStrQuo <- "' "
  msgStrNot <- "' not found in "
  msgStrFnd <- "' found in "
  msgStrInp <- inNameRef |> paste0(" file data...")
  msgCol0   <- paste0(msgStrCol, valCol0, msgStrQuo, inNameRef, msgStrInp)
  msgNCol0  <- paste0("Column '", valCol0, "' not found in ", inNameRef, "file data...")
  if(colMatchN) {
    msg1 |> get_msgPrefix() |> paste0(msgStrCol, valCol0, msgStrFnd, "...") |> message()
    matchCol0 <- namesRef[colMatch0]
  } else {
    msg1 |> get_msgPrefix() |> paste0(msgStrCol, valCol0, msgStrNot, "...") |> message()
    msg1 |> get_msgPrefix() |> paste0("Looking for column '", valColLC0, "'...") |> message()
    if(lcMatchN) {
      msg1 |> get_msgPrefix() |> paste0(msgStrCol, valColLC0, msgStrFnd, "...") |> message()
      matchCol0 <- namesRef[lcMatch0]
    } else {
      msg1 |> get_msgPrefix() |> paste0(msgStrCol, valColLC0, msgStrNot, "...") |> message()
      msg1 |> get_msgPrefix() |> paste0("Looking for column names matching string '", inNameLC0, "'...") |> message()
      if (strMatch0) {
        msg1 |> get_msgPrefix() |> paste0("Column name(s) matching string ", inNameLC0, msgStrFnd, "...") |> message()
        if(strMatchN == 1) {
          matchCol0 <- namesRef[strMatch0]
        } else if(strMatchN > 1) {
          msg2 |> get_msgPrefix() |> paste0("Multiple matches found...")
          matches0 <- strMatch0 & argMatch0
          matchesN <- matches0 |> which() |> length()
          if     (matchesN == 1) {matchCol0 <- namesRef[matches0]}
          else if(matchesN >  1) {matchCol0 <- namesRef[matches0[1]]}
          else                   {matchCol0 <- namesRef[strMatch0][1]}
        } else {
          msg1 |> get_msgPrefix() |> paste0("No column names matching string '", inNameLC0, "' found...") |> message()
          # msg1 |> get_msgPrefix() |> paste0("Returning NULL value...") |> message()
          msg0 |> get_msgPrefix() |> paste0("No matches found! Exiting...") |> message()
          return()
        } ### End if(strMatchN == 1)
        ### Get info on argument
        # if     (doTemp0) tempType <- matchCol0  |> str_detect(tempType)
        # else if(doPop0 ) popArea  <- matchCol0  |> str_detect(popArea )
      } ### End if(strMatch0)
    } ### End if(lcMatchN)
    msg2 |> get_msgPrefix() |> paste0("Using column '", matchCol0, "'...")
  } ### End if(colMatchN)

  ### Return List ----------------
  ### Rename columns
  ### Provide info to list
  rename0   <- !(matchCol0 == valColRef)
  if(rename0) {
    msg1 |> get_msgPrefix() |> paste0("Renaming column '", matchCol0, "' to '", valColRef, "'...") |> message()
    inputDf   <- inputDf |> rename_at(c(matchCol0), ~valColRef)
  } ### End if(rename0)

  # list0     <- list()
  # list0[["inputDf" ]] <- inputDf
  # list0[["valCol0" ]] <- valColRef
  # list0[["tempType"]] <- tempType
  # list0[["popArea" ]] <- popArea
  # return(list0)

  ### Return ----------------
  return(inputDf)

}


## Check Input Data ----------------
### Check input data...newer version of check_inputs
### check_input_data
check_inputData <- function(
    type0     = "temp",   ### Type of input; one of: c("temp", "slr", "gdp", "pop")
    minYr0   , ### Min year
    maxYr0   , ### Max year
    inputMin0, ### Min value
    inputMax0, ### Max value
    doReg0    = FALSE,
    doTemp0   = TRUE,
    doPop0    = FALSE,
    doO3      = FALSE,
    tempType  = "conus",  ### One of: c("conus", "global")
    popArea   = "state",  ### One of: c("state", "regional", "conus", "national")
    valCol0  ,     ### E.g., c("temp_C", "slr_cm", "gdp_usd", "state_pop") ### Or "reg_pop", "area_pop", or "national_pop", depending on popArea
    idCols0  ,     ### E.g., "state" or "region" if popArea is "state" or "region", respectively; empty character (i.e., c()) otherwise
    inputDf   = NULL,     ### Tibble of inputs (e.g., as output from run_fun_tryInput)
    module0   = "fredi",  #### "fredi", "sv", or "methane"
    msg0      = 0 ### Level of messaging
    # msgLevel  = 2         ### Level of messaging
){
  # inputDf |> glimpse()
  ### Set Up the Environment ----------------
  #### Messaging ----------------
  sep0       <- ", "
  msgN       <- "\n"
  # msg0       <- msgLevel
  msg1       <- msg0 + 1
  msg2       <- msg0 + 2
  msg3       <- msg0 + 3

  #### Columns & Values ----------------
  #### Columns
  yrCol0      <- "year"
  #### Conditionals
  # doTemp    <- "temp" %in% type0
  # doPop     <- "pop"  %in% type0
  # doO3      <- "o3"   %in% type0
  # doTemp |> print()

  #### Module Options ----------------
  module0    <- module0 |> tolower()
  dListSub0  <- module0 |> paste0("Data")
  cDataStr0  <- "controlData"

  ### Check Data ----------------
  #### NULL Data ----------------
  ### Check that data exists
  ### If it does not, return NULL
  nullData   <- inputDf |> is.null()
  if(nullData) return()

  #### Check Regions ----------------
  if(doReg0) {
    inputDf  <- inputDf |> check_regions(type0=type0, module=module, msg0=msg1)
    nullData <- inputDf |> is.null()
    if(nullData) return()
  } ### End if(doPop | doO3)

  #### Check for Value Column ----------------
  inputDf   <- type0 |> check_valCols(
    valCol0  = valCol0, ### Value column
    doTemp0  = doTemp0,
    doPop0   = doPop0,
    tempType = tempType, ### One of: c("conus", "global")
    popArea  = popArea , ### One of: c("state", "regional", "conus", "national")
    inputDf  = inputDf,   ### Data frame of values
    msg0     = msg1
  ) ### End check_valCols
  nullData   <- inputDf |> is.null()
  if(nullData) return()

  #### Check for Other Columns ----------------
  othCols0  <- c(idCols0, yrCol0) |> unique()
  matches0  <- othCols0 |> get_matches(inputDf |> names(), type="matches")
  naCols0   <- othCols0[!matches0]
  if(naCols0) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Data is missing the following required columns: " ) |> message()
    msg2 |> get_msgPrefix(newline=F) |> paste0("'", naCols0 |> paste(collapse="', '"), "'...") |> message()
    msg2 |> get_msgPrefix(newline=F) |> paste0("Exiting...") |> message()
    return()
  } else{
    msg1 |> get_msgPrefix(newline=F) |> paste0("All required columns present." ) |> message()
  } ### End if(naCols0)
  rm(matches0, naCols0)

  #### Check Numeric ----------------
  msg1 |> get_msgPrefix(newline=F) |> paste0("Checking that columns 'year', '", valCol0, "', are numeric..." ) |> message()
  numCols0  <- c(yrCol0, valCol0)
  isNum0    <- numCols0 |> map(function(colX){inputDf |> pull(all_of(colX)) |> is.numeric()}) |> unlist()
  naNum0    <- paste0("'", numCols0[!isNum0] |> paste(collapse="', '"), "'")
  if((!isNum0) |> any()) {
    msg2 |> get_msgPrefix(newline=F) |> paste0("Warning: Column(s) '",naNum0, "' must be numeric!" ) |> message()
    msg2 |> get_msgPrefix(newline=F) |> paste0("Exiting...") |> message()
    return()
    # msg2 |> get_msgPrefix(newline=F) |> paste0("Converting column(s) '",naNum0, "' to numeric..." )
    # msg3 |> get_msgPrefix(newline=F) |> paste0("This may result in the conversion of some values to NA..." )
  } ### End if(!isNum0)
  rm(isNum0, naNum0)

  #### Filter Data ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("Checking input values for ", type0, " inputs...")
  msg1 |> get_msgPrefix(newline=F) |> paste0("Filtering out missing values...") |> message()
  inputDf   <- inputDf |> filter_all(all_vars(!(. |> is.na())))
  nrowData  <- inputDf |> nrow()
  namesDf   <- inputDf |> names()
  hasData   <- nullData |> ifelse(0, nrowData)
  if(!nrowData) {
    msg2 |> get_msgPrefix(newline=F) |> paste0("At least some non-missing values required!") |> message()
    msg2 |> get_msgPrefix(newline=F) |> paste0("Exiting...") |> message()
    return()
  } ### End if(!nrowData)

  #### Check Years ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("Checking that data has required years...")
  checkYr0  <- inputDf |> pull(all_of(yrCol0)) |> check_inputs(
    xmin  = minYr0,
    xmax  = maxYr0,
    in0   = FALSE ### Whether data needs to be inside or outside of range
  ) ### End check_inputs
  if(!checkYr0[["check"]]) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Warning:") |> message()
    msg2 |> get_msgPrefix(newline=F) |>
      paste0("Data must have at least one non-missing value for years ") |>
      paste0(checkYr0[["message"]] |> unlist() |> paste(collapse=""), "!")
    msg2 |> get_msgPrefix(newline=F) |> paste0("Exiting...") |> message()
    return()
  } ### End if(!checkYr0[["check"]])

  #### Check Values ----------------
  msg0 |> get_msgPrefix(newline=F) |> paste0("Checking that values are within range...")
  checkVal0 <- inputDf |> pull(all_of(valCol0)) |> check_inputs(
    xmin  = inputMin0,
    xmax  = inputMax0,
    in0   = TRUE ### Whether data needs to be inside or outside of range
  ) ### End check_inputs
  if(!checkVal0[["check"]]) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Warning:") |> message()
    msg2 |> get_msgPrefix(newline=F) |>
      paste0("Data values must be ") |>
      paste0(checkVal0[["message"]] |> unlist() |> paste(collapse=""), "!")
    msg2 |> get_msgPrefix(newline=F) |> paste0("Exiting...") |> message()
    return()
    # if(doTemp) {
    #   msg2 |> get_msgPrefix(newline=F) |> paste0("Values outside of range will be changed to NA and result in missing impacts") |> message()
    #
    #   inputDf <- inputDf |> mutate_at(c(valCol0), function(x, y=inputMin, z-inputMax){case_when(x < inputMin | x > inputMax ~ NA, .default=x)})
    #   inputDf <- inputDf |> mutate(
    #     !!sym(valCol0)  := case_when(
    #       !!sym(valCol0) < 0 ~ 0,
    #       !!sym(valCol0) > 30 ~ 0,
    #       !!sym(valCol0) >= 0 & !!sym(valCol0) <= 30 ~ !!sym(valCol0)
    #     ) ### End case_when
    #   ) ### End mutate
    # } ### End if doTemp
  } ### End if(!checkYr0[["check"]])

  ### Format Data ----------------
  #### Calculate State Population ----------------
  calcPop0  <- !(popArea |> str_detect("state")) & doPop0
  if(calcPop0) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Calculating state population from ", popArea, " values...") |> message()
    inputDf <- inputDf |> calc_popFromArea(
      minYr0  = minYr0,
      maxYr0  = maxYr0,
      valCol0 = valCol0,
      popArea = popArea,
      module  = module,
      msg0    = msg2
    ) ### End calc_import_pop
  } ### End if(doPop)

  ### Drop state_order column from pop
  if(doPop0) {
    drop0 <- "state_order"
    inputDf <- inputDf |> select(-any_of(drop0))
    rm(drop0)
  } ### End if(doPop0)

  #### Convert Temperatures ----------------
  ### If doTemp & tempType == "global", mutate temperatures
  doTemps0  <- (tempType == "global") & doTemp0
  if(doTemps0) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Converting temperatures from global to CONUS using convertTemps(from='global')...") |> message()
    inputDf <- inputDf |> mutate_at(c(valCol0), convertTemps, from=tempType, msg0=msg2)
  } ### End if(doTemp & doConvert)
  rm(doConvert)

  ### Return ----------------
  msg1 |> get_msgPrefix(newline=F) |> paste0("Values passed.") |> message()
  return(inputDf)
}




## Calculate state population ----------------
### Calculate state population from area or region
#calc_import_pop
calc_popFromArea <- function(
    df0,      ### Population data
    minYr0    = 2010,
    maxYr0    = 2100,
    valCol0   = "pop",
    popArea   = "state", ### One of: c("state", "regional", "conus", "national")
    module    = "fredi", #### "fredi", "sv", or "methane"
    df_ratios = NULL, # get_frediDataObj("popRatios", "controlData") ### End fun_extendVals
    msg0      = 0        ### Level of messaging
){
  ### Set Up the Environment ----------------
  #### Messaging ----------------
  sep0       <- ", "
  msgN       <- "\n"
  # msg0       <- msgLevel
  msg1       <- msg0 + 1
  msg2       <- msg0 + 2
  msg3       <- msg0 + 3

  #### Columns & Values ----------------
  #### Columns
  yrCol0     <- "year"
  areaCol0   <- "area"
  regCol0    <- "region"
  stateCol0  <- "state"
  postCol0   <- "postal"
  orderCol0  <- "state_order"

  ### Values
  names0     <- df0 |> names()
  hasPost0   <- postCol0 %in% names0

  ### Conditionals
  # doNat    <- popArea |> str_detect("nat")
  # doArea   <- popArea |> str_detect("conus") | doNat
  # doReg    <- popArea |> str_detect("reg"  ) | doArea
  doArea   <- popArea |> str_detect("nat")
  doReg    <- popArea |> str_detect("conus") | doArea
  doState  <- popArea |> str_detect("reg"  ) | doReg

  #### Module Options ----------------
  module0    <- module |> tolower()

  #### Ratios Data ----------------
  nullRatios <- df_ratios |> is.null()
  if(nullRatios) df_ratios <- get_frediDataObj("popRatios", "scenarioData") # |> filter(year >= minYr0, year <= maxYr0)
  select0    <- c(postCol0, orderCol0, yrCol0) |> c("area2nat", "reg2area", "state2reg")
  df_ratios  <- df_ratios |> select(any_of(select0))
  rm(select0)

  ### Format Data ----------------
  #### Drop national values
  #### Join with ratios
  # c(postCol0, orderCol0, yrCol0)
  join0   <- c(postCol0, orderCol0, yrCol0) |> get_matches(names0)
  df0     <- df0 |>
    filter(!(postal |> str_detect("US"))) |>
    left_join(df_ratios, by=join0)
  rm(df_ratios)

  ### Calculate Population ----------------
  ### Area population
  if(doArea) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Calculating area population from national population...") |> message()
    df0 <- df0 |> mutate(pop = pop * area2nat)
  } ### End if(doArea)
  ### Regional population
  if(doReg) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Calculating regional population from area population...") |> message()
    df0 <- df0 |> mutate(pop = pop * reg2area)
  } ### End if(doReg)
  ### State population
  if(doState) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Calculating state population from regional population...") |> message()
    df0 <- df0 |> mutate(pop = pop * state2reg)
  } ### End if(doReg)

  ### Select columns & arrange values ----------------
  select0  <- c(regCol0, stateCol0, postCol0, yrCol0, valCol0)
  df0      <- df0 |>
    arrange_at(c(orderCol0, yrCol0)) |>
    select(all_of(select0))

  ### Return ----------------
  return(df0)
}



### Update defaults
update_inputDefaults <- function(
    defaultsList,
    inputsList
){
  ### Names
  names0 <- defaultsList |> names()
  ### Iterate over names and replace data
  list0  <- names0 |> map(function(name0){
    doSlr0  <- name0 |> str_detect("slr")
    nullIn0 <- inputsList[[name0]] |> is.null()
    if(!nullIn0) {
      df0 <- inputsList[[name0]]
    } else{
      if(doSlr0) {
        nullIn0 <- inputsList[["temp"]] |> is.null()
        if(!nullIn0) {
          df0 <- inputsList[["temp"]]
        } else{
          df0 <- defaultsList[[name0]]
        } ### End if(!nullIn0)
      } else {
        df0 <- defaultsList[[name0]]
      } ### End if(doSlr0)
    } ### End  ### End if(!nullIn0)
    return(df0)
  }) |> set_names(names0)
  ### Return
  return(list0)
}
