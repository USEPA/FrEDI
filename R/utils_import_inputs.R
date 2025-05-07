## get_msg_prefix ----------------
# ### Function to get message prefix
# get_msg_prefix <- function(
#     level  = 0,
#     prefix = ""
# ){
#   msgP    <- "\t"   |> rep(level) |> paste(collapse="")
#   msgP    <- prefix |> paste0(msgP)
#   return(msgP)
# }


## Initial Functions ----------------
### Info about which column contains info on region
# get_import_inputs_idCols
get_inputsIDCols <- function(
    type0   = "temp", ### c("gdp", "pop", "temp", "slr", "ch4", "nox", "o3"),
    doTemp0 = TRUE,
    doReg0  = FALSE,
    doPop0  = FALSE,
    doO3    = FALSE,
    popArea = "state", ### One of: c("state", "regional", "area", "national")
    module0 = "sv"
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

  ### If doTemp0 & doSv0: Add scenario column
  doSv0   <- "sv"  %in% module0
  doSlr0  <- "slr" %in% type0
  doScen0 <- doSv0 & (doTemp | doSlr0)
  if(doScen0) {
    cols0 <- cols0 |> c("scenario")
  } ### End if(doSv0 & doTemp)

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
  ### Values
  module0 <- module0 |> tolower()
  mTypes0 <- mTypes0 |> tolower()

  ### Data from namespace
  ### Scenarios
  df0     <- "controlData" |>
    get_frediDataObj("co_moduleScenarios") |>
    filter(module %in% module0)

  ### Drop SLR if not present
  slrStr0 <- "slr"
  doSlr0  <- mTypes0 |> str_detect(slrStr0) |> any()
  if(!doSlr0) df0 <- df0 |> filter(!(inputName %in% slrStr0))

  ### Drop GDP if SV
  gdpStr0 <- "gdp"
  doGdp0  <- !(module0 %in% "sv")
  if(!doGdp0) df0 <- df0 |> filter(!(inputName %in% gdpStr0))

  ### Return
  return(df0)
}

### Function to get default scenario
get_defaultScenario <- function(
    name0,
    dfInfo,
    minYr0  = 2010,
    maxYr0  = 2100,
    yrCol0  = "year",
    module0 = "fredi"
){
  # name0 |> print();
  ### Conditionals
  tempStr0 <- "temp"
  slrStr0  <- "slr"
  doTemp   <- name0 |> str_detect(tempStr0)
  doSlr    <- name0 |> str_detect(slrStr0)
  index0   <- doSlr |> ifelse(tempStr0, name0)
  isSvMod0 <- "sv" %in% module0
  doSv0    <- isSvMod0 & (doTemp | doSlr)

  ### Info
  info0    <- dfInfo |> filter(inputName %in% name0)
  idColX   <- info0  |> pull(idCol0)
  scen0    <- info0  |> pull(all_of(idColX))
  refYr0   <- info0  |> pull(refYear)
  doReg0   <- info0  |> pull(doReg0)
  yCol0    <- info0  |> pull(valueCol)

  ### Conditionals
  doRefYr0 <- !(refYr0 |> is.na())
  minYr0   <- doRefYr0 |> ifelse(refYr0, minYr0)
  # c(name0, index0, idColX, scen0) |> print()

  ### Get input data, then filter to scenario
  df0      <- "scenarioData" |>
    get_frediDataObj(index0) |>
    filter_at(c(idColX), function(x, y=scen0){x %in% y})

  ### Filter to years
  # c(minYr0, maxYr0) |> print()
  # df0 |> pull(year) |> range() |> print()
  df0     <- df0 |>
    filter(year >= minYr0) |>
    filter(year <= maxYr0)
  # df0 |> pull(year) |> range() |> print()

  ### Select columns
  idCols0  <- yrCol0
  if(doSv0 ) idCols0 <- "scenario" |> c(idCols0)
  if(doTemp) yCol0   <- yCol0 |> paste0("_", c("conus", "global")) |> c("slr_cm")
  if(doReg0) idCols0 <- c("postal") |> c(idCols0)
  cols0   <- idCols0 |> c(yrCol0, yCol0) |> unique()
  df0     <- df0 |> select(all_of(cols0))
  # df0 |> glimpse()

  ### Return
  # name0 |> print();
  # df0 |> pull(year) |> range() |> print()
  return(df0)
}

### Add info to data
get_defaultScenarios <- function(
    dfInfo,
    mTypes0 = c("gcm", "slr"),
    minYr0  = 2010,
    maxYr0  = 2100,
    yrCol0  = "year",
    module0 = "fredi"
){
  ### Drop SLR if that is not present
  ### Get list of input names
  slrStr0      <- "slr"
  mTypes0      <- mTypes0 |> tolower()
  doSlr0       <- slrStr0 %in% mTypes0
  if(!doSlr0) dfInfo |> filter(!(inputName %in% slrStr0))
  inputNames0  <- dfInfo  |> pull(inputName)
  # mTypes0 |> print(); doSlr0 |> print(); inputNames0 |> print()

  ### Get list of defaults
  inputDefs    <- inputNames0 |> map(
    get_defaultScenario,
    dfInfo  = dfInfo,
    minYr0  = minYr0,
    maxYr0  = maxYr0,
    yrCol0  = "year",
    module0 = module0
  ) |> set_names(inputNames0)

  ### Return
  return(inputDefs)
}


### Check and format inputs list
# notNull    <- inNames    |> map(function(name0){!(inputsList[[name0]] |> is.null())}) |> unlist() |> which()
# ### Filter values and update inNames
# inputsList <- inputsList[inNames][notNull]
# inNames    <- inputsList |> names()
# dfInfo     <- dfInfo     |> filter(inputName %in% inNames)
# inNames    <- dfInfo     |> pull(inputName)
# inputsList <- inputsList[inNames]
# ### Check if there are values
# nInputs    <- inputsList |> length()
# nNames     <- inNames    |> length()
# if (!nNames) {
#   paste0(msg0) |> paste0("Error! `inputsList` argument requires a list with named elements.") |> message()
#   msgN |> paste0(msg0) |> paste0("Exiting...") |> message()
#   return()
# } else if(!nInputs){
#   return(inputsList)
# } ### End if(!hasInputs)

format_inputsList <- function(
    dfInfo,   ### Outputs of get_dfInputInfo
    inputsList = list(temp=NULL, slr=NULL, pop=NULL, gdp=NULL),
    # maxYr0     = 2100,
    tempType   = "conus",
    popArea    = "state",
    module0    = "fredi",
    silent     = FALSE,
    msg0       = 0
){
  ### Messaging
  msgN       <- "\n"

  ### Input strings
  tempStr0   <- "temp"
  slrStr0    <- "slr"
  popStr0    <- "pop"
  o3Str0     <- "o3"

  ### Reference names from dfInfo0
  inNames0   <- dfInfo |> pull(inputName)

  ### If there are no elements in inputsList, return an empty list
  nInputs    <- inputsList |> length()
  if(!nInputs) return(list())

  # ### Check which user-supplied inputs are missing
  # ### Get input names that match inNames0 and filter to values that are not null
  # isNullIn   <- inNames0   |> map(check_nullListElement, list0=inputsList) |> set_names(inNames0)

  ### Drop NULL list elements from inputsList
  ### Order inputsList by order of inNames0
  # inputsList |> glimpse()
  inputsList <- inputsList |> drop_nullListElements(matches=FALSE)
  inNames    <- inNames0   |> get_matches(y=inputsList |> names())
  inputsList <- inputsList[inNames]
  nInputs    <- inputsList |> length()
  nNames     <- inNames    |> length()

  ### Check if there are values
  if(!nInputs) {
    return(inputsList)
  } else if (!nNames) {
    paste0(msg0) |> paste0("Error! `inputsList` argument requires a list with named elements.") |> message()
    msgN |> paste0(msg0) |> paste0("Exiting...") |> message()
    # return(list())
    return()
  } else if(!(nNames = nInputs)){
    paste0(msg0) |> paste0("Error! Number of names of `inputsList` must equal number of list elements.") |> message()
    msgN |> paste0(msg0) |> paste0("Exiting...") |> message()
    # return(list())
    return()
  } ### End if(!nInputs)

  ### If there are inputs, filter dfInfo to values in inNames
  ### Rename and select columns
  ### Add values
  # old0       <- c("inputName", "minYear", "maxYear", "inputMin", "inputMax", "regional", "valueCol")
  # new0       <- c("type0", "minYr0", "maxYr0", "inputMin0", "inputMax0", "doReg0", "valCol0")
  old0       <- c("inputName", "minYear", "maxYear", "inputMin", "inputMax", "valueCol")
  new0       <- c("type0", "minYr0", "maxYr0", "inputMin0", "inputMax0", "valCol0")
  cols0      <- new0 |> c("doTemp0", "doReg0", "doPop0", "doO3")
  dfInfo     <- dfInfo |>
    filter(inputName %in% inNames) |>
    rename_at(c(old0), ~c(new0)) |>
    select(all_of(cols0)) |>
    mutate(tempType = case_when(doTemp0 ~ tempType, .default = NA)) |>
    mutate(popArea  = case_when(doPop0  ~ popArea , .default = NA))

  ### Get ID columns
  select0    <- c("type0", "doTemp0", "doReg0", "doPop0", "doO3", "popArea")
  idCols0    <- dfInfo |>
    select(all_of(select0)) |>
    pmap(get_inputsIDCols) |>
    set_names(inNames)
  rm(select0)

  ### Check input data
  # dfInfo |> glimpse()
  inputsList <- dfInfo |>
    as.list() |>
    c(list(idCols  = idCols0   )) |>
    c(list(inputDf = inputsList)) |>
    pmap(check_inputData, module=module0) |>
    set_names(inNames)

  ### Drop NULL list elements from inputsList
  ### Order inputsList by order of inNames0
  inputsList <- inputsList |> drop_nullListElements(matches=FALSE)
  inNames    <- inNames0   |> get_matches(y=inputsList |> names())
  inputsList <- inputsList[inNames]

  ### Return inputs list
  return(inputsList)
}

### Update defaults ----------------
update_inputDefault <- function(
    type0,
    dfInfo,
    defaultsList,
    inputsList = list(),
    minYear = "frediData" |> get_frediDataObj("fredi_config", "minYear"),
    maxYear = "frediData" |> get_frediDataObj("fredi_config", "maxYear")
){
  ### Messaging
  msgN       <- "\n"

  ### Input strings
  tempStr0   <- "temp"
  slrStr0    <- "slr"


  ### Data
  dfDef      <- defaultsList[[type0]] |> filter(year >= minYear, year <= maxYear) |> ungroup()
  dfIn       <- inputsList  [[type0]]

  ### If input is present, return the data
  hasIn      <- dfIn |> is.data.frame()
  if(hasIn) {
    dfIn <- dfIn |> filter(year >= minYear, year <= maxYear) |> ungroup()
    return(dfIn)
  } ### End if(hasIn)

  ### Otherwise, check if the input is temp or slr
  ### Reference names from dfInfo0
  inNames0   <- dfInfo |> pull(inputName)

  ### Check whether temp, slr is present in inNames0:
  ### - If !doSlr0 & hasSlr: Not possible with how values are defined
  ### - If doSlr0 & !hasSlr:
  ###     - If hasTemp, calculate temps2slr from user input
  ### Current type
  isTemp0    <- tempStr0 %in% type0
  isSlr0     <- slrStr0  %in% type0
  isTempSlr  <- isTemp0 | isSlr0
  if(!isTempSlr) return(dfDef)

  ### If temp or slr, check for additional conditionals
  ### - Check whether temp, slr are in names
  doTemp0    <- tempStr0 %in% inNames0
  doSlr0     <- slrStr0  %in% inNames0
  ### If isTemp0 & !doTemp0, return NULL
  if(isTemp0 & !doTemp0) return()

  ### Get ref years
  infoT  <- dfInfo |> filter(inputName %in% tempStr0)
  infoS  <- dfInfo |> filter(inputName %in% slrStr0 )
  ### Column
  yrCol0 <- "year"
  idCol0 <- "scenario"
  yColT  <- infoT |> pull(valueCol)
  drop0  <- yColT |> paste0("_", c("conus", "global")) |> c(idCol0)

  ### If isSlr0: Check if there is a temperature input
  ### If there is a temperature input, calculate global temps, slr height
  if(isSlr0) {
    dfTemp  <- inputsList[[tempStr0]]
    hasTemp <- dfTemp |> is.data.frame()
    if(hasTemp) {
      ### Calculate global temperatures and SLR
      dfTemp <- dfTemp |>
        mutate(scenario = type0) |>
        group_by_at(c(idCol0)) |>
        group_map(
          .x |> format_tempData_byGroup(
            .y        = .y,
            xCol0     = yrCol0,
            yCol0     = yColT,
            xOut0     = infoT |> pull(refYear) |> seq(maxYear),
            tempType0 = "conus",
            method0   = "linear",
            rule0     = 1
          ) ### End format_tempData_byGroup
        ) |> bind_rows() |>
        ungroup() |>
        select(-any_of(drop0))
      ### Zero out values
      dfTemp <- dfTemp |> zero_out_scenario(
        refYr0  = infoS |> pull(refYear) ,
        xCol0   = yrCol0,
        yCol0   = infoS |> pull(valueCol),
        idCols0 = idCols0
      ) ### End zero_out_scenario
      ### Filter to years
      dfTemp <- dfTemp |> filter(year >= minYear, year <= maxYear)
      ### Return
      return(dfTemp)
    } ### End if(hasTemp)
    ### Otherwise, return default
    return(dfDef)
  } ### End if(isSlr0)

  ### Otherwise, rename and drop columns
  dfDef[[yColT]] <- dfDef[[yColT |> paste0("_global")]]
  dfDef      <- dfDef |> select(-any_of(drop0))

  ### Return
  return(dfDef)
}

# update_inputDefaults <- function(
    #
#     defaultsList,
#     inputsList
# ){
#   ### Names
#   names0 <- defaultsList |> names()
#   ### Iterate over names and replace data
#   list0  <- names0 |> map(function(name0){
#     doSlr0  <- name0 |> str_detect("slr")
#     nullIn0 <- inputsList[[name0]] |> is.null()
#     if(!nullIn0) {
#       df0 <- inputsList[[name0]]
#     } else{
#       if(doSlr0) {
#         nullIn0 <- inputsList[["temp"]] |> is.null()
#         if(!nullIn0) {
#           df0 <- inputsList[["temp"]]
#         } else{
#           df0 <- defaultsList[[name0]]
#         } ### End if(!nullIn0)
#       } else {
#         df0 <- defaultsList[[name0]]
#       } ### End if(doSlr0)
#     } ### End  ### End if(!nullIn0)
#     return(df0)
#   }) |> set_names(names0)
#   ### Return
#   return(list0)
# }

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
    msg0 |> get_msgPrefix() |> paste0("No filename provided. Exiting...") |> message()
    return()
  } ### End if(nullName)

  ### If file name exists, check file
  msg0 |> get_msgPrefix() |> paste0("User specified ", inputName |> paste0("file"), "...")
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
  msg1 |> get_msgPrefix() |> paste0(list0[["fileMsg"]]) |> message()
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
    type0   = "pop"  , ### Label for type of input
    module0 = "fredi", #### "fredi", "sv", or "methane"
    msg0    = 0        ### Level of messaging
){
  ### Set Up the Environment ----------------
  #### Messaging ----------------
  sep0       <- ", "
  msgN       <- "\n"
  # msg0       <- msgLevel
  msg1       <- msg0 + 1
  msg2       <- msg0 + 2
  msg3       <- msg0 + 3
  msg0 |> get_msgPrefix(newline=F) |> paste0("Checking regions, states in ", type0 , " data...") |> message()

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
  doReg0     <- names0 |> str_detect("region") |> any()
  doState0   <- names0 |> str_detect("state" ) |> any()
  ### Standardize region, if present
  if(doReg0)   df0 <- df0 |> mutate(region = region |> str_replace_all("_|\\.", " "))
  if(doReg0)   checkCols0 <- checkCols0 |> c(regCol0  )
  if(doState0) checkCols0 <- checkCols0 |> c(stateCol0)


  #### Module Options ----------------
  module0    <- module0 |> tolower()
  areas0     <- "controlData" |> get_frediDataObj("co_moduleAreas") |>
    filter_at(c("module"), function(x, y=module0){x %in% y}) |>
    pull(all_of(areaCol0))
  co_states  <- "controlData" |> get_frediDataObj("co_states") |>
    filter_at(c(areaCol0), function(x, y=areas0){x %in% y}) |>
    # select(-any_of(area))
    select(any_of(regIdCols0))

  ### Format Data ----------------
  ### Drop missing values and drop area, region, columns
  drop0      <- colIds0 |> get_matches(names0)
  df0        <- df0 |> filter_all(all_vars(!(. |> is.na())))
  # df0        <- df0 |>
  #   filter_all(all_vars(!(. |> is.na()))) |>
  #   rename_at(c(old0), ~new0)

  ### Join data into states
  namesState <- co_states |> names()
  join0      <- names0  |> get_matches(y=namesState)
  # co_states |> names() |> print(); names0 |> print(); join0 |> print()
  # old0       <- colIds0 |> get_matches(y=join0)
  old0       <- colIds0
  new0       <- old0    |> paste0(lblStr0)
  # old0 |> print(); new0 |> print()
  df0        <- co_states |>
    select(-any_of(old0)) |>
    rename_at(c(new0), ~old0) |>
    left_join(df0, by=join0) |>
    arrange_at(c(orderCol0, yrCol0))

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
    filter_all(all_vars(!(. |> is.na())))
  # df0        <- df0 |>
  #   filter_all(all_vars(!(. |> is.na()))) |>
  #   select(-any_of(colIds0)) |>
  #   rename_at(c(new0), ~old0)

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
  sep0      <- ", "
  msgN      <- "\n"
  # msg0      <- msgLevel
  msg1      <- msg0 + 1
  msg2      <- msg0 + 2
  msg3      <- msg0 + 3
  msg0 |> get_msgPrefix(newline=F) |> paste0("Checking for column '", valCol0, "' in ", inputName, " data...") |> message()

  #### Columns & Values ----------------
  #### Conditionals
  doTemp0   <- "temp" %in% inputName
  doPop0    <- "pop"  %in% inputName

  ### Initial names and values
  valColRef <- valCol0
  inNameRef <- inputName
  namesRef  <- inputDf |> names()
  ### Convert to lowercase
  valColLC0 <- valCol0   |> tolower()
  inNameLC0 <- inputName |> tolower()
  namesLC0  <- namesRef  |> tolower()
  # namesRef |> print(); c(valColRef) |> print()

  ### Get matches ----------------
  ### Match values
  # colMatch0 <- valColRef |> get_matches(namesRef, type="matches")
  # lcMatch0  <- valColLC0 |> get_matches(namesLC0, type="matches")
  colMatch0 <- namesRef |> get_matches(valColRef, type="matches")
  lcMatch0  <- namesLC0 |> get_matches(valColLC0, type="matches")
  strMatch0 <- namesLC0 |> str_detect(inNameLC0)
  if     (doTemp0) argMatch0 <- namesLC0 |> str_detect(tempType)
  else if(doPop0 ) argMatch0 <- namesLC0 |> str_detect(popArea )
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
  msgStrNot <- "' not found in data"
  msgStrFnd <- "' found in data"
  msgStrInp <- inNameRef |> paste0(" data...")
  msgCol0   <- paste0(msgStrCol, valCol0, msgStrQuo, inNameRef, msgStrInp)
  msgNCol0  <- paste0("Column '", valCol0, "' not found in ", inNameRef, " data...")
  if(colMatchN) {
    msg1 |> get_msgPrefix() |> paste0(msgStrCol, valCol0, msgStrFnd, "...") |> message()
    colMatch0 <- colMatch0 |> which()
    matchCol0 <- namesRef[colMatch0]
    # matchCol0 |> print()
  } else {
    msg1 |> get_msgPrefix() |> paste0(msgStrCol, valCol0, msgStrNot, "...") |> message()
    msg1 |> get_msgPrefix() |> paste0("Looking for column '", valColLC0, "'...") |> message()
    if(lcMatchN) {
      msg1 |> get_msgPrefix() |> paste0(msgStrCol, valColLC0, msgStrFnd, "...") |> message()
      lcMatch0  <- lcMatch0 |> which()
      matchCol0 <- namesRef[lcMatch0]
    } else {
      msg1 |> get_msgPrefix() |> paste0(msgStrCol, valColLC0, msgStrNot, "...") |> message()
      msg1 |> get_msgPrefix() |> paste0("Looking for column names matching string '", inNameLC0, "'...") |> message()
      if (strMatchN) {
        msg1 |> get_msgPrefix() |> paste0("Column name(s) matching string ", inNameLC0, msgStrFnd, "...") |> message()
        if(strMatchN == 1) {
          strMatch0 <- strMatch0 |> which()
          matchCol0 <- namesRef[strMatch0]
        } else if(strMatchN > 1) {
          msg2 |> get_msgPrefix() |> paste0("Multiple matches found...")
          matches0 <- strMatch0 & argMatch0
          matchesN <- matches0 |> which() |> length()
          if     (matchesN == 1) {matchCol0 <- namesRef[matches0 |> which()]}
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
  # c(matchCol0, valColRef) |> print(); inputDf |> glimpse()
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
    valCol0  , ### E.g., c("temp_C", "slr_cm", "gdp_usd", "state_pop") ### Or "reg_pop", "area_pop", or "national_pop", depending on popArea
    doTemp0   = TRUE,
    doReg0    = FALSE,
    doPop0    = FALSE,
    doO3      = FALSE,
    tempType  = "conus",  ### One of: c("conus", "global")
    popArea   = "state",  ### One of: c("state", "regional", "conus", "national")
    idCols0  , ### E.g., "state" or "region" if popArea is "state" or "region", respectively; empty character (i.e., c()) otherwise
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
  msg0 |> get_msgPrefix(newline=T) |> paste0("Checking '", type0, " data...") |> message()

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
    inputDf  <- inputDf |> check_regions(type0=type0, module0=module0, msg0=msg1)
    nullData <- inputDf |> is.null()
    if(nullData) return()
  } ### End if(doPop | doO3)

  #### Check for Value Column ----------------
  inputDf   <- type0 |> check_valCols(
    inputDf  = inputDf, ### Data frame of values
    valCol0  = valCol0, ### Value column
    doTemp0  = doTemp0,
    doPop0   = doPop0 ,
    tempType = tempType, ### One of: c("conus", "global")
    popArea  = popArea , ### One of: c("state", "regional", "conus", "national")
    msg0     = msg1
  ) ### End check_valCols
  nullData   <- inputDf |> is.null()
  if(nullData) return()

  #### Check for Other Columns ----------------
  othCols0  <- c(idCols0, yrCol0) |> unique()
  matches0  <- othCols0 |> get_matches(inputDf |> names(), type="matches")
  naCols0   <- othCols0[!matches0]
  naColsN0  <- naCols0 |> length()
  if(naColsN0) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Data is missing the following required columns: " ) |> message()
    msg2 |> get_msgPrefix(newline=F) |> paste0("'", naCols0 |> paste(collapse="', '"), "'...") |> message()
    msg2 |> get_msgPrefix(newline=F) |> paste0("Exiting...") |> message()
    return()
  } else{
    msg1 |> get_msgPrefix(newline=F) |> paste0("All required columns present." ) |> message()
  } ### End if(naCols0)
  rm(matches0, naCols0, naColsN0)

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
  rm(calcPop0)

  ### Drop state_order column from pop
  if(doPop0) {
    drop0 <- "state_order"
    inputDf <- inputDf |> select(-any_of(drop0))
    rm(drop0)
  } ### End if(doPop0)
  rm(doPop0)

  #### Convert Temperatures ----------------
  ### If doTemp & tempType == "global", mutate temperatures
  doTemps0  <- (tempType == "global") & doTemp0
  if(doTemps0) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Converting temperatures from global to CONUS using convertTemps(from='global')...") |> message()
    inputDf <- inputDf |> mutate_at(c(valCol0), convertTemps, from=tempType, msg0=msg2)
  } ### End if(doTemp & doConvert)
  rm(doTemps0)

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
  if(nullRatios) df_ratios <- "scenarioData" |> get_frediDataObj("popRatios") # |> filter(year >= minYr0, year <= maxYr0)
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






## End script ----------------
