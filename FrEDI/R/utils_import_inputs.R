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

###### get_import_inputs_valCols ######
### 2024.06.18: eventually move to FrEDI Data
### Info about which column contains input values
get_import_inputs_valCols <- function(
    popArea = "state" ### One of: c("state", "regional", "area", "national")
){
  ### Initialize list
  list0    <- list()
  list0[["temp"]] <- c("temp_C")
  list0[["slr" ]] <- c("slr_cm")
  list0[["gdp" ]] <- c("gdp_usd")
  list0[["pop" ]] <- c("pop")
  ### Return
  return(list0)
}

###### get_import_inputs_idCols ######
### 2024.06.18: eventually move to FrEDI Data
### Info about which column contains info on region
get_import_inputs_idCols <- function(
    popArea = "state" ### One of: c("state", "regional", "area", "national")
){
  ### Initialize list
  list0    <- list()
  list0[["temp"]] <- c("year")
  list0[["slr" ]] <- c("year")
  list0[["gdp" ]] <- c("year")
  list0[["pop" ]] <- list()
  list0[["pop" ]][["national"]] <- c("year")
  list0[["pop" ]][["conus"   ]] <- c("year")
  list0[["pop" ]][["regional"]] <- c("region", "year")
  list0[["pop" ]][["state"   ]] <- c("state", "year")
  ### Update population value column based on pop area
  list0[["pop" ]] <- list0[["pop"]][[popArea]]
  ### Return
  return(list0)
}

###### fun_tryInput ######
### This function attempts to load a user-specified input file
fun_tryInput <- function(
    filename = NULL,
    silent   = FALSE, ### Whether to message
    msg0     = ""     ### Message prefix
){
  ###### Messaging ######
  msgUser <- !silent
  msg1    <- msg0 |> paste0("\t")
  msg2    <- msg1 |> paste0("\t")
  msg3    <- msg2 |> paste0("\t")

  ###### Initialize results ######
  return_list <- list()

  ###### Defaults ######
  ### Check if the file exists and try to load the file
  ### Set input to null if it doesn't exist
  if(!(filename |> is.null())){
    fileExists  <- filename |> file.exists()
    ### If the file exists, try loading the file and then check the class of the result
    if(fileExists){
      fileInput   <- try(filename |> read.csv(check.names = F), silent=T)
      classInput  <- fileInput |> class()
      inputExists <- ("data.frame" %in% classInput)

      ### If loading the inputs was successful
      if(inputExists){
        return_list[["fileInput" ]] <- fileInput
        return_list[["fileStatus"]] <- "loaded"
        return_list[["fileMsg"   ]] <- "Data loaded."
      } else{
        return_list[["fileInput" ]] <- NULL
        return_list[["fileStatus"]] <- "other-error"
        return_list[["fileMsg"   ]] <- "Not able to read CSV data."
      } ### End if(inputExists)
    }  else{
      return_list[["fileInput" ]] <- NULL
      return_list[["fileStatus"]] <- "no-file"
      return_list[["fileMsg"   ]] <- "Input file does not exist."
    } ### End if(fileExists)

    ### Message the user
    # message("\t", return_list[["fileMsg"]])
    if(msgUser){ msg0 |> paste0(return_list[["fileMsg"]]) |> message() }
  } ### End if(!(filename |> is.null()))

  ###### Return ######
  return(return_list)
}


###### run_fun_tryInput ######
### Function to iterate over a list of file names
run_fun_tryInput <- function(
    inputName = "temp",      ### Type of input; one of: c("temp", "slr", "gdp", "pop")
    fileName  = "temps.csv", ### File path and name of CSV to be imported
    msgLevel  = 1
){
  ### Messages
  msgN   <- "\n"
  msg0_i <- get_msg_prefix(level=msgLevel)
  msg1_i <- get_msg_prefix(level=msgLevel + 1)
  msg2_i <- get_msg_prefix(level=msgLevel + 2)
  msg_i1 <- paste0("User specified ", inputName |> paste0("file"), "...")
  msg_i2 <- paste0("File does not exist! Returning a null data frame for", inputName |> paste0("Input", "."))
  msg_i3 <- paste0("Importing data from ", fileName, "...")

  ### Initialize data frame as a null value
  df_i   <- NULL

  ### Check if a file name was supplied
  ### - If no file supplied, return NULL
  ### - Otherwise, continue
  has_i  <- fileName |> length()
  if(!has_i) return(df_i)

  ### If a file name was supplied, message user
  ### Get info about the input
  ### Then check if the file exists
  msg0_i |> paste0(msg_i1) |> message()
  has_i  <- fileName |> file.exists()

  ### If file does not exist, message user and return NULL
  ### If the file exists, try to load the file
  if(!has_i) {
    msg1_i |> paste0(msg_i2) |> message()
    return(df_i)
  } else {
    msg1_i |> paste0(msg_i3) |> message()
    try_i  <- fileName |> fun_tryInput(silent=T)
    msg_i4 <- try_i[["fileMsg"]]
    has_i  <- "loaded" %in% try_i[["fileStatus"]]
  } ### End if(!has_i)

  ### Message user, then check if file import was successful
  msg1_i |> paste0(msg_i4) |> message()
  if(!has_i) return(df_i)
  else       df_i <- try_i[["fileInput"]]

  ### Return df_i
  return(df_i)
}



###### check_inputs ######
### Check Input Ranges
### If input range is outside the range, return "flag" and row numbers of flagged values
### If input range is all inside the range, return "allgood"
check_inputs <- function(
    x,           ### Data column
    xmin = NULL, ### Minimum of range
    xmax = NULL  ### Maximum of range
){
  ###### Initialize Values ######
  ### Initialize flagged rows
  maxFlagRows <- minFlagRows <- c()

  ###### Check Minimum Value ######
  ### Which observations are below the minimum
  if(!(xmin |> is.null())) minFlagRows <- (x < xmin) |> which()

  ###### Check Maximum Value ######
  ### Which observations are below the minimum
  if(!(xmax) |> is.null()) maxFlagRows <- (x > xmax) |> which()

  ###### Combine Any Flagged Rows ######
  flagRows   <- minFlagRows |> c(maxFlagRows)

  ###### Summarize Flag Status ######
  has_flags  <- (flagRows |> length()) > 0
  # flagStatus <- ifelse(has_flags, "flag", "allgood")

  ###### Return List ######
  flagList   <- list(flagged = has_flags, rows = flagRows)
  return(flagList)
}


###### check_pop_regions ######
### Check that all regions, states, present in data
check_pop_regions <- function(
    df0,         ### Tibble with population info
    msgLevel = 1 ### Level of messaging
){
  ###### Load Data from FrEDI ######
  ### Get objects from FrEDI name space
  ### Get input scenario info: co_info
  ### Get state info: co_states
  fListNames <- c("co_states")
  sListNames <- c("df_ratios")
  for(name_i in fListNames){name_i |> assign(rDataList[["frediData"]][["data"]][[name_i]]); rm(name_i)}
  for(name_i in sListNames){name_i |> assign(rDataList[["stateData"]][["data"]][[name_i]]); rm(name_i)}

  ###### Messages ######
  msgN       <- "\n"
  msg0_i     <- get_msg_prefix(level=msgLevel)
  msg1_i     <- get_msg_prefix(level=msgLevel + 1)
  msg2_i     <- get_msg_prefix(level=msgLevel + 2)
  msg_i1     <- paste0("Checking pop inputs for unique ")
  msg_i2     <- paste0("Missing population data for the following " )
  msg_i3     <- paste0("Dropping pop inputs from outputs.")

  ###### Format Data ######
  ### Drop missing values
  df0        <- df0 |> filter_all(all_vars(!(. |> is.na())))

  ###### Check Data ######
  ### Check that there is data
  nullData   <- df0 |> is.null()
  nrowData   <- df0 |> nrow()
  hasData    <- !nullData & nullData |> ifelse(0, nrowData)

  ### Check whether data has regions, states to check
  namesState <- co_states |> names()
  namesPop   <- df0 |> names()
  namesCheck <- c("region", "state") |> (function(x, y=namesPop){x[x %in% y]})()
  hasCols    <- namesCheck |> length()

  ### Check whether population data present and regions, states present
  doCheck    <- hasData & hasCols

  ###### Check Regions, States ######
  ### Check columns if region or state is present
  if(doCheck) {
    ### Message user
    msgPop  <- paste0(msg_i1, namesCheck |> paste0("s") |> paste(collapse=", "), "...")
    msgN |> paste0(msg0_i, msgPop) |> message()

    ### Get unique values for data, then join with state info
    ### Drop missing values
    join0   <- namesState |> (function(x, y=namesPop){x[x %in% y]})()
    dfCheck <- df0     |> select(all_of(join0)) |> unique()
    dfCheck <- dfCheck |> left_join(co_states, by=c(join0), relationship="many-to-many")
    dfCheck <- dfCheck |> filter_all(all_vars(!(. |> is.na())))

    ### Iterate over columns
    for(col_i in namesCheck) {
      ### Unique ref values
      ### Unique data values
      ### Missing reference values
      refVals  <- co_states |> pull(all_of(col_i)) |> unique()
      newVals  <- dfCheck   |> pull(all_of(col_i)) |> unique()
      naVals   <- refVals   |> (function(x, y=newVals){x[!(x %in% y)]})()
      passVals <- (naVals |> length()) == 0

      ### Message
      if(passVals) {
        msgVals <- paste0("All ", col_i, "s present...")
        msg1_i |> paste0(msgVals) |> message()
      } else{
        msgVals <- paste0(msg_i2, col_i, "(s): ") |> paste0(naVals |> paste(collapse=", "))
        msg1_i |> paste0(msgVals) |> message()
        msg1_i |> paste0(msg_i3) |> message()
        return(NULL)
      } ### End if(passVals)
    } ### End for(col_i in namesCheck)
  } ### End if(hasPop)

  ###### Return ######
  return(df0)
}



###### check_valCols ######
### Check value columns
check_valCols <- function(
    inputDf,   ### Data frame of values
    inputName, ### Input name c("temp", "slr", "gdp", "pop")
    valCol   , ### Value column
    tempType = "conus",  ### One of: c("conus", "global")
    popArea  = "state", ### One of: c("state", "regional", "conus", "national")
    msgLevel = 2
) {
  ###### Messages ######
  msgN      <- "\n"
  msg0_i    <- get_msg_prefix(level=msgLevel)
  msg1_i    <- get_msg_prefix(level=msgLevel + 1)
  msg2_i    <- get_msg_prefix(level=msgLevel + 2)
  msg3_i    <- get_msg_prefix(level=msgLevel + 3)

  ###### Conditionals ######
  doTemp    <- "temp" %in% inputName
  doPop     <- "pop"  %in% inputName

  ###### Data Columns ######
  ### Get value column and id column
  if(valCol |> is.null()) valCol <- get_import_inputs_valCols(popArea=popArea)[[inputName]]
  # if(idCol  |> is.null()) idCol  <- get_import_inputs_idCols (popArea=popArea)[[inputName]]

  ###### Check Values ######
  namesDF   <- inputDf |> names()
  checkVCol <- valCol %in% namesDF
  vColMatch <- namesDF[checkVCol]
  nMatches  <- vColMatch |> length()
  ### Check if there are any string matches, and get number of matches
  if(!nMatches) {
    ### Messages
    msg_vcol0 <- paste0("Column ", "\"", valCol, "\"", " not found in ", inputName, "file data!")
    msg_vcol1 <- paste0("Looking for columns with matches to the string ", "\"", inputName, "\"", "...")
    msg1_i |> paste0(msg_vcol0) |> message()
    # msg2_i |> paste0(msg_vcol1) |> message()
    ### Look for matches
    checkVCol <- namesDF |> tolower() |> str_detect(inputName |> tolower())
    vColMatch <- namesDF[checkVCol]
    nMatches  <- vColMatch |> length()
    matches2  <- nMatches > 1
    ### If matches, check if there are more than once match
    if(nMatches) {
      ### Messages
      msg_vcol2 <- paste0(nMatches, " match", matches2 |> ifelse("es", ""), " found!")
      msg1_i |> paste0(msg_vcol1, msg_vcol2) |> message()
      ### If number of matches is greater than one and doTemp:
      if(matches2) {
        ### - If doTemp:
        ###     - First, look for "conus" string and, if found, use that one
        ###     - If not, check for "global" string and, if found, use that one
        ###     - Otherwise, use first column
        if(doTemp) {
          ### Find CONUS, global
          findConus  <- vColMatch |> tolower() |> str_detect("conus")
          findGlobal <- vColMatch |> tolower() |> str_detect("global")
          ### Columns
          vColConus  <- vColMatch[findConus]
          vColGlobal <- vColMatch[findGlobal]
          ### Number of matches
          nConus     <- vColConus  |> length()
          nGlobal    <- vColGlobal |> length()
          ### Matched column
          vColMatch  <- case_when(
            nConus  ~ vColConus [1],
            nGlobal ~ vColGlobal[1],
            .default = vColMatch[1]
          ) ### End case_when
          ### Update tempType
          tempType   <- case_when(
            nGlobal ~ "global",
            .default = "conus"
          ) ### End case_when
          ### Remove values
          ### c("find", "vCol", "n") |> map(function(x, y=c("conus", "global")){ x |> paste0(y)}) |> unlist()
          rm(findConus, vColConus, nConus)
          rm(findGlobal, vColGlobal, nGlobal)
        } ### End if(doTemp)

        ### - Else if doPop:
        ###     - First, look for "state" string and, if found, use that one
        ###     - Otherwise, use first column
        else if(doPop) {
          ### Find matches
          findState <- vColMatch |> tolower() |> str_detect("state")
          findReg   <- vColMatch |> tolower() |> str_detect("reg")
          findConus <- vColMatch |> tolower() |> str_detect("conus")
          findNat   <- vColMatch |> tolower() |> str_detect("nat")
          ### Columns
          vColState <- vColMatch[findState]
          vColReg   <- vColMatch[findReg  ]
          vColConus <- vColMatch[findConus]
          vColNat   <- vColMatch[findNat  ]
          ### Number of matches
          nState    <- vColState |> length()
          nReg      <- vColReg
          nConus    <- vColConus
          nNat      <- vColNat
          ### Matched column
          vColMatch  <- case_when(
            nState  ~ vColState[1],
            nReg    ~ vColReg  [1],
            nConus  ~ vColConus[1],
            nNat    ~ vColNat  [1],
            .default = vColMatch[1]
          ) ### End case_when
          ### Update popArea
          popArea    <- case_when(
            nReg    ~ "regional",
            nConus  ~ "conus",
            nNat    ~ "national",
            .default = "state"
          ) ### End case_when
          ### Remove values
          rm(findState, vColState, nState)
          rm(findReg  , vColReg  , nReg  )
          rm(findConus, vColConus, nConus)
          rm(findNat  , vColNat  , nNat  )
        } ### End else if(doPop)

        ### - Otherwise, use first column
        else{
          vColMatch <- vColMatch[1]
          ### End if(doTemp)
        } ### End else
      } ### End if(matches2)

      ### Rename columns
      inputDf <- inputDf |> rename_at(c(vColMatch), ~valCol)
    } else{
      ### Message user
      msg_vcol2 <- paste0("No matches found! Exiting...")
      msg1_i |> paste0(msg_vcol1, msg_vcol2) |> message()
      return()
    } ### End if(nMatches)
    ### Message user
    renameVal <- doPop |> ifelse("state_", "") |> paste0(valCol)
    msg_vcol2 <- paste0("Using column ", "\"", vColMatch, "\"", ", and renaming to ", "\"", renameVal, "\"", "...")
    msg1_i |> paste0(msg_vcol2) |> message()
  } ### End if(hasValCol) (no else)

  ###### Return ######
  list0 <- list()
  list0[["inputDf" ]] <- inputDf
  list0[["valCol"  ]] <- valCol
  list0[["tempType"]] <- tempType
  list0[["popArea" ]] <- popArea
  return(list0)
}


###### check_input_data ######
### Check input data...newer version of check_inputs
check_input_data <- function(
    inputName = "temp",   ### Type of input; one of: c("temp", "slr", "gdp", "pop")
    inputDf   = NULL,     ### Tibble of inputs (e.g., as output from run_fun_tryInput)
    valCol    = NULL,     ### E.g., c("temp_C", "slr_cm", "gdp_usd", "state_pop") ### Or "reg_pop", "area_pop", or "national_pop", depending on popArea
    idCol     = NULL,     ### E.g., "state" or "region" if popArea is "state" or "region", respectively; empty character (i.e., c()) otherwise
    tempType  = "conus",  ### One of: c("conus", "global")
    popArea   = "state",  ### One of: c("state", "regional", "conus", "national")
    yearMin   = NULL,     ### Check min year
    yearMax   = NULL,     ### Check max year
    msgLevel  = 2         ### Level of messaging
){
  ###### Load Data from FrEDI ######
  ### Get objects from FrEDI name space
  ### Get input scenario info: co_info
  ### Get state info: co_states
  co_info   <- "co_inputInfo"  |> get_frediDataObj("frediData")
  co_states <- "co_states"     |> get_frediDataObj("frediData")
  df_ratios <- "popRatiosData" |> get_frediDataObj("scenarioData")


  ### Get columns
  select0   <- c("inputName", "inputMin", "inputMax")
  co_info <- co_info |> select(all_of(select0))
  co_info <- co_info |> rename_at(c("inputName"), ~"inputType")
  rm(select0)


  ###### Messages ######
  msgN      <- "\n"
  msg0_i    <- get_msg_prefix(level=msgLevel)
  msg1_i    <- get_msg_prefix(level=msgLevel + 1)
  msg2_i    <- get_msg_prefix(level=msgLevel + 2)
  msg_i1    <- paste0("Checking input values for ", inputName, " inputs...")
  msg_i2    <- paste0("Dropping ", inputName, " inputs from outputs.")
  msg_i3    <- paste0("Data is missing the following required columns: " )
  msg_i4    <- paste0("Data for column ", valCol, " is not numeric!")
  msg_i5    <- paste0("Some values for column ", valCol, " are outside the range!")


  ###### Conditionals ######
  doTemp    <- "temp" %in% inputName
  doPop     <- "pop"  %in% inputName
  # doTemp |> print()

  ###### Check NULL ######
  ### Check that data exists
  ### If it does, message user; otherwise, return NULL
  # has_i     <- inputDf |> length()
  nullData   <- inputDf |> is.null()
  if(nullData) return(inputDf)


  ###### Filter Data ######
  msgNA     <- paste0("Filtering out missing values...")
  inputDf   <- inputDf |> filter_all(all_vars(!(. |> is.na())))
  nrowData  <- inputDf |> nrow()
  namesDF   <- inputDf |> names()
  # hasData   <- !nullData & nullData |> ifelse(0, nrowData)
  hasData   <- nullData |> ifelse(0, nrowData)
  if(hasData) {msgN |> paste0(msg0_i, msg_i1) |> message()} else {return(NULL)}



  ###### Data Columns ######
  ### Get value column and id column
  if(valCol |> is.null()) valCol <- get_import_inputs_valCols(popArea=popArea)[[inputName]]
  if(idCol  |> is.null()) idCol  <- get_import_inputs_idCols (popArea=popArea)[[inputName]]


  ###### Input Info ######
  ### Get info for input i
  inputInfo <- co_info |> filter(inputType %in% inputName)

  ### Min and Max Values
  min_i     <- inputInfo |> pull(inputMin) |> unique()
  max_i     <- inputInfo |> pull(inputMax) |> unique()
  hasMin_i  <- !(min_i |> is.na())
  hasMax_i  <- !(max_i |> is.na())

  ### Message
  msgRange  <- case_when(hasMin_i ~ ">=" |> paste(min_i), .default="") |>
    paste0(case_when(hasMin_i & hasMax_i ~ " and "), .default="") |>
    paste0(case_when(hasMax_i ~ "<=" |> paste(max_i), .default=""))

  ### Column names
  yearCol_i <- "year"
  cols_i    <- idCol |> c(valCol) |> c(yearCol_i) |> unique()

  ###### ** Check Columns ######
  checkList <- inputDf |> check_valCols(inputName=inputName, valCol=valCol, tempType=tempType, popArea=popArea)
  valCol    <- checkList[["valCol"  ]]
  inputDf   <- checkList[["inputDf" ]]
  tempType  <- checkList[["tempType"]]
  popArea   <- checkList[["popArea" ]]
  # for(name_i in checkList) name_i |> assign(checkList[[name_i]]); rm(name_i)
  # rm(checkList)

  hasValCol <- !(valCol |> is.null())
  if(!hasValCol) return()

  ### Check other columns
  namesDF   <- inputDf |> names()
  whichCols <- cols_i %in% namesDF
  checkCols <- whichCols |> all()

  ### If columns don't pass, message user and return NULL
  ### Otherwise, continue
  if(!checkCols) {
    msg2_i |> paste0(msg_i3) |> paste0(namesDF[whichCols] |> paste(collapse=", "), "!") |> message()
    msg2_i |> paste0(msg_i2) |> message()
    return(NULL)
  } ### End if(!checkCols)

  ###### ** Check Values ######
  ### If data has required columns, check the value column:
  ### - Check that values are numeric
  ### - If values are numeric, check min and max values
  dataVals  <- inputDf  |> pull(all_of(valCol))
  nVals     <- dataVals |> length()
  checkNum  <- dataVals |> is.numeric() |> all()
  if(!checkNum) {
    msg2_i |> paste0(msg_i4) |> message()
    msg2_i |> paste0(msg_i2) |> message()
    return(NULL)
  } ### End if(!checkNum)

  ### Check that years are numeric
  checkYear  <- inputDf |> pull(all_of(yearCol_i)) |> is.numeric() |> all()
  if(!checkYear) {
    msg2_i |> paste0("Data for column year is not numeric!") |> message()
    msg2_i |> paste0(msg_i2) |> message()
    return(NULL)
  } ### End if(!checkNum)

  ### Check that appropriate years are present
  rangeYrs  <- c(yearMin, yearMax)
  hasMinYr  <- !(yearMin |> is.null())
  hasMaxYr  <- !(yearMax |> is.null())
  hasYears  <- hasMinYr | hasMaxYr

  if(hasYears) {
    ### Check
    whichYrs  <- c()
    if(hasMinYr) {
      nMinYrs <- inputDf  |> filter(year <= yearMin) |> pull(year) |> unique() |> length()
      if(nMinYrs) whichYrs <- whichYrs |> c(yearMin)
      rm(nMinYrs)
    } ### End if(hasMinYr)
    if(hasMaxYr) {
      nMaxYrs  <- inputDf |> filter(year <= yearMin) |> pull(year) |> unique() |> length()
      whichYrs <- whichYrs |> c(yearMax)
      rm(nMaxYrs)
    } ### End if(hasMaxYr)
    naYears   <- rangeYrs |> get_matches(y=whichYrs)
    checkYrs  <- (rangeYrs |> length()) == (naYears |> length())
    ### Messages
    if(!checkYrs) {
      msg_yrs <- "Data must have at least one non-missing value "
      msg_min <- "in or before the year " |> paste0(yearMin)
      msg_and <- (hasMinYr & hasMaxYr) |> ifelse("and at least one non-missing value ", "")
      msg_min <- "in or after the year " |> paste0(yearMax)
      msg2_i |> paste0(msg_yrs, msg_min, msg_and, msg_min, "!") |> message()
      return(NULL)
    } ### End if(!checkYrs)
  } ### End if(hasYears)


  ### Check that values are in range
  rangeVals <- dataVals |> range(na.rm=T)
  checkMin  <- case_when(hasMin_i ~ (dataVals >= min_i) |> any(), .default = T)
  checkMax  <- case_when(hasMax_i ~ (dataVals <= max_i) |> any(), .default = T)
  checkVals <- checkMin & checkMax

  if(!checkVals) {
    msg2_i |> paste0(msg_i5) |> message()
    msg2_i |> paste0("Values for column ", valCol, " must be ", msgRange, ".") |> message()
    msg2_i |> paste0(msg_i2) |> message()
    return(NULL)
  } ### End if(!checkNum)


  ###### Format Columns ######
  ### If checks pass, convert all id columns to character
  mutate0   <- idCol |> (function(x, y=yearCol_i){x[!(x %in% y)]})()
  inputDf   <- inputDf |> mutate_at(c(mutate0), as.character)


  ###### Convert Temperatures ######
  ### If doTemp & tempType == "global", mutate temperatures
  doConvert <- tempType == "global"
  if(doTemp & doConvert) {
    msg_temp <- paste0("Converting temperatures from global to CONUS using convertTemps(from=\"global\")...")
    msgN |> paste0(msg2_i, msg_temp) |> message()
    inputDf <- inputDf |> mutate_at(c(valCol), convertTemps, from=tempType)
  } ### End if(doTemp & doConvert)
  rm(doConvert)


  ###### Calculate State Population ######
  # ### Check regions, states, postal for correct values if pop input present
  # if(doPop) inputDf <- inputDf |> calc_import_pop(popArea=popArea, msgLevel=msgLevel + 1)
  if(doPop) {
    ### Calculate population
    doCalc  <- !("state" %in% popArea )
    if(doCalc) {
      msg_pop <- paste0("Calculating state population from ", popArea, " values...")
      paste0(msg1_i, msg_pop) |> message()
      if("region" %in% (inputDf |> names())) {
        inputDf <- inputDf |> filter(!(region |> str_detect("National")))
        inputDf <- inputDf |> filter(!(region |> is.na()))
      } ### End if("region" %in% (inputDf |> names()))
      inputDf <- inputDf |> calc_import_pop(popArea=popArea)
      inputDf <- inputDf |> filter(!(region |> is.na()))
    } ### End if(doCalc)
    ### Rename state population column
    # rename0  <- c("pop")
    # renameTo <- c("state_pop")
    # doRename <- rename0 %in% (inputDf |> names())
    # if(doRename) inputDf <- inputDf |> rename_at(c(rename0), ~renameTo)
  } ### End if(doPop)


  ###### Return ######
  paste0(msg0_i, "Values passed.") |> message()
  return(inputDf)
}




###### calc_import_pop ######
### Function to calculate state population from inputs
calc_import_pop <- function(
    df0      = NULL,    ### Population data
    popArea  = "state", ### One of: c("state", "regional", "conus", "national")
    msgLevel = 1        ### Level of messaging
){
  ###### Messages ######
  msgN     <- "\n"
  msg0_i   <- get_msg_prefix(level=msgLevel)
  msg1_i   <- get_msg_prefix(level=msgLevel + 1)
  msg2_i   <- get_msg_prefix(level=msgLevel + 2)
  msg_i1   <- paste0("Calculating state population from ") |> paste0(popArea, " level...")

  ###### Load Data from FrEDI ######
  ### Get objects from FrEDI name space
  ### Get input scenario info: co_info
  ### Get state info: co_states
  # fListNames <- c("co_states")
  # sListNames <- c("df_ratios")
  # for(name_i in fListNames){name_i |> assign(rDataList[["frediData"]][["data"]][[name_i]]); rm(name_i)}
  # for(name_i in sListNames){name_i |> assign(rDataList[["stateData"]][["data"]][[name_i]]); rm(name_i)}
  co_info   <- "co_inputScenarioInfo" |> get_frediDataObj("frediData")
  co_states <- "co_states"    |> get_frediDataObj("frediData")
  df_ratios <- "df_popRatios" |> get_frediDataObj("stateData")
  # df_ratios  <- df_ratios |> as_tibble()

  ###### Data Info ######
  hasPop   <- df0 |> length()
  namesPop <- df0 |> names()
  namesSta <- co_states    |> names()
  namesRat <- df_ratios |> names()

  ### Join data with pop ratios
  join0    <- namesPop |> (function(x, y=namesRat){x[x %in% y]})()
  df0      <- df0 |> left_join(df_ratios, by=c(join0))
  rm(join0)

  ###### Calculate Population ######
  hasState <- "state" %in% popArea
  calcPop  <- hasPop & !hasState
  # hasPop |> c(popArea, hasState, calcPop) |> print()
  if(calcPop) {
    msgN |> paste0(msg0_i, msg_i1) |> message()
    ### Check pop area info
    hasNat   <- "national" %in% popArea
    hasArea  <- "conus"    %in% popArea
    hasReg   <- "regional" %in% popArea

    ### Check other columns present
    hasNat   <- (hasNat   & "pop" %in% namesPop)
    hasArea  <- (hasArea  & "pop" %in% namesPop) | hasNat
    hasReg   <- (hasReg   & "pop" %in% namesPop) | hasNat | hasArea
    # hasNat |> c(hasArea, hasReg) |> print()

    ### Info on popArea
    doArea   <- hasNat
    doReg    <- hasArea
    doState  <- hasReg
    # doArea |> c(doReg, doState) |> print()

    ### If do area:  Calculate conus pop from national
    ### If do reg:   Calculate region pop from national
    ### If do state: Calculate region pop from national
    if(doArea ) {df0 <- df0 |> mutate(pop = pop * area2nat)}
    if(doReg  ) {df0 <- df0 |> mutate(pop = pop * reg2area)}
    if(doState) {df0 <- df0 |> mutate(pop = pop * state2reg)}
  } ### End if(calcPop)

  ###### Format Data ######
  ### Join data with region info, then check for columns, regions
  namesPop <- df0 |> names()
  join0    <- namesPop  |> (function(x, y=namesSta){x[x %in% y]})()
  df0      <- co_states |> left_join(df0, by=c(join0))
  df0      <- df0 |> filter_all(all_vars(!(. |> is.na())))
  df0      <- df0 |> check_pop_regions(msgLevel = msgLevel)
  popPass  <- !(df0 |> is.null())

  ### Select columns
  select0  <- c("region", "state", "postal", "year", "pop")
  if(popPass) {
    df0      <- df0 |> select(all_of(select0))
    df0      <- df0 |> arrange_at(c(select0))
  } ### End if(popPass)

  ###### Return ######
  return(df0)
}
