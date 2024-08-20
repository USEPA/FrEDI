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


###### get_import_inputs_idCols ######
### Info about which column contains info on region
get_import_inputs_idCols <- function(
    type0  = c("gdp", "pop", "temp", "slr"),
    popArea = "state" ### One of: c("state", "regional", "area", "national")
){
  ### Initialize vectors
  cols0  <- c("year")
  cols1  <- c()
  ### Check if population or ozone
  doPop0   <- "pop" %in% type0
  doO3     <- "o3"  %in% type0
  doState0 <- c("state") %in% c(popArea)
  doReg0   <- c("regional") %in% c(popArea)
  if(doPop0 & (doState0 | doReg0)) {
    cols1 <- doState0 |> ifelse("state", "region")
  } else if(doO3) {
    cols1 <- c("state", "model")
  } ### End if(doPop0)
  ### Update list
  cols0   <- cols1 |> c(cols0)
  ### Return
  return(cols0)
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
  msg_i2 <- paste0("File does not exist! Returning a null data frame for ", inputName |> paste0(" input", "."))
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
check_regions <- function(
    df0,                ### Tibble with population info
    module   = "fredi", #### "fredi", "sv", or "methane"
    msgLevel = 1        ### Level of messaging
){
  ###### Module Options ######
  module0    <- module |> tolower()
  inNames0   <- c("gdp", "pop")
  doMain     <- "fredi"   %in% module0
  doSV       <- "sv"      %in% module0
  doFredi    <- doMain | doSV
  doMethane  <- "methane" %in% module0
  dListSub0  <- doFredi |> ifelse("frediData", "package")
  dListName0 <- doFredi |> ifelse("rDataList", "listMethane")

  ###### Load Data from FrEDI ######
  ### Get objects from FrEDI name space
  ### Get input scenario info: co_info
  ### Get state info: co_states
  co_states <- "co_states"     |> get_frediDataObj(listSub=dListSub0, listName=dListName0)
  co_region <- "co_regions"    |> get_frediDataObj(listSub=dListSub0, listName=dListName0)

  ###### Messages ######
  msgN       <- "\n"
  msg0_i     <- get_msg_prefix(level=msgLevel)
  msg1_i     <- get_msg_prefix(level=msgLevel + 1)
  msg2_i     <- get_msg_prefix(level=msgLevel + 2)
  msg_i1     <- paste0("Checking inputs for unique ")
  msg_i2     <- paste0("Missing input data for the following " )
  msg_i3     <- paste0("Dropping inputs from outputs.")

  ###### Format Data ######
  ### Drop missing values
  df0        <- df0 |> filter_all(all_vars(!(. |> is.na())))

  ### Join states, regions
  ### Drop region from states, rename region_label
  join0     <- c("region")
  drop0     <- c("area", "us_area", "fips")
  if(doFredi) {
    renameAt0 <- c("region_id")
    renameTo0 <- c("region"   )
  } else {
    renameAt0 <- c()
    renameTo0 <- c()
  } ### End if(doFredi)
  co_region <- co_region |> rename_at(c(renameAt0), ~renameTo0)
  co_region <- co_region |> select(-any_of(drop0))
  co_states <- co_states |> select(-any_of(drop0))
  co_states <- co_states |> left_join(co_region, by=join0)
  rm(join0, renameAt0, renameTo0)
  ### Drop region, rename region label
  renameTo0 <- c("region")
  renameAt0 <- c(renameTo0)  |> paste0("_label")
  drop0     <- c(renameTo0)
  co_states <- co_states |> select(-any_of(drop0))
  co_states <- co_states |> rename_at(c(renameAt0), ~renameTo0)
  # co_states$region |> unique() |> print()
  rm(drop0, renameAt0, renameTo0)

  ### Join data with region info, then check for columns, regions
  move0    <- c("region", "state", "postal")
  # join0    <- df0       |> names() |> get_matches(y=co_states |> names()) |> get_matches(y=drop0, matches=F)
  join0    <- df0       |> names() |> get_matches(y=co_states |> names())
  # df0 |> glimpse(); co_states |> glimpse()
  df0      <- co_states |> left_join(df0, by=c(join0))
  df0      <- df0       |> relocate(all_of(move0))
  df0      <- df0       |> filter_all(all_vars(!(. |> is.na())))
  rm(move0, join0)

  ###### Check Data ######
  ### Check that there is data
  nullData   <- df0 |> is.null()
  nrowData   <- df0 |> nrow()
  hasData    <- !nullData & nullData |> ifelse(0, nrowData)


  ### Check whether data has regions, states to check
  namesState <- co_states |> names()
  namesPop   <- df0       |> names()
  namesCheck <- c("region", "state") |> get_matches(y=namesPop)
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
    join0   <- namesState |> get_matches(y=namesPop)
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
      naVals   <- refVals   |> get_matches(y=newVals, matches=F)
      passVals <- (naVals |> length()) == 0
      # naVals |> print()

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
    tempType = "conus", ### One of: c("conus", "global")
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
  # if(valCol |> is.null()) valCol <- get_import_inputs_valCols(popArea=popArea)[[inputName]]
  # if(idCol  |> is.null()) idCol  <- get_import_inputs_idCols (popArea=popArea)[[inputName]]

  ###### Check Values ######
  ### Initial names and values
  valCol0   <- valCol
  namesDf0  <- inputDf |> names()
  inName0   <- inputName
  ### Convert to lowercase
  valCol    <- valCol    |> tolower()
  namesDf   <- namesDf0  |> tolower()
  inName    <- inputName |> tolower()
  inputDf   <- inputDf   |> set_names(namesDf)
  ### Check if there is a match
  checkVCol <- valCol %in% namesDf
  vColMatch <- namesDf[checkVCol]
  nMatches  <- vColMatch |> length()
  ### Check if there are any string matches, and get number of matches
  if(!nMatches) {
    ### Messages
    msg_vcol0 <- paste0("Column ", "\"", valCol0, "\"", " not found in ", inName0, "file data!")
    msg_vcol1 <- paste0("Looking for columns with matches to the string ", "\"", inName0, "\"", "...")
    msg1_i |> paste0(msg_vcol0) |> message()
    # msg2_i |> paste0(msg_vcol1) |> message()
    ### Look for matches
    checkVCol1 <- namesDf |> str_detect(inName)
    vColMatch1 <- namesDf[checkVCol1]
    nMatches1  <- vColMatch1 |> length()
    matches1   <- nMatches1 > 1
    ### If matches, check if there are more than once match
    if(nMatches1) {
      ### Messages
      msg_vcol2 <- paste0(nMatches1, " match", matches1 |> ifelse("es", ""), " found!")
      msg1_i |> paste0(msg_vcol1, msg_vcol2) |> message()
      ### If number of matches is greater than one and doTemp:
      if(matches1) {
        ### - If doTemp:
        ###     - First, look for "conus" string and, if found, use that one
        ###     - If not, check for "global" string and, if found, use that one
        ###     - Otherwise, use first column
        if(doTemp) {
          ### Try to match based on the tempType
          # tempType |> print(); vColMatch1 |> print()
          findTemp2  <- vColMatch1 |> str_detect(tempType)
          vColMatch2 <- vColMatch1[findTemp2]
          nMatches2  <- vColMatch2 |> length()
          matches2   <- nMatches2 > 0
          ### If no matches found, message user and change type
          if(!matches2) {
            tempType2  <- ("conus" %in% tempType) |> ifelse("global", "conus")
            tempType2 |> print()
            findTemp2  <- vColMatch1 |> str_detect(tempType2)
            vColMatch2 <- vColMatch1[findTemp2]
            nMatches2  <- vColMatch2 |> length()
            matches2   <- nMatches2 > 0
            if(matches2) {
              msg_vcol3 <- paste0("Only a match for `tempType=", tempType2, "` found! Changing `tempType`...")
              msg2_i |> paste0(msg_vcol1, msg_vcol3) |> message()
              tempType  <- tempType2
              rm(msg_vcol3)
            } ### End if(matches2)
            rm(tempType2)
          } else{
            msg_vcol3 <- paste0("Match for `tempType=", tempType, "` found!")
            msg2_i |> paste0(msg_vcol1, msg_vcol3) |> message()
            rm(msg_vcol3)
          } ### End if(!matches2)
          ### Update matched column
          # vColMatch1 |> print(); vColMatch2 |> print()
          vColMatch <- vColMatch2[1]
          rm(findTemp2, vColMatch2, nMatches2, matches2)
        } ### End if(doTemp)

        ### - Else if doPop:
        ###     - First, look for "state" string and, if found, use that one
        ###     - Otherwise, use first column
        else if(doPop) {
          ### Find matches
          findState <- vColMatch1 |> str_detect("state")
          findReg   <- vColMatch1 |> str_detect("reg")
          findConus <- vColMatch1 |> str_detect("conus")
          findNat   <- vColMatch1 |> str_detect("nat")
          ### Columns
          vColState <- vColMatch1[findState]
          vColReg   <- vColMatch1[findReg  ]
          vColConus <- vColMatch1[findConus]
          vColNat   <- vColMatch1[findNat  ]
          ### Number of matches
          nState    <- vColState |> length()
          nReg      <- vColReg
          nConus    <- vColConus
          nNat      <- vColNat
          ### Matched column
          vColMatch2 <- case_when(
            nState  ~ vColState[1],
            nReg    ~ vColReg  [1],
            nConus  ~ vColConus[1],
            nNat    ~ vColNat  [1],
            .default = vColMatch1[1]
          ) ### End case_when
          ### Update popArea
          popArea    <- case_when(
            nState  ~ "state",
            nReg    ~ "regional",
            nConus  ~ "conus",
            nNat    ~ "national",
            .default = popArea
          ) ### End case_when
          ### Update matched column
          vColMatch <- vColMatch2
          ### Remove values
          rm(findState, vColState, nState)
          rm(findReg  , vColReg  , nReg  )
          rm(findConus, vColConus, nConus)
          rm(findNat  , vColNat  , nNat  )
        } ### End else if(doPop)

        ### - Otherwise, use first column
        else{
          vColMatch <- vColMatch1[1]
          ### End if(doTemp)
        } ### End else
      } ### End if(matches2)

      ### Rename columns
      # vColMatch |> print(); namesDf  |> print(); namesDf0 |> print()
      cMatch0   <- (namesDf %in% vColMatch) |> which()
      # cMatch0 |> print(); namesDf [-cMatch0] |> print(); namesDf0[-cMatch0] |> print()
      # namesDf[cMatch0] |> c(valCol, valCol0) |> print()
      renameAt0 <- namesDf[cMatch0]
      renameTo0 <- valCol
      doRename0 <- !(renameTo0 %in% renameAt0)
      # namesDf [-cMatch0] |> print(); namesDf0[-cMatch0] |> print()
      if(doRename0) {
        msg_vcol2 <- paste0("Using column ", "\"", namesDf0[cMatch0], "\"", ", and renaming to ", "\"", valCol0, "\"", "...")
        msg1_i |> paste0(msg_vcol2) |> message()
        # inputDf <- inputDf |> rename_at(c(vColMatch), ~valCol)
        inputDf <- inputDf |> rename_at(c(renameAt0), ~renameTo0)
      } ### End if(doRename0)
      ### Update columns
      namesDf [cMatch0] <- valCol
      namesDf0[cMatch0] <- valCol0
    } else{
      ### Message user
      msg_vcol2 <- paste0("No matches found! Exiting...")
      msg1_i |> paste0(msg_vcol1, msg_vcol2) |> message()
      return()
    } ### End if(nMatches)
  } ### End if(hasValCol) (no else)

  ###### Rename Columns ######
  renameAt0 <- namesDf
  renameTo0 <- namesDf0
  # renameAt0 |> print(); renameTo0 |> print(); inputDf |> glimpse()
  inputDf   <- inputDf |> rename_at(c(renameAt0), ~renameTo0)

  ###### Return ######
  list0 <- list()
  list0[["inputDf" ]] <- inputDf
  list0[["valCol"  ]] <- valCol0
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
    module    = "fredi",  #### "fredi", "sv", or "methane"
    msgLevel  = 2         ### Level of messaging
){
  # inputDf |> glimpse()

  ###### Module Options ######
  module0    <- module |> tolower()
  inNames0   <- c("gdp", "pop")
  doMain     <- "fredi"   %in% module0
  doSV       <- "sv"      %in% module0
  doFredi    <- doMain | doSV
  doMethane  <- "methane" %in% module0
  dListSub0  <- doFredi |> ifelse("frediData", "package")
  dListName0 <- doFredi |> ifelse("rDataList", "listMethane")

  ###### Load Data from FrEDI ######
  ### Get objects from FrEDI name space
  ### Get input scenario info: co_info
  ### Get state info: co_states
  co_info   <- "co_inputInfo"  |> get_frediDataObj(listSub=dListSub0, listName=dListName0)
  co_states <- "co_states"     |> get_frediDataObj(listSub=dListSub0, listName=dListName0)
  # co_region <- "co_regions"    |> get_frediDataObj(listSub=dListSub0, listName=dListName0)


  ### Get columns
  select0   <- c("inputName", "inputMin", "inputMax")
  co_info   <- co_info |> select(all_of(select0))
  co_info   <- co_info |> rename_at(c("inputName"), ~"inputType")
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
  doO3      <- "o3"   %in% inputName
  # doTemp |> print()

  ###### Check NULL ######
  ### Check that data exists
  ### If it does, message user; otherwise, return NULL
  # has_i     <- inputDf |> length()
  nullData   <- inputDf |> is.null()
  if(nullData) return(inputDf)
  #browser()

  ###### Filter Data ######
  msgNA     <- paste0("Filtering out missing values...")
  inputDf   <- inputDf |> filter_all(all_vars(!(. |> is.na())))
  nrowData  <- inputDf |> nrow()
  namesDf   <- inputDf |> names()
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
  namesDf   <- inputDf |> names()
  whichCols <- cols_i %in% namesDf
  checkCols <- whichCols |> all()

  ### If columns don't pass, message user and return NULL
  ### Otherwise, continue
  if(!checkCols) {
    msg2_i |> paste0(msg_i3) |> paste0(namesDf[whichCols] |> paste(collapse=", "), "!") |> message()
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
  #browser()
  if(!checkVals) {
    msg2_i |> paste0(msg_i5) |> message()
    msg2_i |> paste0("Values for column ", valCol, " must be ", msgRange, ".") |> message()
    if(doTemp) {
      paste0("Values outside of range will be changed to 0 and result in 0 impacts") |> message()

      inputDf <- inputDf |> mutate(
        !!sym(valCol)  := case_when(
          !!sym(valCol) < 0 ~ 0,
          !!sym(valCol) > 30 ~ 0,
          !!sym(valCol) >= 0 & !!sym(valCol) <= 30 ~ !!sym(valCol)
        ) ### End case_when
      ) ### End mutate
    } ### End if doTemp

    if(!doTemp){
      msg2_i |> paste0(msg_i2) |> message()
      return(NULL)}
  } ### End if(!checkNum)


  ###### Format Columns ######
  ### If checks pass, convert all id columns to character
  mutate0   <- idCol   |> get_matches(y=yearCol_i, matches=F)
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
      inputDf <- inputDf |> calc_import_pop(popArea=popArea, module=module)
      inputDf <- inputDf |> filter(!(region |> is.na()))
    } ### End if(doCalc)
    ### Rename state population column
    # rename0  <- c("pop")
    # renameTo <- c("state_pop")
    # doRename <- rename0 %in% (inputDf |> names())
    # if(doRename) inputDf <- inputDf |> rename_at(c(rename0), ~renameTo)
  } ### End if(doPop)

  ###### Check Regions ######
  if(doPop | doO3) {
    msg_reg <- paste0("Checking that all states, etc. are present...")
    paste0(msg1_i, msg_reg) |> message()
    inputDf  <- inputDf   |> check_regions(module=module, msgLevel=msgLevel + 1)
    regPass  <- !(inputDf |> is.null())
    ### Message if error
    msg_reg <- paste0("Warning: missing states in ", inputName, " inputs!", msgN, msg2_i, "Dropping ", inputName, " inputs...")
    if(!regPass) paste0(msg1_i, msg_reg) |> message()
  } ### End if(doPop | doO3)


  ###### Return ######
  paste0(msg0_i, "Values passed.") |> message()
  return(inputDf)
}




###### calc_import_pop ######
### Function to calculate state population from inputs
calc_import_pop <- function(
    df0      = NULL,    ### Population data
    popArea  = "state", ### One of: c("state", "regional", "conus", "national")
    module   = "fredi", #### "fredi", "sv", or "methane"
    msgLevel = 1        ### Level of messaging
){
  ###### Messages ######
  msgN     <- "\n"
  msg0_i   <- get_msg_prefix(level=msgLevel)
  msg1_i   <- get_msg_prefix(level=msgLevel + 1)
  msg2_i   <- get_msg_prefix(level=msgLevel + 2)
  msg_i1   <- paste0("Calculating state population from ") |> paste0(popArea, " level...")

  ###### Module Options ######
  module0    <- module |> tolower()
  inNames0   <- c("gdp", "pop")
  doMain     <- "fredi"   %in% module0
  doSV       <- "sv"      %in% module0
  doFredi    <- doMain | doSV
  doMethane  <- "methane" %in% module0
  dListSub0  <- doFredi |> ifelse("frediData", "package")
  dListName0 <- doFredi |> ifelse("rDataList", "listMethane")

  ###### Load Data from FrEDI ######
  ### Get objects from FrEDI name space
  ### Get input scenario info: co_info
  ### Get state info: co_states
  co_info   <- "co_inputInfo"  |> get_frediDataObj(listSub=dListSub0, listName=dListName0)
  co_states <- "co_states"     |> get_frediDataObj(listSub=dListSub0, listName=dListName0)
  df_ratios <- "popRatiosData" |> get_frediDataObj("scenarioData")

  ###### Data Info ######
  hasPop   <- df0 |> length()
  namesPop <- df0 |> names()
  namesSta <- co_states |> names()
  namesRat <- df_ratios |> names()

  ### Join data with pop ratios
  join0    <- namesPop |> get_matches(y=namesRat)
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


  ### Select columns
  select0  <- c("region", "state", "postal", "year", "pop")
  df0      <- df0 |> select(all_of(select0))
  df0      <- df0 |> arrange_at(c(select0))
  # if(popPass) {
  #   df0      <- df0 |> select(all_of(select0))
  #   df0      <- df0 |> arrange_at(c(select0))
  # } ### End if(popPass)

  ###### Return ######
  return(df0)
}
