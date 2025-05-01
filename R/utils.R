###### Helpers ######
### Get message prefix
get_msgPrefix <- function(level=1, newline=FALSE){
  ### Message new line
  msgN0  <- newline |> ifelse("\n", "")
  ### Message indents
  msgX0  <- "\t" |> rep(level) |> paste(collapse="")
  ### Message prefix
  msg0   <- msgN0 |> paste0(msgX0)
  ### Return
  return(msg0)
}

### Function to format module
fun_moduleDataStr <- function(module0="fredi"){
  module0 |> tolower() |> paste0("Data")
}

### Determine whether something is default or custom
get_returnListStatus <- function(cond0=TRUE){
  # cond0 |> ifelse("Default", "Custom")
  cond0 |> ifelse("Default", "Custom")
}

### Function to make it easier to subset a vector
# @export
# get_matches <- function(
#     x,      ### Vector to subset
#     y,      ### Vector to check x against
#     matches = TRUE,    ### Whether to subset to matches (TRUE) or non-matches (FALSE)
#     type    = "values" ### c("values", "matches", "which")
# ){
#   ### Check which type to return
#   type      <- type |> tolower()
#   doValues  <- "values"  %in% type
#   doMatches <- "matches" %in% type
#   doWhich   <- "which"   %in% type
#   ### Get matches
#   set0      <- x %in% y
#   ### Get anti-match if matches = FALSE
#   if(!matches) set0 <- !set0
#   # ### Get which values to return
#   # which0    <- set0 |> which()
#   # ### Get subset
#   # x0        <- x[set0]
#   ### If doValues, return values
#   ### If doMatches, return matches
#   ### If doWhich, return which
#   if     (doValues) return(x[set0])
#   else if(doWhich ) return(set0 |> which())
#   else              return(set0)
#   ### End function
# }
### Match values
### x = Vector to subset
### y = Value(s) to check against (if multiple values, then must be same length as x)
### comp0 = Type of comparison, one of:
### - in  = Check if x in y values
### - eq  = Check if equal...
### - geq = Check if greater than or equal to
### - leq = Check if less than or equal to
### - g   = Check if greater than
### - l   = Check if less than or equal to
### matches = Whether to subset to matches (TRUE) or non-matches (FALSE)
### type    = "values" ### c("values", "matches", "which")
#' @export
get_matches <- function(
    x,
    y,
    matches = TRUE,
    type    = "values",
    comp    = "in",
    ...
){
  ### Convert to lower
  comp  <- comp |> tolower()
  ### Which comparison to do
  doIn  <- "in" %in% comp
  doEq  <- "eq"  %in% comp
  doGeq <- "geq" %in% comp
  doLEq <- "leq" %in% comp
  doG   <- "g"   %in% comp
  doL   <- "l"   %in% comp
  z     <- T
  ### Which ones to do
  if(doIn){
    z <- x %in% y
  } else if(doEq) {
    z <- x == y
  } else {
    if(doGeq) z <- z & (x >= y)
    if(doLeq) z <- z & (x <= y)
    if(doG  ) z <- z & (x >  y)
    if(doL  ) z <- z & (x <  y)
  } ### End if(doEq)
  ### Reverse
  if(!matches) z <- !z
  ### Check which type to return
  type      <- type |> tolower()
  doValues  <- "values"  %in% type
  doMatches <- "matches" %in% type
  doWhich   <- "which"   %in% type
  if     (doValues) return(x[z])
  else if(doWhich ) return(z |> which())
  else              return(z)
}


### Get matches for a list
get_matches_list <- function(
    x,      ### List to subset
    y,      ### Vector to match values against
    matches = TRUE    , ### Whether to subset to matches (TRUE) or non-matches (FALSE)
    type    = "values", ### c("values", "matches", "which"),
    lType   = "name"  , ### "name" or "value"
    # method  = "val"   , ### "val" or "num"
    comp    = "in"      ### Type of comparison, if numeric
){
  ### Check which type to return
  lType     <- lType  |> tolower()
  doNames   <- "names"  %in% lType
  doValues  <- "values" %in% lType
  # ### Which method to use
  # method    <- method |> tolower()
  # doNum     <- "num"    %in% method
  # doVal     <- "val"    %in% method

  ### Get matches
  if(doNames) xVals <- x |> names()
  else        xVals <- x |> unlist()

  ### Get matches
  zVals     <- xVals |> get_matches(y=y, matches=matches, type="matches", comp=comp)

  ### Get anti-match if matches = FALSE
  if(!matches) set0 <- !set0

  ### Check which type to return
  type      <- type |> tolower()
  doValues  <- "values"  %in% type
  doMatches <- "matches" %in% type
  doWhich   <- "which"   %in% type
  if     (doValues) return(xVals[zVals])
  else if(doWhich ) return(zVals |> which())
  else              return(zVals)
  ### End function
}

### This function makes it easier to get data objects from the sysdata.rda file
get_frediDataObj <- function(
    listName = "frediData", ### Could be ScenarioData
    listSub  = NULL, ### Sublist name
    listItem = NULL,
    listall  = FALSE,
    pkg      = "FrEDI",
    lib.loc  = .libPaths()[1], ### Library path to look for packages
    silent   = TRUE,
    msg0     = 0
){
  ### Messaging
  msgUser <- !silent
  msgN    <- "\n"
  msg1    <- msg0 + 1
  msg2    <- msg0 + 2

  ### Check if the item exists in memory, and if it does, then evaluate that item
  ### Otherwise, get the value from the namespace
  ### Conditionals
  exists0 <- listName |> exists()
  # exists0 |> print()

  ### If the listname exists in the name space, parse it
  ### Otherwise, grab it from a package name space
  if(exists0) {
    new_x <- parse(text=listName) |> eval()
  } else      {
  # if(!exists0) {
    ### Check if package & list name
    pkgList0    <- lib.loc |> installed.packages()
    pkgExists0  <- pkg %in% pkgList0
    if(!pkgExists0) {
      msg0 |> get_msgPrefix() |> paste0("Looking for package '", pkg, "' in `lib.loc= `", lib.loc, "'`...") |> message()
      msg1 |> get_msgPrefix() |> paste0("Package doesn't exist...") |> message()
      msg1 |> get_msgPrefix() |> paste0("Exiting...") |> message()
      return()
    } else          {
      new_x <- getFromNamespace(listName, ns=pkg)
    } ### End ### End if(!pkgExists0)
  } ### End else(exists0)

  ### Check if there are Sub lists or items
  # new_x |> names() |> print()
  doSub   <- listSub  |> length()
  doItem  <- listItem |> length()
  # listSub |> print(); listItem |> print(); c(doSub, doItem) |> print();

  ### Subset list to sublist
  if(doSub ) new_x <- new_x[[listSub]]

  ### Subset list to subitem
  if(doItem) new_x <- new_x[[listItem]]

  ### If listall, return list of names
  if(listall) new_x <- new_x |> names()

  ### Return
  return(new_x)
} ### End get_frediDataObj


### fun_extendVals
### Function to extend values out to a particular year
fun_extendVals <- function(
    df0,  ### Data to extend
    from0 = NULL,
    to0   = 2300, ### Year to extend to
    # cols0 = c() , ### Columns to extend values
    sort0 = NULL  ### Columns for sorting
){
  ### Columns for sorting
  doFrom <- !(from0 |> is.null())
  if(doFrom) {from0 <- df0 |> pull(year) |> max()}
  df1    <- df0 |> filter(year == from0) |> select(-c("year"))
  df2    <- tibble(year = (from0 + 1):to0)
  df3    <- df1 |> cross_join(df2)
  rm(df1, df2)

  ### Bind data
  df0    <- df0 |> bind_rows(df3)
  rm(df3)

  ### Format data
  names0 <- df0 |> names()
  doSort <- !(sort0 |> is.null())
  if(doSort) {sort0 <- names0}
  df0    <- df0 |> arrange_at(c(sort0))

  ### Return data
  gc()
  return(df0)
}


### get_scenario_id
### Function to standardize the creation of the scenario_id
get_scenario_id <- function(
    df0,
    include0 = c("region", "state", "postal", "model"), ### Character vector of column names to include
    idCol0   = c("scenario_id"), ### Name of new column
    sep0     = "_",
    silent   = TRUE,
    msg0     = 0
){
  ### Messaging
  msgUser <- !silent
  msgN    <- "\n"
  msg1    <- msg0 + 1
  msg2    <- msg0 + 2
  sepM    <- ", " ### Comma for collapsing lists

  ### Columns to include
  main0   <- c("sector", "variant", "impactType", "impactYear")
  cols0   <- main0  |> c(include0) |> unique()

  ### Subset cols0 to those in data
  names0  <- df0 |> names()
  which0  <- cols0 |> get_matches(y=df0 |> names(), matches=T, type="matches")
  cols0   <- cols0[ which0]
  naCols0 <- cols0[!which0]
  naNum0  <- naCols0 |> length()
  if(naNum0){
    msg0 |> get_msgPrefix(newline=F) |> paste0("In get_scenario_id:") |> message()
    msg1 |> get_msgPrefix(newline=F) |> paste0("Data is missing columns '", naCols0 |> paste(collapse=sepM), "'...") |> message()
    msg1 |> get_msgPrefix(newline=F) |> paste0("Creating ", idCol0 , " from columns '", cols0 |> paste(collapse=sepM), "'...") |> message()
  } ### End if(naNum0)
  ### Select columns and get scenario IDs
  # vals0  <- df0 |> select(all_of(cols0))
  vals0  <- df0 |>
    ungroup() |>
    select(all_of(cols0)) |>
    apply(1, function(x){
      x |> as.vector() |> paste(collapse=sep0)
    }) |> unlist()
  # vals0 |> head() |> glimpse()
  df0    <- df0   |>
    mutate(id = vals0) |>
    rename_at(c("id"), ~idCol0)
  # df0    <- df0   |> ungroup()
  # df0 |> glimpse()
  ### Return
  # gc()
  return(df0)
}

### Function to get National values from state
get_nationalRegion <- function(
    df0  = "controlData" |> get_frediDataObj("co_states"),
    col0 = "area",
    str0 = "US"
){
  df0 |> filter_at(c(col0), function(x, y=str0){x %in% y})
}

### Get sectors
check_inputSectors <- function(
  sectorList = NULL,
  module0    = "fredi",
  msg0       = 0
){
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2

  ### Values
  module0      <- module0 |> tolower()
  modData0     <- module0 |> fun_moduleDataStr()

  ### Initialize status list
  list0        <- list()

  ### Default sectors tibble and lists
  co_sectors   <- modData0 |>
    get_frediDataObj("configData", "co_sectors", msg0=msg0) |>
    mutate(sectorLC = sector_label |> tolower())
  sectorIDs0   <- co_sectors |> pull(sector) |> unique()
  nSectors0    <- sectorIDs0 |> length()
  # sectorLbls0  <- co_sectors |> pull(sector_label) |> unique()

  ### If no inputs are provided, update the list
  hasSectors   <- sectorList |> length()
  if(!hasSectors) {
    list0[["status"]] <- get_returnListStatus()
    list0[["ids0"  ]] <- co_sectors |> pull(sector) |> unique()
    list0[["lbls0" ]] <- co_sectors |> pull(sector_label) |> unique()
    return(list0)
  } ### End if(!hasSectors)

  ### Get unique and covert to lowercase
  ### Create a tibble with input values
  sectorList   <- sectorList |> unique()
  nInputs      <- sectorList |> length()
  dfSectors    <- tibble() |>
    mutate(sector0  = sectorList) |>
    mutate(sectorLC = sector0 |> tolower())

  ### Otherwise, create a tibble with input values
  join0        <- "sectorLC"
  dfMatch      <- dfSectors |> full_join(co_sectors, by=join0)
  nRow0        <- dfMatch |> nrow()

  ### Matched sectors, user-supplied sectors not in data, and data sectors un-matched in user inputs
  # sectorList |> print(); sectorLbls |> print()
  naSectors0   <- dfMatch  |> filter( (sector |> is.na())) |> pull(sector0) |> unique()
  avlSectors0  <- dfMatch  |> filter(!(sector |> is.na()) &  (sector0 |> is.na())) |> pull(sector_label) |> unique()
  # anySectors0  <- dfMatch  |> filter(!(sector |> is.na()) & !(sector0 |> is.na())) |> pull(sector_label) |> unique()
  # anySectors0  <- dfMatch  |> filter(!(sector |> is.na()) & !(sector0 |> is.na())) |> pull(sector_label) |> unique()
  dfMatch      <- dfMatch  |> filter(!(sector |> is.na()) & !(sector0 |> is.na()))
  anySectors0  <- dfMatch  |> pull(sector_label) |> unique()
  sectorIDs    <- dfMatch  |> pull(sector) |> unique()

  ### Number of sectors
  # nSectors0    <- co_sectors  |> length()
  hasNa0       <- naSectors0  |> length()
  hasAny0      <- anySectors0 |> length()
  hasAll0      <- hasAny0 == nSectors0
  # hasAll0      <- anySectors0 == nSectors0

  ### Strings for unmatched and available sectors
  naSectStr0  <- "'" |> paste0(naSectors0  |> paste(collapse="', '")) |> paste0("'")
  avlSectStr0 <- "'" |> paste0(avlSectors0 |> paste(collapse="', '")) |> paste0("'")
  anySectStr0 <- "'" |> paste0(anySectors0 |> paste(collapse="', '")) |> paste0("'")

  ### Otherwise, message user about values
  if(hasNa0) {
    ### Message user
    msg0 |> get_msgPrefix(newline=T) |> paste0("Warning! Error in `sectorList`:") |> message()
    ### Message NA sectors
    msg1 |> get_msgPrefix() |> paste0("Impacts are not available for sectors: ") |> message()
    msg2 |> get_msgPrefix() |> paste0(naSectStr0, "...") |> message()
    ### Message Available sectgrs
    msg1 |> get_msgPrefix(newline=T) |> paste0("Available sectors: ") |> message()
    msg2 |> get_msgPrefix() |> paste0(avlSectStr0, "'") |> message()
  } ### End if(length(missing_sectors)>=1)

  ### If there are any matches, message the user and update the list
  if(hasAny0) {
    msg0 |> get_msgPrefix(newline=T) |> paste0("Getting impacts for sectors: ") |> message()
    msg1 |> get_msgPrefix()|> paste0("'", sectorList |> paste(collapse="', '"), "'") |> message()
  } else{
    msg1 |> get_msgPrefix(newline=T) |> paste0("Exiting...") |> message()
    return()
  } ### End if(anySectors)

  ### If all sectors are present, update list
  if(hasAny0 == nSectors0) {
  # if(!(hasAny0) | hasAny0 == nSectors0) {
    list0[["status"]] <- get_returnListStatus()
    list0[["ids0"  ]] <- co_sectors |> pull(sector) |> unique()
    list0[["lbls0" ]] <- co_sectors |> pull(sector_label) |> unique()
    # return(list0)
  } else if(hasAny0) {
    list0[["status"]] <- FALSE |> get_returnListStatus()
    list0[["ids0"  ]] <- sectorIDs
    list0[["lbls0" ]] <- anySectors0
    # return(list0)
  }  ### End if(hasAll0 | !(hasAny0))

  ### Return
  return(list0)
}

### Check agg levels
check_aggLevels <- function(
    aggLevels,
    module0 = "fredi",
    msg0    = 0
){
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  ### Values
  module0    <- module0 |> tolower()
  modData0   <- module0 |> fun_moduleDataStr()
  ### Aggregation levels
  aggList0   <- modData0 |> get_frediDataObj("configData", "aggList0", msg0=msg1)  |> tolower()
  aggLevels  <- aggLevels |> tolower()
  aggNone0   <- "none" %in% aggLevels
  aggAll0    <- "all"  %in% aggLevels
  aggLvlsN   <- 0
  ### If none specified, no aggregation (only SLR interpolation)
  ### Otherwise, aggregation depends on length of agg levels
  if      (aggNone0 ) {
    aggLevels <- "none"
  } else if (aggAll0) {
    aggLevels <- aggList0
  } else{
    aggLevels <- aggLevels |> get_matches(y=aggList0)
    aggLvlsN  <- aggLevels |> length()
  } ### End if (aggAll0 )
  doAgg      <- aggAll0 | aggLvlsN
  rm(aggList0, aggNone0, aggAll0, aggLvlsN)
  ### Add to list
  return(aggLevels)
}

### Check if list elements are NULL
check_nullListElement <- function(
    name0,
    list0
){
  list0[[name0]] |> is.null()
}

### Function to drop null elements from a list
drop_nullListElements <- function(
    list0,
    matches = TRUE
){
  ### Figure out which elements are null, then subset list
  isNull0   <- list0 |> map(is.null) |> unlist()
  if(!matches) match0 <- !isNull0
  else         match0 <- isNull0
  list0     <- list0 [match0]
  ### Return list
  return(list0)
}


## Format Input Scenarios ----------------
### This function helps format input scenarios
# if(doTemp0) {
#   df0 <- df0 |> group_map(
#     .x |> format_tempData_byGroup(
#       .y        = .y,
#       xCol0     = yrCol0,
#       yCol0     = valCol0,
#       xOut0     = minYr0:maxYear,
#       tempType0 = "conus",
#       method0   = "linear",
#       rule0     = 1
#     ) ### End format_tempData_byGroup
#   ) |> bind_rows()
# } else{
#   df0 <- df0 |> group_map(
#     interpolate_byGroup,
#     xCol0   = yrCol0,
#     yCols0  = valCol0,
#     xOut0   = minYear:maxYear,
#     method0 = "linear",
#     rule0   = 1
#   ) |> bind_rows()
# } ### End if(doTemp0)

format_inputScenarios <- function(
    df0,       ### Scenario input data frame to format
    name0,     ### Name of input c("temp, "slr", "gdp", "pop")
    minYear,   ### Minimum year
    maxYear,   ### Maximum year
    # valCols0,  ### Value columnsm,
    # idCols0,   ### ID columns
    info0,     ### Other info
    tempType = "conus",
    # hasInput0,  ### Whether the user provided an input
    msg0     = 0
    # msgLvl0  = 0
){
  ### Messaging
  msgUser    <- !silent
  msgN       <- "\n"
  msg1       <- msg0 + 1

  ### If no input provided, return empty value
  hasInput0 <- df0 |> length()
  if(!hasInput0) { return() }

  ### Columns
  yrCol0    <- "year"

  ### Values
  info0     <- info0 |> filter(inputName %in% name0)
  label0    <- info0 |> pull(inputType)
  valCol0   <- info0 |> pull(valueCol)
  refYr0    <- info0 |> pull(refYear)
  doTemp0   <- info0 |> pull(doTemp0)
  doRef0    <- !(refYr0 |> is.na())
  minYr0    <- doRef0 |> ifelse(refYr0, minYear)

  ### Filter data and arrange at ID columns
  msg0 |> get_msgPrefix(newline=F) |> paste0(label0, " scenario from user inputs...")
  idCols0   <- df0 |> names() |> get_matches(y=c(yrCol0, valCol0), matches=F)
  df0       <- df0 |>
    filter_all(all_vars(!(. |> is.na()))) |>
    arrange_at (c(idCols0, yrCol0)) |>
    group_by_at(c(idCols0))

  ### Zero values at ref year if doRef0
  df0       <- df0 |> zero_out_scenario(
    refYr0  = refYr0,
    xCol0   = yrCol0,
    yCol0   = valCol0,
    idCols0 = idCols0
  ) ### End zero_out_scenario
  # if(doRef0) {
  #   df0 <- df0 |> filter(year > refYr0)
  #   df0 <- df0 |> summarize_at(c(yrCol0), min) |>
  #     mutate_at(c(yrCol0), function(x, y=refYr0){y}) |>
  #     mutate(y = 0) |>
  #     rename_at(c("y"), ~valCol0) |>
  #     bind_rows(df0) |>
  #     arrange_at (c(idCols0, yrCol0))
  # } ### End if(doRef0)


  ### Check if interpolation is required
  ### If so, interpolate values
  df0 <- df0 |> group_map(
    .x |> interpolate_byGroup(
      .y      = .y,
      xCol0   = yrCol0,
      yCols0  = valCol0,
      xOut0   = minYr0:maxYear,
      method0 = "linear",
      rule0   = 1
    ) ### End interpolate_byGroup()
  ) |> bind_rows()

  ### Return
  return(df0)
}



### Function to rename physical drivers
rename_physDrivers <- function(
    nameX,
    listX,
    infoX,
    idColX = "inputName",
    yColX  = "driverValue"
){
  colX <- infoX |> filter(inputName %in% nameX) |> pull(valueCol)
  dfX  <- listX[[nameX]] |> rename_at(c(colX), ~yColX)
  return(dfX)
}

### Combine driver scewnarios
combine_physDrivers <- function(
    list0, ### List of driver scenarios
    info0,
    idCol0   = "inputName",
    idColNew = "driverName",
    yColNew  = "driverValue",
    msg0     = 0
){
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2

  ### Rename columns
  names0   <- list0 |> names()

  ### Iterate over list and change name of columns, then bind
  df0      <- names0 |> map(
    rename_physDrivers,
    listX  = list0,
    infoX  = info0,
    idColX = idCol0,
    yColX  = yColNew
  ) |>
    set_names(names0) |>
    bind_rows(.id="inputName")
  rm(list0)

  ### Add model types info
  ### Model types info
  dfMTypes <- "controlData" |>
    get_frediDataObj("co_modelTypes", msg0=msg1) |>
    filter(inputName %in% names0)

  ### Join data and info
  join0    <- "inputName"
  from0    <- "inputName"
  to0      <- "driverName"
  df0      <- df0 |>
    left_join(dfMTypes, by=join0) |>
    rename_at(c(idCol0), ~idColNew)
  rm(dfMTypes)

  ### Return
  # gc()
  return(df0)
}

# combine_physDrivers <- function(
    #     list0 ### List of driver scenarios
#     # list0, ### List of driver scenarios
#     # info0, ### Dataframe with scenario info, e.g.: df_inputInfo
#     # info1 = get_frediDataObj("co_modelTypes", "controlData")
# ){
#   ### Rename columns
#   names0   <- list0 |> names()
#   list0    <- names0 |> map(function(name0){
#     doTemp0 <- name0 |> str_detect("temp")
#     info0   <- "controlData" |> get_frediDataObj("co_modelTypes") |>
#       filter(inputName %in% name0) |>
#       rename_at(c("inputName"), ~c("driverName"))
#     valCol0 <- info0 |> pull(valueCol) |> paste0(case_when(doTemp0 ~ "_conus", .default=""))
#     ### Rename and select columns
#     select0 <- c("year", "driverValue")
#     df0     <- list0[[name0]] |>
#       rename_at(c(valueCol0), ~"driverValue") |>
#       select(all_of(select0)) |>
#       cross_join(info0)
#     ### Return
#     return(df0)
#   }) |> bind_rows()
#
#   ### Return
#   gc()
#   return(df0)
# }



## Interpolate annual values ----------------
### Interpolate Annual Values
interpolate_byGroup <- function(
    .x,       ### Data, filtered to a scenario
    .y,       ### Group info
    xCol0   = "year",
    yCols0  = "value",
    xOut0   = NULL,
    from0   = NULL,
    to0     = NULL,
    by0     = 1,
    method0 = "linear",
    rule0   = 1
){
  ### Get values
  names0   <- .x     |> names()
  xCol0    <- xCol0  |> get_matches(y=names0)
  yCols0   <- yCols0 |> get_matches(y=names0)

  ### Arrange data
  select0  <- c(xCol0, yCols0)
  .x       <- .x |>
    select(any_of(select0)) |>
    distinct() |>
    filter_at (c(select0), function(x){!(x |> is.na())}) |>
    arrange_at(c(xCol0))
  names0   <- .x |> names()
  rm(select0)

  ### Get from0, to0, and/or xOut0 if any are NULL
  xIn0     <- .x |> pull(all_of(xCol0))
  doXOut0  <- xOut0 |> is.null()
  doFrom0  <- from0 |> is.null()
  doTo0    <- to0   |> is.null()
  from0    <- case_when(doFrom0 ~ xIn0 |> min(na.rm=T), .default=from0)
  to0      <- case_when(doTo0   ~ xIn0 |> max(na.rm=T), .default=to0  )
  if(doXOut0) xOut0 <- seq(from0, to0, by=by0)

  ### Check if values require interpolation
  # doInterp <- ifelse(unique(df0$year) == unique(minYear:maxYear), FALSE, TRUE)
  doInterp <- (xOut0 %in% xIn0) |> all() |> ifelse(F, T)
  if(!doInterp) {
    df0 <- .x
  } else {
    ### Interpolate values, add to tibble, rename columns, and join with group data
    ### - Initialize values
    df0      <- tibble(x = xOut0) |> rename_at(c("x"), ~xCol0)
    ### - Interpolate values and bind
    df0      <- yCols0 |> map(function(
    colY,
    valsX = xIn0,
    outX  = xOut0
    ){
      valsY <- .x |> pull(all_of(colY))
      dfY   <- valsX |>
        approx(y=valsY, xout=outX, method=method0, rule=rule0) |>
        as.data.frame() |>
        as_tibble() |>
        select(c("y")) |>
        rename_at(c("y"), ~colY)
    }) |>
      bind_cols() |>
      bind_cols(df0) |>
      relocate(any_of(xCol0))
  } ### End if(!doInterp)

  ### Cross join values
  df0      <- .y |> cross_join(df0)

  ### Return
  return(df0)
}


### Interpolate temps by group
format_tempData_byGroup <- function(
    .x,       ### Data, filtered to a scenario
    .y,       ### Group info
    xCol0     = "year",
    yCol0     = "temp_C",
    xOut0     = NULL,
    from0     = NULL,
    to0       = NULL,
    by0       = 1,
    tempType0 = "global",
    # argCol0   = "inputArgVal", ### Column to look for tempType
    method0   = "linear",
    rule0     = 1,
    globalStr = "global",
    conusStr  = "conus"
){
  # .y |> glimpse(); .x |> glimpse();
  ### Values and columns
  doGlobal  <- tempType0 |> str_detect(globalStr)
  tempType1 <- tempType0
  tempType2 <- case_when(doGlobal ~ conusStr, .default = globalStr)

  ### Interpolate values
  .x        <- .x |> interpolate_byGroup(
    .y      = .y,
    xCol0   = xCol0,
    yCols0  = yCol0,
    xOut0   = .x |> pull(year),
    from0   = from0,
    to0     = to0,
    method0 = method0,
    rule0   = rule0
  ) ### End interpolate_byGroup
  # .x |> glimpse()

  ### Interpolate values, convert temperatures, calculate slr_cm
  ### Calculate SLR
  old0      <- yCol0 |> c("y2")
  new0      <- c("temp_C_" |> paste0(c(tempType1, tempType2)))
  .x        <- .x |>
    mutate(y2 = .x |> pull(all_of(yCol0)) |> convertTemps(from=tempType0)) |>
    rename_at(c(old0), ~new0)
  # .x |> glimpse()

  ### Slr values
  dfSlr     <- .x |>
    pull(temp_C_global) |>
    temps2slr(years=.x |> pull(all_of(xCol0))) |>
    rename_at(c("year"), ~xCol0)

  ### Add SLR values and cross join with y
  .x        <- .x |> left_join(dfSlr, by=xCol0)
  rm(dfSlr)

  ### Return
  return(.x)
}


### Function to zero out values for temperature and SLR scenarios
zero_out_scenario <- function(
    df0, ### Data
    refYr0,
    xCol0   = "year",
    yCol0   = "value",
    idCols0 = c()
){
  ### Values & Columns
  sort0 <- idCols0 |> c(xCol0)
  ### If !doRef, return data as is
  doRef <- !(refYr0) |> is.na()
  if(!doRef) return(df0)
  ### Otherwise, zero out values
  ### Filter to values greater than the ref year
  df1   <- df0 |> filter_at(c(xCol0), function(x, y=refYr0){x > y})
  ### Get zero values
  df0   <- df0 |>
    summarize_at(c(xCol0), min) |>
    mutate_at(c(xCol0), function(x, y=refYr0){y}) |>
    mutate(y = 0) |>
    rename_at(c("y"), ~yCol0) |>
    bind_rows(df1) |>
    arrange_at (c(sort0))
  rm(df1)
  ### Return
  return(df0)
}



## Calculate national population
### df0 is a data frame with columns c("region", "state", "postal", "year", "pop")
calc_nationalPop <- function(df0){
  ### Columns
  yrCol0   <- c("year")
  popCol0  <- c("pop")
  natCol0  <- c("national_pop")
  regCols0 <- c("region", "state", "postal")
  ### Check whether national values are present
  regions0 <- df0 |> pull(region) |> unique()
  natStr0  <- "National"
  hasNat   <- regions0 |> str_detect(natStr0) |> any()
  ### Drop missing values
  df0      <- df0 |> filter_all(all_vars(!(. |> is.na())))
  ### If national is present, filter to those values
  ### Otherwise, filter to opposite
  select0  <- c(yrCol0, popCol0)
  if(hasNat) {
    df0      <- df0 |> filter( (region |> str_detect(natStr0)))
  } else {
    select0  <- regCols0 |> c(select0)
    df0      <- df0 |> filter(!(region |> str_detect(natStr0)))
  } ### End if(hasNat)
  ### Get distinct values
  df0      <- df0 |> select(all_of(select0)) |> distinct()
  ### If !hasNat, then group by year and sum population
  if(!hasNat) {
    df0      <- df0 |>
      group_by_at (c(yrCol0)) |>
      summarize_at(c(popCol0), sum, na.rm=T) |> ungroup()
  } ### End if(!hasNat)
  ### Rename columns
  df0      <- df0 |> rename_at(c(popCol0), ~c(natCol0))
  ### Return
  return(df0)
}


### Create national scenario from national population and GDP information
### gdp0 is a data frame with columns c("year", "gdp_usd")
### pop0 is a data frame with columns c("region", "state", "postal", "year", "pop")
# create_nationalScenario <- function(
#     gdp0,
#     pop0,
#     # gdp0    = "scenarioData" |> get_frediDataObj("gdp_default"),
#     # pop0    = "scenarioData" |> get_frediDataObj("pop_default"),
#     natPop0 = NULL
# ){
#   ### If national population is NULL, calculate national population
#   nullNpop <- natPop0 |> is.null()
#   if(nullNpop) natPop0 <- pop0 |> calc_nationalPop()
#   ### Columns
#   yrCol0   <- c("year")
#   popCol0  <- c("pop")
#   natCol0  <- c("national_pop")
#   gdpCols0 <- c("gdp_usd", "gdp_percap")
#   regCols0 <- c("region", "state", "postal")
#   ### Select columns
#   colsG0   <- c(yrCol0, gdpCols0)
#   colsP0   <- c(regCols0, yrCol0, popCol0)
#   colsN0   <- c(yrCol0, natCol0)
#   gdp0     <- gdp0 |> select(any_of(colsG0))
#   pop0     <- pop0 |> select(any_of(colsP0))
#   ### Join GDP and national population by year
#   nat0     <- gdp0 |> left_join(natPop0, by=yrCol0)
#   rm(gdp0)
#   ### Calculate GDP per capita
#   nat0     <- nat0 |> mutate(gdp_percap = gdp_usd / national_pop)
#   ### Join nat0 with state population by year
#   nat0     <- nat0 |> left_join(pop0, by=yrCol0, relationship="many-to-many")
#   ### Arrange by colsP0
#   sort0    <- c(regCols0, yrCol0)
#   nat0     <- nat0 |> arrange_at(vars(sort0))
#   ### Return
#   return(nat0)
# }
create_nationalScenario <- function(
    gdp0,
    pop0,
    # gdp0    = "scenarioData" |> get_frediDataObj("gdp_default"),
    # pop0    = "scenarioData" |> get_frediDataObj("pop_default"),
    natPop0 = NULL,
    msg0    = 0
){
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  ### Standarize population data
  drop0    <- c("area", "region", "state", "state_order")
  join0    <- c("postal")
  select0  <- c("region","state", "postal", "state_order", "year", "pop")
  pop0     <- pop0 |>
    select(-any_of(drop0)) |>
    left_join(controlData[["co_states"]], by=join0) |>
    select(all_of(select0))
  rm(drop0, join0, select0)

  ### If national population is NULL, calculate national population
  nullNpop <- natPop0 |> is.null()
  if(nullNpop) natPop0 <- pop0 |> calc_nationalPop()

  ### Columns
  yrCol0   <- c("year")
  popCol0  <- c("pop")
  natCol0  <- c("national_pop")
  gdpCols0 <- c("gdp_usd", "gdp_percap")
  regCols0 <- c("region", "state", "postal")

  ### Select columns
  colsG0   <- c(yrCol0, gdpCols0)
  colsP0   <- c(regCols0, yrCol0, popCol0)
  colsN0   <- c(yrCol0, natCol0)
  gdp0     <- gdp0 |> select(any_of(colsG0))
  pop0     <- pop0 |> select(any_of(colsP0))

  ### Join GDP and national population by year
  nat0     <- gdp0 |> left_join(natPop0, by=yrCol0)
  rm(gdp0)

  ### Calculate GDP per capita
  nat0     <- nat0 |> mutate(gdp_percap = gdp_usd / national_pop)

  ### Join nat0 with state population by year
  nat0     <- nat0 |> left_join(pop0, by=yrCol0, relationship="many-to-many")
  rm(pop0)

  ### Arrange by colsP0
  sort0    <- c(regCols0, yrCol0)
  nat0     <- nat0 |> arrange_at(vars(sort0))

  ### Return
  return(nat0)
}



## update_popScalars
### Update scalars with regional population scenario
update_popScalars <- function(
    df_pop,   ### Population scenario
    # df_pop    = create_nationalScenario(), ### Pop scenario
    df_scalars, ### Tibble of scalars
    natScalars = c("gdp_percap", "gdp_usd"),
    natTypes   = c("econMultiplier", "econScalar"),
    regScalars = c("reg_pop"),
    # regScalars = c("pop"),
    regTypes   = c("physScalar"),
    popCol0    = c("pop"),
    yrCol0     = c("year"),
    valCol0    = c("value"),
    natArea0   = c("US")
    # scalars0   = c("reg_pop", "gdp_percap", "gdp_usd"),
    # groupCols  = c("region", "postal", "year"),
    # groupCols = c("region", "state", "postal", "year"),
    # natReg0    = get_nationalRegion() |> select(c("region", "postal"))
){
  ### Columns & Values ----------------
  ### Columns
  # yrCol0    <- c("year")
  # valCol0   <- c("value")
  # rCols0  <- c("region", "state", "postal")
  areaCol0  <- "area"
  postCol0  <- "postal"
  regCol0   <- "region"
  rCols0    <- c(areaCol0, regCol0, postCol0)
  ### Name and type
  nameCol0  <- "scalarName"
  typeCol0  <- "scalarType"

  ### Type cols
  # popCol0   <- c("pop")
  # natCol0   <- c("national_pop")
  natCols0  <- natScalars
  regCols0  <- regScalars |> str_replace("reg_pop", popCol0)
  # gdpCols0  <- c("gdp_usd", "gdp_percap")

  ### Types and columns
  natTypes0 <- c("econMultiplier", "econScalar")
  regTypes0 <- c("physScalar")

  ### Values
  # naStr0   <- c("N/A")
  # natStr0  <- c("National")
  # natReg0   <- controlData[["co_states"]]
  coStates   <- controlData[["co_states"]] |> select(any_of(rCols0))

  ### Format Data ----------------
  ### Drop region population, gdp_percap
  # df_scalars <- df_scalars |> filter_at(c(scCol0), get_matches, y=scalars0, matches=FALSE, type="matches")

  ### National GDP Scenario ----------------
  ### Select columns, pivot longer, add column values
  # dfScalars  <- tibble()
  idColsNat0 <- yrCol0
  selectNat0 <- c(idColsNat0, natCols0)
  sortNat0   <- c(typeCol0, nameCol0, idColsNat0)
  joinNat0   <- c("area")
  dfNat      <- df_pop |>
    select(all_of(selectNat0)) |> distinct() |>
    pivot_longer(
      -all_of(idColsNat0),
      names_to  = nameCol0,
      values_to = valCol0
    ) |>
    cross_join(tibble(scalarType=natTypes)) |>
    arrange_at(c(sortNat0)) |>
    mutate(regional = 0) |>
    mutate(area     = natArea0) |>
    # mutate(region=natStr0, state=naStr0, postal=naStr0)
    left_join(coStates, by=areaCol0) |>
    select(-any_of(areaCol0))
  ### Bind values
  # dfScalars  <- dfScalars |> bind_rows(dfNat)
  df_scalars <- df_scalars |> bind_rows(dfNat)
  # df_scalars <- df_scalars |> row_bind(df_gdp)
  # df_scalars <- df_scalars |> row_bind(df_gdp |> mutate(scalarType = "econScalar"))
  rm(selectNat0, sortNat0, dfNat)

  ### State Population ----------------
  ### - Select columns
  ### - Add additional scalar attributes
  ### - Rename column
  # from0      <- c("pop")
  # to0        <- c("value")
  idColsReg0 <- c(postCol0, yrCol0)
  selectReg0 <- c(idColsReg0, regCols0)
  sortReg0   <- c(typeCol0, nameCol0, idColsReg0)
  dfReg      <- df_pop |>
    select(all_of(selectReg0)) |> distinct() |>
    pivot_longer(
      -all_of(idColsReg0),
      names_to  = nameCol0,
      values_to = valCol0
    ) |>
    cross_join(tibble(scalarType=regTypes)) |>
    arrange_at(c(sortReg0)) |>
    mutate(regional = 1) |>
    left_join(coStates, by=postCol0) |>
    select(-any_of(areaCol0))
  ### Bind values
  # dfScalars  <- dfScalars |> bind_rows(dfReg)
  df_scalars <- df_scalars |> bind_rows(dfReg)
  # df_scalars <- df_scalars |> rbind(df_pop)
  # rm(select0, sort0, df_pop)

  ### Return ----------------
  # gc()
  # return(dfScalars)
  return(df_scalars)
}




## extend_slrScalars ----------------
### Values
extend_slrScalars <- function(
    df0,        ### Tibble of initial results
    scalars,    ### Tibble of scalar values: df_Scalars
    minYr0,
    maxYr0,
    # slrScalars  = "frediData" |> get_frediDataObj("configData", "co_slrScalars"),
    # minYr0      = "frediData" |> get_frediDataObj("fredi_config", "minYear0"),
    # maxYr0      = "frediData" |> get_frediDataObj("fredi_config", "maxYear0"),
    refYear0   = "frediData" |>
      get_frediDataObj("configData", "co_slrScalars") |>
      pull(refYear) |> unique() |>
      min(),
    elasticity = NULL,
    module0    = "fredi",
    msg0       = 0
){
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2

  ### Values & Columns ----------------
  #### Values
  module0    <- module0 |> tolower()
  modData0   <- module0 |> fun_moduleDataStr()

  ### SLR Scalar Info ----------------
  ### Get SLR scalar info and format it
  from0      <- c("refYear")
  to0        <- c("year0")
  slrScalars <- modData0 |>
    get_frediDataObj("configData", "co_slrScalars", msg0=msg0) |>
    rename_at(c(from0), ~to0) |>
    mutate(year0 = year0 |> as.character())
  rm(from0, to0)

  ### Separate Data ----------------
  #### SLR and non-SLR data
  ### Not all SLR sectors need to have scalars extended with extend_slrScalars()
  ### Divide data into those that are in slrScalars and those that aren't
  sectors0   <- slrScalars |> pull(sector) |> unique()
  dfSame     <- df0     |> filter(!(sector %in% sectors0))
  df0        <- df0     |> filter(  sector %in% sectors0)

  #### Check if there is any SLR data to extend; if not, return
  doExtend   <- df0     |> nrow()
  if(!doExtend) {return(dfSame)}

  ### SLR data pre- and post-reference year
  # refYear0 |> print(); df0 |> glimpse()
  names0     <- df0     |> names()
  scalars    <- scalars |> filter(year >= refYear0)
  dfRef      <- df0     |> filter(year <= refYear0)
  df0        <- df0     |> filter(year >= refYear0)


  ### Join Data & Scalar Info ----------------
  ### Drop columns from data
  ### Join initial results & scalar info
  ### Adjustments
  # df0        <- df0 |> match_scalarValues(sectors=sectors, scalars=df_scalars, module0=module0, scalarType="econMultiplier", doAdj0=T)
  join0      <- c("sector", "impactType")
  physEcon0  <- c("physScalar", "physAdj", "econMultiplier", "econAdj")
  adjStr0    <- c("physAdj", "econAdj")
  mutate0    <- physEcon0  |> get_matches(y=adjStr0, matches=F) |> paste0("Name")
  drop0      <- slrScalars |> names() |>  get_matches(y=join0, matches=F)
  drop1      <- c("physScalar", "econScalar", "econMultiplier", "physEconScalar")
  drop2      <- physEcon0  |> map(paste0, c("Name", "Value")) |> unlist()
  drop3      <- c(drop0, drop1, drop2) |> unique()
  df0        <- df0 |> select(-any_of(drop3))
  # df0 |> glimpse(); slrScalars |> glimpse()
  df0        <- df0 |> left_join(slrScalars, by=join0)
  # df0 |> glimpse()
  # df0        <- df0 |> mutate_at(c(mutate0), replace_na, "none")
  rm(drop0, drop1, drop2, drop3, mutate0, join0)


  ### Update Scalars ----------------
  ### Physical scalars and adjustment
  from0      <- c("physScalarAdj") |> paste0(c("Name", "Value"))
  to0        <- from0 |> str_replace("Multiplier", "")
  df0        <- df0 |> match_scalarValues(sectors=sectors0, scalars=scalars, module0=module0, minYr0=refYear0, maxYr0=maxYr0, scalarType="physScalar", doAdj0=F)
  df0        <- df0 |> match_scalarValues(sectors=sectors0, scalars=scalars, module0=module0, minYr0=refYear0, maxYr0=maxYr0, scalarType="physScalar", doAdj0=T)
  df0        <- df0 |> rename_at(c(from0), ~to0)
  rm(from0, to0)
  # df0 |> glimpse()

  ### Economic multiplier and adjustment
  ### Get economic adjustment values
  from0      <- c("econMultiplierAdj") |> paste0(c("Name", "Value"))
  to0        <- from0 |> str_replace("Multiplier", "")
  df0        <- df0 |> match_scalarValues(sectors=sectors0, scalars=scalars, module0=module0, minYr0=refYear0, maxYr0=maxYr0, scalarType="econMultiplier", doAdj0=F)
  df0        <- df0 |> match_scalarValues(sectors=sectors0, scalars=scalars, module0=module0, minYr0=refYear0, maxYr0=maxYr0, scalarType="econMultiplier", doAdj0=T)
  df0        <- df0 |> rename_at(c(from0), ~to0)
  rm(from0, to0)
  # df0 |> glimpse()

  ### Calculate Scalars ----------------
  ### Calculate scalars
  df0        <- df0 |> calcScalars(extendSlr=TRUE, module0=module0, elasticity=elasticity)
  # df0 |> glimpse()

  ### Drop Columns ----------------
  ### Drop columns
  drop0      <- c("c2")
  df0        <- df0 |> select(-any_of(drop0))
  rm(drop0)

  ### Bind Results ----------------
  ### Bind results back in
  df0        <- df0    |> select(all_of(names0))
  df0        <- df0    |> filter(year > refYear0)
  df0        <- dfRef  |> bind_rows(df0)
  df0        <- dfSame |> bind_rows(df0)
  rm(dfRef, dfSame)
  # df0 |> filter(!(scaled_impacts |> is.na())) |> filter(year > 2100) |> glimpse()

  ### Return ----------------
  ### Return
  gc()
  return(df0)
}

## Get Sector Info ----------------
### This helper function helps get info about sector groups
### Add steps to filter Regions, Model Types to specific module
### Remove state from IDs
get_co_sectorsInfo <- function(
    # module     = "fredi",
    sectors0   = NULL,  ### Sector IDs
    # addRegions = FALSE, ### Whether to include regions & states
    # addModels  = FALSE, ### Whether to include models
    # addId      = TRUE , ### Whether to add scenario ID
    # slrStr     = "Interpolation",
    idCol0     = c("scenario_id"),
    colTypes   = c("ids", "labels", "extra"), ### Types of columns to include: IDs, labels, or extra. If only labels, will return labels without the "_label"

    ### Get objects from FrEDI
    dfSects    , ### "co_sectors"     |> get_frediDataObj("frediData", "rDataList"),
    dfVars     , ### "co_variants"    |> get_frediDataObj("frediData", "rDataList"),
    dfITypes   , ### "co_impactTypes" |> get_frediDataObj("frediData", "rDataList"),
    dfIYears   , ### "co_impactYears" |> get_frediDataObj("frediData", "rDataList"),
    dfStates   , ### "co_states"      |> get_frediDataObj("frediData", "rDataList"),
    dfMTypes   , ### "co_modelTypes"  |> get_frediDataObj("frediData", "rDataList"),
    dfModels     ### "co_models"      |> get_frediDataObj("frediData", "rDataList")
){
  ### Conditionals ----------------
  colTypes   <- colTypes |> tolower()
  doIds      <- "ids"    %in% colTypes
  doLabs     <- "labels" %in% colTypes
  doExtra    <- "extra"  %in% colTypes
  onlyLabs   <- !doIds

  ### Column Names ----------------
  sectCol0   <- c("sector")
  colsMain0  <- sectCol0 |> c("variant", "impactType", "impactYear")
  regCol0    <- c("region")
  stateCol0  <- c("state")
  postCol0   <- c("postal")
  regCols0   <- c(regCol0, postCol0)
  mTypeCol0  <- c("model_type")
  modCol0    <- c("model")
  dMaxCol0   <- c("driverDataMax")

  manyStr0   <- "many-to-many"
  ### Adjust values in vectors depending on conditionals
  include0   <- c("region", "postal", "model")
  # regCols    <- c(regCol0, stateCol0, postCol0)
  modCols    <- c("inputName", "driver" |> paste0(c("Type", "Unit", "Unit_label")))
  # if(!addRegions) {regCols <- c(); include0 <- include0 |> get_matches(y=c("region", "postal"), matches=F)}
  # if(!addModels ) {modCols <- c(); include0 <- include0 |> get_matches(y=modCol0, matches=F)}
  # include0   <- c(regCols, modCols)

  ### Filter data to specific sectors ----------------
  df0        <- dfSects
  hasSectors <- sectors0 |> length()
  if(hasSectors) {df0 <- df0 |> filter(sector %in% sectors0)}
  rm(hasSectors, dfSects)
  # df0 |> nrow() |> print()

  ### Join Data ----------------
  ### Join with variants, impact types, and impact years info
  df0        <- df0 |> left_join(dfVars  , by=sectCol0)
  df0        <- df0 |> left_join(dfITypes, by=sectCol0, relationship=manyStr0)
  df0        <- df0 |> left_join(dfIYears, by=sectCol0, relationship=manyStr0)
  rm(dfVars, dfITypes, dfIYears)
  # df0 |> nrow() |> print()

  ### Join with dfReg and dfStates if addStates:
  ### - Rename column in states and join states with regions
  ### - Join data with states
  # if(addRegions) {df0 <- df0 |> cross_join(dfStates)}
  df0        <- df0 |> cross_join(dfStates)
  # df0 |> nrow() |> print()
  rm(dfStates)

  ### Join with dfModels if addModels:
  ### Join with model types
  # mTypeCols0 <- dfMTypes |> names()
  # df0        <- df0      |> left_join(dfMTypes, by=mTypeCol0, relationship=manyStr0)
  # # if(addModels) {
  df0        <- df0 |>
    left_join(dfMTypes, by=mTypeCol0, relationship=manyStr0) |>
    select(-any_of(dMaxCol0)) |>
    left_join(dfModels, by=mTypeCol0, relationship=manyStr0)
  # } ### End if(addModels)
  # df0 |> nrow() |> print()

  ### Add IDs ----------------
  # if(addId) (
  include0   <- include0 |> get_matches(y=df0 |> names())
  sort0      <- c(colsMain0, include0)
  df0        <- df0 |>
    arrange_at(c(sort0)) |>
    get_scenario_id(include0=include0, idCol0=idCol0) |>
    group_by_at(c(sort0, idCol0))
  # ) ### End if(addId)


  ### Column names ----------------
  colsAgg0   <- c("sectorprimary", "includeaggregate")
  colsDesc0  <- c("impactType_description", "physicalmeasure")
  colsScalar <- c("physScalar", "physAdj", "damageAdj", "econScalar", "econMultiplier")
  colsCoeff  <- c("c0", "c1", "exp0", "year0")
  colsTypes  <- c(colsAgg0, colsDesc0, colsScalar |> paste0("Name"), colsCoeff)
  colsMod0   <- c("inputName", "driver" |> paste0(c("Type", "Unit", "Unit_label")))

  ### Columns to rename
  rename0    <- c(colsMain0, include0)
  # renameIds  <- rename0 |> map(function(x, y=rename0, str0="_ids"){
  renameIds  <- rename0 |> map(function(x, y=rename0, str0=""){
    case_when(x %in% y ~ x |> paste0("_", str0), .default=x)
  }) |> unlist()
  renameLabs <- rename0 |> map(function(x, y=rename0, str0="_label"){
    case_when(x %in% y ~ x |> paste0("_", str0), .default=x)
  }) |> unlist()


  ### Columns for selecting
  # names0    <- df0 |> names()
  select0    <- c()
  if(doIds  ) select0  <- select0 |> c(colsMain0, regCol0, stateCol0, postCol0, mTypeCol0, modCol0) |> unique()
  if(doLabs ) select0  <- select0 |> c(renameLabs) |> unique()
  if(doExtra) select0  <- select0 |> c(colsAgg0, colsTypes, colsMod0) |> unique()
  select0    <- select0 |> get_matches(y=df0 |> names())
  df0        <- df0 |> relocate(any_of(select0))
  # ### Columns for arranging
  # if(doIds  ) {sort0   <- rename0} else if(doLabs){sort0 <- renameLabs}
  # if(addId  ) {select0 <- select0 |> c(idCol0)}
  # sort0      <- c(colsMain0, regCols, colsMod0) |> get_matches(y=df0 |> names())

  ### Rename values, select values, sort
  # df0 |> glimpse()
  # df0        <- df0 |> rename_at(renameIds, ~rename0)
  # df0        <- df0 |> select(any_of(select0))
  # df0        <- df0 |> arrange_at(c(sort0))
  # ### Rename label columns
  # if(onlyLabs) {df0 <- df0 |> rename_at(c(renameLabs), ~renameIds)}

  ### Return ----------------
  gc()
  return(df0)
}

## Scalar Functions ----------------
## match_scalarValues
### This function matches interpolated scalar values for each scalar type to the time series scenario information
### Scalar types are: physAdj, physMultiplier, damageAdj, econScalar, econMultiplier
### Function "match_scalarValues" replaces "get_popWts", "get_physMultipliers", and "get_econScalars"
# regTypes0  = "df_scalars" |> get_frediDataObj("stateData", "rDataList") |> pull(national_or_regional) |> unique()
# df0        <- regTypes0 |> map(function(
    #   rTypeX,
#   rTypeColX = c(regTypeCol0),
#   colX      = c(scalarName0),
#   joinX     = c(regCols0, scalarName0, yrCol0)
# ){
#   ### Whether to do regional or not
#   ### Filter scalars to region type
#   doReg  <- rTypeX == 1
#   df1X   <- scalars |> filter_at(c(rTypeColX), function(x, y=rTypeX){x %in% y})
#   valsX  <- df1X    |> pull(all_of(colX)) |> unique()
#   ### If scalars don't vary by region, drop region columns and update joinX
#   ### Filter data and join
#   if(!doReg) {df1X   <- df1X |> select(-any_of(regColsX))}
#   joinX  <- joinX |> get_matches(y=df1X |> names()) |> get_matches(y=df0 |> names())
#   df0X   <- df0   |> filter_at(c(colX), function(x, y=valsX){x %in% y})
#   df0X   <- df0X  |> left_join(df1X, by=joinX)
#   return(df0X)
# }) |> bind_rows()
match_scalarValues <- function(
    df0,       ### Initial results dataframe
    sectors0   = NULL,
    scalars,
    # scalars    = "df_scalars" |> get_frediDataObj("stateData", "rDataList"),
    scalarType = "physScalar", ### Type of scalar (one of: c("damageAdj", "econScalar", "physAdj", "phsScalar"))
    doAdj0     = FALSE,
    minYr0     = "frediData" |> get_frediDataObj("fredi_config", "minYear0"),
    maxYr0     = "frediData" |> get_frediDataObj("fredi_config", "maxYear0"),
    module0    = "fredi",
    msg0       = 0
){
  # df0 |> glimpse(); scalars |> glimpse();
  # scalarType |> print()
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN        <- "\n"
  msg1        <- msg0 + 1
  msg2        <- msg0 + 2

  ### Columns & Values ----------------
  module0     <- module0 |> tolower()
  modData0    <- module0 |> fun_moduleDataStr()

  ### General columns
  regCols0    <- c("region", "state", "postal")
  yrCol0      <- c("year")

  ### Scalar columns
  sectCol0    <- c("sector")
  regTypeCol0 <- c("regional")
  typeCol0    <- c("scalarType")
  nameCol0    <- c("scalarName")
  valCol0     <- c("value")

  ### Rename scalarType input
  adjStr0     <- case_when(doAdj0 ~ "Adj", .default = "")
  adjYr0      <- minYr0
  scalarType0 <- scalarType; rm(scalarType)
  scName0     <- scalarType0 |> paste0("Name")
  scalarName0 <- scalarType0 |> paste0(adjStr0, "Name")
  scalarVal0  <- scalarType0 |> paste0(adjStr0, "Value")

  ### Adjustment ----------------
  ### If doAdj0, add Adjustment Name
  # df0 |> glimpse()
  if(doAdj0) {
    dfAdj0      <- df0 |> select(all_of(scName0)) |> rename_at(c(scName0), ~scalarName0)
    df0         <- df0 |> bind_cols(dfAdj0)
    rm(dfAdj0)
  } ### End if(doAdj0)

  ### Scalar Info ----------------
  ### Scalar Info
  # sectors0 |> print()
  dfInfo0     <- modData0 |>
    get_frediDataObj("configData", "co_sectorScalars", msg0=msg0) |>
    filter_at(c(sectCol0), function(x, y=sectors0){x %in% y}) |>
    filter_at(c(typeCol0), function(x, y=scalarType0){x %in% y})
  # dfInfo0 |> glimpse()
  ### List of scalars
  scaleNames0 <- dfInfo0 |> pull(all_of(nameCol0)) |> unique()
  # scaleNames0 |> print()

  ### Filter scalars ----------------
  ### - Filter to data years
  ### - Filter to specific scalarType an drop scalarType column
  ### - Rename scalarName to "[scalarType]Name", where [scalarType] is supplied by the argument
  drop0       <- typeCol0 |> c("region", "state")
  scalars     <- scalars |>
    filter_at(c(typeCol0), function(x, y=scalarType0){x %in% y}) |>
    filter_at(c(nameCol0), function(x, y=scaleNames0){x %in% y}) |>
    # filter_at(c(yrCol0  ), function(x, y=minYr0, z=maxYr0){x >= y & x <= z}) |>
    rename_at(c(nameCol0, valCol0), ~c(scalarName0, scalarVal0)) |>
    # rename_at(c("scalarName"), ~c(colX)) |>
    # rename_at(c(nameCol0), ~scalarName0) |>
    select(-any_of(drop0))
  # scalars     <- scalars |> filter_at(c(typeCol0), function(x, y=scalarType0){x %in% y}); scalars |> nrow() |> print()
  # # scalars |> glimpse()
  # scalars |> pull(all_of(nameCol0)) |> unique() |> print()
  # scalars     <- scalars |> filter_at(c(nameCol0), function(x, y=scaleNames0){x %in% y}); scalars |> nrow() |> print()
  # scalars     <- scalars |> rename_at(c(nameCol0, valCol0), ~c(scalarName0, scalarVal0));
  # scalars     <- scalars |> select(-any_of(drop0))
  # scalars |> glimpse()
  rm(drop0)
  # "got here5" |> print()

  ### Filter Data ----------------
  ### Divide scalars into national and regional and iterate over region types to match scalar values
  names0     <- df0       |> names()
  names1     <- scalars   |> names()
  regTypes0  <- scalars   |> pull(all_of(regTypeCol0)) |> unique()
  df0        <- regTypes0 |> map(
    match_scalarValues_byRegion,
    df0       = df0,      ### Dataframe with scalars
    scalars   = scalars,  ### Scalar dataframe
    doAdjX    = doAdj0, ### Whether to get an adjustment value
    adjYrX    = adjYr0, ### ### Adjustment year
    rTypeColX = regTypeCol0,
    regColsX  = regCols0,
    colX      = scalarName0,
    joinX     = c(regCols0, scalarName0, yrCol0)
  ) |> bind_rows()

  ### Add placeholder column
  hasData0  <- df0 |> nrow()
  # df0 |> glimpse()
  if(!hasData0) df0[[scalarVal0]] <- NA

  # ### Rename
  # adjStr0   <- case_when(doAdj0 ~ "Adj", .default = "")
  # old0      <- c(nameCol0, valCol0)
  # new0      <- scalarType0 |> paste0(adjStr0, c("Name", "Value"))
  # df0       <- df0 |> rename_at(c(old0), ~new0)

  ### Return ----------------
  # gc()
  # df0 |> glimpse()
  return(df0)
}

### Match scalars by region type
match_scalarValues_byRegion <- function(
    rTypeX,
    df0,      ### Dataframe with scalars
    scalars,  ### Scalar dataframe
    doAdjX    = FALSE, ### Whether to get an adjustment value
    adjYrX,   ### Adjustment year
    colX      = "scalarName",
    rTypeColX = "regional",
    regColsX  = c("region", "state", "postal"),
    joinX     = c("postal", "scalarName", "year")
){
  ### Whether to do regional or not
  ### Filter scalars to region type
  # c(rTypeX, rTypeColX, colX) |> print(); scalars |> glimpse(); df0 |> glimpse()
  df1X   <- scalars |> filter_at(c(rTypeColX), function(x, y=rTypeX){x %in% y})
  doRegX <- rTypeX |> as.logical()
  ### If scalars don't vary by region, drop region columns and update joinX
  if(!doRegX) {df1X <- df1X |> select(-any_of(regColsX))}
  ### If doing and adjustment, filter to the year and then drop the year column
  if( doAdjX) {df1X <- df1X |> filter(year == adjYrX) |> select(-c("year"))}
  ### Filter data and join
  dropX  <- c("groupId", "dynamic", "regional")
  joinX  <- joinX |> get_matches(y=df1X |> names()) |> get_matches(y=df0 |> names())
  valsX  <- df1X  |> pull(all_of(colX)) |> unique()
  df1X   <- df1X  |> select(-any_of(dropX)) |> ungroup()
  # df1X |> names() |> print(); df0 |> names() |> print()
  # df1X |> glimpse(); df0 |> glimpse()
  df0    <- df0   |> filter_at(c(colX), function(x, y=valsX){x %in% y})
  # valsX  <- df0   |> pull(all_of(colX)) |> unique()
  # df0    <- df0   |> filter_at(c(colX), function(x, y=valsX){x %in% y})
  df0    <- df0   |> left_join(df1X, by=joinX)
  ### Return
  return(df0)
}

# ### match_scalarValues_byGroup
# match_scalarValues_byGroup <- function(
#     .x,
#     .y,
#     df0,      ### Data
#     typeCol0  = "scalarType",
#     nameCol0  = "scalarName",
#     rTypeCol0 = "regional"
# ){
#   ### Whether to do regional or not
#   ### Filter scalars to region type
#   type0  <- .y |> pull(all_of( typeCol0)) |> unique()
#   name0  <- .y |> pull(all_of( nameCol0)) |> unique()
#   rType0 <- .y |> pull(all_of(rTypeCol0)) |> unique()
#   doReg0 <- rType0 == 1
#   ### Filter to type and name
#   .x     <-
#     vals0  <- df1X    |> pull(all_of(colX)) |> unique()
#   ### If scalars don't vary by region, drop region columns and update joinX
#   ### Filter data and join
#   if(!doReg) {df1X   <- df1X |> select(-any_of(regColsX))}
#   joinX  <- joinX |> get_matches(y=df1X |> names()) |> get_matches(y=df0 |> names())
#   df0X   <- df0   |> filter_at(c(colX), function(x, y=valsX){x %in% y})
#   df0X   <- df0X  |> left_join(df1X, by=joinX)
#   return(df0X)
# }
#
# ### match_scalarValues_byGroup
# match_scalarValues_byGroup2 <- function(
#     .x,
#     .y,
#     df0, ### Data
#     typeCol0  = "scalarType",
#     nameCol0  = "scalarName",
#     rTypeCol0 = "regional"
# ){
#   ### Whether to do regional or not
#   ### Filter scalars to region type
#   type0  <- .y |> pull(all_of( typeCol0)) |> unique()
#   name0  <- .y |> pull(all_of( nameCol0)) |> unique()
#   rType0 <- .y |> pull(all_of(rTypeCol0)) |> unique()
#   doReg0 <- rType0 == 1
#   ### Filter to type and name
#   .x     <-
#   vals0  <- df1X    |> pull(all_of(colX)) |> unique()
#   ### If scalars don't vary by region, drop region columns and update joinX
#   ### Filter data and join
#   if(!doReg) {df1X   <- df1X |> select(-any_of(regColsX))}
#   joinX  <- joinX |> get_matches(y=df1X |> names()) |> get_matches(y=df0 |> names())
#   df0X   <- df0   |> filter_at(c(colX), function(x, y=valsX){x %in% y})
#   df0X   <- df0X  |> left_join(df1X, by=joinX)
#   return(df0X)
# }




### Function to get sector scalars
get_sectorScalars <- function(
    sectors,
    mTypes,
    maxYr0   = "frediData" |> get_frediDataObj("fredi_config", "maxYear0"),
    refYear0 = "frediData" |> get_frediDataObj("configData", "co_slrScalars") |> pull(refYear) |> unique() |> min(),
    module0  = "fredi",
    msg0     = 0
){
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2

  ### Modules
  module0  <- module0 |> tolower()
  modData0 <- module0 |> fun_moduleDataStr()

  ### Subset sector scalar info
  ## Filter to years, update with info from socioeconomic scenario
  scalars <- modData0 |>
    get_frediDataObj("configData", "co_sectorScalars", msg0=msg0) |>
    filter(sector %in% sectors) |>
    pull(scalarName) |>
    unique()

  ### Add SLR scalars
  ### Whether to do SLR vales
  slrStr0 <- "slr"
  hasSlr0 <- mTypes |> str_detect(slrStr0) |> any()
  doExtr0 <- maxYr0 > refYear0
  doSlr0  <- hasSlr0 & doExtr0
  colsSlr <- c("physScalarName", "econMultiplierName")
  ### Which scalars to do for SLR
  if(doSlr0) {
    scalars <- modData0 |>
      get_frediDataObj("configData", "co_slrScalars", msg0=msg0) |>
      filter(sector %in% sectors) |>
      select(all_of(colsSlr)) |>
      pivot_longer(names_to="name", values_to="value") |>
      select(c("value")) |> unique()
  } ### End if(doSlr0)

  ### Return
  return(scalars)
}

## initialize_resultsDf ----------------
### Initialize results data frame
initialize_resultsDf <- function(
    df_se      = create_nationalScenario(), ### SE scenario
    elasticity = NULL,
    module0    = "fredi",
    minYr0     = "frediData" |> get_frediDataObj("fredi_config", "minYear0"),
    maxYr0     = "frediData" |> get_frediDataObj("fredi_config", "maxYear0"),
    sectors    = "frediData" |> get_frediDataObj("configData", "co_sectors") |> pull(sector) |> unique(), ### Vector of sectors
    mTypes     = "frediData" |> get_frediDataObj("configData", "co_models") |> pull(model_type) |> unique(),
    refYear0   = "frediData" |> get_frediDataObj("configData", "co_slrScalars") |> pull(refYear) |> unique() |> min(),
    # types0     = "controlData" |> get_frediDataObj("co_scalarTypes") |> pull(scalarType) |> unique(),
    # types0     = controlData[["co_scalarTypes"]] |> pull(scalarType),
    msg0       = 0
){
  ### Messaging ----------------
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  msg0 |> get_msgPrefix(newline=T) |> paste0("Formatting initial results", "...") |> message()

  ### Columns & Values----------------
  ### Modules
  module0  <- module0 |> tolower()
  modData0 <- module0 |> fun_moduleDataStr()

  ### Columns
  mainCols0  <- c("sector", "variant", "impactType", "impactYear")
  # regCols0   <- c("region", "state", "postal")
  regCols0   <- c("region", "postal")
  mTypeCol0  <- c("model_type")
  modCol0    <- c("model")
  yrCol0     <- c("year")
  # dropCols0  <- c("region", "state")
  colsScalar <- c("physScalar", "physAdj", "damageAdj", "econScalar", "econMultiplier")
  # colsCoeff  <- c("c0", "c1", "exp0", "year0")
  colsCoeff  <- c("c0", "c1", "c2", "exp0", "year0")

  ### Whether to do SLR extrapolation
  slrStr0    <- "slr"
  hasSlr     <- slrStr0 %in% mTypes
  doExtr0    <- maxYr0 > refYear0
  doSlr0     <- hasSlr & doExtr0

  ### Sector Info ----------------
  # select0    <- c(mainCols0, regCols0, mTypeCol0, modCol0, colsScalar, colsCoeff)
  df0        <- modData0 |>
    get_frediDataObj("configData", "co_sectorsInfo", msg0=msg1) |>
    # filter(sector %in% sectors) |>
    # select(-any_of(dropCols0))
    filter(sector %in% sectors) |>
    ungroup()
  # df0 |> pull(physScalarName) |> unique() |> print()

  ### Join with SE Scenario ----------------
  ### Initialized results: Join sector info with socioeconomic scenario
  # df_se |> glimpse(); df_info |> glimpse(); # df_scalars |> glimpse()
  df_se       <- df_se |> select(-c("region"))
  join0       <- df_se |> names() |> get_matches(df0 |> names())
  df0         <- df0   |> left_join(df_se, by=join0, relationship="many-to-many")
  rm(join0)

  ### Update Scalar Info ----------------
  ### - Get relevant scalars
  ### - Get seScalars
  ### - Filter to relevant scalars, years, and update with seScalars
  # seScalars   <- df_se   |> update_popScalars()
  sectScalars <- sectors  |> get_sectorScalars(module0=module0, mTypes=mTypes, maxYr0=maxYr0)
  df_scalars  <- modData0 |>
    get_frediDataObj("stateData", "scalarData", msg0=msg1) |>
    filter(scalarName %in% sectScalars) |>
    filter(year >= minYr0, year <= maxYr0)
  # df0 |> glimpse();
  # df_scalars |> glimpse()
  # df_scalars |> filter(scalarType=="physScalar") |> pull(scalarName) |> unique() |> print()

  ### Update variable scalars
  df_scalars  <- df_se |>
    update_popScalars(df_scalars=df_scalars) |>
    mutate(scalarName = case_when(scalarName %in% c("pop") ~ "reg_pop", .default=scalarName))
    # mutate_at(c("scalarName"), function(x, y="pop", z="reg_pop"){
    #   case_when(
    #     x %in% y ~ z,
    #     .default = x
    #   )})
  # df_scalars |> filter(scalarType=="physScalar") |> pull(scalarName) |> unique() |> print()
  ### Add in scenario info
  # df_scalars |> glimpse(); seScalars |> glimpse()
  # df_scalars  <- df_scalars |> left_join(seScalars)
  # ### Update scalars
  # df_scalars <- df_scalars |> update_popScalars(df_se, popCol="pop")
  # rm(seScalars)

  ### Match Scalar Values ----------------
  ### Update scalar info:
  ### Physical scalars, Physical adjustment, Damage Adjustment
  ### Economic scalar, Economic multiplier
  # "got here1" |> print()
  df0        <- df0 |> match_scalarValues(sectors=sectors, scalars=df_scalars, module0=module0, minYr0=minYr0, maxYr0=maxYr0, scalarType="physScalar")
  # return(df0)
  df0        <- df0 |> match_scalarValues(sectors=sectors, scalars=df_scalars, module0=module0, minYr0=minYr0, maxYr0=maxYr0, scalarType="physAdj")
  df0        <- df0 |> match_scalarValues(sectors=sectors, scalars=df_scalars, module0=module0, minYr0=minYr0, maxYr0=maxYr0, scalarType="damageAdj")
  df0        <- df0 |> match_scalarValues(sectors=sectors, scalars=df_scalars, module0=module0, minYr0=minYr0, maxYr0=maxYr0, scalarType="econScalar")
  df0        <- df0 |> match_scalarValues(sectors=sectors, scalars=df_scalars, module0=module0, minYr0=minYr0, maxYr0=maxYr0, scalarType="econMultiplier")
  # "got here1" |> print()
  # df0        <- types0
  # df0 |> glimpse(); df0 |> pull(region) |> unique() |> print()

  ### Calculate Economic Adjustment Values ----------------
  ### Get economic adjustment values: econAdjName, econAdjValue
  from0      <- c("econMultiplierAdj") |> paste0(c("Name", "Value"))
  to0        <- from0 |> str_replace("Multiplier", "")
  # df0        <- df0 |> get_scalarAdjValues(scalars=df_scalars, module0=module0, scalarType0="econMultiplier")
  df0        <- df0 |> match_scalarValues(sectors=sectors, scalars=df_scalars, module0=module0, minYr0=minYr0, maxYr0=maxYr0, scalarType="econMultiplier", doAdj0=T)
  df0        <- df0 |> rename_at(c(from0), ~to0)
  rm(from0, to0)
  # "got here2" |> print()

  ### Calculate Scalars ----------------
  ### Calculate scalars
  df0        <- df0 |> calcScalars(extendSlr=F, elasticity=elasticity, module0=module0)
  # "got here3" |> print()

  ### Extend SLR Scalars for Years > 2090 ----------------
  ### Scalars for SLR past 2090
  slrStr0    <- "slr"
  hasSlr     <- slrStr0 %in% mTypes
  doSlr      <- hasSlr & (maxYr0 > refYear0)
  if(doSlr) {
    ### Separate GCM & SLR values
    dfSlr0 <- df0 |> filter((model_type |> tolower()) == slrStr0)
    df0    <- df0 |> filter((model_type |> tolower()) != slrStr0)
    ### Get extended scalars
    dfSlr0 <- dfSlr0 |> extend_slrScalars(
      scalars    = df_scalars,
      minYr0     = minYr0,
      maxYr0     = maxYr0,
      refYear0   = refYear0,
      elasticity = elasticity,
      module0    = module0
    ) ### End extend_slrScalars

    ### Bind Values back in
    df0    <- df0 |> bind_rows(dfSlr0)
    # df0        <- df0 |> filter(year <= maxYr0)
    rm(dfSlr0)
  } ### End if(doSlr)
  # df0 |> pull(region) |> unique() |> print()

  ### Join and Arrange ----------------
  ### Drop columns
  # drop0      <- c("region", "postal", "model_type") |> c(colsScalar, colsCoeff)
  # drop0      <- c("region", "postal") |> c(colsScalar, colsCoeff)
  # df0        <- df0 |> select(-any_of(drop0))
  # rm(drop0)
  # ### Join data
  # join0      <- df0 |> names() |> get_matches(df_info0 |> names())
  # df0        <- df_info0 |> left_join(df0, by=join0, relationship="many-to-many")
  # rm(join0, df_info0)
  # ### Arrange data
  sort0      <- mainCols0 |> c("state_order") |> c(modCol0, yrCol0)
  df0        <- df0 |> arrange_at(c(sort0))


  ### Return ----------------
  ### Return
  gc()
  return(df0)
}


## calcScalars ----------------
### Calculate Scalars
### This function calculates the physical scalar value, the economic scalar value, and their product
### The physical and economic scalars refer to the time series column from which the Annual Sectors tab
###   in the Excel tool draws values.
calcScalars <- function(
    data,      ### Initial results dataframe
    extendSlr  = FALSE, ### Whether to run methods for extending SLR scalars
    elasticity = NULL , ### An elasticity to use to adjust values
    module0    = "fredi"
){
  ### Adjust Elasticity for VSL ----------------
  ### Adjust Elasticity for VSL only
  hasVal <- !(elasticity |> is.null())
  if(hasVal){
    data <- data |> mutate(exp0 = case_when(
      econScalarName == "vsl_usd" ~ elasticity,
      .default = exp0
    ))
  } ### End if(!(elasticity |> is.null()))


  ### Physical Scalar ----------------
  ### Calculate physical scalar values
  if(extendSlr) {
    data   <- data |> mutate(physScalar = c2 * physScalarValue / physAdjValue)
  } else{
    data   <- data |> mutate(physScalar = physScalarValue * physAdjValue * damageAdjValue)
  } ### End if(extendSlr)


  ### Economic Scalar ----------------
  ### Calculate economic multiplier and scalar
  ### Economic multipliers are the economic multiplier value divided by the adjustment
  ### - The economic multiplier value is 1, GDP, or GDP per capita
  ### - The economic adjustment value is usually the economic multiplier value at a reference year
  ### The economic scalar is calculated using the following equation.
  ### - Constants c0, c1, and exp0 are from the impactTypes data frame
  data   <- data |> mutate(econMultiplier = (econMultiplierValue / econAdjValue)**exp0 )
  data   <- data |> mutate(econScalar     = c0 + c1 * econScalarValue * econMultiplier )
  # } ### End if(extendSlr)


  ### Physical Economic Scalar ----------------
  ### Combine the physical and economic scalar.
  if(extendSlr) {
    data   <- data |> mutate(physEconScalar = econScalar + physScalar)
  } else{
    data   <- data |> mutate(physEconScalar = econScalar * physScalar)
  } ### End if(extendSlr)


  ###### Return ----------------
  gc()
  return(data)
}

## Impact Calculation Functions ----------------
### interpolate_impacts function
interpolate_impacts <- function(
    fun0  = NULL, ### List of functions
    df0   = NULL, ### Drivers
    xCol0 = "modelUnitValue", ### Temperatures or SLRs to interpolate,
    yCol0 = "scaled_impacts"
){
  ### Values
  xVals0 <- df0 |> pull(all_of(xCol0))
  yVals0 <- xVals0 |> fun0()
  rm(xCol0, xVals0, fun0)
  ### Add values to df0
  df0[[yCol0]] <- yVals0
  rm(yCol0, yVals0)
  ### Return
  gc()
  return(df0)
}


### get_gcmScaledImpacts function
get_gcmScaledImpacts <- function(
    df0,   ### Tibble of initial results for GCM sectors, with scenario ID
    df1,   ### Tibble of drivers
    module0 = "fredi",
    minYr0 = "frediData" |> get_frediDataObj("fredi_config", "minYear0"),
    maxYr0 = "frediData" |> get_frediDataObj("fredi_config", "maxYear0"),
    xCol0  = "modelUnitValue",
    yCol0  = "scaled_impacts",
    idCol0 = "scenario_id",
    sort0  = c("scenario_id", "year"),
    msg0   = 0
    # msg0   = "\t"
){
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  msg0 |> get_msgPrefix(newline=T) |> paste0("Calculating temperature-driven scaled impacts", "...") |> message()

  ### Modules
  module0    <- module0 |> tolower()
  modData0   <- module0 |> fun_moduleDataStr()

  ### Get list of unique scenarios from df0
  ids0       <- df0 |> pull(all_of(idCol0))
  rm(df0)

  ### List of impact functions
  ### Get list of groups with unique impact functions
  funList0   <- modData0 |>
    get_frediDataObj("stateData", "gcmFuns", msg0=msg1) |>
    get_matches_list(y=ids0, type="values", lType="names", comp="in")

  ### Separate into tibbles that have and do not have functions
  # scenarios0 <- ids0     |> get_matches(y=funNames0, matches=T)
  scenarios0 <- funList0 |> names()
  scenariosN <- ids0     |> get_matches(y=scenarios0, matches=F)

  ### Check if there are values without functions
  hasDoFuns0 <- scenarios0 |> length()
  hasNoFuns0 <- scenariosN |> length()
  # c(hasDoFuns0, hasNoFuns0) |> print()

  ### Initialize empty tibble for results
  ### Columns to select
  df0        <- tibble()
  select0    <- c(idCol0, sort0, xCol0, yCol0) |> unique()

  ### Get impacts for scenario_ids that have functions
  if(hasDoFuns0) {
    ### Get impacts
    dfDoFuns1 <- funList0 |>
      map(interpolate_impacts, df1=df1, xCol0=xCol0, yCol0=yCol0) |>
      set_names(scenarios0) |>
      bind_rows(.id="scenario_id")
    ### Select columns
    dfDoFuns1 <- dfDoFuns1 |> select(all_of(select0))
    ### Bind to df0
    df0       <- df0 |> bind_rows(dfDoFuns1)
    rm(dfDoFuns1)
  } ### End if(hasFuns0)
  rm(funList0, hasDoFuns0)

  ### For groups that don't have functions, create a tibble with na values
  if(hasNoFuns0) {
    ### Get impacts
    dfNoFuns1 <- tibble()
    dfNoFuns1[[idCol0]] <- scenariosN
    dfNoFuns1[[yCol0 ]] <- NA
    dfNoFuns1 <- dfNoFuns1 |> cross_join(df1)
    # df0 |> names() |> print(); dfNoFuns1 |> names() |> print()
    ### Select columns
    dfNoFuns1 <- dfNoFuns1 |> select(all_of(select0))
    ### Bind to df0
    df0       <- df0 |> bind_rows(dfNoFuns1)
    rm(dfNoFuns1)
  } ### End if(hasNoFuns0)
  rm(hasNoFuns0)

  ### Arrange
  df0        <- df0 |> arrange_at(c(sort0))
  # df0 |> glimpse()

  ### Return
  gc()
  return(df0)
}

### get_slrScaledImpacts function
### Scenario ID: c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model")
get_slrScaledImpacts <- function(
    df0,    ### Initial results for SLR sectors
    df1,    ### Driver data frames
    module0 = "fredi",
    minYr0  = "frediData" |> get_frediDataObj("fredi_config", "minYear0"),
    maxYr0  = "frediData" |> get_frediDataObj("fredi_config", "maxYear0"),
    xCol0   = "modelUnitValue",
    yCol0   = "scaled_impacts",
    idCol0  = "scenario_id",
    sort0   = c("scenario_id", "year"),
    slrMax0 = "frediData" |> get_frediDataObj("frediData", "co_modelTypes") |>
      filter(modelType_id == "slr") |>
      pull(modelMaxOutput) |> unique(),
    msg0    = 0
    # df_imp0 = "frediData"  |> get_frediDataObj("slrImpacts", "rDataList"),
    # df_max0 = "frediData" |> get_frediDataObj("slrExtremes", "rDataList"),
    # msg0    = "\t"
){
  ### Messaging ----------------
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  msg0 |> get_msgPrefix(newline=T) |> paste0("Calculating SLR-driven scaled impacts", "...") |> message()

  ### Columns & Values ----------------
  ### Modules
  module0    <- module0 |> tolower()
  modData0   <- module0 |> fun_moduleDataStr()

  ### Get unique sectors and driver names
  ids0       <- df0 |> pull(scenario_id) |> unique()
  rm(df0)
  # names1     <- df1 |> names()

  ### Get rDataList Objects ----------------
  ### Filter to sectors and year range
  ### Impacts
  select0    <- c(idCol0) |> c("model", "year", "scaled_impacts")
  df_imp0    <- modData0  |>
    get_frediDataObj("stateData", "slrImpacts", msg0=msg1) |>
    filter(hasScenario) |>
    filter(scenario_id %in% ids0) |>
    select(all_of(select0)) |>
    filter(year >= minYr0, year <= maxYr0)
  rm(select0)
  ### Extremes
  select0    <- c(idCol0) |> c("year", "driverValue_ref", "impacts_intercept", "impacts_slope")
  df_max0    <- modData0  |>
    get_frediDataObj("stateData", "slrExtremes", msg0=msg1) |>
    filter(hasScenario) |>
    filter(scenario_id %in% ids0) |>
    select(all_of(select0)) |>
    filter(year >= minYr0, year <= maxYr0)
  rm(select0)
  ### Figure out which values have a scenario
  scenarios0 <- df_imp0 |> pull(all_of(idCol0)) |> unique()
  scenariosN <- ids0    |> get_matches(y=scenarios0, matches=F)
  # df_max0 |> glimpse(); df_imp0 |> glimpse(); df0 |> glimpse()
  ### Which values have functions
  hasDoSlr0  <- scenarios0 |> length()
  hasNoSlr0  <- scenariosN |> length()

  ### Initialize empty tibble for results
  ### Columns to select
  df0        <- tibble()
  select0    <- c(idCol0, sort0, xCol0, yCol0) |> unique()

  ### Get Impacts for Non-Valid IDs ----------------
  # } ### End if(hasNoSlr0)
  if(hasNoSlr0) {
    ### Initialize tibble
    dfNone0 <- tibble()
    dfNone0[[idCol0]] <- scenariosN
    dfNone0[[yCol0 ]] <- NA
    ### Rename values, select columns, bind rows
    dfNone0 <- dfNone0 |> cross_join(df1)
    dfNone0 <- dfNone0 |> select(all_of(select0))
    df0     <- df0     |> bind_rows(dfNone0)
    rm(df0)
  } ### End if(hasNoSlr0)

  ### Get Impacts for Valid IDs ----------------
  if(hasDoSlr0) {
    #### Join with Driver Info ----------------
    # df_imp0 |> glimpse(); df_max0 |> glimpse(); df1 |> glimpse()
    join0   <- c("year")
    df_imp0 <- df1 |> left_join(df_imp0, by=join0)
    df_max0 <- df1 |> left_join(df_max0, by=join0)
    rm(join0)
    # df_ext0 |> glimpse(); df_imp0 |> glimpse();

    ### Get extreme impacts ----------------
    ### Figure out which years have modelUnitValue >= driverValue_ref
    ### Filter to appropriate years
    df_max0 <- df_max0 |> filter_at(c(xCol0), function(x, y=df_max0[["driverValue_ref"]]){x >= y})
    maxYrs0 <- df_max0 |> pull(year) |> unique()
    doMax0  <- df_max0 |> nrow()
    ### Calculate scaled impacts for values > slrMax0
    if(doMax0) {
      df_max0 <- df_max0 |> mutate(delta_y = df_max0[[xCol0]]  - driverValue_ref)
      df_max0 <- df_max0 |> mutate(yCol    = impacts_intercept + impacts_slope * delta_y)
    } else{
      df_max0 <- df_max0 |> mutate(yCol = NA)
    } ### End if(doMax0)
    rm(doMax0)
    ### Rename values, select columns, bind rows
    df_max0 <- df_max0 |> rename_at(c("yCol"), ~yCol0)
    df_max0 <- df_max0 |> select(all_of(select0))
    df0     <- df0     |> bind_rows(df_max0)
    rm(df_max0)

    ### Get other impacts----------------
    ### modelUnitValue < driverValue_ref
    ### Group by cols
    ### Filter to values not in extreme years
    df_imp0 <- df_imp0 |> filter(!(year %in% maxYrs0))
    doImp0  <- df_imp0 |> nrow()
    # doImp0 |> print()
    if(doImp0) {
      #### Interpolate driver values
      df1     <- df1 |> interp_slrByYear(yCol=yCol0, minYr0=minYr0, maxYr0=maxYr0)
      ### Mutate values
      # mutate0 <- "model" |> paste0(c("Lo", "Hi"))
      df1     <- df1 |> mutate_at(c(mutate0), str_replace, " ", "")
      rm(mutate0)
      ### Interpolate
      # df_imp0 |> glimpse()
      # group0  <- sort0   |> get_matches(y=df_imp0 |> names()) |> c("year") |> unique()
      df_imp0 <- scenarios0 |> fredi_slrInterp(df1=df_imp0, df2=df1, idCol0=idCol0, minYr0=minYr0, maxYr0=maxYr0)
      # rm(group0, slr_names, slrVals0); rm(cols0, cols1)
    } else{
      df_imp0 <- df_imp0 |> mutate(yCol = NA)
      df_imp0 <- df_imp0 |> rename_at(c("yCol"), ~yCol0)
    } ### End if(nrow_oth)
  } ### End if(hasDoSlr0)
  ### Select columns, bind rows
  df_imp0 <- df_imp0 |> select(all_of(select0))
  df0     <- df0     |> bind_rows(df_imp0)
  rm(df_imp0)

  ### Return ----------------
  gc()
  return(df0)
}

## map_getScaledImpacts
map_getScaledImpacts <- function(
    type0, ### Model type
    df0  , ### Tibble with sector info (e.g., initialize_resultsDf)
    df1  , ### Tible with driver info
    module0 = "fredi",
    minYr0  = "frediData" |> get_frediDataObj("fredi_config", "minYear0"),
    maxYr0  = "frediData" |> get_frediDataObj("fredi_config", "maxYear0"),
    idCol0  = "scenario_id",
    yCol0   = "scaled_impacts",
    yrCol0  = "year",
    drCol0  = "driverValue",
    # drCol0  = "modelUnitValue",
    msg0    = 0
){
  ### Messaging
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  ### Filter values to type
  ### Select columns
  select0 <- c(idCol0)
  select1 <- c(yrCol0, drCol0)
  df0     <- df0 |> filter(model_type %in% type0) |> select(all_of(select0)) |> unique()
  df1     <- df1 |> filter(model_type %in% type0) |> select(all_of(select1))
  rm(select0, select1)
  ### Get scaled impacts
  doGcm   <- (type0 |> tolower()) %in% "gcm"
  doSlr   <- (type0 |> tolower()) %in% "slr"
  if(doGcm) {df0 <- df0 |> get_gcmScaledImpacts(df1=df1, module0=module0, minYr0=minYr0, maxYr0=maxYr0, msg0=msg0)}
  if(doSlr) {df0 <- df0 |> get_slrScaledImpacts(df1=df1, module0=module0, minYr0=minYr0, maxYr0=maxYr0, msg0=msg0)}
  ### Select columns
  select0 <- c(idCol0, yrCol0, drCol0, yCol0)
  df0     <- df0 |> select(all_of(select0))
  rm(select0)
  ### Return
  gc()
  return(df0)
}

## calc_scaled_impacts_fredi
calc_scaled_impacts_fredi <- function(
    results0, ### Initial results DF with scenarios
    drivers0, ### Tibble with driver scenarios
    module0 = "fredi",
    minYr0 = "frediData" |> get_frediDataObj("fredi_config", "minYear0"),
    maxYr0 = "frediData" |> get_frediDataObj("fredi_config", "maxYear0"),
    idCol0 = "scenario_id",
    yCol0  = "scaled_impacts",
    yrCol0 = "year",
    drCol0 = "modelUnitValue"
){
  ### Columns & Values ----------------
  ### Modules
  module0    <- module0 |> tolower()
  modData0   <- module0 |> fun_moduleDataStr()

  ### Select Data ----------------
  ### Filter sector info to sectors in study
  ### Mutate info to interpolation and get distinct values
  select0  <- c("scenario_id", "model_type")
  results0 <- results0 |> select(all_of(select0)) |> distinct()
  rm(select0)

  ### Calculate Scaled Impacts ----------------
  # df_info0 |> pull(region) |> unique() |> print()
  mTypes0  <- drivers0 |> pull(model_type) |> unique()
  df0      <- mTypes0  |> map(
    map_getScaledImpacts,
    df0     = results0,
    df1     = drivers0,
    module0 = module0,
    minYr0  = minYr0,
    maxYr0  = maxYr0
  ) |>
    set_names(mTypes0) |>
    bind_rows(.id="model_type")
  rm(mTypes0)

  ### Select Columns ----------------
  select0  <- c(idCol0, yrCol0, yCol0)
  df0      <- df0 |> select(all_of(select0))
  rm(select0)

  ### Return ----------------
  gc()
  return(df0)
}

## calc_impacts_fredi
calc_impacts_fredi <- function(
    df0, ### Tibble with scalars/initialized results
    df1, ### Tibble with scaled impacts
    df2, ### Tibble with drivers
    module0 = "fredi",
    yCol0   = "scaled_impacts"
){
  ### Modules
  module0    <- module0 |> tolower()
  modData0   <- module0 |> fun_moduleDataStr()

  ### Add driver info to df0
  # df_info |> glimpse(); df0 |> filter(year > 2100) |> glimpse()
  join0      <- c("model_type", "year")
  drop0      <- df2 |> names() |> get_matches(df0 |> names()) |> get_matches(join0, matches=F)
  df2        <- df2 |> select(-any_of(drop0))
  df0        <- df0 |> left_join(df2, by=join0)
  rm(join0, df2)

  ### Add model info to df0
  # df_info |> glimpse(); df0 |> filter(year > 2100) |> glimpse()
  join0      <- c("scenario_id", "year")
  df0        <- df0 |> left_join(df1, by=join0, relationship="many-to-many")
  rm(join0, df1)

  ### Physical impacts = physScalar * scaled_impacts
  ### Annual impacts = phys-econ scalar value by the scaled impacts
  # df0 |> glimpse()
  df0      <- df0 |> mutate(physical_impacts = scaled_impacts * physScalar)
  df0      <- df0 |> mutate(annual_impacts   = scaled_impacts * physEconScalar)
  # df0 |> nrow() |> print()

  ### Return
  gc()
  return(df0)
}


## SLR Helper Functions ----------------
### yr0 = Year for analysis
### df0 = Driver scenario with columns year, yVal
### df1 = slr_cm with columns year, xRef
# get_slrHiLoInfo <- function(
#     year, ### Year
#     xVal, ### Value in yr0
#     df1 = "slr_cm"  |>
#       get_frediDataObj("frediData", "rDataList") |>
#       (function(
#           df0,
#           modCol0 = "model",
#           xCol0   = "driverValue",
#           xCol1   = "xRef",
#           yrCol0  = "year"
#       ){
#         ### Columns
#         grp0  <- c(yrCol0, xCol1)
#         sort0 <- c(grp0, modCol0)
#         ### Factor models
#         df0   <- df0 |>
#           mutate(model_id    = df0 |> pull(all_of(modCol0)) |> str_replace(" ", "")) |>
#           mutate(model_label = df0 |> pull(all_of(modCol0)) |> str_replace("cm", " cm")) |>
#           mutate(model_cm    = model_id |> str_replace("cm", "") |> as.numeric()) |>
#           mutate(model       = model_id)
#         cols0 <- c("model_cm", "model") |> c(modCol0) |> unique()
#         lvls0 <- df0 |> select(all_of(cols0)) |> unique() |> arrange_at(c("model_cm"))
#         df0   <- df0 |> mutate_at(c(modCol0), factor, lvls0 |> pull(all_of(modCol0)))
#         ### Rename columns and organize
#         df0   <- df0 |>
#           rename_at(c(xCol0), ~xCol1) |>
#           arrange_at(c(sort0)) |>
#           group_by_at(c(grp0)) |>
#           slice(1)
#         ### Return
#         return(return(df0))
#       })(),
#     # yrCol0 = "year",
#     xCol0  = "xVal",
#     xCol1  = "xRef",
#     idCol1 = "model",
#     base1  = "x",
#     eqStr  = "Eq",
#     loStr  = "Lo",
#     hiStr  = "Hi"
# ){
#   ### Columns
#   cols0 <- c(idCol1, xCol1)
#   cols1 <- c(idCol1, base1)
#   ### Filter data to years
#   df1   <- df1 |>
#     filter(year == yr0) |>
#     select(all_of(cols0))
#   ### For equal values, get lower value
#   eq0   <- df1 |>
#     filter(xRef == val0) |> first() |>
#     rename_at(c(cols0), ~cols1 |> paste0(eqStr))
#   ### For lower value, get max value and then get lowest value
#   lo0   <- df1 |>
#     filter(xRef <= val0) |> last() |>
#     rename_at(c(cols0), ~cols1 |> paste0(loStr))
#   ### For higher value, get min value and then get lowest value
#   hi0   <- df1 |>
#     filter(xRef >= val0) |> first() |>
#     rename_at(c(cols0), ~cols1 |> paste0(hiStr))
#   ### Join
#   cols0 <- idCol1 |> paste0(c(eqStr, loStr, hiStr))
#   df0   <- list(df0, eq0, lo0, hi0) |> bind_cols() |> ungroup()
#   df0   <- df0 |> mutate_at(c(cols0), as.character)
#   rm(eq0, lo0, hi0)
#   ### Return
#   return(df0)
# }


### utils for aggregate_impacts
## interp_slrByYear
get_loHiValues <- function(
    df0,      ### Driver scenario with columns year, slr_cm
    xCol0     = "modelUnitValue",
    xRef0     = "xRef",
    yrCol0    = "year",
    modCol0   = "model",
    boundCol0 = "bound",
    module0   = "fredi",
    silent    = TRUE,
    msg0      = 0
){
  ### Messaging
  # msgUser <- !silent
  # msg1       <- msg0 |> paste0("\t")
  msgN     <- "\n"
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2

  ### Columns & Values
  ### Modules
  module0    <- module0 |> tolower()
  modData0   <- module0 |> fun_moduleDataStr()

  ### Rename y Column and join df0 and slrDf
  loStr0  <- "Lo"
  hiStr0  <- "Hi"
  xStr0   <- "x"
  xVal0   <- xStr0 |> paste0("Val")
  bounds0 <- c(hiStr0, loStr0)

  ### Format Data
  ### - Filter driver scenario, select columns, and rename column with driver values
  select0 <- c(yrCol0, xCol0)
  df0     <- df0 |>
    filter((model_type |> tolower()) %in% "slr") |>
    select(all_of(select0)) |>
    rename_at(c(xCol0), ~xVal0)
  rm(select0)

  ### Join with SLR heights data
  ### co_slr with SLR heights
  old0    <- c(modCol0, xRef0)
  new0    <- c(modCol0, xStr0)
  # df1     <- "co_slr"  |> get_frediDataObj("frediData", "rDataList")
  df1     <- "controlData"  |> get_frediDataObj("co_slr", msg0=msg0)
  df0     <- df0       |> left_join(df1, by=yrcol0); rm(df1)
  df0     <- bounds0   |> map(function(boundX, oldX=old0, newX=new0){
    doLo <- boundX %in% loStr0
    if(doLo) {dfX <- df0 |> filter(xRef <= xVal)}
    else     {dfX <- df0 |> filter(xRef >= xVal)}
    dfX  <- dfX |> rename_at(c(oldXX), ~newX |> paste0(boundX))
    return(dfX)
  }) |>
    # set_names(bounds0) |> bind_rows(.id=boundCol0)
    reduce(left_join, by=c(yrCol0, xVal0))

  ### Get Type
  df0     <- df0 |> mutate(matchType = case_when(
    xLo |> is.na() ~ 0,
    xHi |> is.na() ~ 2,
    xLo == xHi ~ 1,
    .default = NA
  )) ### End mutate

  ### Add adjustment
  # df0     <- df0 |> mutate(eqMod = modelLo == modelHi)
  df0     <- df0 |>
    mutate(numer = xHi - xVal) |>
    mutate(denom = xHi - xLo ) |>
    mutate(adj   = case_when(matchType == 1 ~ 1, .default = numer / denom))

  ### Rename yValue and return
  df0     <- df0 |> rename_at(c(xVal0), ~xCol0)

  ### Return
  gc()
  return(df0)

}


## fredi_slrInterp
fredi_slrInterp <- function(
    # ids0,     ### Unique scenario IDs
  df0,      ### Tibble with unique scenario IDs with hasScenario column
  df1,      ### Tibble with slr impacts by SLR scenario (model)
  df2,      ### slrScenario
  minYr0    = "frediData" |> get_frediDataObj("fredi_config", "minYear0"),
  maxYr0    = "frediData" |> get_frediDataObj("fredi_config", "maxYear0"),
  xCol0     = "modelUnitValue", ### Column to look for the driver value
  yCol0     = "scaled_impacts", ### Column to store results in
  idCol0    = "scenario_id",
  yrCol0    = "year",
  modCol0   = "model"
){
  ### Columns & Values
  ### Rename y Column and join df0 and slrDf
  loStr0   <- "Lo"
  hiStr0   <- "Hi"
  yStr0    <- "y"
  bounds0  <- c(loStr0, hiStr0)
  modLvls0 <- df2 |> pull(all_of(modCol0)) |> levels()

  ### Format Data
  ### Initialize results, then join slr scenario info into impacts
  # df0      <- tibble(id = ids0) |> rename_at(c("id"), ~idCol0) |> cross_join(df2)
  ### Factor models in impacts data
  ### Rename df1 at yCol0
  select0  <- c(idCol0, modCol0, yrCol0, yCol0)
  df1      <- df1 |>
    filter_at(c(idCol0), function(x, y=df0 |> pull(all_of(idCols0))){x %in% y}) |>
    mutate_at(c(modCol0), factor, modLvls0) |>
    select(all_of(select0)) |>
    rename_at(c(yCol0, yStr0))

  ### Join drivers to scenario IDs, then join with Lo/Hi impacts
  ### c("scenario_id", "model", "year", "scaled_impacts")
  ### Join by idCol0, yrCol0, modCol0
  old0     <- c(modCol0, yStr0)
  newLo0   <- old0 |> paste0(loStr0)
  newHi0   <- old0 |> paste0(hiStr0)
  joinLo0  <- c(idCol0, yrCol0) |> paste0(loStr0)
  joinHi0  <- c(idCol0, yrCol0) |> paste0(hiStr0)
  df0      <- df0 |> cross_join(df2)
  df0      <- df0 |>
    left_join(df1 |> rename_at(c(old0), ~new0), by=joinLo0) |>
    left_join(df1 |> rename_at(c(old0), ~new0), by=joinHi0)
  rm(df1)

  ### Calculate Scaled Impacts, then rename yCol0
  df0      <- df0 |> mutate(yVal = yLo + (yHi - yLo) * (1 - adj))
  df0      <- df0 |> rename_at(c("yVal"), ~yCol0)

  ### Return
  gc()
  return(df0)
}


## End Document ----------------


