## Get Unique Scenarios ----------------
### Functions to get unique scenarios from data
get_uniqueDf0 <- function(
    df0,
    group0  = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "model_type"),
    cols0   = c("year", "gdp_usd", "gdp_percap", "national_pop", "pop"),
    type0   = "first" ### Or slice
){
  ### Type
  type0   <- type0 |> tolower()
  doFirst <- "first" %in% type0
  doSlice <- "slice" %in% type0
  ### Format Data
  ### Get group0 in names0
  ### Group, add ID, filter to one group, ungroup, and drop ID column
  names0     <- df0    |> names()
  group0     <- group0 |> get_matches(y=names0)
  df0        <- df0    |>
    group_by_at(c(group0)) |>
    mutate(id=cur_group_id())
  ### Filter to first
  if(doFirst) {
    df0 <- df0 |> filter(id == 1)
  } else if(doSlice) {
    df0 <- df0 |> slice(1)
  } ### End if(doFirst)

  ### Select and get distinct
  df0        <- df0 |> ungroup() |> select(any_of(cols0))
  df0        <- df0 |> distinct()

  ### Return
  return(df0)
}

## Select Columns ----------------
### Function to adjust columns and message user
aggImpacts_adjustColumns <- function(
    cols0,
    names0  = c()  , ### Names of data
    groups0 = c()  , ### Grouping columns (provide if typ0=="sum)
    doNat   = FALSE, ### Aggregate over national
    doIType = TRUE , ### Aggregate over impact types
    type0   = "group", ### Or sum
    # msg0    = ""
    msg0    = 0
){
  #### Messaging ----------------
  ### Not used currently; preserving it in messaging logicals for the future
  msgN        <- "\n"
  msg0        <- 0
  msg1        <- msg0 + 1
  msg2        <- msg0 + 2
  msg3        <- msg0 + 3

  ### Columns & Values ----------------
  ### Cols
  cols0        <- cols0 |> unique()
  ### Stringts
  groupStr0    <- "group"
  sumStr0      <- "sum"
  commaStr     <- ", "
  tabStr       <- "\t"
  # msg1         <- msg0 |> paste0(tabStr |> rep(1) |> paste(collapse=""))
  # msg2         <- msg0 |> paste0(tabStr |> rep(2) |> paste(collapse=""))
  # msg3         <- msg0 |> paste0(tabStr |> rep(3) |> paste(collapse=""))
  ### Conditionals
  type0        <- type0 |> tolower()
  doGroup      <- groupStr0 %in% type0
  doSum        <- sumStr0   %in% type0
  #### Scalar columns
  annSumCol0   <- cols0[cols0 |> tolower() |> str_detect(c("annual_impacts"))]
  physSumCol0  <- cols0[cols0 |> tolower() |> str_detect(c("physical_impacts"))]
  scaledSumCol <- cols0[cols0 |> tolower() |> str_detect(c("scaled_impacts"))]
  physMeasCol0 <- cols0[cols0 |> tolower() |> str_detect(c("physicalmeasure"))]
  scalarTypes0 <- c("phys", "damage", "econ")
  scalarStrs0  <- c("Scalar", "Adj", "Multiplier")
  scalarCols0  <- scalarTypes0 |> map(paste0, scalarStrs0) |> unlist()
  scalarNames0 <- scalarCols0  |> paste0("Name")
  scalarVals0  <- cols0[cols0 |> tolower() |> str_detect(scalarNames0 |> paste0("Value") |> c(scalarCols0) |> tolower() |> paste(collapse="|"))]
  # c("physScalar", "physAdj", "damageAdj", "econScalar", "econAdj", "econMultiplier") |> paste("Name")

  ### Columns List ----------------
  ### List of columns to check
  # colGroups    <- c("scalarVal", "scalarName", "scaledVal", "physVal")
  ### Add values to list
  listCols     <- list()
  listCols[["na"]] <- list(colsX=names0, matchX=T)
  if(doSum) {
    baseCols <- c(scaledSumCol, scalarVals0)
    listCols[["grp"]] <- list(colsX=groups0, matchX=F)
    ### If do national, drop scaled impacts
    ### If do impact types, drop scaled impacts, physical impacts
    if(doNat|doIType) {
      listCols[["nat"]] <- list(colsX=baseCols, matchX=F)
    } else if(doIType) {
      listCols[["imp"]] <- list(colsX=baseCols |> c(physSumCol0), matchX=F)
    } ### End if(doIType)
  } else if(doGroup) {
    sumCols <- c(annSumCol0, physSumCol0, scaledSumCol)
    listCols[["sum"]] <- list(colsX=sumCols, matchX=F)
    if(doNat|doIType) {
      listCols[["nat"]] <- list(colsX=c(scalarVals0), matchX=F)
    } else if(doIType) {
      listCols[["imp"]] <- list(colsX=c(physMeasCol0, scalarNames0, scalarVals0), matchX=F)
    } ### End if(doIType)
  } ### End if(doNat)
  ### List names
  listNames    <- listCols |> names()

  ### Check Columns ----------------
  ### Values
  # listNames |> print()
  checkCols    <- listCols |>
    map(function(x0, colsX=cols0){
      colsX |> get_matches(y=x0[["colsX"]], matches=x0[["matchX"]], type="matches")
    }) |> set_names(listNames)
  dropCols     <- checkCols |> map(function(x, y=cols0){y[!x]}) |> set_names(listNames)
  dropStrs     <- checkCols |> map(function(x, z=commaStr){x |> paste(collapse=z)}) |> set_names(listNames)
  dropVals     <- checkCols |> (function(x0, y0=`&`){Reduce(y0, x0)})()
  # dropVals |> print()
  cols0        <- cols0[dropVals]
  # cols0 |> print()
  ### Message user if some columns aren't present
  nCols0       <- cols0    |> length()
  nDrop0       <- dropCols |> map(function(x){x |> length()}) |> set_names(listNames)
  # nDrops0      <- dropVals |> sum()
  nDrops0      <- (!dropVals) |> sum()
  # nDrops0 |> print()
  warning0     <- nDrops0  |  !nCols0

  ### Message Info ----------------
  ### Message strings
  colStr0      <- type0    |> paste0("Cols")
  actionStr0   <- doSum    |> ifelse("aggregate over ", "group by ")
  otherStr0    <- doSum    |> ifelse("summary columns ", "grouping columns ")
  str3         <- doSum    |> ifelse("At least one column required for aggregating", "This could results in non-sensical or double-counted results")
  remainStr0   <- nDrops0  |> ifelse("remaining ", "")
  msgW         <- warning0 |> ifelse(msg2, msg1)
  ### Add values to list
  listMsg     <- list()
  listMsg[["na"]] <- list(str0=colStr0, str2="not present in data")

  if(doSum) {
    listMsg[["grp"]] <- list(str0=otherStr0, str1=actionStr0)
  } else if(doGroup) {
    listMsg[["sum"]] <- list(str0=otherStr0, str1=actionStr0)
  } ### End if(doNat)

  if(doNat|doIType) {
    listMsg[["nat"]] <- list(str0=colStr0, str1=actionStr0, str2="when 'national' or 'impacttype' present in aggLevels")
  } else if(doIType) {
    listMsg[["imp"]] <- list(str0=colStr0, str1=actionStr0, str2="when 'impacttype' present in aggLevels")
  } ### End if(doIType)


  ### Message User ----------------
  if(warning0) {
    msg1 |> get_msgPrefix() |> paste0("Warning for ", colStr0, ":") |> message()
    ### Specific columns
    msgsW  <- list(name0=listNames, x0=nDrop0, y0=dropStrs, list0=listMsg) |>
      pmap(function(name0, x0, y0, list0){
        if(x0) {
          msg2 |> get_msgPrefix() |> paste0(list0$str1, list0$str0, " = c(", y0, ")", list0$str2, "!") |> message()
          msg2 |> get_msgPrefix() |> paste0("Dropping these columns from ", colStr0, "...") |> message()
        } ### End if(x0)
        return()
      }) ### End pmap
    ### Empty column warning
    if(!nCols0) {
      msg2 |> get_msgPrefix() |> paste0(remainStr0 |> str_to_title(), colStr0, " = c() has length=0!") |> message()
      msg3 |> get_msgPrefix() |> paste0(str3, "...")
      # return()
    } ### End if(!nGroups)
  } ### End if(msgG & msgUser)

  ### Return ----------------
  return(cols0)
}

## Aggregation Functions ----------------
### Function to interpolate between impact years
### Separate into years after 2090 and before 2090
# dfYrs0  = "frediData" |>
#   get_frediDataObj("stateData", "co_impactYears") |>
#   (function(df0, col0="impactYear_label", col1="impYr", naStr="N/A"){
#     df0 <- df0 |> mutate_at(c(col0), na_if, naStr)
#     df0 <- df0 |> filter_at(c(col0), function(x){!(x |> is.na())})
#     df0 <- df0 |> select(all_of(col0)) |> distinct() |> arrange_at(c(col0))
#     df0 <- df0 |> rename_at(c(col0), ~col1)
#     df0 <- df0 |> mutate(impYrNum = impYr |> as.numeric())
#     df0 <- df0 |> mutate(row0 = row_number())
#     df0 <- df0 |> arrange_at(c(col1), desc)
#     return(df0)
#   })()
interpolate_impYear <- function(
    data,
    col0    = c("impactYear"),
    group0  = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "model_type"),
    sum0    = c("physical_impacts", "annual_impacts"),
    lbl0    = c("Interpolation"),
    naStr0  = c("N/A", "Interpolation"),
    yrCol0  = c("year"),
    dfYrs0  = "controlData" |>
      get_frediDataObj("co_impYrLvls") |>
      filter(!(impYrNum |> is.na())) |>
      arrange_at(c("impYrNum")) |>
      select(c("impYrNum", "row0")) |>
      rename_at(c("impYrNum", "row0"), ~c("yr0", "num0"))
){
  # if(msgUser){msg0 (1) |> paste0("Interpolating between impact year estimates...") |> message()}
  ### Columns & Values ----------------
  ### Columns
  idCol0   <- "id"
  xCol0    <- "x"
  numCol0  <- "yr0"
  typeCol0 <- "impYrType"
  names0   <- data   |> names() |> c(idCol0)
  group0   <- group0 |> get_matches(y=col0, matches=F)
  sum0     <- sum0   |> get_matches(y=names0)

  ### Values
  minYr0   <- dfYrs0 |> pull(all_of(numCol0)) |> min()
  maxYr0   <- dfYrs0 |> pull(all_of(numCol0)) |> max()

  ### Format data ----------------
  ### Group and add id
  ### Add info about impact year
  data     <- data |>
    group_by_at(c(group0), .add=F) |>
    mutate(id=cur_group_id()) |>
    rename_at(c(col0), ~xCol0) |>
    mutate(x = case_when(x %in% naStr0 ~ NA, .default=x)) |>
    mutate_at(c(xCol0), as.numeric) |>
    mutate(impYrType = case_when(
      x |> is.na() ~ 0,
      year > maxYr0 ~ case_when(
        x == maxYr0 ~ 1, .default = 2),
      .default = 3
    ))
  # return(data)
  # data |> glimpse()

  ### Filter data ----------------
  ### - Data that doesn't need interpolation:
  ###   - Filter to impYrType < 2 (NA or year > maxYr0)
  dataNA   <- data |>
    filter_at(c(typeCol0), function(x, y=c(0, 1)){x %in% y}) |>
    select(-any_of(typeCol0)) |>
    rename_at(c(xCol0), ~col0)
  ### - Data that does need interpolation: Filter and rename
  data     <- data |>
    filter_at(c(typeCol0), function(x, y=3){x %in% y}) |>
    select(-any_of(typeCol0))
  # dataNA |> glimpse(); data |> glimpse();

  ### Interpolate Data ----------------
  ### Separate and Join Interpolation Data ----------------
  ## Group data and separate into group data and necessary data
  hasData  <- data  |> nrow()
  if(hasData) {
    join0    <- c(idCol0, yrCol0)
    select0  <- join0 |> c(xCol0) |> c(sum0)
    # select0   <- c(join0, cols0)
    ids0     <- data |> pull(id) |> unique()
    dataKeys <- data |> group_keys() |> mutate(id = ids0)
    # data      <- data |> ungroup()    |> select(all_of(select0))
    data     <- data |> group_by_at(c(join0)) |> select(all_of(select0))
    ### Filter data
    data     <- dfYrs0 |>
      pmap(filter_impYrs, col0=xCol0, sum0=sum0, df0=data) |>
      reduce(left_join, by=join0)

    ### Calculate Impacts ----------------
    ### Calculate year adjustment
    # sumCols0 <- dfYrs0 |> pull(row0) |> map(function(x, y=sum0){y |> paste0(x)})
    sumCols1 <- sum0 |> paste0(1)
    sumCols2 <- sum0 |> paste0(2)
    data     <- data   |>
      mutate(numer = year - x1) |>
      mutate(denom = x2 - x1) |>
      mutate(adj   = numer / denom)
    ### Calculate value
    data[,sum0] <- data[,sumCols1] + (data[,sumCols2] - data[,sumCols1]) * (data$adj)

    ### Join and Bind Data ----------------
    ### Join data with keys
    data     <- data |>
      left_join(dataKeys, by=idCol0) |>
      mutate(x = NA)
    rm(dataKeys)
  } ### End if(hasData)

  ### Rename xCol0 to col0
  data     <- data   |> rename_at(c(xCol0), ~col0)
  ### Bind data
  dataNA   <- dataNA |> ungroup() |> select(all_of(names0))
  data     <- data   |> ungroup() |> select(all_of(names0))
  data     <- data   |> bind_rows(dataNA)
  rm(dataNA)
  ### Mutate column value, arrange, drop id column
  sort0    <- c(idCol0, col0, yrCol0)
  data     <- data |>
    mutate_at(c(col0), as.character) |>
    mutate_at(c(col0), function(x, y=lbl0){y}) |>
    arrange_at(c(sort0)) |>
    select(-any_of(idCol0))

  ### Return ----------------
  return(data)
} ### if(doIYear)

### Function to filter data to specific impact years
filter_impYrs <- function(
    yr0  = 2010, ### Impact year, as.numeric (or same type as data in col0)
    num0 = 1, ### Order of the year (1 for lower, 2 for upper)
    col0 = "impactYear",
    sum0 = "annual_impacts",
    df0  ### Data to filter
){
  ### Values
  from0 <- c(col0, sum0)
  to0   <- from0 |> paste0(num0)
  ### Filter and rename columns
  df0   <- df0 |>
    filter_at(c(col0), function(x, y=yr0){x %in% y}) |>
    rename_at(c(from0), ~to0)
  ### Return
  return(df0)
}

### Function to summarize, accounting for non-NA values
map_nonNAValues <- function(
    col0   = "annual_impacts",
    fun0   = "sum",
    df0,
    # naStr0 = "_na",
    na.rm  = TRUE
){
  df0 |> get_nonNAValues(col0=col0, fun0=fun0, na.rm=na.rm)
}


get_nonNAValues <- function(
    df0,
    col0   = "annual_impacts",
    fun0   = "sum",
    # naStr0 = "_na",
    na.rm  = TRUE
){
  ### colX |> paste0(naStrX)
  naCol  <- "check"
  yCol   <- "yVal"
  vals0  <- df0 |> pull(all_of(col0))
  # df0 |> glimpse(); vals0 |> length() |> print()
  ### Rename value
  # df0    <- df0 |> mutate(yVal  = vals0)
  df0    <- df0 |>
    rename_at(c(col0), ~yCol) |>
    mutate(check = case_when(yVal |> is.na() ~ 0, .default = 1)) |>
    summarize_at(c(yCol, naCol), .funs=c(fun0), na.rm=na.rm)
  ### Sum vs average
  doSum0 <- "sum"  %in% fun0
  doAve0 <- "mean" %in% fun0
  ### Mutate vals
  if(doSum0) df0 <- df0 |> mutate(yVal = case_when(check == 0 ~ NA, .default = yVal))
  if(doAve0) df0 <- df0 |> mutate(yVal = case_when(check == 0 ~ NA, .default = yVal / check))
  ### Rename and drop check column
  df0   <- df0 |>
    # rename_at(c(naCol, yCol), ~col0 |> paste0(c("", naStrX)))
    rename_at(c(yCol), ~col0) |>
    select(-any_of(naCol))
  ### Return
  return(df0)
}

### Function to calculate model averages
calc_modelAves <- function(
    data,  ### Grouped data
    col0   = c("model"),
    group0 = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "model_type"),
    sum0   = c("physical_impacts", "annual_impacts"),
    lbl0   = c("Average"),
    fun0   = c("mean"),
    naStr0 = c("Interpolation", "Average"),
    # slrStr = c("slr"),
    yrCol0 = c("year"),
    na.rm  = TRUE
){
  ### Calculate number of non missing values
  idCol0   <- "id"
  group0   <- group0 |> get_matches(y=col0, matches=F)
  names0   <- data |> names() |> c(idCol0) |> unique()
  data     <- data |>
    group_by_at(c(group0)) |>
    mutate(id = cur_group_id())

  ### Add column label to NA strings
  ### Separate data into values that require interpolation and those that don't
  naStr0   <- naStr0 |> c(lbl0) |> unique()
  dataNA   <- data |> filter_at(c(col0), function(x, y=naStr0){x %in% y})
  data     <- data |> filter_at(c(col0), function(x, y=naStr0){!(x %in% y)})
  ### Select data
  join0    <- c(idCol0, yrCol0)
  select0  <- join0 |> c(sum0)
  ids0     <- data  |> pull(id) |> unique()
  dfKeys   <- data  |> group_keys() |> mutate(id = ids0)
  dfSum    <- data  |> group_by_at(c(join0)) |> select(all_of(select0))
  dfSum    <- sum0  |>
    map(map_nonNAValues, fun0=fun0, df0=dfSum, na.rm=na.rm) |>
    set_names(sum0)
  ### Join data and add label
  dfSum    <- dfSum |>
    reduce(left_join, by=join0) |>
    left_join(dfKeys, by=idCol0) |>
    mutate(colVal = lbl0) |>
    rename_at(c("colVal"), ~col0)
  ### Bind data
  dataNA   <- dataNA |> ungroup() |> select(all_of(names0))
  dfSum    <- dfSum  |> ungroup() |> select(all_of(names0))
  data     <- data   |> ungroup() |> select(all_of(names0))
  data     <- data   |>
    bind_rows(dfSum) |>
    bind_rows(dataNA)
  rm(dfSum, dataNA)
  ### Arrange data, drop id column
  sort0    <- c(idCol0, col0, yrCol0)
  data     <- data  |>
    arrange_at(c(sort0)) |>
    select(-any_of(idCol0))
  ### Return
  return(data)
}

### Function to calculate non-NA values
sum_national <- function(
    data,   ### Grouped data
    cols0    = c("region", "state", "postal"),
    group0   = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "model_type"),
    sum0     = c("physical_impacts", "annual_impacts"),
    # lbls0    = list(region="National Total", state="All", postal="US"),
    natLbls0 = controlData$co_states |>
      filter(postal %in% "US") |>
      select(c("region_label", "state", "postal")) |>
      rename_at(c("region_label"), ~"region"),
    fun0     = c("sum"),
    yrCol0   = c("year"),
    na.rm    = TRUE
){
  ### Calculate number of non missing values
  idCol0   <- "id"
  postCol0 <- "postal"
  # group0   <- group0 |> get_matches(y=cols0, matches=F) |> c(yrCol0)
  group0   <- group0 |> get_matches(y=cols0, matches=F)
  # names0   <- data   |> names() |> c(idCol0) |> unique()
  names0   <- data   |> names() |> get_matches(y=c(group0, cols0, yrCol0, sum0)) |> c(idCol0) |> unique()
  # data |> glimpse()
  # select0  <- names0 |> get_matches(y=c(group0, cols0, yrCol0, sum0))
  data     <- data |>
    # select(all_of(select0)) |>
    group_by_at(c(group0)) |>
    mutate(id = cur_group_id())
  # data |> glimpse()
  ### Drop any national values
  lblCols0 <- natLbls0 |> names()
  naStr0   <- natLbls0 |> pull(all_of(postCol0))
  data     <- data     |> filter_at(c(postCol0), function(x, y=naStr0){!(x %in% y)})
  ### Select data
  join0    <- c(idCol0, yrCol0)
  select0  <- join0 |> c(sum0)
  ids0     <- data  |> pull(id) |> unique()
  dfKeys   <- data  |> group_keys() |> mutate(id = ids0)
  # dfSum    <- data  |> group_by_at(c(idCol0)) |> select(all_of(select0))
  dfSum    <- data  |> group_by_at(c(join0)) |> select(all_of(select0))
  dfSum    <- sum0  |>
    map(map_nonNAValues, fun0=fun0, df0=dfSum, na.rm=na.rm) |>
    set_names(sum0)
  ### Join data and keys, add labels
  # dfKeys |> glimpse()
  # return(dfSum)
  dfSum    <- dfSum |>
    reduce(left_join, by=join0) |>
    left_join(dfKeys, by=idCol0) |>
    cross_join(natLbls0)
  # return(dfSum)
  ### Bind data
  data     <- data  |> ungroup() |> select(all_of(names0))
  dfSum    <- dfSum |> ungroup() |> select(all_of(names0))
  data     <- data  |> bind_rows(dfSum)
  rm(dfSum)
  ### Arrange data and drop id column
  sort0    <- c(idCol0, postCol0, yrCol0)
  data     <- data  |>
    arrange_at(c(sort0)) |>
    select(-any_of(idCol0))
  # data |> glimpse()
  ### Return
  return(data)
}


### Function to sum over impact types
sum_impType <- function(
    data,  ### Grouped data
    col0   = c("impactType"),
    group0 = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "model_type"),
    sum0   = c("physical_impacts", "annual_impacts"),
    lbl0   = "all",
    naStr0 = c("N/A", "NA", "all"),
    fun0   = "sum",
    yrCol0 = "year",
    na.rm  = TRUE
){
  ### Calculate number of non missing values
  idCol0   <- "id"
  group0   <- group0 |> get_matches(y=col0, matches=F)
  names0   <- data   |> names() |> c(idCol0) |> unique()
  data     <- data |>
    group_by_at(c(group0)) |>
    mutate(id = cur_group_id())
  # lbl0 |> glimpse(); idCol0 |> glimpse(); col0 |> glimpse(); yrCol0 |> glimpse()
  ### Add impact type label to NA strings
  ### Separate data into values that require interpolation and those that don't
  naStr0   <- naStr0 |> c(lbl0) |> unique()
  dataNA   <- data |> filter_at(c(col0), function(x, y=naStr0){x %in% y})
  # data     <- data |> filter_at(c(col0), function(x, y=naStr0){!(x %in% y)})
  data     <- data |> filter_at(c(col0), function(x, y=naStr0){!(x %in% y)})
  # rm(data)
  hasData  <- data  |> nrow()
  if(hasData) {
    # data |> glimpse()
    ### Select data
    join0    <- c(idCol0, yrCol0)
    select0  <- join0 |> c(sum0)
    # select0  <- c(idCol0, sum0)
    ids0     <- data  |> pull(id) |> unique()
    dfKeys   <- data  |> group_keys() |> mutate(id = ids0)
    data     <- data  |> group_by_at(c(join0)) |> select(all_of(select0))
    data     <- sum0  |>
      map(map_nonNAValues, fun0=fun0, df0=data , na.rm=na.rm) |>
      set_names(sum0)
    ### Join data and add label
    data     <- data  |>
      reduce(left_join, by=join0) |>
      left_join(dfKeys, by=idCol0) |>
      mutate(colVal = lbl0) |>
      rename_at(c("colVal"), ~col0)
  } ### End if(hasData)
  ### Bind data
  dataNA   <- dataNA |> ungroup() |> select(all_of(names0))
  data     <- data   |> ungroup() |> select(all_of(names0))
  data     <- data   |> bind_rows(dataNA)
  rm(dataNA)
  ### Add impact type, arrange data, drop id column
  sort0    <- c(idCol0, col0, yrCol0)
  # lbl0 |> glimpse(); idCol0 |> glimpse(); col0 |> glimpse(); yrCol0 |> glimpse()
  data     <- data   |>
    mutate_at (c(col0), function(x, y=lbl0){y}) |>
    arrange_at(c(sort0)) |>
    select(-any_of(idCol0))
  ### Return
  return(data)
}
