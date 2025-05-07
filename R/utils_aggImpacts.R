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
    msg0    = ""
){
  ### Columns & Values ----------------
  ### Cols
  cols0        <- cols0 |> unique()
  ### Stringts
  groupStr0    <- "group"
  sumStr0      <- "sum"
  commaStr     <- ", "
  tabStr       <- "\t"
  msg1         <- msg0 |> paste0(tabStr |> rep(1) |> paste(collapse=""))
  msg2         <- msg0 |> paste0(tabStr |> rep(2) |> paste(collapse=""))
  msg3         <- msg0 |> paste0(tabStr |> rep(3) |> paste(collapse=""))
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
    msg1 |> paste0("Warning for ", colStr0, ":") |> message()
    ### Specific columns
    msgsW  <- list(name0=listNames, x0=nDrop0, y0=dropStrs, list0=listMsg) |>
      pmap(function(name0, x0, y0, list0){
        if(x0) {
          msg2 |> paste0(list0$str1, list0$str0, " = c(", y0, ")", list0$str2, "!") |> message()
          msg2 |> paste0("Dropping these columns from ", colStr0, "...") |> message()
        } ### End if(x0)
        return()
      }) ### End pmap
    ### Empty column warning
    if(!nCols0) {
      msg2 |> paste0(remainStr0 |> str_to_title(), colStr0, " = c() has length=0!") |> message()
      msg3 |> paste0(str3, "...")
      # return()
    } ### End if(!nGroups)
  } ### End if(msgG & msgUser)

  ### Return ----------------
  return(cols0)
}

## Aggregation Functions ----------------
### Function to interpolate between impact years
### Separate into years after 2090 and before 2090
interpolate_impYear <- function(
    data,
    col0    = c("impactYear"),
    group0  = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "model_type"),
    sum0    = c("physical_impacts", "annual_impacts"),
    yrCol0  = c("year"),
    naStr0  = c("N/A"),
    newStr0 = c("Interpolation"),
    dfYrs0  = "co_impactYears" |>
      get_frediDataObj("frediData", "rDataList") |>
      (function(df0, col0="impactYear_label", col1="impYr", naStr="N/A"){
        df0 <- df0 |> mutate_at(c(col0), na_if, naStr)
        df0 <- df0 |> filter_at(c(col0), function(x){!(x |> is.na())})
        df0 <- df0 |> select(all_of(col0)) |> distinct() |> arrange_at(c(col0))
        df0 <- df0 |> rename_at(c(col0), ~col1)
        df0 <- df0 |> mutate(impYrNum = impYr |> as.numeric())
        df0 <- df0 |> mutate(row0 = row_number())
        df0 <- df0 |> arrange_at(c(col1), desc)
        return(df0)
      })(),
    maxYr0  = dfYrs0 |> pull(impYrNum) |> max()
){
  # if(msgUser){msg0 (1) |> paste0("Interpolating between impact year estimates...") |> message()}
  ### Columns ----------------
  names0        <- data   |> names()
  group0        <- group0 |> get_matches(y=names0) |> get_matches(y=c(col0, yrCol0), matches=F)
  sum0          <- sum0   |> get_matches(y=names0)

  ### Format data ----------------
  ### Ungroup first, then add columns to help with processing data
  data          <- data |>
    group_by_at(c(group0), .add=FALSE) |>
    mutate(id=cur_group_id()) |>
    mutate_at(c(col0), na_if, naStr0) |>
    mutate(impYrNum  = data |> pull(all_of(col0)) |> as.numeric()) |>
    mutate(impYrType = case_when(
      impYrNum |> is.na() ~ 0,
      year > maxYr0 ~ case_when(impYrNum == maxYr0 ~ 1, .default = 2),
      .default = 3
    ))

  ### Filter data ----------------
  ### - Data that doesn't need interpolation:
  ###   - Filter to impYrType < 2
  ###   - For impYrType == 1, filter to higher impact year estimate
  filter0       <- c("impYrType")
  dataNA        <- data |>
    filter_at(c(filter0), function(x, y=2){x < y}) |>
    select(-any_of(filter0)) |>
    ungroup()
  ### - Data that does need interpolation:
  data          <- data |>
    filter_at(c(filter0), function(x, y=2){x > y}) |>
    select(-any_of(filter0))
  ### Drop filter0
  rm(filter0)

  ### Interpolate Data ----------------
  ### Separate and Join Interpolation Data ----------------
  ## Group data and separate into group data and necessary data
  # rename_at(c("id"), ~idCol0)
  idCol0        <- c("id")
  xCol0         <- c("x")
  impYrCol0     <- c("impYrNum")
  join0         <- c(idCol, yrCol0)
  select0       <- c(join0, cols0)
  ids0          <- data    |> pull(id) |> unique()
  dataKeys      <- data    |> group_keys() |> mutate(id = ids0)
  data          <- data    |> ungroup()    |> select(all_of(select0))
  data          <- dfYrs0  |>
    select(c("impYr", "row0")) |>
    map(function(impYr, row0, colX=impYrCol0, colsX=c(impYrCol0, sum0), colsY=xCol0 |> c(sum0)){
    data |>
        filter_at(c(colX), function(x, y=impYr){x %in% y}) |>
        rename_at(c(colsX), ~colsY |> paste0(row0))
    }) |> reduce(left_join, by=join0)
  rm(join0, select0, ids0)

  ### Calculate Impacts ----------------
  ### Calculate year adjustment
  rows0         <- dfYrs0 |> pull(row0) |> unique()
  sumCols2      <- sum0   |> paste0(rows0 |> max())
  sumCols1      <- sum0   |> paste0(rows0 |> min())
  drop0         <- xCol0  |> paste0(rows0) |> c(sumCols2, sumCols1)
  data          <- data   |>
    mutate(numer = year - x1) |>
    mutate(denom = x2 - x1) |>
    mutate(adj   = numer / denom)
  ### Calculate value
  data[,sum0]   <- data[,sumCols1] + (data[,sumCols2] - data[,sumCols1]) * (data$adj)
  ### Drop columns
  data          <- data |> select(-any_of(drop0))

  ### Join and Bind Data ----------------
  ### Join data with keys
  join0         <- c(idCol, yrCol0)
  data          <- data |> left_join(dataKeys, by=join0)
  rm(dataKeys)
  ### Bind data
  data          <- data |> bind_rows(dataNA)
  rm(dataNA)
  ### Arrange, mutate, drop
  data          <- data |>
    arrange_at(c(join0)) |>
    rename_at(c(impYrCol0), ~col0) |>
    mutate_at(c(col0), as.character) |>
    mutate_at(c(col0), function(x, y=naStr0){y})
  # rm(dataKeys, dataNA)

  ### Return ----------------
  return(data)
} ### if(doIYear)

### Function to summarize, accounting for non-NA values
get_nonNAValues <- function(
    df0,
    col0   = "annual_impacts",
    fun0   = "sum",
    naStr0 = "_na",
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
    cols0  = c("model"),
    group0 = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "model_type"),
    sum0   = c("physical_impacts", "annual_impacts"),
    lbl0   = c("Average"),
    fun0   = c("mean"),
    slrStr = c("slr"),
    yrCol0 = c("year"),
    na.rm  = TRUE
){
  ### Calculate number of non missing values
  # idCol1 <- idCol0 |> paste0("X")
  # data[[idCol1]] <- data[[idCol0]] |> paste0("_", data[[yrCol0]])
  idCol0   <- c("id")
  group0   <- group0 |> get_matches(y=cols0, matches=F) |> c(yrCol0)
  names0   <- data |> names() |> c(idCol0) |> unique()
  data     <- data |>
    group_by_at(c(group0), .add=F) |>
    mutate(id = cur_group_id())

  ### Separate data into values that require interpolation and those that don't
  slrStr   <- slrStr |> tolower()
  dataNA   <- data |> filter_at(c(cols0), function(x, y=slrStr){(x |> tolower()) %in% y})
  data     <- data |> filter_at(c(cols0), function(x, y=slrStr){!((x |> tolower()) %in% y)})
  ### Select data
  select0  <- c(idCol0, sum0)
  naStr0   <- "_na"
  naCols0  <- sum0  |> paste0(naStr0)
  ids0     <- data  |> pull(id) |> unique()
  dfKeys   <- data  |> group_keys() |> mutate(id = ids0)
  data     <- data  |> group_by_at(c(idCol0)) |> select(all_of(select0))
  data     <- sum0  |>
    map(function(col0){data |> get_nonNAValues(col0=col0, fun0=fun0)}) |>
    reduce(left_join, by=idCol0)
  ### Join data and keys
  data     <- dfKeys |>
    left_join(data, by=idCol0) |>
    mutate(model = lbl0)
  ### Bind data
  data     <- data  |> select(all_of(names0)) |> bind_rows(dfNA)
  rm(dfNA)
  ### Add impact type and arrange data
  data     <- data  |>
    arrange_at(c(idCol0, col0)) |>
    mutate_at(c(col0), function(x, y=lbl0){y}) |>
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
  # idCol1 <- idCol0 |> paste0("X")
  # data[[idCol1]] <- data[[idCol0]] |> paste0("_", data[[yrCol0]])
  idCol0   <- c("id")
  # group0   <- group0 |> get_matches(y=cols0, matches=F) |> c(yrCol0)
  group0   <- group0 |> get_matches(y=cols0, matches=F)
  names0   <- data |> names() |> c(idCol0) |> unique()
  # data |> glimpse()
  data     <- data |>
    group_by_at(c(group0)) |>
    mutate(id = cur_group_id())
  # data |> glimpse()
  ### Select data
  join0    <- c(idCol0, yrCol0)
  select0  <- join0 |> c(sum0)
  naStr0   <- "_na"
  naCols0  <- sum0  |> paste0(naStr0)
  ids0     <- data  |> pull(id) |> unique()
  dfKeys   <- data  |> group_keys() |> mutate(id = ids0)
  # dfSum    <- data  |> group_by_at(c(idCol0)) |> select(all_of(select0))
  dfSum    <- data  |> group_by_at(c(join0)) |> select(all_of(select0))
  dfSum    <- sum0  |>
    map(function(col0){
      dfSum |> get_nonNAValues(col0=col0, fun0=fun0)
    }) |>
    set_names(sum0) |>
    reduce(left_join, by=join0)
  ### Join data and keys
  ### Add labels
  dfSum    <- dfKeys |>
    left_join(dfSum, by=idCol0) |>
    cross_join(natLbls0)
  ### Bind data
  data     <- data  |> ungroup() |> select(all_of(names0))
  dfSum    <- dfSum |> ungroup() |> select(all_of(names0))
  data     <- data  |> bind_rows(dfSum)
  rm(dfSum)
  ### Arrange data and drop columns
  data     <- data  |>
    arrange_at(c(idCol0, cols0)) |>
    select(-any_of(idCol0))
  # data |> glimpse()
  ### Return
  return(data)
}


### Function to sum over impact types
sum_impType <- function(
    data,  ### Grouped data
    cols0  = c("impactType"),
    group0 = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model", "model_type"),
    sum0   = c("physical_impacts", "annual_impacts"),
    lbl0   = c("all"),
    fun0   = c("sum"),
    naStr0 = c("N/A", "NA"),
    yrCol0 = c("year"),
    na.rm  = TRUE
){
  ### Calculate number of non missing values
  # idCol1 <- idCol0 |> paste0("X")
  # data[[idCol1]] <- data[[idCol0]] |> paste0("_", data[[yrCol0]])
  idCol0   <- c("id")
  group0   <- group0 |> get_matches(y=cols0, matches=F)
  names0   <- data   |> names() |> unique()
  data     <- data |>
    group_by_at(c(group0)) |>
    mutate(id = cur_group_id())

  ### Separate data into values that require interpolation and those that don't
  dataNA   <- data |> filter_at(c(cols0), function(x, y=naStr0){x %in% y})
  data     <- data |> filter_at(c(cols0), function(x, y=naStr0){!(x %in% y)})
  hasData  <- data |> nrow()
  if(hasData) {
    # data |> glimpse()
    ### Select data
    join0    <- c(idCol0, yrCol0)
    select0  <- join0 |> c(sum0)
    # select0  <- c(idCol0, sum0)
    naStr0   <- "_na"
    naCols0  <- sum0  |> paste0(naStr0)
    ids0     <- data  |> pull(id) |> unique()
    dfKeys   <- data  |> group_keys() |> mutate(id = ids0)
    # data     <- data  |> group_by_at(c(idCol0)) |> select(all_of(select0))
    dfSum    <- data  |> group_by_at(c(join0)) |> select(all_of(select0))
    data     <- sum0  |>
      map(function(col0){
        data |> get_nonNAValues(col0=col0, fun0=fun0)}
      ) |>
      set_names(sum0) |>
      reduce(left_join, by=join0)
    ### Join data and keys
    data     <- dfKeys |>
      left_join(data, by=idCol0) |>
      # mutate(impactType = NA)
      mutate(impactType = NA)
  } ### End if(hasData)
  ### Bind data
  # names0 |> print()
  data     <- data  |>
    ungroup() |>
    select(-any_of(idCol0)) |>
    select(all_of(names0)) |>
    bind_rows(dataNA) |>
    mutate_at(c(cols0), function(x, y=lbl0){y})
  rm(dataNA)
  # ### Add impact type and arrange data
  # data     <- data  |>
  #   # arrange_at(c(idCol0, yrCol0)) |>
  #   mutate_at(c(col0), function(x, y=lbl0){y})
  ### Return
  return(data)
}
