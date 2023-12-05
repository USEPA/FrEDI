###### Helpers ######
### Get column as vector
get_vector <- function(
    data, column = NULL
){
  ### Select column and get values as vector
  col0  <- column |> is.null() |> ifelse(c(), column)
  vals0 <- data[[column]] |> as.vector()
  ### Return
  return(vals0)
}

### Get unique values from a dataframe column
get_uniqueValues <- function(
    data, column = NULL, sort=TRUE
){
  ### Select column and get values as vector
  vals0 <- data  |> get_vector(column)
  vals0 <- vals0 |> unique()
  # vals0 |> print()
  ### Sort
  if(sort){vals0 <- vals0 |> sort()}
  ### Return
  return(vals0)
}

### Get message prefix
get_msgPrefix <- function(level=1){
  ### Other vals
  mnl0   <- "\n" ### Message new line
  msg0   <- "\t" ### Message indent level 0
  mcom   <- ", " ### Comma for collapsing lists
  mqu0   <- "\'" ### Message quote
  mqu1   <- mqu0 |> paste0(mcom, mqu0, collapse="")
  mend0  <- "..."
  msg_x  <- msg0
  return(msg_x)
}

### This function makes it easier to get data objects from the sysdata.rda file
get_frediDataObj <- function(
    x        = NULL,       ### Object name
    listSub  = "frediData", ### Sublist name
    listall  = FALSE,
    listName = "rDataList",
    pkg      = "FrEDI",
    lib.loc  = .libPaths()[1] ### Library path to look for packages
){
  ### Messaging
  msg0    <- "\t"
  ### Check if list name exists
  exists0 <- listName |> exists()
  ### If the listname exists in the name space, parse it
  ### Otherwise, grab it from a package name space
  if(exists0){new_x <- parse(text=listName) |> eval()}
  else       {
    ### Check if package & list name
    pkgList0    <- lib.loc |> installed.packages()
    pkgExists0  <- pkg %in% pkgList0
    if(!pkgExists0){
      msg0 |> paste0("Package doesn't exist...") |> message()
      msg0 |> paste0("Exiting...") |> message()
      return()
    } ### End if(!pkgExists0)
    else           {new_x <- getFromNamespace(listName, ns=pkg)}
  } ### End else(exists0)

  ### Whether to list all items in data object or not
  # if(listall) {return_x <- new_x |> names()}
  # else        {return_x <- new_x[[x]]}
  if(listall) {return_x <- new_x |> names()}
  else        {return_x <- new_x[[listSub]][["data"]][[x]]}
  ### Return
  return(return_x)
} ### End get_frediDataObj

###### interpolate_annual ######
### Created 2021.02.05. Updated 2021.02.05
### Interpolate Annual Values
### This function interpolates missing values for temperature, GDP, or population inputs
interpolate_annual <- function(
    data      = NULL, ### Input dataframe, with list of years
    years     = NULL, ### List of years to interpolate
    column    = NULL, ### Column to create results for
    rule      = NULL, ### for interpolation,
    method    = NULL, ### method for interpolation; default=linear
    region    = "National.Total", ### Region if "addRegion"
    byState   = FALSE ### If breakdown by state
){
  ###### Data Info ######
  ##### Other values
  region0   <- region
  rm(region)
  ##### By state
  if (byState) {stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  ##### Columns
  dataCols  <- data |> names()
  defCols   <- c("year", "region") |> c(stateCols0)
  defCol0   <- dataCols[!(dataCols %in% defCols)][1]
  column0   <- column |> is.null() |> ifelse(defCol0, column)
  othCols   <- dataCols[!(dataCols %in% c(defCols, column0))]
  # defCol0 |> print(); column0 |> print()
  rm(defCol0)

  ###### Format data
  # column0 |> print()
  data      <- data |> filter(!(column0 |> is.na()))
  values0   <- data[[column0]]
  years0    <- data[["year" ]]

  ### Interpolation years
  doYears   <- years |> is.null()
  if(doYears){
    years  <- years0 |> range(na.rm=TRUE)
    years  <- years[1]:years[2]
  } ### End if(doYears)
  rm(doYears)

  ##### Regions
  addRegion <- !("region" %in% dataCols)
  if (addRegion) {data <- data |> mutate(region = region0)}
  rm(addRegion)

  ###### Interpolation Info ######
  ### - Return NA values outside of extremes
  ### - If only one value is provided use the same for left and right
  ### - Interpolation method
  nullRule  <- rule |> is.null()
  repRule   <- rule |> length() < 2
  defRule   <- c(1)
  if(nullRule){rule <- defRule}
  if(repRule ){rule <- rule |> rep(2)}
  method    <- method |> is.null() |> ifelse("linear", method)

  ###### Interpolate NA values ######
  ### Filter to the region and then interpolate missing values
  cols0     <- c("x", "y")
  cols1     <- c("year") |> c(column0)
  ### Iterate over states if byState=T
  ### Otherwise, iterate over regions
  if (byState) {
    states0   <- data |> get_uniqueValues("state" )
    df_interp <- states0 |> map(function(state_i){
      ### Values
      df_i     <- data |> filter(state==state_i)
      x_i      <- df_i[["year" ]]
      y_i      <- df_i[[column0]]
      ### Approximate
      new_i   <- approx(x=x_i, y=y_i, xout=years, rule=rule, method=method)
      new_i   <- new_i |> as_tibble()
      new_i   <- new_i |> rename_at(c(cols0), ~cols1)
      new_i   <- new_i |> mutate(state=state_i)
      # new_i |> names() |> print()
      ### Return
      return(new_i)
    }) |> bind_rows()
  } else { ### By state
    regions0  <- data |> get_uniqueValues("region")
    df_interp <- regions0 |> map(function(region_i){
      ### Values
      df_i     <- data |> filter(region==region_i)
      x_i      <- df_i[["year" ]]
      y_i      <- df_i[[column0]]
      ### Approximate
      new_i   <- approx(x=x_i, y=y_i, xout=years, rule=rule, method=method)
      new_i   <- new_i |> as_tibble()
      new_i   <- new_i |> rename_at(c(cols0), ~cols1)
      new_i   <- new_i |> mutate(region = region_i)
      # new_i |> names() |> print()
      ### Return
      return(new_i)
    }) |> bind_rows()
  } ### End else
  # df_interp |> names() |> print()

  ### Drop yCol from data
  # data |> glimpse(); df_interp |> glimpse(); cols1 |> print()
  data   <- data |> select(-all_of(cols1))
  ### Join with original data:
  names0 <- data      |> names()
  names1 <- df_interp |> names()
  join0  <- names0[names0 %in% names1]
  doJoin <- (names0 |> length() > 0) & (join0 |> length() > 0)
  ### Group data
  data   <- data |>
    group_by_at(c(names0)) |>
    summarize(n=n(), .groups="keep") |> ungroup() |>
    select(-c("n"))

  ### Do join
  if(doJoin){
    # df_interp |> glimpse(); data |> glimpse()
    data <- data |> left_join(df_interp, by=c(join0))
  } else{
    data <- df_interp
  } ### End else

  ### Arrange data
  arrange0 <- c(join0, "year")
  data     <- data  |> arrange_at(c(arrange0))

  ### Return
  return(data)
} ### End function

###### get_scenario_id ######
### Function to standardize the creation of the scenario_id
get_scenario_id <- function(
    data_x,
    include=c("region_dot", "model_dot") ### Character vector of column names to include
){
  ### Other vals
  mnl0   <- "\n" ### Message new line
  msg0   <- "\t" ### Message indent level 0
  mcom   <- ", " ### Comma for collapsing lists
  mqu0   <- "\'" ### Message quote
  mqu1   <- mqu0 |> paste0(mcom, mqu0, collapse="")
  mend0  <- "..."
  ### Columns to include
  main0  <- c("sector", "variant", "impactType", "impactYear")
  cols0  <- main0  |> c(include)
  ### Check names
  names0 <- data_x |> names()
  cCheck <- (cols0 %in% names0)
  nCheck <- (!cCheck) |> which() |> length()
  if(nCheck){
    paste0("In get_scenario_id:") |> message()
    msg0 |> paste0("Data is missing columns ", mqu0, cols0[!cCheck] |> paste(collapse=mqu1), mqu0, mend0) |> message()
    paste0("Creating `scenario_id` from columns ", mqu0, cols0[cCheck] |> paste(collapse=mqu1), mqu0, mend0) |> message()
    ### New names
    cols0  <- cols0[cCheck]
  } ### End if(nCheck)
  ### Subset data
  scen_x <- data_x[,cols0]
  ### Get scenario IDs
  scen_x <- scen_x |> apply(1, function(x){x |> as.vector() |> paste(collapse ="_")}) |> unlist()
  data_x <- data_x |> mutate(scenario_id = scen_x)
  ### Return
  return(data_x)
}

###### summarize_seScenario ######
### Summarize regional population
summarize_seScenario <- function(
    df0, ### Dataframe with population & national values like gdp_usd
    national = TRUE ### If false, summarizes to regional level; otherwise, does national
){
  ### By state
  byState  <- "state" %in% (df0 |> names())
  if( byState ){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  if(!national){
    popCol0  <- byState |> ifelse("state_pop", "reg_pop")
    regCols0 <- c("region") |> c(stateCols0)
  } else{
    # "got here" |> print()
    popCol0  <- c()
    regCols0 <- c()
  } ### End else

  ### Regional column names
  # regCols0 <- regCol0 |> c(stateCols0)
  # regCols0 |> print()
  ### Columns to group by
  group0   <- regCols0 |> c("gdp_usd", "national_pop", "gdp_percap")
  group0   <- group0 |> c("byState", "year")
  ### Columns to drop
  sumCols0 <- popCol0 |> c("n")
  ### Add column count
  df0      <- df0 |> mutate(n=1)

  ### Mutate data
  if(byState){
    ### Add state column
    df0 <- df0 |> mutate(byState = 1)
    ### Bind a copy of the data
    df0 <- df0 |>
      mutate_at(c(stateCols0), function(y){"N/A"}) |>
      mutate(byState = 0) |>
      rbind(df0)
    ### Summarize
    df0 <- df0 |> group_by_at(c(group0)) |> summarize_at(c(sumCols0), sum, na.rm=T) |> ungroup()
    # df0 |> glimpse()
    # rm(group0)
    # } ### End if(byState)
  } else{
    ### Add state column
    df0 <- df0 |> mutate(byState = 0)
  } ### End if(byState)

  ### Drop count column
  drop0    <- c("n")
  df0      <- df0 |> select(-all_of(drop0))

  ### Return
  return(df0)
}


###### update_popScalars ######
### Update scalars with regional population scenario
update_popScalars <- function(
    df_scalars, ### Tibble of scalars
    df_pop,     ### Socioeconomic scenario
    groupCols = c("region") |> c("state", "postal") |> c("year"),
    popCol    = c("state_pop")
){
  ### Drop region population
  df_scalars <- df_scalars |> filter(scalarName!="reg_pop")
  # df_scalars <- df_scalars |> mutate(slrSector="N/A")

  ### Format population data
  drop0      <- c("gdp_usd", "national_pop", "gdp_percap")
  df_pop     <- df_pop |> summarize_seScenario(national=FALSE)
  df_pop     <- df_pop |> select(-any_of(drop0))

  ### Add additional scalar attributes
  df_pop     <- df_pop |> mutate(scalarName           = "reg_pop")
  df_pop     <- df_pop |> mutate(scalarType           = "physScalar")
  df_pop     <- df_pop |> mutate(national_or_regional = "regional")
  ### Rename column
  df_pop     <- df_pop |> rename_at(c(popCol), ~"value")

  ### Bind values to scalars
  # df_scalars |> glimpse(); df_pop |> glimpse()
  df_scalars <- df_scalars |> rbind(df_pop)
  # df_scalars |> glimpse();

  ### Return
  return(df_scalars)
}

###### extend_slrScalars ######
### Scalars for SLR past 2090
extend_slrScalars <- function(
    df0,        ### Dataframe of initial results
    df_se,      ### Socioeconomic scenario
    df_scalars, ### Main scalar values: df_mainScalars
    elasticity  = NULL
){
  ###### Elasticity ######
  ### Update exponent
  if(!(elasticity |> is.null())){
    # df_scalars <- df_scalars |> mutate(exp0 = (exp0 == 1) |> ifelse(exp0, elasticity))
    df_scalars <- df_scalars |> mutate(exp0 = (econScalarName=="vsl_usd") |> ifelse(elasticity, exp0))
  } ### End if(!(elasticity |> is.null()))

  ###### By State ######
  ### By state
  byState    <- TRUE
  stateCols0 <- c("state", "postal")
  popCol0    <- byState |> ifelse("state_pop", "reg_pop")

  ###### Ref Year ######
  ### Filter to years >= refYear (2090)
  df_info    <- "co_slrScalars" |> get_frediDataObj("frediData")
  refYear0   <- (df_info[["refYear"]] |> unique())[1]
  df_scalars <- df_scalars |> filter(year >= refYear0)
  df_se      <- df_se      |> filter(year >= refYear0)
  df0        <- df0        |> filter(year >= refYear0)
  # df_se      <- df_se      |> filter(year >= refYear0)

  ### Drop columns from data
  drop0      <- c("physScalar", "physAdj", "damageAdj", "econScalar", "econMultiplier", "econAdj")
  drop1      <- drop0 |> paste0("Name") |> c(drop0 |> paste0("Value"))
  drop1      <- drop1 |> c("physScalar", "econScalar", "econMultiplier", "physEconScalar")
  drop1      <- drop1 |> c("c1", "exp0")
  df0        <- df0   |> select(-all_of(drop1))
  rm(drop0, drop1)

  ###### Join Data & Scalar Info ######
  ### Join initial results & scalar info
  drop0      <- c("byState")
  join0      <- c("sector")
  df_info    <- df_info |> select(-all_of(drop0))
  df0        <- df0     |> left_join(df_info, by=c(join0))
  rm(drop0, join0)

  ###### Set Default Columns ######
  ### Make some columns none and 1
  mutate0    <- c("damageAdj", "econScalar") |> paste0("Name")
  mutate1    <- c("damageAdj", "econScalar") |> paste0("Value")
  df0[,mutate0] <- "none"
  df0[,mutate1] <- 1
  rm(mutate0, mutate1)

  ###### Get Multiplier Values ######
  ### Gather econMultiplier scalars
  gather0    <- c("gdp_usd", "gdp_percap")
  select0    <- gather0 |> c("year")
  # select0    <- gather0 |> c("byState", "year")
  df_mult0   <- df_se   |> summarize_seScenario(national=T)
  df_mult0   <- df_mult0 |> filter(byState==0)
  df_mult0   <- df_mult0 |> select(all_of(select0))
  df_mult0   <- df_mult0 |> pivot_longer(
    cols      = all_of(gather0),
    names_to  = "econMultiplierName",
    values_to = "econMultiplierValue"
  ) ### End pivot_longer()

  ### Drop values
  rm(gather0, select0)

  ### Join with data
  join0      <- c("econMultiplierName", "year")
  df0        <- df0 |> left_join(df_mult0, by=c(join0))
  # df0        <- df0 |> mutate(econAdjName = econMultiplierName)
  rm(join0)

  ###### Economic Adjustment Values ######
  ### Get adjustment values & join with data
  rename0    <- c("econMultiplierValue", "year")
  rename1    <- c("econAdjValue", "refYear")
  join0      <- c("econMultiplierName", "refYear") #|> c("byState")
  select0    <- join0    |> c("econAdjValue")
  df_adj0    <- df_mult0 |> filter(year == refYear0)
  df_adj0    <- df_adj0  |> rename_at(c(rename0), ~rename1)
  # df0 |> glimpse(); df_adj0 |> glimpse()
  df_adj0    <- df_adj0  |> select(all_of(select0))
  df0        <- df0      |> left_join(df_adj0, by=c(join0))
  ### Drop values
  rm(rename0, rename1, select0, join0, df_mult0, df_adj0)
  # "got here1" |> print()
  ###### Economic Multiplier & Scalar ######
  ### Calculate econ scalar values
  df0        <- df0 |> mutate(econMultiplier = c1 * (econMultiplierValue / econAdjValue)**exp0)
  df0        <- df0 |> mutate(econScalar     = econScalarValue * econMultiplier)

  ###### Physical Scalar Values ######
  ###### Get Multiplier Values ######
  ### Gather econMultiplier scalars
  type0      <- "physScalar"
  rename0    <- c("scalarName", "value")
  rename1    <- c("physScalarName", "physScalarValue")
  join0      <- c("physScalarName", "year") |> c("byState", "region") |> c(stateCols0)
  # select0    <- rename1 |> c("year")
  select0    <- rename1 |> c("year") |> c("byState", "region") |> c(stateCols0)
  vals0      <- df0[["physScalarName"]] |> unique()
  ### Filter, rename, select, join
  df_phys0   <- df_scalars |> filter(scalarType == type0)
  df_phys0   <- df_phys0   |> filter(scalarName %in% vals0)
  df_phys0   <- df_phys0   |> rename_at(c(rename0), ~rename1)
  df_phys0   <- df_phys0   |> select(all_of(select0))
  # df0 |> glimpse(); df_phys0 |> glimpse()
  df0        <- df0        |> left_join(df_phys0, by=c(join0))
  ### Drop values
  rm(type0, rename0, rename1, join0, select0, vals0)

  ###### Physical Adjustment Values ######
  ### Get adjustment values & join with data
  rename0    <- c("physScalarName", "physScalarValue", "year")
  rename1    <- c("physAdjName", "physAdjValue", "refYear")
  join0      <- c("physAdjName", "refYear") |> c("byState", "region") |> c(stateCols0)
  select0    <- rename1  |> c("refYear") |> c("byState", "region") |> c(stateCols0)
  df_adj0    <- df_phys0 |> filter(year == refYear0)
  df_adj0    <- df_adj0  |> rename_at(c(rename0), ~rename1)
  df_adj0    <- df_adj0  |> select(all_of(select0))
  df0        <- df0      |> mutate(physAdjName = physScalarName)
  # df0 |> glimpse(); df_adj0 |> glimpse()
  df0        <- df0      |> left_join(df_adj0, by=c(join0))
  ### Drop values
  rm(rename0, rename1, select0, join0, df_phys0, df_adj0)

  ###### Physical Scalar & Phys Econ Scalar ######
  ### Calculate econ scalar values
  df0        <- df0 |> mutate(physScalar     = c2 * physScalarValue / physAdjValue)
  df0        <- df0 |> mutate(physEconScalar = econScalar * physScalar)

  ### Drop columns & join
  drop0      <- c("c2", "refYear")
  df0        <- df0    |> filter(year > refYear)
  df0        <- df0    |> select(-all_of(drop0))
  rm(drop0)

  ###### Return ######
  ### Return
  return(df0)
}

###### match_scalarValues ######
### Last updated 2023.11.15
### Match Scalar Values
### This function matches interpolated scalar values for each scalar type to the time series scenario information
### Scalar types are: physAdj, physMultiplier, damageAdj, econScalar, econMultiplier
### Function "match_scalarValues" replaces "get_popWts", "get_physMultipliers", and "get_econScalars"
match_scalarValues <- function(
    df0,     ### Initial results dataframe
    scalars, ### Scalars dataframe
    scalarType
){
  ### Check if scalars are state-level
  byState <- "state" %in% colnames(df0)
  if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}

  ###### Filter to Scalar Type ######
  # scalarType |> print()
  scalarType0      <- scalarType; rm(scalarType)
  scalars          <- scalars |> filter(scalarType==scalarType0)
  scalars          <- scalars |> select(-c("scalarType"))
  # "got here1" |> print(); scalars |> glimpse()

  ### Scalar columns to rename
  rename0          <- "scalarName"
  rename1          <- scalarType0 |> paste0(c("Name"))
  scalarColName    <- rename1
  scalars          <- scalars |> rename_at(c(rename0), ~rename1)

  ###### National vs Regional Scalars ######
  # scalars$national_or_regional |> unique() |> print()
  scalars_regional <- scalars |> filter(national_or_regional != "national")
  scalars_national <- scalars |> filter(national_or_regional == "national")
  ### Drop columns
  scalars_regional <- scalars_regional |> select(-c("national_or_regional"))
  scalars_national <- scalars_national |> select(-c("national_or_regional"))
  # scalars_national |> glimpse()

  ### Scalar names
  scalarNames_reg  <- scalars_regional[[scalarColName]] |> unique()
  scalarNames_nat  <- scalars_national[[scalarColName]] |> unique()
  # "got here2" |> print(); scalarNames_nat |> print()

  ###### Create Filters ######
  ### Filter the df0 to those for which the scalar identifier == "none"...value = 1
  filter_none      <- df0[[scalarColName]] == "none"
  filter_reg       <- df0[[scalarColName]] %in% scalarNames_reg
  filter_nat       <- df0[[scalarColName]] %in% scalarNames_nat

  ###### Filter Data ######
  df_none          <- df0[filter_none,] #|> mutate(value = 1)
  df_regional      <- df0[filter_reg,]
  df_national      <- df0[filter_nat,]
  ### Whether filtered data has rows
  has_national     <- df_national |> nrow()
  has_regional     <- df_regional |> nrow()
  # scalars |> glimpse()

  ###### Select Columns ######
  ### Columns
  select0 <- c("sector", "sector_label", "modelType")
  select0 <- select0 |> c("variant", "impactType", "impactYear")
  select0 <- select0 |> c("region") |> c(stateCols0)
  select0 <- select0 |> c("sectorprimary", "includeaggregate")
  select0 <- select0 |> c("byState", "year")
  select0 <- select0 |> c(scalarType0 |> paste0(c("Name"))) #|> c("value")
  ### Select
  df_none       <- df_none     |> select(all_of(select0)) |> mutate(value=1)
  df_regional   <- df_regional |> select(all_of(select0))
  df_national   <- df_national |> select(all_of(select0))

  ###### Mutate Data ######
  # scalars_regional |> glimpse(); df_regional |> glimpse();
  ### Join & drop
  if(has_regional){
    join0            <- c("region") |> c(stateCols0) |> c(scalarColName) |> c("byState", "year")
    df_regional      <- df_regional |> left_join(scalars_regional, by=c(join0))
    rm(join0)
  } ### End if(has_regional)

  ###### National values ######
  # scalars_national |> glimpse(); df_national |> glimpse();
  ### Join & drop
  if(has_national){
    # join0            <- c(scalarColName) |> c("byState", "year")
    # drop0            <- c("region") |> c(stateCols0)
    join0            <- c(scalarColName) |> c("year")
    drop0            <- c("region") |> c(stateCols0) |> c("byState")
    scalars_national <- scalars_national |> select(-all_of(drop0))
    df_national      <- df_national |> left_join(scalars_national, by=c(join0))
    rm(join0, drop0)
  } ### End if(has_national)
  # df_national1 |> glimpse()

  ###### Rename  ######
  # df_none |> glimpse(); df_regional |> glimpse()
  data_x    <- df_none |> rbind(df_regional)
  data_x    <- data_x  |> rbind(df_national)
  rm(df_none, df_regional, df_national)

  ### Add placeholder column
  hasData0  <- data_x |> nrow()
  # data_x |> glimpse()
  if(hasData0){
    ### Replace NA values ?
    # data_x  <- data_x |> mutate(value = value |> replace_na(1))
    data_x  <- data_x |> mutate(value = value)
  } else{
    data_x |> mutate(value=1)
  } ### End if(hasData0)

  ### Rename
  rename0 <- "value"
  rename1 <- scalarType0 |> paste0(c("Value"))
  # "aloha" |> print(); data_x |> names() |> print()
  data_x  <- data_x |> rename_at(c(rename0), ~rename1)
  ### Join
  # exCols0 <- scalarType0 |> paste0(c("Name", "Value")) |> c("value")
  # join0   <- select0 |> (function(x){x[!(x %in% exCols0)]})()
  join0   <- select0
  df0     <- df0   |> left_join(data_x, by=c(join0))

  ###### Return ######
  return(df0)
}


###### get_econAdjValues ######
### Last updated 2023.11.30
### Get Economic Adjustment/Multiplier
### This function matches interpolated scalar values for each scalar type to the time series scenario information
### Scalar types are: physAdj, physMultiplier, damageAdj, econScalar, econMultiplier
### Function "get_econAdjValues" replaces "get_econMultipliers"
get_econAdjValues <- function(
    data,       ### Initial results dataframe
    scenario,   ### Population and GDP scenario
    multipliers ### List of multipliers
){
  ###### Multipliers
  none0       <- "none"
  multipliers <- multipliers |> (function(x){x[!(x %in% none0)]})()
  ###### Scenario information
  # Get column names:
  cNames      <- scenario |> names()
  cNames      <- cNames   |> (function(x){x[(x %in% multipliers)]})()
  ###### By state
  # Check if data is broken down by state:
  byState     <- "state" %in% cNames
  # group0      <- c("gdp_usd", "national_pop", "gdp_percap")
  idCols0     <- c("byState", "year")
  select0     <- cNames |> c(idCols0)
  scalars     <- scenario |> summarize_seScenario(national=T)
  scalars     <- scalars  |> select(all_of(select0))

  ###### Format scalar data
  ###### Get values for a single region since the multipliers are the same for all regions
  ###### Gather scenario information
  scalars     <- scalars  |> pivot_longer(
    -all_of(idCols0),
    names_to  = "econMultiplierName",
    values_to = "econMultiplierValue"
  ) ### End pivot_longer()
  ### Rename year to year0 and convert to character
  scalars     <- scalars |> mutate(year0 = min(year)  |> as.character())
  # data |> glimpse(); scalars |> glimpse()

  ###### Format data and separate
  # data |> glimpse(); scalars |> glimpse()
  data        <- data |> mutate(econAdjName = econMultiplierName)
  df_none     <- data |> filter(econMultiplierName == none0)
  data        <- data |> filter(econMultiplierName != none0)
  ### Which to do
  hasNone0    <- df_none |> nrow()
  hasOther0   <- data    |> nrow()
  ###### ScalarName == "None"
  ### Columns
  mutate0     <- c("econMultiplierValue", "econAdjValue")
  drop0       <- c("econAdjName")
  ### Filter the data to those for which the scalar identifier == "none"...value = 1
  ### Set econMultiplierValue, econAdjValue == 1 if scalarMultiplierName=none
  if(hasNone0 ) {df_none[,mutate0] <- 1}
  if(hasOther0) {
    ###### Multiplier Adjustment
    ### Rename scalars and convert year to character
    # # rename0   <- c("year" , "econMultiplierName", "econMultiplierValue")
    # # rename1   <- c("year0", "econAdjName"       , "econAdjValue")
    # # join0     <- rename0 |> (function(x){x[!(x %in% mutate0)]})() |> c("byState")
    # # join1     <- rename1 |> (function(x){x[!(x %in% mutate0)]})() |> c("byState")
    # rename0   <- c("econMultiplierName", "econMultiplierValue")
    # rename1   <- c("econAdjName"       , "econAdjValue")
    # join0     <- rename0 |> (function(x){x[!(x %in% mutate0)]})() |> c("byState", "year")
    # join1     <- rename1 |> (function(x){x[!(x %in% mutate0)]})() |> c("byState", "year0", "year", "econMultiplierName")
    ### Scalar Adjustments
    ## scalars |> glimpse()
    # Take value at base year
    rename0   <- c("econMultiplierName", "econMultiplierValue")
    rename1   <- c("econAdjName"       , "econAdjValue")
    drop0     <- c("year")
    drop1     <- c("econAdjValue")
    join0     <- c("byState", "year0","econAdjName")
    base_vals <- scalars   |> filter(year == year0)
    base_vals <- base_vals |> select(-all_of(drop0))|> rename_at(c(rename0), ~rename1)
    scalarAdj <- scalars   |> rename_at(c(rename0), ~rename1)
    scalarAdj <- scalarAdj |> select(-all_of(drop1))
    scalarAdj <- scalarAdj |> left_join(base_vals, by=c(join0), relationship = "many-to-many")
    rm(rename0, rename1, drop0, drop1, join0)
    # scalarAdj   <- scalarAdj |> mutate(year0 = year0 |> as.character())
    # data$year |> class() |> print(); scalarAdj$year |> class() |> print()

    ###### Join with scalars
    # scalars   <- scalars |> left_join(scalarAdj, by =  c("byState", "year0","year", "econMultiplierName"="econAdjName")) |> mutate(econAdjName = econMultiplierName)
    join0     <- c("byState", "year0","year", "econAdjName")
    # join1     <- c("econMultiplierName") |> c("year0", "year", "econMultiplierName", "byState")
    join1     <- c("econMultiplierName") |> c("year0", "year", "econAdjName", "byState")
    scalars   <- scalars |> mutate(econAdjName = econMultiplierName)
    # scalars |> glimpse(); scalarAdj |> glimpse(); data |> glimpse()
    scalars   <- scalars |> left_join(scalarAdj, by=c(join0))
    data      <- data    |> left_join(scalars  , by=c(join1))
  } ### End if(hasOther0)

  ###### Rename value column
  data        <- data |> rbind(df_none) #|> select(-all_of(drop0))
  # data |> glimpse()
  ###### Return results values
  return(data)
}

###### initialize_resultsDf ######
### Initialize results data frame
initialize_resultsDf <- function(
    df_se,       ### Dataframe with socioeconomic scenario
    df_info,     ### Dataframe with sectors info
    df_scalars,  ### Dataframe of main scalars
    elasticity = NULL
){
  ###### By State ######
  # if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  stateCols0 <- c("state", "postal")
  byState    <- (stateCols0 %in% (df_se |> names())) |> any()
  popCol     <- byState |> ifelse("state_pop", "reg_pop")

  ###### Adjust Data ######
  ### Adjust scenario info for SLR sectors
  # df_se      <- df_se |> summarize_seScenario(national=T)
  df_se      <- df_se |> summarize_seScenario(national=F)

  ###### Initialize Results ######
  ### Initialized results: Join sector info with socioeconomic scenario
  # df_se |> glimpse(); df_info |> glimpse(); df_scalars |> glimpse()
  join0      <- c("byState")
  df0        <- df_info |> left_join(df_se, by=c(join0), relationship="many-to-many")
  rm(join0)
  # df0 |> glimpse()

  ###### Update Scalar Info ######
  ### Update scalar info
  ### Physical scalars
  df0        <- df0 |> match_scalarValues(df_scalars, scalarType="physScalar")
  ### Physical adjustment
  df0        <- df0 |> match_scalarValues(df_scalars, scalarType="physAdj")
  ### Damage adjustment
  df0        <- df0 |> match_scalarValues(df_scalars, scalarType="damageAdj")
  ### Economic scalar
  df0        <- df0 |> match_scalarValues(df_scalars, scalarType="econScalar")

  ###### Economic Adjustment Values ######
  ### Get economic adjustment values
  df_mult    <- "co_econMultipliers" |> get_frediDataObj("frediData")
  df0        <- df0 |> get_econAdjValues(scenario=df_se, multipliers=df_mult[["econMultiplierName"]])

  ###### Calculate Scalars ######
  ### Calculate scalars
  df0        <- df0 |> calcScalars(elasticity = elasticity)

  ###### SLR Scalars for Years > 2090 ######
  ### Scalars for SLR past 2090
  slrScalars <- "co_slrScalars" |> get_frediDataObj("frediData")
  types0     <- df0[["modelType"]] |> unique() |> tolower()
  refYear0   <- (slrScalars[["refYear"]] |> unique())[1]
  has_slr    <- "slr" %in% types0
  maxYr0     <- df0[["year"]] |> max()
  do_slr     <- has_slr & (maxYr0 > refYear0)
  if(do_slr){
    ### Separate GCM & SLR values
    df_gcm0    <- df0 |> filter(modelType |> tolower() != "slr")
    df_slr0    <- df0 |> filter(modelType |> tolower() == "slr")

    ### Filter to reference year
    df_slr1    <- df_slr0 |> filter(year <= refYear0)
    df_slr2    <- df_slr0 |> filter(year >= refYear0)
    rm(df_slr0)

    ### Get extended scalars
    df_slr2    <- df_slr2 |> extend_slrScalars(
      df_se      = df_se,
      df_scalars = df_scalars,
      elasticity = elasticity
    ) ### End extend_slrScalars
    ### Ensure there are no duplicate years
    df_slr2    <- df_slr2 |> filter(year > refYear0)

    ### Add results back together
    df_slr0    <- df_slr1 |> rbind(df_slr2)
    df0        <- df_gcm0 |> rbind(df_slr0)
    rm(df_slr1, df_slr2, df_slr0, df_gcm0)
  } ### End if(do_npd)

  ###### Return ######
  ### Return
  return(df0)
}


###### calcScalars ######
### Last updated 2021.02.05
### Calculate Scalars
### This function calculates the physical scalar value, the economic scalar value, and their product
### The physical and economic scalars refer to the time series column from which the Annual Sectors tab
###   in the Excel tool draws values.
calcScalars <- function(
    data,   ### Initial results dataframe
    elasticity = NULL ### An elasticity to use to adjust values
){
  ###### Calculate physical scalar ######
  ### Physical scalars are the product of the physical scalar, the physical adjustment, and the damage adjustment
  # data |> glimpse()
  data <- data |> mutate(physScalar = physScalarValue * physAdjValue * damageAdjValue )

  ###### Adjust Elasticity for VSL ######
  ### Adjust Elasticity for VSL only
  if(!(elasticity |> is.null())){
    data <- data |> mutate(exp0 = (econScalarName=="vsl_usd") |> ifelse(elasticity, exp0))
    # if(is.numeric(elasticity)){data   <- data |> mutate(exp0 = elasticity)}
  }

  ###### Economic adjustments ######
  ### Economic multipliers are the economic multiplier value divided by the adjustment
  ### The economic multiplier value is 1, GDP, or GDP per capita
  ### The economic adjustment value is usually the economic multiplier value at a reference year
  data <- data |> mutate(econMultiplier = (econMultiplierValue / econAdjValue)**exp0 )

  ###### Economic scalars ######
  ### The economic scalar is calculated using the following equation.
  ### Constants c0, c1, and exp0 are from the
  data <- data |> mutate(econScalar = c0 + c1 * econScalarValue * (econMultiplier) )

  ###### Economic-physical scalar ######
  ### Combine the physical and economic scalar.
  data <- data |> mutate(physEconScalar  = econScalar * physScalar )

  ###### Return ######
  return(data)
}

###### get_gcmScaledImpacts ######
get_gcmScaledImpacts <- function(
    df0, ### Data frame of initial results
    df1  ### Data frame of drivers
){
  ### By state
  # df0 |> glimpse()
  names0     <- df0 |> names()
  byState    <- "state" %in% names0
  if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  rm(names0)

  ### Get df_imp0 & list_impactFunctions
  sectors0   <- df0[["sector"]] |> unique()
  df_imp0    <- "data_scaledImpacts" |> get_frediDataObj("stateData")
  df_imp0    <- df_imp0 |> filter(sector %in% sectors0)

  ### Drivers
  df1        <- df1 |> filter(modelType == "gcm")
  xVar0      <- df1[["modelUnitValue"]]
  years0     <- df1[["year"          ]]

  ### Get list of unique impact functions
  funList0   <- "list_impactFunctions" |> get_frediDataObj("stateData")
  funNames0  <- funList0 |> names() |> unique()
  df0        <- df0 |> mutate(hasScenario = scenario_id %in% funNames0)

  ### Check whether the scenario has an impact function (scenarios with all missing values have no functions)
  gcmFuns0   <- (df0 |> filter(hasScenario == 1))[["scenario_id"]] |> unique()
  gcmNoFuns0 <- (df0 |> filter(hasScenario != 1))[["scenario_id"]] |> unique()
  # df_gcm     <- tibble(scenario_id = gcmFuns0)
  hasFuns0   <- gcmFuns0   |> length() > 0
  hasNoFuns0 <- gcmNoFuns0 |> length() > 0
  # df_gcm    <- df_gcm |> mutate(hasScenario = 1 * (scenario_id %in% funNames0))

  # testIds0 <- (df_imp0 |> filter(sector=="WindDamage" & hasScenario==1))$scenario_id |> unique()
  # testIds1 <- df0$scenario_id |> unique()
  # (c("WindDamage_NA_NA_NA_Northeast_N/A_N/A_MIROC5", "WindDamage_NA_NA_NA_Northeast_N/A_N/A_CCSM4") %in% testIds0) |> print()
  # (c("WindDamage_NA_NA_NA_Northeast_N/A_N/A_MIROC5", "WindDamage_NA_NA_NA_Northeast_N/A_N/A_CCSM4") %in% gcmFuns0) |> print()
  # gcmFuns0[!(gcmFuns0 %in% testIds0)] |> head() |> print()
  # testIds0[!(testIds0 %in% gcmFuns0)] |> head() |> print()

  ### Initialize results
  df0        <- tibble()
  rename0    <- c("xVar")
  rename1    <- c("modelUnitValue")
  select0    <- c("scenario_id", "year", rename1, "scaled_impacts")

  ### Get impacts for scenario_ids that have functions
  if(hasFuns0){
    ### Subset impact list
    funList0   <- funList0 |> (function(x){x[  names(x) %in% gcmFuns0 ]})()
    ### Get impacts
    df_hasFuns <- funList0 |> interpolate_impacts(xVar = xVar0, years = years0)
    df_hasFuns <- df_hasFuns |> rename_at(c(rename0), ~rename1) #|> filter(year>=minYear)
    df_hasFuns <- df_hasFuns |> select(all_of(select0))
    df0 <- df0 |> rbind(df_hasFuns)
    rm(funList0, df_hasFuns)
  } #; return(df_i)
  if(hasNoFuns0){
    ### Initialize values
    df_noFuns0  <- tibble(scenario_id = gcmNoFuns0)
    ### Join with driver values
    join0       <- "joinCol"
    df_noFuns0  <- df_noFuns0 |> mutate(joinCol=1) |>
      left_join(
        df1 |> mutate(joinCol=1),
        by = c(join0)
      ) |>
      select(-all_of(join0))
    ### Add scaled impact
    df_noFuns0  <- df_noFuns0 |> mutate(scaled_impacts = NA)
    df_noFuns0  <- df_noFuns0 |> select(all_of(select0))
    # df0 |> names() |> print(); df_noFuns0 |> names() |> print()
    df0         <- df0 |> rbind(df_noFuns0)
    rm("df_noFuns0")
  }

  ### Arrange
  arrange0    <- c("scenario_id", "year")
  df0         <- df0 |> arrange_at(c(arrange0))

  ### Return
  return(df0)
}

###### get_slrScaledImpacts ######
get_slrScaledImpacts <- function(
    df0, ### Initial results for SLR sectors
    df1  ### Driver data frames
    ){
  ####### By State
  ### By state
  byState     <- "state" %in% (df0 |> names())
  if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}

  ###### Get rDataList Objects #######
  ### Get objects from rDataLsit
  df_ext0     <- "slrExtremes"   |> get_frediDataObj("stateData")
  df_imp0     <- "slrImpacts"    |> get_frediDataObj("stateData")
  co_modTypes <- "co_modelTypes" |> get_frediDataObj("frediData")
  # co_models   <- "co_models"     |> get_frediDataObj("frediData")

  ###### Values #######
  slr0        <- "slr"
  # co_modTypes |> glimpse()
  co_modTypes <- co_modTypes |> rename(modelType = modelType_id)
  slrMax0     <- (co_modTypes |> filter(modelType==slr0))[["modelMaxOutput"]][1]

  ###### Format Data #######
  ### Filter to appropriate driver values
  df1         <- df1  |> filter(modelType |> tolower() == slr0) #|> rename(model_type=modelType)
  # df1 |> glimpse()

  ### Filter to appropriate years
  maxYear0    <- df1[["year"]] |> max()
  df_ext0     <- df_ext0 |> filter(year <= maxYear0)
  df_imp0     <- df_imp0 |> filter(year <= maxYear0)

  ### Filter to appropriate sectors
  sectors0    <- df0[["sector"]] |> unique()
  df_ext0     <- df_ext0 |> filter(sector %in% sectors0)
  df_imp0     <- df_imp0 |> filter(sector %in% sectors0)

  ###### Get scenario_id #######
  ### Join with df0 to get scenario_id
  join0       <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0) |> c("byState") |> c("year")
  select0     <- join0 |> c("scenario_id")
  drop0       <- join0 |> (function(x){x[!(x %in% c("year"))]})()
  df0         <- df0 |> select(all_of(select0))
  ### Join
  # df0 |> glimpse(); df_ext0 |> glimpse()
  # df0 |> group_by_at(c(join0)) |> summarize(n=n(),groups="keep") |> filter(n>1) |> glimpse()
  # df_ext0 |> group_by_at(c(join0)) |> summarize(n=n(),groups="keep") |> filter(n>1) |> glimpse()
  df_ext0     <- df_ext0 |> left_join(df0, by=c(join0))
  df_imp0     <- df_imp0 |> left_join(df0, by=c(join0))
  # "got here1" |> print(); df_ext0 |> glimpse(); df_imp0 |> glimpse()
  ### Drop
  df_ext0     <- df_ext0 |> select(-all_of(drop0))
  df_imp0     <- df_imp0 |> select(-all_of(drop0))
  rm(df0); rm(join0, select0, drop0)

  ###### Add driver values #######
  ### Join data with drivers
  join0       <- c("year")
  select0     <- c("scenario_id")
  drop0       <- c("model_type")
  df_max0     <- df1     |> left_join(df_ext0, by=c(join0))
  df_slr0     <- df1     |> left_join(df_imp0, by=c(join0))
  ### Drop columns
  df_max0     <- df_max0 |> select(-any_of(drop0))
  df_slr0     <- df_slr0 |> select(-any_of(drop0))
  ### Relocate columns
  df_max0     <- df_max0 |> relocate(all_of(select0))
  df_slr0     <- df_slr0 |> relocate(all_of(select0))
  rm(df_ext0, df_imp0);
  rm(join0, select0, drop0)
  # "got here1" |> print(); df_max0 |> glimpse(); df_slr0 |> glimpse()
  # rm(select0, select1)

  ###### Scaled Impacts >= Max #######
  ### Filter to appropriate years
  df_max0     <- df_max0 |> filter(modelUnitValue >= driverValue_ref)
  maxYrs0     <- df_max0 |> get_uniqueValues(column="year")
  nrow_max    <- maxYrs0 |> length()

  ## Calculate scaled impacts for values > slrMax0
  if(nrow_max){
    df_max0  <- df_max0   |> mutate(deltaDriver    = modelUnitValue    - driverValue_ref)
    df_max0  <- df_max0   |> mutate(scaled_impacts = impacts_intercept + impacts_slope * deltaDriver)
    # df_max0 |> filter(deltaDriver < 0) |> nrow() |> print()
  } else{
    df_max0  <- df_max0 |> mutate(scaled_impacts = NA)
  } ### End if(nrow_max)

  ###### Scaled Impacts < Max #######
  ### Get impacts and create scenario ID for values <= slrMax0
  ### Join with slrImpacts
  df_slr0     <- df_slr0 |> filter(!(year %in% maxYrs0))
  nrow_oth    <- df_slr0 |> nrow()
  # "got here2" |> print()

  ### Group by cols
  # cols0     <- c("modelType",  "modelUnitValue")
  # cols1     <- c("model_type", "driverValue")
  cols0       <- c("modelUnitValue")
  cols1       <- c("driverValue")
  ### Group by cols
  slr_names   <- df_slr0 |> names()
  select0     <- c("scenario_id")
  group0      <- select0 |> (function(x){x[x %in% (df_slr0 |> names())]})()
  group0      <- group0  |> c(cols1)
  # rm("group0", "slr_names")

  ### Calculate impacts for values not above the maximum value
  # nrow_oth |> print()
  if(nrow_oth){
    #### Interpolate driver values
    mutate0   <- c("lower_model", "upper_model")
    slrVals0 <- df1      |> rename_at(c(cols0), ~cols1)
    slrVals0 <- slrVals0 |> interp_slr_byYear()
    slrVals0 <- slrVals0 |> mutate_at(c(mutate0), function(y){gsub(" ", "", y)})

    ### Interpolate
    # df_slr0 |> glimpse()
    # df_slr0  <- df_slr0 |> rename_at(c(cols0), ~cols1)
    df_slr0  <- df_slr0 |> fredi_slrInterp(slr_x=slrVals0, groupByCols=group0)
    # df_slr0   <- df_slr0 |> rename_at(c(cols1), ~cols0)
    rm(group0, slr_names, slrVals0)
    rm(cols0, cols1)
  } else{
    df_slr0  <- df_slr0 |> mutate(scaled_impacts = NA)
  } ### End if(nrow_oth)
  # df_slr0 |> filter(!is.na(scaled_impacts)) |> nrow() |> print()

  ### Get scenario ID and adjust the model value
  # df_max0 |> glimpse(); df_slr0 |> glimpse()
  select1     <- select0 |> c("year", "modelUnitValue", "scaled_impacts") |> unique()
  df_slr0     <- df_slr0 |> select(all_of(select1))
  df_max0     <- df_max0 |> select(all_of(select1))
  df_slr0     <- df_slr0 |> rbind(df_max0)
  rm(df_max0, select1)

  ###### Arrange ######
  ### Add other results back in
  # "got here7" |> print()
  arrange0    <- c("scenario_id", "year")
  select1     <- arrange0 |> c("modelUnitValue", "scaled_impacts")
  df_slr0     <- df_slr0 |> select(all_of(select1))
  df_slr0     <- df_slr0 |> arrange_at(c(arrange0))
  # slrIds       <- df_slr0[["scenario_id"]] |> unique() |> sort()
  rm(select1, arrange0)
  # slrIds |> head() |> print();

  ###### Filter ######
  # df_slr0     <- df_slr0 |> mutate(across("scenario_id",str_replace, '(\\d)[0-9]{1,3}cm', '\\Interpolation'))
  df_slr0     <- df_slr0 |> filter(!(scaled_impacts |> is.na()))
  # df_slr0 |> glimpse()

  ###### Return ######
  return(df_slr0)
}


###### interpolate_impacts ######
### Created 2021.02.05. Last updated 2021.02.11.
### 2021.02.11: Changed name from interpolate_temps to interpolate_impacts and added SLR.
### Calculate impacts (binning)
### This function uses the dplyr group_map capabilities to interpolate scaled impacts by temperature or sea level rise (SLR) relationships
interpolate_impacts <- function(
    functions   = NULL, ### List of functions
    xVar        = NULL, ### Temperatures or SLRs to interpolate,
    years       = NULL  ### Years
){
  ### Names of functions and number of functions
  functionNames    <- functions |> names()
  numFunctions     <- functions |> length()

  ### Iterate over the groups
  scaledImpacts_x   <- 1:numFunctions |> map(function(i){
    ### Group, get group function, then get impacts
    scenario_i       <- functionNames[i]
    fun_i            <- functions[[scenario_i]]
    scaledImpacts_i  <- xVar |>  fun_i()
    df_i             <- tibble(
      year           = years,
      xVar           = xVar,
      scaled_impacts = scaledImpacts_i,
      scenario_id    = scenario_i
    )
    return(df_i)
  }) |> bind_rows() ### End group map
  # scaledImpacts_x |> names() |> print()
  return(scaledImpacts_x)
}

###### get_annual_model_stats  ######
### Created 2021.01.11. Last updated 2021.02.26:
### Updated method for calculating model statistics and dealing with NA values.
### This function returns a table with gcm averages, minimums, and maximums
get_annual_model_stats <- function(
    data      = NULL, ### Dataframe of results
    sectors   = NULL, ### Name of sectors to get statistics for
    yVar      = "annual_impacts", ### Column to get averages for
    groupCols = c("sector", "variant", "impactType", "impactYear", "model_type") ### Column(s) to use for grouping
){
  if (byState) {stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  ### Get unique sectors if none are specified
  sectors0    <- data[["sector"]] |> as.character() |> unique()
  if(sectors |> is.null()){sectors <- sectors0}

  ### By State
  cNames       <- data |> colnames()
  byState      <- "state" %in% cNames

  ### Rename columns
  # data |> glimpse()
  rename0      <- c(yVar)
  rename1      <- c("yvar")
  data         <- data |> rename_at(c(rename0), ~rename1)
  rm(rename0, rename1)

  ###### Which observations are NA ######
  ### Determine which observations are NA
  data         <- data |> mutate(not_na = !(yvar |> is.na()))
  data         <- data |> mutate(not_na = not_na * 1)

  ### Model Type
  modelAves0   <- c("Model Average", "Interpolation") ### Labels for model averages
  modelType0  <- data[["model_type"]] |> unique() |> tolower()
  modelType0  <- modelType0[1]
  modelLbl0   <- (modelType0=="gcm") |> ifelse(modelAves0[1], modelAves0[2])

  ###### Reshape the data ######
  # groupCols0 <- c("sector", "variant", "model_type", "impactType", "impactYear", "region")
  # if(groupCols |> is.null()){groupCols <- groupCols0}
  groupCols0  <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0)
  groupCols0  <- groupCols0 |> c("model", "model_type")
  groupCols0  <- groupCols0 |> c("year", "yvar")
  groupByCols <- groupCols0 |> (function(x){x[x %in% (data |> names())]})()

  ### Reshape the data and prepare a column indicating which rows have is.na() for all models
  data        <- data |> select(all_of(groupByCols))
  data        <- data |> mutate(notNA = !(yvar |> is.na()))

  ###### Summarize by group columns ######
  ### Add group column with year
  group0      <- groupByCols |> (function(x){x[!(x %in% c("model", "yvar"))]})()
  sum0        <- c("notNA"   )
  rename0     <- c("sumNotNA")
  ### Summarize values
  df_summary  <- data |>
    group_by_at(c(group0)) |>
    summarize_at(c(sum0), sum, na.rm=T) |> ungroup()
  ### Rename columns
  df_summary  <- df_summary |> rename_at(c(sum0), ~rename0)
  df_summary  <- df_summary |> mutate(sum_notNA = 1 * (sum_notNA > 0))
  df_summary  <- df_summary |> mutate(sum_notNA = sum_notNA |> na_if(0))

  ###### Add the summary back into the data ###### groupByCols
  data        <- data |> left_join(df_summary, by = c(groupByCols))

  ###### Calculate stats ######
  ### Separate observations that are all NA from those that have at least one non NA value
  ### Initialize dataframes
  data_naOnly <- data |> filter(  sum_notNA |> is.na() ) |> select(-all_of(drop0))
  data_nMiss  <- data |> filter(!(sum_notNA |> is.na())) |> select(-all_of(drop0))
  ### Number of rows
  nrow_naOnly <- data_naOnly |> nrow()
  nrow_nMiss  <- data_nMiss  |> nrow()
  ### Drop columns
  drop0       <- c("sum_notNA")
  data        <- data |> select(-all_of(drop0))
  rm(drop0)

  ### If there are values that are only NA, make values NA
  if(nrow_naOnly){
    mutate0     <- c("min", "mean", "max")
    data_naOnly <- data_naOnly |> mutate_at(c(mutate0), function(y){NA})
  } ### End if(nrow_naOnly)

  ### Otherwise, calculate stats
  if(nrow_nMiss){
    df_sum0         <- tibble::lst(min, mean, max)
    sum0            <- c("yvar")
    data_nMiss      <- data_nMiss |>
      group_by_at(c(groupByCols)) |>
      summarize_at(c(sum0), df_sum0, na.rm=T) |>
      ungroup()
    rm(df_sum0, sum0)
  } ### End if(nrow_nMiss)

  ### Bind values
  df_results <- data_nMiss |> rbind(data_naOnly)
  rm(data_nMiss, data_naOnly)

  ###### Bind results ######
  rename0    <- c("min", "mean", "max")
  rename1    <- "model" |> paste0(rename0 |> str_to_title())
  df_results <- df_results |> mutate(model=modelLbl0)
  df_results <- df_results |> rename_at(c(rename0), ~rename1)
  rm(rename0, rename1)

  ###### Return ######
  return(df_results)
}


###### interp_slr_byYear ######
### utils for aggregate_impacts
interp_slr_byYear <- function(
    data, ### Driver scenario with columns year, slr_cm
    yCol = "driverValue", ### Column to look for the driver value
    silent=TRUE
){
  ###### Defaults ######
  ### Rename y Column
  if(yCol |> is.null()){yCol <- "driverValue"}
  oldColName_y <- yCol |> c()
  newColName_y <- "yValue" |> c()
  newColRef_y  <- newColName_y |> paste0("_ref")
  data         <- data |> rename_at(c(oldColName_y), ~newColName_y)
  ### Messaging
  if(silent |> is.null()){silent <- T}
  msgUser    <- !silent

  ###### Get Data Objects ######
  co_models  <- "co_models" |> get_frediDataObj("frediData")
  slr_df     <- "slr_cm"    |> get_frediDataObj("frediData")

  ###### Assign data ######
  ### SLR scenario info
  # assign("co_models", rDataList[["frediData"]][["data"]][["co_models"]])
  # co_models  <- "co_models" |> get_frediDataObj("frediData")
  co_slrs    <- co_models |> filter(modelType=="slr") |> rename(model=model_label)
  slr_levels <- c("0cm" ) |> c(co_slrs[["model_dot"]])
  slr_labels <- c("0 cm") |> c(co_slrs[["model"]])
  slr_orders <- slr_levels |> factor(levels=slr_levels) |> as.numeric()
  slr_min    <- (slr_orders |> min(na.rm=T)) #+ 1
  slr_max    <-  slr_orders |> max(na.rm=T)

  ### Sea level rise information
  # assign("slr_df", rDataList[["slr_cm"]])
  df_slr_years <- slr_df[["year"]] |> unique()
  ### Refactor model
  slr_df       <- slr_df |> mutate(model        = model |> as.character())
  slr_df       <- slr_df |> mutate(model_factor = model |> factor(slr_levels, slr_labels))
  slr_df       <- slr_df |> mutate(model_level  = model_factor |> as.numeric())
  slr_df       <- slr_df |> arrange_at(c("model_level", "year"))
  slr_df       <- slr_df |> mutate(model = model_factor |> as.character())
  slr_df       <- slr_df |> select(-c("model_factor"))

  ### Character vector of model names
  c_slrs0      <- slr_labels

  ### Check that years are unique
  data_years   <- data[["year"]] |> unique()
  n_data_years <- data_years |> length()
  nrows_data   <- data |> nrow()
  flag0        <- nrows_data > n_data_years
  ### Message user
  if(flag0){
    "\t" |> paste0("values for 'yCol' are not unique...")
    "\t" |> paste0("Averaging over 'yCol' values...")
    data <- data |> group_by_at(c("year")) |> summarize_at(c("yValue"), mean, na.rm=T) |> ungroup()
  } ### End if(flag0)
  rm(n_data_years, nrows_data, flag0)

  ###### Prepare data ######
  ### Filter to appropriate years
  data    <- data |> filter(year %in% df_slr_years)
  n_years <- data |> nrow()
  ### Figure if years are missing
  dataYrs <- data[["year"]]

  ###### Standard Columns ######
  ### JoinCols
  join0   <- c("year", "model_level")
  select0 <- c("year") |> c(newColName_y, newColRef_y) |> c("model")
  select1 <- c("year") |> c(newColName_y) |> c("lower_model", "upper_model", "lower_slr", "upper_slr")
  ### Format data
  slr_df  <- slr_df |> rename(yValue_ref = driverValue)
  ### Join
  df_new  <- data |> left_join(slr_df, by = "year")
  ### Filter to specific observations
  df_lo   <- df_new |> filter(yValue_ref <= yValue)
  df_hi   <- df_new |> filter(yValue_ref >= yValue)
  ### Get unique years
  yrs_lo  <- df_lo[["year"]] |> unique() |> sort()
  yrs_hi  <- df_hi[["year"]] |> unique() |> sort()
  ### Figure out which years are missing
  nas_lo  <- dataYrs[!(dataYrs %in% yrs_lo)]
  nas_hi  <- dataYrs[!(dataYrs %in% yrs_hi)]
  ### Create tibbles of missing years
  dfNaLo  <- tibble(year = yrs_lo, model_level = slr_min) |> left_join(df_new, by = c(join0))
  dfNaHi  <- tibble(year = yrs_lo, model_level = slr_max) |> left_join(df_new, by = c(join0))
  ### Bind missing years back in
  df_lo   <- df_lo |> rbind(dfNaLo) |> arrange_at(c(join0[1]))
  df_hi   <- df_hi |> rbind(dfNaHi) |> arrange_at(c(join0[1]))
  ### Drop values
  rm(yrs_lo, yrs_hi, nas_lo, nas_hi, dfNaLo, dfNaHi); rm(df_new)

  ### Summarize values
  ### Columns
  group0  <- c("year")
  sum0    <- c("model_level")
  join0   <- c("year")
  join1   <- join0 |> c(sum0)
  rename0 <- c("yValue_ref", "model")
  rename1 <- c("lower_slr", "lower_model")
  rename2 <- c("upper_slr", "upper_model")

  ### Get lower values
  ### - Summarize
  df_lo <- df_lo |> group_by_at(c(group0)) |>
    summarize_at(c(sum0), max, na.rm=T) |> ungroup()
  ### - Join
  df_lo <- data  |> left_join(df_lo, by=c(join0))
  df_lo <- df_lo |> left_join(slr_df , by=c(join1))
  ### - Select & rename
  df_lo <- df_lo |> select(all_of(select0))
  df_lo <- df_lo |> rename_at(c(rename0), ~rename1)

  ### Get upper values
  ### - Summarize
  df_hi   <- df_hi |>
    group_by_at (c(group0)) |>
    summarize_at(c(sum0), min, na.rm=T) |> ungroup()
  ### - Join
  df_hi <- data  |> left_join(df_hi, by=c(join0))
  df_hi <- df_hi |> left_join(slr_df , by=c(join1))
  ### - Select & rename
  df_hi <- df_hi |> select(all_of(select0))
  df_hi <- df_hi |> rename_at(c(rename0), ~rename2)
  ### Remove values
  rm(group0, sum0, join1, rename0, rename1, rename2)
  rm(slr_df)

  ### Join all
  join0 <- c("year") |> c(newColName_y)
  data  <- df_lo |> left_join(df_hi, by=c(join0))
  data  <- data  |> select(all_of(select1))
  rm(df_lo, df_hi)

  ### Add adjustment
  data  <- data |> mutate(denom_slr  = upper_slr - lower_slr  )
  data  <- data |> mutate(numer_slr  = upper_slr - yValue)
  data  <- data |> mutate(adj_slr    = numer_slr / denom_slr  )
  data  <- data |> mutate(is_inf     = adj_slr |> abs() |> is.infinite())
  data  <- data |> mutate(adj_slr    = adj_slr * (!is_inf))
  data  <- data |> mutate(adj_slr    = adj_slr |> replace_na(0))
  data  <- data |> select(-c("is_inf"))

  ### Rename yValue and return
  data <- data |> rename_at(c(newColName_y), ~oldColName_y)
  return(data)

}

###### fun_slrModel2Height ######
### Helper function to convert SLR model to height in cm
fun_slrModel2Height <- function(
    col_x, ### column "model_dot"
    include   = c("factor", "values"),
    valType   = c("numeric", "character", "factor"),
    labelType = c("numeric", "character") ### Used for factor or label

){
  ### Checks
  do_factor <- "factor" %in% include
  do_values <- "values" %in% include
  do_both   <- do_factor & do_values
  ### Value types and priority
  valTypes <- c("numeric", "character", "factor")
  valType0 <- valType
  valType0 <- valTypes |> (function(y, types_y=valTypes){
    ls1 <- ls0 <- types_y
    c0  <- ls0[1] %in% y
    c1  <- ls0[2] %in% y
    c3  <- ls0[2] %in% y
    if(c0) {ls1 <- ls0[1]}
    else if(c1) {ls1 <- ls0[2]}
    else        {ls1 <- ls0[3]}
    return(ls1)
  })()
  do_numb  <- "numeric"   %in% valType
  do_char  <- "character" %in% valType
  do_fact  <- "factor"    %in% valType
  # valType |> print(); labelType |> print()
  ### Label types and priority
  labTypes <- c("numeric", "character")
  label_x0 <- labelType |>
    (function(y, types_y=labTypes){
      ls1 <- ls0 <- types_y
      c0  <- do_numb | do_char
      c1  <- ls0[1] %in% y
      if(c0) {ls1 <- ls0[1]}
      else if(c1) {ls1 <- ls0[1]}
      else        {ls1 <- ls0[2]}
      return(ls1)
    })()
  # label_x0 |> print()
  labChar       <- "character" %in% label_x0
  # label_x0 |> print(); labChar |> print()
  ### Original labels
  lvl_x0        <- col_x |> unique()
  df_x0         <- tibble(levels=lvl_x0)
  ### Standardize
  df_x0$labels  <- gsub("_" , "", df_x0$levels)
  df_x0$numbers <- gsub("cm", "", df_x0$labels)
  df_x0$values  <- df_x0$numbers |> as.character() |> as.numeric()
  ### Sprt
  df_x0         <- df_x0 |> arrange_at(.vars=c("values"))
  ### Create factor list
  list_x        <- list(factors=df_x0)
  ### Adjust values
  vals_x        <- NULL
  if(do_values){
    if(labChar){labels_x <- df_x0$labels}
    else       {labels_x <- df_x0$values}
    vals_x <- col_x  |> factor(levels=df_x0$levels, labels=labels_x)
    if(do_char){vals_x <- vals_x |> as.character()}
    if(do_numb){vals_x <- vals_x |> as.numeric()}
    list_x[["values"]] <- vals_x
  }
  ### Return list
  if     (do_both  ) {return_x <- list_x}
  else if(do_factor) {return_x <- list_x$factors}
  else               {return_x <- list_x$values}
  ### Return
  return(return_x)
}

####### fun_getNeighbors ######
### Figure out which SLR heights are immediately above and below a driver value
fun_getNeighbors <- function(
    x, ### X values
    values, ### values to compare
    col = "driverValue" # which column to compare
){

  # ### Mutate data
  ### Add a dummy column with a standardized name
  values$newCol <- values[[col]]

  ### Look for equal values
  values_equal <- values |> filter(newCol==x)
  num_equal    <- values_equal |> nrow()

  ### If there are equal values, get the upper and lower value
  ### If there are no equal values, figure if there are any values below the value
  if(num_equal>0){
    ### If there is only one value that is equal, return that value twice
    ### If there is more than one value that is equal, return the lower most and uppermost equal values
    if(num_equal==1){
      lo_order <- values_equal$order |> unique()
      hi_order <- lo_order
    } else{
      c_orders <- values_equal[["order"]]
      lo_order <- values_equal[["order"]] |> min(na.rm=T)
      hi_order <- values_equal[["order"]] |> max(na.rm=T)
    } ### End if(num_equal==1)
  } else{
    values_below <- values |> filter(newCol < x)
    num_below    <- values_below |> nrow()

    ### Get the values above it
    values_above <- values |> filter(newCol > x)
    num_above    <- values_above |> nrow()

    ### If there are values below, get the values above it
    if(num_below==0){
      ### Return the zero value for the low value and the first value above for the hi value
      lo_order     <- 1
      hi_order     <- values_above$order |> min(na.rm=T)
    } else{
      ### Figure out if there are any values above it
      ### - Return the max value for the low value and the hi value
      ### - Otherwise, return the max value for the low value and the hi value
      if(num_above==0){
        lo_order     <- values_below[["order"]] |> max(na.rm=T)
        hi_order     <- lo_order
      } else{
        lo_order     <- values_below[["order"]] |> max(na.rm=T)
        hi_order     <- values_above[["order"]] |> min(na.rm=T)
      } ### End if(num_above==0)
    } ### End if(num_below==0)
  } ### End if(num_equal>0)
  # lo_order |> print()
  lo_values    <- values |> filter(order==lo_order) |> mutate(type="lower")
  hi_values    <- values |> filter(order==hi_order) |> mutate(type="upper")
  new_values   <- lo_values |> rbind(hi_values)
  new_values   <- lo_values |> rbind(hi_values)
  ### Return
  return(new_values)
}

###### fun_getScale ######
### This function creates a set of breaks for a particular column
### It returns a list of breaks, the power of 10, and the limits
fun_getScale <- function(
    data,
    scaleCol = "driverValue",
    # zero = F,
    nTicks = 5
  ){
    ### Defaults
    if(scaleCol |> is.null()){scaleCol <- "driverValue"}
    ### Default is not to zero out in case there are negative numbers
    # if(is.null(zero))     zero <- F
    if(nTicks   |> is.null()){nTicks <- 5}

    ### Min/max values
    xMin <- data[[scaleCol]] |> min(na.rm=T)
    xMax <- data[[scaleCol]] |> max(na.rm=T)

    ### Set minimum to zero unless the minimum is less than zero
    if(xMin > 0){xMin <- 0}
    if(xMax < 0){xMax <- 0}

    ### Min/max values
    ### Limit names, values, bounds, etc
    df_minMax <- tibble(
      name  = c("min", "max"),
      value =  c(xMin, xMax),
      bound =  c(floor(xMin), ceiling(xMax)),
      boundType = c("floor", "ceiling")
    ) ### End tibble

    ### Absolute value, Power of 10 and y-scale info
    df_minMax <- df_minMax |> mutate(bound_abs = bound |> abs())
    ### Calculate log 10 and replace values of infinity with 0
    df_minMax <- df_minMax |> mutate(log10 = (bound_abs |> log10()))
    df_minMax <- df_minMax |> mutate(log10 = log10 |> abs() |> na_if(Inf))
    df_minMax <- df_minMax |> mutate(log10 = log10 |> replace_na(0))
    ### Then get the floor of the log10 value
    df_minMax <- df_minMax |> mutate(power10 = log10 |> floor())

    ### Get maximum power of 10, then scale to zero for negative numbers
    ### Integer division of power of 10 by 3 to get number of thousands
    ### Then get the modulus of the thousands
    x_power10Max <- df_minMax[["power10"]] |> max(na.rm=T)
    x_power1000  <- x_power10Max  %/% 3
    x_mod1000    <- x_power10Max  %% 3

    ### Rounded bounded values (round to 1 essentially)
    divideByPower         <- x_power10Max - 1
    minMax_scaled         <- df_minMax[["value"]] / 10**divideByPower
    bounds_scaled_rounded <- minMax_scaled[1] |> floor() |> c(minMax_scaled[2] |> ceiling())
    bounds_rounded        <- bounds_scaled_rounded * 10**divideByPower

    ###### Establish the range of x
    x_range      <- bounds_rounded
    x_range_p10  <- x_range / 10**x_power10Max
    x_range_dif  <- x_range_p10[2] - x_range_p10[1]

    ### Determine unit of breaks in power of 10
    x_unit_p10     <- 0.5
    x_breaks_p10   <- seq(x_range_p10[1], x_range_p10[2], by=x_unit_p10)
    n_Ticks        <- x_breaks_p10 |> length()
    if(n_Ticks>nTicks){
      x_unit_p10   <- 1
      x_breaks_p10 <- seq(x_range_p10[1], x_range_p10[2], by=x_unit_p10)
      n_Ticks      <- x_breaks_p10 |> length()
      if(n_Ticks>nTicks){
        x_unit_p10   <- 2
        x_breaks_p10 <- seq(x_range_p10[1], x_range_p10[2], by=x_unit_p10)
        n_Ticks      <- x_breaks_p10 |> length()
      } ### End if(n_Ticks>nTicks)
    } ### End if(n_Ticks>nTicks)
    x_breaks       <- x_breaks_p10 * 10**x_power10Max
    # return(x_breaks)

    ### Create list to return
    return_list <- list()
    return_list[["breaks"   ]] <- x_breaks
    return_list[["limits"   ]] <- df_minMax[["value"]]
    return_list[["bounds"   ]] <- bounds_rounded
    return_list[["power10"  ]] <- x_power10Max
    return_list[["power1000"]] <- x_power1000
    return_list[["mod1000"  ]] <- x_mod1000

    ### Return
    return(return_list)
  }

####### fredi_slrInterp ######
fredi_slrInterp <- function(
    data_x,
    slr_x, ### slrScenario
    groupByCols
){
  names_slr      <- data_x |> names(); #names_slr |> print()
  # byState        <- "state" %in% names_slr
  # if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  ### Summary columns
  slrSumCols     <- c("scaled_impacts")
  n_slrSumCols   <- slrSumCols |> length()
  bounds0        <- c("lower", "upper")
  mutate0        <- c("model", "slr")
  slrMutCols     <- c("lower_model", "upper_model")

  ### Info names
  ### "year", "driverValue", "lower_model" , "upper_model", "lower_slr" ,  "upper_slr"
  data_xAdj      <- slr_x; rm("slr_x")
  names_slrAdj   <- data_xAdj |> names()
  #names_slrAdj |> print(); other_slrCols |> print(); join_slrCols |> print()
  other_slrCols  <- names_slrAdj |> (function(x){x[!(x %in% c("year"))]})()
  join_slrCols   <- c(groupByCols, "year") ### sectorprimary, includeaggregate
  ### Other columns
  dropCols0      <- c("model", "model_dot")
  otherCols0     <- c("modelType", "denom_slr", "numer_slr", "adj_slr")
  joinCols1      <- join_slrCols |> (function(x){x[!(x %in% dropCols0)]})()
  joinCols1      <- joinCols1 |> c(paste0("_", "model"))
  joinCols1      <- joinCols1 |> c(paste0("_", "slr"  ))
  joinCols1      <- joinCols1 |> c(otherCols0)

  ### Format values
  data_xAdj      <- data_xAdj |> mutate_at(c(slrMutCols), as.character)

  ### Join with slrInfo and convert columns to character
  # join_cols0 |> print(); data_x |> glimpse(); data_xAdj |> glimpse()
  # join0          <- c("driverValue", "year")
  # data_xAdj |> glimpse(); data_x |> glimpse()
  join0          <- c("year")
  data_xAdj      <- data_xAdj |> mutate(equal_models = lower_model == upper_model)
  data_x         <- data_x    |> left_join(data_xAdj, by=join0)
  rm(join0)
  # data_x |> filter(!is.na(scaled_impacts)) |> nrow() |> print()
  rm(data_xAdj)

  ### Filter to conditions
  data_xEqual    <- data_x |> filter( equal_models) |> select(-c("equal_models"));
  data_xOther    <- data_x |> filter(!equal_models) |> select(-c("equal_models"));
  rm(data_x)
  ### Which types to calculate
  hasEqual       <- data_xEqual |> nrow()
  hasOther       <- data_xOther |> nrow()
  ### Process observations that are equal
  if(hasEqual){
    ### Filter observations that are zeros only and make the summary column values zero
    data_xEqual0 <- data_xEqual |> filter(lower_model=="0cm") |> filter(model_dot=="30cm")
    data_xEqual1 <- data_xEqual |> filter(lower_model!="0cm") |> filter(model_dot==lower_model)
    # c(nrow(data_xEqual0), nrow(data_xEqual1)) |> print()
    rm(data_xEqual)
    ### For observations that are zeros only and make the summary column values zero
    mutate0      <- slrSumCols
    data_xEqual0 <- data_xEqual0 |> mutate_at(c(mutate0), function(y){0})
    rm(mutate0)
    ### Bind values back together
    data_xEqual  <- data_xEqual0 |> rbind(data_xEqual1)
    rm(data_xEqual0, data_xEqual1)
    ### Mutate model_dot
    data_xEqual  <- data_xEqual |> mutate(model_dot="Interpolation")
    ### Select appropriate columns
    # data_xEqual |> glimpse()
    select0      <- names_slr
    drop0        <- dropCols0
    data_xEqual  <- data_xEqual |> select(all_of(select0))
    data_xEqual  <- data_xEqual |> select(-all_of(drop0 ))
    rm(select0, drop0)
  } ### End if length(which_equal) > 0

  ### Observations that are greater than zero
  if(hasOther){
    ### Lower and upper column names and new names
    # slrSumCols |> print()
    lowerSumCols   <- slrSumCols |> paste0("_", "lower")
    upperSumCols   <- slrSumCols |> paste0("_", "upper")
    ### Filter lower model_dot observations to those with a lower model_dot value == "0 cm" and others and drop model_dot column
    data_xLower0   <- data_xOther  |> filter(lower_model=="0cm") #|> mutate(lower_model = "30cm")
    data_xLower1   <- data_xOther  |> filter(lower_model!="0cm")
    # rm("data_xLower0", "data_xLower1")
    data_xUpper    <- data_xOther |> filter(model_dot==upper_model)
    # data_xOther |> glimpse(); #data_xLower0 |> glimpse(); data_xUpper |> glimpse()
    rm(data_xOther)
    ### Rename columns
    # # data_xLower0   <- data_xLower0 |> rename_with(slrSumCols, ~lowerSumCols[slrSumCols==.x])
    # # data_xLower1   <- data_xLower1 |> rename_with(slrSumCols, ~lowerSumCols[slrSumCols==.x])
    # # data_xUpper    <- data_xUpper  |> rename_with(slrSumCols, ~upperSumCols[slrSumCols==.x])
    data_xLower0   <- data_xLower0 |> rename_at(c(slrSumCols), ~lowerSumCols)
    data_xLower1   <- data_xLower1 |> rename_at(c(slrSumCols), ~lowerSumCols)
    data_xUpper    <- data_xUpper  |> rename_at(c(slrSumCols), ~upperSumCols)
    # data_xLower0 |> glimpse(); data_xUpper |> glimpse()
    # rm(rename0, rename1, rename2)
    # rm("lowerSumCols", "upperSumCols")

    ### Convert values for observations with a lower model_dot value =="0 cm" to zero then filter to lower models
    data_xLower0   <- data_xLower0 |> mutate_at(c(lowerSumCols), function(y){0})
    data_xLower0   <- data_xLower0 |> filter(model_dot=="30 cm")
    data_xLower1   <- data_xLower1 |> filter(model_dot==lower_model)
    data_xLower    <- data_xLower0 |> rbind(data_xLower1)
    rm(data_xLower0, data_xLower1)

    ### Drop columns
    data_xLower    <- data_xLower |> select(-all_of(dropCols0))
    data_xUpper    <- data_xUpper |> select(-all_of(dropCols0))

    ### Join upper and lower data frames
    join0          <- data_xLower |> names() |> (function(x){x[!(x %in% c(lowerSumCols))]})()
    # data_xLower |> glimpse(); data_xUpper |> glimpse(); join0 |> print()
    data_xOther    <- data_xLower |> left_join(data_xUpper, by = c(join0))
    rm("data_xLower", "data_xUpper")

    ### Calculate the new value
    # data_xOther |> names() |> print()
    slrLowerVals  <- data_xOther[, lowerSumCols]
    slrUpperVals  <- data_xOther[, upperSumCols]
    slrOtherAdj   <- data_xOther[["adj_slr"]]
    slrNewFactors <- (slrUpperVals - slrLowerVals) * (1 - slrOtherAdj)
    slrNewValues  <-  slrLowerVals + slrNewFactors
    data_xOther[,slrSumCols] <- slrNewValues
    rm("slrLowerVals", "slrUpperVals", "slrOtherAdj", "slrNewFactors", "slrNewValues")
    rm("lowerSumCols", "upperSumCols")

    ### When finished, drop columns and mutate model_dot column
    # names_slr |> print(); data_xOther |> glimpse()
    data_xOther <- data_xOther |> select( any_of(names_slr))
    data_xOther <- data_xOther |> select(-any_of(dropCols0))
    # data_xOther |> glimpse()
  } ### End if (nrow(data_xOther) > 0)

  ### Bind SLR averages together
  # data_xEqual |> glimpse(); data_xOther |> glimpse()
  data_x <- data_xEqual |> rbind(data_xOther)
  rm(data_xEqual, data_xOther)

  return(data_x)
}


