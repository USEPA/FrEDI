###### Helpers ######
### Get column as vector
# get_vector <- function(
    #     data, column = NULL
# ){
#   ### Select column and get values as vector
#   col0  <- column |> is.null() |> ifelse(c(), column)
#   vals0 <- data[[column]] |> as.vector()
#   ### Return
#   return(vals0)
# }

# ### Get unique values from a dataframe column
# get_uniqueValues <- function(
    #     data, column = NULL, sort=TRUE
# ){
#   ### Select column and get values as vector
#   vals0 <- data  |> get_vector(column)
#   vals0 <- vals0 |> unique()
#   # vals0 |> print()
#   ### Sort
#   if(sort){vals0 <- vals0 |> sort()}
#   ### Return
#   return(vals0)
# }

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

### Determine whether something is default or custom
get_returnListStatus <- function(cond0=TRUE){
  cond0 |> ifelse("Default", "Custom")
}

### Function to make it easier to subset a vector
#' @export
get_matches <- function(
    x, ### Vector to subset
    y, ### Vector to check x against
    matches = TRUE,    ### Whether to subset to matches (TRUE) or non-matches (FALSE)
    type    = "values" ### c("values", "matches", "which")
){
  ### Check which type to return
  type      <- type |> tolower()
  doValues  <- "values"  %in% type
  doMatches <- "matches" %in% type
  doWhich   <- "which"   %in% type

  ### Get matches
  set0      <- x %in% y

  ### Get anti-match if matches = FALSE
  if(!matches) set0 <- !set0

  ### Get which values to return
  which0    <- set0 |> which()

  ### Get subset
  x0        <- x[set0]

  ### If doValues, return values
  ### If doMatches, return matches
  ### If doWhich, return which
  if     (doValues) return(x0)
  else if(doWhich ) return(which0)
  else              return(set0)
  ### End function
}


### This function makes it easier to get data objects from the sysdata.rda file
# get_frediDataObj <- function(
#     x        = NULL,        ### Object name
#     listSub  = "frediData", ### Sublist name
#     listall  = FALSE,
#     listName = "rDataList",
#     pkg      = "FrEDI",
#     lib.loc  = .libPaths()[1], ### Library path to look for packages
#     msg0     = "\t"
# ){
#   ### Messaging
#   msg1    <- msg0 |> paste0("\t")
#   ### Check if list name exists
#   exists0 <- listName |> exists()
#   # exists0 |> print()
#   ### If the listname exists in the name space, parse it
#   ### Otherwise, grab it from a package name space
#   if(exists0) {
#     new_x <- parse(text=listName) |> eval()
#     # new_x |> names() |> print()
#   } else      {
#     ### Check if package & list name
#     pkgList0    <- lib.loc |> installed.packages()
#     pkgExists0  <- pkg %in% pkgList0
#     if(!pkgExists0) {
#       msg0 |> paste0("Package doesn't exist...") |> message()
#       msg0 |> paste0("Exiting...") |> message()
#       return()
#     } else          {
#       new_x <- getFromNamespace(listName, ns=pkg)
#     } ### End ### End if(!pkgExists0)
#   } ### End else(exists0)
#
#   ### Whether to list all items in data object or not
#   # if(listall) {return_x <- new_x |> names()}
#   # else        {return_x <- new_x[[x]]}
#   if(listall) {
#     return_x <- new_x |> names()
#   } else      {
#     # return_x <- new_x[[listSub]][["data"]][[x]]
#     # new_x |> names() |> print()
#     # new_x[[listSub]] |> names() |> print()
#     return_x <- new_x[[listSub]][[x]]
#   } ### End if(listall)
#   ### Return
#   return(return_x)
# } ### End get_frediDataObj


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


###### get_scenario_id ######
### Function to standardize the creation of the scenario_id
get_scenario_id <- function(
    data_x,
    include=c("region", "model") ### Character vector of column names to include
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
    region    = "NationalTotal", ### Region if "addRegion"
    byState   = FALSE, ### If breakdown by state
    byModel   = FALSE  ### If breakdown by model
){
  ###### Data Info ######
  ##### Other values
  region0   <- region
  rm(region)
  ##### By state
  if (byState) {stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  if (byModel) {modelCols0 <- c("model")} else{modelCols0 <- c()}
  ##### Columns
  dataCols  <- data |> names()
  defCols   <- c(stateCols0) |> c(modelCols0) |> c("year")
  defCols   <- c("region") |> c(stateCols0) |> c(modelCols0) |> c("year")
  # # defCol0   <- dataCols[!(dataCols %in% defCols)][1]
  defCol0   <- dataCols |> get_matches(y=defCols, matches=FALSE) |> first()
  column0   <- column   |> is.null() |> ifelse(defCol0, column)
  # othCols   <- dataCols[!(dataCols %in% c(defCols, column0))]
  othCols   <- dataCols |> get_matches(y=defCols |> c(column0), matches=FALSE)
  # defCol0 |> print(); column0 |> print()
  # data |> glimpse(); dataCols |> print()
  rm(defCol0)

  ###### Format data
  # column0 |> print()
  data      <- data |> filter(!(column0 |> is.na()))
  years0    <- data |> pull(year) |> unique()

  ### Interpolation years
  doYears   <- years |> is.null()
  if(doYears){
    years  <- years0 |> range(na.rm=TRUE)
    years  <- years[1]:years[2]
  } ### End if(doYears)
  rm(doYears)

  ##### Regions
  addRegion <- !("region" %in% dataCols)
  # addRegion |> print()
  if (addRegion) {data <- data |> mutate(region = region0)}
  rm(addRegion)

  ###### Interpolation Rules ######
  ### - Return NA values outside of extremes
  ### - If only one value is provided use the same for left and right
  ### - Interpolation method
  nullRule  <- rule |> is.null()
  repRule   <- rule |> length() < 2
  defRule   <- c(1)
  if(nullRule){rule <- defRule}
  if(repRule ){rule <- rule |> rep(2)}
  method    <- method |> is.null() |> ifelse("linear", method)

  ### Column names
  cols0     <- c("x", "y")
  cols1     <- c("year") |> c(column0)

  ### Get group IDs and add to data
  groupCol0 <- defCols |> get_matches(y=c("year"), matches=F)
  group0    <- data |> select(any_of(groupCol0))
  group0    <- group0 |> apply(1, function(x){x |> as.vector() |> paste(collapse ="_")}) |> unlist()
  data      <- data   |> mutate(group_id = group0)
  # groupCol0 |> print(); data |> glimpse()
  # data |> group_by_at(c(all_of(groupCol0), "year")) |> summarize(n=n(), .groups="drop") |> filter(n>1) |> glimpse()

  ###### Iteration ######
  ### Iterate over groups
  groups0   <- data |> pull(group_id) |> unique()
  df_interp <- groups0 |> map(function(group_i){
    ### Values
    df_i     <- data |> filter(group_id==group_i)
    x_i      <- df_i |> pull(year)
    y_i      <- df_i |> pull(all_of(column0))
    ### Approximate
    new_i   <- approx(x=x_i, y=y_i, xout=years, rule=rule, method=method)
    new_i   <- new_i |> as_tibble()
    new_i   <- new_i |> rename_at(c(cols0), ~cols1)
    new_i   <- new_i |> mutate(group_id=group_i)
    # new_i |> names() |> print()
    ### Return
    return(new_i)
  }) |> bind_rows()

  ### Determine join and join data
  # data |> glimpse(); df_interp |> glimpse(); cols1 |> print()
  names0 <- data |> names() |> get_matches(y=cols1, matches=F)
  join0  <- names0 |> get_matches(y=df_interp |> names())
  doJoin <- (names0 |> length() > 0) & (join0 |> length() > 0)
  # doJoin |> print(); names0 |> print(); join0 |> print()

  ### Do join
  if(doJoin){
    # data |> glimpse(); df_interp |> glimpse()
    data <- data |> select(-all_of(cols1)) |> distinct()
    data <- data |> left_join(df_interp, by=join0)
  } else{
    data <- df_interp
  } ### End else
  # data |> glimpse()

  ### Arrange data
  drop0    <- c("group_id")
  arrange0 <- join0 |> c("year") |> unique() |> get_matches(y=drop0, matches=F)
  data     <- data |> select(-any_of(drop0))
  data     <- data |> arrange_at(c(arrange0))

  ### Return
  gc()
  return(data)
} ### End function



### Function to zero out values for temperature and SLR scenarios
zero_out_scenario     <- function(
    df0,    ### Tibble with scenario and years, value column
    conn,   ### DB Connection Object
    type0   = "temp", ### Or SLR
    valCol0 = NULL  , ### If NULL, function will use default
    yrRef0  = NULL  , ### If NULL, function will use default
    refVal0 = 0       ### Default to zero
){
  ###### Load Data from FrEDI ######
  ### Get objects from FrEDI name space
  ### Get input scenario info: co_info
  ### Get state info: co_states
  #co_info <- "co_inputInfo"  |> get_frediDataObj("frediData")
  co_info <-  DBI::dbReadTable(conn,"co_inputInfo")
  #co_mods <- "co_modelTypes" |> get_frediDataObj("frediData")
  co_mods <-  DBI::dbReadTable(conn,"co_modelTypes")

  ### Subset input info
  df_info <- co_info |> filter(inputName %in% type0)
  df_mods <- co_mods |> filter(inputName %in% type0)
  valCol0 <- (valCol0 |> is.null()) |> ifelse(df_info |> pull(valueCol), valCol0)
  yrRef0  <- (yrRef0  |> is.null()) |> ifelse(df_mods |> pull(modelRefYear), yrRef0)

  ### Zero out rows at ref year
  # df0 |> glimpse(); df0 |> pull(year) |> range() |> print(); yrRef0 |> print()
  # df1 <- tibble(year=yrRef0) |> mutate(y = 0) |> rename_at(c("y"), ~valCol0)
  drop0   <- c("year") |> c(valCol0)
  df1     <- tibble(x = yrRef0, y = refVal0)
  # df1 |> glimpse()
  df1     <- df1 |> rename_at(c("x", "y"), ~drop0)

  ### Df2
  join0   <- c("year")
  df2     <- df0 |> select(-any_of(drop0)) |> distinct() |> mutate(year = yrRef0)
  df1     <- df1 |> left_join(df2, by=join0)
  rm(drop0, join0)

  ### Then, drop rows in yrRef0 and bind zero rows to values
  # df0 |> glimpse(); df1 |> glimpse()
  df0    <- df0 |> filter(year > yrRef0)
  df0    <- df0 |> rbind(df1)
  rm(df1)

  ### Order values
  order0 <- df0 |> names() |> get_matches(y=valCol0, matches=F)
  df0    <- df0 |> arrange_at(order0)
}


###### format_inputScenarios ######
### This function helps format input scenarios
format_inputScenarios <- function(
    df0,       ### Scenario input data frame to format
    conn,      ### DB COnnection Object
    name0,     ### Name of input c("temp, "slr", "gdp", "pop")
    hasInput0, ### Whether the user provided an input
    idCols0,   ### ID columns
    valCols0,  ### Value columns
    minYear,   ### Minimum year
    maxYear,   ### Maximum year
    info0      ### Other info
){
  ### Message user
  label0  <- info0 |> filter(inputName == name0) |> pull(inputType)
  msg_i1  <- "Creating " |> paste0(label0, " scenario from user inputs...")
  msg_i2  <- "No "       |> paste0(label0, " scenario provided...using default scenario...")
  msg_i   <- hasInput0 |> ifelse(msg_i1, msg_i2)

  ### Info (need different info if calculating SLR from temperatures)
  doSlr   <- (name0 %in% c("slr")) & ("temp_C" %in% (df0 |> names()))
  name1   <- doSlr  |> ifelse("temp", name0)
  infoSlr <- info0  |> filter(inputName == "slr")
  info1   <- info0  |> filter(inputName == name1)
  if(doSlr) valCol0 <- info1  |> pull(valueCol)
  else      valCol0 <- valCols0
  yrRef0  <- info1  |> pull(ref_year)

  if(doSlr) 1 |> get_msgPrefix() |> paste0("Creating ", label0 , " scenario from user temperature scenario...") |> message()
  else      1 |> get_msgPrefix() |> paste0(msg_i) |> message()

  ### Format data
  select0 <- idCols0 |> c(valCol0) |> c("year") |> unique()
  # select0 |> print(); df0 |> glimpse()
  df0     <- df0 |> select(all_of(select0))
  df0     <- df0 |> filter_all(all_vars(!(. |> is.na())))

  ### Interpolate user inputs
  hasInput0 <- !(df0 |> is.null())
  if(hasInput0) {
    ### Zero out values if temp or slr
    doZero0 <- name0 %in% c("temp", "slr")
    if(doZero0) df0 <- df0 |> zero_out_scenario(conn = conn, type0=name1)

    ### Calculate values
    yrs0 <- yrRef0:maxYear |> unique()
    if("pop" %in% name0) {
      df0  <- df0 |> interpolate_annual(years=yrs0, column=valCol0, rule=1, byState=T, byModel=F) |> ungroup()
    } else if("o3" %in% name0) {
      df0  <- df0 |> interpolate_annual(years=yrs0, column=valCol0, rule=1, byState=T, byModel=T) |> ungroup()
    } else {
      df0  <- df0 |> mutate(region = "NationalTotal")
      df0  <- df0 |> interpolate_annual(years=yrs0, column=valCol0, rule=1) |> ungroup()
      df0  <- df0 |> select(-c("region"))
    } ### End if("pop" %in% name0)


    ### If SLR, calculate SLR values from temperatures
    if(doSlr) {
      ### - First, calculate global temps
      df0     <- df0 |> mutate(temp_C = temp_C |> convertTemps(from="conus"))
      ### Then, calculate SLR heights
      join0   <- c("year")
      drop0   <- c("temp_C")
      df1     <- temps2slr(temps = df0 |> pull(temp_C), years = df0 |> pull(year))
      df0     <- df0 |> left_join(df1, by=join0)
      df0     <- df0 |> select(-any_of(drop0))
      df0     <- df0 |> zero_out_scenario(conn = conn, type0=name0)
      rm(join0, drop0)
    } ### End if(doSlr)
  } ### End if(hasInput0)
  # df0 |> glimpse()
  ### Filter to appropriate years
  df0     <- df0 |> filter(year >= minYear) |> filter(year <= maxYear)

  ### Return
  return(df0)
}


###### Function to get years from data
### Get a sequence from a set of years
get_years_fromData <- function(years0, by0=1){
  min0 <- years0 |> min(na.rm=T)
  max0 <- years0 |> max(na.rm=T)
  yrs0 <- min0   |> seq(max0, by=by0)
  return(yrs0)
}


###### Interpolate GDP scenario
### df0 is a data frame with columns c("year", "gdp_usd")
interpolate_gdp <- function(df0){
  ### Import Functions to Namespace
  interpolate_annual <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  ### Select columns
  select0 <- c("year", "gdp_usd")
  df0     <- df0 |> select(all_of(select0))
  ### Get years
  years0  <- df0 |> pull(year) |> get_years_fromData()
  ### Add region="NationalTotal"
  df0     <- df0 |> mutate(region = "NationalTotal")
  ### Interpolate annual
  sum0    <- c("gdp_usd")
  df0     <- df0 |> interpolate_annual(years=years0, column=sum0, rule=1, byState=F)
  ### Drop region
  drop0   <- c("region")
  df0     <- df0 |> select(-any_of(drop0))
  ### Return
  return(df0)
}


###### Interpolate population
### df0 is a data frame with columns c("region", "state", "postal", "year", "pop")
interpolate_pop <- function(df0){
  ### Import Functions to Namespace
  interpolate_annual <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  ### Select column
  select0 <- c("region", "state", "postal", "year", "pop")
  df0     <- df0 |> select(all_of(select0))
  ### Get years
  years0  <- df0 |> pull(year) |> get_years_fromData()
  ### Interpolate annual
  sum0    <- c("pop")
  df0     <- df0 |> interpolate_annual(years=years0, column=sum0, rule=1, byState=T)
  ### Return
  return(df0)
}

### Calculate national population
### df0 is a data frame with columns c("region", "state", "postal", "year", "pop")
calc_nationalPop <- function(df0){
  ### Select columns
  popCol0  <- c("pop")
  select0  <- c("region", "state", "postal", "year") |> c(popCol0)
  df0      <- df0 |> select(all_of(select0)) |> unique()
  ### Filter out missing values
  df0      <- df0 |> filter(!(region |> str_detect("National")))
  df0      <- df0 |> filter(!(region |> is.na()))
  ### Summarize population over states and regions
  group0   <- c("year")
  sum0     <- c(popCol0)
  df0      <- df0 |>
    group_by_at (c(group0)) |>
    summarize_at(c(sum0), sum, na.rm=T) |> ungroup()
  ### Rename values
  renameAt <- sum0
  renameTo <- "national_pop"
  df0      <- df0 |> rename_at(c(renameAt), ~c(renameTo))
  ### Return
  return(df0)
}


### Create national scenario from national population and GDP information
### gdp0 is a data frame with columns c("year", "gdp_usd")
### pop0 is a data frame with columns c("region", "state", "postal", "year", "pop")
create_nationalScenario <- function(
    gdp0,
    pop0,
    natPop0=NULL
){
  ### If national population is NULL, calculate national population
  nullNpop <- natPop0 |> is.null()
  if(nullNpop) natPop0 <- pop0 |> calc_nationalPop()
  ### Select columns
  colsG0   <- c("year", "gdp_usd")
  colsP0   <- c("region", "state", "postal", "year", "pop")
  colsN0   <- c("year", "national_pop")
  gdp0     <- gdp0 |> select(all_of(colsG0))
  pop0     <- pop0 |> select(all_of(colsP0))
  ### Join GDP and national population by year
  join0    <- c("year")
  nat0     <- gdp0 |> left_join(natPop0, by=join0)
  ### Calculate GDP per capita
  nat0     <- nat0 |> mutate(gdp_percap = gdp_usd / national_pop)
  ### Join nat0 with state population by year
  nat0     <- nat0 |> left_join(pop0, by=join0, relationship="many-to-many")
  ### Arrange by colsP0
  arrange0 <- colsP0 |> get_matches(y=c("pop"), matches=F)
  nat0     <- nat0 |> arrange_at(vars(arrange0))
  ### Return
  return(nat0)
}




###### update_popScalars ######
### Update scalars with regional population scenario
update_popScalars <- function(
    df_scalars, ### Tibble of scalars
    df_pop,     ### Socioeconomic scenario
    groupCols = c("region") |> c("state", "postal") |> c("year"),
    popCol    = c("pop")
    # popCol    = c("state_pop")
){
  ###### Format Data ######
  ### Drop region population, gdp_percap
  # df_scalars <- df_scalars |> filter(scalarName != "reg_pop")
  filter0    <- c("scalarName")
  vals0      <- c("reg_pop", "gdp_percap", "gdp_usd")
  df_scalars <- df_scalars |> filter_at(c(filter0), function(x, y=vals0){!(x %in% y)})
  rm(filter0, vals0)

  ###### National Scenario ######
  select0    <- c("gdp_usd", "gdp_percap", "year")
  sort0      <- c("scalarName", "year")
  idCol0     <- c("year")
  df_nat     <- df_pop
  df_nat     <- df_nat |> select(all_of(select0)) |> unique()
  df_nat     <- df_nat |> pivot_longer(
    -all_of(idCol0),
    names_to  = "scalarName",
    values_to = "value"
  ) ### End pivot_longer
  df_nat     <- df_nat |> mutate(scalarType           = "econMultiplier")
  df_nat     <- df_nat |> mutate(region               = "National")
  df_nat     <- df_nat |> mutate(state                = "N/A")
  df_nat     <- df_nat |> mutate(postal               = "N/A")
  df_nat     <- df_nat |> mutate(national_or_regional = "national")
  df_nat     <- df_nat |> arrange_at(c(sort0))
  ### Bind values
  df_scalars <- df_scalars |> rbind(df_nat)
  rm(select0, sort0, idCol0)

  ###### GDP per cap ######
  ### Filter to scalarName and mutate scalarType
  df_gdp     <- df_nat
  df_gdp     <- df_gdp |> filter(scalarName %in% "gdp_percap")
  df_gdp     <- df_gdp |> mutate(scalarType = "econScalar")
  ### Bind values
  df_scalars <- df_scalars |> rbind(df_gdp)
  rm(df_nat, df_gdp)

  ###### Population ######
  ### - Select columns
  ### - Add additional scalar attributes
  ### - Rename column
  drop0      <- c("gdp_usd", "national_pop", "gdp_percap")
  from0      <- c("pop")
  to0        <- c("value")
  df_pop     <- df_pop |> select(-any_of(drop0))
  df_pop     <- df_pop |> mutate(scalarName           = "reg_pop")
  df_pop     <- df_pop |> mutate(scalarType           = "physScalar")
  df_pop     <- df_pop |> mutate(national_or_regional = "regional")
  df_pop     <- df_pop |> rename_at(c(from0), ~to0)
  ### Bind values
  df_scalars <- df_scalars |> rbind(df_pop)
  rm(drop0, from0, to0, df_pop)


  ###### Return ######
  gc()
  return(df_scalars)
}

###### extend_slrScalars ######
### Scalars for SLR past 2090
extend_slrScalars2 <- function(
    df0,        ### Tibble of initial results
    conn,       ### DB Connection Object
    # df_se,      ### Socioeconomic scenario
    df_scalars, ### Tibble of scalar values: df_Scalars
    refYear0    = DBI::dbReadTable(conn,"co_slrScalars") |> pull(refYear) |> unique() |> first(),
    elasticity  = NULL
){
  ###### State columns ######
  ### By state
  stateCols0 <- c("state", "postal")
  popCol0    <- c("pop")

  ###### Ref Year ######
  ### Filter to years >= refYear (2090)
  df_info    <- DBI::dbReadTable(conn,"co_slrScalars")
  df_scalars <- df_scalars |> filter(year >= refYear0)
  df_se      <- df_se      |> filter(year >= refYear0)
  df0        <- df0        |> filter(year >= refYear0)

  ### Drop columns from data
  drop0      <- c("physScalar", "physAdj", "damageAdj", "econScalar", "econMultiplier", "econAdj")
  drop1      <- drop0 |> paste0("Name") |> c(drop0 |> paste0("Value"))
  drop1      <- drop1 |> c("physScalar", "econScalar", "econMultiplier", "physEconScalar")
  drop1      <- drop1 |> c("c1", "exp0")
  df0        <- df0   |> select(-all_of(drop1))
  rm(drop0, drop1)

  ###### Elasticity ######
  ### Update exponent
  if(!(elasticity |> is.null())){
    df_info <- df_info |> mutate(exp0 = (econMultiplierName=="vsl_usd") |> ifelse(elasticity, exp0))
  } ### End if(!(elasticity |> is.null()))
  ### Add column
  df_info    <- df_info |> mutate(econAdjName = "none")

  ###### Join Data & Scalar Info ######
  ### Join initial results & scalar info
  join0      <- c("sector","impactType")
  df0        <- df0 |> left_join(df_info, by=join0)
  rm(join0)

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
  select0    <- gather0  |> c("year")
  df_mult0   <- df_se    |> select(all_of(select0)) |> distinct()
  df_mult0   <- df_mult0 |> pivot_longer(
    cols      = all_of(gather0),
    names_to  = "econMultiplierName",
    values_to = "econMultiplierValue"
  ) ### End pivot_longer()

  ### Drop values
  rm(gather0, select0)

  ### Join with data
  join0      <- c("econMultiplierName", "year")
  df0        <- df0 |> left_join(df_mult0, by=join0)
  rm(join0)

  ###### Economic Adjustment Values ######
  ### Get adjustment values & join with data
  rename0    <- c("econMultiplierValue", "year")
  rename1    <- c("econAdjValue", "refYear")
  join0      <- c("econMultiplierName", "refYear")
  select0    <- join0    |> c("econAdjValue")
  df_adj0    <- df_mult0 |> filter(year == refYear0)
  df_adj0    <- df_adj0  |> rename_at(c(rename0), ~rename1)
  # df0 |> glimpse(); df_adj0 |> glimpse()
  df_adj0    <- df_adj0  |> select(all_of(select0))
  df0        <- df0      |> left_join(df_adj0, by=join0)
  ### Drop values
  rm(rename0, rename1, select0, join0, df_mult0, df_adj0)
  # "got here1" |> print()

  ###### CHECK for ECON Value "none" and add 1 #####
  df0        <- df0 |> mutate(econAdjValue = case_when(
    econAdjName == "none" ~ 1,
    .default = econAdjValue
  ),
  econMultiplierValue = case_when(
    econMultiplierName == "none" ~ 1,
    .default = econMultiplierValue
  ))


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
  join0      <- c("physScalarName", "year") |> c("region") |> c(stateCols0)
  # select0    <- rename1 |> c("year")
  select0    <- rename1 |> c("year") |> c("region") |> c(stateCols0)
  vals0      <- df0     |> pull(physScalarName) |> unique()

  ### Filter, rename, select, join
  df_phys0   <- df_scalars |> filter(scalarType == type0)
  # df0 |> glimpse(); df_phys0 |> glimpse()
  df_phys0   <- df_phys0   |> filter(scalarName %in% vals0)
  df_phys0   <- df_phys0   |> rename_at(c(rename0), ~rename1)
  df_phys0   <- df_phys0   |> select(all_of(select0))
  # df0 |> glimpse(); df_phys0 |> glimpse()
  df0        <- df0        |> left_join(df_phys0, by=join0)
  ### Drop values
  rm(type0, rename0, rename1, join0, select0, vals0)

  ###### Physical Adjustment Values ######
  ### Get adjustment values & join with data
  rename0    <- c("physScalarName", "physScalarValue", "year")
  rename1    <- c("physAdjName", "physAdjValue", "refYear")
  join0      <- c("physAdjName", "refYear") |> c("region") |> c(stateCols0)
  select0    <- rename1  |> c("refYear") |> c("region") |> c(stateCols0)
  df_adj0    <- df_phys0 |> filter(year == refYear0)
  df_adj0    <- df_adj0  |> rename_at(c(rename0), ~rename1)
  df_adj0    <- df_adj0  |> select(all_of(select0))
  df0        <- df0 |> mutate(physAdjName = physScalarName)
  # df0 |> glimpse(); df_adj0 |> glimpse()
  df0        <- df0 |> left_join(df_adj0, by=join0)
  ### Drop values
  rm(rename0, rename1, select0, join0, df_phys0, df_adj0)

  ###### CHECK for PHYS Value "none" and add 1 #####
  df0        <- df0 |> mutate(physScalarValue = case_when(
    physScalarName== "none" ~ 1,
    .default = physScalarValue
  ),
  physAdjValue = case_when(
    physScalarName == "none" ~ 1,
    .default = physAdjValue
  ))

  ###### Physical Scalar & Phys Econ Scalar ######
  ### Calculate econ scalar values
  df0        <- df0 |> mutate(physScalar     = c2 * physScalarValue / physAdjValue)
  df0        <- df0 |> mutate(physEconScalar = econScalar + physScalar)

  ### Drop columns & join
  drop0      <- c("c2", "refYear")
  df0        <- df0 |> filter(year > refYear)
  df0        <- df0 |> select(-all_of(drop0))
  rm(drop0)

  ###### Return ######
  ### Return
  return(df0)
}

extend_slrScalars <- function(
    df0,        ### Tibble of initial results
    conn,
    scalars,    ### Tibble of scalar values: df_Scalars
    slrScalars  = DBI::dbReadTable(conn,"co_slrScalars"),
    refYear0    = slrScalars |> pull(refYear) |> unique() |> min(),
    elasticity  = NULL
){
  ###### Separate Data ######
  ### Not all SLR sectors need to have scalars extended with extend_slrScalars()
  ### Divide data into those that are in slrScalars and those that aren't
  sectors0   <- slrScalars |> pull(sector) |> unique()
  # sectors0   <- c("CoastalProperties", "HTF")
  dfSame     <- df0     |> filter(!(sector %in% sectors0))
  df0        <- df0     |> filter(  sector %in% sectors0)
  doExtend   <- df0     |> nrow()
  if(!doExtend) {return(dfSame)}

  ###### Filter to reference year ######
  # refYear0 |> print()
  scalars    <- scalars |> filter(year >= refYear0)
  dfRef      <- df0     |> filter(year <= refYear0)
  df0        <- df0     |> filter(year >= refYear0)
  names0     <- df0     |> names()

  ###### Rename Columns ######
  renameAt0  <- c("refYear")
  renameTo0  <- c("year0")
  slrScalars <- slrScalars |> rename_at(c(renameAt0), ~renameTo0)
  slrScalars <- slrScalars |> mutate(year0 = year0 |> as.character())
  rm(renameAt0, renameTo0)

  ###### Join Data & Scalar Info ######
  ### Drop columns from data
  ### Join initial results & scalar info
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

  ###### Update Scalars ######
  ### Physical scalars
  df0        <- df0 |> match_scalarValues(conn = conn, scalars=scalars, scalarType="physScalar")
  # df0 |> glimpse()
  ### Physical scalar adjustment values
  renameAt0  <- c("physScalarAdj") |> paste0(c("Name", "Value"))
  renameTo0  <- renameAt0 |> str_replace("Scalar", "")
  df0        <- df0 |> get_scalarAdjValues(scalars=scalars, scalarType0="physScalar")
  df0        <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  # df0 |> glimpse()

  ### Economic multiplier
  df0        <- df0 |> match_scalarValues(conn = conn, scalars=scalars, scalarType="econMultiplier")
  # df0 |> glimpse()
  ### Get economic adjustment values
  renameAt0  <- c("econMultiplierAdj") |> paste0(c("Name", "Value"))
  renameTo0  <- renameAt0 |> str_replace("Multiplier", "")
  df0        <- df0 |> get_scalarAdjValues(scalars=scalars, scalarType0="econMultiplier")
  df0        <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  # df0 |> glimpse()

  ###### Calculate Scalars ######
  ### Calculate scalars
  df0        <- df0   |> calcScalars(extendSlr=TRUE, elasticity=elasticity)
  # df0 |> glimpse()

  ###### Drop Columns ######
  ### Drop columns
  drop0      <- c("c2")
  df0        <- df0   |> select(-any_of(drop0))
  rm(drop0)

  ###### Bind Results ######
  ### Bind results back in
  df0        <- df0    |> select(all_of(names0))
  df0        <- df0    |> filter(year > refYear0)
  df0        <- dfRef  |> bind_rows(df0)
  df0        <- dfSame |> bind_rows(df0)
  rm(dfRef, dfSame)
  # df0 |> filter(!(scaled_impacts |> is.na())) |> filter(year > 2100) |> glimpse()

  ###### Return ######
  ### Return
  gc()
  return(df0)
}

###### get_co_sectorsInfo ######
### This helper function helps get info about sector groups
get_co_sectorsInfo <- function(
    sectors0   = NULL,  ### Sector IDs
    conn,               ### DB Connection Object
    addRegions = FALSE, ### Whether to include regions & states
    addModels  = FALSE, ### Whether to include models
    colTypes   = c("ids", "labels", "extra") ### Types of columns to include: IDs, labels, or extra. If only labels, will return labels without the "_label"
){
  ### Get objects from FrEDI
  # co_sectors  <- "co_sectors"     |> get_frediDataObj("frediData")
  # co_variants <- "co_variants"    |> get_frediDataObj("frediData")
  # co_impTypes <- "co_impactTypes" |> get_frediDataObj("frediData")
  # co_impYears <- "co_impactYears" |> get_frediDataObj("frediData")
  # co_regions  <- "co_regions"     |> get_frediDataObj("frediData")
  # co_states   <- "co_states"      |> get_frediDataObj("frediData")
  # co_modTypes <- "co_modelTypes"  |> get_frediDataObj("frediData")
  # co_models   <- "co_models"      |> get_frediDataObj("frediData")
  co_sectors  <- DBI::dbReadTable(conn,"co_sectors")
  co_variants <- DBI::dbReadTable(conn,"co_variants")
  co_impTypes <- DBI::dbReadTable(conn,"co_impactTypes")
  co_impYears <- DBI::dbReadTable(conn,"co_impactYears")
  co_regions  <- DBI::dbReadTable(conn,"co_regions")
  co_states   <- DBI::dbReadTable(conn,"co_states")
  co_modTypes <- DBI::dbReadTable(conn,"co_modelTypes")
  co_models   <- DBI::dbReadTable(conn,"co_models")

  ### Conditionals
  colTypes    <- colTypes |> tolower()
  doIds       <- "ids"    %in% colTypes
  doLabs      <- "labels" %in% colTypes
  doExtra     <- "extra"  %in% colTypes
  onlyLabs    <- !doIds

  ### Initialize some values as empty vectors
  ### Adjust values in vectors depending on conditionals
  colsReg0    <- c()
  colsMod0    <- c()
  if(addRegions) colsReg0 <- c("region", "state", "postal")
  if(addModels ) colsMod0 <- c("model")

  ### Column names
  colsData0   <- c("sector", "variant", "impactType", "impactYear") |> c(colsReg0) |> c("modelType") |> c(colsMod0)
  colsIds0    <- colsData0 |> get_matches(y=c("modelType", "state", "postal"), matches=FALSE) |> paste0("_id")
  colsLabs0   <- colsData0 |> get_matches(y=c("modelType", "state", "postal"), matches=FALSE) |> paste0("_label")
  colsVars    <- c("sectorprimary", "includeaggregate", "damageAdjName")
  colsTypes   <- c("impactType_description", "physicalmeasure") |>
    c(c("physScalar", "physAdj", "econScalar", "econMultiplier") |> paste0("Name")) |>
    c("c0", "c1", "exp0", "year0")
  colsMods0   <- c("maxUnitValue", "inputName") |>
    c("model" |> paste0(c("UnitDesc", "Unit_id", "Unit_label"))) |>
    c("model" |> paste0(c("UnitScale", "RefYear", "MaxOutput", "MaxExtrap")))
  colsMods0   <- colsMods0 |> get_matches(y=c("model" |> paste0(c("UnitScale", "RefYear", "MaxOutput", "MaxExtrap"))), matches=F)

  ### Add additional columns
  colsOth0    <- c()
  if(doExtra) {
    colsOth0 <- c(colsVars, colsTypes)
    if(addModels) colsOth0 <- colsOth0 |> c(colsMods0, "maxUnitValue")
  } ### if(doAll)


  ### Filter data
  hasSectors  <- sectors0 |> length()
  if(hasSectors) co_sectors <- co_sectors |> filter(sector_id %in% sectors0)

  ### Rename columns
  renameAt0   <- c("modelType")
  co_modTypes <- co_modTypes |> rename_at(c(renameAt0 |> paste0("_id")), ~c(renameAt0))
  rm(renameAt0)

  ### Join with co_variants, co_impactTypes, co_impactYears
  join0   <- c("sector_id")
  join1   <- c("modelType")
  df0     <- co_sectors |> left_join(co_variants, by=join0)
  df0     <- df0        |> left_join(co_impTypes, by=join0, relationship="many-to-many")
  df0     <- df0        |> left_join(co_impYears, by=join0, relationship="many-to-many")
  df0     <- df0        |> left_join(co_modTypes, by=c(join1), relationship="many-to-many")
  rm(join0, join1)

  ### Join with co_regions and co_states if addStates
  if(addRegions) {
    ### Rename column in states
    ### Join states with regions
    ### Join data with states
    join0     <- c("region_id")
    join1     <- c("joinCol")
    renameAt0 <- c("region")
    co_states <- co_states |> rename_at(c(renameAt0), ~c(renameAt0 |> paste0("_id")))
    # co_states <- co_states |> mutate   (region = region |> str_replace(" ", ""))
    co_states <- co_states |> left_join(co_regions, by=join0)
    co_states <- co_states |> mutate(joinCol = 1)
    df0       <- df0       |> mutate(joinCol = 1)
    df0       <- df0       |> left_join(co_states, by=c(join1), relationship="many-to-many")
    df0       <- df0       |> select(-all_of(join1))
    rm(renameAt0, join0, join1)
  } ### End if(addModels)

  ### Join with co_models if addModels
  if(addModels) {
    # join0     <- c("modelType")
    join0     <- df0 |> names() |> get_matches(y=co_models |> names())
    df0       <- df0 |> left_join(co_models, by=join0, relationship="many-to-many")
    rm(join0)
  } ### End if(addModels)

  ### Rename values
  # df0 |> glimpse()
  # renameTo <- c("sector", "variant", "impactYear", "impactType") |> c(colsReg0) |> c(colsMod0)
  renameTo0 <- colsData0 |> get_matches(y=c("modelType", "state", "postal"), matches=FALSE)
  renameAt0 <- renameTo0 |> paste0("_id")
  df0       <- df0       |> rename_at(c(renameAt0), ~renameTo0)

  ### Select values
  select0   <- c()
  names0    <- df0 |> names()
  if(doIds  ) select0 <- select0 |> c(colsData0) |> unique()
  if(doLabs ) select0 <- select0 |> c(colsLabs0) |> unique()
  if(doExtra) select0 <- select0 |> c(colsOth0 ) |> unique()
  df0       <- df0 |> select(all_of(select0))

  ### Arrange values
  arrange0  <- c()
  if     (doIds ) arrange0 <- c(colsData0)
  else if(doLabs) arrange0 <- c(colsLabs0)
  df0       <- df0 |> arrange_at(c(arrange0))

  ### Rename columns
  if(onlyLabs) {
    renameAt0 <- colsLabs0
    renameTo0 <- colsData0
    df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
    rm(rename0, renameAt, renameTo)
  } ### End if(onlyLabs)

  ### Return
  # df0 |> glimpse()
  return(df0)
}

###### match_scalarValues ######
### This function matches interpolated scalar values for each scalar type to the time series scenario information
### Scalar types are: physAdj, physMultiplier, damageAdj, econScalar, econMultiplier
### Function "match_scalarValues" replaces "get_popWts", "get_physMultipliers", and "get_econScalars"
match_scalarValues <- function(
    df0,       ### Initial results dataframe
    conn,      ### DB Connection Object
    scalars    = DBI::dbReadTable(conn,"stateData") |> (function(stateData){
                                                       stateData    <- unserialize(stateData$value |> unlist())
                                                       scalars      <- stateData[["df_scalars"]]
                                                       return(scalars)
                                                      })()   ,
    scalarType ### Type of scalar (one of: c("damageAdj", "econScalar", "physAdj", "phsScalar"))
){
  # df0 |> glimpse(); scalars |> glimpse()
  ###### State columns ######
  stateCols0  <- c("state", "postal")

  ###### FrEDI Data ######
  ### Scalar columns to rename
  renameAt0   <- "scalarName"
  renameTo0   <- scalarType |> paste0(c("Name"))
  scalarName  <- renameTo0
  scalars     <- scalars |> rename_at(c(renameAt0), ~renameTo0)

  ### Filter to years
  select0     <- c("year")
  years0      <- df0     |> pull(year) |> get_years_fromData()
  minYr0      <- years0  |> min()
  maxYr0      <- years0  |> max()
  scalars     <- scalars |> filter(year >= minYr0, year <= maxYr0)
  rm(select0)

  ###### Filter Scalars ######
  ### Filter to Scalar Type
  # scalarType |> print(); scalars |> glimpse();
  scalarType0 <- scalarType
  scalars     <- scalars |> filter(scalarType %in% scalarType0)
  scalars     <- scalars |> select(-c("scalarType"))
  rm(scalarType)

  ###### National vs Regional Scalars ######
  # scalars$national_or_regional |> unique() |> print()
  scalars_reg <- scalars |> filter(national_or_regional != "national")
  scalars_nat <- scalars |> filter(national_or_regional == "national")

  ### Drop columns
  drop_reg0   <- c("national_or_regional")
  drop_nat0   <- drop_reg0 |> c("region") |> c(stateCols0)
  scalars_reg <- scalars_reg |> select(-all_of(drop_reg0))
  scalars_nat <- scalars_nat |> select(-all_of(drop_nat0)) |> distinct()
  rm(drop_reg0, drop_nat0)
  # scalars_nat |> glimpse()

  ### Scalar names
  names_reg   <- scalars_reg |> pull(all_of(scalarName)) |> unique()
  names_nat   <- scalars_nat |> pull(all_of(scalarName)) |> unique()

  ###### Filter Data ######
  ### Filter data
  # scalarName |> print(); df0 |> glimpse()
  filter0     <- c(scalarName)
  df_none     <- df0 |> filter_at(c(filter0), function(x){x %in% "none"   })
  df_reg      <- df0 |> filter_at(c(filter0), function(x){x %in% names_reg})
  df_nat      <- df0 |> filter_at(c(filter0), function(x){x %in% names_nat})
  rm(df0, filter0)
  # df_none |> glimpse()

  ### Check whether filtered data has rows
  has_none    <- df_none |> nrow()
  has_reg     <- df_reg  |> nrow()
  has_nat     <- df_nat  |> nrow()
  # c(has_none, has_reg, has_nat) |> print()
  # scalars |> glimpse()

  ###### Mutate Data ######
  ### Initialize results
  df0              <- tibble()

  ### Add values to values with no scalar
  if(has_none) {
    # "has_none" |> print()
    df_none <- df_none |> mutate(value=1)
    df0     <- df0     |> rbind(df_none)
    # df_none |> glimpse(); df0 |> glimpse()
    rm(df_none)
  } ### End if(has_none)
  # df0 |> glimpse()

  ### Regional
  ### Join & drop
  if(has_reg) {
    join0   <- c("region") |> c(stateCols0) |> c(scalarName) |> c("year")
    df_reg  <- df_reg  |> left_join(scalars_reg, by=join0)
    df0     <- df0     |> rbind(df_reg)
    rm(join0, df_reg)
  } ### End if(has_regional)
  # df0 |> glimpse()

  ### National values
  # scalars_nat |> glimpse(); df_nat |> glimpse();
  ### Join & drop
  if(has_nat) {
    join0   <- c(scalarName) |> c("year")
    df_nat  <- df_nat  |> left_join(scalars_nat, by=join0)
    df0     <- df0     |> rbind(df_nat)
    # df_nat |> glimpse(); df0 |> glimpse()
    rm(join0, df_nat)
  } ### End if(has_national)

  ### Add placeholder column
  hasData0  <- df0 |> nrow()
  # df0 |> glimpse()
  if(!hasData0) df0  <- df0 |> mutate(value = NA)

  ### Rename
  renameAt0 <- "value"
  renameTo0 <- scalarType0 |> paste0(c("Value"))
  df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)

  ###### Return ######
  return(df0)
}


###### get_econAdjValues ######
### Get Economic Adjustment/Multiplier
### This function matches interpolated scalar values for each scalar type to the time series scenario information
### Scalar types are: physAdj, physMultiplier, damageAdj, econScalar, econMultiplier
### Function "get_econAdjValues" replaces "get_econMultipliers"
get_scalarAdjValues <- function(
    data,       ### Initial results dataframe
    conn,       ### DB Connection Object
    scalars,    ### Scalars data frame
    scalarType0 = "econMultiplier",
    multipliers = DBI::dbReadTable(conn,"co_econMultipliers") |> pull(econMultiplierName) |> get_matches(y="none", matches=FALSE) # multipliers ### List of multipliers
){
  ###### Multipliers
  none0       <- "none"

  ###### Format data
  ### - Add adjName column
  ### - Separate data
  ### - Check number of rows
  colName0    <- scalarType0 |> paste0("Name")
  adjName0    <- "adjName"
  adjValue0   <- "adjValue"
  # names0      <- data |> names() |> c("adjName", "adjValue")
  data        <- data |> mutate(adjName = data |> pull(all_of(colName0)))
  # data |> glimpse(); data |> pull(adName) |> unique() |> print()
  df_none     <- data |> filter(  adjName %in% none0)
  df_other    <- data |> filter(!(adjName %in% none0))
  # df_none |> dim() |> print(); df_other |> dim() |> print()
  rm(data)

  ### Number of rows
  hasNone0    <- df_none  |> nrow()
  hasOther0   <- df_other |> nrow()

  ### Initialize results
  data        <- tibble()

  ###### ScalarName == "None"
  ### Filter the data to those for which the scalar identifier == "none"...value = 1
  ### Set econMultiplierValue, adjValue == 1 if scalarMultiplierName=none
  if(hasNone0 ) {
    df_none  <- df_none  |> (function(x, y=adjValue0){x[,y] <- 1; return(x)})()
    # df_none |> glimpse()
    data     <- data     |> rbind(df_none)
    rm(df_none)
  } ### End if(hasNone0 )

  ### Other values:
  if(hasOther0) {
    ###### Format scalar data
    ### Get values for a single region since the multipliers are the same for all regions
    ### Gather scenario information
    ### Rename year to year0 and convert to character
    filter0     <- scalarType0
    drop0       <- c("scalarType")
    renameAt0   <- c("year" ) |> c("scalarName" , "value")
    renameTo0   <- c("year0") |> c(adjName0, adjValue0)
    mutate0     <- c("year0")
    ### Filter
    scalars     <- scalars |> filter(scalarType %in% filter0)
    scalars     <- scalars |> select(-all_of(drop0))
    scalars     <- scalars |> rename_at(c(renameAt0), ~renameTo0)
    scalars     <- scalars |> mutate_at(c(mutate0), as.character)
    rm(filter0, renameAt0, renameTo0, mutate0, drop0)
    # data |> glimpse(); scalars |> glimpse()
    # scalars |> pull("national_or_regional") |> unique() |> print()

    ### Divide into national and regional values
    natStr0     <- "national"
    dropReg     <- c("national_or_regional")
    dropNat     <- c("region", "state", "postal") |> c(dropReg)
    scalarsNat  <- scalars  |> filter(  national_or_regional %in% natStr0 ) |> select(-any_of(dropNat))
    scalarsReg  <- scalars  |> filter(!(national_or_regional %in% natStr0)) |> select(-any_of(dropReg))
    natScalars  <- scalarsNat |> pull(all_of(adjName0)) |> unique()
    regScalars  <- scalarsReg |> pull(all_of(adjName0)) |> unique()
    # natScalars |> print(); regScalars |> print()
    # scalarsNat |> glimpse(); scalarsReg |> glimpse()
    # df_other |> dim() |> print()
    dfOthNat    <- df_other |> filter_at(c(adjName0), function(x, y=natScalars){x %in% y})
    dfOthReg    <- df_other |> filter_at(c(adjName0), function(x, y=regScalars){x %in% y})
    # list(df_other, dfOthNat, dfOthReg) |> map(nrow) |> unlist() |> print()
    # dfOthNat |> glimpse(); dfOthReg |> glimpse()
    rm(natStr0, dropReg, dropNat, df_other)

    ### Join adjustments with data
    ### Number of rows
    hasNat      <- dfOthNat |> nrow()
    hasReg      <- dfOthReg |> nrow()
    ### National values
    if(hasNat) {
      join0    <- dfOthNat |> names() |> get_matches(scalarsNat |> names())
      dfOthNat <- dfOthNat |> left_join(scalarsNat, by=join0)
      # dfOthNat |> dim() |> print()
      # dfOthNat |> glimpse()
      data     <- data     |> rbind(dfOthNat)
      rm(join0, scalarsNat, dfOthNat)
    } ### End if(hasNat)

    ### Regional values
    if(hasReg) {
      join0    <- dfOthReg |> names() |> get_matches(scalarsReg |> names())
      dfOthReg <- dfOthReg |> left_join(scalarsReg, by=join0)
      # dfOthReg |> dim() |> print()
      # dfOthReg |> glimpse()
      data     <- data     |> rbind(dfOthReg)
      rm(join0, scalarsReg, dfOthReg)
    } ### End if(hasReg)
  } ### End if(hasOther0)

  ###### Bind values and rename
  # data |> glimpse()
  renameAt0   <- c("adjName", "adjValue")
  renameTo0   <- c(scalarType0 |> paste0(c("AdjName", "AdjValue")))
  data        <- data |> rename_at(c(renameAt0), ~renameTo0)
  # data |> glimpse()

  ###### Return results values
  gc()
  return(data)
}


###### initialize_resultsDf ######
### Initialize results data frame
initialize_resultsDf <- function(
    df_se,     ### Dataframe with socioeconomic scenario
    conn,      ### DB Connection Object
    sectors    = DBI::dbReadTable(conn,"co_sectors") |> pull(sector_id), ### Vector of sectors
    elasticity = NULL,
    #df_scalars = "df_scalars" |> get_frediDataObj("stateData"),  ### Tibble of main scalars
    slrScalars = DBI::dbReadTable(conn,"co_slrScalars"),
    refYear0   = slrScalars |> pull(refYear) |> unique() |> min(),
    msg0       = "\t"
){
  ###### Values ######
  ###### Messaging
  msg1       <- msg0 |> paste0("\t")
  paste0(msg1, "Formatting initial results", "...") |> message()

  ###### State Columns
  stateCols0 <- c("state", "postal")
  popCol0    <- c("pop")

  ###### Get min, max year
  years0     <- df_se  |> pull(year) |> get_years_fromData()
  minYr0     <- years0 |> min()
  maxYr0     <- years0 |> max()

  ### Format scalars
  ### Filter to years, update with info from socioeconomic scenario
  stateData   <- DBI::dbReadTable(conn,"stateData")
  stateData    <- unserialize(stateData$value |> unlist())
  df_scalars      <- stateData[["df_scalars"]]
  df_scalars <- df_scalars |> filter(year >= minYr0, year <= maxYr0)
  df_scalars <- df_scalars |> update_popScalars(df_se, popCol=popCol0)

  ###### Scalar Info ######
  ### Get info
  df_info    <- sectors |> get_co_sectorsInfo(
    conn = conn,
    addRegions = TRUE , ### Whether to include regions & states
    addModels  = FALSE, ### Whether to include models
    colTypes   = c("ids", "extra") ### Types of columns to include: IDs, labels, or extra. If only labels, will return labels without the "_label"
  ) ### End get_co_sectorsInfo()

  ### Format info
  drop0      <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0) |> c("modelType") |> paste0("_label")
  # df_info |> glimpse(); select0 |> print()
  df_info    <- df_info |> filter(sector %in% sectors)
  df_info    <- df_info |> select(-any_of(drop0))
  df_info    <- df_info |> distinct()
  rm(drop0)

  ### Model Types
  types0     <- df_info |> pull(modelType) |> unique() |> tolower()
  has_slr    <- "slr" %in% types0

  ###### SE Data ######
  ### Format region
  df_info    <- df_info |> mutate(region = region |> str_replace("\\.", ""))
  df_info    <- df_info |> mutate(region = region |> str_replace(" ", ""))

  ###### Initialize Results ######
  ### Initialized results: Join sector info with socioeconomic scenario
  # df_se |> glimpse(); df_info |> glimpse(); # df_scalars |> glimpse()
  join0      <- df_info |> names() |> get_matches(df_se |> names())
  df0        <- df_info |> left_join(df_se, by=join0, relationship="many-to-many")
  rm(join0)

  ###### Update Scalar Info ######
  ### Update scalar info
  ### Physical scalars
  df0        <- df0 |> match_scalarValues(conn = conn, scalars=df_scalars, scalarType="physScalar")
  ### Physical adjustment
  df0        <- df0 |> match_scalarValues(conn = conn, scalars=df_scalars, scalarType="physAdj")
  ### Damage adjustment
  df0        <- df0 |> match_scalarValues(conn = conn, scalars=df_scalars, scalarType="damageAdj")
  ### Economic scalar
  # df0 |> glimpse(); df_scalars |> glimpse()
  df0        <- df0 |> match_scalarValues(conn = conn, scalars=df_scalars, scalarType="econScalar")
  ### Economic multiplier
  # df0 |> glimpse(); df_scalars |> glimpse()
  df0        <- df0 |> match_scalarValues(conn = conn, scalars=df_scalars, scalarType="econMultiplier")
  # df0 |> glimpse(); df0 |> pull(region) |> unique() |> print()

  ###### Economic Adjustment Values ######
  ### Get economic adjustment values
  renameAt0  <- c("econMultiplierAdj") |> paste0(c("Name", "Value"))
  renameTo0  <- renameAt0 |> str_replace("Multiplier", "")
  df0        <- df0 |> get_scalarAdjValues(conn = conn, scalars=df_scalars, scalarType0="econMultiplier")
  df0        <- df0 |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)

  ###### Calculate Scalars ######
  ### Calculate scalars
  df0        <- df0 |> calcScalars(elasticity=elasticity)

  ###### SLR Scalars for Years > 2090 ######
  ### Scalars for SLR past 2090
  do_slr     <- has_slr & (maxYr0 > refYear0)
  if(do_slr) {
    ### Separate GCM & SLR values
    slrStr0    <- "slr"
    df_gcm0    <- df0 |> filter((modelType |> tolower()) != slrStr0)
    df_slr0    <- df0 |> filter((modelType |> tolower()) == slrStr0)

    ### Get extended scalars
    df_slr0    <- extend_slrScalars(
      df0        = df_slr0,
      conn       = conn,
      scalars    = df_scalars,
      refYear0   = refYear0,
      elasticity = elasticity
    ) ### End extend_slrScalars

    ### Add results back together
    df0        <- df_gcm0 |> bind_rows(df_slr0)
    df0        <- df0     |> filter(year <= maxYr0)
    rm(slrStr0, df_slr0, df_gcm0)
  } ### End if(do_npd)
  # df0 |> pull(region) |> unique() |> print()

  ###### Arrange ######
  sort0      <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0) |> c("modelType")
  df0        <- df0 |> arrange_at(c(sort0))
  rm(sort0)

  ###### Return ######
  ### Return
  gc()
  return(df0)
}


###### calcScalars ######
### Calculate Scalars
### This function calculates the physical scalar value, the economic scalar value, and their product
### The physical and economic scalars refer to the time series column from which the Annual Sectors tab
###   in the Excel tool draws values.
calcScalars <- function(
    data,      ### Initial results dataframe
    extendSlr  = FALSE, ### Whether to run methods for extending SLR scalars
    elasticity = NULL   ### An elasticity to use to adjust values
){
  ###### Adjust Elasticity for VSL ######
  ### Adjust Elasticity for VSL only
  hasVal <- !(elasticity |> is.null())
  if(hasVal){
    data <- data |> mutate(exp0 = case_when(
      econScalarName == "vsl_usd" ~ elasticity,
      .default = exp0
    ))
  } ### End if(!(elasticity |> is.null()))


  ###### Physical Scalar ######
  ### Calculate physical scalar values
  if(extendSlr) {
    data   <- data |> mutate(physScalar = c2 * physScalarValue / physAdjValue)
  } else{
    data   <- data |> mutate(physScalar = physScalarValue * physAdjValue * damageAdjValue)
  } ### End if(extendSlr)


  ###### Economic Scalar ######
  ### Calculate economic multiplier and scalar
  ### Economic multipliers are the economic multiplier value divided by the adjustment
  ### - The economic multiplier value is 1, GDP, or GDP per capita
  ### - The economic adjustment value is usually the economic multiplier value at a reference year
  ### The economic scalar is calculated using the following equation.
  ### - Constants c0, c1, and exp0 are from the impactTypes data frame
  data   <- data |> mutate(econMultiplier = (econMultiplierValue / econAdjValue)**exp0 )
  data   <- data |> mutate(econScalar     = c0 + c1 * econScalarValue * econMultiplier )
  # } ### End if(extendSlr)


  ###### Physical Economic Scalar ######
  ### Combine the physical and economic scalar.
  if(extendSlr) {
    data   <- data |> mutate(physEconScalar = econScalar + physScalar)
  } else{
    data   <- data |> mutate(physEconScalar = econScalar * physScalar)
  } ### End if(extendSlr)


  ###### Return ######
  gc()
  return(data)
}

###### get_gcmScaledImpacts ######
get_gcmScaledImpacts <- function(
    df0, ### Tibble of initial results for GCM sectors, with scenario ID
    df1, ### Tibble of drivers
    conn, ### DB Connection Object
    msg0 = "\t"
){
  ###### Messaging
  msg1       <- msg0 |> paste0("\t")
  paste0(msg1, "Calculating temperature-driven scaled impacts", "...") |> message()


  ### State Columns
  stateCols0 <- c("state", "postal")

  ### Ensure that data is filtered to the correct model
  df0        <- df0 |> filter(modelType == "gcm")
  df1        <- df1 |> filter(modelType == "gcm")

  ### Drivers
  xVar0      <- df1 |> pull(modelUnitValue)
  years0     <- df1 |> pull(year)

  ### Get list of unique impact functions
  #funList0 <- "gcmImpFuncs" |> get_frediDataObj("stateData")
  stateDat   <- DBI::dbReadTable(conn,"stateData")
  stateDat   <- unserialize(stateDat$value |> unlist())
  funList0   <- stateDat |> pluck("gcmImpFuncs")

  funNames0  <- funList0 |> names()
  scenarios0 <- df0 |> pull(scenario_id) |> unique()
  df0        <- df0      |> mutate(hasScenario = (scenario_id %in% funNames0))

  ### Check whether the scenario has an impact function (scenarios with all missing values have no functions)
  gcmFuns0   <- df0 |> filter( hasScenario)
  gcmNoFuns0 <- df0 |> filter(!hasScenario)
  rm(df0)

  ### Which values have functions
  hasFuns0   <- gcmFuns0   |> nrow()
  hasNoFuns0 <- gcmNoFuns0 |> nrow()
  # hasFuns0 |> c(hasNoFuns0) |> print()

  ### Initialize results
  df0        <- tibble()
  renameAt0  <- c("xVar")
  renameTo0  <- c("modelUnitValue")
  select0    <- c("scenario_id", "year") |> c(renameTo0) |> c("scaled_impacts")

  ### Get impacts for scenario_ids that have functions
  if(hasFuns0) {
    ### Subset impact list
    gcmFuns0   <- gcmFuns0 |> pull(scenario_id) |> unique()
    funList0   <- funList0 |> (function(x){x[(x |> names()) %in% gcmFuns0]})()
    ### Get impacts
    df_hasFuns <- funList0   |> interpolate_impacts(xVar=xVar0, years=years0)
    df_hasFuns <- df_hasFuns |> rename_at(c(renameAt0), ~renameTo0)
    df_hasFuns <- df_hasFuns |> select(all_of(select0))
    df0        <- df0        |> rbind(df_hasFuns)
    rm(funList0, df_hasFuns)
  } ### End if(hasFuns0)

  ### For groups that don't have functions, create a tibble with na values
  if(hasNoFuns0) {
    gcmNoFuns0  <- gcmNoFuns0 |> mutate(scaled_impacts = NA)
    gcmNoFuns0  <- gcmNoFuns0 |> select(all_of(select0))
    # df0 |> names() |> print(); gcmNoFuns0 |> names() |> print()
    ### Bind values
    df0         <- df0 |> rbind(gcmNoFuns0)
    rm(gcmNoFuns0)
  } ### End if(hasNoFuns0)

  ### Arrange
  arrange0    <- c("scenario_id", "year")
  df0         <- df0 |> arrange_at(c(arrange0))
  # df0 |> glimpse()
  ### Return
  gc()
  return(df0)
}

###### get_slrScaledImpacts ######
get_slrScaledImpacts <- function(
    df0, ### Initial results for SLR sectors
    df1, ### Driver data frames
    conn,### DB Connection Object
    msg0 = "\t"
){
  ###### Messaging
  msg1       <- msg0 |> paste0("\t")
  paste0(msg1, "Calculating SLR-driven scaled impacts", "...") |> message()

  ####### Columns & Values ######
  ### State columns
  stateCols0 <- c("state", "postal")

  ###### Get rDataList Objects #######
  ### Get objects from rDataLsit
  #df_ext0    <- "slrExtremes" |> get_frediDataObj("stateData")
  #df_imp0    <- "slrImpacts"  |> get_frediDataObj("stateData")
  stateDat   <- DBI::dbReadTable(conn,"stateData")
  stateDat   <- unserialize(stateDat$value |> unlist())
  df_ext0    <- stateDat |> pluck("slrExtremes")
  df_imp0    <- stateDat |> pluck("slrImpacts")
  ###### Format Data #######
  ### Filter to appropriate model types
  ### Filter to appropriate driver values
  df0        <- df0 |> filter(modelType == "slr")
  df1        <- df1 |> filter(modelType == "slr")

  ### Grab info
  select0    <- c("modelUnitValue", "year")
  df1        <- df1 |> select(all_of(select0))
  rm(select0)

  ### Get max model value, max year, unique sectors, and scenario IDS
  slrMax0    <- df0 |> pull(modelMaxOutput) |> unique()
  sectors0   <- df0 |> pull(sector) |> unique()
  maxYear0   <- df0 |> pull(year  ) |> max()

  ### Filter to appropriate sectors
  df_ext0    <- df_ext0 |> filter(sector %in% sectors0)
  df_imp0    <- df_imp0 |> filter(sector %in% sectors0)

  ### Filter to appropriate years
  df_ext0    <- df_ext0 |> filter(year <= maxYear0)
  df_imp0    <- df_imp0 |> filter(year <= maxYear0)

  ### Add in model type
  df_ext0    <- df_ext0 |> mutate(modelType = "slr")
  df_imp0    <- df_imp0 |> mutate(modelType = "slr")

  ### Filter to values with scenarios
  include0   <- c("region") |> c(stateCols0) |> c("model2")
  df_imp0    <- df_imp0 |> mutate(model2 = "Interpolation")
  df_imp0    <- df_imp0 |> get_scenario_id(include=include0) |> ungroup()
  df_imp0    <- df_imp0 |> select(-c("model2"))
  # df_imp0 |> pull(scenario_id) |> unique() |> head() |> print(); df0 |> pull(scenario_id) |> unique() |> head() |> print()
  groups0    <- df_imp0 |> filter(!(scaled_impacts |> is.na())) |> pull(scenario_id) |> unique()
  df_imp0    <- df_imp0 |> mutate(hasScenario = (scenario_id %in% groups0) |> as.numeric())
  rm(include0, groups0)

  ### Figure out which values have a scenario
  scenarios0 <- df_imp0 |> filter(hasScenario == 1) |> pull(scenario_id) |> unique()
  df_imp0    <- df_imp0 |> filter(hasScenario == 1)
  df0        <- df0     |> mutate(hasScenario = (scenario_id %in% scenarios0) |> as.numeric())
  scenarios1 <- df0     |> pull(scenario_id) |> unique()

  ### Filter to values with a scenario
  df_imp0    <- df_imp0 |> filter(scenario_id %in% scenarios1)


  ### Check whether the scenario has an impact function (scenarios with all missing values have no functions)
  df_hasSlr  <- df0 |> filter(hasScenario == 1)
  df_noSlr   <- df0 |> filter(hasScenario != 1)
  rm(df0)

  ### Mutate values
  df_hasSlr  <- df_hasSlr |> mutate(model = "Interpolation")
  df_noSlr   <- df_noSlr  |> mutate(model = "Interpolation")

  ### Get distinct values
  df_hasSlr  <- df_hasSlr |> distinct()
  df_noSlr   <- df_noSlr  |> distinct()

  ### Get scenario ID
  include0   <- c("region") |> c(stateCols0) |> c("model")
  df_hasSlr  <- df_hasSlr |> get_scenario_id(include=include0) |> ungroup()
  df_noSlr   <- df_noSlr  |> get_scenario_id(include=include0) |> ungroup()

  ### Which values have functions
  hasSlr0    <- df_hasSlr |> nrow()
  hasNoSlr0  <- df_noSlr  |> nrow()
  # df_ext0 |> glimpse(); df_imp0 |> glimpse(); df1 |> glimpse(); df_hasSlr |> glimpse(); df_noSlr |> dim()

  ### Initialize impacts
  df0        <- tibble()

  ###### Join Data ######
  if(hasSlr0) {
    # ### Join with df1
    # df_ext0 |> glimpse(); df_imp0 |> glimpse(); df1 |> glimpse()
    join0       <- c("year")
    df_ext0     <- df1 |> left_join(df_ext0, by=join0)
    df_imp0     <- df1 |> left_join(df_imp0, by=join0)
    # df_ext0 |> glimpse(); df_imp0 |> glimpse();

    ###### Scaled Impacts >= Max
    ### Filter to appropriate years
    df_max0     <- df_ext0 |> filter(modelUnitValue >= driverValue_ref)
    maxYrs0     <- df_max0 |> pull(year) |> unique()
    nrow_max    <- maxYrs0 |> length()
    rm(df_ext0)

    ### Calculate scaled impacts for values > slrMax0
    if(nrow_max) {
      df_max0  <- df_max0 |> mutate(deltaDriver    = modelUnitValue    - driverValue_ref)
      df_max0  <- df_max0 |> mutate(scaled_impacts = impacts_intercept + impacts_slope * deltaDriver)
    } else{
      df_max0  <- df_max0 |> mutate(scaled_impacts = NA)
    } ### End if(nrow_max)

    ###### Scaled Impacts < Max
    ### Get impacts and create scenario ID for values <= slrMax0
    ### Join with slrImpacts
    df_slr0     <- df_imp0 |> filter(!(year %in% maxYrs0))
    nrow_slr    <- df_slr0 |> nrow()
    rm(df_imp0)

    # ### Group by cols
    # cols0       <- c("modelUnitValue")
    # cols1       <- c("driverValue")
    ### Group by cols
    slr_names   <- df_slr0 |> names()
    # select0     <- c("scenario_id")
    # group0      <- select0 |> (function(x){x[x %in% (df_slr0 |> names())]})()
    # group0      <- group0  |> c(cols1)
    group0      <- c("scenario_id") |> get_matches(y=df_slr0 |> names(), matches=TRUE)
    group0      <- group0  |> c("modelUnitValue")

    ### Calculate impacts for values not above the maximum value
    # nrow_oth |> print()
    if(nrow_slr) {
      #### Interpolate driver values
      mutate0   <- c("lower_model", "upper_model")
      # slrVals0 <- df1      |> rename_at(c(cols0), ~cols1)
      # slrVals0 <- slrVals0 |> interp_slr_byYear()
      slrVals0 <- df1      |> interp_slr_byYear(conn = conn, yCol="modelUnitValue")
      slrVals0 <- slrVals0 |> mutate_at(c(mutate0), function(y){y |> str_replace(" ", "")})

      ### Interpolate
      # df_slr0 |> glimpse()
      df_slr0  <- df_slr0 |> fredi_slrInterp(slr_x=slrVals0, groupByCols=group0)
      # rm(group0, slr_names, slrVals0); rm(cols0, cols1)
    } else{
      df_slr0  <- df_slr0 |> mutate(scaled_impacts = NA)
    } ### End if(nrow_oth)

    ### Mutate model
    df_slr0   <- df_slr0 |> mutate(model = "Interpolation")
    df_max0   <- df_max0 |> mutate(model = "Interpolation")

    ### Calculate scenario ID
    include0   <- c("region") |> c(stateCols0) |> c("model")
    df_slr0    <- df_slr0 |> get_scenario_id(include=include0) |> ungroup()
    df_max0    <- df_max0 |> get_scenario_id(include=include0) |> ungroup()

    ### Select columns & bind values
    # df_slr0 |> glimpse(); df_max0 |> glimpse()
    select0   <- c("scenario_id", "year") |> c("scaled_impacts")
    df_slr0   <- df_slr0 |> select(all_of(select0))
    df_max0   <- df_max0 |> select(all_of(select0))
    df_slr0   <- df_slr0 |> rbind(df_max0)
    rm(select0, df_max0)

    ### Join and bind values
    join0     <- c("scenario_id", "year")
    df_hasSlr <- df_hasSlr |> left_join(df_slr0, by=join0)
    df0       <- df0       |> rbind(df_hasSlr)
    rm(join0, df_slr0, df_hasSlr)
  } ### End if(hasSlr0)

  if(hasNoSlr0) {
    ### Mutate values
    ### Join and bind values
    join0     <- c("scenario_id", "year")
    df_noSlr  <- df_noSlr |> mutate(scaled_impacts = NA)
    df0       <- df0      |> rbind(df_noSlr)
    rm(join0, df_noSlr)
  } ### End if(hasNoSlr0)

  ###### Arrange ######
  arrange0    <- c("scenario_id", "year")
  df0         <- df0 |> arrange_at(c(arrange0))

  ###### Return ######
  gc()
  return(df0)
}

####### calc_scaled_impacts_fredi ######
calc_scaled_impacts_fredi <- function(
    sectors0, ### Sector IDs
    drivers0,  ### Tibble with driver scenarios
    conn ### DB Connection Object
){
  ### Column names
  popCol0    <- "pop"
  stateCols0 <- c("state", "postal")

  ###### ** Get Scenario IDs ######
  ### Mutate model for SLR sectors
  select0    <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0) |> c("modelType", "model")
  drop0      <- c("maxUnitValue")
  mutate0    <- c("model", "model_label")
  df_info0   <- DBI::dbReadTable(conn,"co_sectorsInfo")
  df_info0   <- df_info0 |> filter(sector %in% sectors0)
  df_info0   <- df_info0 |> select(-any_of(drop0))

  # df_info0 |> pull(region) |> unique() |> print()
  gcmInfo0   <- df_info0 |> filter(modelType == "gcm")
  slrInfo0   <- df_info0 |> filter(modelType == "slr") |>
    mutate_at(mutate0, function(x){"Interpolation"}) |>
    distinct()
  rm(select0, mutate0, drop0, df_info0)

  ### Create scenario ID and separate by model type
  include0   <- c("region") |> c(stateCols0) |> c("model")
  gcmInfo0   <- gcmInfo0 |> get_scenario_id(include = include0)
  slrInfo0   <- slrInfo0 |> get_scenario_id(include = include0)
  df_info0   <- gcmInfo0 |> rbind(slrInfo0)
  rm(include0)

  ### Join with driver info
  join0      <- drivers0 |> names() |> get_matches(y=gcmInfo0 |> names())
  gcmInfo0   <- gcmInfo0 |> left_join(drivers0, by=join0)
  slrInfo0   <- slrInfo0 |> left_join(drivers0, by=join0)

  ### Number of GCM and SLR rows
  nrow_gcm   <- gcmInfo0 |> nrow()
  nrow_slr   <- slrInfo0 |> nrow()

  ###### ** Calculate Scaled Impacts  ######
  ### Initialize and empty data frame df_results
  dfImpacts0 <- tibble()
  dfGroups0  <- tibble()

  ### ** -- GCM Scaled Impacts
  ### Calculate scaled impacts, then bind impacts and info
  if(nrow_gcm) {
    select0    <- c("scenario_id", "year") |> c("scaled_impacts")
    df_gcm0    <- gcmInfo0   |> get_gcmScaledImpacts(conn = conn, df1=drivers0)
    df_gcm0    <- df_gcm0    |> select(all_of(select0))
    dfImpacts0 <- dfImpacts0 |> rbind(df_gcm0)
    dfGroups0  <- dfGroups0  |> rbind(gcmInfo0)
    rm(df_gcm0, gcmInfo0)
  } ### End if(nrow_gcm)

  ###### ** -- SLR Scaled Impacts
  if(nrow_slr){
    select0    <- c("scenario_id", "year") |> c("scaled_impacts")
    df_slr0    <- slrInfo0   |> get_slrScaledImpacts(conn = conn, df1=drivers0)
    # dfImpacts0 |> glimpse(); df_slr0 |> glimpse()
    df_slr0    <- df_slr0    |> select(all_of(select0))
    # df_slr0 |> filter(!(scaled_impacts |> is.na())) |> glimpse()
    dfImpacts0 <- dfImpacts0 |> rbind(df_slr0)
    dfGroups0  <- dfGroups0  |> rbind(slrInfo0)
    # df_slr0 |> filter(!(scaled_impacts |> is.na())) |> glimpse()
    rm(df_slr0, slrInfo0)
  } ### End if(nrow_slr)
  # dfImpacts0 |> glimpse(); dfGroups0 |> glimpse()

  ### Join results with initialized results and update missing observations with NA
  ### Drop columns, then join with scenario results
  join0       <- c("scenario_id", "year")
  select0     <- join0 |> c("scaled_impacts")
  # dfImpacts0 |> glimpse(); dfGroups0 |> glimpse()
  dfImpacts0  <- dfImpacts0 |> select(all_of(select0))
  df0         <- dfGroups0  |> left_join(dfImpacts0, by=join0)
  rm(dfGroups0, dfImpacts0, select0, join0)

  ###### Return ######
  gc()
  return(df0)
}

####### calc_impacts_fredi ######
calc_impacts_fredi <- function(
    df0, ### Tibble with scalars/initialized results
    df1,  ### Tibble with scaled impacts
    conn### DB Connection Object
){
  ### Column names
  stateCols0 <- c("state", "postal")
  sectors0   <- df0 |> pull(sector) |> unique()

  ### Get info
  df_info    <- sectors0 |> get_co_sectorsInfo(
    conn = conn,
    addRegions = FALSE,   ### Whether to include regions & states
    addModels  = TRUE ,   ### Whether to include models
    colTypes   = c("ids") ### Types of columns to include: IDs, labels, or extra. If only labels, will return labels without the "_label"
  ) ### End get_co_sectorsInfo()

  ### Mutate info
  mutate0    <- c("model")
  gcmInfo0   <- df_info |> filter(modelType == "gcm")
  slrInfo0   <- df_info |> filter(modelType == "slr") |>
    mutate_at(mutate0, function(x){"Interpolation"}) |>
    distinct()
  rm(df_info)
  df_info    <- gcmInfo0 |> rbind(slrInfo0)
  # df_info |> pull(region) |> unique() |> print()

  ### Add model info to df0
  # df_info |> glimpse(); df0 |> filter(year > 2100) |> glimpse()
  join0      <- c("sector", "variant", "impactType", "impactYear", "modelType")
  df0        <- df0 |> left_join(df_info, by=join0, relationship="many-to-many")
  rm(join0, df_info)

  ### Add scenario ID
  include0   <- c("region") |> c(stateCols0) |> c("model")
  df0        <- df0 |> get_scenario_id(include = include0)

  ### Join results with initialized results and update missing observations with NA
  join0    <- c("scenario_id", "year")
  drop0    <- df0 |> names() |> get_matches(y=df1 |> names()) |> get_matches(y=join0, matches=FALSE)
  # drop0 |> print()
  # df0      <- df0 |> select(-all_of(drop0))
  df1      <- df1 |> select(-all_of(drop0))
  df0      <- df1 |> left_join(df0, by=join0)
  # df0      <- df1 |> left_join(df0, by=join0)
  rm(df1)
  # df0 |> filter(year > 2100) |> glimpse()
  # df0 |> dim() |> print();

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





###### interpolate_impacts ######
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
  scaledImpacts_x   <- numFunctions |> seq_len() |> map(function(i){
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
  gc()
  return(scaledImpacts_x)
}

###### get_annual_model_stats  ######
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
  sectors0    <- data |> pull(sector) |> as.character() |> unique()
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
  modelType0  <- data |> pull(model_type) |> unique() |> tolower()
  modelType0  <- modelType0[1]
  modelLbl0   <- (modelType0=="gcm") |> ifelse(modelAves0[1], modelAves0[2])

  ###### Reshape the data ######
  groupCols0  <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0)
  groupCols0  <- groupCols0 |> c("model", "model_type")
  groupCols0  <- groupCols0 |> c("year", "yvar")
  groupByCols <- groupCols0 |> get_matches(y = data |> names())

  ### Reshape the data and prepare a column indicating which rows have is.na() for all models
  data        <- data |> select(all_of(groupByCols))
  data        <- data |> mutate(notNA = !(yvar |> is.na()))

  ###### Summarize by group columns ######
  ### Add group column with year
  group0      <- groupByCols |> get_matches(y = c("model", "yvar"), matches=FALSE)
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
      group_by_at (c(groupByCols)) |>
      summarize_at(c(sum0), df_sum0, na.rm=T) |> ungroup()
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
  gc()
  return(df_results)
}


###### interp_slr_byYear ######
### utils for aggregate_impacts
interp_slr_byYear <- function(
    data,  ### Driver scenario with columns year, slr_cm
    conn,  ### DB connection object
    yCol   = "driverValue", ### Column to look for the driver value
    silent = TRUE
){
  ###### Defaults ######
  ### Rename y Column
  oldColName_y <- yCol |> c()
  newColName_y <- "yValue" |> c()
  newColRef_y  <- newColName_y |> paste0("_ref")
  data         <- data |> rename_at(c(oldColName_y), ~newColName_y)
  ### Messaging
  if(silent |> is.null()){silent <- T}
  msgUser    <- !silent

  ###### Get Data Objects ######
  #co_models  <- "co_models" |> get_frediDataObj("frediData")
  #slr_df     <- "slr_cm"    |> get_frediDataObj("frediData")
  co_models  <- DBI::dbReadTable(conn, "co_models")
  slr_df     <- DBI::dbReadTable(conn, "slr_cm")

  ###### Assign data ######
  ### SLR scenario info
  co_slrs    <- co_models |> filter(modelType=="slr") |> rename(model=model_label)
  slr_levels <- c("0cm" ) |> c(co_slrs |> pull(model_id))
  slr_labels <- c("0 cm") |> c(co_slrs |> pull(model))
  slr_orders <- slr_levels |> factor(levels=slr_levels) |> as.numeric()
  slr_min    <- (slr_orders |> min(na.rm=T)) #+ 1
  slr_max    <-  slr_orders |> max(na.rm=T)

  ### Sea level rise information
  # assign("slr_df", rDataList[["slr_cm"]])
  df_slr_years <- slr_df |> pull(year) |> unique()
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
  data_years   <- data |> pull(year) |> unique()
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
  dataYrs <- data |> pull(year)

  ###### Standard Columns ######
  ### JoinCols
  join0   <- c("year", "model_level")
  select0 <- c("year") |> c(newColName_y, newColRef_y) |> c("model")
  select1 <- c("year") |> c(newColName_y) |> c("lower_model", "upper_model", "lower_slr", "upper_slr")
  ### Format data
  slr_df  <- slr_df |> rename(yValue_ref = driverValue)
  ### Join
  # df_new  <- data |> left_join(slr_df, by = c("year", "modelType"))
  df_new  <- data |> left_join(slr_df, by = c("year"))
  ### Filter to specific observations
  df_lo   <- df_new |> filter(yValue_ref <= yValue)
  df_hi   <- df_new |> filter(yValue_ref >= yValue)
  ### Get unique years
  yrs_lo  <- df_lo |> pull(year) |> unique() |> sort()
  yrs_hi  <- df_hi |> pull(year) |> unique() |> sort()
  ### Figure out which years are missing
  nas_lo  <- dataYrs[!(dataYrs %in% yrs_lo)]
  nas_hi  <- dataYrs[!(dataYrs %in% yrs_hi)]
  ### Create tibbles of missing years
  dfNaLo  <- tibble(year = yrs_lo, model_level = slr_min) |> left_join(df_new, by = join0)
  dfNaHi  <- tibble(year = yrs_lo, model_level = slr_max) |> left_join(df_new, by = join0)
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
  df_lo <- data  |> left_join(df_lo, by=join0)
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
  df_hi <- data  |> left_join(df_hi, by=join0)
  df_hi <- df_hi |> left_join(slr_df , by=c(join1))
  ### - Select & rename
  df_hi <- df_hi |> select(all_of(select0))
  df_hi <- df_hi |> rename_at(c(rename0), ~rename2)
  ### Remove values
  rm(group0, sum0, join1, rename0, rename1, rename2)
  rm(slr_df)

  ### Join all
  join0 <- c("year") |> c(newColName_y)
  data  <- df_lo |> left_join(df_hi, by=join0)
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

  ### Return
  gc()
  return(data)

}



####### fun_getNeighbors ######
### Figure out which SLR heights are immediately above and below a driver value
fun_getNeighbors <- function(
    x,      ### X values
    values, ### values to compare
    col = "driverValue" ### Which column to compare
){

  ### Mutate data
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

  ### Filter values
  lo_values    <- values |> filter(order==lo_order) |> mutate(type="lower")
  hi_values    <- values |> filter(order==hi_order) |> mutate(type="upper")
  new_values   <- lo_values |> rbind(hi_values)
  new_values   <- lo_values |> rbind(hi_values)

  ### Return
  gc()
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
  gc()
  return(return_list)
}

####### fredi_slrInterp ######
fredi_slrInterp <- function(
    data_x,
    slr_x, ### slrScenario
    groupByCols
){
  names_slr      <- data_x |> names()
  ### Summary columns
  slrSumCols     <- c("scaled_impacts")
  n_slrSumCols   <- slrSumCols |> length()
  bounds0        <- c("lower", "upper")
  mutate0        <- c("model", "slr")
  slrMutCols     <- c("lower_model", "upper_model")

  ### Info names
  data_xAdj      <- slr_x
  names_slrAdj   <- data_xAdj |> names()
  rm(slr_x)

  # other_slrCols  <- names_slrAdj |> (function(x){x[!(x %in% c("year"))]})()
  other_slrCols  <- names_slrAdj |> get_matches(y=c("year"), matches=FALSE)
  join_slrCols   <- groupByCols |> c("year") ### sectorprimary, includeaggregate
  ### Other columns
  # dropCols0      <- c("model", "model_label")
  dropCols0      <- c("model")
  otherCols0     <- c("modelType", "denom_slr", "numer_slr", "adj_slr")
  # joinCols1      <- join_slrCols |> (function(x){x[!(x %in% dropCols0)]})()
  joinCols1      <- join_slrCols |> get_matches(y=dropCols0, matches=FALSE)
  joinCols1      <- joinCols1 |> c(paste0("_", "model"))
  joinCols1      <- joinCols1 |> c(paste0("_", "slr"  ))
  joinCols1      <- joinCols1 |> c(otherCols0)

  ### Format values
  data_xAdj      <- data_xAdj |> mutate_at(c(slrMutCols), as.character)

  ### Join with slrInfo and convert columns to character
  # join0          <- c("year", "modelType")
  join0          <- c("year")
  data_xAdj      <- data_xAdj |> mutate(equal_models = lower_model == upper_model)
  data_x         <- data_x    |> left_join(data_xAdj, by=join0)
  rm(join0, data_xAdj)

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
    data_xEqual0 <- data_xEqual |> filter(lower_model=="0cm") |> filter(model=="30cm")
    data_xEqual1 <- data_xEqual |> filter(lower_model!="0cm") |> filter(model==lower_model)
    rm(data_xEqual)
    ### For observations that are zeros only and make the summary column values zero
    mutate0      <- slrSumCols
    data_xEqual0 <- data_xEqual0 |> mutate_at(c(mutate0), function(y){0})
    rm(mutate0)
    ### Bind values back together
    data_xEqual  <- data_xEqual0 |> rbind(data_xEqual1)
    rm(data_xEqual0, data_xEqual1)
    ### Mutate model
    data_xEqual  <- data_xEqual |> mutate(model="Interpolation")
    ### Select appropriate columns
    select0      <- names_slr
    drop0        <- dropCols0
    data_xEqual  <- data_xEqual |> select(all_of(select0))
    data_xEqual  <- data_xEqual |> select(-all_of(drop0 ))
    rm(select0, drop0)
  } ### End if length(which_equal) > 0

  ### Observations that are greater than zero
  if(hasOther){
    ### Lower and upper column names and new names
    lowerSumCols   <- slrSumCols |> paste0("_", "lower")
    upperSumCols   <- slrSumCols |> paste0("_", "upper")
    ### Filter lower model observations to those with a lower model value == "0 cm" and others and drop model column
    data_xLower0   <- data_xOther  |> filter(lower_model=="0cm")
    data_xLower1   <- data_xOther  |> filter(lower_model!="0cm")
    data_xUpper    <- data_xOther |> filter(model==upper_model)
    rm(data_xOther)
    ### Rename columns
    data_xLower0   <- data_xLower0 |> rename_at(c(slrSumCols), ~lowerSumCols)
    data_xLower1   <- data_xLower1 |> rename_at(c(slrSumCols), ~lowerSumCols)
    data_xUpper    <- data_xUpper  |> rename_at(c(slrSumCols), ~upperSumCols)

    ### Convert values for observations with a lower model value =="0 cm" to zero then filter to lower models
    data_xLower0   <- data_xLower0 |> mutate_at(c(lowerSumCols), function(y){0})
    data_xLower0   <- data_xLower0 |> filter(model=="30cm")
    data_xLower1   <- data_xLower1 |> filter(model==lower_model)
    data_xLower    <- data_xLower0 |> rbind(data_xLower1)
    rm(data_xLower0, data_xLower1)

    ### Drop columns
    # data_xLower |> glimpse()
    data_xLower    <- data_xLower |> select(-all_of(dropCols0))
    data_xUpper    <- data_xUpper |> select(-all_of(dropCols0))

    ### Join upper and lower data frames
    # join0          <- data_xLower |> names() |> (function(x){x[!(x %in% c(lowerSumCols))]})()
    join0          <- data_xLower |> names() |> get_matches(y=lowerSumCols, matches=FALSE)
    data_xOther    <- data_xLower |> left_join(data_xUpper, by = join0)
    rm(data_xLower, data_xUpper)

    ### Calculate the new value
    # data_xOther |> names() |> print()
    slrLowerVals  <- data_xOther[, lowerSumCols]
    slrUpperVals  <- data_xOther[, upperSumCols]
    slrOtherAdj   <- data_xOther |> pull(adj_slr)
    slrNewFactors <- (slrUpperVals - slrLowerVals) * (1 - slrOtherAdj)
    slrNewValues  <-  slrLowerVals + slrNewFactors
    data_xOther[,slrSumCols] <- slrNewValues
    rm(slrLowerVals, slrUpperVals, slrOtherAdj, slrNewFactors, slrNewValues)
    rm(lowerSumCols, upperSumCols)

    ### When finished, drop columns and mutate model column
    data_xOther <- data_xOther |> select( any_of(names_slr))
    data_xOther <- data_xOther |> select(-any_of(dropCols0))
  } ### End if (nrow(data_xOther) > 0)

  ### Bind SLR averages together
  data_x <- data_xEqual |> rbind(data_xOther)
  rm(data_xEqual, data_xOther)

  ### Return
  gc()
  return(data_x)
}


###### combine_driverScenarios ######
combine_driverScenarios <- function(
    list0, ### List of driver scenarios
    info0, ### Dataframe with scenario info, e.g.: df_inputInfo
    info1 = DBI::dbReadTable(conn,"co_modelTypes")
){
  ### Rename columns
  names0   <- list0 |> names()
  infoX    <- info0
  info0    <- infoX |> filter(inputName %in% names0)
  cols0    <- info0 |> pull(valueCol)
  renameTo <- c("modelUnitValue")
  df0      <- list(df_i=list0, col_i=cols0) |> pmap(function(df_i, col_i){
    df_i |> rename_at(c(col_i), ~renameTo)
  }) |> bind_rows(.id="driverName")
  rm(list0, renameTo)

  ### Select info and rename
  rename0  <- c("inputName", "inputUnit")
  renameTo <- c("driverName", "modelUnit_label")
  info0    <- info0 |> rename_at(c(rename0), ~renameTo)
  info0    <- info0 |> select(all_of(renameTo))
  rm(rename0, renameTo)

  ### Select model info
  rename0  <- c("modelType_id")
  renameTo <- c("modelType")
  info1    <- info1 |> rename_at(c(rename0), ~renameTo)
  # info0 |> glimpse(); info1 |> glimpse(); df0 |> glimpse()
  rm(rename0, renameTo)

  ### Join info
  join0    <- c("driverName")
  join1    <- c("modelUnit_label")
  df0      <- df0 |> left_join(info0, by=join0)
  df0      <- df0 |> left_join(info1, by=c(join1))

  ### Return
  gc()
  return(df0)
}
