###### Documentation ######
#' Project annual average impacts from methane, NOx, and ozone.
#'
#'
#'
#' @description
#' This function allows users to estimate impacts from changes to methane, NOx, and ozone. Additional documentation to follow.

###### run_fredi_methane ######
### This function creates a data frame of sector impacts for default values or scenario inputs.
### run_fredi relies on the following helper functions: "interpolate_annual", "match_scalarValues","get_econAdjValues" , "calcScalars", "interpolate_tempBin"
run_fredi_methane <- function(
    inputsList = list(o3 = NULL, ch4=NULL, nox=NULL, gdp=NULL, pop=NULL), ### List of inputs
    elasticity = 1,    ### Override value for elasticity for economic values
    maxYear    = 2100,
    thru2300   = FALSE,
    outputList = FALSE, ### Whether to return input arguments as well as results. [If TRUE], returns a list instead of a data frame
    allCols    = FALSE, ### Whether to include additional columns in output
    silent     = TRUE   ### Whether to message the user
){
  ###### Load Objects ######
  ### Assign data objects to objects in this namespace
  ### Assign FrEDI config
  fredi_config <- rDataList[["fredi_config"]]
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}


  ###### Set up the environment ######
  ### Level of messaging (default is to message the user)
  msgUser   <- !silent
  ### Uncomment for allCols
  # doPrimary <- F  ### whether to filter to primary impacts
  ### Model years and NPD (FrEDI past 2100)
  minYear   <- listMethane[["package"]][["coefficients"]][["minYear0"]]
  maxYear   <- thru2300 |> ifelse(npdYear0, maxYear)
  do_npd    <- maxYear > maxYear0
  # "got here2" |> print()

  ###### ** Return List ######
  ### Initialize list to return
  returnList <- list() ### List to return args, scenarios, and statuses
  argsList   <- list() ### List of arguments
  statusList <- list() ### List to return custom or default

  ### Initialize return list: add scenarios
  if(outputList) {returnList[["scenarios"]] <- list()}


  ### Initialize status list
  ### Add statuses:
  ### - inputsList items and aggLevels assessed further below
  ### Add to list
  if(outputList){
    statusList[["inputsList"]] <- inputsList
    statusList[["elasticity"]] <- (elasticity == 1) |> get_returnListStatus()
    statusList[["maxYear"   ]] <- (maxYear == maxYear0 & !thru2300) |> get_returnListStatus()
    statusList[["thru2300"  ]] <- (!thru2300) |> get_returnListStatus()
    statusList[["allCols"   ]] <- (!allCols ) |> get_returnListStatus()
    statusList[["silent"    ]] <- ( silent  ) |> get_returnListStatus()
  } ### End if(outputList)


  ### Initialize arguments list
  ### - inputsList items, sectorList, and aggLevels assessed further below
  if(outputList){
    argsList[["inputsList"]] <- inputsList
    argsList[["elasticity"]] <- elasticity
    argsList[["maxYear"   ]] <- maxYear
    argsList[["thru2300"  ]] <- thru2300
    argsList[["allCols"   ]] <- allCols
    argsList[["silent"    ]] <- silent
  } ### End if(outputList)



  ###### ** Elasticity ######
  ### Message user about elasticity
  has_elasticity <- elasticity     |> is.numeric()
  elasticity     <- has_elasticity |> ifelse(elasticity, elasticity0)
  if(!has_elasticity){
    paste0("\t", "Incorrect value type provided for argument 'elasticity'...") |> message()
    paste0("\t\t", "Using default elasticity values.") |> message()
  } ### End if
  rm(has_elasticity, elasticity0)

  ###### ** State Info ######
  byState        <- TRUE
  popCol0        <- "pop"
  stateCols0     <- c("state", "postal")




  ###### Scenarios ######
  ###### ** Check Inputs ######
  paste0("Checking scenarios...") |> message()
  ### Add info to data
  df_inputInfo <- listMethane[["package"]][["co_inputInfo"]]

  ### Input defaults
  inputDefs    <- list()
  gcamDefault  <- "gcam_default" |> get_frediDataObj("scenarioData")
  inputDefs[["temp"]] <- gcamDefault |> select(c("year", "temp_C_conus")) |> rename_at(c("temp_C_conus"), ~"temp_C")
  inputDefs[["slr" ]] <- gcamDefault |> select(c("year", "slr_cm"))
  inputDefs[["gdp" ]] <- "gdp_default" |> get_frediDataObj("scenarioData")
  inputDefs[["pop" ]] <- "pop_default" |> get_frediDataObj("scenarioData")
  rm(gcamDefault)

  ### Input info
  inNames      <- df_inputInfo |> pull(inputName)
  inIDCols     <- get_import_inputs_idCols(popArea="state") |> (function(list0){
    list0[["pop"]] <- c("region", "state", "postal")
    return(list0)
  })()
  # inValCols    <- df_inputInfo |> pull(valueCol) |> str_replace("pop", "state_pop")
  inValCols    <- df_inputInfo |> pull(valueCol)
  inMinYears   <- df_inputInfo |> pull(min_year)
  inMaxYears   <- df_inputInfo |> pull(max_year)


  ### Make sure all inputs are present
  ### Check whether inputs are present
  inputsList   <- inNames     |> paste0("Input") |> map(function(name_i){  inputsList[[name_i]]}) |> set_names(inNames)
  hasInputs    <- inNames     |> map(function(name_i){!(inputsList[[name_i]] |> is.null())}) |> set_names(inNames)
  whichInputs  <- hasInputs   |> unlist()
  hasAnyInputs <- whichInputs |> length()
  # whichInputs |> print()

  ### Create logicals and initialize inputs list
  if(hasAnyInputs) {
    inputsList <- list(
      inputName = inNames,
      inputDf   = inputsList,
      idCol     = inIDCols,
      valCol    = inValCols,
      yearMin   = inMinYears,
      yearMax   = inMaxYears
    ) |>
      pmap(check_input_data) |>
      set_names(inNames)
  } ### if(hasAnyInputs)

  ### Check again for inputs
  hasInputs    <- inNames |> map(function(name_i){!(inputsList[[name_i]] |> is.null())}) |> set_names(inNames)

  ### Update inputs with defaults if values are missing
  for(name_i in inNames) { if(inputsList[[name_i]] |> is.null()) {
    if("slr" %in% name_i) inputsList[[name_i]] <- inputsList[["temp"]]
    else                  inputsList[[name_i]] <- inputDefs[[name_i]]
    rm(name_i)
  } } ### End if, end for

  ### Iterate over list and calculate values
  inputsList   <- list(
    name0     = inNames,
    df0       = inputsList,
    hasInput0 = hasInputs,
    idCols0   = inIDCols,
    valCols0  = inValCols
  ) |> pmap(function(df0, name0, hasInput0, idCols0, valCols0){
    df0 |> format_inputScenarios(
      name0     = name0,
      hasInput0 = hasInput0,
      idCols0   = idCols0,
      valCols0  = valCols0,
      minYear   = minYear,
      maxYear   = maxYear,
      info0     = df_inputInfo
    )
  }) |> set_names(inNames)


  ### For each input:
  ### - Make sure values are at correct range
  ### - Update in status list
  if(outputList){    for(name_i in inNames) {
    name_i1  <- name_i |> paste0("Input")
    inputs_i <- inputsList[[name_i]]
    inputs_i <- inputs_i |> filter(year >= minYear, year <= maxYear)
    inputsList[[name_i]] <- inputs_i
    ### Add lists
    statusList[["inputsList"]][[name_i1]] <- hasInputs[[name_i]] |> get_returnListStatus()
    argsList  [["inputsList"]][[name_i1]] <- inputs_i
    returnList[["scenarios" ]][[name_i ]] <- inputs_i
    rm(name_i)
  } } ### End if(outputList)



  ###### ** Physical Driver Scenario  ######
  ### Select columns
  select0    <- c("inputName")
  filter0    <- c("temp", "slr")
  df_drivers <- inputsList[filter0] |> combine_driverScenarios(info0 = df_inputInfo)
  df_drivers <- df_drivers |> filter(year >= minYear, year <= maxYear)
  # return(df_drivers)

  ###### ** Socioeconomic Driver Scenario ######
  ### Update values
  gdp_df       <- inputsList[["gdp"]]
  pop_df       <- inputsList[["pop"]]
  pop_df       <- pop_df |> mutate(region = region |> str_replace(" ", ""))
  pop_df       <- pop_df |> mutate(region = region |> str_replace("\\.", ""))
  ### ### Subset to desired range
  pop_df       <- pop_df |> filter(year >= minYear, year <= maxYear)
  gdp_df       <- gdp_df |> filter(year >= minYear, year <= maxYear)
  # return(pop_df)
  ### Calculate national population and update national scenario
  seScenario   <- gdp_df |> create_nationalScenario(pop0 = pop_df)

  # return(seScenario)
  # seScenario |> pull(region) |> unique() |> print()
  rm(gdp_df, pop_df)
  # seScenario |> glimpse()

  ###### Calculate Impacts ######
  ###### ** Initialize Impacts Data frame ######
  ### Initialized results: Join sector info and default scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars
  paste0("Calculating impacts...") |> message()
  df_results   <- seScenario |> initialize_resultsDf(sectors=sectorIds, elasticity=elasticity)
  # return(df_results)
  # df_results |> select(c("year", "gdp_usd", "national_pop", "gdp_percap")) |> unique() |> nrow() |> print()
  # return()
  # df_results |> pull(region) |> unique() |> print()
  # df_results |> glimpse()
  # return(df_results)

  ### Get scaled impacts
  df_impacts   <- sectorIds |> calc_scaled_impacts_fredi(drivers0 = df_drivers)
  # df_impacts   <- df_results |> calc_scaled_impacts_fredi(drivers0 = df_drivers)
  # df_impacts |> glimpse()
  # return(df_impacts)
  # return(list(df0=df_results, df1=df_impacts))

  ### Get impacts
  df_results   <- df_results |> calc_impacts_fredi(df1=df_impacts)
  # df_results |> pull(region) |> unique() |> print()
  # df_results   <- df_impacts |> calc_impacts_fredi()
  # return(list(df0=df_results, df1=df_impacts))
  # rm(df_impacts)
  # df_results |> glimpse()
  # df_results |> pull(region) |> unique() |> print()
  # return(df_results)


  ###### Format Results ######
  ### Add in model info
  paste0("Formatting results", "...") |> message()

  ###### ** Get Labels ######
  ### Rename sector
  drop0       <- c("sector", "variant", "impactType", "impactYear", "region", "modelType", "model")
  renameAt0   <- drop0 |> paste0("_label") |> c("modelUnitDesc", "modelUnit_label", "modelUnitValue")
  renameTo0   <- drop0 |> c("driverType", "driverUnit", "driverValue")
  df_results  <- df_results |> select(-any_of(drop0))
  df_results  <- df_results |> rename_at(c(renameAt0), ~renameTo0)
  rm(drop0, renameAt0, renameTo0)
  # df_results |> glimpse()

  ### Rename model type
  renameAt0   <- c("modelType")
  renameTo0   <- c("model_type")
  df_results  <- df_results |> rename_at(c(renameAt0), ~renameTo0)
  rm(renameAt0, renameTo0)
  # df_results |> pull(region) |> unique() |> print()
  # return(df_results)

  ###### ** Columns List ######
  ### Grouping columns
  groupCols0  <- c("sector", "variant", "impactType", "impactYear", "region") |> c(stateCols0)
  groupCols0  <- groupCols0 |> c("model_type", "model")
  groupCols0  <- groupCols0 |> c("sectorprimary", "includeaggregate")
  groupCols0  <- groupCols0 |> c("physicalmeasure")
  groupCols0  <- groupCols0 |> get_matches(y=df_results |> names(), matches=TRUE)
  groupCols0  <- groupCols0 |> unique()
  # groupCols0 |> print()
  ### Driver columns
  driverCols0 <- c("driverType", "driverUnit", "driverValue")
  ### National & regional scenario columns
  scenCols0   <- c("gdp_usd", "national_pop", "gdp_percap") |> c(popCol0)
  ### Impact columns
  impactCols0 <- c("physical_impacts", "annual_impacts")
  ### Columns to select
  select0     <- groupCols0 |> c(driverCols0, scenCols0) |> c("year")
  ### Relocate columns
  # df_results |> glimpse()
  df_results  <- df_results |> relocate(all_of(select0))
  # df_results  <- df_results |> relocate(any_of(select0))

  ### Scalar columns
  scalarCols0 <- c("physScalar", "physAdj", "damageAdj", "econScalar", "econAdj", "econMultiplier")
  infoCols0   <- c("c0", "c1", "exp0", "year0")
  suffix0     <- c("Name", "Value")
  scalarCols0 <- scalarCols0 |> map(~.x |> paste0(c(suffix0))) |> unlist()
  scalarCols0 <- scalarCols0 |> c("physScalar", "econScalar", "physEconScalar")
  scalarCols0 <- scalarCols0 |> c("scaled_impacts")
  scalarCols0 <- infoCols0   |> c(scalarCols0)
  rm(suffix0, infoCols0)
  ### Rearrange or drop scalar columns
  if(allCols){
    df_results  <- df_results |> relocate(any_of(scalarCols0), .after=all_of(select0))
  } else{
    df_results  <- df_results |> select(-any_of(scalarCols0))
  } ### End if(allCols)

  ### Other columns
  # excludeCols
  # otherCols0  <- df_results |> names() |> (function(x){x[!(x %in% c(select0, scalarCols0, impactCols0))]})()
  otherCols0  <- df_results |> names() |> get_matches(y=c(select0, scalarCols0, impactCols0), matches=FALSE)
  df_results  <- df_results |> select(-all_of(otherCols0))

  ### Convert to character and drop sector id
  df_results  <- df_results |> mutate_at(c(groupCols0), as.character)

  ###### ** Primary Columns ######
  mutate0     <- c("sectorprimary", "includeaggregate")
  df_results  <- df_results |> mutate_at(c(mutate0), as.numeric)
  rm(mutate0)
  # df_results |> glimpse()
  # df_results |> pull(region) |> unique() |> print()
  # return(df_results)


  ###### ** Aggregation ######
  ### For regular use (i.e., not impactYears), simplify the data: groupCols0
  if(doAgg) {
    # doAgg |> print()
    group0     <- groupCols0
    df_results <- df_results |> aggregate_impacts(
      aggLevels   = aggLevels,
      groupByCols = group0,
      columns     = impactCols0
    ) ### End aggregate_impacts
  } ### End if(doAgg)

  ###### Arrange Outputs ######
  ### Convert levels to character
  ### Order the rows, then order the columns
  # arrange0   <- groupCols0 |> c("year")
  arrange0   <- groupCols0 |> c("year") |> get_matches(y = df_results |> names())
  # arrange0 |> print()
  ### Select columns
  df_results <- df_results |> arrange_at(c(arrange0))
  df_results <- df_results |> relocate(any_of(select0))
  rm(arrange0)

  ###### Format as Tibble ######
  ### Update results in list
  df_results   <- df_results |> as_tibble()
  df_results   <- df_results |> ungroup()

  ###### Return Object ######
  ### Which object to return
  if(outputList) {
    ### Add items to list/reorganize list
    list_scenarios <- returnList[["scenarios" ]]
    returnList     <- list()
    returnList[["statusList"]] <- statusList
    returnList[["argsList"  ]] <- argsList
    returnList[["scenarios" ]] <- list_scenarios
    returnList[["results"   ]] <- df_results
    returnObj <- returnList
  } else {
    returnObj <- df_results
  } ### End if(outputList)


  ###### Return ######
  ### Message
  message("\n", "Finished", ".")
  return(returnObj)

} ### End function








