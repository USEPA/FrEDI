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
    inputsList = list(ch4=NULL, nox=NULL, gdp=NULL, pop=NULL), ### List of inputs
    elasticity = 1,    ### Override value for elasticity for economic values
    maxYear    = 2100,
    thru2300   = FALSE,
    outputList = FALSE, ### Whether to return input arguments as well as results. [If TRUE], returns a list instead of a data frame
    allCols    = FALSE, ### Whether to include additional columns in output
    silent     = TRUE   ### Whether to message the user
){
  ###### Set up the environment ######
  ###### ** Messaging ######
  ### Level of messaging (default is to message the user)
  msgUser   <- !silent


  ###### ** Load Data ######
  ### Assign data objects to objects in this namespace
  ### Assign FrEDI config
  fredi_config <- rDataList[["fredi_config"]]
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}

  ### Coefficients
  minYear0   <- listMethane[["package"]][["coefficients"]][["minYear0"]]
  maxYear0   <- listMethane[["package"]][["coefficients"]][["maxYear0"]]

  ### Model years and NPD (FrEDI past 2100)
  minYear   <- minYear0
  maxYear   <- thru2300 |> ifelse(npdYear0, maxYear)
  do_npd    <- maxYear > maxYear0



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
  popCol0        <- c("pop")
  stateCols0     <- c("state", "postal")



  ###### Input Scenarios ######
  ###### ** Input Info ######
  paste0("Checking scenarios...") |> message()
  ### Add info to data
  co_inputInfo <- "co_inputInfo" |> get_frediDataObj(listSub="package", listName="listMethane")
  # co_inputInfo <- co_inputInfo |> filter(!inputName %in% "o3")
  co_inputInfo <- co_inputInfo |> mutate(ref_year = 2020)
  co_inputInfo <- co_inputInfo |> mutate(min_year = 2020)
  co_inputInfo <- co_inputInfo |> mutate(max_year = maxYear)

  ### Initialize subset
  df_inputInfo <- co_inputInfo

  ### Input info
  inNames0     <- co_inputInfo |> pull(inputName)
  # inNames0 |> print()


  ###### ** Input Columns ######
  ### Get list with expected name of columns used for unique ids
  ### Get list with expected name of column containing values
  valCols0     <- co_inputInfo |> pull(valueCol) |> as.list() |> set_names(inNames0)
  idCols0      <- inNames0     |> map(get_import_inputs_idCols) |> set_names(inNames0)
  # idCols0      <- list(valCols0=valCols0, df0=inputDefs[inNames0]) |> pmap(function(valCols0, df0){
  #   df0 |> names() |> get_matches(y=valCols0, matches=F)
  # }) |> set_names(inNames0)



  ###### ** Input Defaults ######
  inputDefs    <- inNames0 |> map(function(name0){
    ### Get defaults
    defName0 <- name0    |> paste0("_default")
    df0      <- defName0 |> get_frediDataObj(listSub="scenarioData", listName="listMethane")
    ### Format defaults
    do_o3_0  <- "o3"  %in% name0
    if(do_o3_0 ) {
      df0 <- df0 |> select(c(idCols0[["o3"]], valCols0[["o3"]]))
    } ### End if(do_o3_0 )
    ### Return
    return(df0)
  }) |> set_names(inNames0)





  ###### ** Valid Inputs & Input Info ######
  ### Figure out which inputs are not null, and filter to that list
  ### inputsList Names
  inNames      <- inputsList |> names()
  # inNames |> print(); inputsList |> map(glimpse)
  inWhich      <- inNames    |> map(function(name0, list0=inputsList){!(list0[[name0]] |> is.null())}) |> unlist() |> which()
  ### Filter to values that are not NULL
  inputsList   <- inputsList[inWhich]
  inNames      <- inputsList |> names()
  rm(inWhich)
  ### Check which input names are in the user-provided list
  inWhich      <- inNames %in% inNames0
  inNames      <- inNames[inWhich]
  inputsList   <- inputsList[inNames]
  hasAnyInputs <- inNames |> length()
  rm(inWhich)
  # inNames |> print()


  ### Need scenario for CH4 & NOX or O3:
  ### If has O3, use O3. Otherwise, use CH4
  has_o3     <- !(inputsList[["o3" ]] |> is.null())
  has_o3 |> print()
  # has_ch4    <- !(inputsList[["ch4"]] |> is.null())
  if(has_o3) {
    inputsList <- inputsList |> (function(list0, y=c("ch4", "nox")){list0[!((list0 |> names() %in% y))]})()
    inputDefs  <- inputDefs  |> (function(list0, y=c("ch4", "nox")){list0[!((list0 |> names() %in% y))]})()
    inNames0   <- inNames0   |> get_matches(y=c("ch4", "nox"), matches=F)
    inNames    <- inNames    |> get_matches(y=c("ch4", "nox"), matches=F)
  } else {
    inputsList <- inputsList |> (function(list0, y=c("o3")){list0[!((list0 |> names() %in% y))]})()
    inputDefs  <- inputDefs  |> (function(list0, y=c("o3")){list0[!((list0 |> names() %in% y))]})()
    inNames0   <- inNames0   |> get_matches(y=c("o3"), matches=F)
    inNames    <- inNames    |> get_matches(y=c("o3"), matches=F)
  } ### End if(has_o3)


  ###### ** Check Inputs ######
  ### Filter to valid inputs & get info
  ### Reorganize inputs list
  df_inputInfo <- df_inputInfo |> filter(inputName %in% inNames)
  inNames      <- df_inputInfo |> pull(inputName)

  ### Create logicals and initialize inputs list
  if(hasAnyInputs) {
    ### Min ad max years
    minYrs0    <- inNames |> map(function(name0, df0=df_inputInfo){df0 |> filter(inputName %in% name0) |> pull(min_year) |> unique()}) |> set_names(inNames)
    maxYrs0    <- inNames |> map(function(name0, df0=df_inputInfo){df0 |> filter(inputName %in% name0) |> pull(max_year) |> unique()}) |> set_names(inNames)

    ### Check inputs
    inputsList <- list(
      inputName = inNames,
      inputDf   = inputsList[inNames],
      idCol     = idCols0   [inNames],
      valCol    = valCols0  [inNames],
      yearMin   = minYrs0,
      yearMax   = maxYrs0,
      module    = "methane" |> rep(inNames |> length())
    ) |>
      pmap(check_input_data) |>
      set_names(inNames)
    rm(minYrs0, maxYrs0)

    ### Check again for inputs
    ### Filter to values that are not NULL
    inWhich      <- inNames    |> map(function(name0, list0=inputsList){!(list0[[name0]] |> is.null())}) |> unlist() |> which()
    inputsList   <- inputsList[inWhich]
    inNames      <- inputsList |> names()
    rm(inWhich)
  } ### if(hasAnyInputs)


  ### Update list
  ### For each input:
  ### - Make sure values are at correct range
  ### - Update in status list
  if(outputList){
    statusList[["inputsList"]] <- inputsList |> map(function(df0){
      df0 |> length() |> as.logical() |> get_returnListStatus()
    }) |> set_names(inNames)
    argsList  [["inputsList"]] <- inputsList
  } ### End if(outputList)


  ### Update values
  # inNames |> print()
  hasInputs    <- inNames |> length()

  ### Iterate over list and format values
  if(hasInputs) {
    inputsList   <- list(
      name0     = inNames,
      df0       = inputsList,
      hasInput0 = TRUE |> rep(inNames |> length()),
      idCols0   = idCols0 [inNames],
      valCols0  = valCols0[inNames]
    ) |> pmap(function(df0, name0, hasInput0, idCols0, valCols0){
      df0 |> format_inputScenarios(
        name0     = name0,
        hasInput0 = hasInput0,
        idCols0   = idCols0,
        valCols0  = valCols0,
        minYear   = minYear,
        maxYear   = maxYear,
        info0     = co_inputInfo
      ) ### End format_inputScenarios
    }) |> set_names(inNames)
  } ### End if(hasInputs)

  ### Update inputs with defaults if values are missing
  inputsList   <- inNames0 |> (function(names0, list0=inputDefs, list1=inputsList){
    ### Filter to list
    list0    <- list0[names0]
    ### List names
    names0   <- list0 |> names()
    ### If user provided a scenario, update defaults list
    for(name_i in names0) {
      df_i     <- list1[[name_i]]
      has_i    <- df_i |> length()
      if(has_i) list0[[name_i]] <- df_i
      rm(name_i, df_i, has_i)
    } ### End for(name_i in names0)
    ### Return
    return(list0)
  })()
  # inputsList |> names() |> print()
  ### Update names
  inNames      <- inputsList |> names()
  df_inputInfo <- co_inputInfo |> filter(inputName %in% inNames)

  ### Filter to lists
  inputsList   <- inputsList |> map(function(df0, minYr0=minYear, maxYr0=maxYear){
    df0 <- df0 |> filter(year >= minYear, year <= maxYear)
    return(df0)
  }) |> set_names(inNames)




  ###### Physical Driver Scenario  ######
  ### Need scenario for CH4 & NOX or O3
  has_ch4    <- inputsList[["ch4"]] |> nrow() |> length()
  has_nox    <- inputsList[["nox"]] |> nrow() |> length()
  has_o3     <- inputsList[["o3" ]] |> nrow() |> length()
  has_driver <- (has_ch4 & has_nox) | has_o3
  if(has_o3) {
    df_drivers <- inputsList[["o3"]]
  } else{
    join0      <- c("year")
    df_drivers <- inputsList[["ch4"]] |> left_join(inputsList[["nox"]], by=join0)
    rm(join0)
  } ### End if(has_o3)

  ### Get RR scalar and ozone response data
  df_drivers <- df_drivers |> (function(df0, df1=listMethane$package$state_rrScalar){
    ### Join data
    join0 <- df0 |> names() |> get_matches(y=df1 |> names())
    df0   <- df0 |> left_join(df1, by=join0, relationship="many-to-many")
    rm(df1)
    ### Calculate O3 if methane and nox present
    do_o3 <- df0 |> names() |> get_matches(y=c("CH4_ppbv")) |> length()
    if(do_o3) df0 <- df0 |> calc_o3_conc()
  })()
  # return(df_drivers)

  ###### Socioeconomic Driver Scenario ######
  ### Update values
  gdp_df       <- inputsList[["gdp"]]
  pop_df       <- inputsList[["pop"]]
  pop_df       <- pop_df |> mutate(region = region |> str_replace(" ", ""))
  pop_df       <- pop_df |> mutate(region = region |> str_replace("\\.", ""))
  # return(pop_df)
  ### Calculate national population and update national scenario
  natScenario  <- gdp_df |> create_nationalScenario(pop0 = pop_df)
  rm(gdp_df, pop_df)

  ### Calculate for CONUS values
  seScenario   <- natScenario |> calc_conus_scenario()
  # return(seScenario)
  # seScenario |> pull(region) |> unique() |> print()
  # seScenario |> glimpse()

  ###### Calculate Impacts ######
  ###### ** Calculate Scalars ######
  ### Initialized results: Join sector info and default scenario
  ### Calculate physical scalars and economic multipliers then calculate scalars
  paste0("Calculating impacts...") |> message()
  df_results   <- seScenario |> calc_methane_scalars(elasticity = elasticity)
  # return(df_results)
  # df_results |> select(c("year", "gdp_usd", "national_pop", "gdp_percap")) |> unique() |> nrow() |> print()

  ###### ** Calculate Mortality Rate ######
  df_results <- df_results |> calc_mortality()

  ###### ** Calculate Excess Mortality ######
  df_results <- df_results |> (function(df0, df1=df_drivers){
    ### Join data with drivers
    join0 <- df0 |> names() |> get_matches(y=df1 |> names())
    df0   <- df0 |> left_join(df1, by=join0)
    rm(df1)
    ### Calculate excess mortality
    df0   <- df0 |> mutate(physical_impacts = pop * respMrate * state_mortScalar * O3_pptv)
    ### Calculate annual impacts
    df0   <- df0 |> mutate(annual_impacts   = physical_impacts * econScalar)
  })()


  ###### Format Results ######
  ### Add in model info
  paste0("Formatting results", "...") |> message()


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








