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
  ###### Load Objects ######
  ### Assign data objects to objects in this namespace
  ### Assign FrEDI config
  fredi_config <- rDataList[["fredi_config"]]
  for(name_i in fredi_config |> names()) {name_i |> assign(fredi_config[[name_i]]); rm(name_i)}

  calc_NOx_factor <- listMethane$package$coefficients$NOx      [["fun0"]]
  calc_mortality  <- listMethane$package$coefficients$Mortality[["fun0"]]
  NOxFactor0      <- listMethane$package$coefficients$NOx$NOxFactor0

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
  idCols0      <- list(valCols0=valCols0, df0=inputDefs[inNames0]) |> pmap(function(valCols0, df0){
    df0 |> names() |> get_matches(y=valCols0, matches=F)
  }) |> set_names(inNames0)



  ###### ** Input Defaults ######
  # listMethane$package$ch4_default <- listMethane$package$ch4_default |> rename(CH4_ppbv = delta_ch4)
  # listMethane$package$nox_default <- listMethane$package$nox_default |> rename(NOx_Mt   = NOx      )
  # listMethane$package$o3_response <- listMethane$package$o3_default
  # listMethane$package$o3_default  <- listMethane$package$ch4_default |> left_join(
  #   "nox_default" |> get_frediDataObj(listSub="package", listName="listMethane"),
  #   by=c("year")
  # ) |>
  #   mutate(O3_pptv_per_ppbv = NOx_Mt |> calc_NOx_factor()) |>
  #   mutate(O3_pptv_per_ppbv = O3_pptv_per_ppbv / NOxFactor0) |>
  #   mutate(O3_pptv_per_ppbv = O3_pptv_per_ppbv * CH4_ppbv) |>
  #   select(c("year", "O3_pptv_per_ppbv"))

  inputDefs    <- inNames0 |> map(function(name0){
    ### Get defaults
    defName0 <- name0    |> paste0("_default")
    df0      <- defName0 |> get_frediDataObj(listSub="package", listName="listMethane")
    ### Format defaults
    do_ch4_0 <- "ch4" %in% name0
    do_nox_0 <- "nox" %in% name0
    do_o3_0  <- "o3"  %in% name0
    if(do_ch4_0) df0 <- df0 |> rename(CH4_ppbv = delta_ch4)
    if(do_nox_0) df0 <- df0 |> rename(NOx_Mt   = NOx      )
    if(do_o3_0 ) {
      df0 <- df0 |> rename(O3_pptv_per_ppbv = o3_response)
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
  has_o3     <- inputsList[["o3" ]] |> nrow()
  has_ch4    <- inputsList[["ch4"]] |> nrow()
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




  ###### ** Physical Driver Scenario  ######
  ### Need scenario for CH4 & NOX or O3
  has_ch4    <- inputsList[["ch4"]] |> nrow()
  has_nox    <- inputsList[["nox"]] |> nrow()
  has_o3     <- inputsList[["o3" ]] |> nrow()
  has_driver <- (has_ch4 & has_nox) | has_o3
  if(!has_driver) {
    1 |> get_msgPrefix(newline=T) |> paste0("Warning! `run_fredi_methane()` requires :") |> message()
  }
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








