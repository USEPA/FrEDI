## Overview ----------------
### This file contains helper functions for the FrEDI SV module.
## get_sv_sectorInfo ----------------
#' Retrieve a character vector of sectors available in the FrEDI SV module ([FrEDI::run_fredi_sv]) or a data frame with SV sectors and additional information.
#'
#' @description
#' `get_sv_sectorInfo` returns a character vector with the names of sectors in the FrEDI SV module (default) **or** a data frame of SV sectors with additional information (e.g., associated variants, model type, etc.).
#'
#'
#' @param description Logical value indicating whether to include information about each sector. Returns a data frame if `description=TRUE` and returns a character vector of sector names if `description=FALSE` (default).
#'
#' @param gcmOnly     Logical value indicating whether to return only sectors with impacts driven by temperature changes.
#'
#' @param slrOnly     Logical value indicating whether to return only sectors with impacts driven by sea level changes.
#'
#'
#' @details
#' `get_sv_sectorInfo` returns a character vector with the names of sectors in the FrEDI SV module (`description=FALSE`, default) **or** a data frame with SV sectors and additional information (`description=TRUE`).
#'
#' * If `description=FALSE` (default), `get_sv_sectorInfo` returns a character vector of names of sectors that can be passed to the [FrEDI::run_sv_fredi()] `sector` argument. Specify whether to return only GCM sectors by running `get_sv_sectorInfo(gcmOnly=TRUE)` or SLR sectors by running `get_sv_sectorInfo(slrOnly=TRUE)`.
#' * If `description=TRUE`, `get_sv_sectorInfo` returns a data frame of sectors with related information, such as whether a particular sector is driven primarily by temperature (`modelType="GCM"`) or sea level rise (`modelType="SLR"`), associated driver units (`"degrees Celsius"` for temperature-driven sectors, `"cm"` for SLR-driven sectors), impact units (e.g., mortality, etc.), and sector variants. Users can use [FrEDI::get_sv_sectorInfo] to determine which sectors can be passed to the [FrEDI::run_sv_fredi()] `sector` argument and/or to determine whether a particular sector is driven primarily by temperature (`modelType="GCM"`) or sea level rise (`modelType="SLR"`).
#'
#' Users can specify whether to return only GCM sectors by setting `gcmOnly=TRUE` **or** SLR sectors by setting `slrOnly=TRUE`.
#'
#' @return
#'
#' [FrEDI::get_sv_sectorInfo()] will return SV sectors in the form specified by `description`:
#'
#' * If `description=FALSE` (default), outputs a character vector containing the names of sectors available for the FrEDI SV Module.
#' * If `description=TRUE`, outputs a data frame containing the names of sectors available for the FrEDI SV Module in one column, with information about the sector model type, model type ("GCM" or "SLR") and associated driver unit ("degrees Celsius" or "cm", respectively ), impact units (e.g., mortality, etc.), and available variants in the remaining columns.
#'
#' @examples
#'
#' ### Return a character vector with the names of all of the sectors in the FrEDI SV Module:
#' get_sv_sectorInfo()
#'
#' ### Return a data frame of all of the sectors in the FrEDI SV Module (sector names and additional information)
#' get_sv_sectorInfo(description=T)
#'
#' ### Return a character vector with only the names of the temperature-driven sectors:
#' get_sv_sectorInfo(gcmOnly=T)
#'
#' ### Return a character vector with only the names of the temperature-driven sectors:
#' get_sv_sectorInfo(slrOnly=T)
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#' @export
#' @md
#'
###
get_sv_sectorInfo <- function(
    description = F,
    gcmOnly     = F,
    slrOnly     = F
){
  ### Get data object
  sectorInfo <- svDataList[["svSectorInfo"]]

  ### Filter to model types
  gcmStr0    <- "gcm"
  slrStr0    <- "slr"
  modTypes0  <- c(gcmStr0, slrStr0)
  if(gcmOnly) modTypes0 <- modTypes0 |> get_matches(slrStr0, matches=F)
  if(slrOnly) modTypes0 <- modTypes0 |> get_matches(slrStr0, matches=F)

  ### If not description, return sectors
  if(!description) {
    sectorInfo <- sectorInfo |> pull(sector) |> unique() |> sort()
    return(sectorInfo)
  } ### End if(!description)

  ### Otherwise, select columns, rename, and collapse variants
  ### Select and arrange
  select0    <- c("sector", "modelType", "impactUnit", "driverUnit_label", "variant_label")
  group0     <- c("sector", "modelType", "driverUnit_label")
  # from0      <- c("driverUnit_label", "variant_label", "modelType")
  # to0        <- c("driverUnit", "variants", "model_type")
  from0      <- c("driverUnit_label", "variant_label")
  to0        <- c("driverUnit", "variants")
  sectorInfo <- sectorInfo |>
    mutate(modelType = modelType |> toupper()) |>
    group_by_at(c(group0)) |>
    summarize_at(c("variant_label"), paste0, collapse=", ") |>
    ungroup() |>
    arrange_at(c(group0)) |>
    rename_at(c(from0), ~to0)

  ### Return
  return(sectorInfo)
}


## calc_countyPop ----------------
calc_countyPop <- function(
    id0, ### GeoID
    state0,
    region0,
    df0, ### Tibble with population projections
    xCol0 = "year"  ### X column in df0
    # yCol0 = "pop"   ### Y column in df0
){
  ### Check function
  df0     <- df0 |> filter(state %in% state0)
  fun0    <- svPopList[["popProjList"]][[region0]][[state0]][["county2state"]][[id0]]
  hasFun0 <- !(fun0 |> is.null())
  if(hasFun0) {
    df0 <- df0 |> mutate(y = df0 |> pull(all_of(xCol0)) |> fun0())
  } else{
    df0 <- df0 |> mutate(y = NA)
  } ### End if(hasFun0)

  ### Add ID
  df0      <- df0 |> mutate(geoid10 = id0)

  ### Return
  return(df0)
}

get_countyPop <- function(
    df0,    ### Dataframe of state-level population projection
    years   = seq(2010, 2100, by=5), ### Years for analysis
    xCol0   = "year",  ### X column in df0
    yCol0   = "pop" ,  ### Y column in df0
    funList = svPopList[["popProjList"]] ### List of population projections
){
  ### Filter to years
  df0      <- df0 |> filter(year %in% years)
  # df0 |> glimpse()
  ### Get unique states and regions
  select0  <- c("state", "region")
  pList0   <- df0    |> select(all_of(select0)) |> unique()
  states0  <- pList0 |> pull(state)

  ### Iterate over states:
  df0      <- pList0 |>
    rename_at(c(select0), ~select0 |> paste0(0)) |>
    pmap(function(state0, region0){
      dfX0    <- df0  |> filter(state %in% state0)
      geoIds0 <- svPopList[["popProjList"]][[region0]][[state0]][["county2state"]] |> names()
      dfX0    <- geoIds0 |> map(
        calc_countyPop,
        state0  = state0,
        region0 = region0,
        df0     = dfX0,
      ) |> bind_rows()
      # dfX0 |> glimpse()
      return(dfX0)
    }) |> bind_rows()

  ### Calculate values
  drop0   <- yCol0 |> c("state2county")
  df0     <- df0 |>
    rename_at(c("y"), ~"state2county") |>
    # mutate(county_pop = !!sym(xCol0) * state2county)
    mutate(county_pop = !!sym(yCol0) * state2county) |>
    select(-any_of(drop0))

  ### Return
  gc()
  return(df0)
}

## calc_tractScaledImpacts ----------------
# ### Use this function to calculate tract level impacts
### Use this function to calculate tract level impacts
calc_tractScaledImpacts <- function(
    tract0, ### List of impact functions
    df0,     ### Tibble with driver values
    newEnv
){
  ### Names of functions
  df0       <- df0 |> mutate(fips = tract0)
  ### Function
  fun0      <- newEnv$funList[[tract0]]
  isFun0    <- "function" %in% (fun0 |> class())
  hasFun0   <- (fun0 |> length()) & isFun0
  if(hasFun0) df0 <- df0 |> mutate(y = x |> fun0())
  else        df0 <- df0 |> mutate(y = NA)
  # rm(funList, envir=newEnv); rm(newEnv)
  ### Return
  return(df0)
}

### By scenario
map_svScenario_scaledTract <- function(
    scenario_j, ### List of scenarios
    sector_j,
    varId_j,
    varLbl_j,
    tracts_j,
    newEnv,
    df_j, ### Dataframe of driver values for one scenario with columns driverValue, driverUnit, year
    xCol    = "driverValue",
    sleep   = 1e-7,
    silent  = FALSE,
    .msg0   = 0
    # .msg0  = ""
){
  #### Messaging ----------------
  ### Level of messaging (default is to message the user)
  msgUser  <- !silent
  msgN     <- "\n"
  msg0     <- .msg0
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  msg3     <- msg0 + 3
  ### Message user
  msgScen_j <- "scenario='" |> paste0(scenario_j, "'")
  msgVar_j  <- "variant='"  |> paste0(varLbl_j  , "'")
  msgSect_j <- "Calculating impacts for sector='" |> paste0(sector_j, "'")
  msg0 |> get_msgPrefix(newline=T) |> paste0(msgSect_j) |> paste(msgVar_j, msgScen_j, sep=", ") |> paste0("...") |> message()
  ### Filter drivers
  df_j      <- df_j |>
    filter(scenario == scenario_j) |>
    rename_at(c(xCol), ~"x")
  #### Tract Scaled Impacts ----------------
  ### Calculate scaled impacts
  msg1 |> get_msgPrefix() |> paste0("Calculating scaled impacts for each tract...") |> message()
  impacts_j <- tracts_j |>
    map(calc_tractScaledImpacts, df0=df_j, newEnv=newEnv) |>
    bind_rows() |>
    rename_at(c("x", "y"), ~c(xCol, "sv_impact"))
  # rm(funList, envir=newEnv); rm(newEnv)
  msg1 |> get_msgPrefix() |> paste0("Finished calculating tract-level impacts.") |> message()
  #### Return ----------------
  gc()
  return(impacts_j)
}

### By variant
map_svVariant <- function(
    row_i,  ### List of impact functions
    info_i, ### df_sectorInfo
    sector_i,
    # tracts_i,
    scenarios_i, ### List of scenarios
    df0, ### Dataframe of driver values for one scenario with columns driverValue, driverUnit, year
    funPath,
    xCol    = "driverValue",
    popData,
    svGroups,
    dfGroups,
    dataExt = "rds",
    sleep   = 1e-7,
    silent  = FALSE,
    .msg0   = .msg0,
    .testing = FALSE
    # .msg0  = ""
){
  #### Messaging ----------------
  ### Level of messaging (default is to message the user)
  msgUser  <- !silent
  msgN     <- "\n"
  msg0     <- .msg0
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  msg3     <- msg0 + 3

  #### Values ----------------
  ### Which SV data to use
  doCoastal    <- sector_i %in% "Coastal Properties"
  svName_i     <- doCoastal |> ifelse("svDataCoastal", "svData")
  svInfo_i     <- svDataList[[svName_i]]

  # scenarios_x |> print(); # svName_i |> print()
  info_i       <- info_i[row_i,]
  sectorAbbr_i <- info_i[["impactList_fileExt"]]
  varLabel_i   <- info_i[["variant_label"     ]]
  varAbbr_i    <- info_i[["variant_abbr"      ]]
  weightsCol_i <- info_i[["popWeightCol"      ]]
  naVar_i      <- varAbbr_i |> is.na()
  years_i      <- df0 |> pull(year) |> unique()

  ### Which impacts list to use
  # varAbbr_i     <- varAbbr_i |> (function(y){y |> is.na() |> ifelse(NULL, y)})()
  if(naVar_i) varStr0 <- NULL
  else        varStr0 <- varAbbr_i
  # msgVar_i      <- "variant='" |> paste0(varLabel_i, "'")

  #### Impacts List ----------------
  ### Read in the file into a new environment
  ### Load previous Default Data to compare to new outputs
  impactsName_i <- "impactsList" |> c(sectorAbbr_i, varStr0) |> paste(collapse="_")
  impactsPath_i <- funPath   |> file.path(impactsName_i) |> paste0(".", dataExt)
  newEnv        <- new.env()
  newEnv$funList <- impactsPath_i |> readRDS()
  tracts_i       <- newEnv$funList |> names()
  # tracts_i       <- tracts_i |> head()

  #### Calculate Scaled Impacts ----------------
  ### Iterate over scenarios, and calculate tract impacts
  impacts_i     <- scenarios_i |> map(
    map_svScenario_scaledTract,
    sector_j = sector_i,
    varId_j  = varAbbr_i,
    varLbl_j = varLabel_i,
    tracts_j = tracts_i,
    newEnv   = newEnv,
    df_j     = df0, ### Dataframe of driver values for one scenario with columns driverValue, driverUnit, year
    xCol     = xCol,
    sleep   = 1e-7,
    silent  = FALSE,
    .msg0   = msg0
    # .msg0  = ""
  ) |> set_names(scenarios_i)
  rm(funList, envir=newEnv); rm(newEnv)

  #### Calculate Tract Impacts ----------------
  ### Iterate over scenarios, calculate tract impacts
  impacts_i     <- impacts_i |> map(
    calc_tractImpacts,
    sector    = sector_i,
    popData   = popData,
    svInfo    = svInfo_i,
    svGroups  = svGroups,
    weightCol = weightsCol_i,
    years     = years_i,
    silent    = silent,
    .msg0     = msg0,
    .testing  = .testing
  ) |>
    set_names(scenarios_i) |>
    bind_rows(.id="scenario") |>
    mutate(variant = varLabel_i) |>
    relocate(c("scenario"))
  # impacts_i |> glimpse()
  #### Bind Results ----------------
  # ### Bind results and add variant level
  # impacts_i <- impacts_i |> bind_rows(.id="scenario")
  # impacts_i <- impacts_i |> mutate(variant = varLabel_i)
  # impacts_i <- impacts_i |> relocate(c("scenario"))

  #### Adjust SV Group Values ----------------
  if(!.testing){
    valSuff0  <- c("ref", "sv")
    ### Join and adjust results valueAdj
    valCols0  <- c("impPop", "impact", "national_highRiskPop", "regional_highRiskPop", "aveRate")
    valCols1  <- valCols0  |> map(function(col_j){
      col_j |> paste(valSuff0, sep="_")
    }) |> unlist()
    drop0     <- c("validGroups", "weightCol", "validType", "valueAdj")
    ### Adjust results
    impacts_i <- impacts_i |> left_join(dfGroups, by="svGroupType")
    # impacts_i |> glimpse()
    # valCols1 |> print()
    # return(impacts_i)
    # impacts_i <- impacts_i |> mutate_at(c(valCols1), function(col_j){
    #   col_j * impacts_i$valueAdj
    #   })
    impacts_i[,valCols1] <- impacts_i[,valCols1] * impacts_i$valueAdj
    impacts_i <- impacts_i |> select(-any_of(drop0))
    rm(drop0)
  } ### End if(!.testing)

  ### Return
  return(impacts_i)
} ### End for(row_i in cRows)



## calc_terciles ----------------
calc_terciles <- function(data_x){
  ### Probability values for terciles
  n_quants <- 3
  c_probs  <- 0      |> seq(1, length.out=n_quants + 1)
  c_quants <- data_x |> quantile(na.rm=T, probs = c_probs)
  c_cutoff <- c_quants[3]
  ### Return
  return(c_cutoff)
}

## calc_tractImpacts ----------------
### Use this function to calculate tract level impacts
calc_tractImpacts <- function(
    scaledImpacts,    ### Dataframe of scaled impacts by tract
    sector,           ### Name of sector
    popData,          ### Dataframe of population projections
    svInfo    = NULL, ### Dataframe of sv data
    svGroups  = NULL, ### Character vector of sv group columns
    weightCol = NULL,
    years     = seq(2010, 2100, by=5),
    sleep     = 1e-5,
    silent    = FALSE,
    .msg0     = 0,
    # .msg0     = "",
    .testing  = FALSE
){
  ### Set Up Environment ----------------
  #### Constants ----------------
  regions   <- popData       |> pull(region) |> unique()
  tracts    <- scaledImpacts |> pull(fips  ) |> unique()

  #### Messaging ----------------
  ### Level of messaging (default is to message the user)
  msgUser  <- !silent
  msgN     <- "\n"
  msg0     <- .msg0
  msg1     <- msg0 + 1
  msg2     <- msg0 + 2
  msg3     <- msg0 + 3

  ### Format Data ----------------
  popData       <- popData       |> filter(year %in% years)
  scaledImpacts <- scaledImpacts |> filter(year %in% years)
  scaledImpacts <- scaledImpacts |> filter(!(driverUnit |> is.na()))

  ### Total Impacts ----------------
  msg0 |> get_msgPrefix() |> paste0("Calculating total impacts for each tract...") |> message()
  ### Format svInfo - Add column for none and drop other columns
  drop0       <- c("svCounty")
  x_impacts   <- svInfo    |> mutate(none = 1)
  x_impacts   <- x_impacts |> select(-all_of(drop0))
  rm(svInfo, drop0)
  ### Join svInfo with population projections by region, state, geoid10
  join0       <- c("region", "state", "geoid10")
  x_impacts   <- x_impacts |> left_join(popData, by=c(join0))
  rm(popData, join0)
  ### Join svInfo with the impacts by fips number and drop missing values
  join0       <- c("year", "fips")
  x_impacts   <- x_impacts |> left_join(scaledImpacts, by=c(join0))
  x_impacts   <- x_impacts |> filter(!(driverUnit |> is.na()))
  rm(scaledImpacts)

  ### Population Weight ----------------
  ### Add population weight column
  drop0       <- c("children", "highRiskLabor")
  rename0     <- c(weightCol)
  renameTo    <- c("popWeight")
  x_impacts[["popWeight"]]   <- x_impacts[[weightCol]]
  x_impacts   <- x_impacts |> select(-any_of(drop0))
  rm(drop0, rename0, renameTo)

  ### Tract Population ----------------
  ### Calculate total tract population and drop columns
  drop0       <- c("state", "county", "geoid10", "region_pop", "pop", "county_pop")
  # drop0       <- c("region_pop", "pop", "county_pop")
  x_impacts   <- x_impacts |> mutate(tract_totPop = county_pop * ratioTract2CountyPop)
  x_impacts   <- x_impacts |> select(-any_of(drop0))
  rm(drop0)

  ### Non-Meaningful Groups ----------------
  ### Convert values for non-meaningful SV groups to zero
  svPlus65    <- "sv_plus65"
  svNoHS      <- "sv_noHS"
  do_plus65   <- sector |> str_detect("Air Quality") |> any()
  do_noHS     <- sector |> str_detect("Childhood Asthma") |> any()
  mutate0     <- c()
  if(do_plus65) mutate0 <- mutate0 |> c(svPlus65)
  if(do_noHS  ) mutate0 <- mutate0 |> c(svNoHS)
  x_impacts   <- x_impacts |> mutate_at(c(mutate0), function(z){0})
  rm(svPlus65, svNoHS, do_plus65, do_noHS, mutate0)

  ### Terciles ----------------
  #### National Terciles ----------------
  if(msgUser) msg1 |> get_msgPrefix() |> paste0("Calculating national terciles...") |> message()
  ### Columns
  groupsNat0    <- c("year")
  tractNat0     <- c("national_highRiskTract")
  cutoffNat0    <- c("national_cutoff")
  scaledImpact0 <- c("sv_impact")

  ### Calculate terciles
  select0         <- c(groupsNat0, scaledImpact0)
  quants_national <- x_impacts       |> select(all_of(select0))
  quants_national <- quants_national |>
    group_by_at (c(groupsNat0)) |>
    summarize_at(c(scaledImpact0), calc_terciles) |> ungroup()
  if(.testing) quants_national |> filter(year==2050) |> glimpse()
  rm(select0)

  ### Rename column
  quants_national <- quants_national |> rename_at(c(scaledImpact0), ~c(cutoffNat0))

  ### Join with national quantiles
  if(msgUser) {msg2 |> get_msgPrefix() |> paste0("Joining national terciles to tract-level data...") |> message()}
  x_impacts <- x_impacts |> left_join(quants_national, by=groupsNat0)
  rm(quants_national)

  ### Figure out which tracts are high risk
  ### Calculate high risk populations
  if(msgUser) {msg2 |> get_msgPrefix() |> paste0("Calculating national high risk populations...") |> message()}
  x_impacts[[tractNat0]] <- (x_impacts[[scaledImpact0]] > x_impacts[[cutoffNat0]]) * 1
  x_impacts <- x_impacts |> select(-all_of(cutoffNat0))
  rm(cutoffNat0)

  #### Regional Terciles ----------------
  if(msgUser) {msg1 |> get_msgPrefix() |> paste0("Calculating regional terciles...") |> message()}
  ### Columns
  groupsReg0    <- c(groupsNat0, "region")
  tractReg0     <- c("regional_highRiskTract")
  cutoffReg0    <- c("regional_cutoff")
  ### Calculate terciles
  select0          <- c(groupsReg0, scaledImpact0)
  quants_regional  <- x_impacts |> select(all_of(select0))
  quants_regional  <- quants_regional |>
    group_by_at (c(groupsReg0)) |>
    summarize_at(c(scaledImpact0), calc_terciles) |> ungroup()
  if(.testing) quants_regional |> filter(year==2050) |> glimpse()
  rm(select0)

  ### Rename column
  quants_regional <- quants_regional |> rename_at(c(scaledImpact0), ~c(cutoffReg0))

  ### Join with regional quantiles
  if(msgUser){msg2 |> get_msgPrefix() |> paste0("Joining regional terciles to tract-level data...") |> message()}
  x_impacts <- x_impacts |> left_join(quants_regional, by=groupsReg0)
  rm(quants_regional)

  ### Regional High Risk Tracts
  ### Figure out which tracts are high risk
  ### Calculate high risk populations
  if(msgUser) {msg2 |> get_msgPrefix() |> paste0("Calculating regional high risk populations...") |> message()}
  x_impacts[[tractReg0]] <- (x_impacts[[scaledImpact0]] > x_impacts[[cutoffReg0]]) * 1
  x_impacts <- x_impacts |> select(-all_of(cutoffReg0))
  rm(cutoffReg0)

  ### Total Impacts ----------------
  ### - Impacts = population*popWeight
  c_pop0      <- c("tract_totPop")
  c_impPop0   <- c("tract_impPop")
  c_impact0   <- c("tract_impact")
  ### - Impacted population (e.g., children for Air Quality) (Impacted population = population*popWeight)
  x_impacts[[c_impPop0]] <- x_impacts[[c_pop0   ]] * x_impacts[["popWeight"]]
  ### - Calculate SV impacts for ref pop and impacted SV pop (Impacts = impacted population*sv_impact)
  x_impacts[[c_impact0]] <- x_impacts[[c_impPop0]] * x_impacts[["sv_impact"]]
  ### Drop columns
  x_impacts   <- x_impacts |> select(-all_of(c_pop0))
  rm(c_pop0)

  ### Gather Groups
  ### Gather by svGroupType: all the main SV variables, and racial vars
  x_impacts   <- x_impacts |> pivot_longer(
    cols      = all_of(svGroups),
    names_to  = "svGroupType",
    values_to = "svRatio2Ref"
  ) ### End pivot_longer

  ### Tract Values ----------------
  ### Columns
  c_suffix0    <- c("ref", "sv")
  c_impPop1    <- c_impPop0 |> paste(c_suffix0, sep="_")
  c_impact1    <- c_impact0 |> paste(c_suffix0, sep="_")
  ### New columns
  sumCols0     <- c(c_impPop0, c_impact0)
  refSumCols0  <- sumCols0 |> paste("ref", sep="_")
  svSumCols0   <- sumCols0 |> paste("sv" , sep="_")
  ### - Calculate SV ref pop ("refPop") and weighted (impacted) SV pop ("impactPop")
  svRatio2Ref0 <- x_impacts[["svRatio2Ref"]]
  x_impacts[, refSumCols0] <- x_impacts[, sumCols0] * (1 - svRatio2Ref0)
  x_impacts[, svSumCols0 ] <- x_impacts[, sumCols0] * svRatio2Ref0
  rm(svRatio2Ref0); rm(sumCols0, refSumCols0, svSumCols0)
  ### Calculate high-risk populations
  popNat1      <- c("national_highRiskPop") |> paste(c_suffix0, sep="_")
  popReg1      <- c("regional_highRiskPop") |> paste(c_suffix0, sep="_")
  x_impacts[,popNat1] <-  x_impacts[,c_impPop1] * x_impacts[[tractNat0]]
  x_impacts[,popReg1] <-  x_impacts[,c_impPop1] * x_impacts[[tractReg0]]
  rm(tractNat0, tractReg0)

  ### State Summaries ----------------
  if(!.testing){
    if(msgUser){msg1 |> get_msgPrefix() |> paste0( "Calculating state summaries...") |> message()}
    sumCols0     <- c(c_impPop1, c_impact1) |> c(popNat1, popReg1)
    groupCols0   <- c("region", "svGroupType", "driverUnit", "driverValue", "year")
    # groupCols0   <- c("region", "state", "postal", "svGroupType", "driverUnit", "driverValue", "year")
    ### Group by the grouping columns and summarize the summary columns
    x_impacts    <- x_impacts |>
      group_by_at (c(groupCols0)) |>
      summarize_at(c(sumCols0), sum, na.rm=T) |> ungroup()

    ### Select all of the relevant columns
    select0      <- c(groupCols0, sumCols0)
    x_impacts    <- x_impacts |> select(all_of(select0))
    ### Replace tract in summary names
    x_impacts    <- x_impacts |> rename_at(c(sumCols0), ~gsub("tract_", "", sumCols0));
    rm(sumCols0, groupCols0, select0)

    ### Average Rates
    ### Convert 0 values to NA and then to zero
    rateCols0    <- c("aveRate") |> paste(c_suffix0, sep="_")
    c_impPop1    <- gsub("tract_", "", c_impPop1)
    c_impact1    <- gsub("tract_", "", c_impact1)
    x_impacts[, rateCols0] <- x_impacts[,c_impact1] / x_impacts[,c_impPop1]
    ### Replace NA values
    which0_ref    <- (x_impacts$impPop_ref == 0) |> which()
    which0_sv     <- (x_impacts$impPop_sv  == 0) |> which()
    x_impacts[["aveRate_ref"]][which0_ref] <- 0
    x_impacts[["aveRate_sv" ]][which0_sv ] <- 0
    rm(which0_ref, which0_sv)
  } ### End if(!.testing)


  ### Return ----------------
  msg1 |> get_msgPrefix() |> paste0("Finished calculating total impacts.") |> message()
  gc()
  return(x_impacts)
}

## get_validGroups ----------------
get_validGroups <- function(
    col0 = "none", ### c_popWtCol
    df0  = svDataList[["svDemoInfo"  ]], ### svDemoInfo
    df1  = svDataList[["svValidTypes"]]  ### svValidTypes
){
  ### Column names
  old0 <- c("colName"    , "valid_popWeightCols")
  new0 <- c("svGroupType", "validGroups")
  ### Reshape svDemoInfo
  df0  <- df0 |> filter(colType %in% c("bipoc")) |> select(c(old0[1]))
  df0  <- df0 |> rename_at(c(old0[1]), ~c(new0[1]))
  # children, highRiskLabor, sv_plus65, none
  df0  <- df0 |> mutate(validGroups = "children, highRiskLabor, sv_plus65, none")
  ### Reshape svValidTypes
  df1  <- df1 |> select(c(new0[1], old0[2]))
  df1  <- df1 |> rename_at(c(old0[2]), ~c(new0[2]))
  ### Bind
  df0  <- df1 |> rbind(df0)
  rm(df1, old0, new0)

  ### Calculate weight columns
  col0    <- col0 |> as.character() |> tolower()
  groups0 <- df0  |> pull(validGroups) |> as.character() |> tolower()
  valid0  <- groups0 |> str_detect(col0)
  # col0 |> print(); groups0 |> print(); valid0 |> print()

  ### Add new columns
  df0  <- df0 |> mutate(weightCol = col0)
  df0  <- df0 |> mutate(validType = valid0)
  df0  <- df0 |> mutate(valueAdj  = 1 * !(validType |> is.na()))

  ### Return
  return(df0)
}
