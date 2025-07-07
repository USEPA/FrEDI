###### Overview ######
### This file contains helper functions for the FrEDI SV module.
###### get_sv_sectorInfo ######
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
  ### Get objects
  cData        <- c("sectorInfo", "svSectorInfo")
  for(item_i in cData){item_i |> assign(svDataList[[item_i]])}

  ### Select appropriate columns
  ### Format modelType as uppercase
  select0      <- c("sector", "impactUnit")
  sectorInfo   <- sectorInfo |> select(all_of(select0))
  rm(select0)

  ### Select and arrange
  group0       <- c("sector", "modelType", "driverUnit")
  rename0      <- c("driverUnit_label", "variant_label")
  renameTo     <- c("driverUnit", "variants")
  svSectorInfo <- svSectorInfo |> rename_at(c(rename0), ~renameTo)
  svSectorInfo <- svSectorInfo |> mutate(modelType = modelType |> toupper())
  svSectorInfo <- svSectorInfo |>
    group_by_at(c(group0)) |>
    summarize(variants = variants |> paste(collapse=", "), .groups="drop")

  ### Join with sector info
  join0      <- "sector"
  move0      <- c("modelType", "driverUnit")
  sectorInfo <- sectorInfo |> left_join(svSectorInfo, by=c(join0))
  sectorInfo <- sectorInfo |> relocate(all_of(move0), .after=all_of(join0))
  sectorInfo <- sectorInfo |> arrange_at(c(join0))

  ### GCM or SLR
  doFilter   <- (gcmOnly | slrOnly)
  string0    <- gcmOnly |> ifelse("GCM", "SLR")
  if(doFilter) {sectorInfo <- sectorInfo |> filter(modelType==string0)}

  ### If not description, return sectors
  if(!description) {
    sectorInfo <- sectorInfo |> pull(sector)
    return(sectorInfo)
  } ### End if(!description)

  ### Return
  return(sectorInfo)
}


###### calc_countyPop ######
calc_countyPop <- function(
    fun0 = NULL, ### Function from list svPopList[["popProjList"]]
    df0,         ### Tibble with population projections
    xCol0 = "year", ### X column in df0
    yCol0 = "pop"   ### Y column in df0
){
  ### Check function
  hasFun0 <- !(fun0 |> is.null())
  if(hasFun0){y0 <- df0 |> pull(all_of(xCol0)) |> fun0()} else{y0 <- NULL}
  df0     <- df0 |> mutate(state2county = y0)
  rm(y0)

  ### Calculate county population
  # df0     <- df0 |> mutate(county_pop = pop * state2county)
  df0     <- df0 |> mutate(county_pop = !!sym(xCol0) * state2county)

  ### Select columns
  drop0   <- c("pop", "state2county")
  df0     <- df0 |> select(-all_of(drop0))
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
  ### Get unique states and regions
  select0  <- c("region", "state")
  pList0   <- df0    |> select(all_of(select0)) |> unique()
  states0  <- pList0 |> pull(state)

  ### Iterate over states:
  list0    <- states0 |> as.list() |> set_names(states0)
  for(state_i in states0) {
    ### Get region
    region_i <- pList0 |> filter(state == state_i) |> pull(region)
    ### Filter data to state
    df_i     <- df0 |> filter(state == state_i)
    ### Get list of state functions
    fList_i  <- funList[[region_i]][[state_i]][["county2state"]]
    geoids_i <- fList_i |> names()
    ### Initialize dataframe for results
    pops_i   <- fList_i |> map(function(j){
      j |> calc_countyPop(df0=df_i, xCol0=xCol0, yCol0=yCol0)
    }) |> set_names(geoids_i) |> bind_rows(.id="geoid10")

    ### Add county populations to list
    list0[[state_i]] <- pops_i
    rm(state_i, region_i, df_i, fList_i, geoids_i, pops_i)
  } ### for(state_i in states0)

  ### Bind rows
  list0    <- list0 |> bind_rows()

  ### Return
  gc()
  return(list0)
}


###### calc_tractScaledImpacts ######
### Use this function to calculate tract level impacts
calc_tractScaledImpacts <- function(
    funList,      ### List of impact functions
    driverValues, ### Dataframe of driver values for one scenario with columns driverValue, driverUnit, year
    xCol   = "driverValue",
    sleep  = 1e-7,
    silent = FALSE,
    .msg0  = ""
){
  ### Messaging
  msg0     <- .msg0
  msg1     <- msg0 |> paste("\t")
  msg2     <- msg1 |> paste("\t")
  msg3     <- msg2 |> paste("\t")
  msgUser  <- !silent
  msg0 |> paste0("Calculating scaled impacts for each tract...") |> message()

  ### Names of functions
  c_tracts <- funList |> names()
  years_x  <- driverValues |> pull(year)
  values_x <- driverValues |> pull(all_of(xCol))
  funcs_x  <- funList |> names()

  ###### Iterate over Tracts
  list_x   <- list()
  for(tract_i in c_tracts) {
    ### Initialize data
    df_i <- driverValues
    df_i <- df_i |> mutate(fips      = tract_i)
    df_i <- df_i |> mutate(sv_impact = NA)
    ### Function for tract i and whether the function exists
    fun_i     <- funList[[tract_i]]
    has_fun_i <- !(fun_i |> is.null()) & "function" %in% (fun_i |> class())
    # if(has_fun_i){tract_i |> print()}
    # if(tract_i == "12009980100") fun_i |> print()
    # if(has_fun_i){ df_i <- df_i |> mutate(sv_impact = values_x |> fun_i()) |> try(silent=T) }
    # if("try-error" %in% (df_i |> class())){tract_i |> print(); df_i <- NULL}
    if(has_fun_i){ df_i <- df_i |> mutate(sv_impact = values_x |> fun_i())}
    list_x[[tract_i]] <- df_i
    rm(tract_i, df_i, fun_i, has_fun_i)
  } ### for(tract_i in c_tracts)

  ### Bind and join with driver values
  list_x <- list_x |> set_names(c_tracts) |> bind_rows()

  ### Final time
  msg1 |> paste0("Finished calculating tract-level impacts.") |> message()

  ### Return
  gc()
  return(list_x)
}

###### calc_terciles ######
calc_terciles <- function(data_x){
  ### Probability values for terciles
  n_quants <- 3
  c_probs  <- 0      |> seq(1, length.out=n_quants + 1)
  c_quants <- data_x |> quantile(na.rm=T, probs = c_probs)
  c_cutoff <- c_quants[3]
  ### Return
  return(c_cutoff)
}

###### calc_tractImpacts ######
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
    .msg0     = "",
    .testing  = FALSE
){
  ###### Constants  ######
  regions   <- popData       |> pull(region) |> unique()
  tracts    <- scaledImpacts |> pull(fips  ) |> unique()

  ###### Messages ######
  msg0      <- .msg0
  msg1      <- msg0 |> paste0("\t")
  msg2      <- msg1 |> paste0("\t")
  msg3      <- msg2 |> paste0("\t")
  msgUser   <- !silent

  ###### Format Data ######
  popData       <- popData       |> filter(year %in% years)
  scaledImpacts <- scaledImpacts |> filter(year %in% years)
  scaledImpacts <- scaledImpacts |> filter(!(driverUnit |> is.na()))

  ###### Total Impacts  ######
  msg0 |> paste0("Calculating total impacts for each tract...") |> message()
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

  ###### Population Weight ######
  ### Add population weight column
  drop0       <- c("children", "highRiskLabor")
  rename0     <- c(weightCol)
  renameTo    <- c("popWeight")
  x_impacts[["popWeight"]]   <- x_impacts[[weightCol]]
  x_impacts   <- x_impacts |> select(-any_of(drop0))
  rm(drop0, rename0, renameTo)

  ###### Tract Population ######
  ### Calculate total tract population and drop columns
  drop0       <- c("state", "county", "geoid10", "region_pop", "pop", "county_pop")
  # drop0       <- c("region_pop", "pop", "county_pop")
  x_impacts   <- x_impacts |> mutate(tract_totPop = county_pop * ratioTract2CountyPop)
  x_impacts   <- x_impacts |> select(-any_of(drop0))
  rm(drop0)

  ###### Non-Meaningful Groups ######
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

  ######  National Terciles ######
  if(msgUser) msg1 |> paste0("Calculating national terciles...") |> message()
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
  if(msgUser) {msg2 |> paste0("Joining national terciles to tract-level data...") |> message()}
  #browser()
  x_impacts <- x_impacts |> left_join(quants_national, by=groupsNat0)
  rm(quants_national)

  ### Figure out which tracts are high risk
  ### Calculate high risk populations
  if(msgUser) {msg2 |> paste0("Calculating national high risk populations...") |> message()}
  x_impacts[[tractNat0]] <- (x_impacts[[scaledImpact0]] > x_impacts[[cutoffNat0]]) * 1
  x_impacts <- x_impacts |> select(-all_of(cutoffNat0))
  rm(cutoffNat0)

  ###### Regional Terciles ######
  if(msgUser) {msg1 |> paste0("Calculating regional terciles...") |> message()}
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
  if(msgUser){msg2 |> paste0("Joining regional terciles to tract-level data...") |> message()}
  x_impacts <- x_impacts |> left_join(quants_regional, by=groupsReg0)
  rm(quants_regional)

  ### Regional High Risk Tracts
  ### Figure out which tracts are high risk
  ### Calculate high risk populations
  if(msgUser) {msg2 |> paste0("Calculating regional high risk populations...") |> message()}
  x_impacts[[tractReg0]] <- (x_impacts[[scaledImpact0]] > x_impacts[[cutoffReg0]]) * 1
  x_impacts <- x_impacts |> select(-all_of(cutoffReg0))
  rm(cutoffReg0)

  ###### Total Impacts ######
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

  ###### Gather Groups ######
  ### Gather by svGroupType: all the main SV variables, and racial vars
  x_impacts   <- x_impacts |> pivot_longer(
    cols      = all_of(svGroups),
    names_to  = "svGroupType",
    values_to = "svRatio2Ref"
  ) ### End pivot_longer

  ###### Tract Values ######
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

  ###### State Summaries ######
  if(!.testing){
    if(msgUser){msg1 |> paste0( "Calculating state summaries...") |> message()}
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

    ###### Average Rates ######
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


  ###### Return ######
  msg1 |> paste0("Finished calculating total impacts.") |> message()
  gc()
  return(x_impacts)
}

###### get_validGroups ######
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
