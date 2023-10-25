###### Overview ######
### This file contains helper functions for the FrEDI SV module.
###### get_sv_sectorInfo ######
#' Retrieve a character vector of sectors available in the FrEDI Social Vulnerability (SV) module ([FrEDI::run_fredi_sv]) or a data frame with SV sectors and additional information.
#'
#' @description
#' `get_sv_sectorInfo` returns a character vector with the names of sectors in the FrEDI Social Vulnerability (SV) module (default) **or** a data frame of SV sectors with additional information (e.g., associated variants, model type, etc.).
#'
#'
#' @param description Logical value indicating whether to include information about each sector. Returns a data frame if `description=TRUE` and returns a character vector of sector names if `description=FALSE` (default).
#'
#' @param gcmOnly     Logical value indicating whether to return only sectors with climate impacts modeled using global climate model (GCM) results.
#'
#' @param slrOnly     Logical value indicating whether to return only sectors with climate impacts modeled using sea level rise (SLR) scenarios.
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
###
get_sv_sectorInfo <- function(
    description=F,
    gcmOnly=F,
    slrOnly=F
){
  if(is.null(description)){description<-F}
  if(is.null(gcmOnly    )){gcmOnly    <-F}
  if(is.null(slrOnly    )){slrOnly    <-F}
  #svDataList <- load(file = "C:/Users/wmaddock/Documents/GitHub/FrEDI/createSystemData/data/sv/svDataList.rda")
  # co_sectorsRef$sector_label
  cData <- c("sectorInfo", "svSectorInfo")
  for(item_i in cData){assign(item_i, svDataList[[item_i]])}
  # print(sectorInfo)

  select0 <- c("sector", "modelType", "impactUnit")
  select1 <- c("sector", "modelType", "driverUnit", "variants", "impactUnit")
  ### - Select appropriate columns
  ### - Format modelType as uppercase
  ### - Add variant label
  ### - Add driver value
  sectorInfo <- sectorInfo |> select(c(all_of(select0)))
  sectorInfo <- sectorInfo |> mutate(modelType  = modelType |> toupper())
  sectorInfo <- sectorInfo |> mutate(
    variants = sector |> lapply(function(sector_i){
      variants_i <- (svSectorInfo |> filter(sector==sector_i))$variant_label |> paste(collapse=", ")
      return(variants_i)
    }) |> unlist())
  sectorInfo <- sectorInfo |> mutate(driverUnit=ifelse(modelType=="GCM", "degrees Celsius", "cm"))
  ### Select and arrange
  sectorInfo <- sectorInfo |> select(c(all_of(select1)))
  sectorInfo <- sectorInfo |> arrange_at(.vars=c("sector"))
  ### GCM or SLR
  doFilter   <- (gcmOnly | slrOnly)
  if(doFilter){
    cFilter    <- ifelse(gcmOnly, TRUE, FALSE)
    gcm_string <- "GCM"
    sectorInfo <- sectorInfo |> mutate(is_gcm =  modelType==gcm_string)
    sectorInfo <- sectorInfo |> filter(is_gcm == cFilter)
    sectorInfo <- sectorInfo |> select(-c("is_gcm"))
  }

  ### If not description, return names only
  if(!description) {return_obj <- sectorInfo$sector}
  else             {return_obj <- sectorInfo |> as.data.frame()}
  ### Return
  return(return_obj)
}


###### calc_countyPop ######
### Created 2022.02.14. Last updated 2022.02.14
### This function attempts to load a user-specified input file
### Use this function to calculate tract level impacts

calc_countyPop <- function(
    regPop,  ### Dataframe of population projection
    funList, ### Dataframe of population projections
    years = seq(2010, 2090, by=5)

){
  c_regions <- regPop$region |> unique()
  ### Iterate over regions
  x_popProj <- c_regions |> lapply(function(region_i){
    ### Subset population projection to a specific region
    ### Get states in the region
    ### Get unique years
    df_i     <- regPop |> filter(region==region_i)
    states_i <- funList[[region_i]] |> names()
    years_i  <- df_i$year |> unique() |> sort()

    ### Iterate over states
    regionPop_i <- states_i |> lapply(function(state_j){
      ### Function for state j
      fun_j   <- funList[[region_i]][[state_j]]$state2region

      # state_j |> print()
      df_j <- data.frame(x = years_i, y = fun_j(years_i))
      df_j <- df_j |> rename(year  = x, ratioState2RegionPop = y)
      df_j <- df_j |> mutate(state = state_j, region  = region_i)

      ### Get list of counties in the state
      geoids_j      <- funList[[region_i]][[state_j]]$county2state |> names()
      statePop_j    <- geoids_j |> lapply(function(geoid_k){
        fun_k <- funList[[region_i]][[state_j]]$county2state[[geoid_k]]
        if(!is.null(fun_k)) {y_k <- fun_k(years_i)}
        else                {y_k <- NA}
        df_k  <- data.frame(year = years_i, ratioCounty2StatePop = y_k)
        df_k  <- df_k |> mutate(state = state_j, geoid10 = geoid_k)
        return(df_k)
      }) |> (function(z){do.call(rbind, z)})()
      ### Join with state population
      df_j <- df_j |> left_join(statePop_j, by = c("state", "year"))
      return(df_j)
    }) |> (function(y){do.call(rbind, y)})()
    ### Join with regional population
    df_i <- df_i |> left_join(regionPop_i, by = c("region", "year"))
    df_i <- df_i |> mutate(state_pop  = region_pop * ratioState2RegionPop)
    df_i <- df_i |> mutate(county_pop = state_pop  * ratioCounty2StatePop)
    ### Return
    return(df_i)
  }) |> (function(x){do.call(rbind, x)})()
  ### Return
  x_popProj <- x_popProj |> as.data.frame()
  return(x_popProj)
}

###### calc_tractScaledImpacts ######
### Use this function to calculate tract level impacts
calc_tractScaledImpacts <- function(
    funList, ### List of impact functions
    driverValues, ### Dataframe of driver values for one scenario with columns driverValue, driverUnit, year
    xCol   = "driverValue",
    sleep  = 1e-7,
    silent = FALSE,
    .msg0  = ""
){
  ### Messaging
  msg0 <- .msg0
  msg1 <- msg0 |> paste("\t")
  msg2 <- msg1 |> paste("\t")
  msg3 <- msg2 |> paste("\t")
  msgUser <- !silent
  msg0 |> paste0("Calculating scaled impacts for each tract...") |> message()

  ### Names of functions
  c_tracts <- funList |> names()
  years_x  <- driverValues$year |> as.vector()
  values_x <- driverValues[,xCol] |> as.vector()
  funcs_x  <- funList |> names()
  # # c_tracts <- c_tracts[1:1e3]; funcs_x <- funcs_x[1:1e3]
  # c_tracts <- c(29031880500);
  # c_tracts <- seq(c_tracts - 10, c_tracts + 10);
  # c_tracts <- c_tracts |> as.character()

  ###### Iterate over Tracts ######
  data_x   <- c_tracts |> lapply(function(tract_i){
    ### Initialize data
    df_i      <- data.frame(year = years_x, fips = tract_i)
    y_i       <- NA
    ### Function for tract i and whether the function exists
    which_i   <- (funcs_x %in% c(tract_i)) |> which()
    fun_i     <- funList[[which_i]]
    has_fun_i <- (!is.null(fun_i))

    ### Calculate values if the function is not missing
    ### Add values to the dataframe
    if(has_fun_i){y_i <- fun_i(values_x)}
    ### Add values
    df_i      <- df_i |> mutate(sv_impact = y_i)
    # fun_i(1.667535543) |> print(); df_i |> print()
    ### Sleep and return
    # Sys.sleep(sleep)
    return(df_i)
  })
  ### Bind and join with driver values
  data_x <- data_x |> (function(x){do.call(rbind, x)})()
  data_x <- data_x |> left_join(driverValues, by = c("year"))

  ### Final time
  msg1 |> paste0("Finished calculating tract-level impacts.") |> message()

  return(data_x)
}

###### calc_terciles ######
calc_terciles <- function(data_x){
  ### Probability values for terciles
  n_quants <- 3
  c_probs  <- seq(0, 1, length.out=n_quants + 1)
  c_quants <- quantile(data_x, na.rm=T, probs = c_probs)
  c_cutoff <- c_quants[3]
  return(c_cutoff)
}

###### calc_tractImpacts ######
### Use this function to calculate tract level impacts
calc_tractImpacts <- function(
    scaledImpacts, ### Dataframe of scaled impacts by tract
    sector,
    popData, ### Dataframe of population projections
    svInfo    = NULL, ### Dataframe of sv data
    svGroups  = NULL, ### Character vector of sv group columns
    weightCol = NULL,
    years     = seq(2010, 2090, by=5),
    sleep     = 1e-5,
    silent    = FALSE,
    .msg0     = "",
    .testing  = FALSE
){
  ###### Constants  ######
  # paste0("Calculating total impacts for each tract...") |> message()
  x_sysTime1   <- Sys.time()
  regions      <- popData$region |> unique()
  tracts       <- scaledImpacts$fips |> unique()

  ###### Messages ######
  msg0 <- .msg0
  msg1 <- msg0 |> paste0("\t")
  msg2 <- msg1 |> paste0("\t")
  msg3 <- msg2 |> paste0("\t")
  msgUser <- !silent

  ###### Column Names  ######
  ### Other info
  c_svDataDropCols    <- c("svCounty")
  c_svOtherDropCols   <- c("state", "county", "geoid10") |>
    c("ratioTract2CountyPop", "ratioState2RegionPop", "ratioCounty2StatePop") |>
    c("region_pop", "state_pop", "county_pop")
  c_svJoinPopCols     <- c("region", "state", "geoid10")
  c_svJoinImpactsCols <- c("fips", "year")
  ### Columns to drop
  c_svNACols <- c()
  if     (sector=="Air Quality - Childhood Asthma"   ) {c_svNACols <- c("sv_noHS", "sv_plus65")}
  else if(sector=="Air Quality - Premature Mortality") {c_svNACols <- c("sv_plus65")}
  c_svGroupCols  <- svGroups[svGroups %in% names(svInfo)]
  c_svWeightCols <- c("children", "highRiskLabor", "sv_plus65")
  c_svWeightCols1<- c_svWeightCols |> (function(x){x[which(!(x %in% c("sv_plus65")))]})()

  ###### Other Info ######
  ### Eventually, import from svDemographics
  c_sector    <- sector
  weightsCol  <- weightCol #; weightsCol |> print()
  svGroupCols <- svGroups[svGroups %in% names(svInfo)]
  # svGroups |> print(); svGroupCols |> print()

  ###### Format Data ######
  popData       <- popData       |> filter(year %in% years)
  scaledImpacts <- scaledImpacts |> filter(year %in% years) |> filter(!is.na(driverUnit))

  ###### Total Impacts  ######
  msg0 |> paste0("Calculating total impacts for each tract...") |> message()
  ### Format svInfo - Add column for none and drop other columns
  c_dropCols0 <- c("svCounty")
  x_impacts   <- svInfo |> mutate(none = 1) |> select(-c(all_of(c_dropCols0)))
  rm("svInfo"); rm("c_dropCols0")
  ### Join svInfo with population projections by region, state, geoid10
  c_joinCols0 <- c("region", "state", "geoid10")
  x_impacts   <- x_impacts |> left_join(popData, by = all_of(c_joinCols0))
  rm("popData"); rm("c_joinCols0")
  ### Join svInfo with the impacts by fips number and drop missing values
  x_impacts   <- x_impacts |> left_join(scaledImpacts, by = c("year", "fips"))
  x_impacts   <- x_impacts |> filter(!is.na(driverUnit))
  rm("scaledImpacts")
  # ### Sleep
  # Sys.sleep(sleep)


  ###### Population Weight ######
  ### Add population weight column
  c_weightCols <- c("children", "highRiskLabor") |> (function(y){y[y %in% names(x_impacts)]})()
  x_impacts   <- x_impacts |> mutate(popWeight = x_impacts[[weightsCol]])
  x_impacts   <- x_impacts |> select(-c(all_of(c_weightCols)))
  # (x_impacts$popWeight != 0) |> which() |> length() |> print()
  rm("c_weightCols")

  ###### Tract Population ######
  ### Calculate total tract population and drop columns
  c_dropCols1   <- c("state", "county", "geoid10") |>
    c("ratioTract2CountyPop", "ratioState2RegionPop", "ratioCounty2StatePop") |>
    c("region_pop", "state_pop", "county_pop")
  x_impacts   <- x_impacts |> mutate(tract_totPop = county_pop * ratioTract2CountyPop)
  x_impacts   <- x_impacts |> select(-c(all_of(c_dropCols1)))
  # (x_impacts$tract_totPop != 0) |> which() |> length() |> print()
  rm("c_dropCols1")

  ###### Non-Meaningful Groups ######
  ### Convert values for non-meaningful SV groups to zero
  if     (sector=="Air Quality - Childhood Asthma"   ) {c_svNACols <- c("sv_noHS", "sv_plus65")}
  else if(sector=="Air Quality - Premature Mortality") {c_svNACols <- c("sv_plus65")}
  else                                                 {c_svNACols <- c()}
  x_impacts   <- x_impacts |> mutate_at(.vars=c(all_of(c_svNACols)), function(z){0})
  rm("c_svNACols")

  ######  National Terciles ######
  # x_impacts |> glimpse()
  if(msgUser) {msg1 |> paste0("Calculating national terciles...") |> message()}
  else        {msg2 |> paste0("...") |> message()}
  ### Columns
  groupsNat0    <- c("year")
  tractNat0     <- c("national_highRiskTract")
  cutoffNat0    <- c("national_cutoff")
  scaledImpact0 <- c("sv_impact")
  ### Calculate terciles and rename column
  quants_national <- x_impacts       |> select(c(all_of(groupsNat0), all_of(scaledImpact0)))
  quants_national <- quants_national |>
    group_by_at (.vars = c(all_of(groupsNat0))) |>
    summarize_at(.vars = c(all_of(scaledImpact0)), calc_terciles) |> ungroup()
  ### Rename
  quants_national <- quants_national |> rename_at(.vars=all_of(scaledImpact0), ~all_of(cutoffNat0))
  if(.testing){quants_national |> filter(year==2050) |> glimpse()}
  ### Join with national quantiles
  if(msgUser) {msg2 |> paste0("Joining national terciles to tract-level data...") |> message()}
  else        {msg3 |> paste0(msg1, "...") |> message()}
  x_impacts <- x_impacts |> left_join(quants_national, by = c(all_of(groupsNat0)));
  rm("quants_national");
  ### Figure out which tracts are high risk
  ### Calculate high risk populations
  if(msgUser) {msg2 |> paste0("Calculating national high risk populations...") |> message()}
  else        {msg3 |> paste0(msg1, "...") |> message()}
  x_impacts[[tractNat0]] <- (x_impacts[[scaledImpact0]] > x_impacts[[cutoffNat0]]) * 1
  x_impacts <- x_impacts |> select(-c(all_of(cutoffNat0)));
  rm("cutoffNat0")
  # Sys.sleep(sleep)

  ###### Regional Terciles ######
  if(msgUser) {msg1 |> paste0("Calculating regional terciles...") |> message()}
  else        {msg3 |> paste0("...") |> message()}
  ### Columns
  groupsReg0    <- c(groupsNat0, "region")
  tractReg0     <- c("regional_highRiskTract")
  cutoffReg0    <- c("regional_cutoff")
  ### 126 rows
  ### Calculate terciles and rename column
  quants_regional  <- x_impacts |> select(c(all_of(groupsReg0), all_of(scaledImpact0)))
  quants_regional  <- quants_regional |>
    group_by_at(.vars=c(all_of(groupsReg0))) |>
    summarize_at(.vars=c(all_of(scaledImpact0)), calc_terciles) |> ungroup()
  ### Rename
  quants_regional <- quants_regional |> rename_at(.vars=all_of(scaledImpact0), ~all_of(cutoffReg0))
  if(.testing){quants_regional |> filter(year==2050) |> glimpse()}
  ### Join with regional quantiles
  if(msgUser){msg2 |> paste0("Joining regional terciles to tract-level data...") |> message()}
  else       {msg3 |> paste0(msg1, "...") |> message()}
  x_impacts <- x_impacts |> left_join(quants_regional, by = c(all_of(groupsReg0)));
  rm("quants_regional");
  ### Regional High Risk Tracts
  ### Figure out which tracts are high risk
  ### Calculate high risk populations
  if(msgUser) {msg2 |> paste0("Calculating regional high risk populations...") |> message()}
  else        {msg3 |> paste0("...") |> message()}
  x_impacts[[tractReg0]] <- (x_impacts[[scaledImpact0]] > x_impacts[[cutoffReg0]]) * 1
  x_impacts <- x_impacts |> select(-c(all_of(cutoffReg0)));
  rm("cutoffReg0")
  # Sys.sleep(sleep)

  ###### Total Impacts ######
  ### - Impacts = population*popWeight
  c_pop0      <- c("tract_totPop")
  c_impPop0   <- c("tract_impPop")
  c_impact0   <- c("tract_impact")
  ### - Impacted population (e.g., children for Air Quality) (Impacted population = population*popWeight)
  x_impacts[[c_impPop0]] <- x_impacts[[c_pop0   ]] * x_impacts[["popWeight"]]
  # (x_impacts$tract_impPop != 0) |> which() |> length() |> print()
  ### - Calculate SV impacts for ref pop and impacted SV pop (Impacts = impacted population*sv_impact)
  x_impacts[[c_impact0]] <- x_impacts[[c_impPop0]] * x_impacts[["sv_impact"]]
  # x_impacts |> print() # (x_impacts$tract_impact != 0) |> which() |> length() |> print()
  ### Drop columns
  # x_impacts   <- x_impacts |> select(-c("popWeight", "sv_impact", all_of(c_pop0)))
  x_impacts   <- x_impacts |> select(-c(all_of(c_pop0)))
  rm("c_pop0")

  ###### Gather Groups ######
  ### Gather by svGroupType: all the main SV variables, and racial vars
  x_impacts   <- x_impacts |> gather(key = "svGroupType", value = "svRatio2Ref", c(all_of(svGroups)))

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
  rm("svRatio2Ref0"); rm("sumCols0", "refSumCols0", "svSumCols0")
  ### Calculate high-risk populations
  # popNat0      <- c("national_highRiskPop")
  popNat1      <- c("national_highRiskPop") |> paste(c_suffix0, sep="_")
  popReg1      <- c("regional_highRiskPop") |> paste(c_suffix0, sep="_")
  x_impacts[,popNat1] <-  x_impacts[,c_impPop1] * x_impacts[[tractNat0]]
  x_impacts[,popReg1] <-  x_impacts[,c_impPop1] * x_impacts[[tractReg0]]
  rm("tractNat0", "tractReg0")

  ###### Regional Summaries ######
  if(!.testing){
    if(msgUser){msg1 |> paste0( "Calculating regional summaries...") |> message()}
    sumCols0     <- c(c_impPop1, c_impact1) |> c(popNat1, popReg1)
    groupCols0   <- c("region", "svGroupType", "driverUnit", "driverValue", "year")
    ### Group by the grouping columns and summarize the summary columns
    x_impacts    <- x_impacts |>
      group_by_at (.vars=c(all_of(groupCols0))) |>
      summarize_at(.vars=c(all_of(sumCols0)), sum, na.rm=T) |> ungroup()
    # (x_impacts$tract_impPop_ref != 0) |> which() |> length() |> print()
    ### Select all of the relevant columns
    x_impacts    <- x_impacts |> select(c(all_of(groupCols0), all_of(sumCols0)))
    ### Replace tract in summary names
    x_impacts    <- x_impacts |> rename_at(.vars=c(all_of(sumCols0)), ~gsub("tract_", "", sumCols0));
    rm("sumCols0", "groupCols0")

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
    rm("which0_ref", "which0_sv")
  }

  # Sys.sleep(sleep)

  ### Dataframe
  x_impacts     <- x_impacts |> as.data.frame()

  ###### Return ######
  msg1 |> paste0("Finished calculating total impacts.") |> message()
  return(x_impacts)
}

###### get_validGroups ######
get_validGroups <- function(
    df0, ### svDemoInfo
    df1, ### svValidTypes
    col0 = "none" ### c_popWtCol
){
  ### Column names
  old0 <- c("colName"    , "valid_popWeightCols")
  new0 <- c("svGroupType", "validGroups")
  ### Reshape svDemoInfo
  df0  <- df0 |> filter(colType %in% c("bipoc")) |> select(c(old0[1]))
  df0  <- df0 |> rename_at(.vars=c(old0[1]), ~c(new0[1]))
  # df0  <- df0 |> mutate(validGroups = "none")
  # children, highRiskLabor, sv_plus65, none
  df0  <- df0 |> mutate(validGroups = "children, highRiskLabor, sv_plus65, none")
  ### Reshape svValidTypes
  df1  <- df1 |> select(c(new0[1], old0[2]))
  df1  <- df1 |> rename_at(.vars=c(old0[2]), ~c(new0[2]))
  ### Bind
  df0  <- df1 |> rbind(df0)
  rm("df1", "old0", "new0")

  ### Calculate weight columns
  col0    <- col0 |> as.character() |> tolower()
  groups0 <- df0$validGroups |> as.vector() |> as.character() |> tolower()
  valid0  <- groups0 |> str_match(col0) |> unlist() |> as.vector()
  # col0 |> print(); groups0 |> print(); valid0 |> print()

  df0  <- df0 |> mutate(weightCol = col0)
  df0  <- df0 |> mutate(validType = valid0)
  df0  <- df0 |> mutate(valueAdj  = (1*!is.na(validType)))
  # df0 |> glimpse()
  ### Return
  return(df0)
}
