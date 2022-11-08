###### Overview ######
### This file contains helper functions for the FrEDI SV module.
###### get_sv_sectorInfo ######
#' This function provides information about the sectors in the FrEDI SV Module.
#'
#' @description
#' This helper function returns a character vector with the names of sectors in the FrEDI SV module (default) *or* a data frame of those sectors with related information (`description=TRUE`). If `description=TRUE`, [FrEDI::get_sv_sectorInfo] will return a data frame with sector names, model type ("GCM" or "SLR") and associated driver unit ("degrees Celsius" or "cm", respectively ), impact units (e.g., mortality, etc.), available variants. Users can use [FrEDI::get_sv_sectorInfo] to determine which sectors can be passed to the [FrEDI::run_sv_fredi()] `sector` argument and/or to determine whether a particular sector is driven primarily by temperature (`modelType="GCM"`) or sea level rise (`modelType="SLR"`).
#'
#' @param description=FALSE. Logical value indicating whether to include information about each sector. Returns a data frame if `description=TRUE` and returns a character vector of sector names if `description=FALSE` (default).
#' @param gcmOnly=FALSE. Logical value indicating whether to return only sectors with climate impacts modeled using global climate model (GCM) results.
#' @param slrOnly=FALSE. Logical value indicating whether to return only sectors with climate impacts modeled using sea level rise (SLR) scenarios.
#'
#' @details
#' This helper function returns a character vector with the names of sectors in FrEDI (default) *or* a data frame of those sectors with related information (`description=TRUE`). If `description=TRUE`, [FrEDI::get_sv_sectorInfo] will return a data frame with sector names, model type ("GCM" or "SLR") and associated driver unit ("degrees Celsius" or "cm", respectively ), impact units (e.g., mortality, etc.), and available variants. Users can use [FrEDI::get_sv_sectorInfo] to determine which sectors can be passed to the [FrEDI::run_sv_fredi()] `sector` argument and/or to determine whether a particular sector is driven primarily by temperature (`modelType="GCM"`) or sea level rise (`modelType="SLR"`).
#'
#' If `description=FALSE` (default), this helper function returns a character vector of names of sectors that can be passed to the [FrEDI::run_sv_fredi()] `sector` argument. If `description=TRUE`, `get_sv_sectorInfo()` returns a data frame of sectors with related information, such as whether a particular sector is driven primarily by temperature (`modelType="GCM"`) or sea level rise (`modelType="SLR"`).
#'
#' Users can specify whether to return only GCM sectors *or* SLR sectors by setting `gcmOnly=TRUE` or `slrOnly=TRUE`, respectively. [FrEDI::get_sv_sectorInfo()] will return the sectors in the form specified by `description` (see above).
#'
#' @return
#'
#' * If `description=FALSE` (default), outputs a character vector containing the names of sectors available for the FrEDI SV Module.
#' * If `description=TRUE`, `, outputs a data frame containing the names of sectors available for the FrEDI SV Module in one column, with information about the sector model type, model type ("GCM" or "SLR") and associated driver unit ("degrees Celsius" or "cm", respectively ),  impact units (e.g., mortality, etc.), and available variants in the remaining columns.
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
  sectorInfo <- sectorInfo %>% select(c(all_of(select0)))
  sectorInfo <- sectorInfo %>% mutate(modelType  = modelType %>% toupper)
  sectorInfo <- sectorInfo %>% mutate(
    variants = sector %>% lapply(function(sector_i){
      variants_i <- (svSectorInfo %>% filter(sector==sector_i))$variant_label %>% paste(collapse=", ")
      return(variants_i)
    }) %>% unlist)
  sectorInfo <- sectorInfo %>% mutate(driverUnit=ifelse(modelType=="GCM", "degrees Celsius", "cm"))
  ### Select and arrange
  sectorInfo <- sectorInfo %>% select(c(all_of(select1)))
  sectorInfo <- sectorInfo %>% arrange_at(.vars=c("sector"))
  ### GCM or SLR
  doFilter   <- (gcmOnly | slrOnly)
  if(doFilter){
    cFilter    <- ifelse(gcmOnly, TRUE, FALSE)
    gcm_string <- "GCM"
    sectorInfo <- sectorInfo %>% mutate(is_gcm =  modelType==gcm_string)
    sectorInfo <- sectorInfo %>% filter(is_gcm == cFilter)
    sectorInfo <- sectorInfo %>% select(-c("is_gcm"))
  }

  ### If not description, return names only
  if(!description) {return_obj <- sectorInfo$sector}
  else             {return_obj <- sectorInfo %>% as.data.frame}
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
  c_regions <- regPop$region %>% unique
  ### Iterate over regions
  x_popProj <- c_regions %>% lapply(function(region_i){
    ### Subset population projection to a specific region
    ### Get states in the region
    ### Get unique years
    df_i     <- regPop %>% filter(region==region_i)
    states_i <- funList[[region_i]] %>% names
    years_i  <- df_i$year %>% unique %>% sort

    ### Iterate over states
    regionPop_i <- states_i %>% lapply(function(state_j){
        ### Function for state j
        fun_j   <- funList[[region_i]][[state_j]]$state2region

        # state_j %>% print
        df_j <- data.frame(x = years_i, y = fun_j(years_i))
        df_j <- df_j %>% rename(year  = x, ratioState2RegionPop = y)
        df_j <- df_j %>% mutate(state = state_j, region  = region_i)

        ### Get list of counties in the state
        geoids_j      <- funList[[region_i]][[state_j]]$county2state %>% names
        statePop_j    <- geoids_j %>% lapply(function(geoid_k){
          fun_k <- funList[[region_i]][[state_j]]$county2state[[geoid_k]]
          if(!is.null(fun_k)) {y_k <- fun_k(years_i)}
          else                {y_k <- NA}
          df_k  <- data.frame(year = years_i, ratioCounty2StatePop = y_k)
          df_k  <- df_k %>% mutate(state = state_j, geoid10 = geoid_k)
          return(df_k)
        }) %>% (function(z){do.call(rbind, z)})
        ### Join with state population
        df_j <- df_j %>% left_join(statePop_j, by = c("state", "year"))
        return(df_j)
      }) %>% (function(y){do.call(rbind, y)})
    ### Join with regional population
    df_i <- df_i %>% left_join(regionPop_i, by = c("region", "year"))
    df_i <- df_i %>% mutate(state_pop  = region_pop * ratioState2RegionPop)
    df_i <- df_i %>% mutate(county_pop = state_pop  * ratioCounty2StatePop)
    ### Return
    return(df_i)
  }) %>% (function(x){do.call(rbind, x)})
  ### Return
  x_popProj <- x_popProj %>% as.data.frame
  return(x_popProj)
}

###### calc_tractScaledImpacts ######
### Use this function to calculate tract level impacts
calc_tractScaledImpacts <- function(
    funList, ### List of impact functions
    # scenarios, ### Character vector of scenarios
    # driverValues, ### Dataframe of driver values for one scenario with columns driverValue, driverUnit, year
    # tracts ### Character vectors of tracts
    driverValues, ### Dataframe of driver values for one scenario with columns driverValue, driverUnit, year
    xCol  = "driverValue",
    sleep = 1e-7,
    silent = FALSE,
    .msg0 = ""
){
  ### Messaging
  msg0 <- .msg0
  msg1 <- msg0 %>% paste("\t")
  msg2 <- msg1 %>% paste("\t")
  msg3 <- msg2 %>% paste("\t")
  msgUser <- !silent
  msg0 %>% paste0("Calculating scaled impacts for each tract...") %>% message

  ### Names of functions
  c_tracts <- funList %>% names
  years_x  <- driverValues$year %>% as.vector
  values_x <- driverValues[,xCol] %>% as.vector
  funcs_x  <- funList %>% names

  ###### Iterate over Tracts ######
  data_x   <- c_tracts %>% lapply(function(tract_i){
    ### Initialize data
    df_i      <- data.frame(year = years_x, fips = tract_i)
    y_i       <- NA
    ### Function for tract i and whether the function exists
    which_i   <- (funcs_x %in% c(tract_i)) %>% which
    fun_i     <- funList[[which_i]]
    has_fun_i <- (!is.null(fun_i))

    ### Calculate values if the function is not missing
    ### Add values to the dataframe
    if(has_fun_i){y_i <- fun_i(values_x)}
    ### Add values
    df_i      <- df_i %>% mutate(sv_impact = y_i)
    ### Sleep and return
    # Sys.sleep(sleep)
    return(df_i)
  })
  ### Bind and join with driver values
  data_x <- data_x %>% (function(x){do.call(rbind, x)})
  data_x <- data_x %>% left_join(driverValues, by = c("year"))

  ### Final time
  msg1 %>% paste0("Finished calculating tract-level impacts.") %>% message

  return(data_x)
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
    .msg0     = ""
){
  ###### Constants  ######
  # paste0("Calculating total impacts for each tract...") %>% message
  x_sysTime1   <- Sys.time()
  regions      <- popData$region %>% unique
  tracts       <- scaledImpacts$fips %>% unique
  # c_years_eras <- seq(2000, 2090, by = 5)
  # c_years_eras <- fredi_config$list_years_by5

  ###### Messages ######
  msg0 <- .msg0
  msg1 <- msg0 %>% paste0("\t")
  msg2 <- msg1 %>% paste0("\t")
  msg3 <- msg2 %>% paste0("\t")
  msgUser <- !silent

  ###### Sector Info ######
  ### Eventually, import from svDemographics
  c_sector    <- sector
  weightsCol  <- weightCol #; weightsCol %>% print

  ###### Column Names  ######
  ### Other info
  c_suffix0     <- c("ref", "sv")
  c_svGroupCols <- svGroups[svGroups %in% names(svInfo)]
  # svGroups %>% print; c_svGroupCols %>% print

  ###### Join Data ######
  msg0 %>% paste0("Calculating total impacts for each tract...") %>% message
  ### Format svInfo - Add column for none and drop other columns
  c_dropCols0 <- c("svCounty")
  x_impacts   <- svInfo %>% mutate(none = 1) %>% select(-c(all_of(c_dropCols0)))
  rm("svInfo"); rm("c_dropCols0")
  ### Join svInfo with population projections by FIPS
  # x_impacts %>% head %>% glimpse %>% print; popData %>% head %>% glimpse %>% print
  c_joinCols0 <- c("region", "state", "geoid10")
  x_impacts   <- x_impacts %>% left_join(popData, by = all_of(c_joinCols0))
  rm("popData"); rm("c_joinCols0")
  ### Join svInfo with the impacts by fips number
  # x_impacts %>% head %>% glimpse %>% print; scaledImpacts %>% head %>% glimpse %>% print
  x_impacts   <- x_impacts %>% left_join(scaledImpacts, by = c("year", "fips"))
  rm("scaledImpacts")
  ### Filter out that with missing data (!is.na(driverUnit))
  x_impacts   <- x_impacts %>% filter(year %in% years) %>% filter(!is.na(driverUnit))
  ### Sleep
  Sys.sleep(sleep)

  ###### Population Weight ######
  ### Add population weight column
  c_weightCols <- c("children", "highRiskLabor") %>% (function(y){y[y %in% names(x_impacts)]})
  x_impacts   <- x_impacts %>% mutate(popWeight = x_impacts[[weightsCol]])
  x_impacts   <- x_impacts %>% select(-c(all_of(c_weightCols)))
  rm("c_weightCols")

  ###### Tract Population ######
  ### Calculate total tract population and drop columns
  c_dropCols1   <- c("state", "county", "geoid10") %>%
    c("ratioTract2CountyPop", "ratioState2RegionPop", "ratioCounty2StatePop") %>%
    c("region_pop", "state_pop", "county_pop")
  x_impacts   <- x_impacts %>% mutate(tract_totPop_tot = county_pop * ratioTract2CountyPop)
  x_impacts   <- x_impacts %>% select(-c(all_of(c_dropCols1)))
  rm("c_dropCols1")

  ###### Non-Meaningful Groups ######
  ### Convert values for non-meaningful SV groups to zero
  if     (sector=="Air Quality - Childhood Asthma"   ) {c_svNACols <- c("sv_noHS", "sv_plus65")}
  else if(sector=="Air Quality - Premature Mortality") {c_svNACols <- c("sv_plus65")}
  else                                                 {c_svNACols <- c()}
  x_impacts   <- x_impacts %>% mutate_at(.vars=c(all_of(c_svNACols)), function(z){0})
  rm("c_svNACols")

  ###### Gather Groups ######
  ### Gather by svGroupType: all the main SV variables, and racial vars
  ### - Minority and racial vars must be calculated separately
  x_impacts   <- x_impacts %>% gather(key = "svGroupType", value = "svRatio2Ref", c(all_of(svGroups)))

  ###### Tract Values ######
  ### - Calculate SV ref pop ("refPop") and weighted (impacted) SV pop ("impactPop")
  x_impacts   <- x_impacts %>% mutate(tract_totPop_sv   = tract_totPop_tot * svRatio2Ref)
  x_impacts   <- x_impacts %>% mutate(tract_totPop_ref  = tract_totPop_tot - tract_totPop_sv)
  x_impacts   <- x_impacts %>% select(-c("svRatio2Ref"))

  ### - Impacts = population*popWeight
  c_pop0      <- c("tract_totPop") %>% paste(c_suffix0, sep="_")
  c_impPop0   <- c("tract_impPop") %>% paste(c_suffix0, sep="_")
  c_impact0   <- c("tract_impact") %>% paste(c_suffix0, sep="_")
  ### - Impacted population (e.g., children for Air Quality) (Impacted population = population*popWeight)
  x_impacts[, c_impPop0] <- x_impacts[, c_pop0   ] * x_impacts$popWeight
  ### - Calculate SV impacts for ref pop and impacted SV pop (Impacts = impacted population*sv_impact)
  x_impacts[, c_impact0] <- x_impacts[, c_impPop0] * x_impacts$sv_impact
  ### Drop columns
  x_impacts   <- x_impacts %>% select(-c("popWeight"))
  rm("c_pop0")

  ###### Tertiles ######
  ### Probability values for tertiles
  n_quants        <- 3
  c_probs         <- seq(0, 1, length.out=n_quants + 1)
  ### Columns
  c_groupsNat0    <- c("year", "svGroupType")
  c_groupsReg0    <- c(c_groupsNat0, "region")
  c_sum0          <- c("tract_impact_sv")
  c_quantColsNat  <- c("national_highRiskPop") %>% paste(c_suffix0, sep="_")
  c_quantColsReg  <- c("regional_highRiskPop") %>% paste(c_suffix0, sep="_")

  ######  National Tertiles ######
  # x_impacts %>% glimpse
  if(msgUser) {msg1 %>% paste0("Calculating national tertiles...") %>% message}
  else        {msg2 %>% paste0("...") %>% message}
  quants_national <- x_impacts       %>% select(c(all_of(c_groupsNat0), all_of(c_sum0)))
  quants_national <- quants_national %>%
    group_by_at(.vars=c(all_of(c_groupsNat0))) %>%
    summarize_at(.vars=c(all_of(c_sum0)), function(x){quantile(x, na.rm=T, probs = c_probs)[3]}) %>%
    ungroup %>% rename(national_cutoff = tract_impact_sv)
  ### Join with national quantiles
  if(msgUser) {msg2 %>% paste0("Joining national tertiles to tract-level data...") %>% message}
  else        {msg3 %>% paste0(msg1, "...") %>% message}
  x_impacts <- x_impacts %>% left_join(quants_national, by = c(all_of(c_groupsNat0)));
  rm("quants_national");
  ### National high risk tracts
  ### Message, add new columns, drop unnecessary columns
  if(msgUser) {msg2 %>% paste0("Calculating national high risk populations...") %>% message}
  else        {msg3 %>% paste0(msg1, "...") %>% message}
  ### National
  c_tract0  <- c("national_highRiskTract")
  c_risk0   <- c_quantColsNat
  ### Figure out which tracts are high risk
  x_impacts <- x_impacts %>% mutate(national_highRiskTract = (tract_impact_sv > national_cutoff))
  x_impacts <- x_impacts %>% mutate(national_highRiskTract = national_highRiskTract * 1)
  ### Calculate high risk columns
  x_impacts[, c_risk0] <- x_impacts[[c_tract0]] * x_impacts[, c_impPop0]
  x_impacts <- x_impacts %>% select(-c(all_of(c_tract0)));
  rm("c_tract0", "c_risk0")
  # Sys.sleep(sleep)

  ###### Regional Tertiles ######
  if(msgUser) {msg1 %>% paste0("Calculating regional tertiles...") %>% message}
  else        {msg3 %>% paste0("...") %>% message}
  ### 126 rows
  quants_regional  <- x_impacts %>% select(c(all_of(c_groupsReg0), all_of(c_sum0)))
  quants_regional  <- quants_regional %>%
    group_by_at(.vars=c(all_of(c_groupsReg0))) %>%
    summarize_at(.vars=c(all_of(c_sum0)), function(x){quantile(x, na.rm=T, probs = c_probs)[3]}) %>%
    ungroup %>% rename(regional_cutoff = tract_impact_sv)
  ### Join with regional quantiles
  if(msgUser){msg2 %>% paste0("Joining regional tertiles to tract-level data...") %>% message}
  else       {msg3 %>% paste0(msg1, "...") %>% message}
  x_impacts <- x_impacts %>% left_join(quants_regional, by = c(all_of(c_groupsReg0)));
  rm("quants_regional");
  ### Regional High Risk Tracts
  if(msgUser) {msg2 %>% paste0("Calculating regional high risk populations...") %>% message}
  else        {msg3 %>% paste0("...") %>% message}
  c_tract0  <- c("regional_highRiskTract")
  c_risk0   <- c_quantColsReg
  ### Figure out which tracts are high risk
  x_impacts <- x_impacts %>% mutate(regional_highRiskTract = (tract_impact_sv > regional_cutoff))
  x_impacts <- x_impacts %>% mutate(regional_highRiskTract = regional_highRiskTract*1)
  ### Calculate high risk columns
  x_impacts[, c_risk0] <- x_impacts[[c_tract0]] * x_impacts[, c_impPop0]
  x_impacts <- x_impacts %>% select(-c(all_of(c_tract0)));
  rm("c_tract0", "c_risk0")
  # Sys.sleep(sleep)

  ###### Average Rates ######
  ### Convert 0 values to NA and then to zero
  c_rateCols0   <- c("tract_aveRate") %>% paste(c_suffix0, sep="_")
  x_impacts[, c_rateCols0] <- x_impacts[,c_impact0] / x_impacts[,c_impPop0]
  # x_impacts <- x_impacts %>% mutate(tract_aveRate_ref = tract_impact_ref / tract_impPop_ref)
  # x_impacts <- x_impacts %>% mutate(tract_aveRate_sv  = tract_impact_sv  / tract_impPop_sv )
  ### Replace NA values
  which0_ref    <- (x_impacts$tract_impPop_ref == 0) %>% which
  which0_sv     <- (x_impacts$tract_impPop_sv  == 0) %>% which
  x_impacts[which0_ref, "tract_aveRate_ref"] <- 0
  x_impacts[which0_sv , "tract_aveRate_sv" ] <- 0
  rm("which0_ref", "which0_sv")

  ###### Regional Summaries ######
  if(msgUser){msg1 %>% paste0( "Calculating regional summaries...") %>% message}
  c_sumCols0   <- c(c_impPop0, c_impact0) %>% c(c_quantColsNat, c_quantColsReg, c_rateCols0)
  c_groupCols0 <- c("region", "svGroupType", "driverUnit", "driverValue", "year")
  ### Group by the grouping columns and summarize the summary columns
  x_impacts     <- x_impacts %>%
    group_by_at(.vars  = c(all_of(c_groupCols0))) %>%
    summarize_at(.vars = c(all_of(c_sumCols0)), sum, na.rm=T) %>% ungroup
  ### Select all of the relevant columns
  x_impacts     <- x_impacts %>% select(c(all_of(c_groupCols0), all_of(c_sumCols0)))
  ### Replace tract in summary names
  x_impacts     <- x_impacts %>% rename_at(.vars=c(all_of(c_sumCols0)), ~gsub("tract_", "", c_sumCols0));
  # Sys.sleep(sleep)

  ### Dataframe
  x_impacts     <- x_impacts %>% as.data.frame

  ###### Return ######
  msg1 %>% paste0("Finished calculating total impacts.") %>% message
  return(x_impacts)
}
