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
    # years = seq(2000, 2099, 1)
    years = seq(2010, 2090, by=5)

){
  c_regions <- regPop$region %>% unique
  ### Iterate over regions
  x_popProj <-
    c_regions %>%
    lapply(function(region_i){
      ### Subset population projection to a specific region
      ### Get states in the region
      ### Get unique years
      df_i     <- regPop %>% filter(region==region_i)
      states_i <- funList[[region_i]] %>% names
      years_i  <- df_i$year %>% unique %>% sort

      ### Iterate over states
      regionPop_i <- states_i %>%
        lapply(function(state_j){
          ### Function for state j
          fun_j   <- funList[[region_i]][[state_j]]$state2region

          # state_j %>% print
          df_j <-
            data.frame(
              x = years_i,
              y = fun_j(years_i)
            ) %>%
            rename(
              year    = x,
              ratioState2RegionPop = y
            ) %>%
            mutate(
              state   = state_j,
              region  = region_i
            )

          ### Get list of counties in the state
          # counties_j    <- funList[[region_i]][[state_j]]$county2state %>% names
          # statePop_j    <- counties_j %>%
          #   lapply(function(county_k){
          #     fun_k <- funList[[region_i]][[state_j]]$county2state[[county_k]]

          geoids_j      <- funList[[region_i]][[state_j]]$county2state %>% names
          statePop_j    <- geoids_j %>%
            lapply(function(geoid_k){
              fun_k <- funList[[region_i]][[state_j]]$county2state[[geoid_k]]

              if(!is.null(fun_k)){
                y_k <- fun_k(years_i)
              } else{
                y_k <- NA
              }
              df_k  <- data.frame(
                year = years_i,
                ratioCounty2StatePop = y_k
              ) %>%
                mutate(
                  state   = state_j,
                  # county  = county_k
                  geoid10 = geoid_k
                )
              return(df_k)
            }) %>%
            (function(z){do.call(rbind, z)})

          df_j <- df_j %>%
            left_join(
              statePop_j, by = c("state", "year")
            )

          return(df_j)
        }) %>%
        (function(y){
          do.call(rbind, y)
        })

      df_i <- df_i %>%
        left_join(
          regionPop_i, by = c("region", "year")
        ) %>%
        mutate(
          state_pop  = region_pop * ratioState2RegionPop,
          county_pop = state_pop  * ratioCounty2StatePop
        )
      return(df_i)
    }) %>%
    (function(x){
      do.call(rbind, x)
    }) %>%
    as.data.frame

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
    if(has_fun_i){
      y_i <- fun_i(values_x)
      # y_i %>% print
    }
    df_i      <- df_i %>% mutate(sv_impact = y_i)
    ### Sleep and return
    Sys.sleep(sleep)
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

  ###### Column Names  ######
  ### Other info
  c_svDataDropCols    <- c("svCounty")
  c_svOtherDropCols   <- c("state", "county", "geoid10") %>%
    c("ratioTract2CountyPop", "ratioState2RegionPop", "ratioCounty2StatePop") %>%
    c("region_pop", "state_pop", "county_pop")
  c_svJoinPopCols     <- c("region", "state", "geoid10")
  c_svJoinImpactsCols <- c("fips", "year")
  ### Columns to drop
  c_svNACols <- c()
  if     (sector=="Air Quality - Childhood Asthma"   ) {c_svNACols <- c("sv_noHS", "sv_plus65")}
  else if(sector=="Air Quality - Premature Mortality") {c_svNACols <- c("sv_plus65")}
  c_svGroupCols  <- svGroups[svGroups %in% names(svInfo)]
  c_svWeightCols <- c("children", "highRiskLabor", "sv_plus65")
  c_svWeightCols1<- c_svWeightCols %>% (function(x){x[which(!(x %in% c("sv_plus65")))]})

  ###### Other Info ######
  ### Eventually, import from svDemographics
  c_sector    <- sector
  weightsCol  <- weightCol #; weightsCol %>% print

  ###### Join Data ######
  msg0 %>% paste0("Calculating total impacts for each tract...") %>% message
  ### Format svInfo - Add column for none and drop other columns
  x_impacts   <- svInfo %>% mutate(none = 1) %>% select(-c(all_of(c_svDataDropCols)))
  rm("svInfo")
  ### Join svInfo with population projections by FIPS
  # x_impacts %>% head %>% glimpse %>% print; popData %>% head %>% glimpse %>% print
  x_impacts   <- x_impacts %>% left_join(popData, by = all_of(c_svJoinPopCols))
  rm("popData")
  ### Join svInfo with the impacts by fips number
  # x_impacts %>% head %>% glimpse %>% print; scaledImpacts %>% head %>% glimpse %>% print
  x_impacts   <- x_impacts %>% left_join(scaledImpacts, by = c("year", "fips"))
  rm("scaledImpacts")
  ### Filter out that with missing data (!is.na(driverUnit))
  x_impacts   <- x_impacts %>% filter(!is.na(driverUnit)) %>% as.data.frame
  ### Sleep
  Sys.sleep(sleep)

  ###### Population Weight ######
  ### Format population weight column:
  ### - Multiply by 1 if no population weight
  ### - Otherwise, get population weight value
  ### Calculate weights
  x_impacts   <- x_impacts %>% (function(x){
    if(weightsCol=="none"){
      x    <- x %>% mutate(popWeight = 1)
    }
    else{
      xCol <- x[,weightsCol] %>% as.vector
      x    <- x %>% mutate(popWeight = xCol) %>% select(-c(all_of(c_svWeightCols1)))
      rm("xCol")
    }
    return(x)
  })
  ### Calculate total tract population and drop columns
  x_impacts   <- x_impacts %>% mutate(tract_totPop_tot = county_pop * ratioTract2CountyPop)
  x_impacts   <- x_impacts %>% select(-c(all_of(c_svOtherDropCols)))

  ### Gather by svGroupType: all the main SV variables, and racial vars
  ### - Minority and racial vars must be calculated separately
  x_impacts   <- x_impacts %>% gather(key = "svGroupType", value = "svRatio2Ref", c(all_of(svGroups)))
  ### Convert some svRatio2Ref values to NA:
  ### - Check if sector has any NA columns i.e., `length(c_svNACols)>0`
  ### - If it does: check if svGroupType is in c_svNACols
  ### - Convert svRatio2Ref value to NA if true
  x_impacts   <- x_impacts %>% (function(x){
    if(length(c_svNACols)>0){
      x_inNAGroup <- x$svGroupType %in% c_svNACols
      x <- x %>% mutate(svRatio2Ref = svRatio2Ref %>% na_if(x_inNAGroup))
    }
    return(x)
  })

  ###### Tract Values ######
  ### - Calculate SV ref pop ("refPop") and weighted (impacted) SV pop ("impactPop")
  ### - Calculate SV impacts for ref pop and impacted SV pop
  ### - SV population and reference population
  ### - Impacted population (e.g., children for Air Quality)
  ### - Impacts = population*popWeight
  x_impacts   <- x_impacts %>% mutate(
    tract_totPop_sv   = tract_totPop_tot * svRatio2Ref,
    tract_totPop_ref  = tract_totPop_tot - tract_totPop_sv
  )
  x_impacts   <- x_impacts %>% mutate(
    tract_impPop_sv   = tract_totPop_sv  * popWeight,
    tract_impPop_ref  = tract_totPop_ref * popWeight
  )
  x_impacts   <- x_impacts %>% mutate(
    tract_impact_ref  = tract_impPop_ref * sv_impact,
    tract_impact_sv   = tract_impPop_sv  * sv_impact
  )
  ### Select columns
  x_impacts   <- x_impacts %>% select(-c("svRatio2Ref", "popWeight"))

  ###### Tertiles ######
  ### Probability values for tertiles
  n_quants         <- 3
  c_probs          <- seq(0, 1, length.out=n_quants + 1)
  ### Columns
  c_quantGroups    <- c("year", "region")
  c_quantColsNat   <- c("national_highRiskPop_sv", "national_highRiskPop_ref")
  c_quantColsReg   <- c("regional_highRiskPop_sv", "regional_highRiskPop_ref")
  ### National
  if(msgUser){msg1 %>% paste0("Calculating national tertiles...") %>% message}
  else{msg1 %>% paste0("Calculating high risk populations...") %>% message}
  quants_national  <- x_impacts %>% filter(year %in% years) %>% select(c("year", "tract_impact_sv"))
  quants_national  <- quants_national %>%
    group_by_at(.vars=c(c_quantGroups[1])) %>%
    summarize_at(
      .vars=c("tract_impact_sv"), function(x){quantile(x, na.rm=T, probs = c_probs)[3]}
    ) %>% ungroup
  ### Rename
  quants_national  <- quants_national %>% rename(national_cutoff = tract_impact_sv)
  Sys.sleep(sleep)

  ###### Regional Tertiles ######
  if(msgUser){msg1 %>% paste0("Calculating regional tertiles...") %>% message}
  ### 126 rows
  quants_regional  <- x_impacts %>% filter(year %in% years) %>% select(c("year", "region", "tract_impact_sv"))
  quants_regional  <- quants_regional %>%
    group_by_at(.vars=c(c_quantGroups[1:2])) %>%
    summarize_at(
      .vars=c("tract_impact_sv"), function(x){quantile(x, na.rm=T, probs = c_probs)[3]}
    ) %>% ungroup
  ### Rename
  quants_regional  <- quants_regional %>% rename(regional_cutoff = tract_impact_sv);
  Sys.sleep(sleep)
  if(!msgUser){msg1 %>% paste0("\t", "\t", "...") %>% message}

  ### Join with national quantiles
  if(msgUser){msg1 %>% paste0("Joining national tertiles to tract-level data...") %>% message}
  x_impacts <- x_impacts %>% left_join(quants_national, by = c("year"));
  rm("quants_national"); Sys.sleep(sleep)
  ### Join with regional quantiles
  if(msgUser){msg1 %>% paste0("Joining regional tertiles to tract-level data...") %>% message}
  x_impacts <- x_impacts %>% left_join(quants_regional, by = c("year", "region"));
  rm("quants_regional"); Sys.sleep(sleep)

  ###### High Risk Tracts ######
  ### Message, add new columns, drop unnecessary columns
  if(msgUser){msg1 %>% paste0("Calculating high risk populations...") %>% message}
  else{msg1 %>% paste0("\t", "\t", "...") %>% message}
  ### National
  x_impacts <- x_impacts %>% mutate(
    national_highRiskTract   = (tract_impact_sv > national_cutoff) %>% na_if(F),
    national_highRiskTract   = national_highRiskTract*1,
    national_highRiskPop_sv  = tract_totPop_sv  * national_highRiskTract,
    national_highRiskPop_ref = tract_totPop_ref * national_highRiskTract
  )
  ### Drop columns
  x_impacts <- x_impacts %>% select(-c("national_highRiskTract"));
  msg1 %>% paste0("\t", "\t", "...") %>% message
  ### Regional
  x_impacts <- x_impacts %>% mutate(
    regional_highRiskTract   = (tract_impact_sv > national_cutoff) %>% na_if(F),
    regional_highRiskTract   = regional_highRiskTract*1,
    regional_highRiskPop_sv  = tract_totPop_sv  * regional_highRiskTract,
    regional_highRiskPop_ref = tract_totPop_ref * regional_highRiskTract
  )
  x_impacts <- x_impacts %>% select(-c("regional_highRiskTract"));
  Sys.sleep(sleep)

  ###### Regional Summaries ######
  if(msgUser){msg1 %>% paste0( "Calculating regional summaries...") %>% message}
  c_svSumCols   <- c("tract_impPop_ref", "tract_impPop_sv", "tract_impact_ref", "tract_impact_sv") %>% c(c_quantColsNat, c_quantColsReg)
  c_svGroupCols <- c("region", "svGroupType", "driverUnit", "driverValue", "year")

  ### Select all of the relevant columns
  ### Group by the grouping columns
  ### Summarize the summary columns
  x_impacts     <- x_impacts %>%
    select(c(all_of(c_svSumCols), all_of(c_svGroupCols))) %>%
    group_by_at(.vars=all_of(c_svGroupCols)) %>%
    summarize_at(.vars = c(all_of(c_svSumCols)), sum, na.rm=T) %>% ungroup
  ### Replace tract in summary names
  x_impacts     <- x_impacts %>% rename_at(
    .vars = c(all_of(c_svSumCols)), (function(x){gsub("tract_", "", x)})
  ); Sys.sleep(sleep)

  ### Calculate average rates
  ### Convert 0 values to NA and then to zero
  x_impacts     <- x_impacts %>% mutate(
    aveRate_sv  = impact_sv  / impPop_sv,
    aveRate_ref = impact_ref / impPop_ref
  )
  ### Replace NA values
  x_impacts     <- x_impacts %>% mutate_at(
    .vars = c("aveRate_sv", "aveRate_ref"),
    function(y){y %>% na_if(NaN) %>% replace_na(0)}
  ); Sys.sleep(sleep)
  ### Dataframe
  x_impacts     <- x_impacts %>% as.data.frame

  ###### Return ######
  msg1 %>% paste0("Finished calculating total impacts.") %>% message
  return(x_impacts)
}
