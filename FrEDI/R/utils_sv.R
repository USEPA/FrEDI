###### Overview ######
### This file contains helper functions for the FrEDI SV module.


###### calc_countyPop ######
### Created 2022.02.14. Last updated 2021.02.14
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
  sleep = .00001,
  .msg0 = ""
){
  msg0 <- .msg0
  msg1 <- msg0 %>% paste("\t")
  msg2 <- msg1 %>% paste("\t")
  msg3 <- msg2 %>% paste("\t")

  msg0 %>% paste0("Calculating scaled impacts for each tract...") %>% message
  x_sysTime1 <- Sys.time()
  ### Names of functions
  c_tracts    <- funList %>% names

  # df_i_base   <- driverValues %>% select("year", "driverValue", "driverUnit")
  years_i     <- driverValues$year %>% as.vector
  x_i         <- driverValues[,xCol] %>% as.vector

  ###### Iterate over Tracts ######
  data   <-
    c_tracts %>%
    lapply(function(tract_i){
      ### Function for tract i and whether the function exists
      fun_i     <- funList[[tract_i]]
      has_fun_i <- (!is.null(fun_i))


      ### If the function is not missing
      if(has_fun_i){
        y_i   <- fun_i(x_i)
      } else{
        y_i    <- NA
      }
      df_i <- data.frame(
        year      = years_i,
        sv_impact = y_i,
        fips      = tract_i
      )
      Sys.sleep(sleep)
      return(df_i)

    }) %>%
    (function(x){
      do.call(rbind, x)
    }) %>%
    left_join(
      driverValues, by = c("year")
    )
  x_sysTime2  <- Sys.time()
  # deltaTime <- sysTime2 - sysTime1
  # (x_sysTime2 - x_sysTime1) %>% print
  # paste0("\n", "Finished.") %>% message
  msg1 %>% paste0("Finished calculating tract-level impacts.") %>% message

  return(data)
}

###### calc_tractImpacts ######
### Use this function to calculate tract level impacts
calc_tractImpacts <- function(
  scaledImpacts, ### Dataframe of scaled impacts by tract
  popData, ### Dataframe of population projections
  # svFile, ### Path to SV demographic info file
  svInfo = NULL, ### Dataframe of sv data
  # tracts, ### Character vectors of tracts
  svGroups = NULL, ### Character vector of sv group columns
  sector,
  weightCol = NULL,
  sleep = 1,
  .msg0 = ""
){
  ###### Constants  ######
  # paste0("Calculating total impacts for each tract...") %>% message
  x_sysTime1   <- Sys.time()
  regions      <- popData$region %>% unique
  tracts       <- scaledImpacts$fips %>% unique
  # c_years_eras <- seq(2000, 2090, by = 5)
  c_years_eras <- fredi_config$list_years_by5

  ###### Messages ######
  msg0 <- .msg0
  msg1 <- msg0 %>% paste0("\t")
  msg2 <- msg1 %>% paste0("\t")
  msg3 <- msg2 %>% paste0("\t")
  ###### SV Data  ######
  ### Load data and assign list elements
  # # load(svFile)
  # names_svDataList <- svDataList %>% names
  # for(i in 1:length(names_svDataList)){
  #   assign(names_svDataList[i], svDataList[[i]])
  # }; rm("svDataList")

  ###### Column Names  ######
  ### Other info
  # c_svDropCols        <- c("svCounty", "nca_abbr", "county_pop")
  c_svDataDropCols    <- c("svCounty")
  c_svOtherDropCols   <- c(
    "state", "county", "geoid10",
    "ratioTract2CountyPop", "ratioState2RegionPop", "ratioCounty2StatePop",
    "region_pop", "state_pop", "county_pop"
  )
  c_svJoinPopCols     <- c("region", "state", "geoid10")
  c_svJoinImpactsCols <- c("fips", "year")
  ### Columns to drop
  c_svNACols <- c()
  if(sector=="Air Quality"){
    c_svNACols   <- c("sv_noHS", "sv_plus65")
  }
  c_svGroupCols  <- svGroups[svGroups %in% names(svInfo)]
  c_svWeightCols <- c("children", "highRiskLabor")

  ###### Impact Lists ######
  ### Eventually, import from svDemographics
  c_sector      <- sector
  weightsCol    <- weightCol #; weightsCol %>% print

  msg0 %>% paste0("Calculating total impacts for each tract...") %>% message
  ### Join svInfo and population projections by fips
  ### - Add column for none
  ### - Drop columns
  ### - Join with population projections
  # svInfo %>% names %>% print
  # popData %>% names %>% print

  sv_fips1 <- svInfo$fips %>% unique;# c(length(sv_ids),
  sv_ids   <- svInfo$geoid10 %>% unique;# c(length(sv_ids), class(sv_ids)) %>% print
  pop_ids  <- popData$geoid10 %>% unique; #c(length(pop_ids), class(pop_ids)) %>% print
  join_ids <- sv_ids[sv_ids %in% pop_ids]; #join_ids %>% length %>% print

  x_impacts   <- svInfo %>%
    mutate(none = 1) %>%
    select(-c(all_of(c_svDataDropCols))) %>%
    # filter(geoid10 %in% pop_ids) %>%
    # left_join(
    #   popData %>% filter(geoid10 %in% sv_ids), by = all_of(c_svJoinPopCols)
    # ) %>%
    left_join(
      popData, by = all_of(c_svJoinPopCols)
    ) %>%
    # ### Filter out those with missing population info
    # filter(!is.na(county_pop)) %>%
    as.data.frame; rm("svInfo", "popData"); Sys.sleep(sleep)
  # x_impacts %>% nrow %>% print

  ### - Join with the impacts by fips number
  ### - Filter out that with missing data (!is.na(driverUnit))
  ### Format population weight column:
  ### - Multiply by 1 if no population weight
  ### - Otherwise, get population weight value
  sv_fips   <- x_impacts$fips %>% unique; #c(length(sv_fips), class(sv_fips)) %>% print
  imp_fips  <- scaledImpacts$fips %>% unique; #c(length(imp_fips), class(imp_fips)) %>% print
  join_fips <- sv_fips[sv_fips %in% imp_fips]; #join_fips %>% length %>% print
  sv_fips1[sv_fips1 %in% imp_fips] %>% length %>% print

  sv_fips %>% head %>% print
  imp_fips %>% head %>% print


  scaledImpacts %>% filter(!is.na(sv_impact)) %>% nrow %>% print

  scaledImpacts <- scaledImpacts %>% filter(fips %in% sv_fips)
  scaledImpacts %>% nrow %>% print


  x_impacts   <- x_impacts %>% filter(fips %in% imp_fips)
  x_impacts %>% nrow %>% print

  x_impacts   <- scaledImpacts %>%
    left_join(x_impacts, by = c("year", "fips"))
  # x_impacts   <- scaledImpacts %>%
  #   filter(fips %in% sv_fips) %>%
  #   left_join(x_impacts %>% filter(fips %in% imp_fips), by = c("year", "fips"))


  x_impacts %>% nrow %>% print
  x_impacts %>% filter(!is.na(sv_impact)) %>% nrow %>% print
  # x_impacts %>% filter(!is.na(county_pop)) %>% nrow %>% print

  x_impacts   <- x_impacts %>%
    filter(!is.na(driverUnit))
  # x_impacts %>% filter(!is.na(sv_impact)) %>% nrow %>% print

  x_impacts   <- x_impacts %>%
    (function(x){
      if(weightsCol=="none"){
        x    <- x %>% mutate(popWeight = 1)
      } else{
        xCol <- x[,weightsCol] %>% as.vector
        x    <- x %>% mutate(popWeight = xCol) %>% select(-c(all_of(c_svWeightCols)))
      }
      return(x)
    }) %>%
    ### Total tract population
    mutate(
      tract_totPop_tot = county_pop * ratioTract2CountyPop
    ) %>%
    select(-c(all_of(c_svOtherDropCols))) %>%
    as.data.frame; rm("scaledImpacts"); Sys.sleep(sleep)
  # x_impacts %>% nrow %>% print
  # x_impacts %>% filter(!is.na(sv_impact)) %>% nrow %>% print
  # x_impacts %>% filter(!is.na(tract_totPop_tot)) %>% nrow %>% print

  x_impacts   <- x_impacts %>%
    ### Gather by svGroupType: all the main SV variables, and racial vars
    ### - Minority and racial vars must be calculated separately
    gather(
      key = "svGroupType", value = "svRatio2Ref", c(all_of(svGroups))
      # c(all_of(c_svMainVars_sector), all_of(c_minorityVars))
    )
  # x_impacts$svRatio2Ref %>% class %>% print
  x_impacts   <- x_impacts %>%
    ### Convert some svRatio2Ref values to NA:
    ### - Check if sector has any NA columns i.e., `length(c_svNACols)>0`
    ### - If it does: check if svGroupType is in c_svNACols
    ### - Convert svRatio2Ref value to NA if true
    (function(x){
      if(length(c_svNACols)>0){
        # c_svNACols %>% print
        x_inNAGroup <- x$svGroupType %in% c_svNACols
        x <- x %>%
          mutate(
            svRatio2Ref = svRatio2Ref %>% na_if(x_inNAGroup)
          )
      }
      return(x)
    }) %>%
    as.data.frame; Sys.sleep(sleep) # x_impacts %>% nrow %>% print
  # x_impacts %>% nrow %>% print

  ### - Calculate SV ref pop ("refPop") and weighted (impacted) SV pop ("impactPop")
  ### - Calculate SV impacts for ref pop and impacted SV pop
  ### - SV population and reference population
  ### - Impacted population (e.g., children for Air Quality)
  ### - Impacts = population*popWeight
  # x_impacts$tract_totPop_tot %>% class %>% print
  # x_impacts$svRatio2Ref %>% class %>% print
  x_impacts   <- x_impacts %>%
    mutate(
      tract_totPop_sv   = tract_totPop_tot * svRatio2Ref,
      tract_totPop_ref  = tract_totPop_tot - tract_totPop_sv
    )
  x_impacts   <- x_impacts %>%
    mutate(
      tract_impPop_sv   = tract_totPop_sv  * popWeight,
      tract_impPop_ref  = tract_totPop_ref * popWeight
    )
  x_impacts   <- x_impacts %>%
    mutate(
      tract_impact_ref  = tract_impPop_ref * sv_impact,
      tract_impact_sv   = tract_impPop_sv  * sv_impact
    )
  x_impacts   <- x_impacts %>%
    select(-c("svRatio2Ref", "popWeight")) %>%
    as.data.frame; Sys.sleep(sleep)
  # x_impacts %>% nrow %>% print
  # (Sys.time() - x_sysTime1) %>% print; x_sysTime2  <- Sys.time()

  ###### Probabilities ######
  ### Probability values for tertiles
  n_quants     <- 3
  c_probs      <- seq(0, 1, length.out=n_quants + 1)

  ###### National Tertiles ######
  msg1 %>% paste0("Calculating national tertiles...") %>% message
  c_popRiskCols_national <- c("national_highRiskPop_sv", "national_highRiskPop_ref")

  quants_national <- x_impacts %>%
    filter(year %in% c_years_eras) %>%
    select(c("year", "tract_impact_sv")) %>%
    group_by_at(.vars=c("year")) %>%
    summarize_at(
      .vars=c("tract_impact_sv"), function(x){quantile(x, na.rm=T, probs = c_probs)[3]}
    ) %>%
    rename(
      national_cutoff = tract_impact_sv
    ); Sys.sleep(sleep)
  # (Sys.time() - x_sysTime2) %>% print; x_sysTime2  <- Sys.time()
  # quants_national %>% nrow %>% print

  ###### Regional Tertiles ######
  msg1 %>% paste0("Calculating regional tertiles...") %>% message
  c_popRiskCols_regional <- c("regional_highRiskPop_sv", "regional_highRiskPop_ref")

  ### 126 rows
  quants_regional <- x_impacts %>%
    filter(year %in% c_years_eras) %>%
    select(c("year", "region", "tract_impact_sv")) %>%
    group_by_at(.vars=c("year", "region")) %>%
    summarize_at(
      .vars=c("tract_impact_sv"), function(x){quantile(x, na.rm=T, probs = c_probs)[3]}
    ) %>%
    rename(
      regional_cutoff = tract_impact_sv
    ); Sys.sleep(sleep)
  # (Sys.time() - x_sysTime2) %>% print; x_sysTime2  <- Sys.time()
  # quants_regional %>% nrow %>% print

  ### Join with quantiles
  msg1 %>% paste0("Joining national tertiles to tract-level data...") %>% message
  x_impacts <- x_impacts %>%
    left_join(
      quants_national, by = c("year")
    ); rm("quants_national"); Sys.sleep(sleep)
  # (Sys.time() - x_sysTime2) %>% print; x_sysTime2  <- Sys.time()

  msg1 %>% paste0("Joining regional tertiles to tract-level data...") %>% message
  x_impacts <- x_impacts %>%
    left_join(
      quants_regional, by = c("year", "region")
    ); rm("quants_regional"); Sys.sleep(sleep)
  # (Sys.time() - x_sysTime2) %>% print; x_sysTime2  <- Sys.time()
  # x_impacts %>% names %>% print

  ###### High Risk Tracts ######
  ### Message, add new columns, drop unnecessary columns
  msg1 %>% paste0("Calculating high risk populations...") %>% message
  x_impacts <- x_impacts %>%
    mutate(
      national_highRiskTract = (tract_impact_sv > national_cutoff) %>% na_if(F),
      national_highRiskTract = national_highRiskTract*1,
      national_highRiskPop_sv  = tract_totPop_sv  * national_highRiskTract,
      national_highRiskPop_ref = tract_totPop_ref * national_highRiskTract
    ) %>%
    select(
      -c("national_highRiskTract")
    ); Sys.sleep(sleep)

  paste0("\t", "\t", "...") %>% message

  x_impacts <- x_impacts %>%
    mutate(
      regional_highRiskTract = (tract_impact_sv > national_cutoff) %>% na_if(F),
      regional_highRiskTract   = regional_highRiskTract*1,
      regional_highRiskPop_sv  = tract_totPop_sv  * regional_highRiskTract,
      regional_highRiskPop_ref = tract_totPop_ref * regional_highRiskTract
    ) %>%
    select(
      -c("regional_highRiskTract")
    ); Sys.sleep(sleep)
  # (Sys.time() - x_sysTime2) %>% print; x_sysTime2  <- Sys.time()

  ###### Regional Summaries ######
  msg1 %>% paste0( "Calculating regional summaries...") %>% message
  c_svSumCols   <- c(
    "tract_impPop_ref", "tract_impPop_sv", "tract_impact_ref", "tract_impact_sv",
    c_popRiskCols_national, c_popRiskCols_regional
  )
  c_svGroupCols <- c("region", "svGroupType", "driverUnit", "driverValue", "year")
  ### Select all of the relevant columns
  ### Group by the grouping columns
  ### Summarize the summary columns
  ### Replace tract in summary names
  x_impacts     <- x_impacts %>%
    select(c(all_of(c_svSumCols), all_of(c_svGroupCols))) %>%
    group_by_at(.vars=all_of(c_svGroupCols)) %>%
    summarize_at(
      .vars = c(all_of(c_svSumCols)), sum, na.rm=T
    ) %>%
    rename_at(
      .vars = c(all_of(c_svSumCols)), (function(x){gsub("tract_", "", x)})
    ); Sys.sleep(sleep)

  ### Calculate average rates
  ### Convert 0 values to NA and then to zero
  x_impacts     <- x_impacts %>%
    mutate(
      aveRate_sv  = impact_sv  / impPop_sv,
      aveRate_ref = impact_ref / impPop_ref
    ) %>%
    mutate_at(
      .vars = c("aveRate_sv", "aveRate_ref"),
      function(y){y %>% na_if(NaN) %>% replace_na(0)}
    ); Sys.sleep(sleep)
  # (Sys.time() - x_sysTime2) %>% print; x_sysTime2  <- Sys.time()

  ###### Return ######
  ### Final time
  (Sys.time() - x_sysTime1) %>% print

  # paste0("\n", "Finished.")
  msg1 %>% paste0("Finished calculating total impacts.") %>% message
  return(x_impacts)
}
