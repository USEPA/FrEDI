## Driver Functions ----------------
### Physical Driver Functions ----------------
### Function to calculate NOx factor
# calc_NOx_factor <- ghgData$ghgData$coefficients[["NOx"      ]][["fun0"]]
# slope0     = -0.49
# intercept0 = -1.12
# adj0       = 1e3/556
calc_NOx_factor <- function(
    nox0,
    slope0     = ghgData$ghgData$coefficients$NOx$slope0,
    intercept0 = ghgData$ghgData$coefficients$NOx$intercept0,
    adj0       = ghgData$ghgData$coefficients$NOx$adj0
){
  ### Calculate log
  ### Multiply log by slope and add intercept
  ### Multiply by adjustment
  nox0 <- nox0 |> log()
  nox0 <- nox0 * slope0 + intercept0
  nox0 <- nox0 * adj0
  ### Return
  return(nox0)
}

### Function to calculate NOx ratio
calc_NOx_ratio <- function(
    df0, ### Tibble with NOx values
    factor0 = ghgData$ghgData$coefficients$NOx$NOxFactor0
){
  ### Calculate NOx factor from NOx concentration
  ### Divide by base factor to get NOx ratio
  df0   <- df0 |> mutate(NOxFactor0 = factor0)
  df0   <- df0 |> mutate(NOxFactor  = NOx_Mt |> calc_NOx_factor())
  df0   <- df0 |> mutate(NOxRatio   = NOxFactor / NOxFactor0)
  ### Return
  return(df0)
}

### Function to calculate O3 concentration from CH4 concentration and NOX
calc_o3_conc <- function(
    df0 ### Tibble with CH4, NOx, and O3 response values
){
  ### Calculate NOx ratio
  ### Calculate O3 concentration as a function of CH4 and NOx
  df0   <- df0 |> calc_NOx_ratio()
  df0   <- df0 |> mutate(O3_pptv = CH4_ppbv * NOxRatio * state_o3response_pptv_per_ppbv)
  ### Return
  return(df0)
}


### Format methane drivers
ghgData$stateData$state_o3 |> glimpse()
format_ghg_drivers <- function(
    df0 ### Tibble with scenarios
){
  ### Load and format O3 data
  idCols0  <- c("region", "state", "postal", "model")
  sumCols0 <- c("state_o3response_pptv_per_ppbv")
  select0  <- idCols0 |> c(sumCols0) |> unique()
  df1      <- ghgData$stateData$state_o3 |> select(all_of(select0))
  # df1 |> glimpse(); df0 |> glimpse()

  ### Join data
  names0   <- df0    |> names()
  names1   <- df1    |> names()
  join0    <- names0 |> get_matches(y=names1)
  doJoin0  <- join0  |> length()
  if(doJoin0) df0 <- df0 |> left_join(df1, by=join0, relationship="many-to-many")
  else        df0 <- df0 |> cross_join(df1)
  rm(df1)
  # df0 |> glimpse()

  ### Calculate O3 if methane and nox present
  ch4Str0  <- "CH4_ppbv" |> tolower()
  do_o3    <- names0 |> tolower() |> str_detect(ch4Str0) |> any()
  if(do_o3) df0 <- df0 |> calc_o3_conc()
  # df0 |> glimpse()

  ### Reorganize values
  move0   <- c("region", "state", "postal", "model", "year")
  df0     <- df0 |>
    relocate(any_of(move0)) |>
    arrange_at(c(move0))

  ### Return
  return(df0)
}


### Socioeconomic Drivers ----------------
### Function to calculate CONUS scenario
calc_conus_scenario <- function(
    df0 ### Tibble with state population and GDP information
){
  ### Filter to CONUS states
  ### Group and summarize over year
  ### Join with df0
  filter0 <- c("AK", "HI")
  group0  <- c("year")
  df1     <- df0 |> filter(!postal %in% filter0)
  df1     <- df1 |>
    group_by_at(c(group0)) |>
    summarize(conus_pop = pop |> sum(na.rm=T), .groups="drop")
  ### Join data
  join0   <- df0 |> names() |> get_matches(y=df1 |> names())
  df0     <- df0 |> left_join(df1, by=join0)
  rm(filter0, group0, join0, df1)
  ### Calculate new gdp_percap
  df0     <- df0 |> mutate(gdp_percap_conus = gdp_usd / conus_pop)
  ### Reorganize values
  move0   <- c("region", "state", "postal", "year")
  df0     <- df0 |> relocate(all_of(move0))
  df0     <- df0 |> arrange_at(c(move0))
  ### Return
  return(df0)
}

### Scalars ----------------
### Function to calculate economic scalars
calc_ghg_scalars <- function(
    df0,       ### Tibble with information on CONUS scenario
    scalar0    = "vsl_usd",
    mult0      = "gdp_percap_conus",
    adj0       = ghgData$ghgData$coefficients[["vsl_adj0"]] |> pull(gdp_percap),
    df1        = rDataList$frediData$co_impactTypes |> filter(econScalarName %in% scalar0),
    df2        = rDataList$stateData$df_scalars |> filter(scalarName %in% scalar0),
    elasticity = rDataList$fredi_config$elasticity0
){
  ### Format impact types:
  ### - Get distinct values
  ### - Adjust econMultiplierName from "gdp_percap" to "gdp_percap_conus"
  select0   <- c("econScalarName", "econMultiplierName", "c0", "c1", "exp0", "year0")
  df1       <- df1 |> select(all_of(select0)) |> distinct()
  df1       <- df1 |> mutate(econMultiplierName = mult0)

  ### Add econScalarName
  ### Join with df0
  # df0 |> glimpse(); df1 |> glimpse()
  join0     <- c("econScalarName")
  df0       <- df0 |> mutate(econScalarName=scalar0)
  df0       <- df0 |> left_join(df1, by=join0)

  ### Adjust elasticity
  df0       <- df0 |> mutate(exp0 = (econScalarName=="vsl_usd") |> ifelse(elasticity, exp0))

  ### Filter scalars
  drop0     <- c("region", "state", "postal", "scalarType", "national_or_regional")
  renameAt0 <- c("scalarName", "value")
  renameTo0 <- c("econScalarName", "econScalarValue")
  join0     <- c("econScalarName", "year")
  years0    <- df0 |> pull(year) |> unique()
  df2       <- df2 |> filter(year %in% years0)
  df2       <- df2 |> select(-any_of(drop0))
  df2       <- df2 |> rename_at(c(renameAt0), ~renameTo0)
  df0       <- df0 |> left_join(df2, by=join0)
  rm(drop0, renameAt0, renameTo0, join0, years0)

  ### Economic adjustments (following FrEDI)
  df0     <- df0 |> mutate(econMultiplierValue = !!sym(mult0))
  df0     <- df0 |> mutate(econAdjValue   = adj0)
  df0     <- df0 |> mutate(econMultiplier = (econMultiplierValue / econAdjValue)**exp0 )
  df0     <- df0 |> mutate(econScalar     = c0 + c1 * econScalarValue * econMultiplier)

  ### Reorganize values
  move0   <- c("region", "state", "postal", "year")
  df0     <- df0 |> relocate(all_of(move0))
  df0     <- df0 |> arrange_at(c(move0))

  ### Return
  return(df0)
}

## Impact Functions ----------------
### Scaled Impacts ----------------
### Utility functions for the FrEDI methane module
### Function for mortality
# calc_mortality  <- ghgData$ghgData$coefficients[["Mortality"]][["fun0"]]

# # ghgData$ghgData$mortBasePopState |> glimpse()
# ghgData$ghgData$natMRateInfo |> glimpse()
# ghgData$stateData$state_rrScalar |> glimpse()
# ghgData$stateData$baseMortState |> glimpse()


calc_ghg_mortality <- function(
    df0,  ### Tibble with population and years
    pCol0 = "national_pop"      , ### Column with national population
    sCol0 = "rffMrate_slope"    , ### Column with mortality rate slope,
    iCol0 = "rffMrate_intercept"  ### Column with mortality rate intercept,
){
  ### Get years info
  yrs0    <- df0 |> pull(year) |> unique() |> sort()
  dfYrs0  <- tibble(year = yrs0)

  ### Select columns and format annual national RFF info
  ### Join with dfYrs
  # ghgData$ghgData$rff_nat_pop |> glimpse()
  yrCol0  <- "year"
  join0   <- yrCol0
  select0 <- join0 |> c("ifRespScalar", "rffPop") |> c(sCol0, iCol0) |> unique()
  df1     <- ghgData$ghgData$natMRateInfo |> select(all_of(select0))
  dfJoin0 <- dfYrs0 |> left_join(df1, by=join0)
  rm(join0, select0, df1)
  # dfJoin0 |> glimpse()

  ### Cross join constant state RFF scalar info with national RFF info
  drop0   <- c("StateMortRatio", "baseMrateState")
  df2     <- ghgData$stateData$state_rrScalar |> select(-any_of(drop0))
  dfJoin0 <- df2 |> cross_join(dfJoin0)
  rm(drop0, df2)
  # dfJoin0 |> glimpse()

  ### Join with baseline_state_mortality info by year
  join0   <- c("fips") |> c(yrCol0)
  select0 <- join0 |> c("StateMortRatio", "baseMrateState")
  df3     <- ghgData$stateData$baseMortState |> select(all_of(select0))
  dfJoin0 <- dfJoin0 |> left_join(df3, by=join0)
  rm(join0, select0, df3)
  # dfJoin0 |> glimpse()

  ### Join data with population data
  # relationship="many-to-many"
  drop0   <- c("region", "state", "fips")
  join0   <- c("postal") |> c(yrCol0)
  df0     <- df0     |> select(-any_of(drop0))
  df0     <- dfJoin0 |> left_join(df0, by=join0)
  df0 |> glimpse();
  rm(drop0, join0)

  ### Calculate intermediate populations
  df0     <- df0 |>
    mutate(logPop         = (!!sym(pCol0)) |> log()) |>
    mutate(rffFactor      = logPop       * !!sym(sCol0) + !!sym(iCol0)) |>
    mutate(respMrateNat   = rffFactor    * ifRespScalar) |>
    mutate(respMrateState = respMrateNat * StateMortRatio * state_mortScalar) |>
    mutate(scaled_impacts = pop * respMrateState)

  ### Return data
  return(df0)
}


### Tibble with population and years
### Tibble with columns for mortality rate slope and mortality rate intercept
calc_ghg_morbidity <- function(
    df0,
    refYr0 = 2020
){
  ### Data
  # drop1   <- c("region", "state")
  drop1   <- c("region", "state", "model_str", "year")
  df1     <- ghgData$stateData$df_asthmaImpacts
  df1     <- df1 |>
    filter(year %in% refYr0) |>
    select(-any_of(drop1))

  ### Join df0 and df1
  # df0 |> glimpse(); df1 |> glimpse()
  # join0   <- c("postal", "year")
  join0   <- c("postal")
  # join0   <- df0 |> names() |> get_matches(df1 |> names())
  df0     <- df0 |> left_join(df1, by=join0)
  # df0 |> glimpse()
  rm(df1)

  ### Calculate intermediate populations
  df0     <- df0 |> mutate(baseAsthmaFactor = baseAsthmaNumer / baseAsthmaDenom)
  df0     <- df0 |> mutate(agePopFactor     = ageRangePct / affectedPopBase)
  df0     <- df0 |> mutate(asthmaMrate      = excessAsthma * agePopFactor * baseAsthmaFactor)
  df0     <- df0 |> mutate(scaled_impacts   = pop * asthmaMrate)

  ### Return data
  return(df0)
}


### Physical and Total ----------------
### Function to calculate impacts
calc_ghg_impacts <- function(
    sector0 = "mort",
    df0, ### Tibble with population scenario and mortality
    df1  ### Tibble with ozone concentrations
){
  ### Which sector
  doMort0 <- sector0 |> str_detect("mort")
  doMorb0 <- sector0 |> str_detect("morb")

  ### Drop columns
  # df1 |> glimpse()
  drop1   <- c("region", "state", "model_str")
  df1     <- df1 |> select(-any_of(drop1))

  ### Join data with drivers
  # df0 |> glimpse(); df1 |> glimpse()
  names0  <- df0 |> names()
  names1  <- df1 |> names()
  join0   <- names0 |> get_matches(y=names1)
  df0     <- df0 |> left_join(df1, by=join0)
  rm(df1)

  ### Reorganize values
  names0  <- df0 |> names()
  move0   <- c(
    "sector", "impactType_label", "impactType", "endpoint", "ageRange", "ageType",
    "us_area", "region", "state", "postal", "fips", "model", "model_label", "year"
  ) ### End c()
  sort0   <- c(
    "sector", "impactType_label", "impactType", "endpoint", "ageType",
    "fips", "region", "state", "model_label", "model", "year"
  ) |> get_matches(names0) ### End c()
  df0     <- df0 |> relocate(any_of(move0))
  df0     <- df0 |> arrange_at(c(move0))

  ### Calculate annual impacts
  df0     <- df0 |> mutate(physical_impacts = scaled_impacts   * O3_pptv)
  df0     <- df0 |> mutate(annual_impacts   = physical_impacts * econScalar)

  ### Return
  return(df0)
}
