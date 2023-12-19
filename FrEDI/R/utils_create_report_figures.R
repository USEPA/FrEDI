### Get column values from a tibble
get_column_values <- function(
    df0,    ### Tibble
    col0,   ### Column
    unique0 = FALSE ### Unique values
){
  vals0 <- df0[[col0]]
  if(unique0){vals0 <- vals0 |> unique()}
  return(vals0)
} ### End get_column_values

### Function to create note for figures
create_fig_scale_note <- function(
    ntypes = 1, ### Number of types
    nvars  = 1  ### Number of variants
){
  ### Conditions
  hasTypes <- ntypes > 1
  hasVars  <- nvars  > 1
  either0  <- hasTypes | hasVars
  both0    <- hasTypes & hasVars
  ### Notes
  note0    <- either0  |> ifelse("Note: Figure scale varies by ", "")
  note1    <- hasTypes |> ifelse("impact type", "")
  note2    <- hasVars  |> ifelse("variant", "")
  and0     <- both0    |> ifelse(" and ", "")
  ### Final note
  note0    <- note0 |> paste0(note1, and0, note2)
  ### Return
  return(note0)
} ### create_fig_scale_note

#### Summarize missing values
sum_with_na <- function(
    df0,    ### Dataframe
    group0, ### Grouping columns
    col0      = "yCol",
    threshCol = "threshold", ### Threshold to check against
    drop0     = TRUE         ### Whether to drop is_NA col
){
  ### Check for NA values
  df0    <- df0 |> mutate(is_NA = df0[[col0]] |> is.na())
  # df0    <- df0 |> select(-c(all_of(col0)))

  ### Summarize by impact type
  group0 <- group0 |> c(threshCol) |> unique()
  sum0   <- c(col0, "is_NA")
  df0    <- df0 |>
    group_by_at (.vars = c(group0)) |>
    summarize_at(.vars = c(sum0), sum, na.rm = T) |>
    ungroup()

  ### Check NA values
  df0    <- df0 |> mutate(is_NA = case_when(
    is_NA <  df0[[threshCol]] ~ 1,
    is_NA == df0[[threshCol]] ~ NA,
    .default = NA
  ))

  ### Multiply column
  df0[[col0]] <- df0[[col0]] * df0[["is_NA"]]
  # df0    <- df0 |> rename_at(.vars=c("yCol"), ~c(col0))
  ### Drop columns
  if(drop0){df0 <- df0 |> select(-c("is_NA"))}
  ### Return
  return(df0)
} ### End sum_with_na

### Filter to five year values
filter_years <- function(
    df0, ### data
    byYears = 5, ### Number of years
    years   = NULL ### Override by years
){
  ### Check whether user provided values to years argument
  nullYears <- years |> is.null()
  hasYears  <- !nullYears
  ### If user did not provide values to years argument
  if(!hasYears){
    range0 <- df0[["year"]] |> range(na.rm=TRUE)
    years  <- seq(range0[1], range0[2], by=byYears)
  }
  ### Filter to years
  df0 <- df0 |> filter(year %in% years)
  ### Return
  return(df0)
} ### End filter_years()

### Filter values
### Format values to specific number of decimal places
format_values <- function(
    df0, ### data
    byState = TRUE,
    # cols0  = c("driverValue", "gdp_usd", "national_pop", "gdp_percap", "reg_pop", "annual_impacts"), ### Columns to format
    digits = 16
){
  ### Pop columns
  if(byState){popCols <- c("state", "postal")} else{c()}
  popCol <- byState |> ifelse("state_pop", "reg_pop")
  ### Columns
  cols0 <- c("driverValue", "gdp_usd", "national_pop", "gdp_percap", popCol, "annual_impacts")
  ### Mutate
  df0 <- df0 |> mutate_at(vars(cols0), function(x){format(x, digits=digits)})
  return(df0)
} ### End format_values()

### Run CONUS scenarios
create_constant_temp_scenario <- function(
    temp0,
    type0   = "conus",
    prefix0 = "Other_Integer" ### Prefix for scenario
){
  ### Temperature Type
  isConus <- "conus" %in% (type0 |> tolower())
  ### Format scenario label
  # pre0  <- (type0=="conus") |> ifelse("Other_Integer", "preI_global")
  pre0  <- prefix0
  lab0  <- temp0 |> round(1)
  scen0 <- pre0  |> paste(lab0, sep="_")
  ### Get annual values 1995 - 2010: starting with zero in 1995
  xIn0  <- c(1995, 2010)
  yIn0  <- c(0, temp0)
  xOut0 <- seq(xIn0[1], xIn0[2])
  df0   <- approx(x = xIn0, y = yIn0, xout=xOut0) |>
    as_tibble() |>
    rename(year=x, temp_C=y)
  ### Extend values
  df1   <- tibble(year = seq(2011, 2090, by=1))
  df1   <- df1 |> mutate(temp_C  = temp0)
  df0   <- df0 |> rbind(df1)
  rm("df1")

  ### Get other temp types and rename
  if(isConus){
    df0   <- df0 |> mutate(temp_C_global = temp_C |> FrEDI::convertTemps(from="conus"))
    df0   <- df0 |> mutate(temp_C_conus  = temp_C)
  } ### End if(isConus)
  else       {
    df0   <- df0 |> mutate(temp_C_conus  = temp_C |> FrEDI::convertTemps(from="global"))
    df0   <- df0 |> mutate(temp_C_global = temp_C)
  } ### End else(isConus)
  ### Drop temp_C
  df0   <- df0 |> select(-c("temp_C"))
  ### Get SLR
  ySlr0 <- FrEDI::temps2slr(temps = df0[["temp_C_global"]], years = df0[["year"]])
  df0   <- df0 |> left_join(ySlr0, by="year")
  df0   <- df0 |> mutate(temp_lab = lab0)
  df0   <- df0 |> mutate(scenario = scen0)
  ### Return
  return(df0)
} ### End create_constant_temp_scenario

#### Get scenario inputs
#### Get inputs list for a single scenario
get_scenario_inputsList <- function(
    df0,   ### Data
    byState = TRUE
){
  ### df0 names
  names0  <- df0 |> names()
  ### Initialize scenario
  list0   <- list()
  temp0   <- NULL
  slr0    <- NULL
  gdp0    <- NULL
  pop0    <- NULL
  ### Pop columns
  if(byState){popCols <- c("state", "postal")} else{c()}
  popCol <- byState |> ifelse("state_pop", "reg_pop")
  ### Columns for scenarios
  cTemp0  <- c("year", "temp_C_conus")
  cTemp1  <- c("year", "temp_C")
  cSlr    <- c("year", "slr_cm")
  cGdp    <- c("year", "gdp_usd")
  cPop    <- c("year", "region") |> c(popCols, popCol)
  ### Whether to create scenarios
  doTemp0 <- (cTemp0 %in% names0) |> all()
  doTemp1 <- (cTemp1 %in% names0) |> all()
  doTemp  <- doTemp0 | doTemp1

  doSlr   <- (cSlr %in% names0) |> all()
  doGdp   <- (cGdp %in% names0) |> all()
  doPop   <- (cPop %in% names0) |> all()
  ### Create scenarios
  if(doTemp){
    if(doTemp0){cTemp <- cTemp0}
    else       {cTemp <- cTemp1}
    temp0   <- df0 |> select(all_of(cTemp))
    if(doTemp0){
      temp0 <- temp0 |> rename_at(vars("temp_C_conus"), ~c("temp_C"))
    } ### End if(doTemp0)
    list0[["tempInput"]] <- temp0
    rm("temp0")
  } ### End if(doTemp)

  if(doSlr){
    slr0   <- df0 |> select(all_of(cSlr))
    list0[["slrInput"]] <- slr0
    rm("slr0")
  } ### End if(doSlr)

  if(doGdp){
    gdp0   <- df0 |> select(all_of(cGdp))
    list0[["gdpInput"]] <- gdp0
    rm("gdp0")
  } ### End if(doGdp)

  if(doPop){
    pop0   <- df0 |> select(all_of(cPop))
    list0[["popInput"]] <- pop0
    rm("pop0")
  } ### End if(doPop)

  ### Return
  return(list0)
} ### End get_scenario_inputsList

#### Run a single temp scenario
run_fredi_scenario <- function(
    df0,     ### Data
    sectors  = FrEDI::get_sectorInfo(), ### Which sectors
    scenCols = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
    joinCols = c("year")
){
  ### Filter to scenario
  df1     <- df0 |> select(c(all_of(scenCols))); rm("df0")
  list1   <- df1 |> get_scenario_inputsList()
  ### Run FrEDI
  df2     <- FrEDI::run_fredi(inputsList = list1, sectorList=sectors, aggLevels = "none")
  ### Join scenarios
  # df1 |> names() |> print();  df2 |> names() |> print();
  df2     <- df2 |> left_join(df1, by=c(joinCols))
  # df2 |> names() |> print();
  ### Return
  return(df2)
} ### End run_fredi_scenario

### Aggregate temp scenarios
agg_fredi_scenario <- function(
    df0,      ### Data: output of run_fredi_scenario
    sectors   = FrEDI::get_sectorInfo(), ### Which sectors
    scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
    joinCols  = c("year"),
    aggLevels = c("modelaverage", "national")
){
  ### Pop cols
  byState <- "state" %in% (df0 |> names())
  if(byState){stateCols <- c("state", "postal")} else{stateCols <- c()}
  popCol  <- byState |> ifelse("state_pop", "reg_pop")
  ### Filter to grouping columns
  drop0   <- scenCols[!(scenCols %in% joinCols)]
  ### Run FrEDI
  group0  <- c("sector", "variant", "impactType", "impactYear")
  group0  <- group0 |> c("region", stateCols)
  group0  <- group0 |> c("model_type", "model")
  group0  <- group0 |> c("sectorprimary", "includeaggregate")
  group0  <- group0 |> c(drop0)
  df0     <- df0 |> FrEDI::aggregate_impacts(aggLevels = aggLevels, groupByCols = group0)
  # df0     <- df0 |> FrEDI::aggregate_impacts(aggLevels = aggLevels)
  ### Return
  return(df0)
} ### End agg_fredi_scenario

#### run_scenario
### Run a single scenario
run_scenario <- function(
    scenario, ### Scenario
    df0,      ### Data frame with scenario info
    fredi     = TRUE,
    sectors   = FrEDI::get_sectorInfo(), ### Which sectors
    scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
    joinCols  = c("year"),
    aggLevels = c("modelaverage", "national")
){
  ### Filter to scenario
  scenario0 <- scenario
  df_x0     <- df0 |> filter(scenario==scenario0)
  rm("df0")
  ### Run FrEDI
  if(fredi){
    df_x0 <- df_x0 |> run_fredi_scenario(
      sectors   = sectors,
      scenCols  = scenCols,
      joinCols  = joinCols
    ) ### End run_fredi_scenario
  } ### End if(fredi)
  # "got here1" |> print(); df_x0 |> glimpse()

  ### Aggregate FrEDI
  agg0      <- !("none" %in% aggLevels)
  # agg0 |> print()
  if(agg0){
    # "got here1" |> print()
    df_x0 <- df_x0 |> agg_fredi_scenario(
      scenCols  = scenCols,
      joinCols  = joinCols,
      aggLevels = aggLevels
    ) ### End run_fredi_scenario
  } ### End if(agg0)
  # "got here2" |> print(); df_x0 |> glimpse()

  ### Format other values
  mutate0   <- c("temp_C_conus", "temp_C_global", "slr_cm")
  df_x0     <- df_x0 |> mutate_at(vars(mutate0), as.numeric)

  ### Return
  return(df_x0)
} ### End function run_scenario


### Run list of scenarios
run_scenarios <- function(
    df0, ### Output of create_constant_temp_scenario
    col0      = "scenario", ### Scenario column
    fredi     = TRUE,
    sectors   = FrEDI::get_sectorInfo(), ### Which sectors
    aggLevels = c("modelaverage", "national"),
    scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
    joinCols  = c("year")
){
  ### Unique scenarios
  scenarios0 <- df0[[col0]] |> unique()
  nScenarios <- scenarios0 |> length()

  ### Iterate over the scenarios
  list0  <- scenarios0 |> map(function(.x){
    paste0("Running scenario ", which(scenarios0 == .x), "/" , nScenarios, "...") |> message()
    df_x <- run_scenario(
      .x,
      df0       = df0,
      fredi     = fredi,
      sectors   = sectors,
      aggLevels = aggLevels,
      scenCols  = scenCols,
      joinCols  = joinCols
    ) ### End run_scenario(.x)
    return(df_x)
  }) ### End function(.x), walk
  ### Bind values into a list
  # df0    <- list0 %>% (function(x){do.call(rbind, x)})
  df0    <- list0 |> bind_rows()

  ### Return
  return(df0)
} ### End run_scenarios


### Summarize results by degree of warming for a single specified year
sum_impacts_byDoW <- function(
    df0,
    scenarios,
    bySector    = FALSE,
    year        = 2010,
    models      = c("GCM", "SLR"),
    sumCol      = "annual_impacts",
    groupVars   = c("variant", "impactType", "impactYear"),
    impactYears = c("Interpolation", "NA", "2010", "2090"),
    aggOnly     = FALSE,
    adjVal      = 1/10**9, ### Factor to multiply by
    adjCol      = "impact_billions",
    silent      = FALSE
){
  ### Filter to includeaggregate==1
  if(aggOnly){df0 <- df0 |> filter(includeaggregate==1)}
  ### Filter to sector primary
  primary         <- !bySector
  if(primary){df0 <- df0 |> filter(sectorprimary   ==1)}
  ### Filter to scenarios
  scenarios0 <- scenarios; rm("scenarios")
  df0        <- df0 |> filter(scenario   %in% scenarios0)
  ### Filter to appropriate models
  df0        <- df0 |> filter(model_type %in% models)
  ### Filter to appropriate impact years
  years0     <- impactYears
  df0        <- df0 |> filter(impactYear %in% years0)
  # ### Filter to appropriate year
  do_gcm     <- "gcm" %in% tolower(models)
  year0      <- year
  if(do_gcm){df0 <- df0 |> filter(year == year0)}
  ### Drop unnecessary columns
  # df0        <- df0 |> select(-c("impactYear"))
  ### Summarize by Degree of Warming
  # df0        <- df0 |> summarize_DOW_data(year=year0, bySector = bySector)
  list0   <- years0 %>% map(function(.z){
    df_z <- df0 |> summarize_DOW_data(
      year       = year0,
      bySector   = bySector,
      sumCol     = sumCol,
      groupVars  = groupVars,
      impactYear = .z,
      silent     = silent
    )
    return(df_z)
  })
  ### Bind together
  # df0        <- list0 %>% (function(x){do.call(rbind, x)})
  df0        <- list0 |> bind_rows()
  rm(list0)
  ### Adjust values
  df0[[adjCol]] <- df0[["annual_impacts"]] * adjVal
  ### Add summary year
  df0        <- df0 |> mutate(summaryYear=year0)
  ### Select columns
  # select0    <- c("sector", "region", "model_type", "model", "summaryYear", "driverValue", "annual_impacts", adjCol) |> unique()
  # df0        <- df0 |> relocate(c(all_of(select0)))

  ### Glimpse results
  # df0 %>% glimpse
  ### Return
  return(df0)
} ### End sum_impacts_byDoW

### Summarize results by degree of warming for multiple specified years
sum_impacts_byDoW_years <- function(
    df0, ### Outputs of sum_impactsByDegree
    scenarios,
    bySector    = FALSE,
    years       = c(2010, 2090),
    models      = c("GCM", "SLR"),
    sumCol      = "annual_impacts",
    groupVars   = c("variant", "impactType", "impactYear"),
    impactYears = c("Interpolation", "NA", "2010", "2090"),
    aggOnly     = FALSE,
    adjVal      = 1/10**9, ### Factor to multiply by
    adjCol      = "impact_billions",
    silent      = FALSE
){
  ### Filter to includeaggregate==1
  ### Filter to includeaggregate==1
  # aggOnly         <- TRUE
  if(aggOnly){df0 <- df0 |> filter(includeaggregate==1)}
  ### Filter to sector primary
  primary    <- !bySector
  if(primary){df0 <- df0 |> filter(sectorprimary   ==1)}
  ### Run scenarios
  nYears <- years |> length()

  ### Get list
  list0   <- years |> map(function(.x){
    paste0("Summarizing values for ", which(years == .x), "/" , nYears, " years...") |> message()
    df_x <- sum_impacts_byDoW(
      df0         = df0,
      scenarios   = scenarios,
      bySector    = bySector,
      sumCol      = sumCol,
      groupVars   = groupVars,
      impactYears = impactYears,
      year        = .x,
      models      = models,
      aggOnly     = aggOnly,
      adjVal      = adjVal,
      adjCol      = adjCol,
      silent      = silent
    ) ### End sum_impactsByDegree
    return(df_x)
  }) ### End walk
  ### Bind values together
  # df0 <- list0 %>% (function(x){do.call(rbind, x)})
  df0 <- list0 |> bind_rows()
  rm(list0)
  ### Convert to tibble
  df0 <- df0 |> as_tibble()
  ### Return
  return(df0)
} ### End sum_impacts_byDoW_years

### Get SLR impacts from FrEDI data
get_fig7_slrDataObj <- function(
    drivers=TRUE, ### Whether to return drivers
    impacts=TRUE  ### Whether to return impacts
    ){
  ###### Initialize Return List ######
  list0     <- list()

  ###### Get Data Objects from FrEDI ######
  ### Sector Info
  ### Variant Info
  ### Model Info
  dfSectors <- "co_sectors"  |> get_frediDataObj()
  dfVariant <- "co_variants" |> get_frediDataObj()
  slrRef    <- "co_models"   |> get_frediDataObj()

  ### SLR Driver values
  ### SLR Scaled impct values
  if(drivers){slrCm  <- "slr_cm"     |> get_frediDataObj()}
  if(impacts){slrImp <- "slrImpacts" |> get_frediDataObj(listSub="stateData")}

  ###### SLR Models ######
  ### Format SLR Models
  slrRef    <- slrRef |> filter(modelType=="slr")
  slrRef    <- slrRef |> rename_at(vars("model_label"), ~c("model"))

  ###### Levels & Labels ######
  ### Initial levels & labels
  slrLevels <- slrRef[["model_dot"]]
  slrLabels <- slrRef[["model"]]
  # slrRef[["model"]] |> print()
  ### Add ends to labels
  slrLevels <- c("0cm" , slrLevels, "300cm" )
  slrLabels <- c("0 cm", slrLabels, "300 cm")
  # slrLevels |> print(); slrLabels |> print()
  ### Vector of model labels and number of models
  c_slrs    <- slrLabels
  n_slrs    <- c_slrs |> length()

  ###### Sectors Data ######
  ### Format Sectors data
  select0   <- c("sector_id", "sector_label", "modelType")
  rename0   <- c("sector_label", "modelType")
  rename1   <- c("sector", "model_type")
  mutate0   <- c("sector", "sector_id")
  dfSectors <- dfSectors |> select(c(all_of(select0)))
  dfSectors <- dfSectors |> rename_at(.vars=c(rename0), ~c(rename1))
  dfSectors <- dfSectors |> filter(tolower(model_type)=="slr")
  dfSectors <- dfSectors |> mutate_at(.vars=c(mutate0), as.character)
  rm(select0, rename0, rename1, mutate0)

  ###### Variants Data ######
  ### Format Variants data
  select0   <- c("sector_id", "variant_id", "variant_label", "sectorprimary", "includeaggregate")
  rename0   <- c("variant_label")
  rename1   <- c("variant")
  dfVariant <- dfVariant |> select(c(all_of(select0)))
  dfVariant <- dfVariant |> rename_at(.vars=c(rename0), ~c(rename1))
  rm(select0, rename0, rename1)

  ###### Sector-Variant Data ######
  ### Create Sector-Variant data
  dfSectVar <- dfSectors |> left_join(dfVariant, by=c("sector_id"))

  ###### SLR Driver values ######
  if(drivers){
    ### Format SLR Driver values
    select0   <- c("year", "driverValue", "model")
    rename0   <- c("driverValue")
    rename1   <- c("slr_cm")
    slrCm     <- slrCm |> select(c(all_of(select0)))
    slrCm     <- slrCm |> rename_at(.vars=c(rename0), ~c(rename1))
    rm(select0, rename0, rename1)

    ### Add values for 0cm, 300 cm
    slrCm     <- slrCm  %>% (function(y){
      y    <- y |> mutate(model = model |> as.character())
      y300 <- y |> filter(model=="250cm") |> mutate(model="300cm")
      y    <- y |> rbind(y300)
      return(y)
    })

    ### Mutate labels & levels
    slrCm     <- slrCm |> mutate(model = model |> factor(levels=slrLevels, labels=slrLabels))

    ### Arrange values
    arrange0  <- c("model", "year")
    slrCm     <- slrCm |> arrange_at(.vars=c(arrange0))
    rm(arrange0)

    ### Add to list
    list0     <- list0 |> c(list(slrCm=slrCm))
  } ### End if(drivers)

  ###### SLR Impacts Data ######
  if(impacts){
    ### Format the impacts
    rename0   <- c("sector"   , "variant"   , "scaled_impacts")
    rename1   <- c("sector_id", "variant_id", "annual_impacts")
    drop0     <- c("model_dot")
    slrImp    <- slrImp |> rename_at(.vars=c(rename0), ~c(rename1))
    slrImp    <- slrImp |> select(-c(all_of(drop0)))
    rm(rename0, rename1, drop0)

    ### Adjust names
    exclude0  <- c("year", "annual_impacts")
    mutate0   <- slrImp |> names() %>% (function(y1, y2=exclude0){y1[!(y1 %in% y2)]})
    slrImp    <- slrImp |> mutate_at(.vars=c(mutate0), as.character)
    slrImp    <- slrImp |> mutate(model = model |> factor(levels=slrLevels, labels=slrLabels))
    rm("exclude0", "mutate0")

    ### Join with sector-variant data
    drop0     <- c("sector_id", "variant_id")
    join0     <- c(drop0, "model_type")
    slrImp    <- slrImp |> left_join(dfSectVar, by=c(join0))
    slrImp    <- slrImp |> select(-c(all_of(drop0)))
    rm(join0, drop0)

    ### Mutate other columns
    slrImp    <- slrImp |> mutate(region     = gsub("\\.", " ", region))
    slrImp    <- slrImp |> mutate(impactType = "N/A")
    slrImp    <- slrImp |> mutate(impactYear = "Interpolation")
    slrImp    <- slrImp |> mutate(model_type = model_type |> toupper())

    ### Replace missing values
    slrImp    <- slrImp |> mutate(annual_impacts = annual_impacts |> replace_na(0))

    ### Mutate specific values
    slrImp    <- slrImp %>% (function(y){
      yLo   <- y |> filter(model=="30 cm" ) |> mutate(annual_impacts=0) |> mutate(model="0 cm")
      yHi   <- y |> filter(model=="250 cm") |> mutate(model="300 cm")
      y     <- yLo |> rbind(y) |> rbind(yHi)
      return(y)
    })

    ### Mutate labels & levels
    slrImp    <- slrImp |> mutate(model = model |> as.character())
    slrImp    <- slrImp |> mutate(model = model |> factor(levels=slrLabels, labels=slrLabels))

    ### Arrange values
    arrange0  <- c("sector", "variant", "impactType", "impactYear", "region", "model_type", "model", "year")
    slrImp    <- slrImp |> arrange_at(.vars=c(arrange0))
    rm(arrange0)

    ### Add to list
    list0     <- list0 |> c(list(slrImp=slrImp))
  } ### End if impacts

  ###### Return ######
  ### Add to list
  return(list0)
} ### get_fig7_slrDataObj

### Get values for figure 7 impacts for SLR sectors
get_fig7_slrImpacts <- function(
    slrDrivers, ### Dataframe of drivers
    slrImpacts, ### Dataframe of scaled impacts
    bySector    = FALSE,
    sumCol      = "annual_impacts",
    # groupVars   = c("variant", "impactType", "impactYear"),
    impactYears = c("Interpolation", "NA", "2010", "2090"),
    aggOnly    = TRUE, ### Whether to filter to includeaggregate==1
    years      = c(2050, 2090),
    adjVal     = 1/10**9, ### Factor to multiply by
    adjCol     = "impact_billions"
){
  ###### Model Labels ######
  ### SLR sectors separately:
  ### - Filter to 2090 and 2050 values
  ### - Calculate national totals
  ### - Combine CIRA impacts and SLR trajectories
  modelLabels     <- slrDrivers[["model"]] |> levels() |> as.character()
  modelHeights    <- modelLabels |> map(function(.x){str_split(string=.x, pattern="\\s")[[1]][1]}) |> unlist() |> as.numeric()

  ###### Format data ######
  ### Add variables for plotting with plot_DOW_byModelType
  # slrImpacts      <- slrImpacts |> mutate(impactYear = "Interpolation")

  ### Filter to includeaggregate==1,
  if(aggOnly){slrImpacts <- slrImpacts |> filter(includeaggregate==1)}

  ### Filter to primary==1
  primary <- !bySector
  if(aggOnly){slrImpacts <- slrImpacts |> filter(sectorprimary==1)}

  ### Filter to appropriate categories and years
  if(!bySector){slrImpacts <- slrImpacts |> filter(year %in% years)}
  slrImpacts      <- slrImpacts |> filter(model %in% modelLabels)

  ### Filter to national totals or calculate national totals
  slrImpacts      <- slrImpacts |> filter(region!="National Total")
  c_regions       <- slrImpacts[["region"]] |> unique()
  n_regions       <- c_regions |> length()

  ### Change column names
  rename0         <- "model"
  rename1         <- "SLR_scenario"
  slrDrivers      <- slrDrivers |> rename_at(.vars=c(rename0), ~c(rename1))
  slrImpacts      <- slrImpacts |> rename_at(.vars=c(rename0), ~c(rename1))
  slrImpacts      <- slrImpacts |> mutate(model = year |> factor())
  rm(rename0, rename1)

  ### Initialize totals
  slrTotals       <- slrImpacts
  rm(slrImpacts)

  ###### Summarize Impact Types ######
  ### Summarize over impact types
  if(!bySector){
    #### Count number of impact types
    group0          <- c("sector", "variant", "impactYear", "region", "model_type", "SLR_scenario", "model", "year")
    count_impTypes  <- slrTotals |>
      group_by_at(c(group0)) |>
      summarize(n=n(), .groups="keep") |>
      ungroup()
    # n_impTypes     <- count_impTypes[["n"]] |> max()

    ### Join counts with totals
    join0           <- group0
    slrTotals       <- slrTotals |> left_join(count_impTypes, by = c(join0))
    rm(join0, count_impTypes)

    ### Summarize
    # sum0            <- c("annual_impacts", "is_NA")
    # sum0            <- c(sumCol, "is_NA")
    slrTotals       <- slrTotals |> sum_with_na(
      group0    = group0, ### Grouping columns
      # col0      = "annual_impacts",
      col0      = sumCol,
      threshCol = "n", ### Threshold to check against
      drop0     = TRUE         ###
    ) %>% select(-c("n"))

    slrTotals       <- slrTotals |> mutate(impactType = "All")
    # slrTotals |> names() |> print()
  }

  ###### National Totals ######
  ### Calculate national totals
  group0          <- c("sector", "variant", "impactType", "impactYear", "model_type", "SLR_scenario", "model", "year")
  # group0          <- c("sector", "variant", "impactType", "impactYear", "model_type", "model", "year")
  # sum0            <- c("annual_impacts", "is_NA")
  slrTotals       <- slrTotals |> mutate(threshold = n_regions)
  slrTotals       <- slrTotals |> sum_with_na(
    group0    = group0, ### Grouping columns
    # col0      = "annual_impacts",
    col0      = sumCol,
    threshCol = "threshold", ### Threshold to check against
    drop0     = TRUE         ###
  ) %>% select(-c("threshold"))

  slrTotals       <- slrTotals |> mutate(region = "National Total")
  # slrTotals |> names() |> print()

  ######  #####Adjust Values ######
  ### Adjust values
  # slrTotals[[adjCol]] <- slrTotals[["annual_impacts"]] * adjVal
  slrTotals[[adjCol]] <- slrTotals[[sumCol]] * adjVal

  # ### Add additional values & drop columns
  # drop0           <- c("variant", "impactType", "impactYear")
  # slrTotals       <- slrTotals |> mutate(region = "National Total")
  # # slrTotals       <- slrTotals |> mutate(impactType = "All")
  # slrTotals       <- slrTotals |> select(-c(all_of(drop0)))
  # rm(drop0)

  ###### Format Results ######
  ### Join with driver info
  rename0         <- c("slr_cm"     , "year")
  rename1         <- c("driverValue", "summaryYear")
  join0           <- c("year", "SLR_scenario")
  # join0           <- c("year", "model")
  # select0         <- c("sector", "region", "model_type", "model", "summaryYear", "driverValue", "annual_impacts", adjCol) |> unique()
  # select0         <- c("sector", "region", "model_type", "SLR_scenario", "model", "summaryYear", "driverValue", "annual_impacts", adjCol) |> unique()
  select0         <- c("sector", "region", "model_type", "SLR_scenario", "model", "summaryYear", "driverValue", sumCol, adjCol) |> unique()
  slrTotals       <- slrTotals |> left_join(slrDrivers, by=c(join0))
  slrTotals       <- slrTotals |> rename_at(.vars=c(rename0), ~c(rename1))
  slrTotals       <- slrTotals |> relocate(c(all_of(select0)))
  rm(rename0, rename1, join0)

  ### Adjust column names if bySector
  if(bySector){
    slrTotals       <- slrTotals |> mutate(model=SLR_scenario)
    slrTotals       <- slrTotals |> rename(year=summaryYear)
    slrTotals       <- slrTotals |> select(-c("SLR_scenario"))
  }

  slrTotals |> glimpse() #|> print()
  ### Return
  return(slrTotals)
} ### End get_fig7_slrImpacts

### Plot impacts by degree of warming
plot_DoW_by_modelYear <- function(
    df0,         ### Data (e.g., output from sum_impactsByDegree)
    type0      ="GCM", ### Model type: GCM or SLR
    year0      = 2010,
    xCol       = "driverValue",
    yCol       = "annual_impacts",
    thresh0    = 18,
    nCol       = 4,
    silent     = T,
    options  = list(
      title      = NULL,
      xTitle     = NULL,
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      heights    = NULL,
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ###### Model Types ######
  ### Model Type Checks
  do_gcm     <- "gcm" == tolower(type0)
  do_slr     <- "slr" == tolower(type0)

  ###### Filter Data ######
  ### Filter to model type
  # df0        <- df0 |> filter(summaryYear==year0)
  if(do_gcm){df0 <- df0 |> filter(summaryYear==year0)}
  df0        <- df0 |> filter(model_type ==type0)

  ### Plot by model type
  plot0      <- df0 |> plot_DOW_byModelType(
    modelType = type0,
    xCol      = xCol,
    yCol      = yCol,
    nCol      = nCol,
    options   = options,
    silent    = silent
  ) ### End plot_DOW_byModelType

  ### Return
  return(plot0)
} ### End plot_DoW_by_modelYear

### Plot DOW
plot_DoW <- function(
    df0,         ### Data (e.g., output from sum_impactsByDegree)
    types0   = c("GCM", "SLR"), ### Model type: GCM or SLR
    years0   = c(2010, 2090),
    xCol     = "driverValue",
    yCol     = "annual_impacts",
    thresh0  = 18,
    nCol     = 4,
    silent   = T,
    options  = list(
      title      = NULL,
      xTitle     = NULL,
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      heights    = NULL,
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ### Data frame to iterate over
  # df_types   <- types0   %>%
  #   map(function(.x){tibble(type=.x, year=years0, label=.x |> paste0("_", years0))}) %>%
  #   (function(y){do.call(rbind, y)})
  do_gcm     <- "gcm" %in% tolower(types0)
  do_slr     <- "slr" %in% tolower(types0)
  ### Initialize dataframe
  df_types   <- tibble()
  if(do_gcm){
    df_gcm   <- "GCM" %>%
      # map(function(.x){tibble(type=.x, year=years0, label=.x |> paste0("_", years0))}) %>%
      # (function(y){do.call(rbind, y)})
      map(function(.x){tibble(type=.x, year=years0, label=.x |> paste0("_", years0))}) |>
      bind_rows()
    df_types <- df_types |> rbind(df_gcm)
    rm(df_gcm)
  } ### if(do_gcm)
  ### SLR data
  if(do_slr){
    df_slr   <- tibble(type="SLR", year="all", label="SLR" |> paste0("_", "all"))
    df_types <- df_types |> rbind(df_slr)
    rm(df_slr)
  } ### if(do_slr)
  # "got here" |> print()
  # df_types |> glimpse()

  ### Initialize list to iterate over
  pList0     <- list(x1=df_types[["type"]], x2=df_types[["year"]])
  ### Initialize list
  list0      <- pList0 %>% pmap(function(x1, x2){
    x1 |> paste0("_", x2) |> print()
    plot_y   <- plot_DoW_by_modelYear(
      df0        = df0, ### Data (e.g., output from sum_impactsByDegree)
      type0      = x1,  ### Model type: GCM or SLR
      year0      = x2,
      xCol       = xCol,
      yCol       = yCol,
      thresh0    = thresh0,
      nCol       = nCol,
      options    = options,
      silent     = silent
    ) ### End plot_DoW_by_modelYear

    # plot_y |> print()
    ### Return
    return(plot_y)
  })

  ### Add list names
  # list0 |> print()
  labels0    <- df_types[["label"]]
  list0      <- list0 |> set_names(labels0)

  ### Return
  return(list0)
} ### End plot_DoW

#### Plot information by model type
plot_DoW_by_sector <- function(
    df0,
    models  = c("GCM", "SLR"),
    xCol    = "driverValue",
    yCol    = "impacts_billions",
    options = list(
      title      = "Impacts by Degrees of Warming",
      # subtitle   = NULL,
      # xTitle     = expression("Degrees of Warming (°C)"),
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      lgdPos     = "top",
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ### Data frame to iterate over
  do_gcm     <- "gcm" %in% tolower(models)
  do_slr     <- "slr" %in% tolower(models)
  ### Initialize dataframes
  df_types   <- tibble()
  ### Other values
  years      <- c(2010, 2090)
  ### For GCMs
  if(do_gcm){
    df_gcm   <- "GCM" |> map(function(.x, years0=years){
        # df0 |> glimpse()
        df1      <- df0 |> filter(model_type=="GCM")
        sectors0 <- df1[["sector"]] |> unique()
        df_x     <- sectors0 |> map(function(.y){tibble(type=.x, sector=.y, year=years, label=.y |> paste0("_", years))})
        # df_x     <- df_x %>% (function(y){do.call(rbind, y)})
        df_x     <- df_x |> bind_rows()
        return(df_x)
      })
    # df_gcm   <- df_gcm %>% (function(y){do.call(rbind, y)})
    df_gcm   <- df_gcm |> bind_rows()
    df_types <- df_types |> rbind(df_gcm)
    rm(df_gcm)
  } ### End if(do_gcm)
  ### For SLR
  if(do_slr){
    sectors0 <- (df0 |> filter(model_type=="SLR"))[["sector"]] |> unique()
    df_slr   <- tibble(type="SLR", sector=sectors0, year="all", label=sectors0 |> paste0("_", "all"))
    df_types <- df_types |> rbind(df_slr)
    rm(df_slr, sectors0)
  } ### End if(do_slr)

  ### Get list
  list0    <- models |> map(function(.x){
    paste0("Creating plots for model type ", .x, "...") |> message()
    df_x      <- df0 |> filter(model %in% c(.x))
    ### Sectors
    types_x   <- df_types |> filter(type==.x)
    sectors_x <- types_x[["sector"]]
    # df_types |> glimpse()
    pList_x   <- list(x1=types_x[["sector"]], x2=types_x[["year"]])

    list_x    <- pList_x %>% pmap(function(x1, x2){
      x1 |> paste0("_", x2) |> print()
      df_y   <- df0  |> filter(sector == x1)
      ### If do_gcm, filter to appropriate years
      if(do_gcm){
        c_yrs  <- c("NA", x2)
        df_y   <- df_y |> filter(summaryYear == x2)
        df_y   <- df_y |> filter(impactYear %in% c_yrs)
        # df_y |> glimpse()
      } ### End if(do_gcm)

      plot_y <- df_y |> plot_DOW_byImpactTypes(
        sector    = x1,
        modelType = models,
        yCol      = yCol,
        xCol      = xCol,
        silent    = TRUE,
        options   = options
      )
      # plot_y |> names() |> print()
      ### Return
      return(plot_y)
    })
    ### Add names
    labels_x <- types_x[["label"]]
    list_x   <- list_x |> set_names(labels_x)
    ### Return
    return(list_x)
  })
  ### Add names
  list0   <- list0 |> set_names(models)
  ### Return
  return(list0)
} ### End plot_DoW_by_sector


### Plot driver scenario
plot_slr_scenarios <- function(
    slrDrivers, ### Dataframe of drivers
    title0    = "Global Mean Sea Level Rise",
    subTitle0 = "Sweet et al. SLR Scenarios",
    lgdTitle0 = "Sweet et al. SLR Scenario"
){
  lgd_title0 <- "Sweet et al. SLR Scenario"
  title0     <- title0
  years0     <- seq(2000, 2300, by=25)
  slrDrivers <- slrDrivers |> filter(!(model %in% c("0 cm", "300 cm")))
  dfPoints   <- slrDrivers |> filter(year %in% years0)


  plot0      <- slrDrivers |> ggplot() +
    geom_line(aes(x=year, y=slr_cm, color = model)) +
    geom_point(data=dfPoints, aes(x=year, y=slr_cm, color = model, shape=model))

  plot0      <- plot0 +
    scale_color_discrete(subTitle0) +
    scale_shape_discrete(subTitle0) +
    theme(legend.position = "bottom")

  plot0      <- plot0 +
    scale_x_continuous("Year") +
    scale_y_continuous("GMSL (cm)")

  # plot0      <- plot0 +
  #   theme(panel.background = element_rect(fill="white")) +
  #   theme(panel.grid       = element_line(color="lightgrey")) +
  #   theme(axis.line        = element_line(color="darkgrey"))

  plot0      <- plot0 +
    ggtitle(title0, subTitle0) +
    theme(plot.title       = element_text(hjust = 0.5, size=14)) +
    theme(plot.subtitle    = element_text(hjust = 0.5, size=11))

  ### Return
  return(plot0)
} ### End plot_slr_scenarios

### Function to do some initial summarization
create_default_tablePlot <- function(x=1){
  ### Run FrEDI
  results0 <- FrEDI::run_fredi()
  # results0 |> glimpse()

  ### Filter to values used to report
  results0   <- results0 |>
    filter(model %in% c("Interpolation", "Average")) |>
    filter(includeaggregate == 1) |>
    filter(sectorprimary == 1) |>
    filter(region == "National Total")
  results0 |> glimpse()

  ### Summarize results for specific years
  table0   <- results0 |>
    filter(year %in% seq(2010, 2090, by=10)) |>
    select(c("sector", "variant", "year", "annual_impacts")) |>
    spread(key="year", value="annual_impacts")

  ### Summarize results over all years
  totals0  <- results0 |>
    group_by_at(.vars=c("sector")) |>
    summarize_at(.vars=c("annual_impacts"), sum, na.rm=T) |>
    ungroup()
  totals0  <- totals0|>
    arrange_at(.vars=c("annual_impacts")) |>
    mutate(order=row_number())

  ### Factor results
  results0        <- results0 |> mutate(sector_order  = sector |> factor(levels=totals0[["sector"]]))
  results0        <- results0 |> mutate(sector_factor = sector |> factor(levels=totals0[["sector"]]))
  ### Arrange
  arrange0 <- c("sector_factor", "variant",  "year")
  results0        <- results0 |> arrange_at(.vars=c(arrange0))

  ### Create plot
  plot0    <- results0 |>
    ggplot(aes(x=year, y=annual_impacts/10**12)) +
    geom_area(aes(fill=sector_factor), color="#212121", alpha = 0.75) +
    scale_fill_discrete("Sector") +
    scale_y_continuous("Impacts (Trillions, $2015)") +
    scale_x_continuous("Year", breaks=seq(2010, 2090, by=20)) +
    guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)) +
    theme(legend.position = "bottom")

  ### Create list
  returnList <- list()
  returnList[["table" ]] <- table0
  returnList[["totals"]] <- totals0
  returnList[["plot"  ]] <- plot0
  ### Return
  return(returnList)
} ### End create_default_tablePlot

###### End File ######
