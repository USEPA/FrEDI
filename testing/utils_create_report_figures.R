### Function to get manual colors for regions, states in a region, or models
fun_manual_colors <- function(x=1){
  # colorVals <- c("D81B60", "FF792D", "4E94E4", "735EA0", "67032F", "49D4F5", "D082CD", "68A796")
  # colorVals <- c("D81B60", "B38ADA", "FB9358", "2EAD8A", "49D4F5", "6B7682", "893620", "2C5EB7")
  colorVals <- c("FF5291", "D8B8F7", "7D2E19", "4BD886", "F97E39", "A952EC", "639BFF", "386D79")
  colorVals <- "#" |> paste0(colorVals)
  return(colorVals)
}
# ### Get column values from a tibble
# get_column_values <- function(
#     df0,    ### Tibble
#     col0,   ### Column
#     unique0 = FALSE ### Unique values
# ){
#   vals0 <- df0[[col0]]
#   if(unique0){vals0 <- vals0 |> unique()}
#   return(vals0)
# } ### End get_column_values

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
  # df0    <- df0 |> rename_at(c("yCol"), ~c(col0))
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
    type0 = "conus",
    scen0 = "Other_Integer" |> paste(temp0 |> round(1), sep="_") ### Prefix for scenario
){
  ### Temperature Type
  isConus <- "conus" %in% (type0 |> tolower())
  ### Format scenario label
  # pre0    <- prefix0
  # lab0    <- temp0 |> round(1)
  # scen0   <- pre0  |> paste(lab0, sep="_")
  lab0    <- scen0 |> str_extract("(\\d+)(\\.)(\\d+)")
  ### Get annual values 1995 - 2010: starting with zero in 1995
  xIn0    <- c(1995, 2010)
  yIn0    <- c(0, temp0)
  xOut0   <- seq(xIn0[1], xIn0[2])
  df0     <- approx(x=xIn0, y=yIn0, xout=xOut0) |>
    as_tibble() |>
    rename(year=x, temp_C=y)
  ### Extend values
  df1     <- tibble(year = seq(2011, 2090, by=1))
  df1     <- df1 |> mutate(temp_C = temp0)
  df0     <- df0 |> rbind(df1)
  rm(df1)

  ### Get other temp types and rename
  if(isConus){
    df0 <- df0 |> mutate(temp_C_global = temp_C |> FrEDI::convertTemps(from="conus"))
    df0 <- df0 |> mutate(temp_C_conus  = temp_C)
  } ### End if(isConus)
  else       {
    df0 <- df0 |> mutate(temp_C_conus  = temp_C |> FrEDI::convertTemps(from="global"))
    df0 <- df0 |> mutate(temp_C_global = temp_C)
  } ### End else(isConus)
  ### Drop temp_C
  df0     <- df0 |> select(-c("temp_C"))
  ### Get SLR
  ySlr0   <- FrEDI::temps2slr(temps = df0[["temp_C_global"]], years = df0[["year"]])
  df0     <- df0 |> left_join(ySlr0, by="year")
  df0     <- df0 |> mutate(temp_lab = lab0)
  df0     <- df0 |> mutate(scenario = scen0)
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
    rm(temp0)
  } ### End if(doTemp)

  if(doSlr){
    slr0   <- df0 |> select(all_of(cSlr))
    list0[["slrInput"]] <- slr0
    rm(slr0)
  } ### End if(doSlr)

  if(doGdp){
    gdp0   <- df0 |> select(all_of(cGdp))
    list0[["gdpInput"]] <- gdp0
    rm(gdp0)
  } ### End if(doGdp)

  if(doPop){
    pop0   <- df0 |> select(all_of(cPop))
    list0[["popInput"]] <- pop0
    rm(pop0)
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
  df1     <- df0 |> select(c(all_of(scenCols))); rm(df0)
  list1   <- df1 |> get_scenario_inputsList()
  ### Run FrEDI
  df2     <- FrEDI::run_fredi(inputsList=list1, sectorList=sectors, aggLevels="none")
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
  df0     <- df0 |> FrEDI::aggregate_impacts(aggLevels=aggLevels, groupByCols=group0)
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
    years     = c(2090), ### Years to which to filter results
    scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
    joinCols  = c("year"),
    aggLevels = c("modelaverage", "national"),
    save      = FALSE,
    return    = TRUE,
    outPath   = "." |> file.path("report_figures")
){
  ### Values & conditions
  agg0       <- !("none" %in% aggLevels)
  scenario0  <- scenario
  rm(scenario)
  # agg0 |> print()

  ### File names and info
  fType0     <- "rda"
  fName0     <- "integer_results_byType" |> paste0("_", scenario0)
  # fName0 <- fName0 |> paste0(".", fType0)

  ### Filter to scenario
  cScenarios <- df0 |> pull(scenario) |> unique()
  nScenarios <- cScenarios |> length()
  df0        <- df0 |> filter(scenario==scenario0)
  # df0 |> nrow() |> print()

  ### Message user
  "\n" |> paste0("Running scenario ", (cScenarios == scenario0) |> which(), "/" , nScenarios, "...") |> message()

  ### Run FrEDI
  if(fredi) df0 <- df0 |> run_fredi_scenario(sectors=sectors, scenCols=scenCols, joinCols=joinCols)
  # "got here1" |> print(); df0 |> glimpse()

  ### Aggregate FrEDI
  if(agg0 ) df0 <- df0 |> agg_fredi_scenario(scenCols=scenCols, joinCols=joinCols, aggLevels=aggLevels)
  # "got here2" |> print(); df0 |> glimpse()

  ### Format other values
  mutate0   <- c("temp_C_conus", "temp_C_global", "slr_cm")
  df0       <- df0 |> mutate_at(vars(mutate0), as.numeric)

  ### Filter to years
  doYears   <- !(years |> is.null())
  if(doYears) df0 <- df0 |> filter(year %in% years)

  ### Save results
  if(save) {
    ### Message user
    "\t" |> paste0("Saving integer scenario results...") |> message()
    ### Save results
    df0 |> save_data(fpath=outPath, fname=fName0, ftype=fType0)
  } ### End if(saveFile)

  ### Return
  if(return) return(df0)
} ### End function run_scenario


### Run list of scenarios
run_scenarios <- function(
    df0,      ### Output of create_constant_temp_scenario
    col0      = "scenario", ### Scenario column
    fredi     = TRUE,
    sectors   = FrEDI::get_sectorInfo(), ### Which sectors
    years     = c(2090), ### Years to which to filter results
    aggLevels = c("modelaverage", "national"),
    scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
    joinCols  = c("year"),
    save      = FALSE,
    return    = TRUE,
    outPath   = "." |> file.path("report_figures")
){
  ### Unique scenarios
  scenarios0 <- df0[[col0]] |> unique()
  # nScenarios <- scenarios0 |> length()

  ### Iterate over the scenarios
  results0   <- scenarios0 |> map(
    run_scenario,
    df0       = df0,
    fredi     = fredi,
    sectors   = sectors,
    years     = years,
    aggLevels = aggLevels,
    scenCols  = scenCols,
    joinCols  = joinCols,
    save      = save,
    return    = return,
    outPath   = outPath
    ) |> bind_rows()

  ### Return
  return(results0)
  # list0       <- list()
  # for(scenario_i in scenarios0) {
  #   # ### Message user
  #   # "Running scenario " |> paste0((scenarios0 == scenario_i) |> which(), "/" , nScenarios, "...") |> message()
  #   ### Run scenario
  #   df_i <- scenario_i |> run_scenario(
  #     df0       = df0,
  #     fredi     = fredi,
  #     sectors   = sectors,
  #     aggLevels = aggLevels,
  #     scenCols  = scenCols,
  #     joinCols  = joinCols,
  #     save      = save,
  #     return    = return,
  #     outPath   = outPath
  #   ) ### End run_scenario(scenario_i)
  #   ### Add scenario to list
  #   list0[[scenario_i]] <- df_i
  #   ### Drop values
  #   rm(scenario_i, df_i)
  # } ### for(scenario_i in scenarios0)
  # rm(df0)
  #
  # ### Bind values into a list
  # df0    <- list0 |> bind_rows()
  # rm(list0)
  #
  # ### Return
  # return(df0)
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
  ### Values
  primary    <- !bySector
  scenarios0 <- scenarios
  years0     <- impactYears
  year0      <- year
  rm(scenarios)

  do_gcm     <- "gcm" %in% (models |> tolower())

  ### Filter to includeaggregate>=1
  ### Filter to sector primary
  ### Filter to appropriate year
  if(aggOnly){df0 <- df0 |> filter(includeaggregate>=1)}
  if(primary){df0 <- df0 |> filter(sectorprimary   ==1)}
  if(do_gcm ){df0 <- df0 |> filter(year == year0)}

  ### Filter to scenarios
  ### Filter to appropriate models
  ### Filter to appropriate impact years
  df0        <- df0 |> filter(scenario %in% scenarios0)
  df0        <- df0 |> filter(model_type %in% models)
  df0        <- df0 |> filter(impactYear %in% years0)

  ### Summarize by Degree of Warming
  # list0      <- years0 %>% map(function(.z){
  #   df_z <- df0 |> summarize_DOW_data(
  #     year       = year0,
  #     bySector   = bySector,
  #     sumCol     = sumCol,
  #     groupVars  = groupVars,
  #     impactYear = .z,
  #     silent     = silent
  #   )
  #   return(df_z)
  # })
  list0      <- list()
  for(year_i in years0) {
    ### Summarize DOW data
    df_i <- df0 |> summarize_DOW_data(
      year       = year0,
      bySector   = bySector,
      sumCol     = sumCol,
      groupVars  = groupVars,
      impactYear = year_i,
      silent     = silent
    ) ### End summarize_DOW_data()
    ### Add to list
    list0[[year_i]] <- df_i
    ### Drop values
    rm(year_i, df_i)
  } ### End for(year_i in years0)
  rm(df0)

  ### Bind together
  ### Add summary year
  df0        <- list0 |> bind_rows()
  df0        <- df0 |> mutate(summaryYear=year0)
  rm(list0)

  ### Adjust values
  df0[[adjCol]] <- df0[["annual_impacts"]] * adjVal

  ### Select columns
  # select0    <- c("sector", "region", "model_type", "model", "summaryYear", "driverValue", "annual_impacts", adjCol) |> unique()
  # df0        <- df0 |> relocate(c(all_of(select0)))
  # df0 %>% glimpse()

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
  ### Values
  years0     <- years
  nYears     <- years0 |> length()
  primary    <- !bySector

  ### Filter to includeaggregate>=1
  ### Filter to sector primary
  if(aggOnly){df0 <- df0 |> filter(includeaggregate>=1)}
  if(primary){df0 <- df0 |> filter(sectorprimary   ==1)}

  ### Run scenarios
  ### Get list
  # list0   <- years |> map(function(.x){
  #   "Summarizing values for " |> paste0((years == .x) |> which(), "/" , nYears, " years...") |> message()
  #   df_x <- sum_impacts_byDoW(
  #     df0         = df0,
  #     scenarios   = scenarios,
  #     bySector    = bySector,
  #     sumCol      = sumCol,
  #     groupVars   = groupVars,
  #     impactYears = impactYears,
  #     year        = .x,
  #     models      = models,
  #     aggOnly     = aggOnly,
  #     adjVal      = adjVal,
  #     adjCol      = adjCol,
  #     silent      = silent
  #   ) ### End sum_impactsByDegree
  #   return(df_x)
  # }) ### End walk
  list0      <- list()
  for(year_i in years0) {
    ### Message user
    "Summarizing values for " |> paste0((years == year_i) |> which(), "/" , nYears, " years...") |> message()

    ### Summarize DOW data
    df_i <- df0 |> sum_impacts_byDoW(
      scenarios   = scenarios,
      bySector    = bySector,
      sumCol      = sumCol,
      groupVars   = groupVars,
      impactYears = impactYears,
      year        = year_i,
      models      = models,
      aggOnly     = aggOnly,
      adjVal      = adjVal,
      adjCol      = adjCol,
      silent      = silent
    ) ### End sum_impactsByDegree()

    ### Add to list
    list0[[year_i]] <- df_i

    ### Drop values
    rm(year_i, df_i)
  } ### End for(year_i in years0)
  rm(df0)

  ### Bind values together
  df0        <- list0 |> bind_rows()
  rm(list0)

  ### Convert to tibble
  df0        <- df0 |> as_tibble()

  ### Return
  return(df0)
} ### End sum_impacts_byDoW_years

### Get SLR impacts from FrEDI data
get_fig7_slrDataObj <- function(
    drivers = TRUE, ### Whether to return drivers
    impacts = TRUE
    # impacts = TRUE, ### Whether to return impacts
    # years   = c(2050, 2090),
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
  dfSectors <- dfSectors |> rename_at(c(rename0), ~c(rename1))
  dfSectors <- dfSectors |> filter(tolower(model_type)=="slr")
  dfSectors <- dfSectors |> mutate_at(c(mutate0), as.character)
  rm(select0, rename0, rename1, mutate0)

  ###### Variants Data ######
  ### Format Variants data
  select0   <- c("sector_id", "variant_id", "variant_label", "sectorprimary", "includeaggregate")
  rename0   <- c("variant_label")
  rename1   <- c("variant")
  dfVariant <- dfVariant |> select(c(all_of(select0)))
  dfVariant <- dfVariant |> rename_at(c(rename0), ~c(rename1))
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
    slrCm     <- slrCm |> rename_at(c(rename0), ~c(rename1))
    rm(select0, rename0, rename1)

    ### Add values for 0cm, 300 cm
    slrCm     <- slrCm  |> (function(y){
      y    <- y |> mutate(model = model |> as.character())
      y300 <- y |> filter(model=="250cm") |> mutate(model="300cm")
      y    <- y |> rbind(y300)
      return(y)
    })()

    ### Mutate labels & levels
    slrCm     <- slrCm |> mutate(model = model |> factor(levels=slrLevels, labels=slrLabels))

    ### Filter to specific years
    slrCm     <- slrCm |> filter(year %in% years)

    ### Arrange values
    arrange0  <- c("model", "year")
    slrCm     <- slrCm |> arrange_at(c(arrange0))
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
    slrImp    <- slrImp |> rename_at(c(rename0), ~c(rename1))
    slrImp    <- slrImp |> select(-c(all_of(drop0)))
    rm(rename0, rename1, drop0)

    ### Adjust names
    exclude0  <- c("year", "annual_impacts")
    mutate0   <- slrImp |> names() |> (function(y1, y2=exclude0){y1[!(y1 %in% y2)]})()
    slrImp    <- slrImp |> mutate_at(c(mutate0), as.character)
    slrImp    <- slrImp |> mutate(model = model |> factor(levels=slrLevels, labels=slrLabels))
    rm(exclude0, mutate0)

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
    slrImp    <- slrImp |> (function(y){
      y     <- y |> filter(!(model %in% c("0 cm")))
      yLo   <- y |> filter(model=="30 cm" ) |> mutate(annual_impacts=0) |> mutate(model="0 cm")
      yHi   <- y |> filter(model=="250 cm") |> mutate(model="300 cm")
      y     <- yLo |> rbind(y) |> rbind(yHi)
      return(y)
    })()

    ### Mutate labels & levels
    slrImp    <- slrImp |> mutate(model = model |> as.character())
    slrImp    <- slrImp |> mutate(model = model |> factor(levels=slrLabels, labels=slrLabels))

    ### Filter to specific years
    slrImp    <- slrImp |> filter(year %in% years)

    ### Arrange values
    arrange0  <- c("sector", "variant", "impactType", "impactYear", "region", "model_type", "model", "year")
    slrImp    <- slrImp |> arrange_at(c(arrange0))
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
    aggOnly    = TRUE, ### Whether to filter to includeaggregate>=1
    years      = c(2050, 2090),
    adjVal     = 1/10**9, ### Factor to multiply by
    adjCol     = "impact_billions"
){
  ###### Values
  primary <- !bySector

  ###### Model Labels ######
  ### SLR sectors separately:
  ### - Filter to 2090 and 2050 values
  ### - Calculate national totals
  ### - Combine CIRA impacts and SLR trajectories
  modelLabels     <- slrDrivers[["model"]] |> levels() |> as.character()
  modelHeights    <- modelLabels |> map(function(.x){str_split(string=.x, pattern="\\s")[[1]][1]}) |> unlist() |> as.numeric()

  ###### Format data ######
  ### Filter to includeaggregate>=1
  ### Filter to primary==1
  ### Filter to appropriate categories and years
  if(aggOnly){slrImpacts <- slrImpacts |> filter(includeaggregate>=1)}
  if(aggOnly){slrImpacts <- slrImpacts |> filter(sectorprimary==1)}
  if(!bySector){slrImpacts <- slrImpacts |> filter(year %in% years)}

  ### Filter to appropriate models
  slrImpacts      <- slrImpacts |> filter(model %in% modelLabels)

  ### Filter to national totals or calculate national totals
  slrImpacts      <- slrImpacts |> filter(region!="National Total")
  c_regions       <- slrImpacts[["region"]] |> unique()
  n_regions       <- c_regions |> length()

  ### Change column names
  rename0         <- "model"
  rename1         <- "SLR_scenario"
  slrDrivers      <- slrDrivers |> rename_at(c(rename0), ~c(rename1))
  slrImpacts      <- slrImpacts |> rename_at(c(rename0), ~c(rename1))
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
      summarize(n=n(), .groups="drop")
    # n_impTypes     <- count_impTypes[["n"]] |> max()

    ### Join counts with totals
    join0           <- group0
    slrTotals       <- slrTotals |> left_join(count_impTypes, by = c(join0))
    rm(join0, count_impTypes)

    ### Summarize
    slrTotals       <- slrTotals |> sum_with_na(
      group0    = group0, ### Grouping columns
      col0      = sumCol,
      threshCol = "n",    ### Threshold to check against
      drop0     = TRUE    ###
    ) |> select(-c("n"))

    ### Add totals
    slrTotals       <- slrTotals |> mutate(impactType = "All")
    # slrTotals |> names() |> print()
  }

  ###### National Totals ######
  ### Calculate national totals
  group0          <- c("sector", "variant", "impactType", "impactYear", "model_type", "SLR_scenario", "model", "year")
  slrTotals       <- slrTotals |> mutate(threshold = n_regions)
  slrTotals       <- slrTotals |> sum_with_na(
    group0    = group0,      ### Grouping columns
    col0      = sumCol,
    threshCol = "threshold", ### Threshold to check against
    drop0     = TRUE         ###
  ) |> select(-c("threshold"))

  slrTotals       <- slrTotals |> mutate(region = "National Total")
  # slrTotals |> names() |> print()

  ######  #####Adjust Values ######
  ### Adjust values
  slrTotals[[adjCol]] <- slrTotals[[sumCol]] * adjVal

  ###### Format Results ######
  ### Join with driver info
  rename0         <- c("slr_cm"     , "year")
  rename1         <- c("driverValue", "summaryYear")
  join0           <- c("year", "SLR_scenario")
  select0         <- c("sector", "region", "model_type", "SLR_scenario", "model", "summaryYear", "driverValue") |> c(sumCol, adjCol) |> unique()
  slrTotals       <- slrTotals |> left_join(slrDrivers, by=c(join0))
  slrTotals       <- slrTotals |> rename_at(c(rename0), ~c(rename1))
  slrTotals       <- slrTotals |> relocate(c(all_of(select0)))
  rm(rename0, rename1, join0)

  ### Adjust column names if bySector
  if(bySector){
    slrTotals <- slrTotals |> mutate(model=SLR_scenario)
    slrTotals <- slrTotals |> rename(year=summaryYear)
    slrTotals <- slrTotals |> select(-c("SLR_scenario"))
  } ### End if(bySector)

  slrTotals |> glimpse()
  ### Return
  return(slrTotals)
} ### End get_fig7_slrImpacts

### Plot impacts by degree of warming
plot_DoW_by_modelYear <- function(
    df0,     ### Data (e.g., output from sum_impactsByDegree)
    type0    ="GCM", ### Model type: GCM or SLR
    year0    = 2010,
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
      margins    = c(0, 0.05, .15, 0.05),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ###### Model Types ######
  ### Model Type Checks
  do_gcm     <- "gcm" == tolower(type0)
  do_slr     <- "slr" == tolower(type0)

  ###### Filter Data ######
  ### Filter to summary year
  if(do_gcm){df0 <- df0 |> filter(summaryYear==year0)}

  ### Filter to model type
  df0        <- df0 |> filter(model_type == type0)

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


### Function to create a dataframe to iterate over, by model type
fun_create_df_types <- function(
    types0   = c("GCM", "SLR"), ### Model types to get options for
    years0   = c(2010, 2090),   ### Result years to get impacts for
    bySector = FALSE,           ### Whether to get options by sector
    df0      = FrEDI::get_sectorInfo(description=T) |> select(c("sector", "model_type")) ### If bySector=TRUE, dataframe to get sectors from
    # df0      = tibble() |> mutate(sector="N/A", model_type=types0) ### If bySector=TRUE, dataframe to get sectors from
){
  ### Get sector info
  select0  <- c("sector", "model_type")
  df0      <- df0 |> select(all_of(select0)) |> unique()
  rm(select0)

  ### Create tibble
  df_types <- types0 |> map(function(.x){
    ### Condition
    do_gcm <- "gcm" %in% (.x |> tolower())
    ### Years
    if(do_gcm){yrs_x <- years0} else{yrs_x <- "all"}
    ### Create tibble
    df_x   <- tibble(model_type=.x, year=yrs_x)
    ### Mutate year
    df_x   <- df_x |> mutate(year = year |> as.character())
    ### Add label
    df_x   <- df_x |> mutate(label = model_type |> paste0("_", year))
    ### Return
    return(df_x)
  }) |> bind_rows()
  # df_types |> glimpse()

  ### If bySector = TRUE, add sector info
  if(bySector){
    df_types <- types0 |> map(function(
    .x,
    df_x=df_types |> filter(model_type==.x),
    df_y=df0      |> filter(model_type==.x)
    ){
      ### Add sector info
      join0  <- c("model_type")
      df_x   <- df_x |> left_join(df_y, by=c("model_type"))

      ### Arrange
      sort0  <- c("model_type", "sector", "year")
      df_x   <- df_x |> select(all_of(sort0))
      df_x   <- df_x |> arrange_at(c(sort0))

      ### Mutate label
      df_x   <- df_x |> mutate(label = sector |> paste0("_", year))

      ### Return
      return(df_x)
    }) |> bind_rows()
  } ### End if(bySector)

  ### Return
  return(df_types)
} ### fun_create_df_types


### Plot DOW
plot_DoW <- function(
    df0,     ### Data (e.g., output from sum_impactsByDegree)
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
      margins    = c(0, 0.05, .15, 0.05),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ### Data frame to iterate over
  do_gcm     <- "gcm" %in% (types0 |> tolower())
  do_slr     <- "slr" %in% (types0 |> tolower())

  ### Initialize dataframe
  df_types   <- types0 |> fun_create_df_types(years0=years0)
  # "got here" |> print()
  # df_types |> glimpse()


  # ### Initialize list to iterate over
  list0      <- list()
  for(i in df_types |> nrow() |> seq_len()){
    ### Message user
    x1_i   <- df_types[["model_type"]][i]
    x2_i   <- df_types[["year"      ]][i]
    x_i    <- df_types[["label"     ]][i]
    x_i |> print()

    ### Whether to do GCM
    gcm_i  <- "gcm" %in% (x1_i |> tolower())
    if(gcm_i) x2_i <- x2_i |> as.numeric()

    ### Plot by model year
    plot_i <- df0 |> plot_DoW_by_modelYear(
      type0      = x1_i,  ### Model type: GCM or SLR
      year0      = x2_i,  ### Year
      xCol       = xCol,
      yCol       = yCol,
      thresh0    = thresh0,
      nCol       = nCol,
      options    = options,
      silent     = silent
    ) ### End plot_DoW_by_modelYear()

    ### Add to list
    list0[[x_i]] <- plot_i
    rm(i, x1_i, x2_i, x_i, plot_i)
  } ### End for(i in df_types |> nrow() |> seq_len())
  rm(df0)

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
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      lgdPos     = "top",
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ### Values & Conditions
  years0     <- c(2010, 2090)

  ### Data frame to iterate over
  do_gcm     <- "gcm" %in% (models |> tolower())
  do_slr     <- "slr" %in% (models |> tolower())

  ### Dataframe to iterate over
  df_types   <- models |> fun_create_df_types(years0=years0, bySector=TRUE, df0=df0)

  ### Get list
  list0    <- list()
  for(model_i in models){
    ### Message user
    "Creating plots for model type " |> paste0(model_i, "...") |> message()

    ### Filter tibbles
    # df_i      <- df0      |> filter(model_type %in% model_i)
    types_i   <- df_types |> filter(model_type %in% model_i)

    ### Iterate over rows in types_i
    list_i    <- list()
    for(row_j in types_i |> nrow() |> seq_len()){
      ### Get values
      x1_j     <- types_i[["sector"]][row_j]
      x2_j     <- types_i[["year"  ]][row_j]
      x_j      <- types_i[["label" ]][row_j]
      x_j |> print()

      ### Get type and condition
      type_j   <- types_i[["model_type"]][row_j]
      do_gcm_j <- "gcm" %in% (type_j |> tolower())

      ### Filter to sector
      df_j     <- df0  |> filter(sector == x1_j)

      ### If do_gcm, filter to appropriate years
      if(do_gcm_j){
        yrs_j  <- "NA" |> c(x2_j)
        df_j   <- df_j |> filter(summaryYear == x2_j)
        df_j   <- df_j |> filter(impactYear %in% yrs_j)
        # df_j |> glimpse()
        rm(yrs_j)
      } ### End if(do_gcm_j)

      ### Plot j
      plot_j  <- df_j |> plot_DOW_byImpactTypes(
        sector    = x1_j,
        modelType = type_j,
        yCol      = yCol,
        xCol      = xCol,
        silent    = TRUE,
        options   = options
      ) ### End plot_DOW_byImpactTypes()
      # plot_j |> names() |> print()

      ### Add plot to list
      list_i[[x_j]] <- plot_j

      ### Remove values
      rm(row_j, type_j, do_gcm_j, x1_j, x2_j, x_j, df_j, plot_j)
    } ### End for(row_j in types_i)

    ### Add list to list0
    list0[[model_i]] <- list_i

    ### Remove values
    rm(model_i, types_i, list_i)
  } ### End for(model_i in models)


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
  results0 <- results0 |>
    filter(model %in% c("Interpolation", "Average")) |>
    filter(includeaggregate >= 1) |>
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
    group_by_at(c("sector")) |>
    summarize_at(c("annual_impacts"), sum, na.rm=T) |>
    ungroup()
  totals0  <- totals0|>
    arrange_at(c("annual_impacts")) |>
    mutate(order=row_number())

  ### Factor results
  results0 <- results0 |> mutate(sector_order  = sector |> factor(levels=totals0[["sector"]]))
  results0 <- results0 |> mutate(sector_factor = sector |> factor(levels=totals0[["sector"]]))
  ### Arrange
  arrange0 <- c("sector_factor", "variant",  "year")
  results0 <- results0 |> arrange_at(c(arrange0))

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
