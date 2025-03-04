### Graphics Creation and experimentation for FrEDI
### Setup Environment -----------------------------------------------------
#### Load packages, etc. -----------------------------------------------------
require(devtools)
require(tidyverse)
# require(maps)
# require(sf)
# require(patchwork)

#### File Paths -----------------------------------------------------
# setwd("C:/Users/heaston/FrEDI local/FrEDI Clone/Graphics Creation")
projDir       <- "."
workDir       <- "~" |> file.path("projects", "CIRA", "analysis", "20250114_miscFigures")
codeDir       <- workDir |> file.path("code"   ); codeDir |> list.files(pattern=".R") |> print()
dataDir       <- workDir |> file.path("inputs" ); dataDir |> list.files() |> print()
pCodeDir      <- projDir |> file.path("testing"); pCodeDir |> list.files() |> print()
saveDir       <- workDir |> file.path("outputs"); saveDir |> list.files() |> print()
svDir         <- saveDir |> file.path("outputs"); svDir   |> list.files() |> print()

### Function to load code
loadLocalCode <- function(path0=pCodeDir){path0 |> list.files(full.names=TRUE, pattern=".R") |> walk(source)}

### Strings
csv0          <- paste0(".", "csv")
rda0          <- paste0(".", "rda")
tiff0         <- paste0(".", "tiff")

### Input files
sectNamesFile <- "sector_names"  |> paste0(csv0)
sectLabsFile  <- "sector_labels" |> paste0(csv0)
ncaRegFile    <- "ncaRegions"    |> paste0(csv0)
svTempFile    <- "temp_baseline_" |> paste0("8653", csv0)
svPopFile     <- "pop_"           |> paste0("8653", csv0)
# popDataFile   <- "popDefault"   |> paste0(".", "rda")

sectNamesPath <- dataDir |> file.path(sectNamesFile)
ncaRegPath    <- dataDir |> file.path(ncaRegFile   )
svTempPath    <- svDir   |> file.path(svTempFile   )
svPopPath     <- svDir   |> file.path(svPopFile    )
# popDataPath   <- dataDir |> file.path(popDataFile  )

### Output files
natDataFile   <- "nationalScenario" |> paste0(csv0)
popDataFile   <- "popScenario" |> paste0(csv0)
regPopFile    <- "regionalPop" |> paste0(csv0)

### Whether to load data or run new results
doLoad        <- TRUE
doRun         <- !doLoad
# doRun         <- FALSE

#### Load Local FrEDI ----------------------------------------------------
### - Read in package from local repo (make sure its at the correct branch)
projDir |> devtools::load_all()
loadLocalCode()

### Set Params ------------------------------------------
##### Sector labels ---------------------------------------------------------
#### - Read in new sector name conversions:
# dfSectNames   <- sectNamesPath |> read.csv(); dfSectNames |> glimpse()

### Get dataframe with sector graph labels
### - Get sector types
# fun_sectorTypes() |> glimpse()
### Get sector labels
dfSectLbls <- "graphLabel" |> fun_sectorLabels(
  sectors0 = c("ATS Temperature-Related Mortality", "CIL Temperature-Related Mortality", "Climate-Driven Changes in Air Quality",
               "Electricity Demand and Supply", "Electricity Transmission and Distribution", "Marsh Migration (Primary)",
               "Marsh Migration (Secondary)", "Transportation Impacts from High Tide Flooding"),
  labels0  = c("ATS Temp Mortality", "CIL Temp Mortality", "Climate-Driven AQ",
               "Elec. Demand & Supply", "Elec. Trans. & Distr.", "Marsh Migration (P)",
               "Marsh Migration (S)", "Transport. Impacts HTF")
); dfSectLbls |> glimpse()

### Add sector types
dfSectTypes <- dfSectLbls |>
  addSectorTypes(types0= fun_sectorTypes()) |>
  fun_colorSectors(); dfSectTypes |> glimpse()

# c("Asphalt Roads", "CIL Temperature-Related Mortality", "Extreme Temperature", "Marsh Migration (Secondary)", "Winter Recreation")
### Add sector attributes
# loadLocalCode()
dfSects <- dfSectLbls |>
  addSectorAttribute(
    col0      = c("exclude"), ### New column
    filter0   = c("sector"),
    sectors0  = c("Asphalt Roads", "CIL Temperature-Related Mortality", "Extreme Temperature",
                  "Marsh Migration (Secondary)", "Winter Recreation")
  ) |>
  addSectorAttribute(
    col0      = c("hasPhysical"), ### New column
    filter0   = c("sector"),
    sectors0  = c("Climate-Driven Changes in Air Quality", "Asphalt Roads", "ATS Temperature-Related Mortality",
                  "CIL Crime", "CIL Temperature-Related Mortality", "Extreme Temperature",
                  "Labor", "Lyme Disease", "Marsh Migration (Secondary)", "Outdoor Recreation",
                  # "Rail", "Roads",
                  "Southwest Dust", "Suicide", "Valley Fever", "Vibriosis", "Wildfire")
  ) |>
  addSectorAttribute(
    col0      = c("hasVariants"), ### New column
    filter0   = c("sector"),
    sectors0  = c("Climate-Driven Changes in Air Quality", "ATS Temperature-Related Mortality*",  "CIL Agriculture",
                  "CIL Temperature-Related Mortality", "Coastal Properties", "Electricity Transmission and Distribution",
                  "Extreme Temperature*", "Forestry Loss", "Transportation Impacts from High Tide Flooding",
                  "Marsh Migration (Primary)", "Marsh Migration (Secondary)", "Outdoor Recreation", "Rail", "Roads")
  ); dfSects |> glimpse()


##### Sector groups ---------------------------------------------------------
### These are just sectors for which include aggregate is no
exclSectors      <- c("Asphalt Roads", "CIL Temperature-Related Mortality", "Extreme Temperature",
                      "Marsh Migration (Secondary)", "Winter Recreation")

#### - Physical sectors
sectors_physical <- c("Climate-Driven AQ", "ATS Temp Mortality", "CIL Crime", "CIL Temp Mortality",
                      "Extreme Temperature", "Labor", "Suicide", "Southwest Dust", "Valley Fever",
                      "Wildfire") |> sort()
#### - Sectors with variants
sectors_variants <- c("Climate-Driven AQ*", "ATS Temp Mortality*",  "CIL Agriculture", "CIL Temp Mortality*",
                      "Coastal Properties", "Elec. Trans. & Distr.", "Extreme Temperature*", "Forestry Loss",
                      "Transport. Impacts HTF", "Marsh Migration (Primary)", "Marsh Migration (Secondary)",
                      "Outdoor Recreation", "Rail", "Roads") |> sort()




##### Columns ------------------------------------------
selectCols0   <- c("sector", "variant", "impactType", "region", "model_type", "driverType",
                   "sectorprimary", "physicalmeasure", "year", "driverValue", "gdp_usd")
cStateRawCols <- c("sector", "region", "state", "postal", "physicalmeasure", "national_pop", "pop", "year")

##### Years -----------------------
### Years:
ts_maxYear    <- 2100
yrs_ts        <- seq(2010, ts_maxYear, by=1)
exportYears   <- seq(2020, ts_maxYear, by=5 )
eraYears0     <- seq(2050, ts_maxYear, by=20)


### Setup Custom Scenarios -------------------------
#### Temperature Scenarios
# tempsList[["base"]] <- seq(from=0, to=6, length.out=91)
# tempsList[["miti"]] <- seq(from=0, to=5.9999, length.out=91)
### List names
base0         <- "base"
miti0         <- "miti"
listNames     <- c(base0, miti0)
dfList        <- tibble(abbr = listNames) |>
  mutate(label = c("baseline", "mitigated")) |>
  mutate(yearB = 2000) |>
  mutate(year0 = yrs_ts |> min()) |>
  mutate(yearN = yrs_ts |> max()) |>
  mutate(temp0 = 0) |>
  mutate(tempN = 6 + c(1, -1e-4))
listLabels    <- dfList |> pull(label) |> as.list() |> set_names(listNames)
### Temp timeseries
### Load or run and save
if(doLoad) {
  tempsList <- listNames |> map(function(name_i, lbl_i=listLabels[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_temp_C", csv0)
    obj_i  <- path_i  |> read.csv()
    return(obj_i)
  }) |> set_names(listNames)
  tempsList |> glimpse()
} else {
  tempsList     <- dfList |>
    select(c("yearB", "year0", "yearN", "temp0", "tempN")) |>
    pmap(function(yearB, year0, yearN, temp0, tempN){
      ### Main series
      df0   <- tibble(year = year0:yearN)
      len0  <- df0 |> pull(year) |> length()
      df0   <- df0 |> mutate(temp_C=seq(temp0, tempN, length.out=len0))
      ### Add minimum values
      temp1 <- df0 |> filter(year %in% year0) |> pull(temp_C)
      df1   <- tibble(year = yearB:(year0 - 1))
      len1  <- df1 |> pull(year) |> length()
      df1   <- df1 |> mutate(temp_C=temp1 |> rep(len1))
      ### Bind
      df0   <- df1 |> rbind(df0)
      ### Return
      return(df0)
    }) |> set_names(listNames)
  tempsList |> glimpse()
  ### Save files
  listNames |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=tempsList[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_temp_C", csv0)
    obj_i |> write.csv(file=path_i, row.names=F)
  })
} ### End if doLoad
# tempsList[[2]]$temp_C |> range()
tempsList[[1]]$year |> range()



### Run FrEDI ------------------------------------------
#### Appendix H. -------------------------------------------------------------------
# results_figH <- run_fredi(aggLevels=c("modelaverage", "national", "impactYear")) |>
#   filter(year %in% seq(2010, 2090, by=5))
# # results_figH |> write.csv(file="." |> file.path("FigH_results.csv"), row.names=F) |> try()
# # rm(results_figH)


#### Run FrEDI ------------------------------------------
# loadLocalCode()
### Load or run and save
if(doLoad) {
  listResults0 <- listNames |> map(function(name_i, lbl_i=listLabels[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results", rda0)
    obj_i  <- path_i  |> funLoadData(name0="obj_i")
    return(obj_i)
  }) |> set_names(listNames)
  listResults0 |> glimpse()
} else {
  listResults0  <- tempsList |>
    map(function(df_i){
      list(temp=df_i) |> run_fredi(elasticity=1, aggLevels="none", allCols=TRUE)
    }) |>
    setNames(listNames)
  listResults0 |> glimpse()
  listNames |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=listResults0[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results", rda0)
    obj_i |> save(file=path_i)
  }) ### End walk
} ### End if doLoad
### Check values
listResults0 |> map(function(df0){df0 |> filter(year %in% 2090) |> summarize_at(c("annual_impacts"), sum, na.rm=T)})


### Summarize Scenarios ------------------------------------------
##### National population, gdp, gdp percap scenario --------------------------------------------------------
### Load or run and save
### National scenario
# loadLocalCode()
if(doLoad) {
  natScenario <- read.csv(file=saveDir |> file.path(natDataFile))
} else {
  natScenario <- listResults0[[1]] |> getNationalScenario_fromResults(); natScenario |> glimpse()
  natScenario |> write.csv(file=saveDir |> file.path(natDataFile), row.names=F)
} ### End if doLoad

##### State population scenario --------------------------------------------------------
#### State population scenario
### Load or run and save
if(doLoad) {
  popScenario <- read.csv(file=saveDir |> file.path(popDataFile)); popScenario |> glimpse()
} else {
  popScenario <- listResults0[[1]] |> getPopScenario_fromResults(); popScenario |> glimpse()
  popScenario |> write.csv(file=saveDir |> file.path(popDataFile), row.names=F)
} ### End if doLoad

##### Regional population scenario --------------------------------------------------------
### Load or run and save
if(doLoad) {
  regPop      <- read.csv(file=saveDir |> file.path(regPopFile)); regPop |> glimpse()
} else {
  regPop      <- popScenario |> summarizeRegPop(); regPop |> glimpse()
  regPop |> write.csv(file=saveDir |> file.path(regPopFile), row.names=F)
} ### End if doLoad

##### NCA regions --------------------------------------------------------
### Load or run and save
if(doLoad) {
  ncaRegions  <- read.csv(file=saveDir |> file.path(ncaRegFile)); ncaRegions |> glimpse()
} else {
  # ncaRegions    <- ncaRegPath |> read.csv()
  ncaRegions  <- "co_states" |>
    get_frediDataObj(listSub="frediData") |>
    select(region, state) |>
    mutate(stateLC = state |> tolower()); ncaRegions |> glimpse()
  ncaRegions |> write.csv(file=saveDir |> file.path(ncaRegFile), row.names=F)
} ### End if doLoad






### Aggregate results ------------------------------------------
#### Model average, impact year, national ------------------------------------------
### Aggregate results to model average, impact year, national
# loadLocalCode()
### Load or run and save
if(doLoad) {
  listAgg1     <- listNames |> map(function(name_i, lbl_i=listLabels[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggModelImpYrNat", rda0)
    obj_i  <- path_i  |> funLoadData(name0="obj_i")
    return(obj_i)
  }) |> set_names(listNames)
  listAgg1 |> glimpse()
} else {
  listAgg1     <- listResults0 |>
    map(aggregate_impacts, aggLevels=c("modelaverage", "impactyear", "national")) |>
    setNames(listNames)
  listAgg1 |> glimpse()
  listNames |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=listAgg1[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggModelImpYrNat", rda0)
    obj_i |> save(file=path_i)
  }) ### End walk
} ### End if doLoad

### Check values
listAgg1 |> map(function(df0){df0 |> filter(year %in% 2090) |> summarize_at(c("annual_impacts"), sum, na.rm=T)})


#### Impact type ------------------------------------------
### Aggregate results to impact type
# loadLocalCode()
### Load or run and save
if(doLoad) {
  listAgg2     <- listNames |> map(function(name_i, lbl_i=listLabels[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggAll", rda0)
    obj_i  <- path_i  |> funLoadData(name0="obj_i")
    return(obj_i)
  }) |> set_names(listNames)
  listAgg2 |> glimpse()
} else {
  listAgg2     <- listAgg1 |> map(aggregate_impacts, aggLevels=c("impacttype")) |> setNames(listNames)
  listAgg2 |> glimpse()
  listNames |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=listAgg2[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggAll", rda0)
    obj_i |> save(file=path_i)
  }) ### End walk
} ### End if doLoad

### Check values
listAgg2 |> map(function(df0){df0 |> filter(year %in% 2090) |> summarize_at(c("annual_impacts"), sum, na.rm=T)})


### Adjust ATS Temperature Mortality ------------------------------------------
### Get a list of ATS results, adjusted for Suicide impacts
loadLocalCode()
### Load or run and save
if(doLoad) {
  listATS      <- listNames |> map(function(name_i, lbl_i=listLabels[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_adjustedATS_list", rda0)
    obj_i  <- path_i  |> funLoadData(name0="obj_i")
    return(obj_i)
  }) |> set_names(listNames)
  listATS |> glimpse()
} else {
  # listATS       <- listNames |> getAdjustedATSlist()
  listATS      <- listNames |> map(function(
    name0,
    ### List of data frames to adjust (must contain at least ATS and Suicides)
    list0   = list(none=listResults0[[name0]], agg=listAgg1[[name0]], all=listAgg2[[name0]]),
    sector1 = "ATS Temperature-Related Mortality",
    sector2 = "Suicide",
    join0   = c("state", "model", "year"),
    ### Single vector or list of columns with same number of named elements as list0
    # cols0   = c("physical_impacts", "annual_impacts")
    cols0   = list0 |> names() |>
      map(function(name_i, str_i="physical", cols_i=c("physical_impacts", "annual_impacts")){
        cols_i[!(cols_i |> str_detect(str_i))]
      }) |> set_names(list0 |> names())
  ){
    ### Names
    names0   <- list0 |> names()
    ### Filter to specific sectors
    sectors0 <- sector1 |> c(sector2)
    list0    <- list0   |> map(function(data0){data0 |> filter(sector %in% sectors0)}) |> set_names(names0)
    rm(sectors0)
    ### Adjust data for sector2
    list0    <- list(data0=list0, cols0=cols0) |> pmap(function(data0, cols0){
      data0 |> adjustSector1_bySector2(sector1=sector1, sector2=sector2, cols0=cols0)
    }) |> set_names(names0)
    ### Drop sector2 and just keep sector 1
    list0    <- list0   |> map(function(data0){data0 |> filter(sector %in% sector1)}) |> set_names(names0)
    ### Return
    return(list0)
  }) |> set_names(listNames)
  listATS |> glimpse()
  ### Save data
  listNames |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=listATS[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_adjustedATS_list", rda0)
    obj_i |> save(file=path_i)
  }) ### End walk
} ### End if doLoad




### Scaled & Per Capita Impacts ---------------------------------------
#### Aggregated to: Model average, impact year, national ------------------------------------------
#### - Filter out some values
#### - Summarize across impact type and physical measure
#### - 1. Calculate initial total damages (group by year, etc)
#### - 2. Calculate scalar using total damage and GDP.  (if needed)
# saveDir |> file.path("baseResults_adjSuicide") |> paste0(".", "rda") |> load()
loadLocalCode()
### Load or run and save
if(doLoad) {
  listOthAdj <- listNames |> map(function(name_i, lbl_i=listLabels[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggOthAdj", rda0)
    obj_i  <- path_i  |> funLoadData(name0="obj_i")
    return(obj_i)
  }) |> set_names(listNames)
  listAggAdj |> glimpse()
} else {
  # loadLocalCode()
  listOthAdj <- listNames |> map(function(
    name0,
    df1   = listAgg1[[name0]],
    df2   = listATS [[name0]][["agg"]],
    cols0 = c("physical_impacts", "annual_impacts")
  ){
    ### Replace ATS data
    df0 <- df1 |> replaceATSdata(df2)
    rm(df1, df2)
    ### Format data
    # df0 |> glimpse()
    df0 <- df0 |> formatPlotData(cols0=cols0)
    ### Return
    return(df0)
  }) |> set_names(listNames)
  listOthAdj |> glimpse()
  ### Save data
  listNames |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=listOthAdj[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggOthAdj", rda0)
    obj_i |> save(file=path_i)
  }) ### End walk
} ### End if doLoad
#### Check results
### Check values
listOthAdj |> map(function(df0){
  listOthAdj |> getFilterDf(filters0=c("region", "model", "year", "includeaggregate", "sectorprimary", "year"), years0=2090) |>
    mutate(value=annual_impacts / 1e12) |> pull(value) |> sum(na.rm=T)
})
listATS [["base"]][["agg"]]$sector |> unique()

#### Model average, impact year, national ------------------------------------------
#### - Filter out some values
#### - Summarize across impact type and physical measure
#### - 1. Calculate initial total damages (group by year, etc)
#### - 2. Calculate scalar using total damage and GDP.  (if needed)
# saveDir |> file.path("baseResults_adjSuicide") |> paste0(".", "rda") |> load()
loadLocalCode()
### Load or run and save
if(doLoad) {
  listAggAdj <- listNames |> map(function(name_i, lbl_i=listLabels[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggAllAdj", rda0)
    obj_i  <- path_i  |> funLoadData(name0="obj_i")
    return(obj_i)
  }) |> set_names(listNames)
  listAggAdj |> glimpse()
} else {
  # loadLocalCode()
  listAggAdj <- listNames |> map(function(
    name0,
    df1   = listAgg2[[name0]],
    df2   = listATS [[name0]][["all"]],
    cols0 = c("annual_impacts")
  ){
    ### Replace ATS data
    df0 <- df1 |> replaceATSdata(df2)
    rm(df1, df2)
    ### Format data
    # df0 |> glimpse()
    df0 <- df0 |> formatPlotData(cols0=cols0)
    ### Return
    return(df0)
  }) |> set_names(listNames)
  listAggAdj |> glimpse()
  ### Save data
  listNames |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=listAggAdj[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggAllAdj", rda0)
    obj_i |> save(file=path_i)
  }) ### End walk
} ### End if doLoad
#### Check results
### Check values
listAggAdj |> map(function(df0){
  df0 |> getFilterDf(filters0=c("region", "model", "year", "includeaggregate", "sectorprimary", "year"), years0=2090) |>
    mutate(value=annual_impacts / 1e12) |> pull(value) |> sum(na.rm=T)
})


### Scale to Region ---------------------------------------
#### Model average, impact year, national ------------------------------------------
### Load or run and save
loadLocalCode()
if(doLoad) {
  listOthAdjReg <- listNames |> map(function(name_i, lbl_i=listLabels[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggOthAdjReg", rda0)
    obj_i  <- path_i  |> funLoadData(name0="obj_i")
    return(obj_i)
  }) |> set_names(listNames)
  listAggAdjReg |> glimpse()
} else {
  loadLocalCode()
  listOthAdjReg <- listOthAdj |> map(function(
    df0,
    df1    = regPop,
    yrs0   = eraYears0,
    cols0  = c("annual_impacts", "physical_impacts"),
    group0 = c("sector", "variant", "impactType", "impactYear", "model", "model_type", "region", "sectorprimary", "includeaggregate", "year", "gdp_usd")
  ){
    ### Drop national values
    df0     <- df0 |> getFilterDf(filters0="model")
    # df0     <- df0 |> getFilterDf(filters0="region", reverse0=T)
    df0     <- df0 |> getFilterDf(filters0="year", years0=yrs0)
    ### Summarize by region
    df0     <- df0 |> sumByRegion(group0=group0, cols0=cols0)
    ### Join with population data
    join0   <- c("region", "year")
    df0     <- df0 |> left_join(df1, by=join0)
    df0     <- df0 |> relocate(pop, .after=year)
    rm(join0)
    ### Get scaled impacts
    df0     <- df0 |> formatPlotData(cols0=cols0)
    rm(group0, cols0)
    ### Return
    return(df0)
  }) |> set_names(listNames)
  listOthAdjReg |> glimpse()
  ### Save data
  listNames |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=listOthAdjReg[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggOthAdjReg", rda0)
    obj_i |> save(file=path_i)
  }) ### End walk
} ### End if doLoad
#### Check results national
listOthAdjReg[[1]] |>
  getFilterDf(filters0=c("model", "year", "includeaggregate", "sectorprimary", "year"), years0=2090) |>
  getFilterDf("region") |>
  mutate(value=annual_impacts / 1e12) |> pull(value) |> sum(na.rm=T) |> print()
#### Check results region
listOthAdjReg[[1]] |>
  getFilterDf(filters0=c("model", "year", "includeaggregate", "sectorprimary", "year"), years0=2090) |>
  getFilterDf("region", reverse=T) |>
  mutate(value=annual_impacts / 1e12) |> pull(value) |> sum(na.rm=T) |> print()


#### Impact type ------------------------------------------
### Load or run and save
loadLocalCode()
if(doLoad) {
  listOthAdjReg <- listNames |> map(function(name_i, lbl_i=listLabels[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggOthAdjReg", rda0)
    obj_i  <- path_i  |> funLoadData(name0="obj_i")
    return(obj_i)
  }) |> set_names(listNames)
  listOthAdjReg |> glimpse()
} else {
  # loadLocalCode()
  listOthAdjReg <- listOthAdj |> map(function(
    df0,
    df1    = regPop,
    yrs0   = eraYears0,
    cols0  = c("annual_impacts"),
    group0 = c("sector", "variant", "impactType", "impactYear", "model", "model_type", "region", "sectorprimary", "includeaggregate", "year", "gdp_usd")
  ){
    ### Drop national values
    df0     <- df0 |> getFilterDf(filters0="model")
    # df0     <- df0 |> getFilterDf(filters0="region", reverse0=T)
    df0     <- df0 |> getFilterDf(filters0="year", years0=yrs0)
    ### Summarize by region
    df0     <- df0 |> sumByRegion(group0=group0, cols0=cols0)
    ### Join with population data
    join0   <- c("region", "year")
    df0     <- df0 |> left_join(df1, by=join0)
    df0     <- df0 |> relocate(pop, .after=year)
    rm(join0)
    ### Get scaled impacts
    df0     <- df0 |> formatPlotData(cols0=cols0)
    rm(group0, cols0)
    ### Return
    return(df0)
  }) |> set_names(listNames)
  listOthAdjReg |> glimpse()
  ### Save data
  listNames |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=listOthAdjReg[[name_i]]){
    path_i <- saveDir |> file.path(lbl_i) |> paste0("_results_aggOthAdjReg", rda0)
    obj_i |> save(file=path_i)
  }) ### End walk
} ### End if doLoad
#### Check results national
listOthAdjReg[[1]] |>
  getFilterDf(filters0=c("model", "year", "includeaggregate", "sectorprimary", "year"), years0=2090) |>
  getFilterDf("region") |>
  mutate(value=annual_impacts / 1e12) |> pull(value) |> sum(na.rm=T) |> print()
#### Check results region
listOthAdjReg[[1]] |>
  getFilterDf(filters0=c("model", "year", "includeaggregate", "sectorprimary", "year"), years0=2090) |>
  getFilterDf("region", reverse=T) |>
  mutate(value=annual_impacts / 1e12) |> pull(value) |> sum(na.rm=T) |> print()


### HARTIN ET AL. FIGURES -------------------------
### Base Impacts -------------------------
#### Figure 3.01 (Figure 2). Annual Impacts Stacked Bar ------------
#### Annual Impacts in Select Years for Baseline
#### Pre-process and organize
#### - Filter relevant data for three years of interest and assign category to each sector
#### - Summarize annual impacts
#### - Filter to to national
loadLocalCode()
natStackedBarDf <- listAggAdj[[base0]] |>
  getFilterDf(filters0=c("model", "region", "sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0="year", years0=eraYears0) |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-12) |>
  format_stackedBar_bySector(
    cols0   = c("annual_impacts"),
    join0   = c("sector"),
    group0  = c("category"),
    health0 = c("health", "atsHealth"),
    other0  = FALSE
  ); natStackedBarDf |> glimpse()
natStackedBarDf |> pull(category) |> unique()
natStackedBarDf$category |> unique()
# dfSectTypes |> filter(!lab0 %in% c("na", "cilHealth", "other")) |> select(lab2, color0) |> unique()

#### Check data
natStackedBarDf |> group_by_at(c("sector")) |>
  summarize_at(c("annual_impacts"), sum, na.rm=T) |> ungroup() |>
  mutate(across(c("annual_impacts"), ~ signif(.,2))) |> head()


##### Create Plots
#### Get total damages by year
loadLocalCode()
natStackedBarPlots <- natStackedBarDf |> plot_stackedBar_bySector(
  # plot_stackedBar_bySector,
  yCol0     = "annual_impacts",
  title0    = "Annual U.S. Climate-Driven Damages",
  subTitle0 = "Mean Damages by Year and Category (Trillions $)\nSubset of Climate-Related Impacts",
  lgdTitle0 = "Sector Categories \n(Number of sectors in category)",
  xLab0  = "Year",
  yLab0  = "Damages (Trillions, $2015)",
  theme1 = theme_stackedBar(),
  unit0  = "Trillions"
) # |> set_names(base0) ### End map
natStackedBarPlots
##### Save Figures
base0 |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=natStackedBarPlots[[name_i]]){
  path_i <- saveDir |> file.path("fig301_") |> paste0(lbl_i, tiff0)
  ggsave(plot=obj_i, file=path_i, width=5, height=4, bg="white")
}) ### End walk
rm(natStackedBarDf, natStackedBarPlots)


#### Figure 3.02 (Figure 4). Annual Impacts Box-Whisker -----------------------
#### US Annual Climate-Driven Damages in 2090 by Impact Category
##### Pre-process and organize
# loadLocalCode()
natBoxWhiskerDf <- listAggAdj[[base0]] |>
  getFilterDf(filters0=c("model", "region", "sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0="year", years0=2090) |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9) |>
  # getFilterDf(filters0="region") |>
  format_boxWhisker(
    cols0   = c("annual_impacts"),
    join0   = c("sector"),
    health0 = c("health", "atsHealth"),
    other0  = FALSE
  ); natBoxWhiskerDf |> glimpse()


##### Sectors
# loadLocalCode()
natBoxWhiskerPlots <- natBoxWhiskerDf |> plot_basicBoxWhisker2(
  sectorList = list(
    sector1=c("ATS Temperature-Related Mortality"),
    sector2=c("Climate-Driven Changes in Air Quality", "Labor", "Rail", "Roads", "Suicide",
              "Transportation Impacts from High Tide Flooding", "Wildfire", "Wind Damage")
  ), ### End list
  # lims0     = list(sector1=c(0, 4000), sector2=c(NA, 600), other=c(NA, 40)),
  title0    = "U.S. Annual Climate-Driven Damages in 2090",
  subTitle0 = "By sector, colored by sector category (subset of all climate-related impacts)",
  # subTitle0 = "By sector, colored by sector category",
  lgdTitle0 = "Sector Category",
  yLab0     = "Damages (Billions, $2015)"
  # yLab0     = "Damages (Billions, $2015)",
  # fill0     = list(a=rainbow_6() |> rev(), b=rainbow_6() |> rev(), c=rainbow_6()[-4] |> rev())
  # theme0    = theme_basicBoxWhisker()
) ### End plot_basicBoxWhisker2
natBoxWhiskerPlots
##### Save Plots
natBoxWhiskerPlots |> (function(list0){
  w0     <- list(7.5, 6.3, 6.8) |> set_names(letters[1:3])
  h0     <- list(1.6, 3.0, 4.7) |> set_names(letters[1:3])
  names0 <- list0 |> names()
  names0 |> walk(function(
    name_i, obj_i=list0[[name_i]], w_i=w0[[name_i]], h_i=h0[[name_i]]
  ){
    path_i <- saveDir |> file.path("fig302(2)") |> paste0(name_i, "_", base0, tiff0)
    ggsave(plot=obj_i, file=path_i, width=w_i, height=h_i, bg="white")
  })
})() ### End walk
rm(natBoxWhiskerDf, natBoxWhiskerPlots)


#### Figure 3.03 (Figure 5). Maps of Region Impacts in 2090 ----------------------------------------------
#### Plot average of region impacts/region population for each trial (e.g. per capita)
### Group by year, summarize over per capita values
### Supposedly the average values, but I'm only seeing sums
### Refer to function creating map plots...may need to edit to color by region
mapSectorImpactsBaseDf <- listAggAdj[[base0]] |>
  getFilterDf(filters0=c("model", "sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0=c("region"), reverse0=TRUE) |>
  getFilterDf(filters0="year", years0=2090) |> (function(
    df0,
    group0 = c("sector", "region", "state", "postal", "year", "gdp_usd", "national_pop", "gdp_percap", "pop"),
    sum0   = c("annual_impacts")
  ){
    ### Group and summarize
    df0 <- df0 |> group_by_at(c(group0)) |> summarize_at(sum0, sum, na.rm=T) |> ungroup()
    ### Calculate per capita results
    df0 <- df0 |> formatPlotData(cols0=sum0)
    ### Add data to map
    df0 <- df0 |> addData2Map(join0="state_lc")
    ### Return
    return(df0)
  })(); mapSectorImpactsBaseDf |> glimpse()

### Create map
mapSectorImpactsBasePlots <- stateMapData |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9) |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9 * 1e5) |>
  getSectorMaps(); mapSectorImpactsBasePlots

### Save map
mapSectorImpactsBasePlots |> names() |> walk(function(name_i, obj_i=mapSectorImpactsBasePlots[[name_i]]){
  path_i <- saveDir() |> paste0("fig304map_", name_i, tiff0)
  ggsave(plot=obj_i, filename=path_i, width=8, height=10)
})


#### Figure fa303-2: Donuts by Region ---------------------------------------------
#### Summarize results by region
# p_regionMap |> ggsave(filename="fig303_map.tiff", width=9, height=5)
# naStr0 = c(NA, NaN, Inf, -Inf, "NA", "NaN")  regPop
# mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9)
# loadLocalCode()
# testRegions <- tibble(region=ncaRegions |> pull(region) |> unique() |> c("National Total"))
# testRegions |> getFilterDf(filters0="region", reverse0=T)

#### Make regional donut charts, by category (absolute); already in 2020 dollars
# dfSectTypes |> glimpse()
loadLocalCode()
### Add sector types
# dfSectTypes <- dfSectLbls |>
#   addSectorTypes(types0= fun_sectorTypes()) |>
#   fun_colorSectors(); dfSectTypes |> glimpse()
loadLocalCode()
regPieMarginDf <- listAggAdjReg[["base"]] |>
  getFilterDf(filters0=c("sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0="region", reverse0=T) |>
  getFilterDf(filters0="year", years0=2090) |>
  # filter(region %in% "Midwest") |> # glimpse()
  # filter(region %in% "Northeast") |>
  format_regPieChart(
    col0    = "annual_impacts_percap",
    group0  = c("region", "sector", "color1", "darkop"),
    newColor = "#D3D3D3",
    df1      = dfSectTypes
  ); regPieMarginDf |> glimpse()

# c("ATS Temperature-Related Mortality", "Climate-Driven Changes in Air Quality", "Labor",
#   "CIL Agriculture", "Rail", "Roads", "Southwest Dust", "Wildfire", "Wind Damage",
#   "Transportation Impacts from High Tide Flooding") |> get_matches(y=regPieMarginDf$sector |> unique(), matches=F)
### Make plot list
loadLocalCode()
regPieMarginDfPlots <- regPieMarginDf |> (function(df0){
  regions0 <- df0      |> pull(region) |> unique()
  plots0   <- regions0 |> map(function(reg_i){
    reg_i |> plot_regPieMargin(df0=df0)
  }) |> set_names(regions0)
})(); ggarrange(plotlist=regPieMarginDfPlots, nrow=4, ncol=2)
# regPieMarginDfPlots

### Save Plots
regPieMarginDfPlots |> names() |> walk(function(
    name_i,
    obj_i = regPieMarginDfPlots[[name_i]]
){
  path_i <- saveDir |> file.path("fa303-2_") |> paste0(base0, "_", name_i, tiff0)
  ggsave(file=path_i, plot=obj_i, width=5, height=5, bg="white")
}) ### End walk
rm(regPieMarginDf, regPieMarginDfPlots)




#### Figure 3.04: State Maps ---------------------
##### a-c. Physical Impacts ---------------------
### 3.04a: "ATS Temperature-Related Mortality"
### 3.04b: "Climate-Driven Changes in Air Quality"
### 3.04c: "Transportation Impacts from High Tide Flooding"
### Colors for HTF: mid="#D7EFF0", high="#568278", na.value="white"

### Get info
infoMapPhysBase <- tibble(sector = c(
  "ATS Temperature-Related Mortality", "Climate-Driven Changes in Air Quality", "Transportation Impacts from High Tide Flooding")) |>
  mutate(list_abbr = c("ats", "aq", "htf")) |>
  mutate(total_title = c("Temperature", "Air Quality") |>
           paste0("-Related Premature Mortality") |>
           c("Transportation Impacts from High-Tide Flooding") |>
           map(function(lbl0, x0="Annual", y0="in 2090 by State"){paste(x0, lbl0, y0)}) |>
           unlist()) |>
  mutate(percap_title = "Per 100,000 people") |>
  pivot_longer(
    cols      =c("total_title", "percap_title"),
    names_to  = "plot_type",
    values_to = "plot_title"
  ) |>
  mutate(plot_type = plot_type |> str_replace("_title", "")) |>
  mutate(cols0     = "physical_impacts" |> paste0(case_when(plot_type %in% "percap" ~ "_percap", .default=""))) |>
  mutate(k0 = case_when(
    plot_type %in% "percap" ~ case_when(list_abbr %in% c("htf") ~ 1e5 * 1e-9, .default=1e5),
    .default = case_when(list_abbr %in% c("htf") ~ 1e-9, .default=1e-3)
  )) |>
  mutate(lgdTitle  = case_when(
    plot_type %in% "percap" ~ case_when(list_abbr %in% c("htf") ~ "Billions USD", .default="Deaths"),
    .default = case_when(list_abbr %in% c("htf") ~ "Billions USD", .default="Total Deaths (Thousands)")
  )) |>
  mutate(lgdTitle = lgdTitle |> paste0(case_when(
    plot_type %in% "percap" ~ " per\n100,000\nindividuals",
    .default = ""
  ))) |> mutate(symb0 = case_when(
    list_abbr %in% c("htf") ~ "$",
    .default = ""
  )); infoMapPhysBase |> glimpse()

### Get limits
physValRangeBase <- dfDeltaOthAdj |> (function(df0, year0=2090, cols0=infoMapPhysBase$cols0 |> unique(), sect0=infoMapPhysBase$sector |> unique()){
  ### Filter data
  df0    <- df0 |> getFilterDf(filters0=c("sectorprimary", "includeaggregate", "model"))
  df0    <- df0 |> getFilterDf(filters0=c("region"), reverse0=TRUE)
  df0    <- df0 |> getFilterDf(filters0=c("sector"), sectors0=sect0)
  df0    <- df0 |> getFilterDf(filters0=c("year"  ), years0  =year0)
  df0    <- df0 |> group_by_at(c("sector")) |> summarize_at(c(cols0), range, na.rm=T) |> ungroup()
  ### Return
  return(df0)
})(); physValRangeBase
physValRangeBase$physical_impacts_percap*1e5

### Create maps
loadLocalCode()
plotsMapBasePhys <- infoMapPhysBase |> pull(list_abbr) |> unique() |> map(function(
    name0,
    df0   = listOthAdj[["base"]],
    df1   = infoMapPhysBase,
    year0 = 2090,
    join0 = "state_lc"
){
  ### Filter to labels
  df1    <- df1 |> filter(list_abbr %in% name0)
  # df1 |> print()
  ### Get values
  sect0  <- df1 |> pull(sector    ) |> unique()
  type0  <- df1 |> pull(plot_type ) |> (function(x0){x0 |> as.list() |> set_names(x0)})()
  cols0  <- df1 |> pull(cols0     ) |> as.list() |> set_names(type0)
  k0     <- df1 |> pull(k0        ) |> as.list() |> set_names(type0)
  symb0  <- df1 |> pull(symb0     ) |> as.list() |> set_names(type0)
  title0 <- df1 |> pull(plot_title) |> as.list() |> set_names(type0)
  lgd0   <- df1 |> pull(lgdTitle  ) |> as.list() |> set_names(type0)
  sub0   <- "" |> rep(2) |> as.list() |> set_names(type0)
  # title0 |> print(); lgd0 |> print(); k0 |> print()
  ### Filter data
  df0    <- df0 |> getFilterDf(filters0=c("sectorprimary", "includeaggregate", "model"))
  df0    <- df0 |> getFilterDf(filters0=c("region"), reverse0=TRUE)
  df0    <- df0 |> getFilterDf(filters0=c("sector"), sectors0=sect0)
  df0    <- df0 |> getFilterDf(filters0=c("year"  ), years0  =year0)

  ### Mutate data
  df0    <- df0 |> mutate_multiplyConstant(cols=cols0[[1]], k0=k0[[1]])
  df0    <- df0 |> mutate_multiplyConstant(cols=cols0[[2]], k0=k0[[2]])

  ### Add map data
  df0    <- df0 |> addData2Map(join0=join0)

  ### create plot
  p0     <- df0 |> map2StateMap(
    cols0     = cols0,
    names0    = type0,
    lgdLabs0  = lgd0,
    ggTitle0  = title0,
    subTitle0 = sub0,
    lims0     = NULL,
    symb0     = symb0,
    round0    = 1,
    n.breaks0 = 8
  ) ### End map2StateMap

  # ### Return maps
  return(p0)
}) |> set_names(infoMapPhysBase |> pull(list_abbr) |> unique()); plotsMapBasePhys

# plotsMapBasePhys
plotsMapBasePhys |> names() |> walk(function(name_i, obj_i=plotsMapBasePhys[[name_i]]){
  path_i <- saveDir() |> paste0("fig304_", name_i, tiff0)
  ggsave(plot=obj_i, filename=path_i, width=8, height=10)
})




##### d. Total Annual Impacts ---------------------
### See function for creating maps
# ggsave(plot = plot_mortality_total, filename="fig304total.tiff", width=8, height=10)
mapTotalImpactsBaseDf <- listAggAdj[[base0]] |>
  getFilterDf(filters0=c("model", "sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0=c("region"), reverse0=TRUE) |>
  getFilterDf(filters0="year", years0=2090) |> (function(
    df0,
    group0 = c("region", "state", "postal", "year", "gdp_usd", "national_pop", "gdp_percap", "pop"),
    sum0   = c("annual_impacts")
  ){
    ### Group and summarize
    df0 <- df0 |> group_by_at(c(group0)) |> summarize_at(sum0, sum, na.rm=T) |> ungroup()
    ### Calculate per capita results
    df0 <- df0 |> formatPlotData(cols0=sum0)
    ### Add data to map
    df0 <- df0 |> addData2Map(join0="state_lc")
    ### Return
    return(df0)
  })(); mapTotalImpactsBaseDf |> glimpse()

### Create map
mapTotalImpactsBasePlot <- stateMapData |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9) |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9 * 1e5) |>
  map2StateMap(); mapTotalImpactsBasePlot

### Save map
mapTotalImpactsBasePlot |> walk(function(obj_i){
  path_i <- saveDir() |> paste0("fig304_all", tiff0)
  ggsave(plot=obj_i, filename=path_i, width=8, height=10)
})





### Delta Impacts -----------------------------------------------------------
#### Join Impacts -----------------------------------------------
##### Model average, impact year, national ------------------------------------------
### Primary variant, model average, national totals
### Shouldn't need to group by sector, year and sum
### Group and sum to categories (guessing also need to group by era year)
### Drop other categories
### Adjust to millions
loadLocalCode()
dfDeltaOthAdj <- listOthAdj |>
  map(getFilterDf, filters0=c("sectorprimary", "model")) |>
  map(getFilterDf, filters0=c("years"), years0=eraYears0) |>
  joinDeltaResults(
    diff0  = c("driverValue", "physical_impacts", "physical_impacts_percap"),
    join0  = c("sector", "variant", "impactType", "state", "model", "year")
  ); dfDeltaOthAdj |> glimpse()
dfDeltaOthAdj |> glimpse()

### Save data
base0 |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=dfDeltaOthAdj){
  path_i <- saveDir |> file.path("deltas_") |> paste0("_aggOth", rda0)
  obj_i |> save(file=path_i)
}) ### End walk
dfDeltaOthAdj |> summarize_at(c("physical_impacts_delta"), sum, na.rm=T)

### Region values
dfDeltaOthAdjReg <- dfDeltaOthAdj |> (function(
    df0,
    df1    = regPop,
    yrs0   = eraYears0,
    cols0  = c("annual_impacts", "physical_impacts"),
    group0 = c("sector", "variant", "impactType", "impactYear", "model", "model_type", "region", "sectorprimary", "includeaggregate", "year", "gdp_usd")
){
  ### Drop national values
  df0     <- df0 |> getFilterDf(filters0="model")
  # df0     <- df0 |> getFilterDf(filters0="region", reverse0=T)
  df0     <- df0 |> getFilterDf(filters0="year", years0=yrs0)
  ### Summarize by region
  df0     <- df0 |> sumByRegion(group0=group0, cols0=cols0)
  ### Join with population data
  join0   <- c("region", "year")
  df0     <- df0 |> left_join(df1, by=join0)
  df0     <- df0 |> relocate(pop, .after=year)
  rm(join0)
  ### Get scaled impacts
  df0     <- df0 |> formatPlotData(cols0=cols0)
  rm(group0, cols0)
  ### Return
  return(df0)
})(); dfDeltaOthAdjReg |> glimpse()



##### Impact Type -----------------------------------------------
### Primary variant, model average, national totals
### Shouldn't need to group by sector, year and sum
### Group and sum to categories (guessing also need to group by era year)
### Drop other categories
### Adjust to millions
loadLocalCode()
dfDeltaAggAdj <- listAggAdj |>
  map(getFilterDf, filters0=c("sectorprimary", "model")) |>
  map(getFilterDf, filters0=c("years"), years0=eraYears0) |>
  joinDeltaResults(
    diff0  = c("driverValue", "annual_impacts"),
    join0  = c("sector", "variant", "state", "model", "year")
  ); dfDeltaAggAdj |> glimpse()
dfDeltaAggAdj |> glimpse()

### Save data
base0 |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=dfDeltaAggAdj){
  path_i <- saveDir |> file.path("deltas_") |> paste0("_aggAll", rda0)
  obj_i |> save(file=path_i)
}) ### End walk
dfDeltaAggAdj |> summarize_at(c("annual_impacts_delta"), sum, na.rm=T)

### Region values
dfDeltaOthAdjReg <- dfDeltaAggAdj |> (function(
    df0,
    df1    = regPop,
    yrs0   = eraYears0,
    cols0  = c("annual_impacts", "physical_impacts"),
    group0 = c("sector", "variant", "impactType", "impactYear", "model", "model_type", "region", "sectorprimary", "includeaggregate", "year", "gdp_usd")
){
  ### Drop national values
  df0     <- df0 |> getFilterDf(filters0="model")
  # df0     <- df0 |> getFilterDf(filters0="region", reverse0=T)
  df0     <- df0 |> getFilterDf(filters0="year", years0=yrs0)
  ### Summarize by region
  df0     <- df0 |> sumByRegion(group0=group0, cols0=cols0)
  ### Join with population data
  join0   <- c("region", "year")
  df0     <- df0 |> left_join(df1, by=join0)
  df0     <- df0 |> relocate(pop, .after=year)
  rm(join0)
  ### Get scaled impacts
  df0     <- df0 |> formatPlotData(cols0=cols0)
  rm(group0, cols0)
  ### Return
  return(df0)
})(); dfDeltaOthAdjReg |> glimpse()



#### Figure 3.06 (Figure 2 deltas) Stacked Bar-----------------------------
deltaStackedBarDf <- dfDeltaAggAdj |>
  getFilterDf(filters0=c("model", "region", "sectorprimary", "includeaggregate")) |>
  # getFilterDf(filters0="year", years0=eraYears0) |>
  mutate_multiplyConstant(cols=c("annual_impacts_delta"), k0=1e-9) |>
  format_stackedBar_bySector(
    cols0   = c("annual_impacts_delta"),
    join0   = c("sector"),
    group0  = c("category"),
    health0 = c("health", "atsHealth"),
    other0  = FALSE
  ); deltaStackedBarDf |> glimpse()
# deltaStackedBarDf |> pull(category) |> unique()
# dfSectTypes |> filter(!lab0 %in% c("na", "cilHealth", "other")) |> select(lab2, color0) |> unique()

#### Check data
deltaStackedBarDf |> group_by_at(c("sector")) |>
  summarize_at(c("annual_impacts_delta"), sum, na.rm=T) |> ungroup() |>
  mutate(across(c("annual_impacts_delta"), ~ signif(.,2))) |> head()


##### Create Plots
#### Get total damages by year
loadLocalCode()
deltaStackedBarPlots <- deltaStackedBarDf |> plot_stackedBar_bySector(
  # plot_stackedBar_bySector,
  yCol0     = "annual_impacts_delta",
  title0    = "Net Annual U.S. Climate-Related Benefits",
  subTitle0 = bquote("Net damages avoided from a 0.0001\u00B0C decrease in warming by 2090\nSubset of climate-related impacts"),
  lgdTitle0 = "Sector Categories \n(Number of sectors in category)",
  xLab0  = "Year",
  yLab0  = "Avoided Damages\n(Billions, $2015)",
  theme1 = theme_stackedBar(),
  unit0  = "Billions"
) # |> set_names(base0) ### End map
deltaStackedBarPlots
##### Save Figures
base0 |> walk(function(name_i, lbl_i=listLabels[[name_i]], obj_i=deltaStackedBarPlots[[name_i]]){
  path_i <- saveDir |> file.path("fig306_") |> paste0(lbl_i, tiff0)
  ggsave(plot=obj_i, file=path_i, width=5, height=4, bg="white")
}) ### End walk
rm(deltaStackedBarDf, deltaStackedBarPlots)

#### Check data
deltaStackedBarDf |> filter(year == 2090) |>
  summarize_at(c("annual_impacts_delta"), sum, na.rm=T) |>
  mutate(across(c("annual_impacts_delta"), ~ signif(.,2))) |> print()
deltaStackedBarDf |> filter(year == 2090) |>
  group_by_at(c("sector")) |>
  summarize_at(c("annual_impacts_delta"), sum, na.rm=T) |>
  mutate(across(c("annual_impacts_delta"), ~ signif(.,2))) |> print()


#### Figure 3.07 (Figure 4 deltas). Box-Whisker -------------------------------------------------
##### Pre-process and organize
#### US Annual Climate-Driven Damages in 2090 by Impact Category
##### Pre-process and organize
### National totals, model averages, includeaggregate, sectorprimary
# loadLocalCode()
deltaBoxWhiskerDf <- listAggAdj[[base0]] |>
  getFilterDf(filters0=c("model", "region", "sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0="year", years0=2090) |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-6) |>
  # getFilterDf(filters0="region") |>
  format_boxWhisker(
    cols0   = c("annual_impacts"),
    join0   = c("sector"),
    health0 = c("health", "atsHealth"),
    other0  = FALSE
  ); deltaBoxWhiskerDf |> glimpse()


##### Sectors
# loadLocalCode()
deltaBoxWhiskerPlots <- deltaBoxWhiskerDf |> plot_basicBoxWhisker2(
  sectorList = list(
    sector1=c("ATS Temperature-Related Mortality"),
    sector2=c("Climate-Driven Changes in Air Quality", "Labor", "Rail", "Roads", "Suicide",
              "Transportation Impacts from High Tide Flooding", "Wildfire", "Wind Damage")
  ), ### End list
  # lims0     = list(sector1=c(0, 4000), sector2=c(NA, 600), other=c(NA, 40)),
  title0    = "U.S. Annual Climate-Mitigation Benefits in 2090",
  subTitle0 = "By sector, colored by sector category (subset of all climate-related impacts)",
  # subTitle0 = "By sector, colored by sector category",
  lgdTitle0 = "Sector Category",
  yLab0     = "Avoided Damages (Billions, $2015)"
  # yLab0     = "Damages (Billions, $2015)",
  # fill0     = list(a=rainbow_6() |> rev(), b=rainbow_6() |> rev(), c=rainbow_6()[-4] |> rev())
  # theme0    = theme_basicBoxWhisker()
) ### End plot_basicBoxWhisker2
deltaBoxWhiskerPlots
##### Save Plots
deltaBoxWhiskerPlots |> (function(list0){
  w0     <- list(7.5, 6.3, 6.8) |> set_names(letters[1:3])
  h0     <- list(1.6, 3.0, 4.7) |> set_names(letters[1:3])
  names0 <- list0 |> names()
  names0 |> walk(function(
    name_i, obj_i=list0[[name_i]], w_i=w0[[name_i]], h_i=h0[[name_i]]
  ){
    path_i <- saveDir |> file.path("fig307") |> paste0(name_i, "_", base0, tiff0)
    ggsave(plot=obj_i, file=path_i, width=w_i, height=h_i, bg="white")
  })
})() ### End walk
rm(deltaBoxWhiskerDf, deltaBoxWhiskerPlots)



#### Figure 3.08 (figure 5 deltas)---------------------------------------------
### Plot sum of regional impacts/regional population but this time for deltas
### Impacts per cap delta
##### Plot map

#   labs(title= paste0("2090 Regional Baenefits Per Capita"),
#        subtitle=("0.0001\u00B0C decrease in warming by 2090"),
#        fill= "Benefits per Capita",
#        x="",
#        y="")
# ggsave(plot = p, filename="fig308_map.tiff", width=9, height=5)
# df_region_sum |> mutate(across(c("impacts_percap"), ~ signif(.,2))) |> print()
# df_sector_region_sum |> mutate(across(c("impacts_percap"), ~ signif(.,2))) |> print()

### Need to supply default titles as arguments
mapSectorImpactsDeltaDf <- listAggAdj[[Delta0]] |>
  getFilterDf(filters0=c("model", "sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0=c("region"), reverse0=TRUE) |>
  getFilterDf(filters0="year", years0=2090) |> (function(
    df0,
    group0 = c("sector", "region", "state", "postal", "year", "gdp_usd", "national_pop", "gdp_percap", "pop"),
    sum0   = c("annual_impacts")
  ){
    ### Group and summarize
    df0 <- df0 |> group_by_at(c(group0)) |> summarize_at(sum0, sum, na.rm=T) |> ungroup()
    ### Calculate per capita results
    df0 <- df0 |> formatPlotData(cols0=sum0)
    ### Add data to map
    df0 <- df0 |> addData2Map(join0="state_lc")
    ### Return
    return(df0)
  })(); mapSectorImpactsDeltaDf |> glimpse()

### Create map
mapSectorImpactsDeltaPlots <- stateMapData |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9) |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9 * 1e5) |>
  getSectorMaps(); mapSectorImpactsDeltaPlots

### Save map
mapSectorImpactsDeltaPlots |> names() |> walk(function(name_i, obj_i=mapSectorImpactsDeltaPlots[[name_i]]){
  path_i <- saveDir() |> paste0("fig308_", name_i, tiff0)
  ggsave(plot=obj_i, filename=path_i, width=8, height=10)
})


#### Figure fa308-2: Donuts by Region ---------------------------------------------
### make regional donut charts, by category (absolute)
### already in 2020 dollars
### abs(fill = "Top 4 Sectors",title = paste("All Regions"))
deltaPieMarginDf <- dfDeltaOthAdjReg |>
  getFilterDf(filters0=c("sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0="region", reverse0=T) |>
  getFilterDf(filters0="year", years0=2090) |>
  # filter(region %in% "Midwest") |> # glimpse()
  # filter(region %in% "Northeast") |>
  format_regPieChart(
    col0    = "annual_impacts_delta_percap",
    group0  = c("region", "sector", "color1", "darkop"),
    newColor = "#D3D3D3",
    df1      = dfSectTypes
  ); deltaPieMarginDf |> glimpse()

# c("ATS Temperature-Related Mortality", "Climate-Driven Changes in Air Quality", "Labor",
#   "CIL Agriculture", "Rail", "Roads", "Southwest Dust", "Wildfire", "Wind Damage",
#   "Transportation Impacts from High Tide Flooding") |> get_matches(y=deltaPieMarginDf$sector |> unique(), matches=F)
### Make plot list
# loadLocalCode()
deltaPieMarginDfPlots <- deltaPieMarginDf |> (function(df0){
  regions  <- df0     |> pull(region) |> unique()
  plots0   <- regions |> map(function(delta_i){
    delta_i |> plot_regPieMargin(df0=df0)
  }) |> set_names(regions)
})(); ggarrange(plotlist=deltaPieMarginDfPlots, nrow=4, ncol=2)
# deltaPieMarginDfPlots

### Save Plots
deltaPieMarginDfPlots |> names() |> walk(function(
    name_i,
    obj_i = deltaPieMarginDfPlots[[name_i]]
){
  path_i <- saveDir |> file.path("fa308-2_delta_") |> paste0(name_i, tiff0)
  ggsave(file=path_i, plot=obj_i, width=5, height=5, bg="white")
}) ### End walk
rm(deltaPieMarginDf, deltaPieMarginDfPlots)

#### Figure 3.09: State Maps ---------------------------------------------
##### a-c: Physical Impacts ---------------------------------------------
# aq_breaks <- results_state_sf_delta |>
#   st_drop_geometry() |>
#   filter(sector == c_state_sectors[2]) |>
#   group_by() |>
#   summarise(max = physical_impacts_delta |> max() |> round(2) + 1e-2, min=physical_impacts_delta |> min() |> round(2) - 1e-2)
# scale_fill_gradient2(name="Deaths Avoided",mid="white",high="#A5AB81",na.value="grey",
#                      limits = c(temp_breaks$min, temp_breaks$max),
#                      labels = c("Minimum", "Maximum"),
#                      breaks=c(temp_breaks$min, temp_breaks$max))

### Get info
infoMapPhysDelta <- tibble(sector = c(
  "ATS Temperature-Related Mortality", "Climate-Driven Changes in Air Quality", "Transportation Impacts from High Tide Flooding")) |>
  mutate(list_abbr = c("ats", "aq", "htf")) |>
  mutate(total_title = c("Temperature", "Air Quality") |>
           paste0("-Related Premature Mortality") |>
           c("Transportation Impacts from High-Tide Flooding") |>
           map(function(lbl0, x0="Avoided Annual", y0="in 2090 by State"){paste(x0, lbl0, y0)}) |>
           unlist()) |>
  mutate(percap_title = "Per 100,000 people") |>
  pivot_longer(
    cols      =c("total_title", "percap_title"),
    names_to  = "plot_type",
    values_to = "plot_title"
  ) |>
  mutate(plot_type = plot_type |> str_replace("_title", "")) |>
  mutate(cols0     = "physical_impacts" |> paste0(case_when(plot_type %in% "percap" ~ "_percap", .default="")) |> paste0("_delta")) |>
  mutate(k0 = case_when(
    plot_type %in% "percap" ~ case_when(list_abbr %in% c("htf") ~ 1e5 * 1e-6, .default=1e5),
    .default = case_when(list_abbr %in% c("htf") ~ 1e-9, .default=case_when(list_abbr %in% c("ats") ~ 1e-3, .default=1))
  )) |>
  mutate(lgdTitle  = case_when(
    plot_type %in% "percap" ~ case_when(list_abbr %in% c("htf") ~ "Millions USD", .default="Deaths Avoided"),
    .default = case_when(list_abbr %in% c("htf") ~ "Billions USD", .default="Total Deaths Avoided (Thousands)")
  )) |>
  mutate(lgdTitle = lgdTitle |> paste0(case_when(
    plot_type %in% "percap" ~ " per\n100,000\nindividuals",
    .default = ""
  ))) |> mutate(symb0 = case_when(
    list_abbr %in% c("htf") ~ "$",
    .default = ""
  )); infoMapPhysDelta |> glimpse()

### Get limits
physValRangeDelta <- dfDeltaOthAdj |> (function(df0, year0=2090, cols0=infoMapPhysDelta$cols0 |> unique(), sect0=infoMapPhysDelta$sector |> unique()){
  ### Filter data
  df0    <- df0 |> getFilterDf(filters0=c("sectorprimary", "includeaggregate", "model"))
  df0    <- df0 |> getFilterDf(filters0=c("region"), reverse0=TRUE)
  df0    <- df0 |> getFilterDf(filters0=c("sector"), sectors0=sect0)
  df0    <- df0 |> getFilterDf(filters0=c("year"  ), years0  =year0)
  df0    <- df0 |> group_by_at(c("sector")) |> summarize_at(c(cols0), range, na.rm=T) |> ungroup()
  ### Return
  return(df0)
})(); physValRangeDelta
physValRangeDelta$physical_impacts_percap_delta*1e5

### Create maps
loadLocalCode()
plotsMapPhysDelta <- infoMapPhysDelta |> pull(list_abbr) |> unique() |> map(function(
    name0,
    df0   = dfDeltaOthAdj,
    df1   = infoMapPhysBase,
    year0 = 2090,
    join0 = "state_lc"
){
  ### Filter to labels
  df1    <- df1 |> filter(list_abbr %in% name0)
  # df1 |> print()
  ### Get values
  sect0  <- df1 |> pull(sector    ) |> unique()
  type0  <- df1 |> pull(plot_type ) |> (function(x0){x0 |> as.list() |> set_names(x0)})()
  cols0  <- df1 |> pull(cols0     ) |> as.list() |> set_names(type0)
  k0     <- df1 |> pull(k0        ) |> as.list() |> set_names(type0)
  symb0  <- df1 |> pull(symb0     ) |> as.list() |> set_names(type0)
  title0 <- df1 |> pull(plot_title) |> as.list() |> set_names(type0)
  lgd0   <- df1 |> pull(lgdTitle  ) |> as.list() |> set_names(type0)
  sub0   <- "" |> rep(2) |> as.list() |> set_names(type0)
  # title0 |> print(); lgd0 |> print(); k0 |> print()
  ### Filter data
  df0    <- df0 |> getFilterDf(filters0=c("sectorprimary", "includeaggregate", "model"))
  df0    <- df0 |> getFilterDf(filters0=c("region"), reverse0=TRUE)
  df0    <- df0 |> getFilterDf(filters0=c("sector"), sectors0=sect0)
  df0    <- df0 |> getFilterDf(filters0=c("year"  ), years0  =year0)

  ### Mutate data
  df0    <- df0 |> mutate_multiplyConstant(cols=cols0[[1]], k0=k0[[1]])
  df0    <- df0 |> mutate_multiplyConstant(cols=cols0[[2]], k0=k0[[2]])

  ### Add map data
  df0    <- df0 |> addData2Map(join0=join0)

  ### create plot
  p0     <- df0 |> map2StateMap(
    cols0     = cols0,
    names0    = type0,
    lgdLabs0  = lgd0,
    ggTitle0  = title0,
    subTitle0 = sub0,
    colors0   = list(low="#DD8047", mid="white", high="#A5AB81", na.value="grey"),
    lims0     = NULL,
    symb0     = symb0,
    round0    = 1,
    n.breaks0 = 8
  ) ### End map2StateMap

  # ### Return maps
  return(p0)
}) |> set_names(infoMapPhysDelta |> pull(list_abbr) |> unique()); plotsMapPhysDelta

# plotsMapBasePhys
plotsMapPhysDelta |> names() |> walk(function(name_i, obj_i=plotsMapPhysDelta[[name_i]]){
  path_i <- saveDir |> paste0("fig309_", name_i, tiff0)
  ggsave(plot=obj_i, filename=path_i, width=8, height=10)
})




##### d: Total Annual Impacts ----------------------------------------
### See function for creating maps
# ggsave(plot = plot_mortality_total, filename="fig309combined.tiff", width=8, height=10)
mapTotalImpactsDeltaDf <- listAggAdj[[Delta0]] |>
  getFilterDf(filters0=c("model", "sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0=c("region"), reverse0=TRUE) |>
  getFilterDf(filters0="year", years0=2090) |> (function(
    df0,
    group0 = c("region", "state", "postal", "year", "gdp_usd", "national_pop", "gdp_percap", "pop"),
    sum0   = c("annual_impacts")
  ){
    ### Group and summarize
    df0 <- df0 |> group_by_at(c(group0)) |> summarize_at(sum0, sum, na.rm=T) |> ungroup()
    ### Calculate per capita results
    df0 <- df0 |> formatPlotData(cols0=sum0)
    ### Add data to map
    df0 <- df0 |> addData2Map(join0="state_lc")
    ### Return
    return(df0)
  })(); mapTotalImpactsDeltaDf |> glimpse()

### Create map
mapTotalImpactsDeltaPlot <- stateMapData |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9) |>
  mutate_multiplyConstant(cols=c("annual_impacts"), k0=1e-9 * 1e5) |>
  map2StateMap(); mapTotalImpactsDeltaPlot

### Save map
mapTotalImpactsDeltaPlot |> walk(function(obj_i){
  path_i <- saveDir() |> paste0("fig309_all", tiff0)
  ggsave(plot=obj_i, filename=path_i, width=8, height=10)
})




#### Figure 3.10: Benefits (Annual Impacts) Pie charts ---------------------------------------------------------------------
### Are these different? Arranged in a grid. Need to check.
### Plots of annual_impacts_deltas
benPieMarginDf <- dfDeltaOthAggReg |>
  getFilterDf(filters0=c("sectorprimary", "includeaggregate")) |>
  getFilterDf(filters0="region", reverse0=T) |>
  getFilterDf(filters0="year", years0=2090) |>
  # filter(region %in% "Midwest") |> # glimpse()
  # filter(region %in% "Northeast") |>
  format_regPieChart(
    col0    = "annual_impacts_delta",
    group0  = c("region", "sector", "color1", "darkop"),
    newColor = "#D3D3D3",
    df1      = dfSectTypes
  ); benPieMarginDf |> glimpse()

# c("ATS Temperature-Related Mortality", "Climate-Driven Changes in Air Quality", "Labor",
#   "CIL Agriculture", "Rail", "Roads", "Southwest Dust", "Wildfire", "Wind Damage",
#   "Transportation Impacts from High Tide Flooding") |> get_matches(y=benPieMarginDf$sector |> unique(), matches=F)
### Make plot list
# loadLocalCode()
benPieMarginDfPlots <- benPieMarginDf |> (function(df0){
  regions  <- df0     |> pull(region) |> unique()
  plots0   <- regions |> map(function(ben_i){
    ben_i |> plot_regPieMargin(df0=df0)
  }) |> set_names(regions)
})(); ggarrange(plotlist=benPieMarginDfPlots, nrow=4, ncol=2)
# benPieMarginDfPlots

### Save Plots
### Layout: 4 columns
ggarrange(plotlist=benPieMarginDfPlots, nrow=2, nrow=4) |> walk(function(obj_i){
  path_i <- saveDir |> file.path("Figure10_deltas") |> paste0(tiff0)
  ggsave(file=path_i, plot=obj_i, width=14, height=12, bg="white")
}) ### End walk
# rm(benPieMarginDf, benPieMarginDfPlots)




### SV Impacts -------------------------
#### SV sectors -------------------------------------
##### SV Group Types -------------------------------------
### Data frame to factor SV groups
# svGroupTypesDf <- tibble(svGroupType = "sv_" |> paste0(bipoc, lowIncome, noHS, plus65))
svGroupTypesDf <- 1 |> (function(x){
  ### First group
  df1 <- tibble(svGroupType = "sv_" |> paste0(c("bipoc", "lowIncome", "noHS", "plus65")))
  df1 <- tibble(svGroupLabel = c("BIPOC", "Low Income", "No High-School Diploma", "Over age 65"))
  ### Second group
  df2 <- tibble(svGroupType  = "race_" |> paste0(c(
    "amInd", "asian", "black", "hispanic", "multiRacial", "otherRace", "pacIsl", "white"
  ))) |> tibble(svGroupLabel = paste0(c(
    "American Indian or Alaska Native", "Asian", "Black or African American", "Hispanic or Latino",
    "Two or more races", "Other Race", "Pacific Islander", "White, non Hispanic"
  )))
  ### Bind
  df0 <- df1 |> bind_rows(df2)
  rm(df1, df2)
  ### Return
  return(df0)
})(); svGroupTypesDf |> glimpse()

##### All SV Sectors -------------------------------------
### Vector of SV sectors to run
svSectorsAll   <- c("Air Quality - Premature Mortality",  "Air Quality - Childhood Asthma", "Extreme Temperature",
                    "Labor", "Roads", "Coastal Properties", "Transportation Impacts from High Tide Flooding")
svSectorsDfAll <- svSectorsAll |> (function(vals0){
  ### Add to tibble
  df0   <- tibble(sector = vals0)
  df0   <- df0 |> mutate(
    svSectorDesc = "Climate-Driven Air Quality - " |>
      paste0(c("Age 65+ Mortality", "Childhood Asthma")) |> c(
        "Temperature-Related Mortality", "Lost Labor Hours", "Road Infrastructure Damage",
        "Coastal Flooding Property Damage", "Transportation Impacts fromHigh Tide Flooding"
      ))
  df0 <- df0 |> mutate(abbr0 = c("aq_mort", "aq_asthma", "extemp", "labor", "roads", "coastal", "htf"))
  df0 <- df0 |> mutate(var0  = c(NA, NA, NA, NA, "Without Adaptation", "Without Adaptation", NA))
  ### Return
  return(df0)
})(); svSectorsAll |> glimpse()


##### Select SV Sectors -------------------------------------
### Select SV sectors for plotting
svSectors   <- c("Air Quality - Premature Mortality", "Labor")
svSectorsDf <- svSectorsDfAll |> (function(df0, vals0=svSectors){
  ### Add to tibble
  df0 <- df0 |> filter(sector %in% vals0)
  df0 <- df0 |> mutate(svSectorLabel = c("Climate-Driven Air Quality", "Labor"))
  df0 <- df0 |> mutate(svPhysLabel   = c("Mortality", "Hours Lost Per Capita" ))
  df0 <- df0 |> mutate(svPhysYLab    = c("Mortality Rate per 100,000 Individuals", "Labor Hours Lost Per Capita"))
  df0 <- df0 |> mutate(sv_k0         = c(1e5, 1))
  ### Return
  return(df0)
})(); svSectorsDf |> glimpse()




#### Load scenarios -------------------------------------
# # svTempDf      <- svTempPath |> read.csv()
# # svPopDf       <- svPopPath  |> read.csv()
# svTempPath |> read.csv() |> glimpse()
# svPopPath  |> read.csv() |> glimpse()
# svInputsList  <- list(temp=svTempPath, pop=svPopPath) |> import_inputs(temptype="global", popArea="national")
svInputsList  <- list(temp=tempsList |> set_names(listNames) |> bind_rows(.id="scenario"))



#### Run FrEDI SV -------------------------------------
svOutputsList <- svSectorsAll |> map(function(sector0, inputs0=svInputsList){
  run_fredi_sv(sector=sector0, inputsList=inputs0)
}) |> set_names(svSectorsAll); svOutputsList |> glimpse()

#### Figure 3.05 (Figure 6): SV Physical Impacts (Select Sectors) -------------------------------------
svPhysRatesDf <- svSectors |> map(function(
    sector0,
    list0   = svOutputsList,
    groups0 = c("svGroupType"),
    risks0  = c("national_highRiskPop", "impPop") |> map(paste0, "_", c("sv", "ref")) |> unlist(),
    rates0  = c("aveRate_sv"),
    year0   = 2090,
    df1     = svGroupTypesDf,
    df2     = svSectorsDf
){
  ### Columns
  group0  <- groups0 |> c(df1 |> names()) |> c(df2 |> names()) |> unique()

  ### Filter observations
  ### Add SV group labels
  df0     <- list0[[sector0]]
  df0     <- df0 |> getFilterDf(filters0="year", years0=year0)
  df0     <- df0 |> left_join(df1, by=groups0)
  df0     <- df0 |> left_join(df2, by="sector")
  rm(list0, df1, df2)

  ### Separate df0 into two data frames
  df1     <- df0 |> filter(svGroupType |> str_detect("sv"))
  df2     <- df0 |> filter(svGroupType |> str_detect("race"))
  rm(df0)

  ### Calculate risks
  sort1   <- groups0 |> c("risk_per_pct")
  df1     <- df1 |> group_by_at(c(group0)) |> summarize_at(c(risks0), sum, na.rm=T) |> ungroup()
  df1     <- df1 |> replace(. |> is.na(), 0)
  df1     <- df1 |> mutate(ratio_natHighRiskPop_sv2ref = national_highRiskPop_sv / national_highRiskPop_ref)
  df1     <- df1 |> mutate(ratio_impPop_sv2ref         = impPop_sv / impPop_ref)
  df1     <- df1 |> mutate(risk_per_pct = (ratio_natHighRiskPop_sv2ref / ratio_impPop_sv2ref - 1) * 100)
  df1     <- df1 |> arrange_at(c(sort1))
  rm(sort1)

  ### Calculate rates
  sort2   <- groups0 |> c("rateper100k")
  df2     <- df2 |> group_by_at(c(group0)) |> summarize_at(c(rates0), sum, na.rm=T) |> ungroup()
  df2     <- df2 |> replace(. |> is.na(), 0)
  df2     <- df2 |> mutate(rate_per = aveRate_sv * sv_k0)
  df2     <- df2 |> arrange_at(c(sort2))
  rm(sort2)

  ### Add data to list
  list0   <- list()
  list0[["rates"]] <- df1
  list0[["risks"]] <- df2

  ### Return
  gc()
  return(list0)
}) |> set_names(svSectors); svPhysRatesDf |> glimpse()


svPhysRatesPlots <- svSectors |> map(function(
    sector0,
    list0   = svOutputs2090,
    title1  = ":\nDifference in Risk Relative to the \nNational Population in 2090",
    title2  = ":\nAdditional * in 2090",
    theme0  = theme_bw() + theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x  = element_text(size=10),
        axis.text.y  = element_text(size=10),
        axis.title.y = element_text(vjust=2.5)
      )
){
  ### Filter observations
  list0   <- list0[[sector0]]
  df1     <- list0[["rates"]]
  df2     <- list0[["risks"]]
  rm(list0)
  ### Create plot and add geom
  sector1 <- df1 |> pull(svSectorLabel) |> unique()
  gtitle1 <- sector1 |> paste0(title1)
  xCol1   <- "svGroupType"
  yCol1   <- "risk_per100"
  xLab1   <- "Socially-Vulnerable Groups"
  yLab1   <- "Percent"
  fill1   <- colors_7()[6]
  # fill1   <- colors_6()[1]
  ### Initialize plot
  p1      <- df1 |> ggplot(aes(x=reorder(.data[[xCol1]], .data[[yCol1]]), y=.data[[yCol1]]))
  ### Add geom_bar
  p1      <- p1 + geom_bar(stat="identity", fill=fill1, width=0.6)
  ### Add geom_text
  p1      <- p1 + geom_text(
    aes(label=risk_per |> round(0) |> paste0("%"), vjust=0.5, hjust=-1),
    position=position_dodge(1), color="grey20", size=4
  ) ### End geom_text
  ### Flip coordinates
  p1      <- p1 + coord_flip()
  ### Scales
  p1      <- p1 + scale_x_discrete  (xLab1)
  p1      <- p1 + scale_y_continuous(yLab1)

  ### Create plot and add geom
  sector2 <- df2 |> pull(svSectorLabel) |> unique()
  pMeas2  <- df2 |> pull(svPhysLabel  ) |> unique()
  gtitle2 <- sector2 |> paste0(title2) |> str_replace("\\*", pMeas2)
  xCol2   <- "svGroupType"
  yCol2   <- "rate_per"
  xLab2   <- "Race & Ethnicity"
  yLab2   <- df2 |> pull(svPhysYLab  ) |> unique()
  fill2   <- colors_7()[7]
  ### Initialize plot
  p2      <- df2 |> ggplot(aes(x=reorder(.data[[xCol2]], .data[[yCol2]]), y=.data[[yCol2]]))
  ### Add geom_bar
  p2      <- p2 + geom_bar(stat="identity", fill=fill2, width=0.6)
  ### Add geom_text
  p2      <- p2 + geom_text(
    aes(label=.data[[yCol2]] |> round(0) |> paste0("%"), vjust=0.5, hjust=-1),
    position=position_dodge(1), color="grey20", size=4
  ) ### End geom_text
  ### Flip coordinates
  p2      <- p2 + coord_flip()
  ### Scales
  p2      <- p2 + scale_x_discrete  (xLab2)
  p2      <- p2 + scale_y_continuous(yLab2)

  ### Plot list
  list0   <- list()
  list0[["risks"]] <- p1
  list0[["rates"]] <- p2
  rm(p1, p2)

  ### GG arrange
  p0      <- ggarrange(plotlist=list0, ncol=2)

  ### Return
  return(p0)
}) |> set_names(svSectors) |>
  (function(list0){
    ggarrange(plotlist=list0, nrow=2)
  })(); svPhysRatesPlots |> glimpse()


ggsave(plot=svPhysRatesPlots, filename=saveDir |> file.path("fig305_sv") |> paste0(base0, tiff0), width=12, height=8)



#### Other SV Figures -------------------------------------
##### Average Rates by Race -------------------------------------
#minority group rates
### calculate as 1) sum sv rates for all regions (for policy and baseline)
### 2) subtract policy from baseline national sum for each race
### 3) divide each race rate by white rate (normalize to 100)
### interpretation: under the rule, based on the spatial patterns of different racial groups and intersections with avoided climate damages, X group is projected to benefit X% as much as the reference population (e.g., white).(All benefit because all values are positive)
svAveRatesDf <- svSectorsAll |> map(function(
    sector0,
    list0   = svOutputsList,
    groups0 = c("svGroupType"),
    rates0  = c("aveRate_sv"),
    year0   = 2090,
    df1     = svGroupTypesDf,
    df2     = svSectorsDfAll
){
  ### Columns
  group0  <- groups0 |> c(df1 |> names()) |> c(df2 |> names()) |> c("year") |> unique()

  ### Filter observations
  ### Add SV group labels
  df0     <- list0[[sector0]]
  df0     <- df0 |> getFilterDf(filters0="year", years0=year0)
  df0     <- df0 |> left_join(df1, by=groups0)
  df0     <- df0 |> left_join(df2, by="sector")
  rm(list0, df1, df2)

  ### Filter to variant
  scens0  <- df0 |> pull(scenario) |> unique() |> sort()
  var0    <- df0 |> pull("var0") |> unique()
  doVar0  <- !(var0 |> is.na())
  if(doVar0) df0 <- df0 |> filter(variant %in% var0)
  rm(var0, doVar0)

  ### Separate df0 into two data frames
  df1     <- df0 |> filter(svGroupType |> str_detect("sv_bipoc"))
  df2     <- df0 |> filter(svGroupType |> str_detect("race"))
  df3     <- df0 |> filter(svGroupType |> str_detect("sv"))
  rm(df0)

  ### Calculate reference rates
  group1  <- group0  |> c("scenario", "region")
  sum1    <- c("aveRate_total")
  select1 <- group1  |> c(sum1)
  sort1   <- groups0 |> c(sum1)
  df1     <- df1 |> mutate(impPop_total  = impPop_sv    + impPop_ref)
  df1     <- df1 |> mutate(impact_total  = impact_sv    + impact_ref)
  df1     <- df1 |> mutate(aveRate_total = impact_total / impPop_total)
  # df1     <- df1 |> group_by_at(c(group2)) |> summarize_at(c(sum1), mean, na.rm=T) |> ungroup()
  # df1     <- df1 |> replace(. |> is.na(), 0)
  df1     <- df1 |> select(all_of(select1))
  df1     <- df1 |> pivot_wider(names_from="scenario", values_from=sum1)
  df1     <- df1 |> mutate(delta = df1[[scens0[1]]] - df1[[scens0[2]]])
  df1     <- df1 |> rename_at(c("delta"), ~sum1)
  df1     <- df1 |> group_by_at(c(group0)) |> summarize_at(c(sum1), sum, na.rm=T) |> ungroup()
  df1     <- df1 |> replace(. |> is.na(), 0)
  df1     <- df1 |> arrange_at(c(sort1))
  rm(group1, sum1, select1, sort1)

  ### Calculate average rates
  group2  <- group0  |> c("scenario", "region")
  sort2   <- groups0 |> c(rates0)
  df2     <- df2 |> group_by_at(c(group2)) |> summarize_at(c(rates0), mean, na.rm=T) |> ungroup()
  df2     <- df2 |> replace(. |> is.na(), 0)
  df2     <- df2 |> pivot_wider(names_from="scenario", values_from=rates0)
  df2     <- df2 |> mutate(delta = df2[[scens0[1]]] - df2[[scens0[2]]])
  df2     <- df2 |> rename_at(c("delta"), ~rates0)
  df2     <- df2 |> group_by_at(c(group0)) |> summarize_at(c(rates0), sum, na.rm=T) |> ungroup()
  df2     <- df2 |> replace(. |> is.na(), 0)
  df2     <- df2 |> arrange_at(c(sort2))
  rm(group2, sort2)

  ### Join data
  df0     <- df1 |> left_join(df2, by=group0)
  df0     <- df0 |> mutate(rate_per = aveRate_sv / aveRate_total * 1e2)
  rm(df1, df2)

  ### Return
  gc()
  return(df0)
}) |> bind_rows(); svAveRatesDf |> glimpse()


### Get plots
svAveRatesPlot <- svAveRatesDf |> (function(
    df0,
    xLab0   = "",
    yLab0   = "Percent (%)",
    colors0 = colors_7() |> rev(),
    theme0  = theme_minimal() +
      theme(legend.position="bottom") +
      theme(panel.grid.major.y = element_blank()) +
      theme(axis.text.x = element_text(size=10)) +
      theme(axis.text.y = element_text(size=10)) +
      theme(strip.text = element_text(size=12))
){
  ### Initialize plot
  p0 <- df0 |> ggplot(aes(x=svGroupType, y=rate_per))
  ### Add geoms
  p0 <- p0 + geom_bar(position="dodge", width=0.7, stat="identity", alpha=0.8)
  p0 <- p0 + geom_hline(yintercept=c(100), linetype="longdash", colour="black")
  ### Add scales
  p0 <- p0 + scale_x_discrete  (xLab0)
  p0 <- p0 + scale_y_continuous(yLab0)
  p0 <- p0 + scale_fill_manual(values=colors0, labels=function(x) x |> str_wrap(width=25))
  ### Flip coordinates and facet
  p0 <- p0 + coord_flip()
  p0 <- p0 + facet_wrap(.~svSectorDesc)
  ### Add theme
  p0 <- p0 + theme0
  ### Return
  return(p0)
})(); svAveRatesPlot |> print()
### Save plot
ggsave(plot=svAveRatesPlot, filename=saveDir |> file.path("sv_racerates") |> paste0(tiff0), width=9, height=6)

##### Delta Rates by SV Group -------------------------------------
#all SV group rates
### calculate as 1) sum sv rates for all regions (for policy and baseline)
### 2) subtract policy from baseline national sum for each race
### 3) divide each race rate by white rate (normalize to 100)
### interpretation: under the rule, based on the spatial patterns of different SV groups and intersections with avoided climate damages, X group is projected to benefit X% as much as the reference population for that group (e.g., white, high income, et.).(All benefit because all values are positive)
### Change to map
svDeltaRatesDf <- svSectorsAll |> map(function(
    sector0,
    list0   = svOutputsList,
    groups0 = c("svGroupType"),
    rates0  = c("aveRate_sv"),
    year0   = 2090,
    df1     = svGroupTypesDf,
    df2     = svSectorsDfAll
){
  ### Columns
  group0  <- groups0 |> c(df1 |> names()) |> c(df2 |> names()) |> c("year") |> unique()

  ### Filter observations
  ### Add SV group labels
  df0     <- list0[[sector0]]
  df0     <- df0 |> getFilterDf(filters0="year", years0=year0)
  df0     <- df0 |> left_join(df1, by=groups0)
  df0     <- df0 |> left_join(df2, by="sector")
  rm(list0, df1, df2)

  ### Filter to variant
  scens0  <- df0 |> pull(scenario) |> unique() |> sort()
  var0    <- df0 |> pull("var0") |> unique()
  doVar0  <- !(var0 |> is.na())
  if(doVar0) df0 <- df0 |> filter(variant %in% var0)
  rm(var0, doVar0)

  ### Separate df0 into two data frames
  df0     <- df0 |> filter(svGroupType |> str_detect("sv"))

  ### Calculate totals, average rates
  df0     <- df0 |> mutate(impPop_total  = impPop_sv    + impPop_ref)
  df0     <- df0 |> mutate(impact_total  = impact_sv    + impact_ref)
  df0     <- df0 |> mutate(aveRate_total = impact_total / impPop_total)

  ### Select, pivot, and calculate deltas
  group3  <- group0  |> c("scenario", "region")
  sum3    <- "aveRate_" |> paste0(c("sv", "ref", "total"))
  sList3  <- scens0  |> map(function(x0, y0=sum3){y0 |> paste0("_", x0)}) |> set_names(scens0)
  select3 <- group3  |> c(sum3)
  df0     <- df0 |> select(all_of(select3))
  df0     <- df0 |> pivot_wider(names_from="scenario", values_from=sum3)
  df0     <- df0 |> mutate(delta_aveRate_sv    = df0[[sList3[[scens0[1]]][1]]] - df0[[sList3[[scens0[2]]][1]]])
  df0     <- df0 |> mutate(delta_aveRate_ref   = df0[[sList3[[scens0[1]]][2]]] - df0[[sList3[[scens0[2]]][2]]])
  df0     <- df0 |> mutate(delta_aveRate_total = df0[[sList3[[scens0[1]]][3]]] - df0[[sList3[[scens0[3]]][3]]])

  ### Group and summarize
  df0     <- df0 |> group_by_at(c(group0)) |> summarize_at(c("delta_" |> paste0(sum3)), sum, na.rm=T) |> ungroup()
  df0     <- df0 |> replace(. |> is.na(), 0)

  ### Add lines and arrange
  sort3   <- groups0 |> c("rate_per_total")
  df0     <- df0 |> mutate(rate_per_total = marginal_sv / marginal_total * 1e2)
  df0     <- df0 |> mutate(rate_per_ref   = marginal_sv / marginal_ref   * 1e2)
  df0     <- df0 |> arrange_at(c(sort3))

  ### Return
  gc()
  return(df0)
}) |> bind_rows(); svDeltaRatesDf |> glimpse()

svDeltaRatesPlot <- svDeltaRatesDf |> (function(
    df0,
    xLab0   = "",
    yLab0   = "Percent (%)",
    colors0 = colors_7() |> rev(),
    theme0  = theme_minimal() +
      theme(legend.position="bottom") +
      theme(panel.grid.major.y = element_blank()) +
      theme(axis.text.x = element_text(size=11)) +
      theme(axis.text.y = element_text(size=11)) +
      theme(strip.text = element_text(size=11))
){
  ### Initialize plot
  p0 <- df0 |> ggplot(aes(x=svGroupType, y=rate_per_ref))
  ### Add geoms
  p0 <- p0 + geom_bar(position="dodge", width=0.7, stat="identity", alpha=0.8)
  p0 <- p0 + geom_hline(yintercept=c(100), linetype="longdash", colour="black")
  ### Add scales
  p0 <- p0 + scale_x_discrete  (xLab0)
  p0 <- p0 + scale_y_continuous(yLab0)
  p0 <- p0 + scale_fill_manual(values=colors0, labels=function(x) x |> str_wrap(width=25))
  ### Flip coordinates and facet
  p0 <- p0 + coord_cartesian() + coord_flip()
  p0 <- p0 + facet_wrap(.~svSectorDesc)
  ### Add theme
  p0 <- p0 + theme0
  ### Return
  return(p0)
})(); svDeltaRatesPlot |> print()
### Save plot
ggsave(plot=svDeltaRatesPlot, filename=saveDir |> file.path("sv_svrates2") |> paste0(tiff0), width=9, height=6)
### Save data
svDeltaRatesDf |>
  select(c("sector", "year", "svGroupType", "delta_aveRate_sv", "delta_aveRate_ref", "delta_aveRate_total", "rate_per_ref")) |> arrange_at(c("sector", "year", "svGroupType")) |>
  write.csv(file=saveDir |> file.path("sv_svrates2") |> paste0(csv0), row.names=FALSE)










### End Doc --------------------



