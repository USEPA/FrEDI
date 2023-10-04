###### Set Up Environment ######
###### ** Load Packages ######
require(tidyverse)
require(ggpubr)
require(cowplot)
require(FrEDI)


###### ** Set Paths ######
projectPath      <- getwd()     |> file.path("FrEDI")
testsPath        <- projectPath |> file.path("data_tests")
codePath         <- testsPath   |> file.path("R")

### Output Paths
mainResultsPath <- projectPath     |> file.path("report_figures")
dowResultsPath  <- mainResultsPath |> file.path("DoW")
fig7ResultsPath <- mainResultsPath |> file.path("fig7")
appxResultsPath <- mainResultsPath |> file.path("appendix_figures")
# ### Check and create paths
# mainResultsPath |> check_and_create_path()
# dowResultsPath  |> check_and_create_path()
# fig7ResultsPath |> check_and_create_path()
# appxResultsPath |> check_and_create_path()

###### ** Load Code ######
### Custom function to load code from a specified path
# codeFiles <- codePath |> list.files(pattern=".R", full.names = T); codeFiles |> basename()
# for(code_i in codeFiles){code_i |> source()}
loadCustomFunctions <- function(fpath=codeFiles, local=FALSE){
  xFiles <- fpath |> list.files(pattern=".R", full.names = T)
  xFiles |> basename() |> print()
  for(x_i in xFiles){x_i |> source(local=local)}
}
codePath  |> loadCustomFunctions()


###### ** Constants ######
### Numeric columns: Specify so that we can print out the associated data
### Number of digits to format
c_numVars      <- c("driverValue", "gdp_usd", "national_pop", "gdp_percap", "reg_pop", "annual_impacts")
### Integer temperatures: data frame of inputs
c_conusTemps   <- 0:7
c_globalTemps  <- c(1.487, 2.198)
c_globTempLabs <- c(1.5, 2)
nConus         <- c_conusTemps  |> length()
nGlobal        <- c_globalTemps |> length()

### Model Info
c_modelTypes  <- c("GCM", "SLR")
c_modelTypes1 <- "GCM"
c_modelTypes2 <- "SLR"

###### ** Data options ######
### Adjust c_digits for number of digits after zero when saving to file
### Adjust c_years  for sequence of years to save to CSV
saveFile       <- FALSE
c_digits       <- 16
c_years        <- seq(2010, 2090, by=5)

###### ** Image options ######
### Set `fig7theme=NULL` for grey plot backgrounds or `fig7theme="bw"` to test code.
### Adjust breakChars for wrapping sector names in Fig 7
imgDevice     <- "pdf"
imgRes        <- 200
imgUnits      <- "in"
breakChars    <- 18
fig7theme     <- NULL

###### Format Sector Names
### Check the sector names (for wrapping for Figure 7)
c_sectorNames        <- get_sectorInfo()
new_sector_names     <- c_sectorNames |> format_sectorNames(thresh0 = breakChars); new_sector_names

###### Load Scenario Inputs ######
### Load scenario inputs
inputs_df_int0 <- c_conusTemps |>
  map(create_constant_temp_scenario, type0 = "int") %>%
  (function(x){do.call(rbind, x)})
inputs_df_int1 <- c_globalTemps |>
  map(create_constant_temp_scenario, type0 = "global") %>%
  (function(x){do.call(rbind, x)})
inputs_df_int  <- inputs_df_int0 |> rbind(inputs_df_int1)
rm("inputs_df_int0", "inputs_df_int1")
inputs_df_int |> glimpse()


### Unique scenarios
c_scen_int  <- inputs_df_int[["scenario"]] |> unique()
c_scen_con  <- c_scen_int[1:nConus]
c_scen_glo  <- c_scen_int[nConus+(1:nGlobal)]

###### Run Scenarios ######
###### Run scenarios
###### ** Results By Type ######
### Run scenarios in FrEDI. Get model averages and national totals

df_int_byType <- inputs_df_int |> run_scenarios(
  col0      = "scenario",
  fredi     = TRUE,
  aggLevels = c("modelaverage", "national"),
  scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
  joinCols  = c("year")
)

### Glimpse results
df_int_byType |> glimpse()
# ### Save results


if(saveFile){
  df_int_byType |>
    save_data(fpath = dowResultsPath, fname = "integer_results_byType", ftype = "rda")

  df_int_byType |>
    filter_years(years=c_years) |>
    format_values(cols0=c_numVars, digits=c_digits) |>
    save_data(fpath = dowResultsPath, fname = "integer_results_byType", ftype = "csv", row.names = F)
}

###### ** Result Totals ######
#### Aggregate Impact Types, Impact Years
df_int_totals <- df_int_byType %>% run_scenarios(
  col0      = "scenario",
  fredi     = FALSE,
  aggLevels = c("impactyear", "impacttype"),
  scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
  joinCols  = c("year")
)
### Glimpse results
df_int_totals |> glimpse()
### Save results
if(saveFile){
  df_int_totals |>
    save_data(fpath = dowResultsPath, fname = "integer_results_totals", ftype = "rda")

  df_int_totals |>
    filter_years(years=c_years) |>
    format_values(cols0=c_numVars, digits=c_digits) |>
    save_data(fpath = dowResultsPath, fname = "integer_results_totals", ftype = "csv", row.names = F)
}

###### Figure 7: Degrees of Warming ######
###### ** GCM ######
###### ** -- Data
###### Summarize GCM sectors for degrees of warming
codePath  |> loadCustomFunctions()
sum_gcm_totals <- df_int_totals |> sum_impacts_byDoW_years(
  scenarios   = c_scen_con,
  bySector    = FALSE,
  sumCol      = "annual_impacts",
  impactYears = c("Interpolation"),
  models      = c("GCM"),
  years       = c(2010, 2050, 2090),
  adjVal      = 1/10**9, ### Factor to multiply by
  adjCol      = "impact_billions"
)
### Glimpse
sum_gcm_totals |> glimpse()
### Save 2090 summary table
if(saveFile){
  sum_gcm_totals |>
    save_data(fpath = fig7ResultsPath, fname = "gcm_results_byDoW_totals", ftype = "csv", row.names = F)
}

###### ** -- Plots
#### Create plots
### Scale isn't the same across sectors
# codePath  |> loadCustomFunctions()
# (3 |> get_p10Labels(type="p1000"))[["label"]][1]
# test_scale <- sum_gcm_totals |> filter(sector %in% "ATS Extreme Temperature") |> get_colScale(col0="annual_impacts", nTicks=5); test_scale
codePath  |> loadCustomFunctions()
plots_dow_gcm <- sum_gcm_totals |>
  # filter(sector %in% c_sectorNames[1:2]) |>
  plot_DoW(
  types0     = c("GCM"), ### Model type: GCM or SLR
  # years0     = c(2010, 2050, 2090),
  years0     = c(2010),
  xCol       = "driverValue",
  yCol       = "annual_impacts",
  thresh0    = breakChars
)
### Glimpse
plots_dow_gcm[["GCM_2010"]]
plots_dow_gcm[["GCM_2050"]]
plots_dow_gcm[["GCM_2090"]]
### Save
codePath  |> loadCustomFunctions()
if(saveFile){
  ### Save plots as Rdata
  plots_dow_gcm |> save_data(fpath = fig7ResultsPath, fname = "gcm_fig7_plots", ftype = "rda")

  ### Save plots as image files
  saved0 <- plots_dow_gcm |> save_fig7_images(
    modelType = "GCM", ### Or SLRhttp://127.0.0.1:8557/graphics/da2b5ffe-689d-4fb9-ae88-f9bb22bf6d59.png
    fpath     = fig7ResultsPath,
    device    = imgDevice,
    units     = imgUnits
  )
}

###### ** SLR ######
###### ** -- Data
### Read in and format the impacts
### Note that the SLR sectors have no multipliers
codePath  |> loadCustomFunctions()
ciraSLRData  <- get_fig7_slrDataObj(drivers=T, impacts=T)
ciraSLRData[["slrImp"]] |> glimpse()
ciraSLRData[["slrCm" ]] |> glimpse()

ciraSLRData[["slrImp"]][["model"]] |> unique()
ciraSLRData[["slrCm" ]][["model"]] |> unique()

### View the scaled impacts for a single sector
ciraSLRData[["slrImp"]] |>
  filter(sector=="Coastal Properties") |>
  filter(region=="Southeast") |>
  filter(variant=="No Additional Adaptation") |>
  mutate(model = model) |>
  mutate(impacts_billions=annual_impacts/10^9) |>
  ggplot() + geom_line(aes(x=year, y=impacts_billions, color = model)) +
  ggtitle("Coastal Properties Impacts for Southeast") +
  scale_x_continuous("Year") +
  scale_y_continuous("Impacts (2015$ Billions)") +
  scale_color_discrete("SLR Scenario")

### Visualize the driver values
ciraSLRData[["slrCm"]] |>
  # ggplot() + geom_line(aes(x=year, y=slr_cm, color = model)) +
  ggplot() + geom_line(aes(x=year, y=slr_cm, color = model)) +
  ggtitle("Sea Level Rise Trajectories", "Values following Sweet et al.") +
  scale_x_continuous("Year") +
  scale_y_continuous("GMSL (cm)") +
  scale_color_discrete("SLR Scenario")

### SLR sectors separately:
### - Filter to 2090 and 2050 values
### - Calculate national totals
### - Combine CIRA impacts and SLR trajectories
codePath  |> loadCustomFunctions()
sum_slr_totals <- get_fig7_slrImpacts(
  slrDrivers = ciraSLRData[["slrCm" ]] |> filter(year >= 2010, year <= 2090),
  slrImpacts = ciraSLRData[["slrImp"]] |> filter(year >= 2010, year <= 2090),
  bySector   = FALSE,
  years      = c(2010, 2050, 2090),
  adjVal     = 1/10**9, ### Factor to multiply by
  adjCol     = "impact_billions"
  )
### Glimpse
sum_slr_totals |> glimpse()
# sum_gcm_totals |> glimpse()
### Save
if(saveFile){
  sum_slr_totals |>
    save_data(fpath = fig7ResultsPath, fname = "slr_results_byDoW_totals", ftype = "csv", row.names = F)
}

###### ** -- Plots
### Create the plots
codePath  |> loadCustomFunctions()
plots_dow_slr <- sum_slr_totals |> plot_DoW(
  types0     = c("SLR"), ### Model type: GCM or SLR
  yCol       = "annual_impacts",
  nCol       = 2,
  thresh0    = breakChars
)
### Glimpse
plots_dow_slr[["SLR_all"]]
### Save
if(saveFile){
  ### Save plots as a data object
  plots_dow_slr |> save_data(fpath = fig7ResultsPath, fname = "slr_fig7_plots", ftype = "rda")

  ### Save plots as image files
  plots_dow_slr |> save_fig7_images(
    modelType = "SLR", ### Or SLR
    fpath     = fig7ResultsPath,
    device    = imgDevice,
    units     = imgUnits
  )
}

###### Appendix Graphics (By Type) ######
###### ** GCM ######
codePath  |> loadCustomFunctions()
sum_gcm_byType <- df_int_byType |> sum_impacts_byDoW_years(
  scenarios   = c_scen_con,
  bySector    = TRUE,
  sumCol      = "annual_impacts",
  impactYears = c("NA", "2010", "2090"),
  models      = c("GCM"),
  years       = c(2010, 2050, 2090),
  adjVal      = 1/10**9, ### Factor to multiply by
  adjCol      = "impact_billions",
  silent      = TRUE
)
### Glimpse
sum_gcm_byType |> glimpse()
### Save summary table
if(saveFile){
  sum_gcm_byType |>
    save_data(fpath = appxResultsPath, fname = "gcm_results_byDoW_byType", ftype = "csv", row.names = F)
}

### Create Plots
codePath  |> loadCustomFunctions()
plots_gcm_byType <- sum_gcm_byType |>
  # filter(sector %in% c_sectorNames[c(10)]) |>
  plot_DoW_by_sector(
    models  = c("GCM"),
    years   = c(2010, 2050, 2090),
    # years   = c(2010),
    yCol    = "annual_impacts"
  )

### Glimpse
# plots_gcm_byType$GCM$`ATS Extreme Temperature_2010`[["NA"]]
# plots_gcm_byType$GCM$`Extreme Temperature_2010`[["2010"]]

### Save
if(saveFile){
  plots_gcm_byType |> save_data(fpath = appxResultsPath, fname = "gcm_appendix_plots", ftype = "rda")

  saved0 <- plots_gcm_byType |> save_appendix_figures(
    df0       = sum_gcm_byType,
    modelType = "GCM", ### Or SLR
    fpath     = appxResultsPath,
    device    = imgDevice,
    res       = imgRes,
    units     = imgUnits
  ) ### End save_appendix_figures
} ### End if(saveFile)

###### ** SLR ######
# inputs_df_int %>% names
codePath  |> loadCustomFunctions()
sum_slr_byType <- get_fig7_slrImpacts(
  slrDrivers  = ciraSLRData[["slrCm" ]] |> filter(year >= 2010, year <= 2090),
  slrImpacts  = ciraSLRData[["slrImp"]] |> filter(year >= 2010, year <= 2090),
  bySector    = TRUE,
  sumCol      = "annual_impacts",
  years       = c(2010, 2050, 2090),
  adjVal      = 1/10**9, ### Factor to multiply by
  adjCol      = "impact_billions"
)
### Glimpse
sum_slr_byType |> glimpse()
### Save
if(saveFile){
  sum_slr_byType |>
    save_data(fpath = appxResultsPath, fname = "slr_results_byDoW_byType", ftype = "csv", row.names = F)
}

### Create plots
codePath  |> loadCustomFunctions()
plots_slr_byType <- sum_slr_byType |> plot_DoW_by_sector(
  models  = c("SLR"),
  xCol    = "year",
  yCol    = "annual_impacts"
)
### Glimpse
plots_slr_byType$SLR$`Coastal Properties_all`[[1]]
### Save
if(saveFile){
  plots_slr_byType |> save_data(fpath = appxResultsPath, fname = "slr_appendix_plots", ftype = "rda")

  saved0 <- plots_slr_byType |> save_appendix_figures(
    df0       = sum_slr_byType,
    modelType = "SLR", ### Or SLR
    fpath     = appxResultsPath,
    device    = imgDevice,
    res       = imgRes,
    units     = imgUnits
  ) ### End save_appendix_figures
} ### End if(saveFile)



##### SLR Trajectories
### Plot the SLR trajectories
codePath  |> loadCustomFunctions()
p_slrScenarios <- plot_slr_scenarios(
  slrDrivers = ciraSLRData[["slrCm"]] |> filter(year >= 2010, year <= 2090),
  title0    = "Global Mean Sea Level Rise",
  subTitle0 = "Sweet et al. SLR Scenarios",
  lgdTitle0 = "Sweet et al. SLR Scenario"
)
### Glimpse
p_slrScenarios
### SAve
if(saveFile){
  p_slrScenarios |> save_image(
    fpath     = appxResultsPath, ### File path
    fname     = "slrScenarios",
    device    = "pdf", ### CSV or RData
    options   = list(
      height = 6,
      width  = 6,
      res    = imgRes,
      units  = imgUnits
    ) ### End options
  )  ### End save_image
} ### End if(saveFile)
# sum_gcm_byType |> glimpse()


###### End File ######
