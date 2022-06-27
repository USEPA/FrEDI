### Created 2021.01.20
### This file contains hard coded values for constants used throughout the temperature binning process
###### Config List for createSystemData ######
frediData_config <- list()
###### Messages for createSystemData ######
frediData_config$messages_data = list(
    loadPackages = "Loading required packages...",
    sourceCode   = "Loading custom functions...",
    loadInputs   = list(try="Loading input data...", success="Input data loaded.", fail="Could not find input data."),
    calcScalars  = list(try="Calculating physical and economic scalars...", success="Physical and economic scalars calculated."),
    interpFuns   = list(try="Getting interpolation functions...", success="Interpolation functions complete."),
    saveRData    = list(try="Saving results...", success="R data loaded.", fail="Could not find R data."),
    outPath      = list(fail="Output directory not found.", success="Created output directory.", skip="No directory created. Exiting..."),
    aggImpacts   = list(try="Aggregating impacts...")
  )
  
###### Constants ######
###### Named constant values that are used frequently in the model
### Millions, billions
frediData_config$k_million <- 10^6
frediData_config$k_billion <- 10^9

###### temps2slr data info ######
# frediData_config$temps2slr <- list(
# ###### Data Folder and Library ######
# rel_cmip      = "Hector_SLR_comparison-main",
# ###### RCP ######
# rcpList       = paste0("rcp", c(26, 45, 85)),
# rcpLabels     = paste ("RCP", c(2.6, 4.5, 8.5)),
# avgYears      = c(1986:2005),
# eqYear        = 1849)
# 
# frediData_config$temps2slr$sample_fNames <- "." %>% 
#   paste(frediData_config$temps2slr$rel_cmip, "sample_outputstream", sep="/") %>%
#   paste(frediData_config$temps2slr$rcpList, sep="_") %>% 
#   paste("csv", sep=".")

###### Data years and eras ######
### Min year, max year
frediData_config$minYear <- 2010
# frediData_config$maxYear <- 2090
frediData_config$maxYear <- 2300





###### Config List for ciraTempBin ######
fredi_config <- frediData_config
###### List of Messages ######
fredi_config$list_messages = list(
    loadPackages  = "Loading required packages...",
    sourceCode    = "Loading custom functions...",
    loadRData     = list(
      try    ="Loading R data...",
      success="R data loaded.",
      fail   ="Could not find R data."
      ),
    loadInputs    = list(
      try    ="Loading input data...",
      success="Input data loaded.",
      fail   ="Could not find input data."
      ),
    globalTemps   = "Converting global temperature scenario to CONUS scenario...",
    updatePopGDP  = "Updating socioeconomic scenario...",
    updateScalars = list(try="Updating physical and economic scalars...", success="Physical and economic scalars updated."),
    scaledImpacts = list(try="Calculating annual scaled impacts", success="Calculation of scaled impacts complete."),
    aggImpacts    = list(try="Aggregating impacts..."),
    impactTypes   = list(try="Summing over impact types..."),
    impactYears   = list(try="Interpolating between impact estimate years..."),
    modelAves     = list(try="Getting model averages..."),
    national      = list(try="Calculating national totals...")
  )
###### Required package list ###### 
### Packages used in the RTool. Previous packages included: "scriptName", "stringr", and "ggplot2" # requiredPackageList <- c("tidyverse", "data.table", "openxlsx" #, #"sqldf", "openxlsx", "matrixStats", "RColorBrewer")

###### Lists of Years ######
### Sequences of years by 20-, 10-, 5-, and 1-year intervales
### Sequence from 2010 to 2090 with 20 year intervals
fredi_config$list_years_by100 <- seq(fredi_config$minYear, fredi_config$maxYear, 100)
fredi_config$list_years_by50  <- seq(fredi_config$minYear, fredi_config$maxYear, 50)
fredi_config$list_years_by20  <- seq(fredi_config$minYear, fredi_config$maxYear, 20)
fredi_config$list_years_by10  <- seq(fredi_config$minYear, fredi_config$maxYear, 10) 
fredi_config$list_years_by5   <- seq(fredi_config$minYear, fredi_config$maxYear, 5)  
fredi_config$list_years       <- seq(fredi_config$minYear, fredi_config$maxYear, 1)
###### Default values ######
### Base year, discount rate
### Types of aggregation
fredi_config$baseYear0     <- 2010
fredi_config$rate0         <- 0.3
fredi_config$discountRate0 <- 0.03
fredi_config$aggList0      <- c("national", "model", "impactYear", "impactType")
fredi_config$groupLevels0  <- c("sector", "adaptation", "impactYear", "impactType", "modelType", "model", "region")
  
###### temps2slr constants ######
fredi_config$temps2slr <- list(
  ### Phi: (i.e., "c", above) is a temperature-independent rate term with e-folding time tau2. I.e., phi is the multi-millennial contribution to GMSL in mm/year.
  ### Scalar: Sensitivity of the GSL rate to a deviation of T(t) from an equilibrium temperature of Te(t).
  ### Alpha: The value obtained in Mann et al. posterior distribution (Kopp et al., 2016 Figure S5a for "a").
  ### Tau1: the timescale on which the actual temperature relaxes toward the equilibrium temperature. Value obtained  in Mann et al. posterior distribution (Kopp et al., 2016 Figure S5a for "tau").
  ### Tau2: e-folding time(scale) for phi. Value obtained  in Mann et al. posterior distribution (Kopp et al., 2016 Figure S5a for "tau_C").
  phi0  = 0.14,
  alpha = 4.0,
  tau1  = 174,
  tau2  = 4175,
  
  ### year0: Reference year = 2000
  ### temp_C0: Initial temperature in C in 2000
  ### equilTemp0: Equilibrium temperature in year 2000
  ### We used HadCrUT4 to determine the appropriate temperature offset between the actual temperature and the equilibrium temperature in 2000.
  ### Met Office Hadley Centre observations datasets. https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/download.html.
  # ### slr_cm0: Initial slr_cm in 2000 = 0
  # year0      = 2000,
  # eqtemp_offset = 0.62,
  # temp_C0    = 0.62,
  # slr_cm0    = 0,
  
  ### Extend out to...
  max_year   = frediData_config$maxYear
)

###### Image Constants ######
### Defaults for regional images
# def_img_device  <- "png"
fredi_config$get_plots <- list(
def_img_unit    = "in",

### Heights and Widths
### Heat Maps: ~28 for GCM, ~10 for SLR
def_heat_width   = 16,
base_heat_ht_per = 0.13, ### Per region and group

### Annual damage images, by region and sector
### Different heights for regions, national (originally 6 for regions, 4.5 for national). 
base_rib_ht         = 2,
base_rib_per        = 1.25, ### Per region
base_rib_width_per  = 1.5, ### Per adaptation
base_rib_width      = 4,

###### Scale column ######
### Dataframe of units
df_units = data.frame(
  log10mod3  = 0:4,
  unitLabel = c("", "Thousands", "Millions", "Billions", "Trillions")
) %>%
  mutate(
    unitName  = unitLabel %>% tolower,
    unitValue = 10^(3*log10mod3),
    valueLog10 = unitValue %>% log10
  ) %>%
  select(unitName, unitLabel, unitValue, valueLog10, log10mod3)
)