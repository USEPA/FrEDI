###### Function to run Degree of Warming (DOW) scenarios and create figures
### 2023.10.05: Renamed to create_DoW_results from create_report_figures
###### Load Packages
require(tidyverse)
require(devtools)
require(FrEDI)
# require(ggpubr)
# require(arrow)
# require(cowplot)

###### create_DoW_results
run_constantTempScenarios <- function(
    sectors  = FrEDI::get_sectorInfo(), ### Which sectors
    gcmYears = c(2010, 2090), ### Which years to report on for GCM sectors
    slrYears = c(2050, 2090), ### Which years to report on for SLR sectors
    tempList = list(conus  = list(
      temps      = c(0:10),
      tempLabels = c(0:10),
      tempType   = "conus",
      prefix     = "Other_Integer"
    ), ### End conus
    global = list(
      temps      = c(1.487, 2.198),
      tempLabels = c(1.5, 2),
      tempType   = "global",
      prefix     = "preI_global"
    ) ### End global
    ), ### End tempList
    aggOnly  = TRUE ,     ### Whether to only include sectors for which "includeaggregate==1" in Fig 7 plots
    silent   = TRUE ,     ### Degree of messaging
    testing  = FALSE,     ### Whether to print out extra diagnostic values
    loadCode = "project", ### Whether to load code as source or devtools
    fpath    = "."  ,     ### Path to main FrEDI directory to load code from if loadCode == "project" or loadCode == "package"
    saveFile = TRUE ,     ### Save file
    outPath  = "." |> file.path("report_figures"),  ### Path to save results if saveFile == TRUE
    return   = TRUE       ### Whether to return list object
){
  # sectors  = FrEDI::get_sectorInfo() ### Which sectors
  # gcmYears = c(2090) ### Which years to report on for GCM sectors
  # slrYears = c(2050, 2090) ### Which years to report on for SLR sectors
  # silent   = TRUE    ### Degree of messaging
  # testing  = TRUE    ### Whether to print out extra diagnostic values
  # aggOnly  = TRUE    ### Whether to only include sectors for which "includeaggregate==1" in Fig 7 plots
  # loadCode = "project" ### Whether to load code as source or devtools
  # fpath    = "."     ### Path to main FrEDI directory to load code from if loadCode == "project" or loadCode == "package"
  # saveFile = TRUE   ### Save file
  # outPath  = "." |> file.path("report_figures")  ### Path to save results if saveFile == TRUE
  # return   = TRUE    ### Whether to return list object
  ###### Initial values ######
  ### Messaging
  do_msg        <- !silent
  ### Initialize Return List
  return0       <- return; rm(return)
  resultsList   <- list()
  ### How to load code
  loadCodeLC    <- loadCode |> tolower()
  loadProject   <- "project" %in% loadCodeLC
  loadPackage   <- "package" %in% loadCodeLC
  loadSource    <- !loadProject & !loadPackage

  ###### Set Up Environment ######
  ###### ** Set Paths ######
  # projectPath    <- getwd()     |> file.path("FrEDI")
  projectPath   <- fpath; rm(fpath)
  codePath      <- projectPath |> file.path("R")
  # projectPath |> list.files() |> print()
  # codePath |> list.files() |> print()

  ### Output Paths
  # ### Check and create paths
  # outPath |> print()
  # outPath |> check_and_create_path()

  ### Output file names
  csv_inputs    <- "DOW_scenarios"
  rda_gcm       <- "gcm_DOW_scenario_results"
  rda_slr       <- "slr_scenario_results"

  ###### ** Load Code ######
  # ### Custom function to load code from a specified path
  # # codeFiles <- codePath |> list.files(pattern=".R", full.names = T); codeFiles |> basename()
  # # for(code_i in codeFiles){code_i |> source()}
  # # projectPath |> file.path("R") |> loadCustomFunctions(pattern="utils_report|utils_plot|utils_save|utils_summarize")
  # loadCustomFunctions <- function(fpath=".", local=FALSE, pattern="utils_report|utils_plot|utils_save|utils_summarize|utils_create"){
  #   xFiles <- fpath |> list.files(pattern=pattern, full.names=T)
  #   xFiles |> basename() |> print()
  #   for(x_i in xFiles){x_i |> source(local=local)}
  # } ### loadCustomFunctions

  ### Load Custom functions if testing the package
  ### Otherwise, load functions from FrEDI
  # getFromNamespace("value", "FrEDI")
  if      (loadProject) projectPath |> devtools::load_all()
  else if (loadPackage) require(FrEDI)
  else                  codePath    |> loadCustomFunctions()

  ###### Sectors ######
  ###### ** Check Sectors ######
  ### Check which sectors are in which type
  gcmSectors0   <- FrEDI::get_sectorInfo(gcmOnly=T)
  slrSectors0   <- FrEDI::get_sectorInfo(slrOnly=T)
  allSectors    <- c(gcmSectors0, slrSectors0)
  sectors0      <- sectors
  ### Lowercase, with spaces removed
  gcmSectorsLC0 <- gcmSectors0 |> tolower() |> trimws()
  slrSectorsLC0 <- slrSectors0 |> tolower() |> trimws()
  sectorsLC0    <- sectors0    |> tolower() |> trimws()
  doAll         <- "all" %in% sectorsLC0
  ### If doAll, use all the sectors. Otherwise, filter to specified sectors
  if(doAll) {
    gcmSectors <- gcmSectors0
    slrSectors <- slrSectors0
  } else{
    ### Check which sectors are present
    which_gcm  <- gcmSectorsLC0 %in% sectorsLC0
    which_slr  <- slrSectorsLC0 %in% sectorsLC0
    ### Filter to sectors
    gcmSectors <- gcmSectors0[which_gcm]
    slrSectors <- slrSectors0[which_slr]
  } ### End if(doAll)
  ### Combine sectors and check whether there are any
  sectors       <- c(gcmSectors, slrSectors)
  any_gcm       <- gcmSectors |> length()
  any_slr       <- slrSectors |> length()
  ### Conditionals
  do_gcm        <- any_gcm
  do_slr        <- any_slr
  c(do_gcm, do_slr) |> print()

  ###### GCM Scenarios ######
  ###### ** Scenario Info ######
  ### Numeric columns: Specify so that we can print out the associated data
  ### Number of digits to format
  # c_numVars      <- c("driverValue", "gdp_usd", "national_pop", "gdp_percap", "pop") |> c("annual_impacts")
  if(do_gcm) {
    ### Data frame of scenarios
    df_scenarios   <- tempList |> map(function(list_i){
      temps_i   <- list_i[["temps"     ]]
      type_i    <- list_i[["tempType"  ]]
      labels_i  <- list_i[["tempLabels"]]
      prefix_i  <- list_i[["prefix"    ]]
      hasLbls_i <- !(labels_i |> is.null())
      if(!hasLbls_i) labels_i <- temps_i
      df_i <- tibble(temp_C = temps_i) |>
        mutate(tempType  = type_i  ) |>
        mutate(tempLabel = labels_i) |>
        mutate(prefix    = prefix_i) |>
        mutate(scenario  = prefix |> paste0("_", tempLabel))
      return(df_i)
    }) |> bind_rows()

    ### Glimpse & save to list
    if(testing) "Creating tibble of integer scenario information..." |> message()
    if(return0) resultsList[["df_scenarios"]] <- df_scenarios
    if(testing) df_scenarios |> glimpse()
    ### Number of scenarios
    cScenarios     <- df_scenarios |> pull(scenario) |> unique()
    nScenarios     <- cScenarios   |> length()
    ### Vector of scenarios
    c_scen_con     <- df_scenarios |> filter(tempType == "conus" ) |> pull(scenario) |> unique()
    c_scen_glo     <- df_scenarios |> filter(tempType == "global") |> pull(scenario) |> unique()
    # c_scen_con |> print(); c_scen_glo |> print(); df_scenarios[["scenario"]] |> print()
    # return(list(x=c_scen_con, y=c_scen_glo, z=df_scenarios))
  } ### End if(do_gcm)

  ###### ** Create Inputs List ######
  ### Message
  if(do_gcm) {
    if(testing|do_msg) "Creating tibble of integer scenarios..." |> print()
    # ### Create constant temp scenarios
    # inputs_df_int  <- df_scenarios |> nrow() |> seq_len() |> map(function(.i, df_i=df_scenarios[.i,]){
    #   create_constant_temp_scenario(
    #     temp0 = df_i[["temp_C"  ]],
    #     type0 = df_i[["tempType"]],
    #     scen0 = df_i[["scenario"]]
    #   ) ### End create_constant_temp_scenario
    # }) |> bind_rows()
    inputs_df_int  <- df_scenarios |> (function(df0){
        renameAt0 <- c("temp_C", "tempType", "scenario")
        renameTo0 <- c("temp0", "type0", "scen0")
        df0       <- df0 |> rename_at(c(renameAt0), ~renameTo0)
        df0       <- df0 |> select(all_of(renameTo0))
      })()
    inputs_df_int <- inputs_df_int |> as.list()
    inputs_df_int <- inputs_df_int |> pmap(create_constant_temp_scenario) |> bind_rows()
    ### Glimpse, message, & save
    # if(return0) resultsList[["df_inputs"]] <- inputs_df_int
    inputs_df_int |> save_data(fpath=outPath, fname=csv_inputs, ftype="csv", row.names=F)
    if(testing) inputs_df_int |> glimpse()
    # return(list(x=c_scen_con, y=c_scen_glo, z=inputs_df_int))
  } ### End if(do_gcm)

  ###### GCM Results #####
  ###### Run scenarios to get results by type
  ### Run scenarios in FrEDI. Get model averages and national totals
  if(do_gcm) {
    if(testing|do_msg) "Running integer scenarios..." |> message()
    aggLvls0       <- c("modelaverage", "national")
    df_int_byType  <- inputs_df_int |>
      # filter(scenario %in% "Other_Integer_1") |>
      run_scenarios(
      col0      = "scenario",
      fredi     = TRUE,
      sectors   = gcmSectors0,
      years     = gcmYears,
      aggLevels = aggLvls0,
      scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
      joinCols  = c("year"),
      return    = TRUE,
      save      = FALSE,
      outPath   = outPath
    ) ### End run_scenarios
    rm(aggLvls0)

    ### Drop SLR values
    df_int_byType  <- df_int_byType |> filter(model_type %in% "GCM")

    ### Glimpse results
    if(return0) resultsList[["df_int_byType"]] <- df_int_byType
    if(testing) df_int_byType |> glimpse()
    ### Save results
    if(do_msg & saveFile) paste0("Saving integer scenario results by type...") |> message()
    if(saveFile) df_int_byType |> save_data(fpath=outPath, fname=rda_gcm, ftype="rda")
    rm(df_int_byType)
  } ### End if(do_gcm)

  ###### SLR Results ######
  if(do_slr) {
    ###### ** Format SLR Data
    ###### ** -- -- Data
    ### Read in and format the impacts
    ### Note that the SLR sectors have no multipliers or impact types
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Formatting SLR scenario model data..." |> message()
    # ciraSLRData    <- get_fig7_slrDataObj(drivers=T, impacts=T)
    ciraSLRData    <- get_fig7_slrDataObj(drivers=T, impacts=T, years=slrYears)
    ### Glimpse
    if(return0) resultsList[["ciraSLRData"]] <- ciraSLRData
    if(testing) ciraSLRData[["slrImp"]] |> glimpse()
    if(testing) ciraSLRData[["slrCm" ]] |> glimpse()
    ### Save results
    if(do_msg & saveFile) paste0("Saving SLR scenario results by type...") |> message()
    if(saveFile) ciraSLRData |> save_data(fpath=outPath, fname=rda_slr, ftype="rda")
    rm(ciraSLRData)
  } ### End if do_slr

  ###### Return ######
  gc()
  if(return0) return(resultsList)
  else        return()
} ### End function
###### End File ######
