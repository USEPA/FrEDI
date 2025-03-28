###### Function to run Degree of Warming (DOW) scenarios and create figures
### 2023.10.05: Renamed to create_DoW_results from create_report_figures
###### Load Packages ######
require(tidyverse)
require(ggpubr)
# require(arrow)
# require(cowplot)
# require(FrEDI)

###### create_DoW_results ######
create_DoW_results <- function(
    sectors  = FrEDI::get_sectorInfo(), ### Which sectors
    gcmYears = c(2090),   ### Which years to report on for GCM sectors
    slrYears = c(2050, 2090), ### Which years to report on for SLR sectors
    byState  = TRUE,      ### Whether values are by state or just by region
    totals   = FALSE,     ### Whether to do totals
    silent   = TRUE,      ### Degree of messaging
    testing  = FALSE,     ### Whether to print out extra diagnostic values
    aggOnly  = TRUE,      ### Whether to only include sectors for which "includeaggregate==1" in Fig 7 plots
    loadCode = "project", ### Whether to load code as source or devtools
    fpath    = "." ,      ### Path to main FrEDI directory to load code from if loadCode == "project" or loadCode == "package"
    saveFile = TRUE,      ### Save file
    outPath  = "." |> file.path("report_figures"),  ### Path to save results if saveFile == TRUE
    img_dev  = "pdf",     ### Image device if saveFile == TRUE
    return   = TRUE       ### Whether to return list object
){
  # sectors  = FrEDI::get_sectorInfo() ### Which sectors
  # gcmYears = c(2090) ### Which years to report on for GCM sectors
  # slrYears = c(2050, 2090) ### Which years to report on for SLR sectors
  # silent   = TRUE    ### Degree of messaging
  # testing  = TRUE    ### Whether to print out extra diagnostic values
  # byState  = TRUE    ### Whether values are by state or just by region
  # aggOnly  = TRUE    ### Whether to only include sectors for which "includeaggregate==1" in Fig 7 plots
  # loadCode = "project" ### Whether to load code as source or devtools
  # fpath    = "."     ### Path to main FrEDI directory to load code from if loadCode == "project" or loadCode == "package"
  # saveFile = TRUE   ### Save file
  # outPath  = "." |> file.path("report_figures")  ### Path to save results if saveFile == TRUE
  # img_dev  = "pdf"   ### Image device if saveFile == TRUE
  # return   = TRUE    ### Whether to return list object
  ###### Initial values ######
  ### Messaging
  do_msg        <- !silent
  ### Initialize Return List
  return0       <- return; rm(return)
  resultsList   <- list()
  ### How to load code
  loadProject   <- "project" %in% (loadCode |> tolower())
  loadPackage   <- "package" %in% (loadCode |> tolower())
  loadSource    <- !loadProject & !loadPackage

  ###### Set Up Environment ######
  ###### ** Set Paths ######
  # projectPath    <- getwd()     |> file.path("FrEDI")
  projectPath   <- fpath; rm(fpath)
  codePath      <- projectPath |> file.path("R")
  # projectPath |> list.files() |> print()
  # codePath |> list.files() |> print()

  ### Output Paths
  dowPath       <- outPath |> file.path("DoW")
  fig7Path      <- outPath |> file.path("fig7")
  appxPath      <- outPath |> file.path("appendix_figures")
  # ### Check and create paths
  # outPath |> check_and_create_path()
  # dowPath  |> check_and_create_path()
  # fig7Path |> check_and_create_path()
  # appxPath |> check_and_create_path()

  ### Output file names
  rda_byType    <- "integer_results_byType"
  rda_totals    <- "integer_results_totals"

  ### Plots by type
  csv_appx_gcm  <- "gcm_results_byDoW_byType"
  csv_appx_slr  <- "slr_results_byDoW_byType"
  rda_appx_gcm  <- "gcm_appendix_plots"
  rda_appx_slr  <- "slr_appendix_plots"

  ### Plots for totals
  csv_fig7_gcm  <- "gcm_results_byDoW_totals"
  csv_fig7_slr  <- "slr_results_byDoW_totals"
  rda_fig7_gcm  <- "gcm_fig7_plots"
  rda_fig7_slr  <- "slr_fig7_plots"

  ###### ** Load Code ######
  ### Custom function to load code from a specified path
  # codeFiles <- codePath |> list.files(pattern=".R", full.names = T); codeFiles |> basename()
  # for(code_i in codeFiles){code_i |> source()}
  # projectPath |> file.path("R") |> loadCustomFunctions(pattern="utils_report|utils_plot|utils_save|utils_summarize")
  loadCustomFunctions <- function(fpath=".", local=FALSE, pattern="utils_report|utils_plot|utils_save|utils_summarize|utils_create"){
    xFiles <- fpath |> list.files(pattern=pattern, full.names = T)
    xFiles |> basename() |> print()
    for(x_i in xFiles){x_i |> source(local=local)}
  } ### loadCustomFunctions
  ### Load Custom functions if testing the package
  ### Otherwise, load functions from FrEDI
  # getFromNamespace("value", "FrEDI")
  if       (loadProject){
    projectPath |> devtools::load_all()
  } else if(loadPackage){
    require(FrEDI)
  } else{
    codePath    |> loadCustomFunctions()
  } ### End else

  ###### ** Data options ######
  ### Adjust c_digits for number of digits after zero when saving to file
  ### Adjust c_years  for sequence of years to save to CSV
  # saveFile       <- FALSE
  c_digits       <- 16
  c_years        <- seq(2010, 2090, by=5)

  ###### ** Image options ######
  ### Set `fig7theme=NULL` for grey plot backgrounds or `fig7theme="bw"` to test code.
  ### Adjust breakChars for wrapping sector names in Fig 7
  # img_dev      <- "pdf"
  img_dev        <- img_dev
  imgRes         <- 200
  imgUnits       <- "in"
  breakChars     <- 18
  fig7theme      <- NULL

  ###### Format Sector Names
  ### Filter to specific sectors
  ### Sectors
  sectors1       <-
  sectorsLC0  <- sectors |> tolower(); rm(sectors)
  doAll       <- "all" %in% sectorsLC0

  if(doAll) {
    plotResults <- plotResults
  } else{
    ### Sectors
    sectors1     <- plotResults |> pull(sector) |> unique()
    sectorsLC1   <- sectors1    |> tolower()
    whichSectors <- sectorsLC1 %in% sectorsLC0
    plotSectors  <- sectors1[whichSectors]
    plotSectors |> print()
    ### Filter sectors
    plotResults <- plotResults |> filter(sector %in% plotSectors)
  } ### End if(doAll)

  ### Check the sector names (for wrapping for Figure 7)
  # c_sectorNames  <- get_sectorInfo()
  c_sectorNames  <- sectors
  newSectorNames <- c_sectorNames |> format_sectorNames(thresh0 = breakChars)



  ### Message and save to list
  if(testing|do_msg) "Formatting sector names for plotting..." |> message()
  if(return0) resultsList[["sectorNames"]] <- c_sectorNames
  if(testing) c_sectorNames |> print(); newSectorNames |> print()

  ###### Scenarios ######
  ###### ** Scenario Info ######
  ### Numeric columns: Specify so that we can print out the associated data
  ### Number of digits to format
  c_popCol       <- byState |> ifelse("state_pop", "reg_pop")
  # c_numVars      <- c("driverValue", "gdp_usd", "national_pop", "gdp_percap", "reg_pop", "annual_impacts")
  c_numVars      <- c("driverValue", "gdp_usd", "national_pop", "gdp_percap") |> c(c_popCol) |> c("annual_impacts")
  ### Integer temperatures: data frame of inputs
  conusPrefix0   <- "Other_Integer"
  globalPrefix0  <- "preI_global"
  ### Temperatures
  # c_conusTemps   <- 0:7
  c_conusTemps   <- 0:10
  c_globalTemps  <- c(1.487, 2.198)
  ### Numbers of scenarios
  n_conusTemps   <- c_conusTemps |> length()
  n_globalTemps  <- c_globalTemps |> length()
  ### Labels
  c_globTempLabs <- c(1.5, 2)

  ### Data frame of scenarios
  df_scenarios   <- tibble(temp_C=c_conusTemps |> c(c_globalTemps)) |>
    mutate(tempType  = c("conus" |> rep(n_conusTemps), "global" |> rep(n_globalTemps))) |>
    mutate(tempLabel = c_conusTemps |> c(c_globTempLabs) |> as.character()) |>
    mutate(prefix    = c(conusPrefix0 |> rep(n_conusTemps), globalPrefix0 |> rep(n_globalTemps))) |>
    mutate(scenario  = prefix |> paste0("_", tempLabel))
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

  ###### ** Inputs List ######
  ### Message
  if(testing|do_msg) "Creating tibble of integer scenarios..." |> print()
  # ### Load scenario inputs
  # inputs_df_int  <- list(
  #   x = df_scenarios[["temp_C"  ]],
  #   y = df_scenarios[["tempType"]],
  #   z = df_scenarios[["prefix"  ]]
  # )
  # ### Create constant temp scenarios
  # inputs_df_int  <- inputs_df_int |> pmap(function(x, y, z){
  #   create_constant_temp_scenario(
  #     temp0   = x,
  #     type0   = y,
  #     prefix0 = z ### Prefix for scenario
  #   )
  # }) |> bind_rows()
  ### Create constant temp scenarios
  inputs_df_int  <- df_scenarios |> nrow() |> seq_len() |> map(function(.i, df_i=df_scenarios[.i,]){
    create_constant_temp_scenario(
      temp0 = df_i[["temp_C"  ]],
      type0 = df_i[["tempType"]],
      scen0 = df_i[["scenario"]]
    )
  }) |> bind_rows()
  ### Glimpse, message, & save
  if(return0) resultsList[["df_inputs"]] <- inputs_df_int
  if(testing) inputs_df_int |> glimpse()
  # return(list(x=c_scen_con, y=c_scen_glo, z=inputs_df_int))

  ###### GCM Scenarios #####
  ###### ** Run Scenarios and get results by type ######
  ###### Run scenarios to get results by type
  ### Run scenarios in FrEDI. Get model averages and national totals
  if(testing|do_msg) "Running integer scenarios..." |> message()
  aggLvls0       <- c("modelaverage", "national")
  if(totals) aggLvls0 <- aggLvls0 |> c("impactyear", "impacttype")
  df_int_byType  <- inputs_df_int |> run_scenarios(
    col0      = "scenario",
    fredi     = TRUE,
    sectors   = sectors,
    # aggLevels = c("modelaverage", "national"),
    aggLevels = aggLvls0,
    scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
    joinCols  = c("year"),
    return    = TRUE,
    save      = FALSE,
    outPath   = dowPath
  )
  rm(aggLvls0)

  ### Glimpse results
  # if(return0) resultsList[["df_int_byType"]] <- df_int_byType
  if(testing) df_int_byType |> glimpse()
  ### Save results
  if(do_msg & saveFile) paste0("Saving integer scenario results by type...") |> message()
  if(saveFile & (!totals)) df_int_byType |> save_data(fpath=dowPath, fname=rda_byType, ftype="rda")
  if(saveFile &   totals ) df_int_byType |> save_data(fpath=dowPath, fname=rda_totals, ftype="rda")
  # for(scenario_i in cScenarios){
  #   ### Run scenario
  #   df_i <- scenario_i |> run_scenario(
  #     df0       = df_scenarios,
  #     fredi     = TRUE,
  #     sectors   = sectors,
  #     aggLevels = c("modelaverage", "national"),
  #     scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
  #     joinCols  = c("year"),
  #     save      = TRUE,
  #     return    = FALSE,
  #     outPath   = "." |> file.path("report_figures")
  #   ) ### End run_scenario(scenario_i)
  #   rm(scenario_i, df_i)
  # } ### End for(row_i in df_scenarios |> nrow() |> seq_len())

  ###### ** Appendix Figs: DoW By Type ######
  # codePath  |> loadCustomFunctions()
  if(!totals){
    if(testing|do_msg) "Summarizing GCM results by sector, impact type, degree of warming (DOW)..." |> message()
    sum_gcm_byType <- df_int_byType |> sum_impacts_byDoW_years(
      scenarios   = c_scen_con,
      bySector    = TRUE,
      sumCol      = "annual_impacts",
      impactYears = c("NA", "2010", "2090"),
      models      = c("GCM"),
      adjVal      = 1/10**9, ### Factor to multiply by
      adjCol      = "impact_billions",
      silent      = TRUE
    )
    ### Glimpse
    # if(return0) resultsList[["sum_gcm_byType"]] <- sum_gcm_byType
    if(testing) sum_gcm_byType |> glimpse()

    ### Save summary table
    if(do_msg & saveFile) paste0("Saving summary of GCM results by sector, impact type, degree of warming...") |> message()
    if(saveFile) sum_gcm_byType |> save_data(fpath=appxPath, fname=csv_appx_gcm, ftype="csv", row.names=F)


    ### Create Plots
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Plotting GCM results by sector, impact type, degree of warming (DOW)..." |> message()
    plots_gcm_byType <- sum_gcm_byType |>
      # filter(sector %in% c_sectorNames[c(10)]) |>
      filter(!(sector %in% c("Roads"))) |>
      plot_DoW_by_sector(
        models  = c("GCM"),
        yCol    = "annual_impacts"
      )
    ### Glimpse
    # if(return0) resultsList[["plots_gcm_byType"]] <- plots_gcm_byType
    if(testing) plots_gcm_byType$GCM$`Extreme Temperature_2010`[["2010"]] |> print()

    ### Save
    if(do_msg & saveFile) paste0("Saving plots of GCM results by sector, impact type, degree of warming...") |> message()
    if(saveFile){
      ### Save plots as a data object
      plots_gcm_byType |> save_data(fpath=appxPath, fname=rda_appx_gcm, ftype="rda")

      ### Save plots as image files
      saved0 <- plots_gcm_byType |> save_appendix_figures(
        df0       = sum_gcm_byType,
        modelType = "GCM", ### Or SLR
        fpath     = appxPath,
        device    = img_dev,
        res       = imgRes,
        units     = imgUnits
      ) ### End save_appendix_figures
    } ### End if(saveFile)
    rm(sum_gcm_byType, plots_gcm_byType)
  } ### End if(!totals)

  ###### ** GCM Totals ######
  if(totals){
    # if(testing|do_msg) "Aggregating integer scenario results..." |> message()
    # #### Aggregate Impact Types, Impact Years
    # # df_int_totals  <- df_int_byType |> run_scenarios(
    # #   col0      = "scenario",
    # #   fredi     = FALSE,
    # #   aggLevels = c("impactyear", "impacttype"),
    # #   scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
    # #   joinCols  = c("year")
    # # )
    # group_totals  <- c("sector", "variant", "impactType", "impactYear", "region", "state", "postal") |>
    #   c("model_type", "model") |>
    #   c("sectorprimary", "includeaggregate") |>
    #   c("scenario", "temp_C_conus", "temp_C_global", "slr_cm")
    # agg_totals    <- c("impactyear", "impacttype")
    # df_int_totals <- tibble()
    # for(scenario_i in cScenarios){
    #   ### Message user
    #   "\n" |> paste0("Running scenario ", (cScenarios == scenario_i) |> which(), "/" , nScenarios, "...") |> message()
    #   ### Get scenario by itself and drop scenario
    #   df_i          <- df_int_byType |> filter(scenario == scenario_i)
    #   df_int_byType <- df_int_byType |> filter(scenario != scenario_i)
    #   ### Aggregate scenario and add to dataframe
    #   df_i          <- df_i |> aggregate_impacts(aggLevels=agg_totals, groupByCols=group_totals)
    #   df_int_totals <- df_int_totals |> rbind(df_i)
    #   rm(df_i, scenario_i)
    # } ### End for(scenario_i in cScenarios)
    # rm(agg_totals, group_totals)
    # # rm(df_int_byType)
    # ### Glimpse results
    # # if(return0) resultsList[["df_int_totals"]] <- df_int_totals
    # if(testing) df_int_totals |> glimpse()
    # ### Save results
    # if(do_msg & saveFile) paste0("Saving aggregated integer scenario results...") |> message()
    # if(saveFile) df_int_totals |> save_data(fpath=dowPath, fname=rda_totals, ftype="rda")
    # # return(list(x=c_scen_con, y=c_scen_glo, z=df_int_totals))
    df_int_totals  <- df_int_byType
    rm(df_int_byType)

    ###### ** -- Figure 7: DoW by Sector
    ###### ** -- -- Data
    ###### Summarize GCM sectors for degrees of warming
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Summarizing GCM results by sector, degree of warming (DOW)..." |> message()
    sum_gcm_totals <- df_int_totals |> sum_impacts_byDoW_years(
      scenarios   = c_scen_con,
      bySector    = FALSE,
      sumCol      = "annual_impacts",
      impactYears = c("Interpolation"),
      models      = c("GCM"),
      aggOnly     = aggOnly,
      years       = gcmYears,
      adjVal      = 1/10**9, ### Factor to multiply by
      adjCol      = "impact_billions"
    )
    rm(df_int_totals)
    ### Glimpse
    # if(return0) resultsList[["sum_gcm_totals"]] <- sum_gcm_totals
    if(testing) sum_gcm_totals |> glimpse()
    if(do_msg & saveFile) paste0("Saving summary of GCM results by sector, degree of warming...") |> message()
    ### Save 2090 summary table
    if(saveFile) sum_gcm_totals |> save_data(fpath=fig7Path, fname=csv_fig7_gcm, ftype="csv", row.names=F)
    # return(list(x=c_scen_con, y=c_scen_glo, z=df_int_totals, w=sum_gcm_totals))

    ###### ** -- -- Plots
    #### Create plots
    ### Scale isn't the same across sectors
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Plotting GCM results by sector, degree of warming (DOW)..." |> message()
    plots_dow_gcm  <- sum_gcm_totals |> plot_DoW(
      types0  = c("GCM"), ### Model type: GCM or SLR
      years   = gcmYears,
      xCol    = "driverValue",
      yCol    = "annual_impacts",
      thresh0 = breakChars
    )
    rm(sum_gcm_totals)
    ### Glimpse
    # if(return0) resultsList[["plots_dow_gcm"]] <- plots_dow_gcm
    if(testing) plots_dow_gcm[["GCM_2090"]] |> print()
    ### Save
    # codePath  |> loadCustomFunctions()
    if(do_msg & saveFile) paste0("Saving plots of GCM results by sector, degree of warming...") |> message()
    if(saveFile){
      ### Save plots as Rdata
      plots_dow_gcm |> save_data(fpath=fig7Path, fname=rda_fig7_gcm, ftype="rda")

      ### Save plots as image files
      saved0 <- plots_dow_gcm |> save_fig7_images(
        modelType = "GCM",
        fpath     = fig7Path,
        device    = img_dev,
        units     = imgUnits
      )
    } ### End if(saveFile)
    rm(plots_dow_gcm)
  } ### End if(totals)

  ###### SLR Results & Figures ######
  ###### ** -- Format SLR Data ######
  ###### ** -- -- Data
  ### Read in and format the impacts
  ### Note that the SLR sectors have no multipliers or impact types
  # codePath  |> loadCustomFunctions()
  if(testing|do_msg) "Formatting SLR scenario model data..." |> message()
  ciraSLRData    <- get_fig7_slrDataObj(drivers=T, impacts=T)
  ### Glimpse
  # if(return0) resultsList[["ciraSLRData"]] <- ciraSLRData
  if(testing) ciraSLRData[["slrImp"]] |> glimpse()
  if(testing) ciraSLRData[["slrCm" ]] |> glimpse()

  # if(testing) ciraSLRData[["slrImp"]][["model"]] |> unique()
  # if(testing) ciraSLRData[["slrCm" ]][["model"]] |> unique()

  ### View the scaled impacts for a single sector
  plot_slr1      <- ciraSLRData[["slrImp"]] |>
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
  if(testing) plot_slr1 |> print()

  ### Visualize the driver values
  plot_slr2      <- ciraSLRData[["slrCm"]] |>
    # ggplot() + geom_line(aes(x=year, y=slr_cm, color = model)) +
    ggplot() + geom_line(aes(x=year, y=slr_cm, color = model)) +
    ggtitle("Sea Level Rise Trajectories", "Values following Sweet et al.") +
    scale_x_continuous("Year") +
    scale_y_continuous("GMSL (cm)") +
    scale_color_discrete("SLR Scenario")
  if(testing) plot_slr2 |> print()

  ##### ** -- Plot Trajectories #####
  ### Plot the SLR trajectories
  # codePath  |> loadCustomFunctions()
  if(testing|do_msg) "Plotting SLR scenarios..." |> message()
  p_slrScenarios <- plot_slr_scenarios(
    slrDrivers = ciraSLRData[["slrCm"]] |> filter(year >= 2010, year <= 2090),
    title0    = "Global Mean Sea Level Rise",
    subTitle0 = "Sweet et al. SLR Scenarios",
    lgdTitle0 = "Sweet et al. SLR Scenario"
  )
  ### Glimpse
  # if(return0) resultsList[["p_slrScenarios"]] <- p_slrScenarios
  if(testing) p_slrScenarios |> print()
  ### Save file
  if(do_msg & saveFile) paste0("Saving plot of SLR scenarios...") |> message()
  if(saveFile){
    p_slrScenarios |> save_image(
      fpath     = outPath, ### File path
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

  ###### ** -- Appendix Figs: DoW By Type ######
  # codePath  |> loadCustomFunctions()
  if(!totals){
    if(testing|do_msg) "Summarizing SLR results by sector, impact type, GMSL (cm)..." |> message()
    sum_slr_byType <- get_fig7_slrImpacts(
      slrDrivers  = ciraSLRData[["slrCm" ]] |> filter(year >= 2010, year <= 2090),
      slrImpacts  = ciraSLRData[["slrImp"]] |> filter(year >= 2010, year <= 2090),
      bySector    = TRUE,
      sumCol      = "annual_impacts",
      adjVal      = 1/10**9, ### Factor to multiply by
      adjCol      = "impact_billions"
    )
    ### Glimpse
    # if(return0) resultsList[["sum_slr_byType"]] <- sum_slr_byType
    if(testing) sum_slr_byType |> glimpse()
    ### Save
    if(do_msg & saveFile) paste0("Saving plot of SLR scenarios by year...") |> message()
    if(saveFile) sum_slr_byType |> save_data(fpath=appxPath, fname=csv_appx_slr, ftype="csv", row.names=F)

    ### Create SLR plots
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Plotting SLR results by sector, impact type, GMSL (cm)..." |> message()
    plots_slr_byType <- sum_slr_byType |> plot_DoW_by_sector(
      models  = c("SLR"),
      xCol    = "year",
      yCol    = "annual_impacts"
    )
    ### Glimpse
    # if(return0) resultsList[["plots_slr_byType"]] <- plots_slr_byType
    if(testing) plots_slr_byType$SLR$`Coastal Properties_all`[[1]] |> print()
    ### Save
    if(do_msg & saveFile) paste0("Saving plot of SLR scenarios by sector, impact type, GMSL (cm)...") |> message()
    if(saveFile) {
      ### Save plots as a data object
      plots_slr_byType |> save_data(fpath=appxPath, fname=rda_appx_slr, ftype="rda")

      ### Save plots as image files
      saved0 <- plots_slr_byType |> save_appendix_figures(
        df0       = sum_slr_byType,
        modelType = "SLR", ### Or SLR
        fpath     = appxPath,
        device    = img_dev,
        res       = imgRes,
        units     = imgUnits
      ) ### End save_appendix_figures
    } ### End if(saveFile)
  } ### if(!totals)

  ###### ** -- Figure 7: DoW by Sector ######
  ### SLR sectors separately:
  ### - Filter to 2090 and 2050 values
  ### - Calculate national totals
  ### - Combine CIRA impacts and SLR trajectories
  # codePath  |> loadCustomFunctions()
  if(totals){
    if(testing|do_msg) "Summarizing SLR results by sector, year, GMSL (cm)..." |> message()
    sum_slr_totals <- get_fig7_slrImpacts(
      slrDrivers = ciraSLRData[["slrCm" ]] |> filter(year >= 2010, year <= 2090),
      slrImpacts = ciraSLRData[["slrImp"]] |> filter(year >= 2010, year <= 2090),
      bySector   = FALSE,
      aggOnly    = aggOnly,
      years      = slrYears,
      adjVal     = 1/10**9, ### Factor to multiply by
      adjCol     = "impact_billions"
    )
    ### Glimpse
    # if(return0) resultsList[["sum_slr_totals"]] <- sum_slr_totals
    if(testing) sum_slr_totals |> glimpse()
    # sum_gcm_totals |> glimpse()
    ### Save
    if(do_msg & saveFile) paste0("Saving summary of SLR results by sector, year, GMSL (cm)...") |> message()
    if(saveFile) sum_slr_totals |> save_data(fpath=fig7Path, fname=csv_fig7_slr, ftype="csv", row.names=F)

    ###### ** -- Plots
    ### Create the plots
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Plotting SLR results by sector, year, GMSL (cm)..." |> message()
    plots_dow_slr  <- sum_slr_totals |> plot_DoW(
      types0     = c("SLR"), ### Model type: GCM or SLR
      yCol       = "annual_impacts",
      nCol       = 2,
      thresh0    = breakChars
    )
    ### Glimpse
    # if(return0) resultsList[["plots_dow_slr"]] <- plots_dow_slr
    if(testing) plots_dow_slr[["SLR_all"]] |> print()
    ### Save
    if(do_msg) paste0("Saving plots of SLR results by sector, year, GMSL (cm)...") |> message()
    if(saveFile & saveFile){
      ### Save plots as a data object
      plots_dow_slr |> save_data(fpath=fig7Path, fname=rda_fig7_slr, ftype="rda")

      ### Save plots as image files
      plots_dow_slr |> save_fig7_images(
        modelType = "SLR", ### Or SLR
        fpath     = fig7Path,
        device    = img_dev,
        units     = imgUnits
      )
    } ### End if(saveFile)
  } ### if(totals)



  ###### Return ######
  return(resultsList)
} ### End function
###### End File ######
