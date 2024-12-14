###### Function to run Degree of Warming (DOW) scenarios and create figures
### 2023.10.05: Renamed to create_DoW_results from create_report_figures
###### Load Packages
require(tidyverse)
require(ggpubr)
# require(arrow)
# require(cowplot)
# require(FrEDI)

###### create_DOW_plots
create_DOW_plots <- function(
    sectors  = FrEDI::get_sectorInfo(), ### Which sectors
    gcmYears = c(2090),       ### Which years to report on for GCM sectors
    slrYears = c(2050, 2090), ### Which years to report on for SLR sectors
    gcmData  = NULL ,     ### Dataframe with data for GCM sectors
    slrData  = NULL ,     ### Dataframe with data for SLR sectors
    totals   = FALSE,     ### Whether to do totals
    digits   = 16   ,     ### Number of digits in sector names for breaks
    silent   = TRUE ,     ### Degree of messaging
    testing  = FALSE,     ### Whether to print out extra diagnostic values
    aggOnly  = TRUE ,     ### Whether to only include sectors for which "includeaggregate==1" in Fig 7 plots
    loadCode = "project", ### Whether to load code as source or devtools
    fpath    = "."  ,     ### Path to main FrEDI directory to load code from if loadCode == "project" or loadCode == "package"
    saveFile = TRUE ,     ### Save file
    outPath  = "." |> file.path("report_figures"),  ### Path to save results if saveFile == TRUE
    img_dev  = "pdf",     ### Image device if saveFile == TRUE
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
  # ### Check and create paths
  # outPath |> print()
  # outPath |> check_and_create_path()

  ### Output file names
  saveStr       <- totals |> ifelse("totals", "byType"  )
  figStr        <- totals |> ifelse("fig7"  , "appendix")

  ### Plot and file names
  rda_data_tot  <- "gcm_DOW_totals"
  rda_data_gcm  <- "gcm_DOW_results" |> paste(saveStr, sep="_")
  rda_data_slr  <- "slr_DOW_results" |> paste(saveStr, sep="_")
  rda_plot_gcm  <- "gcm" |> paste(figStr, "plots", sep="_")
  rda_plot_slr  <- "slr" |> paste(figStr, "plots", sep="_")

  ###### ** Load Code ######
  ### Custom function to load code from a specified path
  # codeFiles <- codePath |> list.files(pattern=".R", full.names = T); codeFiles |> basename()
  # for(code_i in codeFiles){code_i |> source()}
  # projectPath |> file.path("R") |> loadCustomFunctions(pattern="utils_report|utils_plot|utils_save|utils_summarize")
  loadCustomFunctions <- function(fpath=".", local=FALSE, pattern="utils_report|utils_plot|utils_save|utils_summarize|utils_create"){
    xFiles <- fpath |> list.files(pattern=pattern, full.names=T)
    xFiles |> basename() |> print()
    for(x_i in xFiles){x_i |> source(local=local)}
  } ### loadCustomFunctions

  ### Load Custom functions if testing the package
  ### Otherwise, load functions from FrEDI
  # getFromNamespace("value", "FrEDI")
  if      (loadProject) projectPath |> devtools::load_all()
  else if (loadPackage) require(FrEDI)
  else                  codePath    |> loadCustomFunctions()

  ###### ** Data options ######
  ### Adjust digits for number of digits after zero when saving to file
  # digits      <- 16

  ###### ** Image options ######
  ### Set `fig7theme=NULL` for grey plot backgrounds or `fig7theme="bw"` to test code.
  ### Adjust breakChars for wrapping sector names in Fig 7
  # img_dev       <- "pdf"
  img_dev       <- img_dev
  imgRes        <- 200
  imgUnits      <- "in"
  breakChars    <- 18
  fig7theme     <- NULL

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
  ### Whether data objects present
  has_gcm       <- !(gcmData |> is.null())
  has_slr       <- !(slrData |> is.null())
  ### Combine sectors and check whether there are any
  sectors       <- c(gcmSectors, slrSectors)
  any_gcm       <- gcmSectors |> length()
  any_slr       <- slrSectors |> length()
  ### Conditionals
  do_gcm        <- has_gcm & any_gcm
  do_slr        <- has_slr & any_slr
  do_any        <- do_gcm | do_slr
  c(do_gcm, do_slr, do_any) |> print()

  ###### ** Filter to Sectors ######
  # if(do_gcm) gcmData <- gcmData |> filter(sector %in% gcmSectors)
  # if(do_slr) slrData <- slrData |> filter(sector %in% slrSectors)

  ###### ** Format Sector Names
  ### Check the sector names (for wrapping for Figure 7)
  if(do_any) {
    # sectorNames  <- get_sectorInfo()
    sectorNames <- sectors
    newSectors  <- sectorNames |> format_sectorNames(thresh0=breakChars)
    # sectorNames <- tibble(sector = sectors)
    # sectorNames <- sectorNames |> mutate(printName = sector |> format_sectorNames(thresh0=breakChars))
    ### Message and save to list
    if(testing|do_msg) "Formatting sector names for plotting..." |> message()
    if(return0) resultsList[["sectorNames"]] <- sectorNames
    if(testing) sectorNames |> print(); newSectors |> print()
    # if(testing) sectorNames |> glimpse();
  } ### End if(do_any)


  ###### GCM Figures ######
  # codePath  |> loadCustomFunctions()
  ###### ** National Combined ######
  if(totals & do_gcm) {
    if(testing|do_msg) "Aggregating integer scenario GCM sector results..." |> message()
    #### Get data object
    # gcmData  <- gcmData |>
    #### Aggregate Impact Types, Impact Years
    gcmData  <- gcmData |> run_scenarios(
      col0      = "scenario",
      fredi     = FALSE,
      aggLevels = c("impactyear", "impacttype"),
      scenCols  = c("scenario", "year", "temp_C_conus", "temp_C_global", "slr_cm"),
      joinCols  = c("year")
    ) ### End run_scenarios
    ### Filter to national totals
    gcmData  <- gcmData |> filter(region %in% "National Total")
    ### Glimpse
    if(return0) resultsList[["gcm_totals"]] <- gcmData
    if(testing) gcmData |> glimpse()
    if(do_msg & saveFile) paste0("Saving national GCM DOW results, aggregated across impact type and impact year...") |> message()
    ### Save 2090 summary table
    if(saveFile) gcmData |> save_data(fpath=outPath, fname=rda_data_tot, ftype="rda", row.names=F)

    ###### ** -- Figure 7: DoW by Sector
    ###### ** -- -- Data
    ###### Summarize GCM sectors for degrees of warming
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Summarizing GCM results by sector, degree of warming (DOW)..." |> message()
    sum_gcm <- gcmData |> sum_impacts_byDoW_years(
      scenarios   = c_scen_con,
      bySector    = FALSE,
      sumCol      = "annual_impacts",
      impactYears = c("Interpolation"),
      models      = c("GCM"),
      aggOnly     = aggOnly,
      years       = gcmYears,
      adjVal      = 1/10**9, ### Factor to multiply by
      adjCol      = "impact_billions"
    ) ### End sum_impacts_byDoW_years
    rm(gcmData)
    ### Glimpse
    if(return0) resultsList[["sum_gcm"]] <- sum_gcm
    if(testing) sum_gcm |> glimpse()
    if(do_msg & saveFile) paste0("Saving summary of GCM results by sector, degree of warming...") |> message()
    ### Save 2090 summary table
    if(saveFile) sum_gcm |> save_data(fpath=outPath, fname=rda_data_gcm, ftype="csv", row.names=F)
    # return(list(x=c_scen_con, y=c_scen_glo, z=gcmData, w=sum_gcm))

    ###### ** -- -- Plots
    #### Create plots
    ### Scale isn't the same across sectors
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Plotting GCM results by sector, degree of warming (DOW)..." |> message()
    plots_gcm  <- sum_gcm |> plot_DoW(
      types0  = c("GCM"), ### Model type: GCM or SLR
      years   = gcmYears,
      xCol    = "driverValue",
      yCol    = "annual_impacts",
      thresh0 = breakChars
    ) ### End plot_DoW
    rm(sum_gcm)
    ### Glimpse
    if(return0) resultsList[["plots_gcm"]] <- plots_gcm
    if(testing) plots_gcm[["GCM_2090"]] |> print()
    ### Save
    # codePath  |> loadCustomFunctions()
    if(do_msg & saveFile) paste0("Saving plots of GCM results by sector, degree of warming...") |> message()
    if(saveFile){
      ### Save plots as Rdata
      plots_gcm |> save_data(fpath=outPath, fname=rda_fig7_gcm, ftype="rda")

      ### Save plots as image files
      saved0 <- plots_gcm |> save_fig7_images(
        modelType = "GCM",
        fpath     = outPath,
        device    = img_dev,
        units     = imgUnits
      ) ### End save_fig7_images
    } ### End if(saveFile)
    rm(plots_gcm)
  } ### End if(totals)
  ###### ** Appendix Figures ######
  else if(!totals & do_gcm) {
    ### Filter data
    gcmData <- gcmData |> filter(sector %in% sectors)

    ### Message user
    if(testing|do_msg) "Summarizing GCM results by sector, impact type, degree of warming (DOW)..." |> message()
    sum_gcm <- gcmData |> sum_impacts_byDoW_years(
      scenarios   = c_scen_con,
      bySector    = TRUE,
      sumCol      = "annual_impacts",
      impactYears = c("NA", "2010", "2090"),
      models      = c("GCM"),
      adjVal      = 1/10**9, ### Factor to multiply by
      adjCol      = "impact_billions",
      silent      = TRUE
    ) ### End sum_impacts_byDoW_years
    ### Glimpse
    if(return0) resultsList[["sum_gcm"]] <- sum_gcm
    if(testing) sum_gcm |> glimpse()

    ### Save summary table
    if(do_msg & saveFile) paste0("Saving summary of GCM results by sector, impact type, degree of warming...") |> message()
    if(saveFile) sum_gcm |> save_data(fpath=outPath, fname=rda_data_gcm, ftype="csv", row.names=F)


    ### Create Plots
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Plotting GCM results by sector, impact type, degree of warming (DOW)..." |> message()
    plots_gcm <- sum_gcm |>
      # filter(sector %in% sectorNames[c(10)]) |>
      filter(!(sector %in% c("Roads"))) |>
      plot_DoW_by_sector(
        models  = c("GCM"),
        yCol    = "annual_impacts"
      ) ### End plot_DoW_by_sector
    ### Glimpse
    if(return0) resultsList[["plots_gcm"]] <- plots_gcm
    if(testing) plots_gcm$GCM$`Extreme Temperature_2010`[["2010"]] |> print()

    ### Save
    if(do_msg & saveFile) paste0("Saving plots of GCM results by sector, impact type, degree of warming...") |> message()
    if(saveFile){
      ### Save plots as a data object
      plots_gcm |> save_data(fpath=outPath, fname=rda_plot_gcm, ftype="rda")

      ### Save plots as image files
      saved0 <- plots_gcm |> save_appendix_figures(
        df0     = sum_gcm,
        typeCol = "model_type",
        type0   = "GCM",
        fpath   = outPath,
        device  = img_dev,
        res     = imgRes,
        units   = imgUnits
      ) ### End save_appendix_figures
    } ### End if(saveFile)
    rm(sum_gcm, plots_gcm)
  } ### End else(totals)


  ###### SLR Results & Figures ######
  if(do_slr) {
    ### Format SLR Data
    ### Save to different objects
    ### Note that the SLR sectors have no multipliers or impact types
    # codePath  |> loadCustomFunctions()
    slrImpacts <- slrData[["slrImp"]]
    slrHeights <- slrData[["slrCm" ]]
    # if(return0) resultsList[["slrData"]] <- slrData
    if(testing) slrImpacts |> glimpse()
    if(testing) slrHeights |> glimpse()

    # if(testing) slrImpacts[["model"]] |> unique()
    # if(testing) slrHeights[["model"]] |> unique()

    ### View the scaled impacts for a single sector
    plot_slr1      <- slrImpacts |>
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
    plot_slr2      <- slrHeights |>
      # ggplot() + geom_line(aes(x=year, y=slr_cm, color = model)) +
      ggplot() + geom_line(aes(x=year, y=slr_cm, color = model)) +
      ggtitle("Sea Level Rise Trajectories", "Values following Sweet et al.") +
      scale_x_continuous("Year") +
      scale_y_continuous("GMSL (cm)") +
      scale_color_discrete("SLR Scenario")
    if(testing) plot_slr2 |> print()

    ### Plot the SLR trajectories
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Plotting SLR scenarios..." |> message()
    p_slrScenarios <- plot_slr_scenarios(
      slrDrivers = slrHeights |> filter(year >= 2010, year <= 2090),
      title0    = "Global Mean Sea Level Rise",
      subTitle0 = "Sweet et al. SLR Scenarios",
      lgdTitle0 = "Sweet et al. SLR Scenario"
    ) ### End plot_slr_scenarios
    ### Glimpse
    if(return0) resultsList[["p_slrScenarios"]] <- p_slrScenarios
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
  } ### End if do_slr

  ###### ** National Combined ######
  ###### ** -- Figure 7: DoW by Sector
  ### SLR sectors separately:
  ### - Filter to 2090 and 2050 values
  ### - Calculate national totals
  ### - Combine CIRA impacts and SLR trajectories
  # codePath  |> loadCustomFunctions()
  if(totals & do_slr) {
    if(testing|do_msg) "Summarizing SLR results by sector, year, GMSL (cm)..." |> message()
    sum_slr <- get_fig7_slrImpacts(
      slrDrivers = slrHeights |> filter(year >= 2010, year <= 2090),
      slrImpacts = slrImpacts |> filter(year >= 2010, year <= 2090),
      bySector   = FALSE,
      aggOnly    = aggOnly,
      years      = slrYears,
      adjVal     = 1/10**9, ### Factor to multiply by
      adjCol     = "impact_billions"
    ) ### End get_fig7_slrImpacts()
    ### Glimpse
    if(return0) resultsList[["sum_slr"]] <- sum_slr
    if(testing) sum_slr |> glimpse()
    # sum_gcm |> glimpse()
    ### Save
    if(do_msg & saveFile) paste0("Saving summary of SLR results by sector, year, GMSL (cm)...") |> message()
    if(saveFile) sum_slr |> save_data(fpath=outPath, fname=rda_data_slr, ftype="csv", row.names=F)

    ###### ** -- Plots
    ### Create the plots
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Plotting SLR results by sector, year, GMSL (cm)..." |> message()
    nSectors   <- sum_slr  |> pull(sector) |> unique() |> length()
    nCols0     <- nSectors %% 4
    plots_slr  <- sum_slr  |> plot_DoW(
      types0  = c("SLR"), ### Model type: GCM or SLR
      years0  = slrYears,
      xCol    = "driverValue",
      yCol    = "impact_billions",
      nCol    = nCols0,
      thresh0 = breakChars
    ) ### End plot_DoW()
    ### Glimpse
    if(return0) resultsList[["plots_slr"]] <- plots_slr
    if(testing) plots_slr[["SLR_all"]] |> print()
    ### Save
    if(do_msg) paste0("Saving plots of SLR results by sector, year, GMSL (cm)...") |> message()
    if(saveFile & saveFile){
      ### Save plots as a data object
      plots_slr |> save_data(fpath=outPath, fname=rda_plot_slr, ftype="rda")

      ### Save plots as image files
      plots_slr |> save_fig7_images(
        modelType = "SLR", ### Or SLR
        fpath     = outPath,
        device    = img_dev,
        units     = imgUnits
      ) ### End save_fig7_images()
    } ### End if(saveFile)
  } ### if(totals)
  ###### ** Appendix Figures ######
  ### DoW By Type
  # codePath  |> loadCustomFunctions()
  else if(!totals & do_slr) {
    ### Filter if !totals
    sectors |> print()
    slrImpacts <- slrImpacts |> filter(sector %in% sectors)

    if(testing|do_msg) "Summarizing SLR results by sector, impact type, GMSL (cm)..." |> message()
    sum_slr <- get_fig7_slrImpacts(
      slrDrivers = slrHeights |> filter(year >= 2010, year <= 2090),
      slrImpacts = slrImpacts |> filter(year >= 2010, year <= 2090),
      bySector   = TRUE,
      aggOnly    = FALSE,
      sumCol     = "annual_impacts",
      adjVal     = 1/10**9, ### Factor to multiply by
      adjCol     = "impact_billions"
    ) ### End get_fig7_slrImpacts
    ### Glimpse
    if(return0) resultsList[["sum_slr"]] <- sum_slr
    if(testing) sum_slr |> glimpse()
    ### Save
    if(do_msg & saveFile) paste0("Saving plot of SLR scenarios by year...") |> message()
    if(saveFile) sum_slr |> save_data(fpath=outPath, fname=rda_data_slr, ftype="csv", row.names=F)

    ### Create SLR plots
    # codePath  |> loadCustomFunctions()
    if(testing|do_msg) "Plotting SLR results by sector, impact type, GMSL (cm)..." |> message()
    plots_slr_byType <- sum_slr |> plot_DoW_by_sector(
      models  = c("SLR"),
      xCol    = "year",
      yCol    = "annual_impacts"
    ) ### End plot_DoW_by_sector
    ### Glimpse
    if(return0) resultsList[["plots_slr_byType"]] <- plots_slr_byType
    if(testing) plots_slr_byType$SLR$`Coastal Properties_all`[[1]] |> print()
    ### Save
    if(do_msg & saveFile) paste0("Saving plot of SLR scenarios by sector, impact type, GMSL (cm)...") |> message()
    if(saveFile) {
      ### Save plots as a data object
      plots_slr_byType |> save_data(fpath=outPath, fname=rda_plot_slr, ftype="rda")

      ### Save plots as image files
      saved0 <- plots_slr_byType |> save_appendix_figures(
        df0     = sum_slr,
        typeCol = "model_type",
        type0   = "SLR",
        fpath   = outPath,
        device  = img_dev,
        res     = imgRes,
        units   = imgUnits
      ) ### End save_appendix_figures
    } ### End if(saveFile)
  } ### else(totals)


  ###### Return ######
  return(resultsList)
} ### End function
###### End File ######
