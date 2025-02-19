###### Function to run Degree of Warming (DOW) scenarios and create figures
### 2023.10.05: Renamed to create_DoW_results from create_report_figures
###### Load Packages
# require(tidyverse)
# require(devtools)
# require(FrEDI)
# require(ggpubr)
# # require(arrow)
# # require(cowplot)
# # require(sf)
# # require(patchwork)

###### create_sector_maps
create_sector_maps <- function(
    df0      = NULL ,     ### Dataframe with outputs of FrEDI
    sectors  = FrEDI::get_sectorInfo(), ### Which sectors
    year0    = 2090 ,     ### Year to plot
    silent   = TRUE ,     ### Degree of messaging
    testing  = FALSE,     ### Whether to print out extra diagnostic values
    loadCode = "project", ### Whether to load code as source or devtools
    fpath    = "."  ,     ### Path to main FrEDI directory to load code from if loadCode == "project" or loadCode == "package"
    saveFile = TRUE ,     ### Save file
    outPath  = "." |> file.path("maps"),  ### Path to save results if saveFile == TRUE
    img_dev  = "pdf",     ### Image device if saveFile == TRUE
    return   = TRUE       ### Whether to return list object
){
  ### Initial values -----------------------------------------------------
  ### Messaging
  do_msg        <- !silent
  ### Initialize Return List
  return0       <- return; rm(return)
  resultsList   <- list()
  ### How to load code
  loadProject   <- "project" %in% (loadCode |> tolower())
  loadPackage   <- "package" %in% (loadCode |> tolower())
  loadSource    <- !loadProject & !loadPackage

  ### Set Up Environment -----------------------------------------------------
  #### Set Paths -----------------------------------------------------
  # projectPath    <- getwd()     |> file.path("FrEDI")
  projectPath   <- fpath; rm(fpath)
  codePath      <- projectPath |> file.path("R")
  # projectPath |> list.files() |> print()
  # codePath |> list.files() |> print()

  ### Output Paths
  # ### Check and create paths
  # outPath |> print()
  # outPath |> check_and_create_path()

  ### Maps
  mapTotPath    <- outPath |> file.path("fig304total") |> paste0(".", "tiff")
  mapSectPath   <- outPath |> file.path("map_") |> paste0("*", ".", "tiff")

  ### Plot and file names
  rda_data_tot  <- "gcm_DOW_totals"
  rda_data_gcm  <- "gcm_results" |> paste(saveStr, sep="_")
  rda_data_slr  <- "slr_results" |> paste(saveStr, sep="_")
  rda_plot_gcm  <- "gcm" |> paste(figStr, "plots", sep="_")
  rda_plot_slr  <- "slr" |> paste(figStr, "plots", sep="_")

  #### Load Code -----------------------------------------------------
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


  ### Process Results -----------------------------------------------------
  #### Filter Reults -----------------------------------------------------
  ### Subset to 2090 and get model averages
  ### Subset to sectorprimary values
  ### Drop national totals
  fredi2090    <- df0 |>
    getFilterDf(filters0 = c("year", "model"), year0=2090) |>
    getFilterDf(filters0 = c("sectorprimary")) |>
    getFilterDf(filters0 = c("region"), reverse0=TRUE)
  fredi2090$sector |> unique() |> print()

  #### Adjust Suicides -----------------------------------------------------
  ### Adjust suicides
  frediAdj     <- fredi2090 |> adjustSector1_bySector2(
    sector1 = "ATS Temperature-Related Mortality",
    sector2 = "Suicide",
    join0   = c("state", "model", "year"),
    cols0   = c("annual_impacts")
  ) ### End adjustSector1_bySector2
  frediAdj |> glimpse()

  ### Sum results
  frediAdjSum <- frediAdj |> getFilterDf(
    filters0 = c("sectorprimary", "includeaggregate")
  ) |> group_by_at(c(
    "region", "state", "postal", "year", "gdp_usd", "national_pop", "gdp_percap", "pop"
  )) |> summarize_at(
    c("annual_impacts"), sum, na.rm=T
  ) |> ungroup() |>
    mutate(annual_impacts_percap = annual_impacts / pop)
  frediAdjSum |> glimpse()

  #### Map Data -----------------------------------------------------
  # stateGons <- getStatePolygons()
  # stateGons() |> glimpse()
  # stateGons() |> ggplot(aes(long, lat, group = group)) + geom_polygon(fill = "white", colour = "grey50") + coord_quickmap()
  # stateGons() |> ggplot(aes(long, lat, group = group, fill=order)) + geom_polygon(colour = "grey50") + coord_quickmap()

  stateMapData <- frediAdjSum |> addData2Map(join0="state_lc"); stateMapData |> glimpse()

  ### Maps -----------------------------------------------------
  #### Totals -----------------------------------------------------
  stateMapList <- stateMapData |>
    mutate(annual_impacts = (annual_impacts / 1e9)) |>
    mutate(annual_impacts_percap = annual_impacts / 1e9 * 1e5 / pop ) |>
    mapSectorTotals(); stateMapList
  ### Save map
  ggsave(plot=stateMapList, filename=mapTotPath, width=8, height=10)



  #### By sector -----------------------------------------------------
  # frediAdj |> pull(sector) |> unique() |> print()
  ### Get sector map data
  sectorMapsData <- frediAdj |>
    getFilterDf(filters0=c("sectorprimary")) |>
    mutate(annual_impacts_percap = annual_impacts / pop) |>
    addData2Map(join0="state_lc"); sectorMapsData |> glimpse()
  ### Get maps
  sectorMaps <- sectorMapsData |> getSectorMaps()
  ### View map
  # sectorMaps$`CIL Agriculture`
  ### Save maps
  cSectors |> walk(function(sector_i, map_i=sectorMaps[[sector_i]]){
    sector_i |> print()
    ggsave(plot=map_i, filename=mapSectPath |> str_replace("\\*", sector_i), width=8, height=10)
  })

  ###### Return ######
  return(resultsList)
} ### End function
###### End File ######
