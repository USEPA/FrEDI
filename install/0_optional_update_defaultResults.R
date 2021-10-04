### Run from inside ciraTempBin project directory
###### Load Packages ######
require(tidyverse)
###### Package Source Directory ######
### Relative to documents in the home directory, i.e. "~/Documents/
# package_name      <- "ciraTempBin"
# package_location  <- getwd(); package_location
# package_sourceDir <- file.path(package_location, "..", package_name)
package_sourceDir <- getwd()

###### Library ######
list.files(package_sourceDir)
list.dirs(package_sourceDir, recursive = F)

###### Test and Update Data ######
### Uncomment to generate and test example data
devtools::load_all(package_sourceDir)
defaultResults <- tempBin()

###### Save Default Scenario ######
# savePath <- package_sourceDir %>% file.path("data", "defaultResults.RData")
# save(defaultResults, file=savePath)

devtools::load_all(package_sourceDir)
defaultResults %>% filter(model_type=="SLR") %>% get_plots() %>% (function(x){x$heatmaps$SLR})
###### Save Reference SLR Values ######
# scenPath <- package_sourceDir %>% file.path("inst", "extdata", "scenarios")
# tempPath <- file.path(scenPath, "GCAM_scenario.csv")
# df_temp  <- read.csv(tempPath)
# slrPath  <- scenPath %>% file.path("slr_from_GCAM.csv")
# df_slr   <- temps2slr(temps=df_temp$temp_C_global, years=df_temp$year)
# write.csv(df_slr, file=slrPath, row.names = F)
