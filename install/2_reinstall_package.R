######  2. Reinstall package ######
### Use this file to uninstall/reinstall the ciraTempBin package
### Before running, run:
### 00. Test Package
###  0. Update Default Results
###  1. Build Package

###### Load Helper Packages ######
require(tidyverse)

###### Package Path ######
package_sourceDir <- getwd()

###### Clean Package Install Directory ######
### Package library paths, files, and path to package library
r_libPath       <- .libPaths()[1]; r_libPath
package_name    <- "FrEDI"
package_libPath <- paste(r_libPath, package_name, sep="/"); package_libPath
### Remove library if it is in the path, and then check that it was removed
any(package_name == list.files(r_libPath))
if(any(package_name == list.files(r_libPath))) unlink(package_libPath, recursive = T)
any(package_name == list.files(r_libPath))
### Copy package
file.copy(
  from=paste(package_sourceDir, "..", "FrEDI_2.0.tar.gz", sep="/"),
  to=paste(r_libPath, "FrEDI_2.0.tar.gz", sep="/"),
  overwrite=T)

# ###### Detach the Package ######
# if(!exists("detach_package")){
#   ###### Load Function
#   detach_path <- package_sourceDir %>% file.path("..", "install", "detach_package.R")
#   detach_path %>% source
# }
#
# detach_package(FrEDI)

###### Install the Package ######
install.packages(paste(.libPaths()[1], "FrEDI_2.0.tar.gz", sep="/"), repos=NULL, type="source", lib=.libPaths()[1])

###### Test Package ######
require(FrEDI)
testx <- run_fredi()
plotx <- get_plots(testx)
### Path to example scenarios
scenariosPath <- system.file(package="FrEDI") %>% file.path("extdata","scenarios")
### View example scenario names
scenariosPath %>% list.files
### Temperature Scenario File Name
tempInputFile <- scenariosPath %>% file.path("GCAM_scenario.csv")
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath %>% file.path("slr_from_GCAM.csv")
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath %>% file.path("pop_scenario.csv")
#' ### Import inputs
example_inputsList <- import_inputs(
  tempfile = tempInputFile
# tempfile = tempInputFile,
#  slrfile  = slrInputFile,
# popfile  = popInputFile
)
testx <- run_fredi(inputsList = example_inputsList)
plotx <- get_plots(testx, save=T, directory = package_sourceDir %>% file.path("..", "misc", "test_plots"))
