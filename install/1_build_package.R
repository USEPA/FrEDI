######  1. Build Package ######
### Use this file to uninstall/reinstall the ciraTempBin package
### Before running, run:
### 00. Test Package
###  0. Update Default Results
### After running, run:
###  2. Reinstall Package

###### Load Helper Packages ######
require(tidyverse)
###### Package Source Directory ######
### Relative to documents in the home directory, i.e. "~/Documents/
# package_name      <- "FrEDI"
# package_location  <- getwd(); package_location
# package_sourceDir <- file.path(package_location, "..", package_name)
package_sourceDir <- getwd()

###### Library ######
list.files(package_sourceDir)
list.dirs(package_sourceDir, recursive = F)

###### Update Documentation ######
###### Build Manual
###### Add and build vignettes
###### Generate Documentation
roxygen2::roxygenise(package_sourceDir)
devtools::document(pkg = package_sourceDir)
devtools::build_manual(pkg = package_sourceDir)
devtools::build_vignettes(pkg = package_sourceDir)


###### Build Package ######
###### Build Package but don't include vignettes
devtools::build(pkg=package_sourceDir)

if(!exists("detach_package")){
  ###### Load Function
  detach_path <- package_sourceDir %>% file.path("..", "install", "detach_package.R")
  detach_path %>% source
}

detach_package(ciraTempBin)
