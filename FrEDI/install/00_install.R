###### Load Packages ######
require(devtools)
require(tidyverse)
###### Package Source Directory ######
### Open FrEDI.RProj before running this script
package_name      <- "FrEDI"
package_location  <- getwd(); package_location
package_location %>% list.files

###### Library ######
list.files(package_location)
list.dirs(package_location, recursive = F)

###### Test Functions ######
### Uncomment to generate and test example data
load_all(package_location)
testx <- run_fredi()

# ### Uncomment to update and save default results
# ###### Update and Save Default Scenario ######
# rm("testx")
# defaultResults <- run_fredi()
# savePath <- package_location %>% file.path("data", "defaultResults.RData")
# save(defaultResults, file=savePath)

# ### Uncomment to update documentation
###### Update Documentation ######
###### - Build Manual
###### - Add and build vignettes
###### - Generate Documentation
roxygen2::roxygenise(package_location)
devtools::document(pkg = package_location)
devtools::build_manual(pkg = package_location)
devtools::build_vignettes(pkg = package_location)

# ### Uncomment to build package
###### Build Package ######
###### - Build Package but don't include vignettes
devtools::build(pkg=package_location)

###### Test Package ######
###### - Build package, reinstall, and restart R
###### - Build Package but don't include vignettes
# testx <- run_fredi()
require(FrEDI)
testx     <- run_fredi(pv=T)
