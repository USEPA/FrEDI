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

# ###### Test Functions ######
# ### Uncomment to generate and test example data
# load_all(package_location)
# # testx <- run_fredi(aggLevels = "none")
# # testx <- run_fredi(aggLevels = "none", sectorList = "Agriculture") %>% filter(year %in% seq(2000, 2090, by=5))
# # ### Uncomment to update and save default results
# # ###### Update and Save Default Scenario ######
# # rm("testx")
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

###### Copy New Package ######
### Copy package
packageFile_dir    <- package_location %>% file.path( "..")
packageFile_name   <- "FrEDI_2.2.0.tar.gz"
packageFile_path   <- packageFile_dir %>% file.path(packageFile_name)
packageFile_exists <- packageFile_path %>% file.exists

r_libPath       <- .libPaths()[1]; r_libPath
package_name    <- "FrEDI"
package_libPath <- paste(r_libPath, package_name, sep="/"); package_libPath
### Package destination
packageDest        <- r_libPath %>% file.path(packageFile_name)
if(packageFile_exists){
  ###### Remove Previous Library ######
  ### Remove library if it is in the path, and then check that it was removed
  package_exists <- any(package_name == list.files(r_libPath))
  if(package_exists){
    unlink(package_libPath, recursive = T)
  }; any(package_name == list.files(r_libPath))

  ### Copy the file
  file.copy(from = packageFile_path, to = packageDest, overwrite = T)

  ###### Install the Package ######
  install.packages(packageDest, repos=NULL, type="source", lib=r_libPath)

}

# ?devtools::install_github()
# ?with_libpaths
withr::with_libpaths(
  new = .libPaths()[1],
  devtools::install_github(
    repo   = "https://github.com/USEPA/FrEDI",
    subdir = "FrEDI",
    type   = "source",
    force  = TRUE
    # ref = "main"
  )
)

###### Test Package ######
###### - Build package, reinstall, and restart R
###### - Build Package but don't include vignettes
# testx <- run_fredi()
require(FrEDI)
testx     <- run_fredi(pv=T)

