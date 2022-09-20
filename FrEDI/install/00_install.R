###### README ######
### This script is intended to help users install the package from source or the GitHub repository.
### It is intended to cover general cases and may not cover all install requirements.
### If installing from source (rather than GitHub):
### - Copy tar.gz file to your R library before running this script
### - You can check the location of your R library by typing '.libPaths()[1]' into the command line
install_fredi <- function(
  installFrom = NULL, ### Select a number, one or two
  package     = "FrEDI",
  version     = "3.3.1",
  libPath     = .libPaths()[1] ### Location of library path
){
  ###### Load Packages ######
  require(devtools)
  require(tidyverse)
  require(withr)

  ###### Install Type Prompt ######
  if(is.null(installFrom)){
    installPrompt <- "Select install option number (1 - from GitHub, 2 - local, from source):"
    installFrom   <- readline(prompt = installPrompt)
  }

  ###### Package Info ######
  package_name      <- package ### Package name
  package_version   <- version ### Package version number
  package_file      <- package_name %>% paste0("_", package_version, ".", "tar.gz")

  ###### Install from GitHub ######
  if(installFrom == "1"){
    ### Option for a different Git repo
    withr::with_libpaths(
      new = .libPaths()[1],
      remotes::install_github(
        repo   = "https://github.com/USEPA/FrEDI",
        subdir = package_name,
        type   = "source",
        repos  = getOption("repos"),
        force  = TRUE
      )
    )
  }
  ###### Install from Source ######
  else if(installFrom == "2"){
    ### Otherwise, install from source
    "Copy tar.gz file to your R library before running this script..." %>% message
    "Check location of your R library by typing '.libPaths()[1]' into the command line..." %>% message

    ###### Package Location
    package_location  <- libPath; #package_location
    package_path      <- package_location %>% file.path(package_name)
    package_filePath  <- package_location %>% file.path(package_file)
    # package_location %>% list.files

    ###### Check Files
    ### Check existance of package file
    package_fileExists <- package_filePath %>% file.exists
    ### Return without doing anything if package doesn't exist
    if(!package_fileExists){
      paste0("Warning: ", package_file, " not found in ", package_location, "...") %>% message
      paste0("\t", "Add", package_file, " to ", package_location, " and then rerun this script.", "\n") %>% message
      paste0("Exiting...") %>% message
      return()
    } else{
      ### If the package exists, try to install the package
      ### First check whether package already installed
      package_exists     <- package_path %>% dir.exists
      ###### Remove Previous Library
      ### If package is already installed, remove the previous install
      ### Then check that it was removed and install the package
      if(package_exists){
        paste0("Warning: ", package, " already exists!") %>% message
        removePrompt <- paste0("Remove existing install? (y/n)")
        removeInput  <- readline(prompt = removePrompt) %>% tolower
        if(removeInput=="y"){
          unlink(package_path, recursive = T)
          install.packages(package_filePath, repos=NULL, type="source", lib=package_location)
        } else{
          paste0("Exiting without installing...") %>% message
          return()
        }
      } else{
        ###### Install the Package
        install.packages(package_filePath, repos=NULL, type="source", lib=package_location)
      }
    }
  }
  ###### Exit without installing ######
  else{
    paste0("Warning: No option selected.", "\n") %>% message
    paste0("Exiting without installing...") %>% message
    return()
  }
}

# install_fredi()

install_fredi(version="3.3.1")

# # ?devtools::install_github()
# # ?with_libpaths
# withr::with_libpaths(
#   new = .libPaths()[1],
#   devtools::install_github(
#     repo   = "https://github.com/USEPA/FrEDI",
#     subdir = "FrEDI",
#     type   = "source",
#     force  = TRUE,
#     ref = "new_labor"
#   )
# )
###### Test Package ######
# testx     <- FrEDI::run_fredi(pv=T)

