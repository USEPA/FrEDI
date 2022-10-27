### Created 2022.02.25
### The purpose of this function is to assist in adding SV objects to FrEDI sysdata.rda
update_sysdata <- function(
  projectPath = getwd(), ### Path to project
  outPath     = file.path(".", "..", "FrEDI", "R"),
  # outPath       = NULL,
  sv            = T, ### Whether to update SV, population, formatting info
  impacts       = F, ### Whether to update impact info
  # impactSectors = NULL,
  # rDataExt      = "rda", ### r Object Extension
  rDataExt      = "rds", ### r Object Extension
  silent        = F,  ### Whether to message the user
  save          = F, ### Whether to save
  return        = T  ### Whether to return
){
  require(tidyverse)
  paste0("Running update_sysdata():", "\n") %>% message
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user) and save behavior
  msgUser <- !silent

  ###### Directories ######
  projectPath <- ifelse(is.null(projectPath), ".", projectPath)
  outPath     <- ifelse(is.null(outPath), system.file(package="FrEDI"), outPath); 
  inPath_sv   <- projectPath %>% file.path("data", "sv")
  inPath_imp  <- inPath_sv   %>% file.path("impactsLists")
  # outPath_imp <- outPath     %>% file.path("..", "data")
  outPath_imp <- outPath     %>% file.path("..", "inst", "extdata", "sv", "impactLists")
  # inPath_imp %>% list.files %>% print; outPath_imp %>% list.files %>% print

  ###### File Names ######
  ### sysdata.rda
  ### SV demo data
  ### SV pop data
  ### format data
  sysDataName     <- "sysdata" %>% paste0(".", rDataExt)

  ###### List of Objects ######
  df_sv <- data.frame(
    file   = c("svDataList", "svPopData", "format_styles"),
    object = c("svDataList", "svPopList", "format_styles")
  )
  ### Add file name and file path
  df_sv <- df_sv %>% mutate(fileName = file %>% paste0(".", rDataExt))
  df_sv <- df_sv %>% mutate(filePath = inPath_sv %>% file.path(fileName))

  ###### Import sysdata.rda ######
  sysDataPath   <- outPath %>% file.path(sysDataName)
  # sysDataList %>% print
  

  ###### Update file paths
  ### Check file exists
  new_ext       <- ifelse(tolower(rDataExt) == "rds", "rda", "rds")
  nChar_ext     <- rDataExt %>% nchar
  checkFile     <- sysDataPath %>% file.exists()
  if(!checkFile){
    sysDataPath   <- sysDataPath %>% (function(j, nChar0=nChar_ext){substr(j, start=1, stop = nchar(j) - nChar0)}) %>% paste0(new_ext)
  }
  sysDataList   <- sysDataPath %>% (function(x){admisc::objRDA(x)})
  load(sysDataPath)
  
  ###### Update sysdata: SV, pop, formatting ######
  if(sv){
    for(i in 1:nrow(df_sv)){
      file_i   <- df_sv$file[i]
      object_i <- df_sv$object[i]
      path_i   <- df_sv$filePath[i]
      ### Check that file exists
      checkFile   <- path_i %>% file.exists()
      if(!checkFile){
        paste0("File '", file_i, "' does not exist...") %>% print
        paste0("Looking for file '", file_i, "' instead...") %>% print
        ### Update file names
        # file_i   <- file_i %>% (function(j, nChar0=nChar_ext){substr(j, start=1, stop = nchar(j) - nChar0)}) %>% paste0(new_ext)
        path_i   <- path_i %>% (function(j, nChar0=nChar_ext){substr(j, start=1, stop = nchar(j) - nChar0)}) %>% paste0(new_ext)
      }
      ### Load object
      load(path_i)
      ### Add object to list
      sysDataList <- sysDataList %>% c(object_i)
      
      ## Save the results
      if(save){
        paste0("Updating '", file_i, "'...") %>% print
          
        ### Object Names
        names_sysdata   <- sysDataList
        pattern_sysdata <- paste(names_sysdata, collapse = "|")
        eval(substitute(save(list=ls(pattern = x), file=y), list(x=pattern_sysdata, y=sysDataPath)))
      }
      sysDataPath %>% (function(x){admisc::objRDA(x)}) %>% print
    }
  }


  ###### Update sysdata: impacts ######
  # impacts %>% print
  if(impacts){
    impactFileNames <- inPath_imp %>% list.files
    # impactFileNames %>% print
    for(i in 1:length(impactFileNames)){
      fileName_i <- impactFileNames[i]
      inPath_i   <- inPath_imp  %>% file.path(fileName_i)
      outPath_i  <- outPath_imp %>% file.path(fileName_i)
      file.copy(
        from      = inPath_i,
        to        = outPath_i,
        overwrite = ifelse(save, T, F),
        copy.date = T
      )
    }
  }

  ###### Return object ######
  message("\n", "Finished", ".")
  # return()

} ### End function



