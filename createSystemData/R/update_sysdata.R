### Created 2022.02.25
### The purpose of this function is to assist in adding SV objects to FrEDI sysdata.rda
update_sysdata <- function(
  projectPath = getwd(), ### Path to project
  outPath     = file.path(getwd(), "..", "FrEDI", "R"),
  sv            = T, ### Whether to update SV, population, formatting info
  impacts       = F, ### Whether to update impact info
  # impactSectors = NULL,
  rDataExt      = "rda", ### r Object Extension
  silent        = F,  ### Whether to message the user
  save          = F, ### Whether to save
  return        = T  ### Whether to return
){
  paste0("Running createSVData():", "\n") %>% message
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user) and save behavior
  msgUser <- !silent

  ###### Directories ######
  projectPath <- ifelse(is.null(projectPath), ".", projectPath)
  inPath_sv   <- projectPath %>% file.path("data", "sv")
  inPath_imp  <- inPath_sv   %>% file.path("impactsLists")
  outPath_imp <- outPath     %>% file.path("..", "data")

  ###### File Names ######
  ### sysdata.rda
  ### SV demo data
  ### SV pop data
  ### format data
  sysDataName     <- "sysdata"        %>% paste(rDataExt, sep=".")
  # sysDataName2    <- "sysdata2"        %>% paste(rDataExt, sep=".")

  ###### List of Objects ######
  df_sv <- data.frame(
    file   = c("svDataList", "svPopData", "format_styles"),
    object = c("svDataList", "svPopList", "format_styles")
  ) %>%
    mutate(fileName = file %>% paste(rDataExt, sep=".")) %>%
    mutate(filePath = inPath_sv %>% file.path(fileName))

  ###### Import sysdata.rda ######
  sysDataPath   <- outPath %>% file.path(sysDataName)
  # sysDataPath2   <- outPath %>% file.path(sysDataName2)
  sysDataList   <- sysDataPath %>% (function(x){admisc::objRDA(x)})
  sysDataList %>% print
  load(sysDataPath)

  ###### Update sysdata: SV, pop, formatting ######
  if(sv){
    for(i in 1:nrow(df_sv)){
      file_i   <- df_sv$file[i]
      object_i <- df_sv$object[i]
      path_i   <- df_sv$filePath[i]
      ### Load object
      load(path_i)
      ### Add object to list
      sysDataList <- sysDataList %>% c(object_i)
      
      ## Save the results
      if(save){
        paste0("Updating '", file_i, "'...") %>% print
        ### Names
        names_sysdata   <- sysDataList
        pattern_sysdata <- paste(names_sysdata, collapse = "|")
        # eval(substitute(save(list=ls(pattern = x), file=y), list(x=pattern_sysdata, y=sysDataPath2)))
        eval(substitute(save(list=ls(pattern = x), file=y), list(x=pattern_sysdata, y=sysDataPath)))
      }
      # sysDataPath2 %>% (function(x){admisc::objRDA(x)}) %>% print
      sysDataPath %>% (function(x){admisc::objRDA(x)}) %>% print
    }
    # ### Save the results
    # if(save){
    #   paste0("Updating '", file, "'...") %>% print
    #   ### Names
    #   names_sysdata   <- sysDataList
    #   pattern_sysdata <- paste(names_sysdata, collapse = "|")
    #   # eval(substitute(save(list=ls(pattern = x), file=y), list(x=pattern_sysdata, y=sysDataPath2)))
    #   eval(substitute(save(list=ls(pattern = x), file=y), list(x=pattern_sysdata, y=sysDataPath)))
    # }
    # # sysDataPath2 %>% (function(x){admisc::objRDA(x)}) %>% print
    # sysDataPath %>% (function(x){admisc::objRDA(x)}) %>% print
  }


  ###### Update sysdata: impacts ######
  if(impacts){
    impactFileNames <- inPath_imp %>% list.files
    for(i in 1:length(impactFileNames)){
      fileName_i <- impactFileNames[i]
      inPath_i   <- inPath_imp %>% file.path(fileName_i)
      outPath_i  <- inPath_imp %>% file.path(fileName_i)
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



