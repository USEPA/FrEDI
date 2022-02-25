### Created 2022.02.25
### The purpose of this function is to assist in adding SV objects to FrEDI sysdata.rda
update_sysdata <- function(
  projectPath = getwd(), ### Path to project
  outPath     = file.path(getwd(), "..", "FrEDI", "R"),
  sv            = T, ### Whether to update SV, population, formatting info
  impacts       = F, ### Whether to update impact info
  # impactSectors = NULL,
  rDataExt      = ".rda", ### r Object Extension
  silent        = NULL,  ### Whether to message the user
  save          = F, ### Whether to save
  return        = T  ### Whether to return
){
  paste0("Running createSVData():", "\n") %>% message
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user) and save behavior
  silent  <- ifelse(is.null(silent), F, silent)
  msgUser <- !silent
  save    <- ifelse(is.null(save), F, save)

  ### Conditions
  load_demoInfo <- (pop | impacts | format) ### Load `demoinfo`` if pop or `impacts`

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

  # sv_fileName     <- "svDataList"     %>% paste(rDataExt, sep=".")
  # pop_fileName    <- "svPopData"      %>% paste(rDataExt, sep=".")
  # format_fileName <- "format_styles"  %>% paste(rDataExt, sep=".")
  df_sv <- data.frame(
    object = c("svDataList", "svPopData", "format_styles")
  ) %>%
    mutate(fileName = object %>% paste(rDataExt, sep=".")) %>%
    mutate(filePath = inPath_sv %>% file.path(fileName))

  ###### Create Full File Paths ######
  # sysDataPath     <- outPath %>% sysDataName
  # sv_filePath     <- inPath_sv %>% file.path(sv_fileName)
  # pop_filePath    <- inPath_sv %>% file.path(sv_fileName)
  # format_filePath <- inPath_sv %>% file.path(sv_fileName)

  ###### Import sysdata.rda ######
  sysDataPath   <- outPath %>% sysDataName
  sysDataList   <- sysDataPath %>% list.rda()

  ###### Update sysdata: SV, pop, formatting ######
  if(sv){
    for(i in 1:nrow(df_sv)){
      object_i <- df_sv$object[i]
      path_i   <- df_sv$filePath[i]
      ### Load object
      load(path_i)
      ### Add object to list
      sysDataList[[object_i]] <- eval(parse(text=object_i))
    }
    ### Save the results
    if(save){
      ### Names
      names_sysdata   <- sysDataList %>% names
      pattern_sysdata <- paste(names_sysdata, collapse = "|")
      # save(list=ls(pattern = pattern_sysdata), file=path_sysdata)
      eval(substitute(save(list=ls(pattern = x), file=y), list(x=pattern_sysdata, y=path_sysdata)))
    }
  }

  ###### Update sysdata: impacts ######
  if(impacts){
    impactFileNames <- inPath_imp %>% list.files
    for(i in 1:length()){
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
  message("\n\n", "Finished", ".")
  return()

} ### End function



