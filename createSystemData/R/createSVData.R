### Created 2022.02.14
### The purpose of this function is to convert Excel and CSV data for the FrEDI SV module into R objects
createSVData <- function(
  projectPath = getwd(), ### Path to project
  # excelName   = NULL, ### name of excel file with config information
  outPath     = file.path(getwd(), "data"),
  save        = NULL,
  impacts     = F,
  sectorsList = NULL,
  demoinfo    = F, ### Whether to run demographic info
  pop         = F, ### Whether to run population functions
  # drivers     = F, ### Whether to run driver info
  silent      = NULL,  ### Whether to message the user
  save           = F, ### Whether to save
  return         = T  ### Whether to return
){
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user) and save behavior
  silent  <- ifelse(is.null(silent), F, silent)
  msgUser <- !silent
  save    <- ifelse(is.null(save), F, save)
  
  ### Conditions
  load_demoInfo <- (pop | impacts) ### Load `demoinfo`` if pop or `impacts`
  # load_popInfo  <- (pop | impacts) ### Load `demoinfo`` if pop or `impacts`

  ###### Create File Paths ######
  projectPath <- ifelse(is.null(projectPath), ".", projectPath)
  ### Excel configuration file
  extDataPath <- file.path(projectPath, "inst", "extdata", 'sv')
  
  
  ### Output file
  sysDataPath <- projectPath %>% file.path("data")
  sysDataFile <- sysDataPath %>% file.path("sysdata.rdata")
  sysDataFile <- ifelse(!is.null(outPath), outPath, sysDataFile)
  
  ###### Configuration Data ######
  # ### Read in configuration data
  # ### Assign data tables to objects in the list
  # configFile <- projectPath %>% file.path("R", "fredi_config.R")
  # source(configFile)
  # for(i in 1:length(fredi_config)){
  #   assign(names(fredi_config)[i], fredi_config[[i]])
  # }
  
  ###### Import Functions from ciraTempBin ######
  interpolate_annual  <- utils::getFromNamespace("calc_countyPop", "FrEDI")
  
  ###### Initialize Return List ######
  returnList <- list()
  
  ###### SV Demographic Data ######
  ### Create or load SV demographic data
  if(demo){
    get_svDataList(save = save, return = return)
  } else if(load_demoinfo){
    sv_fileName <- "svDataList.rda"
    sv_filePath <- file.path(outPath, sv_fileName)
    load(sv_filePath)
  } else{
    svDataList <- NULL
  }; returnList[["svDataList"]] <- svDataList
  ### Assign list elements to list names if present
  if(!is.null("svDataList")){
    for(name_i in 1:names(svDataList)){
      assign(name_i, svDataList[[name_i]])
    }; rm("svDataList")
  }
  
  ###### Population Data ######
  ### Create or load population scenario/projection list
  if(pop){
    svPopList <- get_svPopList(svData = svData, save = save, return = return)
  } else{
    svPopList <- NULL
  }; returnList[["svPopList"]] <- svPopList
  
  ###### Impacts Functions List ######
  # codePath %>% file.path(paste("get_svImpactsList", "R", sep=".")) %>% source
  # c_svSectors <- svSectorInfo$sector %>% unique
  if(impacts){
    ### Filter sector info to sectors specified by user
    if(!is.null(sectorsList)){
      svSectorInfo <- svSectorInfo %>% filter(sector %in% sectorsList)
    }
    ### Iterate over sectors
    for(sector_i in 1:nrow(svSectorInfo)){
      ### File names
      infileName_i  <- df_sectorInfo$inputDataFile[i]
      adapt_abbr_i  <- df_sectorInfo$adapt_abbr[i]
      sector_i      <- df_sectorInfo$sector[i]
      adapt_i       <- df_sectorInfo$adapt_label[i]
      infile_i      <- infileName_i %>% 
        paste0(ifelse(is.na(adapt_abbr_i), "", " - ")) %>% 
        paste0(ifelse(is.na(adapt_abbr_i), "", adapt_abbr_i)) %>% 
        paste("csv", sep=".")
      # (infile_i %in% (excelDataPath %>% list.files)) %>% print
      
      outfile_i     <- "impactsList" %>%
        paste(impactList_fileExt[i], sep="_") %>%
        paste0(ifelse(is.na(adapt_abbr_i)), "", "adapt_abbr") %>% paste("rda", sep=".")
      
      ### Create impacts list
      impactsList <- get_svImpactsList(
        dataFile = infile_i, createList = T, save=save, return = return
      )
      ### Impacts list name
      # impactsList %>% names %>% length
      ### Save impacts list
      if(save){
        outfilePath_i <- file.path(dataPath, outfile_i)
        outfileDir_i  <- outfilePath_i %>% dirname
        dir_i_exists  <- outfileDir_i %>% dir.exists
        
        if(!dir_i_exists){
          paste0("\t", "`outfileDir_i='", outfileDir_i, "' not found...") %>% message
          paste0("\t", "Exiting without saving...") %>% message
        } else{
          paste0("\t", "Saving impacts list for ", sector_i, ", ", adapt_i, " to '", outfile_i, "'...") %>% message
          save(impactsList, file = outfilePath_i)  
        }
      } ### End if save
      # rm("impactsList")
    } else{
      impactsList <- NULL
    }
    # ### Remove impacts list and function
    # rm("impactsList", "get_svImpactsList")
  }; returnList[["impactsList"]] <- impactsList
  
  ###### Return object ######
  message("\n\n", "Finished", ".")

  if(return){
    return(rDataList)
  }
  
} ### End function


### Uncomment following two lines to create and save data and check the outputs
# test_systemData <- createSystemData(save=F)
# rm("createSystemData")


