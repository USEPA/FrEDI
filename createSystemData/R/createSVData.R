### Created 2022.02.14
### The purpose of this function is to convert Excel and CSV data for the FrEDI SV module into R objects
createSVData <- function(
  projectPath = getwd(), ### Path to project
  # excelName   = NULL, ### name of excel file with config information
  # outPath     = file.path(getwd(), "..", "FrEDI", "R"),
  outPath     = file.path(getwd(), "data", "sv"),
  sv            = T, ### Whether to run demographic info
  pop           = F, ### Whether to run population functions,
  impacts       = F,
  sectors = NULL,
  format        = T, ### Whether to update formatting styles
  # drivers     = F, ### Whether to run driver info
  # rDataExt      = "rda", ### r Object Extension
  rDataExt      = "rds", ### r Object Extension
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

  ###### Create File Paths ######
  projectPath <- ifelse(is.null(projectPath), ".", projectPath)
  ### Excel configuration file
  extDataPath <- projectPath %>% file.path("inst", "extdata", "sv")
  outPath_sv  <- outPath
  outPath_imp <- outPath %>% file.path("impactsLists")
  
  ### SV demo data
  sv_fileName <- "svDataList" %>% paste0(".", rDataExt)
  sv_filePath <- outPath_sv %>% file.path(sv_fileName)
  
  ###### Import Functions from ciraTempBin ######
  calc_countyPop  <- utils::getFromNamespace("calc_countyPop", "FrEDI")
  # outPath     = file.path(getwd(), "..", "FrEDI", "R"),
  # getwd() %>% file.path("..", "FrEDI", "R", "utils_sv.R") %>% source
  
  ###### Initialize Return List ######
  if(return){
    returnList <- list()  
  }
  
  ###### SV Demographic Data ######
  ### Create or load SV demographic data
  if(sv){
    svDataList  <- get_svDataList(
      save    = save, 
      return  = return, 
      outPath = outPath_sv,
      msg0    = "\t"
    )
  } else if(load_demoInfo){ ### Load svDataList
    load(sv_filePath)
  } else{
    svDataList  <- NULL
  }
  
  ###### Population Data ######
  ### Create or load population scenario/projection list
  if(pop){
    svPopList <- get_svPopList(
      svData  = svDataList$svData, 
      save    = save, 
      return  = return, 
      outPath = outPath_sv, 
      msg0    = "\t"
      )
  } else{
    svPopList <- NULL
  }
  
  ###### Formatting ######
  ### For Excel formatting. openxlsx
  df_formatTypes <- svDataList$co_formatTypes
  format_styles  <- df_formatTypes$styleName %>%
    lapply(function(style_i){
      i       <- which(df_formatTypes$styleName == style_i)
      style_i <- createStyle(
        fgFill       = df_formatTypes$fgFill[i],
        halign       = df_formatTypes$halign[i],
        valign       = "center", 
        wrapText     = TRUE,
        border       = df_formatTypes$border[i],
        borderColour = df_formatTypes$borderColour[i],
        fontColour   = df_formatTypes$fontColour[i]
      )
      return(style_i)
    }) %>%
    (function(x){
      names(x) <- df_formatTypes$styleName
      return(x)
    })
  if(save){
    formatFile <- "format_styles" %>% paste0(".", rDataExt)
    formatPath <- outPath_sv %>% file.path(formatFile)
    save(format_styles, file=formatPath)
  }

  ###### Impacts Functions List ######
  if(impacts){
    ### Filter sector info to sectors specified by user
    svSectorInfo <- svDataList$svSectorInfo; 
    
    if(!is.null(sectors)){
      svSectorInfo <- svSectorInfo %>% filter(sector %in% sectors)
    }

    ### Iterate over sectors
    for(i in 1:nrow(svSectorInfo)){
      ### File names
      infileName_i    <- svSectorInfo$inputDataFile[i]
      variant_abbr_i  <- svSectorInfo$variant_abbr[i]; #variant_abbr_i %>% print
      sector_i        <- svSectorInfo$sector[i]
      variant_i       <- svSectorInfo$variant_label[i]
      fileExt_i       <- svSectorInfo$impactList_fileExt[i]
      inFileExt_i     <- "csv"
      # inFileExt_i   <- ifelse(sector == "Coastal Properties", "xlsx", "csv")
      
      infile_i      <- infileName_i %>% 
        paste0(ifelse(is.na(variant_abbr_i), "", " - ")) %>% 
        paste0(ifelse(is.na(variant_abbr_i), "", variant_i)) %>% 
        paste(inFileExt_i, sep=".")
      # (infile_i %in% (excelDataPath %>% list.files)) %>% print
      
      outName_i     <- "impactsList" %>%
        paste(fileExt_i, sep="_") %>%
        paste0(ifelse(is.na(variant_abbr_i), "", "_")) %>%
        paste0(ifelse(is.na(variant_abbr_i), "", variant_abbr_i))
      
      # outfile_i     <- outName_i %>% paste(rDataExt, sep=".")
      outfile_i     <- outName_i %>% paste0(".", rDataExt)
      
      ### SV Data
      if(!is.null(svDataList)){
        if(sector_i == "Coastal Properties"){
          paste0("Using coastal properties data") %>% print
          svInfo <- svDataList$svDataCoastal  
        } else{
          svInfo <- svDataList$svData  
        }
        # svInfo$fips %>% unique %>% head %>% print
      } else{
        svInfo <- NULL
      }
      
      
      ### Create impacts list
      impactsList <- get_svImpactsList(
        dataFile   = infile_i, 
        dataPath   = extDataPath %>% file.path("impacts"),
        outFile    = outName_i,
        svInfo     = svInfo, 
        createList = T, 
        sector     = sector_i,
        save       = save, 
        return     = return, 
        outPath    = outPath_imp, 
        msg0       = "\t"
        # dataFile = infile_i, createList = T, save=F, return = T
      )
      
    } 
  } #; returnList[["impactsList"]] <- impactsList
  else{
    impactsList <- NULL
  }

  
  ###### Return object ######
  message("\n\n", "Finished", ".")
  ### Return svDataList
  if(return){
    returnList[["svDataList"]]    <- svDataList
    returnList[["svPopList"]]     <- svPopList
    returnList[["impactsList"]]   <- impactsList
    returnList[["format_styles"]] <- format_styles
    return(returnList)
  }
  
} ### End function



