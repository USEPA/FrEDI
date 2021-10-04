### Last updated: 2021.02.10
### 2021.02.10: Added SLR sectors
### This function loads the data from a directory specified by the user. There is a default directory structure it will try to load from.
loadData <- function(
  fileName,  ### Full file path, including the directory and file name and extension
  sheetName,
  silent    = NULL
){
  # if(is.null(sheetName)){sheetName <- "tableNames"}
  if(is.null(silent   )){silent    <- F}
  ###### Load table with names of data tables ######
  df_tableNames <- openxlsx::read.xlsx(
    fileName,
    sheet    = sheetName,
    rowNames = T,
    startRow = 2
  ) %>%
    filter(Import==1) %>%
    mutate_at(c("excludeCol_ids", "Notes"), replace_na, "")

  ### Number of data tables
  num_dataTables <- df_tableNames %>% nrow

  ###### Load table with names of data tables ######
  ### Iterate over the list of data tables: import them and add them to the list of tables
  ### Initialize the list
  dataList    <- list()
  tableNames  <- df_tableNames$Table.Name

  for(i in 1:num_dataTables){
    tableName_i <- tableNames[i]
    ### Message the user
    if(!silent){message("\t\t", "Importing table '", tableName_i, "' from Excel...")}
    ### Subset table info
    tableInfo_i <- df_tableNames[i,]
    ### Read in the table
    table_i <-
      openxlsx::read.xlsx(
        fileName,
        colNames = T,
        rowNames = T,
        sheet     = tableInfo_i$Worksheet,
        cols      = tableInfo_i$id_colIndex + 0:tableInfo_i$num_tableCols,
        rows      = tableInfo_i$Header.Row + 0:tableInfo_i$Number.of.Data.Rows,
        na.strings = ""
      )

    ### Exclude some columns
    # tableInfo_i$excludeCol_ids %>% print

    if(tableInfo_i$excludeCol_ids!=""){
      ### Columns to exclude
      excludeCols_i <- tableInfo_i$excludeCol_ids %>% str_split(", ") %>% unlist %>% as.numeric
      ### Subtract the row id column if a list of columns to exclude was provided
      table_i <- table_i[,-(excludeCols_i-1)]
    }

    ### Add the table to the list
    dataList[[i]] <- table_i

  } ### End lapply

  ###### Add list names   ######
  ### Add table names to the list
  names(dataList) <- tableNames

  ###### Assign tables to objects   ######
  ### Assign data tables to objects in this namespace
  num_dataTables <- dataList %>% length
  for(i in 1:num_dataTables){
    assign(tableNames[i], dataList[[i]])
  }

  ###### Modify tables and update in list  ######
  ###### Sector info     ######
  ### Filter to those tables to include;
  # co_sectors  <- co_sectors %>% filter(include==1) %>% select(-include)
  co_sectors  <- co_sectors %>% filter(include==1) %>% select(-include)
  dataList[["co_sectors"]] <- co_sectors
  ### Make a copy of the sectors list to include adaptations then drop adaptation column
  co_sectorsRef <- co_sectors
  dataList[["co_sectorsRef"]]
  co_sectors    <- co_sectors %>% select(-adaptations, -impactYears, -impactTypes)

  ###### Adaptation info     ######
  ### No changes to adaptations

  ###### Impact year info     ######
  ### Load data and gather the data
  ### Column names: `N/A`, `2010`, `2090`, Interpolate
  ### Levels for factoring
  co_impactYearLevels <-
    data.frame(impactYear_label = names(co_impactYears %>% select(-sector_id))) %>%
    mutate(
      impactYear_id    = gsub("/", "", impactYear_label),
      impactYear_excel = gsub("NA", "", impactYear_id)
      )

  ### Reshape impact years
  co_impactYears <- co_impactYears %>%
    gather(key="impactYear_label", value="impactYear2", -sector_id) %>%
    filter(!is.na(impactYear2)) %>% select(-impactYear2) %>%
    left_join(
      co_impactYearLevels, by = "impactYear_label"
    ) #; co_impactYears %>% glimpse
  dataList[["co_impactYears"]]      <- co_impactYears
  dataList[["co_impactYearLevels"]] <- co_impactYearLevels

  ###### Impact types info ######
  ### Drop damage adjustment names (present in adaptations)
  co_impactTypes <- co_impactTypes %>% select(-damageAdjName) #; co_impactTypes %>% glimpse
  dataList[["co_impactTypes"]] <- co_impactTypes

  ###### Model types ######
  ### No changes to model types

  ###### Input scenario info ######
  ### No changes to input scenario info

  ###### Models ######
  ### Combine with model types and update data list
  co_models <- co_models %>% left_join(co_modelTypes %>% rename(modelType = modelType_id), by = "modelType") #; co_models %>% glimpse
  dataList[["co_models"]] <- co_models

  ###### Regions list ######
  ### Combine and add to data list...also a copy with info on national data
  co_regions <- co_regions %>% mutate(region=region_dot)
  dataList[["co_regions"]] <- co_regions

  ###### Join sector and other info ######
  ### Add additional sector info and add to data list
  df_sectorsInfo <- co_sectors %>%
    left_join(co_adaptations %>% select(-c("adaptation_id_excel", "adaptation_label")), by="sector_id") %>%
    left_join(co_impactYears %>% select(-c("impactYear_label", "impactYear_excel")), by="sector_id") %>%
    left_join(co_impactTypes %>% select(-c("impactType_id_excel", "impactType_label", "impactType_description")), by="sector_id") %>%
    rename_at(
      paste(c("sector", "adaptation", "impactYear", "impactType"), "id", sep="_"),
      function(x){substr(x, 1,nchar(x) - nchar("_id"))}
    ) #; df_sectorsInfo %>% glimpse
  dataList[["df_sectorsInfo"]] <- df_sectorsInfo
  
  ###### Initialize the temperature scenario ######
  ### Drop temp_C_conus
  dataList[["co_defaultTemps"]] <- co_defaultTemps #%>% select(-c("temp_C_conus"))
  
  ###### Initialize the socioeconomic scenario ######
  ### Gather the columns from the default scenario and update data list
  co_defaultScenario <- co_defaultScenario %>%
    gather(key   ="region", value="reg_pop", -c("year", "gdp_usd")) %>%
    # gather(key   ="region", value="reg_pop", -c("year", "temp_C_conus", "gdp_usd")) %>%
    # rename(temp_C=temp_C_conus) %>%
    mutate(
      region = substr(region, 5, nchar(region))
    ) #; gather_defaultScenario %>% glimpse
  dataList[["co_defaultScenario"]] <- co_defaultScenario

  ###### Format the scaled impacts table ######
  ### Remove empty rows and GCM and SLR Averages
  ### Refactor adaptations, impact estimate years, and impact types
  ### Update data list
  data_scaledImpacts <- data_scaledImpacts %>%
    filter(
      model %in% co_models$model_id, ### All models
      sector %in% co_sectors$sector_id
      ) %>%
    ### Refactor impact years, impact types
    mutate(
      impactYear = impactYear %>% replace_na("N/A"),
      impactType = impactType %>% replace_na("NA")
    ) %>%
    ### Refactor adaptation
    mutate(
      sector_adaptation = sector %>% paste(adaptation, sep="_"),
      sector_adaptation = sector_adaptation %>%
        factor(levels=paste(co_adaptations$sector_id, co_adaptations$adaptation_id_excel, sep="_"),labels=co_adaptations$adaptation_id),
      adaptation=sector_adaptation
      ) %>%
    select(-sector_adaptation) %>%
    ### Refactor impact years
    mutate(
      impactYear= impactYear %>% factor(levels=co_impactYearLevels$impactYear_label, labels=co_impactYearLevels$impactYear_id)
      ) %>%
    ### Refactor impact types
    mutate(
      sector_impactType = sector %>% paste(impactType, sep="_"),
      sector_impactType = sector_impactType %>%
        factor(levels=paste(co_impactTypes$sector_id, co_impactTypes$impactType_id_excel, sep="_"),
               labels=co_impactTypes$impactType_id),
      impactType=sector_impactType) %>%
    select(-sector_impactType) %>%
    ### Refactor model types
    mutate(
      model_dot  = model %>% factor(levels=co_models$model_id, labels=co_models$model_dot),
      model_type = model_dot %>% factor(levels=co_models$model_dot, labels=co_models$modelType)
    )

  ###### SLR Scenario Info ######
  ### Modify slr_cm info
  # slr_cm %>% names %>% print
  slr_cm <- slr_cm %>%
    gather(value="driverValue", key="model", -c("year")) %>%
    mutate(model = gsub("\\_", "", model)) %>%
    mutate(
      model_dot = model,
      model_type = model_dot %>% factor(levels=co_models$model_dot, labels=co_models$modelType)
    )
  # slr_cm %>% names %>% print
  
  dataList[["slr_cm"]] <- slr_cm
  
  # slrImpacts %>% names %>% print
  slrImpacts <- slrImpacts %>%
    gather(value="scaled_impacts", key="region_slr", -c("year", "sector", "adaptation", "impactType", "impactYear")) %>%
    mutate(
      region = region_slr %>% lapply(function(x){str_split(x, "_")[[1]][1]}) %>% unlist,
      model  = region_slr %>% lapply(function(x){str_split(x, "_")[[1]][2]}) %>% unlist %>% paste("cm", sep="")
    ) %>%
    mutate(model = gsub("\\.", "_", model)) %>%
    select(-c("region_slr")) %>%
    ### Refactor impact years, impact types
    mutate(
      impactYear = impactYear %>% replace_na("N/A"),
      impactType = impactType %>% replace_na("NA")
    ) %>%
    ### Refactor adaptation
    mutate(
      sector_adaptation = sector %>% paste(adaptation, sep="_"),
      sector_adaptation = sector_adaptation %>%
        factor(levels=paste(co_adaptations$sector_id, co_adaptations$adaptation_id_excel, sep="_"),labels=co_adaptations$adaptation_id),
      adaptation=sector_adaptation
    ) %>%
    select(-sector_adaptation) %>%
    ### Refactor impact years
    mutate(
      impactYear= impactYear %>% factor(levels=co_impactYearLevels$impactYear_label, labels=co_impactYearLevels$impactYear_id)
    ) %>%
    ### Refactor impact types
    mutate(
      sector_impactType = sector %>% paste(impactType, sep="_"),
      sector_impactType = sector_impactType %>%
        factor(levels=paste(co_impactTypes$sector_id, co_impactTypes$impactType_id_excel, sep="_"),
               labels=co_impactTypes$impactType_id),
      impactType=sector_impactType) %>%
    select(-sector_impactType) %>%
    # mutate(impactYear = "NA") %>%
    # mutate(impactType = impactType %>% replace_na("NA")) %>%
    mutate(
      model_dot = model,
      model_type = model_dot %>% factor(levels=co_models$model_dot, labels=co_models$modelType)
    )
  # slrImpacts %>% names %>% print
  dataList[["slrImpacts"]] <- slrImpacts
  
  ###### Reshape the outputs ######
  ### Reshape the outputs table (move columns with regional values to rows)
  ### Convert values of zero to NA
  data_scaledImpacts <- data_scaledImpacts %>%
    gather(key = "region_dot", value="scaledImpact", all_of(co_regions$region_dot)) %>%
    mutate_at(vars(adaptation, impactYear, impactType, model_dot), as.character)
  dataList[["data_scaledImpacts"]] <- data_scaledImpacts

  ###### Reshape scalars ######
  ### refactor region
  scalarDataframe <- scalarDataframe %>%
    mutate(region = region %>% factor(levels=c(co_regions$region_label, "National Total"), labels=c(co_regions$region_dot, "National.Total")) %>% as.character)

  dataList[["scalarDataframe"]] <- scalarDataframe


  ### Return the list of dataframes
  return(dataList)

}

