###### Overview ######
### This file contains helper functions for the FrEDI SV module.
###### get_svDataList ######
get_svDataList <- function(
  dataPath       = file.path(getwd(), "inst", "extdata", "sv"), ### Path to data file...default relative to current working directory
  dataFile       = "SVdemographic.xlsx", ### Data file name...defaults to "SVdemographic.xlsx"
  tableSheet     = "tableNames", ### Defaults to "svDemoData"
  tableName      = tableSheet, ### Defaults to `dataSheet`` value
  outPath        = file.path(getwd(), "R"), ### Where to save rdata objects
  saveFile       = "svDataList",
  rDataExt       = "rda", ### R data extension
  save           = F,
  return         = T,
  msg0           = ""
){
  ###### Libraries ######
  # require(tidyverse)
  # require(openxlsx)
  msg0 %>% paste0("Running get_svDataList():", "\n") %>% message
  msg1 <- msg0 %>% paste0("\t")
  ###### File paths ######
  ### File path. Split into standard directory and file name if not standard
  dataFilePath   <- dataPath %>% file.path(dataFile)
  dataFile       <- dataFilePath %>% basename
  dataPath       <- dataFilePath %>% dirname
  
  ### Check if file exists and if it doesn't, exit
  dataPathFiles  <- dataPath %>% list.files
  dataFileExists <- dataFile %in% dataPathFiles
  if(!dataFileExists){
    msg1 %>% paste0("File `dataFile='", dataFile, "'` not found in `dataPath='", dataPath, "'`") %>% message
    msg1 %>% paste0("\n\t", "Exiting...") %>% message
    return()
  }

  ### Otherwise, get information about the file
  ### Check that `dataInfoSheet` and `dataSheet` exists:
  dataFileSheets   <- dataFilePath %>% getSheetNames
  tableSheetExists <- tableSheet %in% dataFileSheets
  if(!tableSheetExists){
    msg1 %>% paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
    msg1 %>% paste0("`tableSheet='", tableSheet, "'` not found in `dataFile='", dataFile, "'`") %>% message
    msg1 %>% paste0("\n\t", "Exiting...") %>% message
    return()
  } ### End if !dataInfoSheetExists

  ###### Load the Excel Workbook ######
  wb_data          <- dataFilePath %>% loadWorkbook
  c_tables         <- wb_data %>% getTables(sheet = tableSheet); rm("wb_data")
  tableExists      <- tableName %in% c_tables
  which_table      <- (tableName == c_tables) %>% which

  ###### Get the table of tables ######
  ### If the files exist then get info about the tables
  ### - Load the Excel Workbook
  ### - Get table names on `dataInfoSheet` and remove the Excel workbook to free up space
  ### - Check that `dataInfoName` is one of the tables on the worksheet
  ### - If it isn't, exit
  msg1 %>% paste0("Loading list of tables from `tableSheet='", tableSheet, "'`...") %>% message
  if(!tableExists){
    msg1 %>% paste0("`tableName='", tableName, "'` not found on `tableSheet='", tableSheet, "'`") %>% message
    msg1 %>% paste0("\n\t", "Exiting...") %>% message
  }
  
  ### If it exists, then load the data:
  ### - Figure out which of the tables is the info table
  ### - Get the range for the table (name in the character vector), then split it and extract from the resulting list
  ### - Extract the column letters from the range and then convert to numbers
  ### - Extract the rows from the range
  range_table <- names(c_tables)[which_table] %>% str_split(":") %>% unlist
  cols_table  <- range_table %>% str_extract("([:alpha:]*)")
  rows_table  <- 1:2 %>% lapply(function(j){
      col_j <- cols_table[j]
      row_j <- gsub(col_j, "", range_table[j])
      return(row_j)
  }) %>% unlist %>% as.numeric
  ### Convert columns to numeric
  cols_table <- cols_table %>% convertFromExcelRef
  ### Finally, read in the table
  # c(cols_infoTable %>% paste(collapse=", "), rows_infoTable %>% paste(collapse=", ") %>% print
  tableNames <- dataFilePath %>% read.xlsx(
    sheet = tableSheet,
    rows  = rows_table[1]:rows_table[2],
    cols  = cols_table[1]:cols_table[2],
    rowNames = T
  ) %>%
    select(-c("id_col", "Notes"))
  
  
  ###### Load the tables ######
  ### Load tables and assign to list
  svDataList <- list()
  for(i in 1:nrow(tableNames)){
    name_i  <- tableNames$Table.Name[i]
    sheet_i <- tableNames$Worksheet[i]
    row1_i  <- tableNames$header_row[i]
    rows_i  <- row1_i + (0:tableNames$num_rows[i])
    cols_i  <- tableNames$first_col[i]:tableNames$last_col[i]
    
    ### Read in table
    table_i <- dataFilePath %>% read.xlsx(
      sheet = sheet_i,
      rows  = rows_i,
      cols  = cols_i,
      rowNames = F
    )
    ### Exclude columns
    exclude_i <- tableNames$excludeCol_ids[i]
    if(!is.na(exclude_i)){
      exclude_i <- paste0("c(", exclude_i, ")")
      exclude_i <- eval(parse(text=exclude_i))
      table_i   <- table_i[,-c(exclude_i)]
    }
    ### Add table to list
    name_i      <- tableNames$rTableName[i]
    svDataList[[name_i]] <- table_i
  }; rm("name_i", "sheet_i", "row1_i", "rows_i", "cols_i", "table_i", "exclude_i")

  ###### SV Group Types ######
  ### Group types, weight columns, sector-adaptation info
  c_svGroupTypes <- (svDataList[["svDemoInfo"]] %>% filter(colType %in% c("sv", "minority")))$colName
  c_svWeightCols <- (svDataList[["svDemoInfo"]] %>% filter(colType == "weight"))$colName
  svSectorInfo   <-  svDataList[["sectorInfo"]] %>% left_join(svDataList[["coAdapt"]], by = "sector_abbr")
  
  svDataList[["c_svGroupTypes"]] <- c_svGroupTypes
  svDataList[["c_svWeightCols"]] <- c_svWeightCols
  svDataList[["svSectorInfo"  ]] <- svSectorInfo
  # rm("c_svGroupTypes", "c_svWeightCols", "svSectorInfo")


  ###### Format GCAM Scenarios ######
  msg1 %>% paste0("Formatting GCAM scenarios...") %>% message
  gcamScenarios <- svDataList$gcamScenarios %>%
    gather(
      key = "scenario", value = "temp_C_conus", -c("year")
    ) %>%
    ### Annual temperature scenarios
    (function(x){
      c_scenarios <- x$scenario %>% unique
      c_range     <- x$year %>% range
      c_years     <- seq(c_range[1], c_range[2], by = 1)
      # c_range %>% print; c_years %>% print
      
      x_new       <- c_scenarios %>% lapply(function(scen_i){
        x_scen_i  <- x %>% filter(scenario==scen_i)
        x_i       <- x_scen_i$year
        y_i       <- x_scen_i$temp_C_conus
        ### Get annual values
        ### Calculate global values
        ### Calculate sea level rise
        df_scen_i <- x_i %>% approx(y = y_i, xout = c_years) %>% as.data.frame %>%
          rename(year = x, temp_C_conus = y) %>%
          mutate(temp_C_global = temp_C_conus %>% (function(y){FrEDI::convertTemps(y, from = "conus")})) %>%
          (function(y){
            y_slr <- FrEDI::temps2slr(temps = y$temp_C_global, years = y$year)
            y     <- y %>% left_join(y_slr, by = "year")
            return(y)
          }) %>%
          mutate(scenario = scen_i)
        return(df_scen_i)
      }) %>%
        (function(y){
          do.call(rbind, y)
        })
    })
  svDataList[["gcamScenarios"  ]] <- gcamScenarios; rm("gcamScenarios")

  ###### Format the SV Data ######
  msg1 %>% paste0("Formatting SV data...") %>% message
  c_svDataTables <- c("svData", "svDataCoastal")

  for(name_i in c_svDataTables){
    msg1 %>% paste0("\t", "Formatting ", name_i ,"...") %>% message
    # name_i %>% print
    table_i    <- svDataList[[name_i]] #; names(table_i) %>% print
    info_i     <- svDataList[["svDemoInfo"]]; #info_i %>% names %>% print
    orderCol_i <- ifelse(name_i=="svData", "colOrder_excel", "colOrder_coastal")
    
    ### Exclude missing columns
    order_i    <- info_i[,orderCol_i] %>% as.vector
    which_i    <- (!is.na(order_i)) %>% which
    info_i     <- info_i[which_i,]
    
    ### Order columns
    order_i    <- order_i[which_i]
    info_i     <- info_i[order_i,]
    nchar_i    <- ifelse(name_i=="svData", 10, 11)
    # info_i$colName %>% print
    names(table_i) <- info_i$colName
    
    ### Standardize formatting
    table_i <- table_i    %>%
      ### Remove "County", "Parish", and "city"
      rename(svCounty = county) %>%
      mutate(
        county   = svCounty %>% (function(x){gsub(" County", "", x)}),
        county   = county   %>% (function(x){gsub(" Parish", "", x)}),
        county   = county   %>% (function(x){gsub(" city", "", x)})
      ) %>%
      ### Standardize county/FIPS number
      mutate(fips_num = fips %>% as.numeric) %>%
      mutate(fips     = fips %>% as.character) %>%
      mutate(
        # nChars     = fips %>% nchar,
        # geoid10    = fips %>% substr(1, ifelse(nChars > 10, 5, 4))
        # geoid10    = fips %>% substr(1, ifelse(nChars > nchar_i, 5, 4))
        nChars     = fips %>% nchar,
        minChar    = nChars %>% min(na.rm=T),
        fips       = paste0(ifelse(nChars > minChar, "", "0"), fips),
        geoid10    = fips %>% substr(1, 5)
      ) %>%
      select(-c("nChars", "fips_num", "minChar"))
    
    
    ### Replace NA, NaN values with 0
    # cols_i  <- (info_i %>% filter(colType=="minority"))$colName #; cols_i %>% print
    cols_i  <- c(c_svGroupTypes, c_svWeightCols)
    cols_i  <- cols_i[cols_i %in% names(table_i)]
    table_i <- table_i    %>%
      mutate_at(.vars=all_of(cols_i), as.numeric) %>%
      mutate_at(.vars=all_of(cols_i), replace_na, 0) %>%
      filter_at(.vars=all_of(cols_i), ~ (!is.na(.x)))
    
    ###### Summarize the County Data ######
    ### Calculate county population
    # "got here" %>% print
    countyPop_i <- table_i %>%
      group_by_at(.vars=c("state", "geoid10")) %>%
      summarize_at(.vars=c("tract_pop"), sum, na.rm=T) %>%
      rename(county_pop = tract_pop)
    
    ###### Join County Data with Main Data######
    ### Drop county and state pop
    table_i <- table_i %>%
      # rename(iclusCounty=county) %>%
      left_join(countyPop_i, by = c("state", "geoid10")) %>%
      mutate(ratioTract2CountyPop = tract_pop / county_pop) %>%
      select(-c("county_pop", "nca_abbr", "tract_pop")) 
    
    ###### Update List ######
    svDataList[[name_i]]    <- table_i
  }; rm("table_i", "info_i", "countyPop_i")
  
  ###### Save the Data ######
  if(save){
    msg1 %>% paste0("Saving data to `outPath='", outPath, "'`...") %>% message
    ### File names
    saveFileName  <- "svDataList" %>% paste(rDataExt, sep=".")
    saveFilePath  <- outPath %>% file.path(saveFileName)
    ### Save table info
    save(svDataList, file=saveFilePath)
  }
  
  if(return){
    msg1 %>% paste0("Finished.", "\n") %>% message
    return(svDataList)
  } else{
    msg1 %>% paste0("Finished.", "\n") %>% message
  }
}



###### get_svPopList ######
get_svPopList <- function(
  dataFile   = "ICLUS_v2_UN_Probabilistic_Median",
  dataPath   = file.path(getwd(), "inst", "extdata", "sv"),
  dataSheet  = "Output",
  dataTable  = "iclusData",
  infoSheet  = "Controls",
  infoTable  = "iclusDataInfo",
  # svDataList = NULL,
  svData     = NULL,
  outFile    = "svPopData",
  outPath    = file.path(getwd(), "R"),
  rDataExt   = "rda", ### R data extension
  save       = F,
  return     = T,
  msg0       = ""
){
  msg0 %>% paste0("Running get_svPopList():", "\n") %>% message
  msg1 <- msg0 %>% paste0("\t")
  ###### Constants ######
  c_intYears <- seq(2000, 2090, by=5) ### years for interpolating
  ###### Check for data ######
  ### Check if there is a data file and if not, exit
  if(is.null(dataFile)){
    msg1 %>% paste0("Argument `dataFile=NULL`. Please provide a valid value to argument `dataFile`.") %>% message
    msg1 %>% paste0("\n", "Exiting...") %>% message
    return()
  }
  ### Check if there the data file exists and if not, exit
  dataFile       <- paste(dataFile, "xlsx", sep=".")
  dataFilePath   <- dataPath %>% file.path(dataFile)
  dataPathFiles  <- dataPath %>% list.files
  dataFileExists <- dataFile %in% dataPathFiles
  if(!dataFileExists){
    msg1 %>% paste0("File `dataFile=", dataFile, "` not found in `dataPath='", dataPath,"'`.") %>% message
    msg1 %>% paste0("\n", "Exiting...") %>% message
  }

  ### Otherwise, get information about the file:
  ### - Create a dataframe with sheet/table info
  ### - Iterate over the tables
  dataFileSheets <- dataFilePath %>% getSheetNames
  x_sv_tables <- c(dataTable, infoTable)
  x_sv_sheets <- c(dataSheet, infoSheet)
  x_sv_df     <- data.frame(
    table = x_sv_tables,
    sheet = x_sv_sheets
  )
  # "got here" %>% print
  ### Load workbook
  wb_data     <- dataFilePath %>% loadWorkbook
  for(i in 1:length(x_sv_sheets)){
    sheet_i <-  x_sv_df$sheet[i]
    which_i <- (x_sv_df$sheet == sheet_i) %>% which
    ### Check that sheet_i exists:
    sheet_i_exists <- sheet_i %in% dataFileSheets
    if(!sheet_i_exists){
      msg1 %>% paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
      msg1 %>% paste0("\t", "`sheet='", sheet_i, "'` not found in `dataFile='", dataFile, "'`") %>% message
      msg1 %>% paste0("\n\t", "Exiting...") %>% message
      return()
    } ### End if !infoSheetExists

    msg1 %>% paste0("Loading data from `sheet='", sheet_i, "'`...") %>% message
    ### If the files exist then get info about the tables
    ### - Load the Excel Workbook, get table names and remove the Excel workbook to free up space
    ### - Check that `dataInfoName` is one of the tables on the worksheet
    ### - If it isn't, exit
    dataTables_i <- wb_data %>% getTables(sheet = sheet_i)
    
    ### Iterate over tables
    tables_i <- (x_sv_df %>% filter(sheet == sheet_i))$table
    for(j in 1:length(tables_i)){
        table_j       <- tables_i[j]
        tableExists_j <- table_j %in% dataTables_i

        ### Check if table exists
        if(!tableExists_j){
          # msg1 %>% paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
          msg1 %>% paste0("Data table='", table_j, "' not found on sheet_i='", sheet_i, "'`") %>% message
          msg1 %>% paste0("\n\t", "Exiting...") %>% message
          return()
        } ### End if !dataSheetExists

        msg1 %>% paste0("\t", "Loading data from `table='", table_j, "'`...") %>% message
        ### If it exists, then load the data:
        ### - Figure out which of the tables is the info table
        ### - Get the range for the table (name in the character vector), then split it and extract from the resulting list
        ### - Extract the column letters from the range and then convert to numbers
        ### - Extract the rows from the range
        which_j <- (table_j == dataTables_i) %>% which
        range_j <- names(dataTables_i)[which_j] %>% str_split(":") %>% unlist
        cols_j  <- range_j %>% str_extract("([:alpha:]*)")
        rows_j  <- 1:2 %>% lapply(function(k){
          col_k <- cols_j[k]
          row_k <- gsub(col_k, "", range_j[k])
          return(row_k)
        }) %>% unlist %>% as.numeric
        ### Convert columns to numeric
        cols_j <- cols_j %>% convertFromExcelRef
        ### Finally, read in the table
        # cols_infoTable %>% print
        # rows_infoTable %>% print
        df_table_j <- dataFilePath %>% read.xlsx(
          sheet = sheet_i,
          rows  = rows_j[1]:rows_j[2],
          cols  = cols_j[1]:cols_j[2],
          rowNames = T
        )
        assign(table_j, df_table_j)
        # dataInfoTable %>% names %>% print
    }
  }; rm("wb_data")
  # "got here" %>% print
  
  ###### Load Data ######
  iclusDataInfo <- iclusDataInfo %>%
    rename(iclusDataType = colType) %>%
    select(-c("colName_excel", "baseYear", "colOrder_excel"))

  # "got here" %>% print
  iclusData <- iclusData %>%
    rename_all(tolower) %>%
    gather(
      key   = "colName", value = "iclus_pop",
      -c("geoid10", "fips", "iclusgeoid", "couname", "stname", "prop", "name")
    ) %>%
    left_join(iclusDataInfo, by = "colName") %>%
    select(-c("colName")) %>%
    ### Rename counties
    rename(state       = stname) %>%
    rename(iclusCounty = couname) %>%
    mutate(county      = iclusCounty %>% (function(x){gsub("Ã±", "ñ", x)})) %>%
    rename(county_pop  = iclus_pop) %>%
    filter(year>=2000) %>%
    # mutate(geoid10 = geoid10 %>% as.character)
    ### Standardize geoid
    # (iclusData$iclusCounty == "") %>% which %>% length %>% print
    mutate(
      nChars     = geoid10 %>% nchar,
      minChar    = nChars %>% min(na.rm=T),
      geoid10    = paste0(ifelse(nChars > minChar, "", "0"), geoid10),
    ) %>%
    select(-c("nChars", "minChar", "iclusCounty"))
    
  ###### Check Against SV Data ######
  msg1 %>% paste0("Checking data against SV data...") %>% message
  ### Load sv data  
  if(is.null(svData)){
    df_sv_path <- outPath %>% file.path("svDataList.rda")
    load(df_sv_path)
    svData    <- svDataList[["svData"]]; rm("svDataList")
  }
  ### Unique regions
  x_regions <- svData$region %>% unique

  ### Join with SV data
  c_ids1      <- iclusData$geoid10 %>% unique %>% length
  iclusData   <- iclusData %>%
    left_join(
      svData %>%
        # group_by_at(.vars=c("region", "state", "county")) %>%
        group_by_at(.vars=c("region", "state", "geoid10")) %>%
        summarise(n=n(), .groups="keep") %>%
        select(-c("n")),
      by = c("state", "geoid10")
      # by = c("state", "county")
    ) %>%
    ### Filter to relevant states, regions, tracts
    filter(!is.na(region)) %>%
    filter(!is.na(state)) %>%
    filter(region %in% x_regions)
  c_ids2 <- iclusData$geoid10 %>% unique %>% length
  # c_ids2 <- iclusData$county %>% unique %>% length
  msg1 %>% paste0("\t", "Removing observations for ", c_ids1 - c_ids2, " counties with missing information...") %>% message
  
  ###### Regional population summary ######
  ### Regional population summary (160 rows)
  msg1 %>% paste0("Creating population projection functions...") %>% message
  msg1 %>% paste0("\t", "Calculating regional population...") %>% message
  iclus_region_pop <-
    iclusData %>%
    group_by_at(.vars=c("region", "year")) %>%
    summarise_at(.vars=c("county_pop"), sum, na.rm=T) %>%
    rename(region_pop = county_pop) %>%
    (function(x){
      x$region %>% unique %>%
        lapply(function(region_i){
          df_i     <- x %>% filter(region == region_i)
          df_i_new <- approx(
            x    = df_i$year,
            y    = df_i$region_pop,
            xout = c_intYears
          ) %>% 
            as.data.frame %>% 
            rename(year = x, region_pop = y) %>%
            mutate(region = region_i)
        }) %>%
        (function(i){do.call(rbind, i)})
    })
  
  ###### State population summary ######
  ### State population summary (1020 rows)
  msg1 %>% paste0("\t", "Calculating state population...") %>% message
  iclus_state_pop <-
    iclusData %>%
    group_by_at(.vars=c("state", "region", "year")) %>%
    summarise_at(.vars=c("county_pop"), sum, na.rm=T) %>%
    rename(state_pop = county_pop) %>%
    (function(x){
      x$state %>% unique %>%
        lapply(function(state_i){
          df_i     <- x %>% filter(state == state_i)
          region_i <- df_i$region %>% unique
          df_i_new <- approx(
            x    = df_i$year,
            y    = df_i$state_pop,
            xout = c_intYears
          ) %>% 
            as.data.frame %>% 
            rename(year = x, state_pop = y) %>%
            mutate(region = region_i) %>%
            mutate(state  = state_i)
        }) %>%
        (function(i){do.call(rbind, i)})
    }) %>%
    ### Join with region info and calculate ratio
    left_join(iclus_region_pop, by = c("year", "region")) %>%
    mutate(
      ratioStatePop2Region = state_pop / region_pop
    )
  
  ###### County population summary ######
  msg1 %>% paste0("\t", "Calculating county population ratios...") %>% message
  ### Join with region info and calculate ratio
  iclusData <- iclusData %>%
    left_join(iclus_state_pop, by = c("year", "region", "state")) %>%
    mutate(
      ratioCountyPop2State = county_pop / state_pop
    ) %>%
    filter(!is.na(ratioCountyPop2State))
  

  ###### Create population projections ######
  ### Create and save a list of population projections
  ### About half an hour to run them all
  x_sysTime1  <- Sys.time()
  popProjList <- list()

  x_regions   <- iclusData$region %>% unique
  msg1 %>% paste0("\t", "Creating functions...") %>% message
  for(i in 1:length(x_regions)){
    region_i  <- x_regions[i]
    df_i      <- iclusData  %>% filter(region==region_i)
    states_i  <- df_i$state %>% unique
    # states_i  <- (iclus_state_pop %>% filter(region==region_i))$state %>% unique
    msg1 %>% paste0("\t\t", region_i, ":") %>% message
    
    ### Initialize empty list for region
    popProjList[[region_i]] <- list()

    
    for(j in 1:length(states_i)){
      ### Get state population projection and initialize list
      state_j    <- states_i[j]
      df_j       <- iclus_state_pop %>% filter(state==state_j)
      geoids_j   <- (df_i %>% filter(state==state_j))$geoid10 %>% unique
      counties_j <- (df_i %>% filter(state==state_j))$county %>% unique
      
      ### Initialize list
      popProjList[[region_i]][[state_j]] <- list()

      ### Create function and add to list
      fun_j    <- approxfun(
        x = df_j$year,
        y = df_j$ratioStatePop2Region
      )

      popProjList[[region_i]][[state_j]][["state2region"]] <- fun_j
      popProjList[[region_i]][[state_j]][["county2state"]] <- list()
      
      ### Get projections by counties and initialize list
      
      for(k in 1:length(geoids_j)){
        geoid_k  <- geoids_j[k] %>% as.character
        df_k     <- df_i %>% filter(state==state_j) %>% filter(as.character(geoid10) == geoid_k)
        county_k <- (df_k$iclusCounty %>% unique)[1]
      # for(k in 1:length(counties_j)){
      #   county_k <- counties_j[k] %>% as.character
      #   df_k     <- df_i %>% filter(state==state_j) %>% filter(county == county_k)
      #   # geoid_k <- (df_k$iclusCounty %>% unique)[1]
        
        ### Create list and add to state
        x_k <- df_k$year
        y_k <- df_k$ratioCountyPop2State
        
        all_na_k <- y_k %>% is.na %>% all
        if(all_na_k){
          # df_k %>% names %>% print
          
          paste0(state_j, ", ", county_k, "=NA for all values") %>% print
          # paste0(state_j, ", ", geoid_k, "=NA for all values") %>% print
          # counties_j %>% paste(collapse=", ") %>% print
          # fun_k    <- NA
        }
        else{
          fun_k   <- approxfun(
            x = df_k$year,
            y = df_k$ratioCountyPop2State
          )
          popProjList[[region_i]][[state_j]][["county2state"]][[geoid_k]] <- fun_k
          # popProjList[[region_i]][[state_j]][["county2state"]][[county_k]] <- fun_k
        }
      }
    }
  }

  x_sysTime2 <- Sys.time()
  msg1 %>% paste0("Created county-level population projection functions in:") %>% message
  (x_sysTime2 - x_sysTime1) %>% print

  ###### List ######
  svPopList <- list(
    iclus_region_pop = iclus_region_pop,
    popProjList      = popProjList
  )
  ###### Save ######
  if(save){
    msg1 %>% paste0("Saving impacts list...") %>% message
    ### Check that the directory exists
    if(dir.exists(outPath) & !is.null(outFile)){
      outFileName <- outFile %>% paste(rDataExt, sep=".")
      outFilePath <- outPath %>% file.path(outFileName)
      save(svPopList, file=outFilePath)
      msg1 %>% paste0("\t", "Population scenario and projection list saved.") %>% message
    } else{
      fail_msg <- ifelse(
        is.null(outFile),
        msg1 %>% paste0("\t", "Warning: `outFile=NULL` does not exist."),
        msg1 %>% paste0("\t", "Warning: directory `outPath='", outPath, "'` does not exist.")
      )
      fail_msg %>% message
      if(return){
        msg1 %>% paste0("\t", "Returning population scenario and projection list and exiting without saving...") %>% message
        return(svPopList)
      } ### End if return
    }
  }

  ###### Return ######
  if(return){
    msg1 %>% paste0("\t", "Returning population scenario and projection list and exiting...") %>% message
    msg1 %>% paste0("Finished.", "\n") %>% message
    return(svPopList)
  } else{
    msg1 %>% paste0("Finished.", "\n") %>% message
  }

}

###### get_svImpactsList ######
### This function reads in impacts data, formats it, and then creates impacts lists from it

get_svImpactsList <- function(
  dataFile   = NULL,
  dataPath   = file.path(getwd(), "inst", "extdata",  "sv", "impacts"),
  outFile    = NULL,
  outPath    = file.path(getwd(), "data", "sv", "impactLists"),
  rDataExt   = "rda", ### R data extension
  createList = F,
  # modelType = "gcm",
  # svData     = NULL,
  sector     = NULL,
  svInfo     = NULL,
  save       = F,
  return     = T,
  msg0       = ""
){
  msg0 %>% paste0("Running get_svImpactsList():", "\n") %>% message
  msg1 <- msg0 %>% paste0("\t")
  ###### Check for data ######
  ### Check if there is a data file and if not, exit
  if(is.null(dataFile)){
    msg1 %>% paste0("Argument `dataFile=NULL`. Please provide a valid value to argument `dataFile`.") %>% message
    msg1 %>% paste0("\n", "Exiting...") %>% message
    return()
  }
  ### Check if there the data file exists and if not, exit
  dataFilePath   <- dataPath %>% file.path(dataFile)
  dataPathFiles  <- dataPath %>% list.files
  dataFileExists <- dataFile %in% dataPathFiles
  if(!dataFileExists){
    msg1 %>% paste0("File `dataFile=", dataFile, "` not found in `dataPath='", dataPath,"'`.") %>% message
    msg1 %>% paste0("\n", "Exiting...") %>% message
  }
  ###### Load data from CSV ######
  msg1 %>% paste0("Loading data from `dataFile=", dataFile, "` in `dataPath='", dataPath,"'`...") %>% message
  df_data <- dataFilePath %>% read.csv
  rows0   <- df_data %>% nrow
  tracts0 <- df_data$tract %>% unique %>% length
  msg1 %>% paste0("\t", "Data has ", rows0, " rows and ", tracts0, " unique tracts.") %>% message

  ###### Gather data ######
  # x_subUnit <- ifelse(modelType=="gcm", "deg", "cm")
  msg1 %>% paste0("Gathering data...") %>% message
  df_data    <- df_data %>%
    gather(
      key   = "driverValue_txt",
      value = "sv_impact",
      -c("tract")
    )

  # df_data %>% names %>% print
  # df_data$driverValue_txt %>% unique %>% print

  df_data    <- df_data %>%
    ### Convert tract to FIPS
    rename(fips = tract) %>%
    mutate(fips = fips %>% as.character) %>%
    ### Convert driver value text to numeric
    mutate(
      # driverValue = driverValue_txt %>% (function(x){gsub(x_subUnit, "", x)}) %>% as.numeric
      driverValue = driverValue_txt %>%
        (function(x){gsub("deg", "", x)}) %>%
        (function(x){gsub("cm", "", x)}) %>%
        (function(x){gsub("X", "", x)}) %>%
        as.numeric
    )
  # df_data$fips %>% unique %>% length %>% print
  

  ###### Check data against sv data ######
  msg1 %>% paste0("Checking data against SV data...") %>% message
  ### Load sv data "svData"
  if(is.null(svInfo)){
    df_sv_path <- outPath %>% file.path("..", "..", "R", "svDataList.rda")
    load(df_sv_path)
    if(sector == "Coastal Properties"){
      svInfo <- svDataList$svDataCoastal  
    } else{
      svInfo <- svDataList$svData  
    }
    # svInfo$fips %>% unique %>% head %>% print
  }
  
  
  ### Standardize fips
  maxChar    <- svInfo$fips %>% nchar %>% max(na.rm=T)
  df_data    <- df_data %>%
    mutate(
      nChars     = fips %>% nchar,
      fips       = paste0(ifelse(nChars < maxChar, "0", ""), fips)
    ) %>%
    select(-c("nChars"))
  
  # c_ids1 <- df_data$fips %>% unique; paste0(length(c_ids1), ": ", paste(head(c_ids1)), collapse=", ") %>% print
  # c_ids2 <- svInfo$fips  %>% unique; paste0(length(c_ids2), ": ", paste(head(c_ids2)), collapse=", ") %>% print
  # c_ids1[c_ids1 %in% c_ids2] %>% length %>% print
  # c_ids2[c_ids2 %in% c_ids1] %>% length %>% print
  
  ### Join with SV data
  df_data <- df_data %>%
    left_join(
      svInfo %>% select(c("region", "state", "county", "fips")),
      by = "fips"
    ); rm("svInfo")
  ###### Remove missing values ######
  tracts_data <- df_data$fips %>% unique %>% length
  msg1 %>% paste0("\t", "Data has ", tracts_data, " tracts...") %>% message
  
  tracts_na   <- (df_data %>% filter(is.na(county)))$fips %>% unique %>% length
  tracts_nan  <- (df_data %>% filter(is.nan(sv_impact) | is.infinite(sv_impact)))$fips %>% unique %>% length
  # tracts_inf <- (df_data %>% filter(is.infinite(sv_impact)))$fips %>% unique %>% length
  ### Counties
  if(tracts_na>0){
    msg1 %>% paste0("\t", "Removing observations for ", tracts_na, " tracts with missing county information...") %>% message
    df_data <- df_data %>% filter(!is.na(county))
  }
  ### NaN
  if(tracts_nan>0){
    msg1 %>% paste0("\t", "Removing observations for ", tracts_nan, " tracts with NaN values for impacts...") %>% message
    df_data <- df_data %>% filter(!is.infinite(sv_impact)) %>% filter(!is.nan(sv_impact))
  }

  ### Return if createList=F
  if(!createList){
    return(df_data)
  }
  Sys.sleep(.1)

  ###### Unique Tracts ######
  c_fips <- df_data$fips %>% unique
  n_fips <- c_fips %>% length
  
  ###### Create the impacts list ######
  ### Start system time
  ### Initialize impact list
  ### Iterate over values
  msg1 %>% paste0("Creating impacts lists...") %>% message
  sysTime1    <- Sys.time()
  impactsList <- list()

  status_pcts <- c(25, 50, 75, 100)
  # status_pcts <- status_labs/100
  status_nums <- ceiling(status_pcts/100)
  
  for(i in 1:n_fips){
    which_status_i <- (i == status_nums) %>% which
    if(length(which_status_i)>0){
      msg1 %>% paste0("\t\t", status_pcts[which_status_i], "% complete...")
    }
    
    fips_i <- c_fips[i]
    df_i   <- df_data %>% filter(fips == fips_i)

    ### 11 rows: df_i %>% nrow %>% print
    x_i    <- c(0, df_i$driverValue)
    y_i    <- c(0, df_i$sv_impact)

    which_there  <- y_i[!is.na(y_i)]
    length_there <- which_there %>% length

    if(length_there<2){
      impactsList[[fips_i]] <- NA
    } else{
      fun_i        <- approxfun(x = x_i, y = y_i)
      impactsList[[fips_i]] <- fun_i
    }
    
    # if(length_there>=2){
    #   impactsList[[fips_i]] <- approxfun(x = x_i, y = y_i)
    # }
    Sys.sleep(.001)
  }
  sysTime2  <- Sys.time()
  deltaTime <- (sysTime2 - sysTime1)

  msg1 %>% paste0("\t", "Created impact list in ", deltaTime) %>% message
  ###### Save the impacts list ######
  if(save){
    msg1 %>% paste0("Saving impacts list...") %>% message
    ### Check that the directory exists
    if(dir.exists(outPath) & !is.null(outFile)){
      outname     <- outFile
      outFileName <- outFile %>% paste(rDataExt, sep=".")
      outFilePath <- outPath %>% file.path(outFileName)
      
      ### Assign list, save to file, and remove list
      assign(outname, impactsList); rm("impactsList")
      save(list=ls(pattern="impactsList_"), file=outFilePath)
      # eval(substitute(rm(x), list(x=outname)))
      msg1 %>% paste0("\t", "Impacts list saved.") %>% message
    } else{
      fail_msg <- ifelse(
        is.null(outFile),
        msg1 %>% paste0("\t", "Warning: `outFile=NULL` does not exist."),
        msg1 %>% paste0("\t", "Warning: directory `outPath='", outPath, "'` does not exist.")
      )
      fail_msg %>% message
      msg1 %>% paste0("\t", "Returning impacts list and exiting without saving...") %>% message
      # return(eval(parse(text=outname)))
    }
  }
  if(return){
    msg1 %>% paste0("\t", "Returning impacts list and exiting...") %>% message
    msg1 %>% paste0("Finished.", "\n") %>% message
    
    if(exists("impactsList")){
      return(impactsList)
    } else{
      return(eval(parse(text=outname)))  
    }
  } else{
    msg1 %>% paste0("Finished.", "\n") %>% message
  }
  
}


