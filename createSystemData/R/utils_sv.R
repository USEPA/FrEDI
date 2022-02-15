###### Overview ######
### This file contains helper functions for the FrEDI SV module.

###### get_svDataList ######
get_svDataList <- function(
  dataPath       = file.path(getwd(), "inst", "extdata", "sv"), ### Path to data file...default relative to current working directory
  dataFile       = "SVdemographic.xlsx", ### Data file name...defaults to "SVdemographic.xlsx"
  dataSheet      = "svDemoData", ### Defaults to "svDemoData"
  dataTableName  = dataSheet, ### Defaults to `dataSheet`` value
  dataInfoSheet  = "Controls", ### Where to find the table with info on columns in svDemoData
  dataInfoName   = "svDemoInfo", ### Table name for table with info on columns in svDemoData
  dataSectorName = "sectorInfo",
  dataAdaptName  = "coAdapt",
  dataFormatName = "co_formatting",
  dataTypesName  = "co_formatTypes",
  gcamSheet      = "GCAM",
  gcamName       = "gcamScenarios",
  outPath        = file.path(getwd(), "data"), ### Where to save rdata objects
  # saveFile      = "svDemoData.rdata",
  save           = F,
  return         = T
){
  ###### Libraries ######
  # require(tidyverse)
  # require(openxlsx)
  ###### File paths ######
  dataFilePath   <- dataPath %>% file.path(dataFile)

  ### Check if file exists and if it doesn't, exit
  dataPathFiles  <- dataPath %>% list.files
  dataFileExists <- dataFile %in% dataPathFiles
  if(!dataFileExists){
    paste0("File `dataFile='", dataFile, "'` not found in `dataPath='", dataPath, "'`") %>% message
    paste0("\n\t", "Exiting...") %>% message
    return()
  }

  ### Otherwise, get information about the file
  dataFileSheets <- dataFilePath %>% getSheetNames

  ### Check that `dataInfoSheet` and `dataSheet` exists:
  dataInfoSheetExists <- dataInfoSheet %in% dataFileSheets
  if(!dataInfoSheetExists){
    paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
    paste0("`dataInfoSheet='", dataInfoSheet, "'` not found in `dataFile='", dataFile, "'`") %>% message
    paste0("\n\t", "Exiting...") %>% message
    return()
  } ### End if !dataInfoSheetExists

  dataSheetExists <- dataSheet %in% dataFileSheets
  if(!dataSheetExists){
    paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
    paste0("`dataSheet='", dataSheet, "'` not found in `dataFile='", dataFile, "'`") %>% message
    paste0("\n\t", "Exiting...") %>% message
    return()
  } ### End if !dataSheetExists

  gcamSheetExists <- gcamSheet %in% dataFileSheets
  if(!gcamSheetExists){
    paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
    paste0("`gcamSheet='", gcamSheet, "'` not found in `dataFile='", dataFile, "'`") %>% message
    paste0("\n\t", "Exiting...") %>% message
    return()
  } ### End if !dataSheetExists

  ###### Load the Excel Workbook ######
  wb_data          <- dataFilePath %>% loadWorkbook
  dataInfoTables   <- wb_data %>% getTables(sheet = dataInfoSheet)
  dataDriverTables <- wb_data %>% getTables(sheet = gcamSheet)
  dataDataTables   <- wb_data %>% getTables(sheet = dataSheet); rm("wb_data")

  ###### Get the data info table ######
  ### If the files exist then get info about the tables
  ### - Load the Excel Workbook
  ### - Get table names on `dataInfoSheet` and remove the Excel workbook to free up space
  ### - Check that `dataInfoName` is one of the tables on the worksheet
  ### - If it isn't, exit
  df_infoTables <-
    data.frame(
      inputArg  = c("dataInfoName", "dataSectorName", "dataAdaptName", "dataFormatName", "dataTypesName"),
      tableName = c(dataInfoName, dataSectorName, dataAdaptName, dataFormatName, dataTypesName),
      tableType = c("dataInfoTable", "dataSectorInfo", "dataAdaptInfo", "dataFormatInfo", "dataFormatTypes")
    )

  for(i in 1:nrow(df_infoTables)){
    table_i       <- df_infoTables$tableName[i]
    inputArg_i    <- df_infoTables$inputArg[i]
    tableType_i   <- df_infoTables$tableType[i]
    tableExists_i <- table_i %in% dataInfoTables

    ### Check if table exists
    if(!tableExists_i){
      paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
      paste0("Data table `", inputArg_i, "='", table_i, "'` not found on `dataInfoSheet='", dataInfoSheet, "'`") %>% message
      paste0("\n\t", "Exiting...") %>% message
      return()
    } ### End if !dataSheetExists

    ### If it exists, then load the data:
    ### - Figure out which of the tables is the info table
    ### - Get the range for the table (name in the character vector), then split it and extract from the resulting list
    ### - Extract the column letters from the range and then convert to numbers
    ### - Extract the rows from the range
    which_infoTable <- (table_i == dataInfoTables) %>% which
    range_infoTable <- names(dataInfoTables)[which_infoTable] %>% str_split(":") %>% unlist
    # cols_infoTable  <- range_infoTable %>% str_extract("([:alpha:]*)") %>% convertFromExcelRef
    cols_infoTable  <- range_infoTable %>% str_extract("([:alpha:]*)")
    ### Get rows
    rows_infoTable  <- 1:2 %>% lapply(function(j){
      col_j <- cols_infoTable[j]
      row_j <- gsub(col_j, "", range_infoTable[j])
      return(row_j)
    }) %>% unlist %>% as.numeric
    ### Convert columns to numeric
    cols_infoTable <- cols_infoTable %>% convertFromExcelRef
    ### Finally, read in the table
    # cols_infoTable %>% print
    # rows_infoTable %>% print
    df_table_i <- dataFilePath %>% read.xlsx(
      sheet = dataInfoSheet,
      rows  = rows_infoTable[1]:rows_infoTable[2],
      cols  = cols_infoTable[1]:cols_infoTable[2],
      rowNames = T
    )
    assign(tableType_i, df_table_i)
    # dataInfoTable %>% names %>% print
  }

  ###### SV Group Types ######
  c_svGroupTypes <- (dataInfoTable %>% filter(colType %in% c("sv", "minority")))$colName
  c_svWeightCols <- (dataInfoTable %>% filter(colType == "weight"))$colName
  ###### Sector-Adaptation Info ######
  svSectorInfo <- dataSectorInfo %>% left_join(dataAdaptInfo, by = "sector_abbr")


  ###### GCAM Scenarios ######
  gcamExists     <- gcamName %in% dataDriverTables

  ### Check if table exists
  ### If it exists, then load the data:
  ### - Figure out which of the tables is the info table
  ### - Get the range for the table (name in the character vector), then split it and extract from the resulting list
  ### - Extract the column letters from the range and then convert to numbers
  ### - Extract the rows from the range
  if(!gcamExists){
    paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
    paste0("Data table `", gcamName, "='", gcamName, "'` not found on `gcamSheet='", gcamSheet, "'`") %>% message
    paste0("\n\t", "Exiting...") %>% message
    return()
  } ### End if !dataSheetExists

  which_gcamTable <- (gcamName == dataDriverTables) %>% which
  range_gcam      <- names(dataDriverTables)[which_gcamTable] %>% str_split(":") %>% unlist
  cols_gcam       <- range_gcam %>% str_extract("([:alpha:]*)")
  ### Get rows
  rows_gcam  <- 1:2 %>% lapply(function(j){
    col_j <- cols_gcam[j]
    row_j <- gsub(col_j, "", range_gcam[j])
    return(row_j)
  }) %>% unlist %>% as.numeric
  ### Convert columns to numeric
  cols_gcam <- cols_gcam %>% convertFromExcelRef
  ### Finally, read in the table
  # cols_infoTable %>% print
  # rows_infoTable %>% print
  gcamScenarios <- dataFilePath %>% read.xlsx(
    sheet = gcamSheet,
    rows  = rows_gcam[1]:rows_gcam[2],
    cols  = cols_gcam[1]:cols_gcam[2],
    rowNames = T
  )

  ###### Get the data info table ######
  ### If the files exist then get info about the tables
  ### - Load the Excel Workbook
  ### - Get table names on `dataInfoSheet` and remove the Excel workbook to free up space
  ### - Check that `dataInfoName` is one of the tables on the worksheet
  ### - If it isn't, exit

  ###### Read in the SV Data Table ######
  paste0("Loading data from `dataSheet='", dataSheet, "'`...") %>% message
  dataTable <- dataFilePath %>% read.xlsx(
    sheet = dataSheet,
    startRow = 1,
    cols     = 1 + 1:nrow(dataInfoTable),
    na.strings = c("NA", "NaN")
  ) %>%
    ### Rename columns: Reorder columns, rename columns
    (function(x){
      x        <- x[,dataInfoTable$colOrder_excel]
      names(x) <- dataInfoTable$colName
      return(x)
    }) %>%
    ### Replace NA values with 0
    (function(x){
      colNames_x <- (dataInfoTable %>% filter(colType=="minority"))$colName
      x          <- x %>% mutate_at(.vars=all_of(colNames_x), replace_na, 0)
      return(x)
    }) %>%
    ### Standardize county number
    mutate(fips_num = fips %>% as.numeric) %>%
    mutate(fips     = fips %>% as.character) %>%
    ### Remove "County", "Parish", and "city"
    rename(svCounty = county) %>%
    mutate(
      county   = svCounty %>% (function(x){gsub(" County", "", x)}),
      county   = county   %>% (function(x){gsub(" Parish", "", x)}),
      county   = county   %>% (function(x){gsub(" city", "", x)})
    ) %>%
    mutate(
      nChars     = fips %>% nchar,
      geoid10    = fips %>% substr(1, ifelse(nChars > 10, 5, 4))
    ) %>%
    select(-c("nChars"))

  ###### Summarize the County Data ######
  data_countyPop <- dataTable %>%
    group_by_at(.vars=c("state", "geoid10")) %>%
    summarize_at(.vars=c("tract_pop"), sum, na.rm=T) %>%
    rename(county_pop = tract_pop)
  # svCountyPop %>% glimpse

  ###### Join County Data with Main Data######
  paste0("Loading data from `gcamSheet='", gcamSheet, "'`...") %>% message
  dataTable <- dataTable %>%
    left_join(
      data_countyPop, by = c("state", "geoid10")
    ) %>%
    mutate(ratioTract2CountyPop = tract_pop / county_pop)


  ###### Driver Scenario ######
  ### Driver units: degrees Celsius, cm
  gcamScenarios <-
    excelDataPath %>%
    file.path(tempScenarios_file) %>%
    read.xlsx(
      sheet = tempScenarios_sheet,
      rows = tempScenarios_rows,
      cols = tempScenarios_cols
    ) %>%
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
  gcamScenarios %>% glimpse

  ###### Rename the Data ######
  ### Object names
  svDataList    <- list(
    svDemoInfo      = dataInfoTable,
    c_svGroupTypes  = c_svGroupTypes,
    c_svWeightCols  = c_svWeightCols,
    svSectorInfo    = dataSectorInfo,
    df_formatInfo   = dataFormatInfo,
    df_formatTypes  = dataFormatTypes,
    gcamScenarios   = gcamScenarios,
    svData          = dataTable
  )
  saveFileNames <- c("svDataList")

  ###### Save the Data ######
  if(save){
    paste0("Saving data to `outPath='", outPath, "'`...") %>% message
    ### File names
    saveFiles     <- paste(saveFileNames, "rdata", sep=".")
    ### Save table info
    save(svDataList, file=file.path(outPath, saveFiles))
  }
  paste0("Finished.") %>% message
  return(
    svDataList
  )
}



###### get_svPopList ######
get_svPopList <- function(
  dataFile   = "ICLUS_v2_UN_Probabilistic_Median",
  dataPath   = file.path(getwd(), "inst", "extdata", "impacts"),
  dataSheet  = "Output",
  dataTable  = "iclusData",

  infoSheet  = "Controls",
  infoTable  = "iclusDataInfo",

  svData     = NULL,

  # infoRows   = 3:31,
  # infoCols   = 2:8,
  outFile    = "svPopData",
  outPath    = file.path(getwd(), "data"),
  save       = F,
  return     = T
){
  ###### Constants ######
  c_intYears <- seq(2000, 2090, by=5) ### years for interpolating
  ###### Check for data ######
  ### Check if there is a data file and if not, exit
  if(is.null(dataFile)){
    paste0("Argument `dataFile=NULL`. Please provide a valid value to argument `dataFile`.") %>% message
    paste0("\n", "Exiting...") %>% message
    return()
  }
  ### Check if there the data file exists and if not, exit
  dataFile       <- paste(dataFile, "xlsx", sep=".")
  dataFilePath   <- dataPath %>% file.path(dataFile)
  dataPathFiles  <- dataPath %>% list.files
  dataFileExists <- dataFile %in% dataPathFiles
  if(!dataFileExists){
    paste0("File `dataFile=", dataFile, "` not found in `dataPath='", dataPath,"'`.") %>% message
    paste0("\n", "Exiting...") %>% message
  }

  ### Otherwise, get information about the file:
  ### - Create a dataframe with sheet/table info
  ### - Iterate over the tables
  dataFileSheets <- dataFilePath %>% getSheetNames

  x_sv_df <- data.frame(
    table = x_sv_tables,
    sheet = c(dataSheet, infoSheet)
  )
  x_sv_tables <- c(dataTable, dataSheet)
  x_sv_sheets <- x_sv_df$sheet %>% unique
  wb_data     <- dataFilePath %>% loadWorkbook
  for(i in 1:length(x_sv_sheets)){
    sheet_i <- x_sv_df$sheet[i]
    which_i <- (x_sv_df$sheet == sheet_i) %>% which
    ### Check that sheet_i exists:
    sheet_i_exists <- sheet_i %in% dataFileSheets
    if(!infoSheetExists){
      paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
      paste0("\t", "`sheet_i='", sheet_i, "'` not found in `dataFile='", dataFile, "'`") %>% message
      paste0("\n\t", "Exiting...") %>% message
      return()
    } ### End if !infoSheetExists


    ### If the files exist then get info about the tables
    ### - Load the Excel Workbook, get table names and remove the Excel workbook to free up space
    ### - Check that `dataInfoName` is one of the tables on the worksheet
    ### - If it isn't, exit
    dataTables_i <- wb_data %>% getTables(sheet = sheet_i)

    ### Iterate over tables
    tables_i <- (x_sv_df %>% filter(sheet == sheet_i))$table
    for(j in 1:length(tables_i)){
        table_j       <- tables_i[j]
        tableExists_j <- x

        ### Check if table exists
        if(!tableExists_j){
          paste0("Loading data from `dataFile='", dataFile, "'` in `dataPath='", dataPath, "'`") %>% message
          paste0("Data table='", table_j, "' not found on sheet_i='", sheet_i, "'`") %>% message
          paste0("\n\t", "Exiting...") %>% message
          return()
        } ### End if !dataSheetExists

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

  ###### Load Data ######
  iclusDataInfo <- dataFilePath %>%
    read.xlsx(
      sheet = infoSheet, rows = infoRows, cols = infoCols, rowNames = T
    ) %>%
    rename(iclusDataType = colType) %>%
    select(-c("colName_excel", "baseYear", "colOrder_excel"))


  iclusData <- dataFilePath %>%
    read.xlsx(
      sheet    = dataSheet, rowNames = T
    ) %>%
    rename_all(tolower) %>%
    gather(
      key   = "colName", value = "iclus_pop",
      -c("geoid10", "fips", "iclusgeoid", "couname", "stname", "prop", "name")
    ) %>%
    left_join(iclusDataInfo, by = "colName") %>%
    select(-c("colName")) %>%
    ### Rename counties
    rename(state = stname) %>%
    rename(iclusCounty = couname) %>%
    mutate(county = iclusCounty %>% (function(x){gsub("Ã±", "ñ", x)})) %>%
    rename(county_pop = iclus_pop) %>%
    filter(year>=2000)

  ###### Check Against SV Data ######
  paste0("\n", "Checking data against SV data...") %>% message
  if(is.null(svData)){
    ### Load sv data "svData"
    df_sv_path <- outPath %>% file.path("svDataList.rda")
    load(df_sv_path)
    svData <- svDataList$svData; rm("svDataList")
  }

  ### Join with SV data
  iclusData <- iclusData %>%
    left_join(
      svData %>%
        group_by_at(.vars=c("region", "state", "geoid10")) %>%
        summarise(n=n(), .groups="keep") %>%
        select(-c("n")),
      by = c("state", "geoid10")
    ) %>%
    ### Filter to relevant states, regions, tracts
    filter(!is.na(region)) %>%
    filter(!is.na(state))
  ###### Remove missing values ######
  state_na  <- (iclusData %>% filter(is.na(state)))$county %>% unique %>% length
  region_na <- (iclusData %>% filter(is.na(region)))$county %>% unique %>% length
  if(state_na>0){
    paste0("\t", "Removing observations for ", state_na, " tracts with missing state information...") %>% message
    iclusData <- iclusData %>% filter(!is.na(state))
  }
  if(region_na>0){
    paste0("\t", "Removing observations for ", region_na, " tracts with missing region information...") %>% message
    iclusData <- iclusData %>% filter(!is.na(region))
  }

  ###### Regional population summary ######
  ### Regional population summary (160 rows)
  sv_region_pop <-
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
          ) %>% as.data.frame %>% rename(year = x, region_pop = y)
        }) %>%
        (function(i){do.call(rbind, i)})
    })

  ###### State population summary ######
  ### State population summary (1020 rows)
  sv_state_pop <-
    iclusData %>%
    group_by_at(.vars=c("state", "region", "year")) %>%
    summarise_at(.vars=c("county_pop"), sum, na.rm=T) %>%
    rename(state_pop = county_pop) %>%
    (function(x){
      x$state %>% unique %>%
        lapply(function(state_i){
          df_i     <- x %>% filter(region == region_i)
          df_i_new <- approx(
            x    = df_i$year,
            y    = df_i$state_pop,
            xout = c_intYears
          ) %>% as.data.frame %>% rename(year = x, state_pop = y)
        }) %>%
        (function(i){do.call(rbind, i)})
    }) %>%
    ### Join with region info and calculate ratio
    left_join(region_pop, by = c("year", "region")) %>%
    mutate(
      ratioStatePop2Region = state_pop/region_pop
    )

  ###### Create population projections ######
  ### Create and save a list of population projections
  ### About half an hour to run them all
  x_sysTime1  <- Sys.time()
  popProjList <- list()

  x_regions   <- sv_region_pop$region %>% unique
  for(i in 1:length(x_regions)){
    region_i <- x_regions[i]
    states_i <- (sv_state_pop %>% filter(region==region_i))$state %>% unique

    ### Initialize empty list for region
    popProjList[[region_i]] <- list()

    for(j in 1:length(states_i)){
      ### Get state population projection and initialize list
      state_j  <- states_i[j]
      df_j     <- state_pop %>% filter(state==state_j)
      popProjList[[region_i]][[state_j]] <- list()

      ### Create function and add to list
      fun_j    <- approxfun(
        x = df_j$year,
        y = df_j$ratioStatePop2Region
      )

      popProjList[[region_i]][[state_j]][["state2region"]] <- fun_j

      ### Get projections by counties and initialize list
      geoids_j <- (iclusData %>% filter(state==state_j))$geoid10 %>% unique
      popProjList[[region_i]][[state_j]][["county2state"]] <- list()

      for(k in 1:length(geoids_j)){
        geoid_k <- geoids_j[j]
        df_k    <- iclusData %>%
          filter(state==state_j) %>%
          filter(geoid10 == geoid_k) %>%
          left_join(state_pop, by = c("state", "region", "year")) %>%
          mutate(ratioCountyPop2State = county_pop/state_pop)


        ### Create list and add to state
        fun_k   <- approxfun(
          x = df_k$year,
          y = df_k$ratioCountyPop2State
        )
        popProjList[[region_i]][[state_j]][["county2state"]][[geoid_k]] <- fun_k
      }
    }
  }

  x_sysTime2 <- Sys.time()
  paste0("Created county-level population projection functions in:") %>% message
  (x_sysTime2 - x_sysTime1) %>% print

  ###### List ######
  svPopList <- list(
    sv_region_pop = sv_region_pop,
    popProjList   = popProjList
  )
  ###### Save ######
  if(save){
    paste0("\n", "Saving impacts list...") %>% message
    ### Check that the directory exists
    if(dir.exists(outPath) & !is.null(outFile)){
      outFilePath <- outPath %>% file.path(paste(outFile, "rdat", sep="."))
      save(svPopList, file=outFilePath)
      paste0("\t", "Population scenario and projection list saved.") %>% message
    } else{
      fail_msg <- ifelse(
        is.null(outFile),
        paste0("\n\t", "Warning: `outFile=NULL` does not exist."),
        paste0("\n\t", "Warning: directory `outPath='", outPath, "'` does not exist.")
      )
      fail_msg %>% message
      if(return){
        paste0("\t", "Returning population scenario and projection list and exiting without saving...") %>% message
        return(svPopList)
      } ### End if return
    }
  }

  ###### Return ######
  if(return){
    paste0("\n\t", "Returning population scenario and projection list and exiting...") %>% message
    return(svPopList)
  }

}

###### get_svImpactsList ######
### This function reads in impacts data, formats it, and then creates impacts lists from it

get_svImpactsList <- function(
  dataFile = NULL,
  dataPath = file.path(getwd(), "inst", "extdata", "impacts"),
  outFile  = NULL,
  outPath  = file.path(getwd(), "data", "impactLists"),
  createList = F,
  # modelType = "gcm",
  save     = F,
  return   = T
){
  ###### Check for data ######
  ### Check if there is a data file and if not, exit
  if(is.null(dataFile)){
    paste0("Argument `dataFile=NULL`. Please provide a valid value to argument `dataFile`.") %>% message
    paste0("\n", "Exiting...") %>% message
    return()
  }
  ### Check if there the data file exists and if not, exit
  dataFilePath   <- dataPath %>% file.path(dataFile)
  dataPathFiles  <- dataPath %>% list.files
  dataFileExists <- dataFile %in% dataPathFiles
  if(!dataFileExists){
    paste0("File `dataFile=", dataFile, "` not found in `dataPath='", dataPath,"'`.") %>% message
    paste0("\n", "Exiting...") %>% message
  }
  ###### Load data from CSV ######
  paste0("Loading data from `dataFile=", dataFile, "` in `dataPath='", dataPath,"'`...") %>% message
  df_data <- dataFilePath %>% read.csv
  rows0   <- df_data %>% nrow
  tracts0 <- df_data$tract %>% unique %>% length
  paste0("\t", "Data has ", rows0, " rows and ", tracts0, " unique tracts.") %>% message

  ###### Gather data ######
  # x_subUnit <- ifelse(modelType=="gcm", "deg", "cm")
  paste0("\n", "Gathering data...") %>% message
  df_data    <- df_data %>%
    gather(
      key   = "driverValue_txt",
      value = "sv_impact",
      -c("tract")
    )

  df_data %>% names %>% print
  df_data$driverValue_txt %>% unique %>% print

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



  ###### Check data against sv data ######
  paste0("\n", "Checking data against SV data...") %>% message
  ### Load sv data "svData"
  df_sv_path <- outPath %>% file.path("svDataList.rdata")
  load(df_sv_path)
  svData <- svDataList$svData; rm("svDataList")
  ### Join with SV data
  df_data <- df_data %>%
    left_join(
      svData %>% select(c("region", "state", "county", "fips")),
      by = "fips"
    )
  ###### Remove missing values ######
  tracts_na  <- (df_data %>% filter(is.na(county)))$fips %>% unique %>% length
  tracts_nan <- (df_data %>% filter(is.nan(sv_impact) | is.infinite(sv_impact)))$fips %>% unique %>% length
  # tracts_inf <- (df_data %>% filter(is.infinite(sv_impact)))$fips %>% unique %>% length
  ### Counties
  if(tracts_na>0){
    paste0("\t", "Removing observations for ", tracts_na, " tracts with missing county information...") %>% message
    df_data <- df_data %>% filter(!is.na(county))
  }
  ### NaN
  if(tracts_nan>0){
    paste0("\t", "Removing observations for ", tracts_nan, " tracts with NaN values for impacts...") %>% message
    df_data <- df_data %>% filter(!is.infinite(sv_impact)) %>% filter(!is.nan(sv_impact))
  }

  ### Return if createList=F
  if(!createList){
    return(df_data)
  }
  Sys.sleep(.1)

  ###### Unique Tracts ######
  c_fips <- df_data$fips %>% unique

  ###### Create the impacts list ######
  ### Start system time
  ### Initialize impact list
  ### Iterate over values
  paste0("\n", "Creating impacts list...") %>% message
  sysTime1    <- Sys.time()
  impactsList <- list()

  for(i in 1:length(c_fips)){
    # for(i in 1:2){
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
    }
    # fun_i        <- approxfun(x = x_i, y = y_i)
    impactsList[[fips_i]] <- fun_i
    Sys.sleep(.001)
  }
  sysTime2 <- Sys.time()
  deltaTime <- (sysTime2 - sysTime1)

  paste0("\t", "Created impact list in ", deltaTime) %>% message

  if(save){
    paste0("\n", "Saving impacts list...") %>% message
    ### Check that the directory exists
    if(dir.exists(outPath) & !is.null(outFile)){
      outFilePath <- outPath %>% file.path(paste(outFile, "rdata", sep="."))
      save(impactsList, file=outFilePath)
      paste0("\t", "Impacts list saved.") %>% message
    } else{
      fail_msg <- ifelse(
        is.null(outFile),
        paste0("\n\t", "Warning: `outFile=NULL` does not exist."),
        paste0("\n\t", "Warning: directory `outPath='", outPath, "'` does not exist.")
      )
      fail_msg %>% message
      paste0("\t", "Returning impacts list and exiting without saving...") %>% message
      return(impactsList)
    }
  }
  paste0("\n\t", "Returning impacts list and exiting...") %>% message
  return(impactsList)
}


