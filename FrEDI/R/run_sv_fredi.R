###### Documentation ######
#' Calculates climate change impacts on socially vulnerable populations throughout the 21st century for available sectors
#'
#' @description
#' This function allows users to project annual average climate change impacts throughout the 21st century (2010-2090) for available sectors (see [FrEDI::get_sectorInfo()]). Users may specify an optional list of custom scenarios. The output is an R data frame object containing annual average impacts, by year, for each sector, adaptation, impact type, model (GCM or SLR scenario), and region.
#'
#' @param inputsList A list of named elements named elements (`names(inputsList)= c("driverInput",  "popInput")`), each containing dataframes of custom scenarios for drivers (temperature or global mean sea level rise) and/or regional population scenarios, respectively. Scenarios must be defined over the period 2010 to 2090. Note: driver inputs (temperature and sea level rise) should start in 2000 or earlier. Population inputs should start in 2010 or earlier.
# ADD SENTENCE ABOUT CHECKING MODEL TYPE
#' @param sectorList A character vector indicating a selection of sectors for which to calculate results (see [FrEDI::get_sectorInfo()]). If `NULL`, all sectors are included.
#### ADD MORE INFO ABOUT ELASTICITY
#' @param silent A `TRUE/FALSE` value indicating the level of messaging desired by the user (default=`TRUE`).
#'
#' @details This function allows users to project annual average climate change impacts throughout the 21st century (2010-2090) for available sectors. [FrEDI::run_fredi_sv()] is the main function in the [FrEDI] R package, described elsewhere (See <https://epa.gov/cira/FrEDI> for more information).
#' @return
#' The output of [FrEDI::run_fredi_sv()] is an R data frame object containing annual average impacts, by year (2010-2090), for each sector, adaptation, model (GCM or SLR scenario), and region.
#'
#' @examples
#' ### Run function with defaults (same as `defaultResults` dataset)
#' sv_defaults <- run_fredi_sv()
#'
#' ### Path to example scenarios
#' scenariosPath <- system.file(package="FrEDI") %>% file.path("extdata","scenarios")
#' ### View example scenario names
#' scenariosPath %>% list.files
#' ### Temperature Scenario File Name
#' tempInputFile <- scenariosPath %>% file.path("GCAM_scenario.csv")
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath %>% file.path("slr_from_GCAM.csv")
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath %>% file.path("pop_scenario.csv")
#' ### Import inputs
#' example_inputsList <- import_inputs(
#'   tempfile = tempInputFile,
#'   slrfile  = slrInputFile,
#'   popfile  = popInputFile
#' )
#'
#' ### Run custom temperature scenario and output impacts without aggregation and with present values (default base year and discount rate)
#' df_tempExOut <- run_fredi(inputsList= tempBin_inputs, aggLevels="none", pv=TRUE, silent=TRUE)
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'
#' @export
#' @md
#'
###### run_fredi_sv ######
### This function creates a dataframe of sector impacts for default values or scenario inputs.
### run_fredi relies on the following helper functions: "interpolate_annual", "match_scalarValues","get_econAdjValues" , "calcScalars", "interpolate_tempBin"
run_fredi_sv <- function(
  inputsList = list(tempInput=NULL, slrInput=NULL, popInput=NULL), ### List of inputs
  sector = NULL, ### Vector of sectors to get results for
  return     = T,
  output2xl  = F,
  outpath    = "~",
  silent     = TRUE  ### Whether to message the user
){
  for(i in 1:length(fredi_config)) assign(names(fredi_config)[i], fredi_config[[i]])
  load(file.path(projectPath, "R", "svDataList.rda"))
  load(file.path(projectPath, "R", "svPopData.rda"))
  c_svGroupTypes <- svDataList$c_svGroupTypes
  # for(i in 1:length(fredi_config)) assign(names(fredi_config)[i], fredi_config[[i]])
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user)
  silent  <- ifelse(is.null(silent), T, silent)
  msgUser <- !silent
  msg0    <- ""
  msg1    <- msg0 %>% paste0("\t")
  msg2    <- msg1 %>% paste0("\t")
  msg3    <- msg2 %>% paste0("\t")

  ###### Sector Info ######
  ### Objects <-
  sectorInfo    <- svDataList$sectorInfo
  svSectorInfo  <- svDataList$svSectorInfo
  # co_modelTypes <- rDataList$co_modelTypes
  ### Sector names
  if(is.null(sector)){
    sector_msg1 <- paste0("Please select a sector: ") %>% print
    sector_msg2 <- 1:nrow(sectorInfo) %>% lapply(function(i){
      sector_i  <- sectorInfo$sector[i]
      msg_i     <- paste0(i, ". ", sector_i)
      msg_i %>% print
      # return(msg_i)
    }) #%>% unlist %>% paste(collapse="")
    # sector_msg3  <- sector_msg1 %>% paste0(sector_msg2)
    sector_msg3  <- "Enter a number:"
    sector_input <- readline(prompt = sector_msg3) %>% as.numeric
    sector       <- sectorInfo$sector[sector_input]
    rm("sector_msg1", "sector_msg2", "sector_msg3", "sector_input")
  }
  c_sector      <- sector
  paste0("Running FrEDI SV for sector ", c_sector) %>% message
  ### Sector info
  which_sector  <- (sectorInfo$sector == c_sector) %>% which
  c_modelType   <- sectorInfo$modelType[which_sector] %>% tolower

  # df_sectorInfo <- svSectorInfo %>% filter(sector==sector)
  which_sector  <- (svSectorInfo$sector == c_sector) %>% which #; which_sector %>% print
  df_sectorInfo <- svSectorInfo[which_sector,]
  # df_sectorInfo %>% print
  # c_adaptLabels <- svSectorInfo$adapt_label
  # c_adaptAbbr   <- svSectorInfo$adapt_abbr

  ###### Load Inputs ######
  ### Create logicals and initialize inputs list
  # which_inputs    <- co_inputScenarioInfo$inputName %in% c("driverInput", "popInput")
  # list_inputs     <- c("driverInput", "popInput")
  list_inputs     <- c("tempInput", "slrInput", "popInput")
  num_inputNames  <- list_inputs %>% length

  if( is.null(inputsList)){
    inputsList <- list()
  } else{
    message("Checking input values...")
  }
  # ### Iterate over the input list
  # # if(!is.null(inputsList)){
  # # co_inputScenarioInfo %>% head %>% print
  # ### Assign inputs to objects
  # for(i in 1:num_inputNames){
  #   which_i     <- co_inputScenarioInfo
  #   inputInfo_i <- co_inputScenarioInfo[i,]
  #   ### Input name and label
  #   input_i     <- inputInfo_i$inputName %>% unique
  #   msgName_i   <- inputInfo_i$inputType %>% unique
  #   ### Input run_fredi argument
  #   inputName_i <- inputInfo_i$tempBinListName %>% unique
  #   ### Min and Max Values
  #   min_i       <- inputInfo_i$inputMin %>% unique
  #   max_i       <- inputInfo_i$inputMax %>% unique
  #   ###### Column Info ######
  #   region_i    <- inputInfo_i$region %>% unique
  #   valueCol_i  <- inputInfo_i$valueCol %>% unique
  #   ### Initialize column names
  #   numCols_i   <- colNames_i <- c("year", valueCol_i) #; print(colNames_i)
  #   ### Add region column
  #   if(region_i == 1){
  #     colNames_i  <- c(colNames_i[1], "region", colNames_i[2])
  #   }
  #   has_i        <- paste0("has_", input_i, "Update")
  #   # has_update_i <- is.null(inputsList[[inputName_i]])
  #   df_input_i   <- inputsList[[inputName_i]]
  #   has_update_i <- !is.null(df_input_i)
  #   ###### Assign inputs to objects ######
  #   assign(has_i,    has_update_i)
  #   assign(inputName_i, df_input_i)
  #
  #   ### Iterate over the input list and check flags for inputs that are not null
  #   if(has_update_i){
  #     msg1 %>% message( "Checking input values for ", msgName_i, "...")
  #     ### Values
  #     values_i       <- df_input_i[,valueCol_i]
  #     ### Substitute NULL for missing values for min and max
  #     if(is.na(min_i)) min_i <- NULL; if(is.na(max_i)) max_i <- NULL
  #     ### Check the status
  #     flag_i         <- values_i %>% check_inputs(xmin = min_i, xmax = max_i)
  #     ### Return and message the user if there is a flag:
  #     flagStatus_i   <- flag_i$flagged
  #     flagRows_i     <- flag_i$rows
  #     ### If flag, message user and return flagStatus_i
  #     if(flagStatus_i){
  #       ### Message labels
  #       numrows_i    <- flagRows_i %>% length
  #       years_i      <- df_input_i$year[flagRows_i]
  #       yearsLabel_i <- paste(years_i, collapse=",")
  #       rangeLabel_i <- paste0("c(", min_i , ",", max_i, ")")
  #       ### Create message and message user
  #       msg1_i       <- "Error in importing inputs for" %>% paste(msgName_i) %>% paste0(":")
  #       msg2_i       <- inputName_i %>% paste("has", numrows_i,  "values outside of defined range", rangeLabel_i)
  #       msg3_i       <- "Please correct values" %>% paste(msgName_i, "values for years", yearsLabel_i)
  #
  #       message(msg2, msg1_i); message(msg3, msg2_i); message(msg3,  msg3_i, "..."); message("Exiting...", "\n")
  #       ### Return list with error and flagged rows
  #       returnList <- list(
  #         error_msg    = paste0("Error in ", inputName_i, ". Values outside range."),
  #         flagged_rows = flagRows_i
  #       )
  #
  #       ### Return list and not an inputs list if an error occurred
  #       return(returnList)
  #     } ### End if flagged_i
  #   } ### End if !is.null(df_i)
  # } ### End iterate over inputs
  # # }

  ###### Which Inputs Are Needed ######
  has_popUpdate <- has_slrUpdate <- has_tempUpdate <- F;
  # c(has_tempUpdate, has_slrUpdate, has_popUpdate) %>% print
  needsTemp <- ifelse(c_modelType=="gcm", T, ifelse(c_modelType=="slr" & (!has_slrUpdate), T, F))
  needsSLR  <- ifelse(c_modelType=="slr", T, F)
  # c(needsTemp, needsSLR) %>% print

  ###### Temperature Scenario ######
  ### User inputs: temperatures have already been converted to CONUS temperatures. Filter to desired range.
  ### Add the point where impacts are zero (the reference year temperature)
  ### For user inputs:
  ### - Select appropriate columns
  ### - Remove missing values of years, temperatures
  ### - Filter to appropriate years
  if(needsTemp){
    refYear_temp <- (rDataList$co_modelTypes %>% filter(modelUnitType=="temperature"))$modelRefYear[1]
    if(has_tempUpdate){
      msg1 %>% message("Creating temperature scenario from user inputs...")
      tempInput <- tempInput %>%
        select(c("year", "temp_C", "scenario")) %>%
        filter( year >  refYear_temp & year <= maxYear) %>%
        filter(!is.na(temp_C) & !(is.na(year)))
    }
    ### Otherwise use default scenario and add scenario column
    else{
      msg1 %>% message("No temperature scenario provided...")
      msg2 %>% message("\t", "Using default temperature scenario...")
      # rDataList$co_defaultTemps %>% names %>% print
      tempInput <- rDataList$co_defaultTemps %>%
        mutate(temp_C = temp_C_global %>% convertTemps(from="global")) %>%
        select(c("year", "temp_C")) %>%
        mutate(scenario="FrEDI Default")
    }
    ### Interpolate over scenarios:
    temp_df <- tempInput$scenario %>% unique %>%
      lapply(function(scenario_i){
        ### - Filter to scenario i and drop scenario column
        ### - Zero out series at the temperature reference year
        # tempInput %>% names %>% print
        input_i <- data.frame(year= refYear_temp, temp_C = 0) %>%
          rbind(
            tempInput %>% filter(scenario==scenario_i) %>% select(-c("scenario"))
          )

        ### Then, interpolate
        ### - Use minimum series year to determine interpolation years
        ### - Add a dummy region for National Total for interpolate_annual
        ### - Interpolate, drop dummy region, and add scenario back in
        df_i <-     input_i %>%
          (function(x){
            minYear_x <- x$year %>% min
            interpYrs <- refYear_temp:maxYear
            x_interp  <- x %>%
              mutate(region="National Total") %>%
              interpolate_annual(
                years = interpYrs, column = "temp_C", rule = 1:2
              ) %>%
              select(-c("region"))
            return(x_interp)
          }) %>%
          mutate(scenario = scenario_i)
        return(df_i)
      }) %>%
      (function(x){do.call(rbind, x)})
    ### Remove intermediate objects
    rm("tempInput")
  } ### End if needsTemp

  ###### SLR Scenario ######
  ### Year where SLR impacts are zero
  ### Follow similar procedure to temperatures:
  ### - Select appropriate columns
  ### - Remove missing values of years, slr
  ### - Filter to appropriate years
  if(needsSLR){
    refYear_slr <- (rDataList$co_modelTypes %>% filter(modelUnitType=="slr"))$modelRefYear %>% unique
    if(has_slrUpdate){
      msg1 %>% message("Creating SLR scenario from user inputs...")
      slrInput  <- slrInput %>%
        select(c("year", "slr_cm", "scenario")) %>%
        filter(!is.na(slr_cm) & !is.na(year)) %>%
        filter( year >  refYear_slr, year <= maxYear)

      ### Interpolate over scenarios
      slr_df <- slrInput$scenario %>% unique %>%
        lapply(function(scenario_i){
          ### - Filter to scenario i and drop scenario column
          ### - Zero out series at the slr reference year
          input_i <- slrInput %>%
            filter(scenario==scenario_i) %>%
            (function(x){
              data.frame(year= refYear_slr, slr_cm = 0) %>% rbind(x)
            })

          ### Then, interpolate
          ### - Use minimum series year to determine interpolation years
          ### - Add a dummy region for National Total for interpolate_annual
          ### - Interpolate, drop dummy region, and add scenario back in
          df_i <-     input_i %>%
            (function(x){
              minYear_x <- x$year %>% min
              interpYrs <- refYear_slr:maxYear
              x_interp  <- x %>%
                mutate(region="National Total") %>%
                select(-c("scenario")) %>%
                interpolate_annual(
                  years = interpYrs, column = "slr_cm", rule = 1:2
                ) %>%
                select(-c("region"))
              return(x_interp)
            }) %>%
            mutate(scenario = scenario_i)
          return(df_i)
        }) %>%
        (function(x){do.call(rbind, x)})
      ### Remove intermediate objects
      rm("slrInput")
    }
    ### If there is no SLR scenario, calculate from temperatures
    ### First convert temperatures to global temperatures
    ### Then convert global temps to SLR
    else{
      msg1 %>% message("No temperature scenario provided...")
      msg2 %>% message("Creating SLR scenario from temperature scenario...")
      slr_df <- temp_df$scenario %>% unique %>%
        lapply(function(scenario_i){
          df_i <- temp_df %>% filter(scenario==scenario_i) %>%
            mutate(temp_C_global = temp_C %>% convertTemps(from="conus")) %>%
            (function(x){
              temps2slr(temps = x$temp_C_global, years = x$year)
            }) %>%
            mutate(scenario=scenario_i)
        }) %>%
        (function(x){do.call(rbind, x)})
    }
  } ### Ends if needsSLR


  ###### Standardize Driver Scenarios ######
  ### Get unique temperature scenarios and unique SLR scenarios
  ### Subset to desired years
  if(c_modelType=="gcm"){
    c_scenarios <- temp_df$scenario %>% unique
    drivers_df  <- temp_df %>% filter(year %in% list_years_by5) %>%
      rename(driverValue = "temp_C") %>%
      mutate(driverUnit  = "degrees Celsius")
    rm("temp_df")
  } else{
    c_scenarios  <- slr_df$scenario %>% unique
    drivers_df   <- slr_df %>% filter(year %in% list_years_by5) %>%
      rename(driverValue = "slr_cm") %>%
      mutate(driverUnit  = "cm")
    rm("slr_df")
    if(needsTemp){
      rm("temp_df")
    }
  }

  ###### Region Population Scenario ######
  ### Population inputs
  if(has_popUpdate){
    msg1 %>% message("Creating population scenario from user inputs...")
    popInput  <- popInput %>%
      select(c("year", "reg_pop")) %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
    pop_df    <- pop_df$region %>% unique %>% lapply(function(region_i){
      pop_i <- pop_df %>% filter(region==region_i)
      df_i  <- approx(pop_i$year, pop_i$reg_pop, xout=list_years_by5) %>%
        as.data.frame %>%
        mutate(year=x, reg_pop=y)
    }) %>%
      rename(region_pop = reg_pop)
    ### Remove intermediate object
    rm("popInput")
  } else{
    msg1 %>% message("No population scenario provided...")
    msg2 %>% message("Using default population scenario...")
    # utils::getFromNamespace("svPopList", "FrEDI")
    pop_df <- svPopList$iclus_region_pop %>%
      filter( year >= minYear) %>% filter( year <= maxYear)
  }

  ###### County Population Scenario ######
  msg1 %>% message("Calculating county population from regional population...")
  df_popProj <-
    calc_countyPop(
      regPop  = pop_df,
      funList = svPopList$popProjList,
      years   = list_years_by5
    ); #rm("popProjList")

  ###### Calculate Impacts ######

  ### Iterate over adaptations/variations
  df_results <-
    # 1:nrow(df_sectorInfo) %>%
    1:1 %>%
    lapply(function(i){
    sectorAbbr_i <- df_sectorInfo$impactList_fileExt[i]
    adaptLabel_i <- df_sectorInfo$adapt_label[i]
    adaptAbbr_i  <- df_sectorInfo$adapt_abbr[i]
    weightsCol_i <- df_sectorInfo$popWeightCol[i]

    msg1 %>% message("Calculating impacts for sector '", c_sector, "', adaptation '", adaptLabel_i, "'...")

    object_i     <- "impactsList" %>%
      paste(sectorAbbr_i, sep="_") %>%
      paste0(ifelse(is.na(adaptAbbr_i), "", "_")) %>%
      paste0(ifelse(is.na(adaptAbbr_i), "", adaptAbbr_i))
    svName_i     <- ifelse(c_sector=="Coastal Properties", "svDataCoastal", "svData")
    # svName_i %>% print

    ###### Iterate Over Scenarios ######
    results_i <- c_scenarios %>% lapply(function(scenario_j){
      ###### Scaled Impacts ######
      drivers_j <- drivers_df %>% filter(scenario == scenario_j) %>% select(-c("scenario"))
      ### Get impact list, calculate scaled impacts, remove impact list
      if(!exists("impactList_i")){
        file_i <- object_i %>% paste("rda", sep=".")
        load(file.path(projectPath, "data", "sv", file_i))
        # impactList_i <- get(object_i, asNamespace("FrEDI"))
        # assign("impactList_i", )
        }
      impacts_j <- calc_tractScaledImpacts(
        # funList      = impactList_i,
        funList      = eval(parse(text=object_i)),
        driverValues = drivers_j,
        .msg0        = msg2
      ) %>%
        mutate(year  = year %>% as.numeric)
      ### Remove impact list
      eval(substitute(rm(x), list(x=object_i)))
      # impacts_j %>% nrow %>% print

      impacts_j <- impacts_j %>% filter(!is.na(sv_impact))
      # impacts_j %>% nrow %>% print

      ###### Total Impacts ######
      ### Load the SV data if not loaded; remove after running
      if(!exists("svInfo")){svInfo <- svDataList[[svName_i]]}

      impacts_j      <- impacts_j %>%
        calc_tractImpacts(
          popData   = df_popProj,
          svInfo    = svInfo,
          weightCol = weightsCol_i,
          sector    = c_sector,
          svGroups  = c_svGroupTypes,
          .msg0     = msg2
        ); if(exists("svInfo")){rm("svInfo")}

      ###### Return Impacts ######
      impacts_j <- impacts_j %>% mutate(scenario = scenario_j)
      return(impacts_j)

    }) %>%
      (function(y){do.call(rbind, y)}) %>%
      mutate(adaptation = adaptLabel_i)
    return(results_i)
  }) %>%
    (function(x){do.call(rbind, x)})





  #
  #
  #   ###### Format Results ######
  #   ### SV Path, Excel path info
  #   pathSV          <- file.path(dataPath, svData_file)
  #   # excel_wb_path   <- resultsPath   %>% file.path(paste(excelTemplateFile, "xlsx", sep="."))
  #   excel_wb_path   <- resultsPath   %>% file.path(paste("FrEDI SV Graphics Template Unformatted", "xlsx", sep="."))
  #   # excel_wb_path   <- resultsPath   %>% file.path(paste("test_formatting", "xlsx", sep="."))
  #   excel_wb_exists <- excel_wb_path %>% file.exists; excel_wb_exists %>% print
  #   excel_wb_sheets <- c("FrEDI Outputs 1", "FrEDI Outputs 2")
  #   ### Initialize list
  #   df_regImpacts <- list()
  #   sysTime3      <- Sys.time()
  #   # df_formatInfo2 <- df_formatInfo[1:12,]
  #   # df_formatInfo2 <- df_formatInfo[14:23,]
  #   df_formatInfo2 <- df_formatInfo
  #   # for(i in 2:nrow(df_sectorInfo)){
  #   for(i in 4:4){
  #     sector_i       <- c_sectorList[i]
  #     df_info_i      <- df_sectorInfo %>% filter(sector == sector_i)
  #     adapt_i        <- df_info_i$adapt_abbr
  #     adaptLabel_i   <- df_info_i$adapt_label
  #     adapt_i   %>% print
  #
  #     ###### Outfile Info ######
  #     outFile_i    <- df_info_i$outputsFile[1]; outFile_i %>% print
  #     outPath_i    <- resultsPath %>% file.path(paste(outFile_i, "xlsx", sep="."))
  #     outPath_dir_exists_i <- outPath_i %>% dirname %>% dir.exists
  #     outPath_exists_i     <- outPath_i %>% file.exists
  #     overwrite_i          <- ifelse(outPath_exists_i, T, F)
  #
  #     ###### Workbook Info ######
  #     df_readme1_i <- data.frame(x=c(sector_i, as.character(Sys.Date())))
  #     if(length(adaptLabel_i)==1){
  #       adaptLabel_i <- paste(sector_i, "All Impacts", sep=", ")
  #       df_readme2_i <- data.frame(x=c(adaptLabel_i, "N/A"))
  #     } else{
  #       df_readme2_i <- data.frame(x=adaptLabel_i)
  #     }
  #     ### Open the workbook and write  ReadMe info
  #     if(excel_wb_exists){
  #       excel_wb      <- excel_wb_path %>% loadWorkbook()
  #
  #       ### Write sector, date/time, and adaptation info to workbook
  #       ### sector & date/time info
  #       excel_wb %>%
  #         writeData(
  #           x = df_readme1_i, sheet = "ReadMe", startCol = 3, startRow = 3, colNames = F
  #         )
  #
  #       ####### Write adaptation info
  #       excel_wb %>%
  #         writeData(
  #           x = df_readme2_i, sheet = "ReadMe", startCol = 3, startRow = 7, colNames = F
  #         )
  #
  #       ###### Add Styles ######
  #       # https://rdrr.io/cran/openxlsx/man/addStyle.html
  #       for(k in 1:nrow(df_formatInfo2)){
  #         # k %>% print
  #
  #         df_info_k <- df_formatInfo2[k,] %>% as.data.frame
  #         format_k  <- df_info_k$styleName[1]
  #         sheet_k   <- df_info_k$worksheet[1] #; sheet_k %>% print
  #         style_k   <- list_styles[[format_k]]
  #
  #
  #         rows_k    <- (df_info_k$first_row[1]):(df_info_k$end_row[1])
  #         cols_k    <- (df_info_k$first_col[1]):(df_info_k$end_col[1])
  #
  #         excel_wb %>% addStyle(
  #           style = style_k,
  #           sheet = sheet_k, rows = rows_k, cols = cols_k,
  #           gridExpand = T, stack = T
  #         )
  #       }; rm("df_info_k", "style_k", "sheet_k", "rows_k", "cols_k")
  #     } ### End if excel_wb_exists
  #
  #     ###### Save Data ######
  #     if(excel_wb_exists){
  #       ### Save data
  #       if(outPath_dir_exists_i){
  #         "Saving workbook..." %>% message
  #         excel_wb %>% saveWorkbook(file=outPath_i, overwrite = overwrite_i)
  #       } ### End if outPath_dir_exists
  #     } ### End if excel_wb_exists
  #   } ### end i
  #   ### Remove iteration objects
  #   # rm("sector_i", "df_info_i", "adapt_i", "adaptLabel_i", "df_readme1_i", "df_readme2_i")
  #   # rm("outFile_i", "outPath_i", "outPath_dir_exists_i", "outPath_exists_i", "overwrite_i", "excel_wb")
  #   ### System time
  #   sysTime4 <- Sys.time()
  #   sysTime4 - sysTime3
  #
  ###### Convert to Dataframe ######
  df_results   <- df_results %>% ungroup %>% as.data.frame
  ###### Return Object ######
  message("\n", "Finished", ".")
  return(df_results)

} ### End function








