### Last updated: 2021.02.10
### 2021.02.10: Updated to process SLRs separately from other sectors
### The purpose of this function is to import data from Excel and use this data to create and save R data objects.
createSystemData <- function(
    dataList    = list(), ### List of data created by reshapeData
    outPath     = "." %>% file.path("data", "sysdata.rda"),
    configPath  = "." %>% file.path("R"   , "fredi_config.R"), ### Path to config file
    save        = FALSE,
    silent      = FALSE,  ### Whether to message the user
    return      = TRUE
){
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user) and save behavior
  msgUser     <- !silent
  
  ###### Initialize Data List ######
  ### Initialize list of data to save
  rDataList   <- list()
  
  ###### Create File Paths ######
  ### Output file
  outPath     <- ifelse(outPath %>% is.null, "." %>% file.path("data", "sysdata.rda"), outPath)
  sysDataFile <- outPath %>% basename()
  sysDataPath <- outPath %>% dirname()
  sysDataFile <- sysDataPath %>% file.path(sysDataFile)
  # sysDataFile %>% print
  ### Config file
  configPath <- ifelse(configPath %>% is.null, "." %>% file.path("R", "fredi_config.R"), configPath)
  configFile <- configPath %>% basename()
  configPath <- configPath %>% dirname()
  configFile <- configPath %>% file.path(configFile)
  # configFile %>% print
  
  ###### Configuration Data ######
  ### Read in configuration data and assign data tables to objects in the list
  if(msgUser) {paste0("Loading config info from '", configFile, "'...") %>% message()}
  # if(msgUser) {messages_data[["loadInputs"]]$try %>% message()}
  configFile %>% source
  # configFile %>% file.exists %>% print; fredi_config %>% print; fredi_config %>% names %>% print
  for(name_i in names(fredi_config)) {assign(name_i, fredi_config[[name_i]]); rm("name_i")}

  ###### Import Functions to Namespace ######
  interpolate_annual  <- utils::getFromNamespace("interpolate_annual" , "FrEDI")
  match_scalarValues  <- utils::getFromNamespace("match_scalarValues" , "FrEDI")
  get_impactFunctions <- utils::getFromNamespace("get_impactFunctions", "FrEDI")
  convertTemps        <- utils::getFromNamespace("convertTemps"       , "FrEDI")
  temps2slr           <- utils::getFromNamespace("temps2slr"          , "FrEDI")
  
  ###### Assign Data Objects ###### 
  ###### This section reads in data from the data file and returns a list of tables
  ###### Table names physScalars_table1, econScalars_table1, outputs_table1, co_sectors
  ### Add loaded tables to data list
  ### Load data from file
  loadDataList <- dataList 
  rDataList    <- rDataList %>% c(loadDataList)
  if(msgUser) {paste0("\t", messages_data[["loadInputs"]]$success) %>% message()}
  for(name_i in names(loadDataList)) {assign(name_i, loadDataList[[name_i]]); rm("name_i")} 
  
  ###### Sector Info ######
  ### Exclude some sectors, get the number of sectors and sector info
  ### Sector info with additional sector info: df_sectorsInfo
  ### Sector info with models: df_sectorsModels
  num_sectors  <- co_sectors %>% nrow #; num_sectors
  sector_ids   <- co_sectors$sector_id
  
  ###### Default Driver Scenarios ######
  ### Get reference years and add to fredi_config
  refYear_temp <- (co_modelTypes %>% filter(modelUnitType=="temperature"))$modelRefYear %>% unique
  refYear_slr  <- (co_modelTypes %>% filter(modelUnitType=="slr"        ))$modelRefYear %>% unique
  fredi_config[["refYear_temp"]] <- refYear_temp
  fredi_config[["refYear_slr" ]] <- refYear_slr
  ### Columns, years for interpolation
  tempCols     <- c("year", "temp_C_conus", "temp_C_global")
  drop0        <- c("region")
  years0       <- refYear_temp:maxYear
  ### Create initial year values
  df_refTemp   <- tibble(year = refYear_temp, temp_C_conus = 0)
  df_refTemp   <- df_refTemp %>% mutate(temp_C_global = temp_C_conus %>% convertTemps(from="conus"))
  df_refSlr    <- tibble(year = refYear_slr, slr_cm = 0)
  ### Zero out CONUS values for temperature at reference year and interpolate annual values
  ### Calculate global temperatures and update in list
  temp_default <- co_defaultTemps %>% filter(year > refYear_temp)
  temp_default <- df_refTemp      %>% rbind(temp_default)
  temp_default <- temp_default    %>% interpolate_annual(years = years0, column="temp_C_conus", rule=1:2)
  temp_default <- temp_default    %>% mutate(temp_C_global = temp_C_conus %>% convertTemps(from="conus"))
  temp_default <- temp_default    %>% select(-c(all_of(drop0)))
  rDataList[["temp_default"]] <- temp_default
  # temp_default %>% glimpse
  
  ### Calculate annual values for SLR from global temperatures and add to list
  ### Ref year not needed since temps2slr will zero out values
  slr_default  <- temps2slr(temps=temp_default$temp_C_global, years=temp_default$year)
  rDataList[["slr_default"]] <- slr_default
  # slr_default %>% names %>% print
  ### Remove intermediate values
  rm("tempCols", "drop0"); rm("temp_default", "slr_default")
  
  ###### Default Socioeconomic Scenario ######
  ### Columns
  gdpCols     <- c("year", "gdp_usd")
  popCols     <- c("year", "region", "reg_pop")
  group0      <- c("year")
  sum0        <- c("reg_pop")
  drop0       <- c("region")
  nationalDot <- c("National.Total")
  
  ### Interpolate annual values for GDP:
  ### Filter to first unique region
  ### Select GDP columns and add national total
  gdp_default <- co_defaultScenario %>% filter(region==co_regions$region[1])
  gdp_default <- gdp_default        %>% select(c(all_of(gdpCols))) %>% mutate(region=nationalDot)
  gdp_default <- gdp_default        %>% interpolate_annual(years = list_years, column = "gdp_usd", rule=1:2)
  gdp_default <- gdp_default        %>% select(-c(all_of(drop0)))
  rDataList[["gdp_default"]] <- gdp_default
  
  ### Interpolate annual values for population and add to data list
  pop_default <- co_defaultScenario %>% select(c(all_of(popCols)))
  pop_default <- pop_default        %>% interpolate_annual(years = list_years, column="reg_pop", rule=1:2)
  rDataList[["pop_default"]] <- pop_default
  # pop_default %>% names %>% print
  
  ### Calculate national population and add to data list
  df_national <- pop_default %>% 
    group_by_at(.vars=c(all_of(group0))) %>% 
    summarize_at(.vars=c(all_of(sum0)), sum, na.rm=T) %>% ungroup
  df_national <- df_national %>% rename_at(.vars=c(all_of(sum0)), ~c("national_pop"))
  rDataList[["national_pop_default"]] <- df_national
  # df_national %>% names %>% print
 
  ### Default scenario: Join national GDP with national population by year
  ### Default scenario: Join national values with regional population by year
  ### Calculate GDP per capita and add to list
  df_national <- gdp_default %>% left_join(df_national, by=c(all_of(group0)))
  df_national <- df_national %>% left_join(pop_default, by=c(all_of(group0)))
  df_national <- df_national %>% mutate(gdp_percap = gdp_usd / national_pop)
  rDataList[["df_defaultScenario"]] <- df_national
  # df_defaultScenario %>% names %>% print
  ### Drop intermediate values
  rm("gdpCols", "popCols", "group0", "sum0", "drop0", "nationalDot")
  rm("gdp_default", "pop_default")
  
  ###### Extreme SLR Scenarios ######
  ### replace NA values and convert to character
  # slr_cm %>% names %>% print; slrImpacts %>% names %>% print; 
  mutate0     <- c("model_type")
  string0     <- c("slr")
  ### Replace NA values
  slr_cm      <- slr_cm     %>% mutate_at(.vars = c(all_of(mutate0)), replace_na, string0)
  slrImpacts  <- slrImpacts %>% mutate_at(.vars = c(all_of(mutate0)), replace_na, string0)
  ### Convert to character
  slr_cm      <- slr_cm     %>% mutate_at(.vars = c(all_of(mutate0)), as.character)
  slrImpacts  <- slrImpacts %>% mutate_at(.vars = c(all_of(mutate0)), as.character)
  ### Create data for extreme values above 250cm
  slrExtremes <- fun_slrConfigExtremes(
    slr_x = slr_cm,    ### rDataList$slr_cm
    imp_x = slrImpacts ### rDataList$slrImpacts
  )
  
  ###### Interpolate SLR Scenarios ######
  ### Extend SLR Heights, Impacts, and Extremes
  c_cm         <- c("model", "year")
  c_imp        <- c("sector", "variant", "impactType", "impactYear", "region", "year")
  slr_cm       <- slr_cm      %>% extend_slr(arrange_x=c_cm)
  slrImpacts   <- slrImpacts  %>% extend_slr(arrange_x=c_imp)
  slrExtremes  <- slrExtremes %>% extend_slr(arrange_x=c_imp)
  ### Update in data list and remove objects
  rDataList[["slr_cm"     ]] <- slr_cm
  rDataList[["slrImpacts" ]] <- slrImpacts
  rDataList[["slrExtremes"]] <- slrExtremes
  rm("slr_cm", "slrImpacts", "slrExtremes"); rm("c_cm", "c_imp")
  
  ###### Format Scalar Tables ######
  ### Interpolate values to annual levels
  # scalarDataframe %>% names %>% print
  df_mainScalars <- fun_formatScalars(
    data_x  = scalarDataframe, ### rDataList$scalarDataframe
    info_x  = co_scalarInfo,   ### rDataList$co_scalarInfo
    years_x = list_years       ### rDataList$list_years
  )
  rDataList[["df_mainScalars"]] <- df_mainScalars
  
  ###### Physical and Economic Scalars ######
  ### Physical scalars: Get population weights, then physical scalar multipliers
  ### Economic scalars: Get economic scalars, then multipliers
  if(msgUser) {messages_data[["calcScalars"]]$try %>% message()}
  
  ### Initialized results: Join sector info and default scenario
  join0          <- c("joinCol")
  df_sectorsInfo <- df_sectorsInfo %>% mutate(joinCol=1)
  df_national    <- df_national    %>% mutate(joinCol=1)
  df_results0    <- df_sectorsInfo %>% left_join(df_national, by=c(all_of(join0)))
  df_results0    <- df_results0    %>% select(-c(all_of(join0)))
  rm("join0"); rm("df_sectorsInfo", "df_national")
  
  ### Physical adjustment
  # df_mainScalars %>% names %>% print; df_results0 %>% names %>% print
  df_results0 <- df_results0  %>% match_scalarValues(df_mainScalars, scalarType="physAdj")
  ### Damage adjustment
  df_results0 <- df_results0  %>% match_scalarValues(df_mainScalars, scalarType="damageAdj")
  ### Economic scalar
  df_results0 <- df_results0  %>% match_scalarValues(df_mainScalars, scalarType="econScalar")
  ### Drop extra columns and add to data list
  drop0       <- c("gdp_usd", "reg_pop", "national_pop", "gdp_percap")
  df_results0 <- df_results0  %>% select(-c(all_of(drop0)))
  rDataList[["df_results0"]] <- df_results0
  ### Message the user
  if(msgUser) {paste0("\t", messages_data[["calcScalars"]]$success) %>% message()}

  ###### Get Scenario Info for Scaled Impacts  ######
  ### Add a column with a scenario id
  ### Get list of scenarios for scenarios with at least some non-NA values
  ### Add information on non-missing scenarios to scaled impacts data
  # data_scaledImpacts %>% glimpse
  data_scaledImpacts <- data_scaledImpacts %>% get_scenario_id(include=c("model_dot", "region_dot"))
  c_scenariosList    <- data_scaledImpacts %>% filter(!is.na(scaledImpact)) %>% get_uniqueValues(column="scenario_id")
  data_scaledImpacts <- data_scaledImpacts %>% mutate(hasScenario = (scenario_id %in% c_scenariosList))
  # rm("c_scenariosList")
  ### Update in data list
  rDataList[["data_scaledImpacts"]] <- data_scaledImpacts
  
  ###### Get Interpolation Functions for Scenarios ######
  ### Iterate over sectors to get interpolation functions with fun_getImpactFunctions()
  ### fun_getImpactFunctions depends on the function fun_tempImpactFunction()
  if(msgUser) {message("\t", messages_data[["interpFuns"]]$try)}
  df_hasScenario       <- data_scaledImpacts %>% filter(hasScenario)
  df_hasScenario       <- df_hasScenario     %>% ungroup %>% as.data.frame
  c_modelTypes         <- c("gcm")
  list_impactFunctions <- list()
  ### Max output value, maximum extrapolation value, unit scale,   extend type
  df_gcm         <- (co_modelTypes %>% filter(modelType_id==c_modelTypes))
  maxOutput_gcm  <- df_gcm[["modelMaxOutput"]][1]
  maxExtrap_gcm  <- df_gcm[["modelMaxExtrap"]][1]
  unitScale_gcm  <- df_gcm[["modelUnitScale"]][1]
  ### Format data
  
  ### Get functions
  functions_gcm  <- df_hasScenario %>% get_impactFunctions(
    groupCol    = "scenario_id",
    xCol        = "modelUnitValue",
    yCol        = "scaledImpact",
    extend_from = maxOutput_gcm,
    extend_to   = maxExtrap_gcm,
    extend_all  = FALSE,
    unitScale   = unitScale_gcm
  ) 
  # paste(modelType_i, length(functions_i)) %>% print
  ### Add values to list and remove intermediate values
  list_impactFunctions <- list_impactFunctions %>% c(functions_gcm)
  rDataList[["list_impactFunctions"]] <- list_impactFunctions
  rm("c_modelTypes", "maxOutput_gcm", "maxExtrap_gcm", "unitScale_gcm")
  rm("df_gcm", "df_hasScenario", "functions_gcm", "list_impactFunctions")
  
  ###### Aggregate R Data objects ######
  ### Message the user
  if(msgUser) {paste0("\t", messages_data[["interpFuns"]]$success) %>% message()}
  
  # ### Make a new data list
  # rDataList <- list(
  #   "gdp_default"               = gdp_default,
  #   "pop_default"               = pop_default,
  #   "national_pop_default"      = national_pop_default,
  #   "df_mainScalars"            = df_mainScalars,
  #   "df_results0"               = df_results0,
  #   "list_impactFunctions"      = list_impactFunctions
  # )
  # ### Combine with the data list
  # rDataList <- loadDataList %>% c(rDataList)
  rDataList[["fredi_config"]] <- fredi_config
  
 
  ###### Save R Data objects ######
  ### If save:
  ### - Message the user
  ### - Check if the output file directory exists
  ### - If the outpath exists, try to save the file
  if(save) {
    paste0("\n", "Saving results to ", sysDataPath, "...") %>% message()
    outPathExists <- sysDataPath %>% dir.exists
    expr0         <- try(save(fredi_config, rDataList, file=sysDataFile), silent=T)
    if(outPathExists) {trySave <- expr0 %>% eval} ### End if outPathExists
  } ### End if save
  # rDataList %>% names %>% print
  
  ###### Return object ######
  paste0("\n\n", "Finished", ".") %>% message()
  
  return(rDataList)
} ### End function


### Uncomment following two lines to create and save data and check the outputs
# test_systemData <- createSystemData(save=F)
# rm("createSystemData")


