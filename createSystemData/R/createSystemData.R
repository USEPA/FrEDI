### Last updated: 2021.02.10
### 2021.02.10: Updated to process SLRs separately from other sectors
### The purpose of this function is to import data from Excel and use this data to create and save R data objects.
createSystemData <- function(
  projectPath = NULL, ### Path to project
  excelName   = NULL, ### name of excel file with config information
  outPath     = NULL,
  save        = NULL,
  silent      = NULL  ### Whether to message the user
){
  ###### Set up the environment ######
  ### Level of messaging (default is to message the user) and save behavior
  silent  <- ifelse(is.null(silent), F, silent)
  msgUser <- !silent
  save    <- ifelse(is.null(save), F, save)

  ###### Create File Paths ######
  projectPath <- ifelse(is.null(projectPath), ".", projectPath)
  ### Excel configuration file
  extDataPath <- file.path(projectPath, "inst", "extdata")
  excelName   <- ifelse(is.null(excelName), "FrEDI_config.xlsx", excelName)
  extDataFile <- extDataPath %>% file.path(excelName)
  
  ### Output file
  sysDataPath <- projectPath %>% file.path("data")
  sysDataFile <- sysDataPath %>% file.path("sysdata.rda")
  sysDataFile <- ifelse(!is.null(outPath), outPath, sysDataFile)
  
  ###### Configuration Data ######
  ### Read in configuration data
  ### Assign data tables to objects in the list
  configFile <- projectPath %>% file.path("R", "fredi_config.R")
  source(configFile)
  # configFile %>% file.exists %>% print
  # fredi_config %>% print
  # fredi_config %>% names %>% print
  for(i in 1:length(fredi_config)){
    assign(names(fredi_config)[i], fredi_config[[i]])
  }
  
  ###### Import Functions from ciraTempBin ######
  interpolate_annual  <- utils::getFromNamespace("interpolate_annual", "FrEDI")
  match_scalarValues  <- utils::getFromNamespace("match_scalarValues", "FrEDI")
  get_impactFunctions <- utils::getFromNamespace("get_impactFunctions", "FrEDI")
  
  ###### Load input data ######
  ###### This section reads in data from the data file and returns a list of tables
  ###### Table names physScalars_table1, econScalars_table1, outputs_table1, co_sectors
  if(msgUser){message("Loading Excel file from '", extDataPath, "'...")}
  if(msgUser){message(messages_data[["loadInputs"]]$try)}
  ### Load data from file
  loadDataList    <- loadData(
    fileName  = extDataFile,
    sheetName = "tableNames",
    silent    = silent
    )

  if(msgUser){message("\t", messages_data[["loadInputs"]]$success)}

  ### Assign data tables to objects in the list
  for(i in 1:length(loadDataList)){
    assign(names(loadDataList)[i], loadDataList[[i]])
  } ### End iterate over i

  ###### Sector Info ######
  ### Exclude some sectors, get the number of sectors and sector info
  ### Sector info with additional sector info: df_sectorsInfo
  ### Sector info with models: df_sectorsModels
  num_sectors  <- co_sectors %>% nrow #; num_sectors
  sector_ids   <- co_sectors$sector_id

  ###### Interpolate Default Scenario ######
  ### Interpolate annual values and calculate GDP per capita
  gdp_default <- co_defaultScenario %>% filter(region==co_regions$region[1]) %>%
    select(year, gdp_usd) %>% mutate(region="National.Total") %>%
    interpolate_annual(years= list_years, column = "gdp_usd", rule=1:2)

  pop_default <- co_defaultScenario %>%
    select(year, reg_pop, region) %>%
    interpolate_annual(years= list_years, column="reg_pop", rule=1:2)

  national_pop_default <- pop_default %>%
    group_by(year) %>%
    summarize_at(.vars=c("reg_pop"), sum, na.rm=T) %>%
    rename(national_pop = reg_pop) %>%
    mutate(region="National.Total")

  # ### National scenario
  national_scenario_default <- gdp_default %>%
    left_join(national_pop_default, by=c("year", "region")) %>%
    mutate(gdp_percap = gdp_usd/national_pop)

  ### Join GDP with national totals and population
  df_defaultScenario <- national_scenario_default %>%
    select(-region) %>%
    left_join(pop_default, by = "year")
  rm("national_scenario_default")

  ###### Format Scalar Tables ######
  ### Interpolate values to annual levels
  scalarNameList <- scalarDataframe$scalarName %>% unique
  numScalarNames <- scalarNameList %>%length
  df_mainScalars <- 1:numScalarNames %>%
    lapply(function(i){
      name_i     <- scalarNameList[i]
      which_i    <- which(co_scalarInfo$scalarName==name_i)
      method_i   <- co_scalarInfo$constant_or_dynamic[which_i]
      region_i   <- co_scalarInfo$national_or_regional[which_i]
      # name_i %>% print
      
      ### Constant or linear
      method_i   <- ifelse(method_i=="constant", method_i, "linear")
      scalar_i   <- scalarDataframe %>% filter(scalarName==name_i) %>%
        interpolate_annual(
          years  = list_years,
          column = "value",
          rule   = 1:2,
          method = method_i) %>%
        mutate(scalarName=name_i)
      return(scalar_i)
    }) %>%
    # (function(x){do.call(rbind, x)}) ; df_allScalars %>% glimpse
    (function(x){do.call(rbind, x)}) %>%
    ###### Add regional population and gdp_percap to scalars
    select(c("scalarName", "region", "year", "value")) %>%
    ### Bind the scalar info to the scalar data
    left_join(co_scalarInfo %>% select(c("scalarName", "scalarType", "national_or_regional")), by=c("scalarName"))



  ###### Physical and Economic Scalars ######
  ### Physical scalars: Get population weights, then physical scalar multipliers
  ### Economic scalars: Get economic scalars, then multipliers
  if(msgUser){message(messages_data[["calcScalars"]]$try)}

  ### Initialized results
  df_results0 <- df_sectorsInfo %>% mutate(dummyCol=1) %>%
    left_join(df_defaultScenario %>% mutate(dummyCol=1), by="dummyCol") %>%
    select(-dummyCol)
  rm("df_defaultScenario")
  ### Physical adjustment
  df_results0 <- df_results0  %>% match_scalarValues(df_mainScalars, scalarType="physAdj")
  ### Damage adjustment
  df_results0 <- df_results0  %>% match_scalarValues(df_mainScalars, scalarType="damageAdj")
  ### Economic scalar
  df_results0 <- df_results0  %>% match_scalarValues(df_mainScalars, scalarType="econScalar")
  ### Drop extra columns
  df_results0 <- df_results0 %>% select(-c("gdp_usd", "reg_pop", "national_pop", "gdp_percap"))
  ### Message the user
  if(msgUser){message("\t", messages_data[["calcScalars"]]$success)}

  ###### Get Scenario Info for Scaled Impacts  ######
  ### Refactor adaptation, impact years, impact types
  ### Add a column with a scenario id
  data_scaledImpacts <- data_scaledImpacts %>%
    mutate(
      scenario_id = paste(sector, variant, impactYear, impactType, model_type, model_dot, region_dot, sep="_")
      )

  ### Get list of scenarios for scenarios with at least some non-NA values
  impactScenariosList <- (
    data_scaledImpacts %>%
      filter(!is.na(scaledImpact)) %>%
      as.data.frame
    )$scenario_id %>% unique

  ### Add information on non-missing scenarios to scaled impacts data
  data_scaledImpacts    <- data_scaledImpacts %>%
    mutate(hasScenario = (scenario_id %in% impactScenariosList))

  ###### Get Interpolation Functions for Scenarios ######
  ### Iterate over sectors to get interpolation functions with fun_getImpactFunctions()
  ### fun_getImpactFunctions depends on the function fun_tempImpactFunction()
  if(msgUser){message("\t", messages_data[["interpFuns"]]$try)}

  df_hasScenario         <- data_scaledImpacts %>% filter(hasScenario)
  list_impactFunctions   <- list()

  # c_modelTypes   <- co_modelTypes$modelType_id
  c_modelTypes   <- c("gcm")
  for(modelType_i in c_modelTypes){
    ### Max output value, maximum extrapolation value, unit scale, extend type
    df_model_i <- (co_modelTypes %>% filter(modelType_id==modelType_i))
    maxOutput_i  <- df_model_i$modelMaxOutput %>% unique
    maxExtrap_i  <- df_model_i$modelMaxExtrap %>% unique
    unitScale_i  <- df_model_i$modelUnitScale %>% unique

    df_i        <- df_hasScenario %>% filter(model_type==modelType_i) %>% ungroup %>% as.data.frame

    functions_i <- df_i  %>%
      get_impactFunctions(
        groupCol    = "scenario_id",
        xCol        = "modelUnitValue",
        yCol        = "scaledImpact",
        extend_from = maxOutput_i,
        extend_to   = maxExtrap_i,
        unitScale   = unitScale_i
      ); # paste(modelType_i, length(functions_i)) %>% print
    ### Add values to list
    list_impactFunctions <- c(list_impactFunctions, functions_i)
  }

  ###### Aggregate R Data objects ######
  ### Message the user
  if(msgUser){message("\t", messages_data[["interpFuns"]]$success)}

  ### Update data list:
  loadDataList[["data_scaledImpacts"]] <- data_scaledImpacts
  ### Make a new data list
  rDataList <- list(
    "gdp_default"               = gdp_default,
    "pop_default"               = pop_default,
    "national_pop_default"      = national_pop_default,
    "df_mainScalars"            = df_mainScalars,
    "df_results0"               = df_results0,
    "list_impactFunctions"      = list_impactFunctions
  )
  ### Combine with the data list
  rDataList <- c(loadDataList, rDataList)

  ###### Save R Data objects ######
  ### If save:
  ### - Message the user
  ### - Check if the output file directory exists
  ### - If the outpath exists, try to save the file
  if(save){
    message("\n", "Saving results to ", sysDataPath, "...")

    outPathExists <- sysDataPath %>% dir.exists

    if(outPathExists){

      trySave <- try(
        save(fredi_config, rDataList, file=sysDataFile),
        silent=T
      )
    } ### End if outPathExists
  } ### End if save

  # rDataList %>% names %>% print

  ###### Return object ######
  message("\n\n", "Finished", ".")

  return(rDataList)
} ### End function


### Uncomment following two lines to create and save data and check the outputs
# test_systemData <- createSystemData(save=F)
# rm("createSystemData")


