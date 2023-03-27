#' reshapeData
#'
#' @param dataList Outputs from `loadData`
#' @param silent Indicate level of messaging
#'
#' @return
#' @export
#'
#' @examples
reshapeData <- function(
    dataList = NULL,
    silent = T
) {
  ###### Assign Objects ######
  ### Assign tables in dataList to object in local environment
  listNames <- dataList %>% names
  # listNames %>% print
  for(name_i in listNames) {name_i %>% assign(dataList[[name_i]],envir = .GlobalEnv)}
  
  
  ###### Modify Tables and Update in List  ######
  ###### ** Sectors     ######
  ### Filter to those tables to include
  ### Make a copy of the sectors list to include variants
  ### Drop variant column from `co_sectors`
  drop0         <- c("include", "variants", "impactYears", "impactTypes")
  co_sectors    <- co_sectors %>% filter(include == 1)
  co_sectorsRef <- co_sectors
  co_sectors    <- co_sectors %>% select(-c(all_of(drop0)))
  ### Update values in list
  dataList[["co_sectors"   ]] <- co_sectors
  dataList[["co_sectorsRef"]] <- co_sectorsRef
  ### Remove intermediate variables
  rm("drop0")
  
  ###### ** Misc ######
  ### No changes to variants
  ### No changes to model types
  ### No changes to input scenario info
  
  ###### ** Impact Years ######
  ### Load data and gather the data by impact year levels
  ### Levels for factoring are impact year levels: `N/A`, `2010`, `2090`, Interpolate
  ### Replace special characters in impact year levels
  drop0 <- c("sector_id")
  drop1 <- c("impactYear2")
  join0 <- c("impactYear_label")
  c_impactYearLevels  <- co_impactYears %>% select(-c(all_of(drop0))) %>% names()
  co_impactYearLevels <- data.frame(impactYear_label = c_impactYearLevels)
  co_impactYearLevels <- co_impactYearLevels %>% mutate(impactYear_id = gsub("/", "", impactYear_label))
  co_impactYearLevels <- co_impactYearLevels %>% mutate(impactYear_excel = gsub("NA", "", impactYear_id))
  
  ### Reshape impact years: gather the data by impact year levels
  ### Drop values with missing `impactYear2`
  co_impactYears <- co_impactYears %>% gather(key = "impactYear_label", value = "impactYear2", -c(all_of(drop0)))
  co_impactYears <- co_impactYears %>% filter(!is.na(impactYear2))
  co_impactYears <- co_impactYears %>% select(-c(all_of(drop1)))
  co_impactYears <- co_impactYears %>% left_join(co_impactYearLevels, by = c(all_of(join0)))
  # co_impactYears %>% glimpse
  ### Update/add tables in list
  dataList[["co_impactYears"     ]] <- co_impactYears
  dataList[["co_impactYearLevels"]] <- co_impactYearLevels
  ### Remove intermediate variables
  rm("drop0", "drop1", "join0")
  
  ###### ** Impact Types Info ######
  ### Drop damage adjustment names (present in variants)
  ### Update in list
  ### Remove intermediate variables
  drop0          <- c("damageAdjName")
  co_impactTypes <- co_impactTypes %>% select(-c(all_of(drop0))) # ; co_impactTypes %>% glimpse
  dataList[["co_impactTypes"]] <- co_impactTypes
  rm("drop0")
  
  ###### ** co_models ######
  ### Combine with model types and update data list
  ### Update in list
  ### Remove intermediate variables
  join0          <- c("modelType")
  co_modelTypes2 <- co_modelTypes %>% rename(modelType = modelType_id)
  co_models      <- co_models     %>% left_join(co_modelTypes2, by = c(all_of(join0))) # ; co_models %>% glimpse
  dataList[["co_models"]] <- co_models
  rm("join0", "co_modelTypes2")
  
  ###### ** co_regions ######
  ### Combine and add to data list...also a copy with info on national data
  ### Update in list
  co_regions <- co_regions %>% mutate(region = region_dot)
  dataList[["co_regions"]] <- co_regions
  
  ###### ** df_sectorsInfo ######
  ### Join sector and other info
  ### Add additional sector info and add to data list
  drop0   <- c("variant_id_excel", "variant_label")
  drop1   <- c("impactYear_label", "impactYear_excel")
  drop2   <- c("impactType_id_excel", "impactType_label", "impactType_description")
  join0   <- c("sector_id")
  rename0 <- c("sector", "variant", "impactYear", "impactType") %>% paste("id", sep = "_")
  rename1 <- gsub("_id", "", c(all_of(rename0)))
  ### Join with co_impactYears, co_impactTypes
  df_sectorsInfo <- co_sectors     %>% left_join(co_variants    %>% select(-c(all_of(drop0))), by = c(all_of(join0)))
  df_sectorsInfo <- df_sectorsInfo %>% left_join(co_impactYears %>% select(-c(all_of(drop1))), by = c(all_of(join0)))
  df_sectorsInfo <- df_sectorsInfo %>% left_join(co_impactTypes %>% select(-c(all_of(drop2))), by = c(all_of(join0)))
  ### Rename
  df_sectorsInfo <- df_sectorsInfo %>% rename_at(.vars = c(all_of(rename0)), ~ all_of(rename1)) # ; df_sectorsInfo %>% glimpse
  ### Update in list
  dataList[["df_sectorsInfo"]] <- df_sectorsInfo
  ### Remove intermediate variables
  rm("drop0", "drop1", "drop2", "join0", "rename0", "rename1")
  
  ###### ** Temperature Scenario ######
  ### ** Initialize Temperature Scenario
  ### Add temp_C_conus
  ### Update in list
  co_defaultTemps <- co_defaultTemps %>% mutate(temp_C_conus = temp_C_global %>% (function(x){FrEDI::convertTemps(x, from="global")}))
  dataList[["co_defaultTemps"]] <- co_defaultTemps #%>% select(-c("temp_C_conus"))
  
  ###### ** Socioeconomic Scenario ######
  ### Gather the columns from the default scenario and update data list
  idCols0 <- c("year", "gdp_usd")
  nChar0  <- nchar("pop_") + 1 ### 5
  co_defaultScenario <- co_defaultScenario %>% gather(key = "region", value = "reg_pop", -c(all_of(idCols0)))
  co_defaultScenario <- co_defaultScenario %>% mutate(region = region %>% substr(nChar0, nchar(region)))
  # gather_defaultScenario %>% glimpse
  ### Update in list
  ### Remove intermediate variables
  dataList[["co_defaultScenario"]] <- co_defaultScenario
  rm("idCols0", "nChar0")
  
  ###### ** GCM Scaled Impacts ######
  ### Remove empty rows and GCM and SLR Averages
  ### Update in list
  ### Remove intermediate variables
  filter0 <- co_models$model_id %>% unique()
  filter1 <- co_sectors$sector_id %>% unique()
  data_scaledImpacts <- data_scaledImpacts %>% filter(model  %in% filter0) ### All models
  data_scaledImpacts <- data_scaledImpacts %>% filter(sector %in% filter1)
  rm("filter0", "filter1")
  
  ###### ** SLR Scenario Info ######
  ### Gather slr_cm columns
  ### Substitute special characters in model name
  ### Make a copy of model called model_dot
  ### Factor model dot by model type
  # slr_cm %>% names %>% print
  idCols0 <- c("year")
  levels0 <- co_models$model_dot
  labels0 <- co_models$modelType
  slr_cm  <- slr_cm %>% gather(value = "driverValue", key = "model", -c(all_of(idCols0)))
  slr_cm  <- slr_cm %>% mutate(model = gsub("\\_", "", model))
  slr_cm  <- slr_cm %>% mutate(model_dot = model)
  slr_cm  <- slr_cm %>% mutate(model_type = model_dot %>% factor(levels0, labels0))
  # slr_cm %>% names %>% print
  ### Update list
  ### Remove intermediate variables
  dataList[["slr_cm"]] <- slr_cm
  rm("idCols0", "levels0", "labels0")
  
  ###### ** SLR Impacts ######
  # slrImpacts %>% names %>% print
  ### Remove special characters from region, model
  idCols0 <- c("year", "sector", "variant", "impactType", "impactYear")
  select0 <- c("region_slr")
  names0  <- c("region", "model")
  join0   <- c("row_id")
  slrImpacts <- slrImpacts %>% gather(value = "scaled_impacts", key = "region_slr", -c(all_of(idCols0)))
  ### Make dataframe with region_slr, region, model and join with slrImpacts
  ### Mutate model
  c_regSlr <- slrImpacts[[select0]] %>% str_split("_") %>% (function(i) {do.call(rbind, i)})
  # c_regSlr %>% head %>% print
  c_regSlr <- c_regSlr %>% (function(x) {colnames(x) <- names0; return(x)})
  c_regSlr <- c_regSlr %>% as_tibble()
  c_regSlr <- c_regSlr %>% mutate(model = gsub("\\.", "_", model) %>% paste0("cm"))
  ### Add row numbers and join
  c_regSlr   <- c_regSlr   %>% mutate(row_id = row_number())
  slrImpacts <- slrImpacts %>% mutate(row_id = row_number())
  ### Check impacts
  "nrow(c_regSlr) == nrow(slrImpacts)" %>% paste0(": ", nrow(c_regSlr) == nrow(slrImpacts)) %>% print()
  ### Join and drop join columns
  slrImpacts <- slrImpacts %>% left_join(c_regSlr, by = c(all_of(join0)))
  slrImpacts <- slrImpacts %>% select(-c(all_of(join0), all_of(select0)))
  ### Drop intermediate values (update in list further down)
  rm("idCols0", "select0", "names0", "join0", "c_regSlr")
  
  
  ###### ** Format Scaled Impacts ######
  list_scaledImpacts <- list(data_scaledImpacts = data_scaledImpacts, slrImpacts = slrImpacts)
  rm("data_scaledImpacts", "slrImpacts")
  
  list_scaledImpacts <- list_scaledImpacts %>%
    names() %>%
    lapply(function(name_i, list_x = list_scaledImpacts) {
      ## Data frame
      df_i <- list_x[[name_i]]
      ### Replace NA values in impactYear, impactType
      df_i <- df_i %>% mutate(impactYear = impactYear %>% as.character() %>% replace_na("N/A"))
      df_i <- df_i %>% mutate(impactType = impactType %>% as.character() %>% replace_na("NA"))
      ### Refactor variants, impact estimate years, and impact types
      ### Refactor variants (by sector, variant)
      levels0 <- co_variants$sector_id %>% paste(co_variants$variant_id_excel, sep = "_")
      labels0 <- co_variants$variant_id
      select0 <- c("sector_variant")
      df_i <- df_i %>% mutate(sector_variant = sector %>% paste(variant, sep = "_"))
      df_i <- df_i %>% mutate(sector_variant = sector_variant %>% factor(levels0, labels0))
      df_i <- df_i %>% mutate(variant = sector_variant)
      df_i <- df_i %>% select(-c(all_of(select0)))
      rm("levels0", "labels0", "select0")
      ### Refactor impact years
      levels0 <- co_impactYearLevels$impactYear_label
      labels0 <- co_impactYearLevels$impactYear_id
      df_i <- df_i %>% mutate(impactYear = impactYear %>% factor(levels0, labels0))
      rm("levels0", "labels0")
      ### Refactor impact types
      levels0 <- co_impactTypes$sector_id %>% paste(co_impactTypes$impactType_id_excel, sep = "_")
      labels0 <- co_impactTypes$impactType_id
      select0 <- c("sector_impactType")
      df_i <- df_i %>% mutate(sector_impactType = sector %>% paste(impactType, sep = "_"))
      df_i <- df_i %>% mutate(sector_impactType = sector_impactType %>% factor(levels0, labels0))
      df_i <- df_i %>% mutate(impactType        = sector_impactType)
      df_i <- df_i %>% select(-c(all_of(select0)))
      rm("levels0", "labels0", "select0")
      ### Refactor model types
      levels0 <- co_models$model_id
      labels0 <- co_models$model_dot
      levels1 <- co_models$model_dot
      labels1 <- co_models$modelType
      ### Factor model only if name_i=="data_scaledImpacts"
      ### Factor model_type
      ### Then convert to character
      doDot0 <- name_i == "data_scaledImpacts"
      if (doDot0) {df_i <- df_i %>% mutate(model_dot = model %>% factor(levels0, labels0))} 
      else        {df_i <- df_i %>% mutate(model_dot = model)}
      df_i <- df_i %>% mutate(model_type = model_dot %>% factor(levels1, labels1))
      ### Convert to character
      mutate0 <- c("variant", "impactYear", "impactType", "model_dot")
      df_i    <- df_i %>% mutate_at(.vars = c(all_of(mutate0)), as.character)
      # rm("levels0", "labels0", "doDot0", "mutate0")
      return(df_i)
    }) %>%
    (function(list_x, names_x = names(list_scaledImpacts)) {
      names(list_x) <- names_x; return(list_x)
    })
  
  ### Update objects in environment
  for (name_i in names(list_scaledImpacts)) {assign(name_i, list_scaledImpacts[[name_i]])}
  
  ### Reshape data_scaledImpacts (move columns with regional values to rows)
  valueCols0         <- co_regions$region_dot
  data_scaledImpacts <- data_scaledImpacts %>% gather(key = "region_dot", value = "scaledImpact", c(all_of(valueCols0)))
  rm("valueCols0")
  
  ### Update scaled impacts in list
  dataList[["data_scaledImpacts"]] <- data_scaledImpacts
  dataList[["slrImpacts"        ]] <- slrImpacts
  
  ###### ** Reshape Scalars ######
  ### refactor region
  ### Update in list
  ### Remove intermediate objects
  levels0 <- co_regions$region_label %>% c("National Total")
  labels0 <- co_regions$region_dot   %>% c("National.Total")
  scalarDataframe <- scalarDataframe %>% mutate(region = region %>% factor(levels0, labels0) %>% as.character())
  dataList[["scalarDataframe"]] <- scalarDataframe
  rm("levels0", "labels0")
  
  ### Return the list of dataframes
  return(dataList)
}
