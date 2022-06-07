### Save default results
require(usethis)
require(devtools)
require(tidyverse)
getwd() %>% load_all()

###### Update Default Results ######
defaultResults <- run_fredi()
usethis::use_data(defaultResults, overwrite=T); rm("defaultResults")

###### Update Scenarios ######
gcamScenarios <- svDataList$gcamScenarios   %>% rename(temp_C  = temp_C_conus) %>% select(-c(temp_C_global)); usethis::use_data(gcamScenarios, overwrite=T)
popScenario   <- svPopList$iclus_region_pop %>% rename(reg_pop = region_pop) %>% select(c(year, region, reg_pop)); usethis::use_data(popScenario, overwrite=T)
# gcamScenarios <- getFromNamespace("svDataList", "FrEDI")$gcamScenarios   %>% rename(temp_C  = temp_C_conus)
# popScenario   <- getFromNamespace("svPopList", "FrEDI")$iclus_region_pop %>% rename(reg_pop = region_pop)

###### Update Impact Lists ######
c_impactListFilePath <- getwd() %>% file.path("..", "createSystemData", "data", "sv", "impactsLists")
c_impactListFiles    <- c_impactListFilePath %>% list.files; c_impactListFiles

for(i in 1:length(c_impactListFiles)){
  fileName_i    <- c_impactListFiles[i]
  inFilePath_i  <- c_impactListFilePath %>% file.path(fileName_i)
  outFilePath_i <- c_impactListFilePath %>% file.path("inst", "extdata", "sv", "impactLists", fileName_i)
  file.copy(from=inFilePath_i, to = outFilePath_i)
}
