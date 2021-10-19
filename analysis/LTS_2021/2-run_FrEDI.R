# Run FrEDI for LTS with CONUS temperatures relative to 1995 (1986-2005)

# CH
# 7/30/2021
# updated 10/19/2021

# install.packages("devtools")
# remove.packages("") 

library(devtools)
library(dplyr)

install.packages("C:/Users/chartin/OneDrive - Environmental Protection Agency (EPA)/Documents/GitHub/FrEDI/", repo=NULL, type = "source")

require(FrEDI)

scenariosPath <- system.file(package="FrEDI") %>% 
  file.path("extdata","scenarios")
scenariosPath %>% 
  list.files
tempInputFile <- "1-temp_1p5.csv"
popInputFile  <- scenariosPath %>% 
  file.path("pop_scenario.csv")

### Import inputs
example_inputsList <- import_inputs(
  tempfile = tempInputFile,
)

customScenarioInputs <- import_inputs(tempfile = tempInputFile)
magicc_temp_1p5 <- run_fredi(inputsList= customScenarioInputs, aggLevels="none") 
write.csv(magicc_temp_1p5, file = "2-FrEDI_magicc_temp_1p5.csv", row.names = F)

#### Scenario #2 ####
tempInputFile <- "1-temp_paris.csv"

example_inputsList <- import_inputs(
  tempfile = tempInputFile,
)

customScenarioInputs <- import_inputs(tempfile = tempInputFile)
magicc_temp_paris <-  run_fredi(inputsList= customScenarioInputs, aggLevels="none")
write.csv(magicc_temp_paris, file = "2-FrEDI_magicc_temp_paris.csv", row.names = F)

#### Scenario #3 ####
tempInputFile <- "1-temp_7wm2.csv"

example_inputsList <- import_inputs(
  tempfile = tempInputFile,
)

customScenarioInputs <- import_inputs(tempfile = tempInputFile)
magicc_temp_7wm2 <-  run_fredi(inputsList= customScenarioInputs, aggLevels="none") 
write.csv(magicc_temp_7wm2, file = "2-FrEDI_magicc_temp_7wm2.csv", row.names = F)








  filter(model != "30 cm" ) %>% #pull out SLR scenario that drops out
  aggregate_impacts(columns = c("annual_impacts"), aggLevels=c("all")) %>% 
  mutate(scenario = "magicc_1p5")