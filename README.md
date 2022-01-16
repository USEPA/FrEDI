# FrEDI: The Framework for Evaluating Damages and Impacts
This tool projects economic damages and impacts from climate change and sea level rise through the 21st century under any scenario.

To install the FrEDI package we recommend cloning the repository.

```
devtools::install.packages("path_to_repo", repo=NULL, type="source)
require(FrEDI)
```

## Load custom scenarios (temperature, population, GDP)

```
scenariosPath <- system.file(package="FrEDI") %>% 
  file.path("extdata","scenarios")

tempInputFile <- "user_specified.csv"
popInputFile  <- scenariosPath %>% 
  file.path("pop_scenario.csv") 
gdpInputFile <- scenariosPath %>% 
  file.path("gdp_scenario.csv")
```

## Import the custom scenarios

```
customScenarioInputs <- import_inputs(
  tempfile = tempInputFile, 
  gdpfile = gdpInputFile,
  popfile = popInputFile)
```

## Run FrEDI
### The user can specify different aggregation levels

```
run_fredi(inputsList= customScenarioInputs,  aggLevels = "all"))
```
