###### Documentation ######
#' Import custom scenarios for temperature, global mean sea level rise (GMSL), population, and GDP from user-specified file names
#'
#' @description
#' This function enables users to import data on custom scenarios for use with [FrEDI::run_fredi()]. Users specify path names to CSV files containing temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and state population scenarios. [FrEDI::import_inputs()] reads in and format any specified files as data frames and returns a list of data frames for imported scenarios.
#'
#'
#'
#' @param inputsList=list(gdp=NULL,pop=NULL,temp=NULL,slr=NULL,ch4=NULL,nox=NULL) A named list containing file paths to CSV files containing scenarios:
#'    * __gdp__ A character string indicating the location of a CSV file containing a custom scenario for U.S. gross domestic product (GDP), for use with [FrEDI::run_fredi()] or [FrEDI::run_fredi_ghg()]. The first column in the CSV should be named `"year"` and contains years associated with the GDP estimates; the second column (named `"gdp_usd"`) should contain values for U.S. GDP in 2015 U.S. dollars  (2015$ USD). The GDP scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()] or [FrEDI::run_fredi_ghg()]). Values for GDP in column `"gdp_usd"` must be greater than or equal to zero.
#'    * __pop__ A character string indicating the location of a CSV file containing a custom population scenario for states and NCA regions, for use with [FrEDI::run_fredi()], [FrEDI::run_fredi_sv()], or [FrEDI::run_fredi_ghg()]. The first column should be named `"year"` and contain years in the interval 2010 to 2300. The second column should be called `"pop"` and contain population values. Whether a third column is required depends on the geographical scale of the data, as specified by the `popArea` argument (for more information, see the `popArea` argument, below.
#'        * If `popArea = "state"`, the third column should be called `"state"` and contain the name of the state associated with each population estimate in a given year. If `popArea = "regional"`, the third column should be called `"region"` and contain the NCA Region label associated with each population estimate in a given year. If `popArea = "national"`, only the `year` and `pop` columns are required.
#'        * If `popArea = "state"`, the file must contain estimates for all CONUS states -- i.e., all states except Alaska and Hawaii must be present in the input file (estimates for Alaska and Hawaii are optional). If `popArea = "regional"`, the file must contain estimates for all seven NCA regions.
#'        * The population scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()] or [FrEDI::run_fredi_ghg()]). Values for population in column `"pop"` must be greater than or equal to zero.
#'    * __temp__ A character string indicating the location of a CSV file containing a custom temperature scenario, for use with `run_fredi()` or `run_fredi_sv()`. The first column in the CSV should be named `"year"` and contain years associated with the temperature estimates; the second column (named `"temp_C"`) should contain values for temperatures (i.e., degrees of warming relative to a baseline era of 1986-2005), in degrees Celsius. The temperature scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#'    * __slr__ A character string indicating the location of a CSV file containing a custom sea level rise scenario, for use with `run_fredi()` or `run_fredi_sv()`. The first column in the CSV should be named `"year"` and contain years associated with the sea level rise estimates; the second column should be named "`slr_cm"` and contain values for global mean sea level rise (GMSL), in centimeters, above a baseline year of 2000. The SLR scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#'    * __o3__ A character string indicating the location of a CSV file containing a custom scenario for changes in U.S. state-level ozone concentrations (relative to a 1986-2005 baseline era). The input ozone scenario requires changes in annual state-level ozone concentrations, by GCM model, in parts per trillion by volume (pptv) relative to a 1986-2005 baseline era. In other words, the input ozone scenario requires ozone concentrations specific to the state, GCM model, and year of the analysis.
#'        * The `o3` input requires a data frame object with six columns with names `"region"`, `"state"`, `"postal"`, `"model"`, `"year"`, and `"O3_pptv"`  containing the region name (`"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"` for CONUS states, or `"Alaska"` and `"Hawaii"` for Alaska and Hawaii, respectively), the state name, the two-character postal code abbreviation for the state, the GCM model name (`"CanESM2"`, `"GFDL-CM3"`, `"GISS-E2-R"`, `"HadGEM2-ES"`, and/or `"MIROC5"`), the year, and the change in ozone concentration (in pptv) relative to a 1986-2005 baseline era.
#'        * Ozone inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by the `maxYear` argument).
#'    * __ch4__ A character string indicating the location of a CSV file containing a custom scenario for changes in U.S. methane concentrations (relative to a 1986-2005 baseline era), at the national level, for use with `run_fredi_ghg()`. The first column in the CSV should be named `"year"` and contain years associated with the national methane estimates; the second column should be named "`CH4_ppbv"` and contain values for methane. The methane scenario must start in 2020 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi_ghg()]).
#'    * __nox__ A character string indicating the location of a CSV file containing a custom scenario for U.S. NOx emission values, at the national level. The first column in the CSV should be named `"year"` and contain years associated with the national NOx estimates; the second column should be named "`Mt"` and contain values for NOx emissions, in megatons per year (Mt). The NOx scenario must start in 2020 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi_ghg()]).
#' @param temptype="conus" A character string indicating whether the temperature values in the temperature input file (as specified by `tempfile`) represent temperature changes, in degrees Celsius, at the global level (`temptype = "global"`) or for the contiguous U.S. (CONUS) only (`temptype = "conus"`, default).
#' @param popArea="state" A character string indicating the geographical scale of population inputs (as specified by `popfile`). Specify one of: `"state"`, for state-level population (default); `"conus"`, for the contiguous U.S. (CONUS) only (i.e., national totals, but excluding Alaska and Hawaii); or `"national"`, for national totals (CONUS population, including Alaska and Hawaii).
#'
#'
#'
#' @details
#' This function enables users to import data on custom scenarios for use with temperature binning. Users provide a named list specifying path names to CSV files containing scenarios for gross domestic product (GDP) (for use with [FrEDI::run_fredi()] or [FrEDI::run_fredi_ghg()]); population (for use with [FrEDI::run_fredi()], [FrEDI::run_fredi_sv()], or [FrEDI::run_fredi_ghg()]); temperature and/or global mean sea level rise (GMSL) (for use with [FrEDI::run_fredi()] or [FrEDI::run_fredi_sv()]); or methane or NOx (for use with [FrEDI::run_fredi_ghg()]). [FrEDI::import_inputs()] reads in and formats any specified files as data frames and returns a list of data frames for imported scenarios.
#'
#' The CSV files should contain estimates aligned with the requirements of the `run_fredi()` or ``
#'
#' * __GDP Scenario.__ The file specified by `inputsList$gdp` must have two columns -- `"year"` and `"gdp_usd"` -- respectively containing the years associated with the GDP estimates and the estimates for U.S. GDP, in 2015$.
#'    * GDP values must be greater than or equal to zero.
#'    * The GDP scenario must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __Population Scenario.__ The file specified by `inputsList$pop` population must have at least two columns -- `"year"` and `"pop"` -- respectively containing the years associated with the population estimates and the population estimates. requires state-level population values. Whether a third column is required depends on the geographical scale of the data, as specified by the `popArea` argument.
#'     * If `popArea = "state"`, the third column should be called `"state"` and contain the name of the state associated with each population estimate in a given year. If `popArea = "regional"`, the third column should be called `"region"` and contain the NCA Region label associated with each population estimate in a given year (one of `"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"` for CONUS states, or `"Alaska"` and `"Hawaii"` for Alaska and Hawaii, respectively). If `popArea = "national"`, only the `year` and `pop` columns are required.
#'     * If `popArea = "state"`, the file must contain estimates for all CONUS states -- i.e., all states except Alaska and Hawaii must be present in the input file (estimates for Alaska and Hawaii are optional). If `popArea = "regional"`, the file must contain estimates for all seven NCA regions.
#'     * The population scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#'     * Population values must be greater than or equal to zero.
#'     * Population estimates must have at least one non-missing value in 2010 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __Temperature Scenario.__ The file specified by `inputsList$temp` must have two columns -- `"year"` and `"temp_C"` -- respectively containing the years associated with the temperature estimates and the temperatures (i.e., degrees of warming) in degrees Celsius relative to a baseline year of 1995 (i.e., the central year of a 1986-2005 reference period).
#'    * If values in `"temp_C"` have global rather than CONUS-specific temperatures, users should specify `temptype = "global"` when running `import_inputs()`, and `import_inputs()` will convert the temperatures to CONUS using [FrEDI::convertTemps] (with argument `from = "global"`).
#'    * Temperature inputs to [FrEDI::run_fredi()] must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __SLR Scenario.__ The file specified by `inputsList$slr` must have two columns -- `"year"` and `"slr_cm"` -- respectively containing the years associated with the GMSL estimates and GMSL estimates, in centimeters, above a 2000 baseline year.
#'    * GMSL heights must be greater than or equal to zero.
#'    * The SLR scenario must have at least one non-missing value in 2000 or earlier and at least one non-missing value in or after the final analysis year (as specified by the [FrEDI::run_fredi()] `maxYear` argument).
#' * __Ozone Scenario.__ The input ozone scenario requires changes in annual state-level ozone concentrations, by GCM model, in parts per trillion by volume (pptv) relative to a 1986-2005 baseline era. In other words, the input ozone scenario requires ozone concentrations specific to the state, GCM model, and year of the analysis.
#'    * `o3` requires a data frame object with six columns with names `"region"`, `"state"`, `"postal"`, `"model"`, `"year"`, and `"O3_pptv"`  containing the region name (`"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"` for CONUS states, or `"Alaska"` and `"Hawaii"` for Alaska and Hawaii, respectively), the state name, the two-character postal code abbreviation for the state, the GCM model name (`"CanESM2"`, `"GFDL-CM3"`, `"GISS-E2-R"`, `"HadGEM2-ES"`, and/or `"MIROC5"`), the year, and the change in ozone concentration (in pptv) relative to a 1986-2005 baseline era.
#'    * Ozone inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * If inputs are specified for ozone _and_ methane or NOx (i.e., `!is.null(inputsList$o3) & (!is.null(inputsList$ch4) | !is.null(inputsList$nox))`), `run_fredi_ghg()` will use the ozone scenario in preference of the methane and NOx scenario.
#'
#' * __Methane Scenario.__ The input methane scenario requires changes in annual methane concentrations, at the national level, in parts per billion by volume (ppbv) relative to a 1986-2005 baseline era.
#'    * `ch4` requires a data frame object with two columns with names `"year"` and `"CH4_ppbv"`  containing the year and the change in methane concentration (in ppbv) relative to a 1986-2005 baseline era.
#'    * Methane inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * `run_fredi_ghg()` will override a user-supplied methane scenario with a user-supplied ozone scenario; in other words, `run_fredi_ghg()` will use the ozone scenario in preference of the methane and NOx scenario.
#'
#' * __NOx Scenario.__ The input NOx scenario requires annual NOx emissions in the US, at the national level, in Megatons (MT) relative to a 1986-2005 baseline.
#'    * `nox` requires a data frame object with two columns with names `"year"` and `"NOx_Mt"`  containing the year and the change in NOx concentration (in Mt) relative to a 1986-2005 baseline era.
#'    * NOx inputs must have at least one non-missing value in 2020 or earlier and at least one non-missing value in or after the final analysis year (as specified by `maxYear`).
#'    * `run_fredi_ghg()` will override a user-supplied methane scenario with a user-supplied ozone scenario; in other words, `run_fredi_ghg()` will use the ozone scenario in preference of the methane and NOx scenario.
#'
#'
#' [FrEDI::run_fredi()], [FrEDI::run_fredi_sv()], and [FrEDI::run_fredi_ghg()] each require a population scenario at the state level. If the population scenario is supplied to [FrEDI::import_inputs()] at a geographical scale above the state level (i.e., if `popArea = "national"`, `popArea = "area"`, or `popArea = "regional"`), [FrEDI::import_inputs()] will calculate state-level estimates from the provided data:
#'
#' * If `popArea = "national"`, [FrEDI::import_inputs()] will use historical U.S. Census data for the period 2010--2023 to allocate total national population to CONUS and non-CONUS regions (i.e., Alaska and Hawaii); U.S. Census values from 2023 are applied in allocations for years after 2023. [FrEDI::import_inputs()] then uses ICLUS data and projections for the period 2010--2100 to allocate CONUS population to specific NCA regions; ICLUS values from 2100 are applied in allocations for years after 2100.
#' * If `popArea = "national"` or `popArea = "regional"`, [FrEDI::import_inputs()] uses ICLUS data and projections for the period 2010--2100 to allocate population for each NCA region to the associated states; ICLUS values from 2100 are applied in allocations for years after 2100.
#'
#'
#' [FrEDI::import_inputs()] outputs a list of data frame objects that can be passed to the main FREDI function [FrEDI::run_fredi()] using the `inputList` argument. For example, specify `run_fredi(inputsList = x)` to generate impacts for a custom scenario `x` (where `x` is a list of data frames such as that output from [FrEDI::import_inputs()]) (see [FrEDI::run_fredi()]).
#'
#' All inputs to [FrEDI::import_inputs()] are optional. If the user does not specify particular named elements (or if there is are any errors reading in inputs from a file path), [FrEDI::import_inputs()] outputs a list with a `NULL` value for the associated list element. After reading in data from the specified CSV files, [FrEDI::import_inputs()] performs basic checks and will also return a `NULL` value for a particular list element if any of the checks fail.
#'
#' When using [FrEDI::import_inputs()] with [FrEDI::run_fredi()], [FrEDI::run_fredi()] defaults back to the default scenarios for any elements of the inputs list that are `NULL` or missing. In other words, running `run_fredi(inputsList = list())` returns the same outputs as running [FrEDI::run_fredi()] (see [FrEDI::run_fredi()]).
#'
#'
#' @return
#' [FrEDI::import_inputs()] returns a list of named elements containing data frames with custom scenarios for temperature, GMSL, GDP, and regional population, respectively:
#'
#' \tabular{ll}{
#' \strong{List Index} \tab \strong{Description} \cr
#' `gdp` \tab Data frame containing a custom GDP scenario imported from the CSV file specified by `inputsList$gdp`, with missing values removed. `gdp` has two columns with names `"year"`, and `"gdp_usd"`, containing the year and the national GDP, respectively. \cr
#' `pop` \tab Data frame containing a custom temperature scenario imported from the CSV file specified by `inputsList$pop`, with missing values removed. `popInput` has three columns with names `"year"`, `"region"`, `"state"`, `"postal"`, and `"pop"`, containing the year, the NCA region name, and the state, the postal code abbreviation (e.g., `postal = "ME"` for `state = "Maine"`), and the state population, respectively. \cr
#' `temp` \tab Data frame containing a custom temperature scenario imported from the CSV file specified by `inputsList$temp`, with missing values removed. `temp` has two columns with names `"year"` and `"temp_C"`, containing the year and CONUS temperatures in degrees Celsius, respectively. \cr
#' `slr` \tab Data frame containing a custom GMSL scenario imported from the CSV file specified by `inputsList$slr`, with missing values removed. `slr` has two columns with names `"year"`, and `"slr_cm"`, containing the year and global mean sea level rise (GMSL) in centimeters, respectively. \cr
#' `o3` \tab Data frame containing a custom scenario with changes in ozone concentrations imported from the CSV file specified by `inputsList$temp`, with missing values removed. `o3` has six columns with names `"region"`, `"state"`, `"postal"`, `"model"`, `"year"`, and `"O3_pptv"`  containing the region name (`"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"` for CONUS states, or `"Alaska"` and `"Hawaii"` for Alaska and Hawaii, respectively), the state name, the two-character postal code abbreviation for the state, the GCM model name (`"CanESM2"`, `"GFDL-CM3"`, `"GISS-E2-R"`, `"HadGEM2-ES"`, and/or `"MIROC5"`), the year, and the change in ozone concentration (in pptv) relative to a 1986-2005 baseline era. \cr
#' `ch4` \tab Data frame containing a custom scenario with changes in methane concentrations imported from the CSV file specified by `inputsList$slr`, with missing values removed. `ch4` has two columns with names `"year"`, and `"CH4_ppbv"`, containing the year and Change in U.S. methane concentrations, in parts per billion by volume (ppbv) relative to a 1995-2006 baseline era, respectively. \cr
#' `nox` \tab Data frame containing a custom NOx emissions scenario imported from the CSV file specified by `inputsList$nox`, with missing values removed. `nox` has two columns with names `"year"` and `"NOx_Mt"`, containing the year and U.S. national-level NOx emissions (in Mt), respectively. \cr
#' }
#'
#'
#' @examples
#' ### Path to example scenarios
#' scenariosPath <- system.file(package="FrEDI") |> file.path("extdata","scenarios")
#'
#' ### View example scenario names
#' scenariosPath |> list.files()
#'
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath |> file.path("slr_from_gcam.csv")
#'
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath |> file.path("State ICLUS Population.csv")
#'
#' ### Import inputs
#' example_inputsList <- import_inputs(inputsList=list(slr=slrInputFile, pop=popInputFile), popArea="state")
#'
#' ### Use imports with FREDI:
#' df_x <- run_fredi(inputsList=example_inputsList)
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#' United Nations. 2015. World population prospects: The 2015 revision. New York: United Nations, Department of Economic and Social Affairs, Population Division.
#'
#' U.S. Census Bureau. 2021. State Population Totals and Components of Change: 2010--2019. Available at <https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html>.
#'
#' U.S. Census Bureau. 2023. State Population Totals and Components of Change: 2020--2023. Available at <https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html>.
#'
#' U.S. Census Bureau. 2023. 2023 National Population Projections Tables: Main Series. Available at <https://www.census.gov/data/tables/2023/demo/popproj/2023-summary-tables.html>.
#'
#' @export
#' @md
#'
#'

## import_inputs ----------------
### This function imports data from user-specified file names.
import_inputs <- function(
  inputsList = list(gdp=NULL, pop=NULL, temp=NULL, slr=NULL, ch4=NULL, nox=NULL, o3=NULL),
  temptype   = "conus", ### "global", or "conus" (default)
  popArea    = "state", ### "national", "conus", "regional", or "state" (default)
  module     = "fredi", ### "fredi", "sv", "methane"
  silent     = FALSE ### Level of messaging
){
  ### Set Up the Environment ----------------
  #### Messaging ----------------
  msgUser    <- !silent
  sep0       <- ", "
  msgN       <- "\n"
  msg0       <- 0
  msg1       <- msg0 + 1
  msg2       <- msg0 + 2
  msg3       <- msg0 + 3
  geo_msg    <- " state..."
  ### Message the user
  msg0 |> get_msgPrefix(newline=T) |> paste0("Importing inputs from file...") |> message()

  #### Module Options ----------------
  ### Get input info
  module0    <- module     |> tolower()
  modTypes0  <- controlData$co_moduleModTypes |> filter(module %in% module0) |> pull(model_type)
  inputInfo0 <- module0    |> get_dfInputInfo(modTypes0) |> mutate(maxYear = NA)
  inNames0   <- inputInfo0 |> pull(inputName)
  # inputInfo0 <- module |> get_dfInputInfo(modTypes0) |> mutate(maxYear = maxYear)
  # inNames0   <- inputInfo0 |> pull(inputName)

  #### Columns & Values ----------------
  ### tempType values
  tempTypes  <- c("global", "conus")
  tempType   <- temptype |> tolower()
  conus      <- "conus" %in% tempType
  ### popArea values
  popAreas   <- c("national", "conus", "regional", "state")
  popArea    <- popArea |> tolower()
  # ### Inputs list names
  # inNames    <- inputsList |> names()


  ### Check Validity of Arguments ----------------
  #### Check List & Names ----------------
  ### Exit if inputs aren't valid
  # inputsList |> glimpse()
  inNames    <- inputsList |> names()
  isList0    <- inputsList |> is.list()
  hasNames0  <- inNames    |> length()
  if(!(isList0 | hasNames0)) {
    msg1 |> get_msgPrefix() |> paste0("Error in import_inputs()!") |> message()
    msg2 |> get_msgPrefix() |> paste0("inputsList must be a named list object!") |> message()
    msg2 |> get_msgPrefix() |> paste0("Exiting...") |> message()
    return()
  } ### End

  ### If there are no list elements, return
  hasInputs0 <- inputsList |> length()
  if(!hasInputs0) {
    msg1 |> get_msgPrefix() |> paste0("Warning: ", "inputsList has length = 0!") |> message()
    msg2 |> get_msgPrefix() |> paste0("Exiting...") |> message()
    return(inputsList)
  } ### End if(!hasInputs0)

  ### Order inputs list by info
  inNames    <- inNames    |> tolower()
  inputsList <- inputsList |> set_names(inNames)
  inNames    <- inNames0   |> get_matches(inNames)
  inputsList <- inputsList[inNames]
  # inputsList |> glimpse()

  ### If there are no list elements, return
  hasInputs0 <- inputsList |> length()
  if(!hasInputs0) {
    msg1 |> get_msgPrefix() |> paste0("Warning: ", "No valid input names provided!") |> message()
    msg2 |> get_msgPrefix() |> paste0("Valid input names for module = ", module, " are: '", inNames0 |> paste(collapse="', '", "'")) |> message()
    msg2 |> get_msgPrefix() |> paste0("Exiting...") |> message()
    return(inputsList)
  } ### End if(!hasInputs0)


  #### Check tempType ----------------
  #### If temp is present, check tempType for validity
  tempStr0   <- "temp"
  doTemp0    <- tempStr0 %in% inNames
  if(doTemp0) {
    ### Check validity
    tempValid  <- tempType %in% tempTypes
    ### If not valid, message and drop those from outputs
    if(!tempValid) {
      ### Messaging
      tTypesStr  <- paste0("'", tempTypes |> paste(collapse="' or '"), "'")
      tTypeStr   <- paste0("'", tempType, "'")
      msg1 |> get_msgPrefix() |> paste0("Warning:") |> message()
      msg2 |> get_msgPrefix() |> paste0("`tempType` must be one of ", tTypesStr, ", not ", tTypeStr, "!") |> message()
      msg2 |> paste0("Dropping temp inputs from inputsList...") |> message()
      rm(tTypesStr, tTypeStr)
      ### Update list
      inNames    <- inNames    |> get_matches(y=tempStr0, matches=F)
      inputsList <- inputsList[inNames]
      # inputInfo0 <- inputInfo0 |> filter(!(inputName %in% "temp"))
    } ### End if(!tempValid)
  } ### End if(doTemp0)
  rm(tempStr0, doTemp0)

  #### Check popArea ----------------
  #### If temp is present, check popArea for validity
  popStr0    <- "pop"
  doPop0     <- popStr0 %in% inNames
  if(doPop0) {
    ### Check popArea for validity
    popValid   <- popArea %in% popAreas
    ### If not valid, message and drop those from outputs
    if(!popValid) {
      ### Messaging
      pAreasStr  <- paste0("'", tempTypes |> paste(collapse="' or '"), "'")
      pAreaStr   <- paste0("'", tempType, "'")
      msg1 |> get_msgPrefix() |> paste0("Warning:") |> message()
      msg2 |> get_msgPrefix() |> paste0("`popArea` must be one of ", pAreasStr, ", not ", pAreaStr, "!") |> message()
      msg2 |> paste0("Dropping pop inputs from inputsList...") |> message()
      inNames    <- inNames    |> get_matches(y=popStr0, matches=FALSE)
      inputsList <- inputsList[inNames]
      # inputInfo0 <- inputInfo0 |> filter(!(inputName %in% "pop"))
    } ### End if(!popValid)
  } ### End if(doPop0)
  rm(popStr0, doPop0)

  ### Check Data for non-NULL elements ----------------
  #### Check for Named List ----------------
  ### Check inputs list
  inputsList <- format_inputsList(
    dfInfo     = inputInfo0,
    inputsList = inputsList,
    tempType   = "conus",
    popArea    = "state",
    module0    = module0,
    msg0       = 1
  ) ### End format_inputsList
  ### Exit if inputs aren't valid
  ### If there are no list elements, return
  validInputs <- inputsList |> is.list()
  hasInputs0  <- inputsList |> length()
  if(!validInputs) {return()}
  if(!hasInputs0) {
    msg1 |> get_msgPrefix() |> paste0("Warning: ", "inputsList has length = 0!") |> message()
    msg2 |> get_msgPrefix() |> paste0("Exiting...") |> message()
    return(inputsList)
  } ### End if(!hasInputs0)


  #### Read in Files ----------------
  ### Try to read in files from remaining elements
  inputsList <- list(
    inputName = inNames,
    filename  = inputsList
  ) |> pmap(
    fun_tryInput,
    silent    = silent,
    msg0      = msg1
  ) |> set_names(inNames) ### End pmap
  # inputsList |> glimpse()

  #### Check Data ----------------
  msg1 |> get_msgPrefix() |> paste0("Checking inputs...") |> message()
  ### Check and format inputs list
  inputsList   <- format_inputsList(
    dfInfo     = inputInfo0,
    inputsList = inputsList,
    tempType   = "conus",
    popArea    = "state",
    module0    = module0,
    msg0       = 1
  ) ### End format_inputsList

  ### Exit if inputs aren't valid
  ### If there are no list elements, return
  validInputs <- inputsList |> is.list()
  hasInputs0  <- inputsList |> length()
  if(!validInputs) {return()}
  if(!hasInputs0) {
    msg1 |> get_msgPrefix() |> paste0("Warning: ", "No remaining valid inputs in inputsList!") |> message()
    msg2 |> get_msgPrefix() |> paste0("Returning an empty list...") |> message()
    return(inputsList)
  } ### End if(!hasInputs0)


  ### Return ----------------
  ### Message, clear unused memory, return
  msg1 |> get_msgPrefix(newline=T) |> paste0("Finished.") |> message()
  # gc()
  return(inputsList)
}

