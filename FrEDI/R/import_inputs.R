#' Import custom scenarios for climate and socioeconomics (temperature and global mean sea level rise (GMSL), population, and GDP) from user-specified file names
#'
#' @description
#' This function enables users to import data on custom scenarios for use with [FrEDI::FREDI] (supplied as inputs to [FrEDI::run_fredi()]). Users specify path names to CSV files containing temperature, global mean sea level rise (GMSL), gross domestic product (GDP), and population scenarios. [FrEDI::import_inputs()] reads in and format any specified files as data frames and returns a list of data frames for imported scenarios.
#'
#' @param tempfile A character string indicating the location of a CSV file containing a custom temperature scenario (first column contains years; second column contains temperatures, in degrees Celsius, above the 1995 baseline year). The temperature scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param slrfile A character string indicating the location of a CSV file containing a custom sea level rise scenario (first column contains years; second column contains values for global mean sea level rise (GMSL), in centimeters, above the 2000 baseline). The SLR scenario must start in 2000 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param popfile A character string indicating the location of a CSV file containing a custom population scenario for NCA regions. The first column contains years in the interval 2010 to 2300. The number of additional columns, column names, and column contents depend on the population format set by `popform`. For more details, see `popform`. The population scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param gdpfile A character string indicating the location of a CSV file containing a custom scenario for gross domestic product (GDP) (first column contains years; second column contains values for GDP, in total 2015$). The GDP scenario must start in 2010 or earlier and end at or after the maximum model run year (e.g., as specified by the `maxYear` argument to [FrEDI::run_fredi()]).
#' @param temptype A character string indicating whether the temperature values in the temperature input file (specified by `tempfile` represent global temperature change (`temptype="global"`) or temperature change for the contiguous U.S. (`temptype="conus"`) in degrees Celsius. By default, the model assumes temperatures are CONUS temperatures (i.e., `temptype="conus"`).
#' @param popform A character string indicating whether the populations in the population input file specified by `popfile` are spread across multiple columns (i.e., `popform="wide"`) or are combined in a single column (i.e., `popform="long"`). By default, the model assumes `popform="wide"`. For both formats (`popform="wide"` or `popform="long"`), the first column contains years. If `popform="wide"` (default), the second through eighth columns of `popfile ` must contain population values for each NCA region, with the associated NCA region as the column name. If `popform="long"`, the second column must contain NCA region names and the third column must contain values for the associated region population.
#'
#' @details
#' This function enables users to import data on custom scenarios for use with temperature binning. Users specify path names to CSV files containing temperature, global mean sea level rise (GMSL), population, and gross domestic product (GDP) scenarios (`tempfile`, `slrfile`, `gdpfile`, and `popfile`, respectively). [FrEDI::import_inputs()] reads in and formats any specified files as data frames and returns a list of data frames for imported scenarios. Users can specify whether the temperature input is for the contiguous U.S. (CONUS) or global using `temptype` and specify the format of the population scenario using `popform`.
#'
#' Values for input scenarios must be within reasonable ranges. Temperature and GMSL inputs must begin in 2000 or earlier, whereas values for population and GDP scenarios can start in 2010 or earlier.
#'
#' * The input temperature scenario (passed to [FrEDI::run_fredi()] via the `inputsList` argument) requires temperatures for CONUS in degrees Celsius relative to 1995 (degrees of warming relative to the baseline). Temperature values must be greater than or equal to zero degrees if warming (CONUS). Users can convert global temperatures to CONUS temperatures using [FrEDI::convertTemps(from="global")] or by specifying [FrEDI::import_inputs(temptype="global")] when importing a temperature scenario from a CSV file.
#' * Values for the sea level rise (SLR) scenario are for global mean sea level rise (GMSL) must be in centimeters (cm) and values must be greater than or equal to zero and less than or equal to 250 cm.
#' * Population and gross domestic product (GDP) values must be greater than or equal to zero.
#'
#' If the population input is spread across multiple columns (i.e., `popform="wide"`), columns must be named according to the NCA regions. If the population input is in the long format, the region value must be in the second column. The NCA region names for population inputs must be in the following character vector: `c("Midwest"`, `"Northeast"`, `"Northern.Plains"`, `"Northwest"`, `"Southeast"`, `"Southern.Plains"`, `"Southwest")`. All regions must be present in the population input file.
#'
#' [FrEDI::import_inputs()] outputs a list of data frames that can be passed to the main FREDI function [FrEDI::run_fredi()] using the `inputList` argument. For example, specify `run_fredi(inputsList=x)` to generate impacts for a custom scenario `x` (where `x` is a list of data frames such as that output from [FrEDI::import_inputs()]) (see [FrEDI::run_fredi()]).
#'
#' All inputs to [FrEDI::import_inputs()] are optional. If the user does not specify a file path for `tempfile`, `slrfile`, `gdpfile`, or `popfile` (or if there is an error reading in inputs from those file paths), [FrEDI::import_inputs()] outputs a list with a `NULL` value for the associated list element. [FrEDI::run_fredi()] defaults back to the default scenarios for any elements of the inputs list that are `NULL` or missing. In other words, running `run_fredi(inputsList=list())` returns the same outputs as running [FrEDI::run_fredi()] (see [FrEDI::run_fredi()]).
#'
#'
#' @return
#' [FrEDI::import_inputs()] returns a list of named elements containing data frames with custom scenarios for temperature, GMSL, GDP, and regional population, respectively:
#'
#' \tabular{ll}{
#' \strong{List Index} \tab \strong{Description} \cr
#' `tempInput` \tab Data frame containing a custom temperature scenario imported from the CSV file specified by `tempfile`, with missing values removed. `tempInput` has two columns with names `c("year"`, `"temp_C")` containing the year and CONUS temperatures in degrees Celsius, respectively. \cr
#' `slrInput` \tab Data frame containing a custom GMSL scenario imported from the CSV file specified by `slrfile`, with missing values removed. `slrInput` has two columns with names `c("year"`, `"slr_cm")` containing the year and global mean sea level rise (GMSL) in centimeters, respectively. \cr
#' `gdpInput` \tab Data frame containing a custom GDP scenario imported from the CSV file specified by `gdpfile`, with missing values removed. `gdpInput` has two columns with names `c("year", "gdp_usd")` containing the year and the U.S. national GDP in 2015$, respectively. \cr
#' `popInput` \tab Data frame containing a custom temperature scenario imported from the CSV file specified by `popfile`, with missing values removed. `popInput` has and three columns with names `c("year"`, `"region"`, `"reg_pop")` containing the year, the NCA region name, and the NCA region population, respectively. \cr
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
#' ### Temperature Scenario File Name
#' tempInputFile <- scenariosPath |> file.path("GCAM_scenario.csv")
#'
#' ### SLR Scenario File Name
#' slrInputFile  <- scenariosPath |> file.path("slr_from_gcam.csv")
#'
#' ### Population Scenario File Name
#' popInputFile  <- scenariosPath |> file.path("pop_scenario.csv")
#'
#' ### Import inputs
#' example_inputsList <- import_inputs(
#'   tempfile = tempInputFile,
#'   slrfile  = slrInputFile,
#'   popfile  = popInputFile,
#'   temptype = "global",
#'   popform  = "wide"
#' )
#'
#' ### Use imports with FREDI:
#' df_x <- run_fredi(inputsList=example_inputsList)
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'
#' @export
#' @md
#'
###### import_inputs ######
### Created 2021.02.08. Last updated 2021.02.08
### This function imports data from user-specified file names.
import_inputs <- function(
  tempfile = NULL, ### File path of CSV with temperature inputs
  slrfile  = NULL,
  popfile  = NULL,
  gdpfile  = NULL,
  temptype = "conus", ### "global", or "conus" (default)
  popform  = "wide", ### "wide" or "long" ### Previously: "gather", "spread"
  byState  = FALSE
){
  ###### Messaging ######
  hasAnyInputs <- list(tempfile, slrfile, popfile, gdpfile) |>
    lapply(function(x){!is.null(x)}) |>
    unlist() |> any()
  silent  <- TRUE
  msgUser <- ifelse(silent, FALSE, TRUE)
  msg0    <- ""
  msg1    <- msg0 |> paste0("\t")
  msg2    <- msg1 |> paste0("\t")
  msg3    <- msg2 |> paste0("\t")
  if(hasAnyInputs){
    "\n" |> paste0(msg0) |> paste0("In import_inputs():") |> message()
  }

  ###### Defaults ######
  ### Set popform default to "wide" (previously "spread")
  ### Set popform to "wide" if none is specified
  popform_default  <- "wide"
  popform          <- ifelse(is.null(popform), popform_default, tolower(popform))
  wide_pop         <- popform=="wide"
  if(byState){geo_msg <- " state..."} else{geo_msg <- " region..."}

  ### Set temperature type default and set temperature type to default if none
  ### is declared. Check whether inputs temperatures are already in CONUS degrees
  temptype_default <- "conus"
  temptype         <- ifelse(is.null(temptype), temptype_default, temptype)
  conus            <- (tolower(temptype) == "conus"); #conus |> print()

  ###### Initialize Inputs List ######
  ### Get input scenario info: co_inputScenarioInfo
  name_dfScenarioInfo <- "co_inputScenarioInfo"
  assign(name_dfScenarioInfo, rDataList[["regionData"]][["data"]][[name_dfScenarioInfo]])
  name_stateInfo <- "co_states"
  assign(name_stateInfo, rDataList[["frediData"]][["data"]][[name_stateInfo]])
  input_names_vector  <- co_inputScenarioInfo$inputName
  num_inputNames      <- co_inputScenarioInfo |> nrow()

  ###### Initialize Results List ######
  inputsList <- list()

  ###### Iterate Over Inputs List ######
  for(i in 1:num_inputNames){
    ###### Input Info ######
    inputInfo_i <- co_inputScenarioInfo[i,]

    ### Input name and label
    input_i     <- inputInfo_i$inputName |> unique()
    msgName_i   <- inputInfo_i$inputType |> unique()
    ### Input argument and run_fredi argument
    inputArg_i  <- inputInfo_i$importArgName |> unique()
    inputName_i <- inputInfo_i$tempBinListName |> unique()
    ### Min and Max Values
    min_i       <- inputInfo_i$inputMin |> unique()
    max_i       <- inputInfo_i$inputMax |> unique()
    ###### Column Info ######
    region_i    <- inputInfo_i$region |> unique()
    valueCol_i  <- inputInfo_i$valueCol |> unique()
    ### Initialize column names
    numCols_i   <- colNames_i <- c("year", valueCol_i)
    cols0       <- c("region")
    statecols0  <- c("region", "state", "postal")
    if(byState){
      cols0      <- statecols0
      colNames_i <- numCols_i <- c("year", "state_pop")
    }
    
    ### Add state/region columns as needed
    if(region_i == 1){
      colNames_i  <- c(colNames_i[1], cols0, colNames_i[2])
    }

    ###### Initialize Results List Element ######
    ### Initialize input in list
    inputsList[[inputName_i]] <- NULL

    ###### Parse File ######
    ### Parse inputArg_i and add to the list, then check if it is null
    inputFile_i  <- parse(text=inputArg_i) |> eval()
    isNullFile_i <- inputFile_i |> is.null()
    # list_i[["inputFile"]] <- inputFile_i
    # isNullFile_i <- list_i[["inputFile"]] |> is.null()

    ###### Format Data Frame ######
    if(!isNullFile_i){
      msg1 |> paste0("User supplied ", msgName_i, " input...") |> message()
      msg2 |> paste0("Importing data from ", inputFile_i, "...") |> message()
      ### Try to import the file and initialize the list value
      fileInput_i   <- inputFile_i |> fun_tryInput(silent=T)
      fileStatus_i  <- fileInput_i[["fileStatus"]]
      df_input_i    <- fileInput_i[["fileInput"]]

      ### Message the user
      if(msgUser){ msg2 |> paste0(fileInput_i[["fileMsg"]]) |> message() }

      ######## For loaded data ######
      ### If the load is a success, add results to the input list
      if(fileStatus_i=="loaded"){
        msg2 |> paste0("Formatting ", msgName_i, " inputs...") |> message()
        ###### Gather population inputs ######
        if(input_i=="pop" & wide_pop){
          msg3 |> paste0("User specified `popform='wide'`...") |>
            paste0("Gathering population by", geo_msg) |>
            message()

          names(df_input_i)[1] <- colNames_i[1]
          if(byState){
            df_input_i <- df_input_i |> gather(key = "state", value="state_pop", -year)
          } else{
            df_input_i <- df_input_i |> gather(key = "region", value="reg_pop", -year)
          }
        }

        ###### Standardize All Columns ######
        ### Rename Inputs and Convert all columns to numeric
        ### Rename Inputs and Convert all columns to numeric
        if(byState){
          df_input_i <- df_input_i |>
            left_join(co_states, by = c("state" = "state"), suffix = c("", ".y")) |>
            select(colNames_i) 
        }
        
        df_input_i  <- df_input_i |>
          rename_inputs(colNames_i) |>
          mutate_all(as.character) |>
          mutate_at(vars(all_of(numCols_i)), as.numeric)

        ###### Convert Global Temps to CONUS ######
        ### Convert Global Temps to CONUS if there are temperature inputs and they
        ### aren't already in CONUS degrees
        if((input_i=="temp") & (!conus)){
          ### Message user
          msg3 |> paste0("User specified `temptype='global'`...") |> message()
          msg3 |> paste0("Converting global temperatures to CONUS temperatures...") |> message()
          ### Convert temps
          df_input_i <- df_input_i |> mutate(temp_C = temp_C |> convertTemps(from="global"))
        }

        # ###### Check Input ######
        # msg2 |> paste0("Checking values...") |> message()
        # ### Values
        # values_i <- df_input_i[,valueCol_i]
        # ### Substitute NULL for missing values for min and max
        # if(is.na(min_i)) min_i <- NULL; if(is.na(max_i)) max_i <- NULL
        # ### Check the status
        # flag_i <- values_i |> check_inputs(xmin = min_i, xmax = max_i)
        # ### Return and message the user if there is a flag:
        # flagStatus_i <- flag_i$flagged
        # flagRows_i   <- flag_i$rows
        # ### If flag, message user and return flagStatus_i
        # if(flagStatus_i){
        #   ### Message labels
        #   numrows_i    <- flagRows_i |> length()
        #   years_i      <- df_input_i$year[flagRows_i]; yearsLabel_i <- paste(years_i, collapse=",")
        #   rangeLabel_i <- paste0("c(", min_i , ",", max_i, ")")
        #   ### Create message and message user
        #   msg1_i       <- msg2 |> paste("Error in importing inputs for", msgName_i) |> paste0("!")
        #   msg2_i       <- msg3 |> paste(inputName_i, "has", numrows_i,  "values outside of defined range", rangeLabel_i)
        #   msg3_i       <- msg3 |> paste("Please correct values", msgName_i, "values for years", yearsLabel_i) |> paste0("...")
        #   ### Message user
        #   "\n" |> paste0(msg0) |> paste0("Warning:") |> message()
        #   msg1_i |> message(); msg2_i |> message(); msg3_i |> message()
        #   "\n" |> paste0(msg0) |> paste0("Exiting...") |> message()
        #
        #   ### Return list with error and flagged rows
        #   returnList <- list(
        #     error_msg    = paste0("Error in ", inputName_i, ". Values outside range."),
        #     flagged_rows = flagRows_i
        #     )
        #
        #   ### Return list and not an inputs list if an error occurred
        #   return(returnList)
        # } ### End if flagged

        ###### Update Results List Element ######
        ### Add results to the file
        inputsList[[inputName_i]] <- df_input_i
      } ### End if status == loaded
    } ### End if !isNullFile
  }  ### End iterate on i

  ###### Return input list ######
  msg0 |> paste0("Finished.") |> message()
  return(inputsList)
}

