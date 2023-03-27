### Created 2021.01.20
### This file contains hard coded values for constants used throughout the temperature binning process
frediConfig <- function(outPath){
  # function(...){
  ###### List of Messages for createSystemData ######
  messages_data <- list(
    loadPackages = "Loading required packages...",
    sourceCode   = "Loading custom functions...",
    loadInputs   = list(try="Loading input data...", success="Input data loaded.", fail="Could not find input data."),
    calcScalars  = list(try="Calculating physical and economic scalars...", success="Physical and economic scalars calculated."),
    interpFuns   = list(try="Getting interpolation functions...", success="Interpolation functions complete."),
    saveRData    = list(try="Saving results...", success="R data loaded.", fail="Could not find R data."),
    outPath      = list(fail="Output directory not found.", success="Created output directory.", skip="No directory created. Exiting..."),
    aggImpacts   = list(try="Aggregating impacts...")
  )
  ###### List of Messages for fredi ######
  messages_fredi <- list(
    loadRData     = list(try="Loading R data...", success="R data loaded.", fail="Could not find R data."),
    loadInputs    = list(try="Loading input data...", success="Input data loaded.", fail="Could not find input data."),
    globalTemps   = "Converting global temperature scenario to CONUS scenario...",
    updatePopGDP  = "Updating socioeconomic scenario...",
    updateScalars = list(try="Updating physical and economic scalars...", success="Physical and economic scalars updated."),
    scaledImpacts = list(try="Calculating annual scaled impacts", success="Calculation of scaled impacts complete."),
    aggImpacts    = list(try="Aggregating impacts..."),
    impactTypes   = list(try="Summing over impact types..."),
    impactYears   = list(try="Interpolating between impact estimate years..."),
    modelAves     = list(try="Getting model averages..."),
    national      = list(try="Calculating national totals...")
  )

  ### Minimum Years
  minYear          <- 2010
  maxYear          <- 2090
  # maxYear          <- 2100

  ### Add items to a list and return the list
  fredi_config     <- list(
    messages_data    = messages_data,
    messages_fredi   = messages_fredi,
    ### Types of aggregation for temperature binning
    aggList0         = c("national", "model", "impactYear", "impactType"),
    ### Minimum and Maximum Years
    minYear          = minYear,
    maxYear          = maxYear,
    list_years       = seq(minYear, maxYear, 1),  ### Sequence from 2010 to 2090 with 1 year intervals
    ### Values to use in reports
    list_years_by5   = seq(minYear, maxYear, 5),  ### Sequence from 2010 to 2090 with 5 year intervals
    ### Years for plotting
    list_years_by10  = seq(minYear, maxYear, 10), ### Sequence for tick marks
    list_years_by20  = seq(minYear, maxYear, 20), ### Years by 20
    ### Groups for plotting
    groupLevels0     = c("sector", "adaptation", "impactYear", "impactType", "modelType", "model", "region"),
    ### Values to use in discounting
    baseYear0        = 2010,
    rate0            = 0.03
  )

  ### List to return
  # returnList <- list(
  #   messages_data    = messages_data,
  #   messages_fredi = messages_fredi,
  #   fredi_config   = fredi_config
  # )
  # ### Save the list
  # configFilePath  <- paste(system.file(package="cirafredi"), "extdata", "fredi_config.rda", sep="/")
  save(fredi_config, file=outPath)
 # return(returnList)
}
### Run the function
# frediConfig()
# rm("frediConfig")

