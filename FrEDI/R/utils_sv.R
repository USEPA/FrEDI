###### Overview ######
### This file contains helper functions for the FrEDI SV module.


###### calc_countyPop ######
### Created 2022.02.14. Last updated 2021.02.14
### This function attempts to load a user-specified input file
### Use this function to calculate tract level impacts

calc_countyPop <- function(
  regPop,  ### Dataframe of population projection
  funList, ### Dataframe of population projections
  years = seq(2000, 2099, 1)

){
  c_regions <- regPop$region %>% unique
  ### Iterate over regions
  x_popProj <-
    c_regions %>%
    lapply(function(region_i){
      ### Subset population projection to a specific region
      ### Get states in the region
      ### Get unique years
      df_i     <- regPop %>% filter(region==region_i)
      states_i <- funList[[region_i]] %>% names
      years_i  <- df_i$year %>% unique %>% sort

      ### Iterate over states
      regionPop_i <- states_i %>%
        lapply(function(state_j){
          ### Function for state j
          fun_j   <- funList[[region_i]][[state_j]]$state2region

          # state_j %>% print
          df_j <-
            data.frame(
              x = years_i,
              y = fun_j(years_i)
            ) %>%
            rename(
              year    = x,
              ratioState2RegionPop = y
            ) %>%
            mutate(
              state   = state_j,
              region  = region_i
            )

          ### Get list of counties in the state
          geoids_j      <- funList[[region_i]][[state_j]]$county2state %>% names
          statePop_j    <- geoids_j %>%
            lapply(function(geoid_k){

              fun_k <- funList[[region_i]][[state_j]]$county2state[[geoid_k]]

              df_k  <-
                data.frame(
                  x = years_i,
                  y = fun_k(years_i)
                ) %>%
                rename(
                  year    = x,
                  ratioCounty2StatePop = y
                ) %>%
                mutate(
                  state   = state_j,
                  geoid10 = geoid_k
                )
              return(df_k)
            }) %>%
            (function(z){do.call(rbind, z)})

          df_j <- df_j %>%
            left_join(
              statePop_j, by = c("state", "year")
            )

          return(df_j)
        }) %>%
        (function(y){
          do.call(rbind, y)
        })

      df_i <- df_i %>%
        left_join(
          regionPop_i, by = c("region", "year")
        ) %>%
        mutate(
          state_pop  = region_pop * ratioState2RegionPop,
          county_pop = state_pop  * ratioCounty2StatePop
        )
      return(df_i)
    }) %>%
    (function(x){
      do.call(rbind, x)
    }) %>%
    as.data.frame

  return(x_popProj)
}


###### get_svImpactsList ######
### This function reads in impacts data, formats it, and then creates impacts lists from it

get_svImpactsList <- function(
  dataFile = NULL,
  dataPath = file.path(getwd(), "inst", "extdata"),
  outFile  = NULL,
  outPath  = file.path(getwd(), "data"),
  createList = F,
  # modelType = "gcm",
  save     = F,
  return   = T
){
  ###### Check for data ######
  ### Check if there is a data file and if not, exit
  if(is.null(dataFile)){
    paste0("Argument `dataFile=NULL`. Please provide a valid value to argument `dataFile`.") %>% message
    paste0("\n", "Exiting...") %>% message
    return()
  }
  ### Check if there the data file exists and if not, exit
  dataFilePath   <- dataPath %>% file.path(dataFile)
  dataPathFiles  <- dataPath %>% list.files
  dataFileExists <- dataFile %in% dataPathFiles
  if(!dataFileExists){
    paste0("File `dataFile=", dataFile, "` not found in `dataPath='", dataPath,"'`.") %>% message
    paste0("\n", "Exiting...") %>% message
  }
  ###### Load data from CSV ######
  paste0("Loading data from `dataFile=", dataFile, "` in `dataPath='", dataPath,"'`...") %>% message
  df_data <- dataFilePath %>% read.csv
  rows0   <- df_data %>% nrow
  tracts0 <- df_data$tract %>% unique %>% length
  paste0("\t", "Data has ", rows0, " rows and ", tracts0, " unique tracts.") %>% message

  ###### Gather data ######
  # x_subUnit <- ifelse(modelType=="gcm", "deg", "cm")
  paste0("\n", "Gathering data...") %>% message
  df_data    <- df_data %>%
    gather(
      key   = "driverValue_txt",
      value = "sv_impact",
      -c("tract")
    )

  df_data %>% names %>% print
  df_data$driverValue_txt %>% unique %>% print

  df_data    <- df_data %>%
    ### Convert tract to FIPS
    rename(fips = tract) %>%
    mutate(fips = fips %>% as.character) %>%
    ### Convert driver value text to numeric
    mutate(
      # driverValue = driverValue_txt %>% (function(x){gsub(x_subUnit, "", x)}) %>% as.numeric
      driverValue = driverValue_txt %>%
        (function(x){gsub("deg", "", x)}) %>%
        (function(x){gsub("cm", "", x)}) %>%
        (function(x){gsub("X", "", x)}) %>%
        as.numeric
    )



  ###### Check data against sv data ######
  paste0("\n", "Checking data against SV data...") %>% message
  ### Load sv data "svData"
  df_sv_path <- outPath %>% file.path("svDataList.rdata")
  load(df_sv_path)
  svData <- svDataList$svData; rm("svDataList")
  ### Join with SV data
  df_data <- df_data %>%
    left_join(
      svData %>% select(c("region", "state", "county", "fips")),
      by = "fips"
    )
  ###### Remove missing values ######
  tracts_na  <- (df_data %>% filter(is.na(county)))$fips %>% unique %>% length
  tracts_nan <- (df_data %>% filter(is.nan(sv_impact) | is.infinite(sv_impact)))$fips %>% unique %>% length
  # tracts_inf <- (df_data %>% filter(is.infinite(sv_impact)))$fips %>% unique %>% length
  ### Counties
  if(tracts_na>0){
    paste0("\t", "Removing observations for ", tracts_na, " tracts with missing county information...") %>% message
    df_data <- df_data %>% filter(!is.na(county))
  }
  ### NaN
  if(tracts_nan>0){
    paste0("\t", "Removing observations for ", tracts_nan, " tracts with NaN values for impacts...") %>% message
    df_data <- df_data %>% filter(!is.infinite(sv_impact)) %>% filter(!is.nan(sv_impact))
  }

  ### Return if createList=F
  if(!createList){
    return(df_data)
  }
  Sys.sleep(.1)

  ###### Unique Tracts ######
  c_fips <- df_data$fips %>% unique

  ###### Create the impacts list ######
  ### Start system time
  ### Initialize impact list
  ### Iterate over values
  paste0("\n", "Creating impacts list...") %>% message
  sysTime1    <- Sys.time()
  impactsList <- list()

  for(i in 1:length(c_fips)){
    # for(i in 1:2){
    fips_i <- c_fips[i]
    df_i   <- df_data %>% filter(fips == fips_i)

    ### 11 rows: df_i %>% nrow %>% print
    x_i    <- c(0, df_i$driverValue)
    y_i    <- c(0, df_i$sv_impact)

    which_there  <- y_i[!is.na(y_i)]
    length_there <- which_there %>% length

    if(length_there<2){
      impactsList[[fips_i]] <- NA
    } else{
      fun_i        <- approxfun(x = x_i, y = y_i)
    }
    # fun_i        <- approxfun(x = x_i, y = y_i)
    impactsList[[fips_i]] <- fun_i
    Sys.sleep(.001)
  }
  sysTime2 <- Sys.time()
  deltaTime <- (sysTime2 - sysTime1)

  paste0("\t", "Created impact list in ", deltaTime) %>% message

  if(save){
    paste0("\n", "Saving impacts list...") %>% message
    ### Check that the directory exists
    if(dir.exists(outPath) & !is.null(outFile)){
      outFilePath <- outPath %>% file.path(paste(outFile, "rdata", sep="."))
      save(impactsList, file=outFilePath)
      paste0("\t", "Impacts list saved.") %>% message
    } else{
      fail_msg <- ifelse(
        is.null(outFile),
        paste0("\n\t", "Warning: `outFile=NULL` does not exist."),
        paste0("\n\t", "Warning: directory `outPath='", outPath, "'` does not exist.")
      )
      fail_msg %>% message
      paste0("\t", "Returning impacts list and exiting without saving...") %>% message
      return(impactsList)
    }
  }
  paste0("\n\t", "Returning impacts list and exiting...") %>% message
  return(impactsList)
}


