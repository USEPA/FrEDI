###### interpolate_annual ######
### Created 2021.02.05. Updated 2021.02.05
### Interpolate Annual Values
### This function interpolates missing values for temperature, GDP, or population inputs
interpolate_annual <- function(
    data     = NULL, ### Input dataframe, with list of years
    years    = NULL, ### List of years to interpolate
    column   = NULL, ### Column to create results for
    rule     = NULL, ### for interpolation,
    method   = NULL ### method for interpolation; default=linear
){
  ##### Names ######
  names_inputs    <- data %>% names
  if(is.null(column)){column <- names_inputs[2]}

  ##### Number of regions and years ######
  num_years       <- years %>% length
  regions         <- data$region %>% unique
  num_regions     <- regions %>% length

  ##### Interpolation rule ######
  ### Return NA values outside of extremes
  if(is.null(rule)){rule <- c(1:1)}
  ### If only one value is provided use the same for left and right
  else if(length(rule)==1){rule <- rep(rule, 2)}

  if(is.null(method)){method <- "linear"}

  ###### Remove missing values ######
  data     <- data %>% filter(!is.na(data[,column]))

  ###### Interpolate missing values for each region ######
  ### Filter to the region and then interpolate missing values
  df_interp    <- lapply(
    1:num_regions,
    function(i){
      region_i <- regions[i]
      df_i     <- data %>% filter(region==region_i)

      new_i    <- approx(
        x      = df_i$year,
        y      = df_i[,column],
        xout   = years,
        rule   = rule,
        method = method
      ) %>%
        as.data.frame
      names(new_i) <- c("year", column)
      new_i$region <- region_i
      return(new_i)
    }
  ) %>%
    (function(i){
      do.call(rbind, i)
    })

  return(df_interp)
} ### End function




###### match_scalarValues ######
### Last updated 2021.02.05
### Match Scalar Values
### This function matches interpolated scalar values for each scalar type to the time series scenario information
### Scalar types are: physAdj, physMultiplier, damageAdj, econScalar, econMultiplier
### Function "match_scalarValues" replaces "get_popWts", "get_physMultipliers", and "get_econScalars"
match_scalarValues <- function(
    data,    ### Initial results dataframe
    scalars, ### Scalars dataframe
    scalarType
){
  ### Scalar columns to rename
  newColNames    <- scalarType %>% paste0(c("Name", "Value"))
  renameCols     <- "scalarName" %>% c("value")
  ### Scalar identifier column
  scalarColName  <- newColNames[1]
  scalarValName  <- newColNames[2]
  ### Rename the scalar identifier column to match that of the data
  scalarNames_1   <- scalars %>% names
  names(scalars)[which(scalarNames_1 == renameCols[1])] <- scalarColName

  ###### Get scalars of particular type ######
  scalars         <- scalars %>% filter(scalarType==scalarType)

  ###### Separate scalar info into national and regional ######
  scalars_regional <- scalars %>% filter(national_or_regional == "regional")
  scalars_national <- scalars %>% filter(national_or_regional == "national")

  ###### ScalarName == "None" ######
  ### Filter the data to those for which the scalar identifier == "none"...value = 1
  df_none     <-  data %>% filter(data[,scalarColName] == "none") %>% mutate(value = 1)

  ###### Regional values ######
  scalars_regional <- scalars %>% filter(national_or_regional == "regional")
  scalarNames_reg  <- scalars_regional[,scalarColName] %>% unique
  df_regional      <- data %>%
    filter(!(data[,scalarColName] == "none") & data[,scalarColName] %in% scalarNames_reg) %>%
    left_join(scalars_regional, by=c("year", "region", scalarColName)) %>%
    select(-c("scalarType", "national_or_regional"))

  ###### National values ######
  scalars_national <- scalars %>% filter(national_or_regional == "national") %>% select(-region)
  scalarNames_nat  <- scalars_national[,scalarColName] %>% unique
  df_national      <- data %>%
    filter(!(data[,scalarColName] == "none") & data[,scalarColName] %in% scalarNames_nat) %>%
    left_join(scalars_national, by=c("year", scalarColName)) %>%
    select(-c("scalarType", "national_or_regional"))


  ###### Rename value column ######
  df_x    <- rbind(df_none, df_regional, df_national)
  names_x <- df_x %>% names
  names(df_x)[which(names_x == renameCols[2])] <- scalarValName

  ###### Return results values ######
  return(df_x)
}




###### get_econAdjValues ######
### Last updated 2021.02.05
### Get Economic Adjustment/Multiplier
### This function matches interpolated scalar values for each scalar type to the time series scenario information
### Scalar types are: physAdj, physMultiplier, damageAdj, econScalar, econMultiplier
### Function "get_econAdjValues" replaces "get_econMultipliers"
get_econAdjValues <- function(
    data,     ### Initial results dataframe
    scenario,  ### Population and GDP scenario
    multipliers ### List of multipliers
){
  ###### Get scenario information ######
  scenarioNames   <- scenario %>% names
  scenarioRegions <- scenario$region %>% unique

  ### update multipliers
  multipliers   <- multipliers[multipliers!="none"]
  scenarioNames <- scenarioNames[scenarioNames %in% multipliers]

  ###### Multipliers are all national for now ######
  ### Get values for a single region since the multipliers are the same for all regions
  scalars         <- scenario %>% filter(region==scenarioRegions[1])

  ###### Gather scenario information ######
  scalars    <- scalars %>%
    select(c("year", all_of(scenarioNames))) %>%
    gather( key = "econMultiplierName", value="econMultiplierValue", -year)

  ###### Multiplier Adjustment ######
  ### Rename scalars and convert year to character
  scalarAdj  <- scalars %>%
    mutate(year = year %>% as.character) %>%
    rename(
      year0        = year,
      econAdjName  = econMultiplierName,
      econAdjValue = econMultiplierValue
    )

  ###### Format data ######
  data <- data %>% mutate(econAdjName = econMultiplierName)

  ###### ScalarName == "None" ######
  ### Filter the data to those for which the scalar identifier == "none"...value = 1
  ### Set econMultiplierValue, econAdjValue == 1 if scalarMultiplierName=none
  df_none     <- data %>%
    filter(econMultiplierName == "none") %>%
    mutate(
      econMultiplierValue = 1,
      econAdjValue        = 1
    ) %>%
    select(-c("econAdjName"))

  ###### Other Multipliers ######
  # names(data) %>% print
  # names(data) %>% print
  # names(data) %>% print
  df_notNone      <- data %>%
    filter(econMultiplierName %in% multipliers) %>%
    left_join(scalars, by=c("year", "econMultiplierName")) %>%
    left_join(scalarAdj, by=c("year0", "econAdjName")) %>%
    select(-c("econAdjName"))

  ###### Rename value column ######
  df_x    <- rbind(df_none, df_notNone)

  ###### Return results values ######
  return(df_x)
}


###### calcScalars ######
### Last updated 2021.02.05
### Calculate Scalars
### This function calculates the physical scalar value, the economic scalar value, and their product
### The physical and economic scalars refer to the time series column from which the Annual Sectors tab
###   in the Excel tool draws values.
calcScalars <- function(
    data,   ### Initial results dataframe
    elasticity = NULL ### An elasticity to use to adjust values
){
  ###### Calculate physical scalar ######
  ### Physical scalars are the product of the physical scalar, the physical adjustment, and the damage adjustment
  df_x   <- data %>% mutate(physScalar = physScalarValue * physAdjValue * damageAdjValue )


  ###### Adjust Elasticity for VSL ######
  ### Adjust Elasticity for VSL only
  if(!is.null(elasticity)){
    df_not_vsl <- df_x %>% filter(econMultiplierName!="vsl_usd")
    df_vsl     <- df_x %>% filter(econMultiplierName=="vsl_usd") %>% mutate(exp0 = elasticity)
    df_x       <- df_not_vsl %>% rbind(df_vsl); rm("df_not_vsl", "df_vsl")
    # if(is.numeric(elasticity)){
    #   df_x   <- df_x %>% mutate(exp0 = elasticity)
    # }
  }

  ###### Calculate economic adjustment ######
  ### Economic multipliers are the economic multiplier value divided by the adjustment
  ### The economic multiplier value is 1, GDP, or GDP per capita
  ### The economic adjustment value is usually the economic multiplier value at a reference year
  df_x   <- df_x %>% mutate(econMultiplier = (econMultiplierValue / econAdjValue)**exp0 )

  ###### Calculate economic scalar ######
  ### The economic scalar is calculated using the following equation.
  ### Constants c0, c1, and exp0 are from the
  df_x   <- df_x %>% mutate(econScalar = c0 + c1 * econScalarValue * (econMultiplier) )

  ###### Calculate economic-physical scalar ######
  ### Combine the physical and economic scalar.
  df_x   <- df_x %>% mutate(physEconScalar  = econScalar * physScalar )

  ###### Return ######
  return(df_x)
}

###### get_scenario_id ######
### Function to standardize the creation of the scenario_id
get_scenario_id <- function(
    data_x,
    include=c("model_dot", "region_dot") ### Character vector of column names to include
){
  ### Other vals
  mnl0   <- "\n" ### Message new line
  msg0   <- "\t" ### Message indent level 0
  mcom   <- ", " ### Comma for collapsing lists
  mqu0   <- "\'" ### Message quote
  mqu1   <- mqu0 %>% paste0(mcom, mqu0, collapse="")
  mend0  <- "..."
  ### Columns to include
  main0  <- c("sector", "variant", "impactYear", "impactType", "model_type")
  cols0  <- main0  %>% c(include)
  ### Check names
  names0 <- data_x %>% names
  cCheck <- (cols0 %in% names0)
  nCheck <- (!cCheck) %>% which %>% length
  if(nCheck){
    c("In get_scenario_id:") %>% c(mnl0, msg0) %>%
      c("Data is missing columns ") %>% c(mqu0, paste(cols0[!cCheck], collapse=mqu1), mqu0, mend0) %>%
      c("Creating `scenario_id` from columns ") %>% c(mqu0, paste(cols0[cCheck], collapse=mqu1), mqu0, mend0)
    ### New names
    cols0  <- cols0[cCheck]
  }
  scen_x <- data_x[,cols0]
  scen_x <- scen_x %>% apply(1, function(x){as.vector(x) %>% paste(collapse ="_")}) %>% unlist
  data_x <- data_x %>% mutate(scenario_id = scen_x)
  return(data_x)
}


###### get_impactFunctions ######
### Last updated 2021.02.05
### Get Impact Functions (createSystemData)
### This is used by createSystemData (see inst/extdata/createSystemData.R) to generate impact functions
### This function can be run separately and its outputs saved as an R data object to facilitate computation time.
get_impactFunctions <- function(
    x         = NULL, ### Data frame with scaled impacts data
    groupCol  = NULL, ### Which column to look for the scenario column name (default = temp_impact_scenario )
    xCol      = NULL, ### Which column to use for x (default = temp_C)
    yCol      = NULL, ### Which column to use for y
    # extrapolate = FALSE, ### Whether to extrapolate by default
    extend_from = NULL, ### Maximum value for model type to extend from, if not missing
    extend_to   = NULL, ### Extend last points for x
    extend_all  = FALSE, ### Whether to extend all models or just those that go to the max model value
    unitScale   = NULL ### Scale between values
){
  ###### Defaults ######
  unitScale   <- ifelse(is.null(unitScale),        1, unitScale)
  # extend_to      <- ifelse(is.null(extend_to     ),        1, unitScale)
  ###### Group data ######
  x$group_id  <- x[,groupCol]
  x$xIn       <- x[,    xCol]
  x$yIn       <- x[,    yCol]
  ###### Extend from/to ######
  ### Make sure they are numeric
  extend_from <- extend_from %>% as.character %>% as.numeric
  extend_to   <- extend_to   %>% as.character %>% as.numeric

  ###### Groups ######
  ### Column Names
  # names_x  <- c(groupCol, xCol, yCol)
  ### Create groups and get group keys
  ### Group keys
  x        <-  x %>% group_by(group_id)
  groups_x <- (x %>% group_keys)$group_id %>% unique

  ### Initialize data
  xIn_min  <- 0
  yIn_min  <- 0
  df_0     <- data.frame(xIn = xIn_min, yIn = yIn_min)
  # df_0     <- data.frame(xIn = 0, yIn = 0)
  ###### Generate list of impact functions ######
  ### Iterate over the groups
  list_x   <- x %>%
    group_map(function(.x, .y, .keep=T){
      group_i     <- .x[,groupCol] %>% unique

      ###### Subset values ######
      ### Subset data to scenario name and exclude NA values, then add a zero value
      df_i        <- .x   %>% select(xIn, yIn) %>% filter(!is.na(yIn))
      df_i        <- df_0 %>% rbind(df_i)

      ###### Information about Extrapolation values ######
      ### Length of df_i
      len_i       <- df_i %>% nrow
      # ### Extend values out to 10 degrees of warming
      xIn_max     <- df_i$xIn[len_i]
      yIn_max     <- df_i$yIn[len_i]
      yMaxNew     <- NA

      # extrapolate %>% print
      ### Whether to extend values
      ### Extend values out to the specified value
      ### - Find linear relationship between last two points
      ### - Interpolate the last few values
      # extrapolate <- TRUE
      extrapolate <- (xIn_max == extend_from) & (extend_from!=extend_to)
      if(extend_all) extrapolate <- TRUE
      # extrapolate %>% print
      if(extrapolate){
        df_ref_i  <- df_i[len_i + -1:0,]
        # df_ref_i %>% print
        ### Get linear trend
        lm_i      <- lm(yIn~xIn, data=df_ref_i)
        ### Extend values
        df_new_i  <- data.frame(xIn = seq(xIn_max + unitScale, extend_to, unitScale))
        df_new_i  <- df_new_i %>% mutate(yIn = xIn * lm_i$coefficients[2] + lm_i$coefficients[1])
        ### Bind the new observations with the other observations
        df_i <- df_i %>% rbind(df_new_i)
        ### Sort and get new y value to extend to
        which_i <- (df_i$xIn == extend_to) %>% which
        yMaxNew <- df_i$yIn[which_i]
      }

      ###### Linear Interpolation ######
      ### Create a piece-wise linear interpolation function using approxfun and defaults
      ###    rule = 1 (Returns NA for x-values outside range)
      ###    ties = mean (take the average of multiple values)
      # fun_i <- approxfun(x = df_i$xIn, y = df_i$yIn, method = "linear", rule = 1)
      fun_i <- approxfun(
        x = df_i$xIn,
        y = df_i$yIn,
        method = "linear",
        yleft  = yIn_min,
        yright = yMaxNew
      )

      return(fun_i)

    }) ### End group map

  ##### Add names to the list
  names(list_x) <- groups_x

  ###### Return Object ######
  return(list_x)
}


###### interpolate_impacts ######
### Created 2021.02.05. Last updated 2021.02.11.
### 2021.02.11: Changed name from interpolate_temps to interpolate_impacts and added SLR.
### Calculate impacts (binning)
### This function uses the dplyr group_map capabilities to interpolate scaled impacts by temperature or sea level rise (SLR) relationships
interpolate_impacts <- function(
    functions   = NULL, ### List of functions
    xVar        = NULL, ### Temperatures or SLRs to interpolate,
    years       = NULL  ### Years
){
  ### Names of functions and number of functions
  functionNames    <- functions %>% names
  numFunctions     <- functions %>% length

  ### Iterate over the groups
  scaledImpacts_x   <-
    1:numFunctions %>%
    lapply(function(i){
      ### Group, get group function, then get impacts
      scenario_i       <- functionNames[i]
      fun_i            <- functions[[scenario_i]]
      scaledImpacts_i  <- xVar %>%  fun_i
      df_i             <- data.frame(
        year           = years,
        xVar           = xVar,
        scaled_impacts = scaledImpacts_i,
        scenario_id    = scenario_i
      )
      return(df_i)
    }) %>%
    (function(i){
      do.call(rbind, i)
    }) ### End group map
  # scaledImpacts_x %>% names %>% print
  return(scaledImpacts_x)
}

###### get_annual_model_stats  ######
### Created 2021.01.11. Last updated 2021.02.26:
### Updated method for calculating model statistics and dealing with NA values.
### This function returns a table with gcm averages, minimums, and maximums
get_annual_model_stats <- function(
    data      = NULL, ### Dataframe of results
    sectors   = NULL, ### Name of sectors to get statistics for
    yVar      = "annual_impacts", ### Column to get averages for
    groupCol = c("sector", "variant", "model_type", "impactType", "impactYear") ### Column(s) to use for grouping
){
  ###### Ungroup data ######
  # data        <- data %>% ungroup
  ###### Subset to sector ######
  ### Get unique sectors if none are specified
  defaultSectors   <- data$sector %>% as.character %>% unique
  if(is.null(sectors)){sectors <- defaultSectors}

  ###### Names of Data ######
  ### Models
  model_labels <- data$model %>% unique
  num_models   <- model_labels %>% length

  ###### Rename the columns ######
  ### To standardize
  renameCols   <- c(yVar)
  newColNames  <- c("yvar")

  ### Keep track of the data names, filter to the standardized data names, then rename the desired column
  # data %>% names %>% print
  data         <- data %>%
    (function(y){
      names_y   <- y %>% names
      whichVar <- which(names_y == yVar)
      names(y)[whichVar]  <- "yvar"
      return(y)
    })

  ###### Which observations are NA ######
  ### Determine which observations are NA
  data         <- data %>%
    mutate(not_na = !is.na(yvar)) %>%
    mutate(not_na = not_na * 1)

  ### Model Type
  model_aves_x  <- c("Model Average", "Interpolation") ### Labels for model averages
  model_type_x  <- (data$model_type %>% unique)[1]
  model_label_x <- ifelse(tolower(model_type_x)=="gcm", model_aves_x[1], model_aves_x[2])

  ###### Reshape the data ######
  # default_groupCols <- c("sector", "variant", "model_type", "impactType", "impactYear", "region")
  default_groupCols <- c("sector", "variant", "model_type", "impactType", "impactYear", "region", "year", "model", "yvar")
  groupByCols       <- default_groupCols[which(default_groupCols %in% names(data))]

  ### Reshape the data and prepare a column indicating which rows have is.na() for all models
  data    <- data %>%
    select(c(all_of(groupByCols))) %>%
    mutate(not_na = !is.na(yvar))

  ###### Summarize by group columns ######
  ### Add group column with year
  groupByCols       <- groupByCols[which(!(groupByCols %in% c("model", "yvar")))]
  df_summary   <- data %>%
    group_by_at(c(all_of(groupByCols))) %>%
    summarize_at(.vars = c("not_na"), sum, na.rm=T) %>%
    rename(sum_notNA = not_na) %>%
    mutate(
      sum_notNA = (sum_notNA > 0)*1,
      sum_notNA = sum_notNA %>% na_if(0)
    )

  ###### Add the summary back into the data ###### groupByCols
  data <- data %>% left_join(df_summary, by = c(groupByCols))

  ###### Calculate stats ######
  ### Separate observations that are all NA from those that have at least one non NA value
  is_naOnly    <- data$sum_notNA %>% is.na

  ### Treat NA only values separate from those with non NA values
  ### First figure out which are which
  which_naOnly <-   is_naOnly  %>% which
  which_nMiss  <- (!is_naOnly) %>% which
  ### Number of each
  num_naOnly   <- which_naOnly %>% length
  num_nMiss    <- which_nMiss %>% length
  ### Initialize dataframes
  data_naOnly  <- data.frame()
  data_nMiss   <- data.frame()

  if(num_naOnly > 0){
    data_naOnly      <- data[which_naOnly,] %>% select(-sum_notNA) %>%
      mutate(min = NA, mean = NA, max=NA)
  }

  if(num_nMiss > 0){
    data_nMiss      <- data[which_nMiss,] %>% select(-sum_notNA) %>%
      group_by_at(c(all_of(groupByCols))) %>%
      summarize_at(.vars=c("yvar"), tibble::lst(min, mean, max), na.rm=T)
  }

  ###### Bind results together ######
  df_results <- data_nMiss %>%
    rbind(data_naOnly) %>%
    mutate(model=model_label_x) %>%
    rename(modelMin = min, modelAve=mean, modelMax=max) %>%
    ungroup

  ###### Return ######
  return(df_results)
}


###### slr_Interp_byYear ######
### utils for aggregate_impacts
slr_Interp_byYear <- function(
    data, ### Driver scenario with columns year, slr_cm
    yCol = "driverValue", ### Column to look for the driver value
    silent=TRUE
){
  ###### Defaults ######
  ### Rename y Column
  if(is.null(yCol)){yCol <- "driverValue"}
  oldColName_y <- yCol %>% c()
  newColName_y <- "yValue" %>% c()
  newColRef_y  <- newColName_y %>% paste0("_ref")
  data <- data %>% rename_at(.vars=c(all_of(oldColName_y)), ~newColName_y)
  ### Messaging
  if(is.null(silent)){silent <- T}
  if(silent){msgUser <- F} else{msgUser <- T}

  ###### Assign data ######
  ### SLR scenario info
  assign("co_models", rDataList[["co_models"]])
  co_slrs    <- co_models %>% filter(modelType=="slr") %>% rename(model=model_label)
  slr_levels <- c("0cm", co_slrs$model_dot)
  slr_labels <- c("0 cm", co_slrs$model)
  slr_orders <- slr_levels %>% factor(levels=slr_levels) %>% as.numeric
  slr_min    <- (slr_orders %>% min(na.rm=T)) #+ 1
  slr_max    <-  slr_orders %>% max(na.rm=T)

  ### Sea level rise information
  assign("slr_df", rDataList[["slr_cm"]])
  df_slr_years <- slr_df$year %>% unique
  ### Refactor model
  slr_df       <- slr_df %>%
    mutate(model        = model %>% as.character) %>%
    mutate(model_factor = model %>% factor(slr_levels, slr_labels)) %>%
    mutate(model_level  = model_factor %>% as.numeric) %>%
    arrange_at(.vars=c("model_level", "year")) %>%
    mutate(model = model_factor %>% as.character) %>%
    select(-c("model_factor")) %>% as.data.frame

  ### Character vector of model names
  c_slrs0      <- slr_labels

  ### Check that years are unique
  data_years   <- data$year %>% unique
  n_data_years <- data_years %>% length
  nrows_data   <- data %>% nrow
  check_unique_years <- nrows_data > n_data_years

  if(check_unique_years){
    if(msgUser){
      message("\t", "values for 'yCol' are not unique...")
      message("\t", "Averaging over 'yCol' values...")
    }
    data <- data %>%
      group_by_at(c("year")) %>%
      summarize_at(c("yValue"), mean, na.rm=T)
  }
  rm("n_data_years", "nrows_data", "check_unique_years")

  ###### Prepare data ######
  ### Filter to appropriate years
  data       <- data %>% filter(year %in% df_slr_years)
  n_years    <- data %>% nrow

  ###### Standard Columns ######
  ### JoinCols
  join0   <- c("year", "model_level")
  select0 <- c("year", newColName_y, newColRef_y, "model")
  select1 <- c("year", newColName_y, "lower_model", "upper_model", "lower_slr", "upper_slr")
  ### Format data
  # y    <- y %>% mutate(model_factor = model_factor %>% as.character)
  x      <- data; rm("data")
  y      <- slr_df %>% rename(yValue_ref = driverValue); rm("slr_df")
  ### Join
  z    <- x %>% left_join(y, by = "year")
  ### Filter observations
  z_lo <- z %>% filter(yValue_ref <= yValue); #n_lo <- z_lo %>% nrow
  z_hi <- z %>% filter(yValue_ref >= yValue); #n_hi <- z_hi %>% nrow
  ### Figure if years are missing
  yrs_z  <- x$year
  yrs_lo <- z_lo$year %>% unique %>% sort; nas_lo <- yrs_z[!(yrs_z %in% yrs_lo)]
  yrs_hi <- z_hi$year %>% unique %>% sort; nas_hi <- yrs_z[!(yrs_z %in% yrs_hi)]
  ### Add years to data
  dfNaLo <- data.frame(year = yrs_lo, model_level = slr_min) %>% left_join(z, by = c(all_of(join0)))
  dfNaHi <- data.frame(year = yrs_lo, model_level = slr_max) %>% left_join(z, by = c(all_of(join0)))
  ### Add missing values back in
  z_lo   <- z_lo %>% rbind(dfNaLo) %>% arrange_at(.vars=c(join0[1]))
  z_hi   <- z_hi %>% rbind(dfNaHi) %>% arrange_at(.vars=c(join0[1]))
  ### Get low values
  x_lo   <- z_lo %>%
    group_by_at(.vars=c("year")) %>%
    summarize_at(.vars=c("model_level"), max, na.rm=T) %>% ungroup %>%
    (function(a, b = x){
      b %>% left_join(a, by = c("year"))
    }) %>%
    left_join(y, by=c("year", "model_level")) %>%
    select(c(all_of(select0))) %>%
    rename(lower_slr = yValue_ref, lower_model = model)

  ### Get hi values
  x_hi   <- z_hi %>%
    group_by_at(.vars=c("year")) %>%
    summarize_at(.vars=c("model_level"), min, na.rm=T) %>% ungroup %>%
    (function(a, b = x){
      b %>% left_join(a, by = c("year"))
    }) %>%
    left_join(y, by=c("year", "model_level")) %>%
    select(c(all_of(select0))) %>%
    rename(upper_slr = yValue_ref, upper_model = model)

  ### Join all
  z <- x_lo %>% left_join(x_hi, by = c("year", all_of(newColName_y)))
  z <- z    %>% select(c(all_of(select1)))

  ### Add adjustment
  z <- z   %>%
    mutate(denom_slr  = upper_slr - lower_slr  ) %>%
    mutate(numer_slr  = upper_slr - yValue) %>%
    mutate(adj_slr    = numer_slr / denom_slr  ) %>%
    mutate(is_inf     = adj_slr %>% is.infinite) %>%
    mutate(adj_slr    = adj_slr * (!is_inf)) %>%
    mutate(adj_slr    = adj_slr %>% replace_na(0)) %>%
    select(-c("is_inf"))

  ### Rename yValue and return
  df_return <- z %>% rename_at(.vars=c(all_of(newColName_y)), ~oldColName_y)
  return(df_return)

}

###### fun_slrModel2height ######
### Helper function to convert SLR model to height in cm
fun_slrModel2Height <- function(
    col_x, ### column "model_dot"
    include   = c("factor", "values"),
    valType   = c("numeric", "character", "factor"),
    labelType = c("numeric", "character") ### Used for factor or label

){
  ### Checks
  do_factor <- "factor" %in% include
  do_values <- "values" %in% include
  do_both   <- do_factor & do_values
  ### Value types and priority
  valTypes <- c("numeric", "character", "factor")
  valType0 <- valType
  valType0 <- valTypes %>% (function(y, types_y=valTypes){
    ls1 <- ls0 <- types_y
    c0  <- ls0[1] %in% y
    c1  <- ls0[2] %in% y
    c3  <- ls0[2] %in% y
    if(c0) {ls1 <- ls0[1]}
    else if(c1) {ls1 <- ls0[2]}
    else        {ls1 <- ls0[3]}
    return(ls1)
  })
  do_numb  <- "numeric"   %in% valType
  do_char  <- "character" %in% valType
  do_fact  <- "factor"    %in% valType
  # valType %>% print; labelType %>% print
  ### Label types and priority
  labTypes <- c("numeric", "character")
  label_x0 <- labelType %>%
    (function(y, types_y=labTypes){
      ls1 <- ls0 <- types_y
      c0  <- do_numb | do_char
      c1  <- ls0[1] %in% y
      if(c0) {ls1 <- ls0[1]}
      else if(c1) {ls1 <- ls0[1]}
      else        {ls1 <- ls0[2]}
      return(ls1)
    })
  # label_x0 %>% print
  labChar  <- "character" %in% label_x0
  # label_x0 %>% print; labChar %>% print
  ### Original labels
  lvl_x0 <- col_x %>% unique
  df_x0  <- data.frame(levels=lvl_x0)
  ### Standardize
  df_x0$labels  <- gsub("_" , "", df_x0$levels)
  df_x0$numbers <- gsub("cm", "", df_x0$labels)
  df_x0$values  <- df_x0$numbers %>% as.character %>% as.numeric
  ### Sprt
  df_x0  <- df_x0 %>% arrange_at(.vars=c("values"))
  ### Create factor list
  list_x <- list(factors=df_x0)
  ### Adjust values
  vals_x <- NULL
  if(do_values){
    if(labChar){labels_x <- df_x0$labels}
    else       {labels_x <- df_x0$values}
    vals_x <- col_x  %>% factor(levels=df_x0$levels, labels=labels_x)
    if(do_char){vals_x <- vals_x %>% as.character}
    if(do_numb){vals_x <- vals_x %>% as.numeric  }
    list_x[["values"]] <- vals_x
  }
  ### Return list
  if     (do_both  ) {return_x <- list_x}
  else if(do_factor) {return_x <- list_x$factors}
  else               {return_x <- list_x$values}
  ### Return
  return(return_x)

}

###### SLR Extremes ######
### Function for dealing with SLR values above the maximum
fun_slrConfigExtremes <- function(
    slr_x, ### rDataList$slr_cm
    imp_x  ### rDataList$slrImpacts
){
  ### Vectors
  slrCols0 <- c("year", "model_cm")
  impCols0 <- c("sector", "variant", "region", "year", slrCols0)
  arrange0 <- c("driverValue", "model_cm")
  suffix0  <- c("1", "2")
  bounds0  <- c("lower", "upper")
  select0  <- c("model_dot", "model", "model_type", "model_cm")
  join0    <- c("year", "model_dot")
  join1    <- c("sector", "variant", "impactType", "impactYear", "region", "year")
  drop0    <- "driverValue" %>% paste0(suffix0) %>% c("scaled_impacts" %>% paste0(suffix0)) %>% c("delta_impacts", "delta_driverValue")
  ### Prepare data
  ### SLR Heights
  slr_df   <- slr_x %>% #filter(year <=2090) %>%
    mutate(model_cm = gsub("cm", "", model_dot) %>% as.character %>% as.numeric) %>%
    arrange_at(.vars=c(all_of(slrCols0)));
  rm("slr_x")
  # slr_df %>% head %>% print
  ### SLR Impacts
  imp_df   <- imp_x %>% #filter(year <=2090) %>%
    mutate(model_cm = gsub("cm", "", model_dot) %>% as.character %>% as.numeric) %>%
    arrange_at(.vars=c(all_of(impCols0)));
  rm("imp_x")
  # imp_df %>% head %>% print

  ### Get upper and lower for each year
  slrYears <- slr_df$year %>% unique %>% sort
  slr_extr <- slrYears %>% lapply(function(
    year_i, data_x = slr_df
  ){
    data_i     <- data_x %>% filter(year==year_i) %>% arrange_at(.vars=c(all_of(arrange0)))
    vals_i     <- data_i$driverValue
    ### Unique values and length
    unique_i   <- vals_i %>% unique;
    n_unique_i <- unique_i %>% length
    ### Figure out which the last values belong to
    last_i     <- vals_i[n_unique_i + (-1):0]
    which_i    <- last_i %>% lapply(function(val_j, vals_y = vals_i){
      (vals_y == val_j) %>% which %>% last
    }) %>% unlist
    ### Get driver values and models for the last values
    drivers_i  <- data_i$driverValue[which_i]
    models_i   <- data_i$model_dot[which_i]
    ### Create a return a dataframe with the model value, driver value, year, and bound
    df_i       <- data.frame(year = year_i, model_dot = models_i, driverValue = drivers_i, valueType = c(bounds0))
    return(df_i)
  }) %>% (function(df_i){do.call(rbind, df_i)})
  # slr_extr %>% glimpse %>% print; slr_extr$model_dot %>% unique %>% print

  ### Join with impacts: 7644 rows
  imp_extr <- slr_extr %>% left_join(imp_df, by = c(all_of(join0)))
  # imp_extr %>% glimpse %>% print; (nrow(imp_extr)/2) %>% print

  ### Separate lower and upper values and join them together
  imp_extr <- imp_extr %>% select(-c(all_of(select0)))
  ext_slr  <- imp_extr %>% (function(data_x){
    ### Separate into lower and upper values and join data
    data_lo <- data_x  %>% filter(valueType=="lower") %>% select(-c("valueType"))
    data_up <- data_x  %>% filter(valueType!="lower") %>% select(-c("valueType"))
    data_x  <- data_lo %>% left_join(data_up, by = c(all_of(join1)), suffix = suffix0)
    # data_x %>% names %>% print
    ### Calculate differences
    data_x  <- data_x  %>% mutate(delta_impacts     = scaled_impacts2 - scaled_impacts1)
    data_x  <- data_x  %>% mutate(delta_driverValue = driverValue2    - driverValue1)
    ### Calculate slope and intercept
    data_x  <- data_x  %>% mutate(driverValue_ref   = driverValue2)
    data_x  <- data_x  %>% mutate(impacts_intercept = scaled_impacts2)
    data_x  <- data_x  %>% mutate(impacts_slope     = delta_impacts/delta_driverValue)
    ### Replace zeros
    which0  <- (data_x$delta_driverValue == 0) %>% which
    data_x$impacts_slope[which0] <- 0
    # data_x %>% names %>% print
    ### Drop intermediate columns and return
    data_x  <- data_x  %>% select(-c(all_of(drop0)))
    return(data_x)
  })
  # ext_slr %>% glimpse %>% print;
  ### Format and Return
  ext_slr <- ext_slr %>% arrange_at(.vars=c(all_of(join1)))
  return(ext_slr)
}

####### Extend SLR Scenarios ######
### Function to extend SLR scenarios in createSystemData
extend_slr   <- function(
    x,
    # maxYear_x = 2090,
    newMax_x  = 2300,
    arrange_x = c("model", "year")
){
  ### Values
  maxYear_x <- x$year %>% max
  ### Format Dataframes
  x_nu <- data.frame(year = (maxYear_x + 1):newMax_x) %>% mutate(dummyCol = 1)
  x_up <- x %>% filter(year == maxYear_x) %>% mutate(dummyCol = 1) %>% select(-c("year"))
  x_lo <- x %>% filter(year <= maxYear_x)
  rm("x")
  ### Join data
  x_up <- x_up %>% left_join(x_nu, by = c("dummyCol")) %>% select(-c("dummyCol"))
  x    <- x_lo %>% rbind(x_up)
  rm("x_nu", "x_up", "x_lo")
  ### Arrange and standardize model type
  x  <- x %>% arrange_at(.vars = c(all_of(arrange_x))) %>% mutate(model_type = "slr")
  return(x)
}

####### fun_getNeighbors ######
### Figure out which SLR heights are immediately above and below a driver value
fun_getNeighbors <- function(
    x, ### X values
    values, ### values to compare
    col = "driverValue" # which column to compare
){

  # ### Mutate data
  ### Add a dummy column with a standardized name
  values <- values %>% as.data.frame
  values$newCol <- values[,col]

  ### Look for equal values
  values_equal <- values %>% filter(newCol==x)
  num_equal    <- values_equal %>% nrow

  ### If there are equal values, get the upper and lower value
  if(num_equal>0){
    ### If there is only one value that is equal, return that value twice
    if(num_equal==1){
      lo_order <- values_equal$order %>% unique
      hi_order <- lo_order
    }
    ### If there is more than one value that is equal, return the lower most and uppermost equal values
    else{
      c_orders <- values_equal$order
      lo_order <- values_equal$order %>% min(na.rm=T)
      hi_order <- values_equal$order %>% max(na.rm=T)
    }
  }
  ### If there are no equal values, figure if there are any values below the value
  else{
    values_below <- values %>% filter(newCol < x)
    num_below    <- values_below %>% nrow

    ### Get the values above it
    values_above <- values %>% filter(newCol > x)
    num_above    <- values_above %>% nrow

    ### If there are values below, get the values above it
    if(num_below==0){
      ### Return the zero value for the low value and the first value above for the hi value
      lo_order     <- 1
      hi_order     <- values_above$order %>% min(na.rm=T)
    } else{
      ### Figure out if there are any values above it
      if(num_above==0){
        ### Return the max value for the low value and the hi value
        lo_order     <- values_below$order %>% max(na.rm=T)
        hi_order     <- lo_order
      }
      ### If there are some numbers above and below it
      else{
        ### Return the max value for the low value and the hi value
        lo_order     <- values_below$order %>% max(na.rm=T)
        hi_order     <- values_above$order %>% min(na.rm=T)
      }
    }

  }
  # lo_order %>% print
  lo_values    <- values %>% filter(order==lo_order) %>% mutate(type="lower")
  hi_values    <- values %>% filter(order==hi_order) %>% mutate(type="upper")
  new_values   <- lo_values %>% rbind(hi_values)

  new_values   <- lo_values %>% rbind(hi_values)
  return(new_values)
}

###### fun_formatScalars ######
### Function to format scalars in createSystemData
fun_formatScalars <- function(
    data_x, ### rDataList$scalarDataframe
    info_x, ### rDataList$co_scalarInfo
    years_x ### rDataList$list_years
){
  names_x <- data_x$scalarName %>% unique
  num_x   <- names_x %>% length
  new_x   <- 1:num_x %>% lapply(function(i){
    ### Figure out name, method, region
    name_i     <- names_x[i]
    # name_i %>% print
    ### Dataframes
    scalar_i   <- data_x %>% filter(scalarName==name_i)
    info_i     <- info_x %>% filter(scalarName==name_i)
    ### Info about scalar
    method_i   <- info_i$constant_or_dynamic[1]
    method_i   <- ifelse(method_i=="constant", method_i, "linear")
    # region_i   <- info_i$national_or_regional[1]
    ### Interpolate data
    scalar_i   <- scalar_i %>% interpolate_annual(
      years  = years_x,
      column = "value", rule   = 1:2,
      method = method_i)
    ### Add in name and return
    scalar_i   <- scalar_i %>% mutate(scalarName=name_i)
    return(scalar_i)
  }) %>% (function(scalars_i){do.call(rbind, scalars_i)})
  ### Join info
  select0 <- c("scalarName", "scalarType", "national_or_regional")
  select1 <- c("scalarName", "region", "year", "value")
  info_x  <- info_x %>% select(c(all_of(select0)))
  new_x   <- new_x  %>% select(c(all_of(select1)))
  new_x   <- new_x  %>% left_join(info_x, by=c("scalarName"))
  ### Return
  return(new_x)
}


###### fun_getScale ######
### This function creates a set of breaks for a particular column
### It returns a list of breaks, the power of 10, and the limits
fun_getScale <-
  function(
    data,
    scaleCol = "driverValue",
    # zero = F,
    nTicks = 5
  ){

    ### Defaults
    if(is.null(scaleCol)) scaleCol <- "driverValue"
    ### Default is not to zero out in case there are negative numbers
    # if(is.null(zero))     zero <- F
    if(is.null(nTicks)){nTicks <- 5}

    ### Min/max values
    data <- data %>% as.data.frame
    xMin <- data[,scaleCol] %>% min(na.rm=T)
    xMax <- data[,scaleCol] %>% max(na.rm=T)

    ### Set minimum to zero unless the minimum is less than zero
    if(xMin > 0){
      xMin <- 0
    }

    if(xMax < 0){
      xMax <- 0
    }

    ### Min/max values
    ### Limit names, values, bounds, etc
    df_minMax <-
      data.frame(
        name  = c("min", "max"),
        value =  c(xMin, xMax),
        bound =  c(floor(xMin), ceiling(xMax)),
        boundType = c("floor", "ceiling")
      ) %>%
      ### Absolute value, Power of 10 and y-scale info
      mutate(bound_abs = bound %>% abs) %>%
      ### Calculate log 10 and replace values of infinity with 0
      mutate(log10 = (bound_abs %>% log10)) %>%
      mutate(log10 = log10 %>% abs %>% na_if(Inf)) %>%
      mutate(log10 = log10 %>% replace_na(0)) %>%
      ### Then get the floor of the log10 value
      mutate(power10 = log10 %>% floor)

    ### Get maximum power of 10, then scale to zero for negative numbers
    ### Integer division of power of 10 by 3 to get number of thousands
    ### Then get the modulus of the thousands
    x_power10Max <- df_minMax$power10 %>% max(na.rm=T)
    x_power1000  <- x_power10Max  %/% 3
    x_mod1000    <- x_power10Max  %% 3

    ### Rounded bounded values (round to 1 essentially)
    divideByPower         <- x_power10Max - 1
    minMax_scaled         <- df_minMax$value / 10^divideByPower
    bounds_scaled_rounded <- c(floor(minMax_scaled[1]), ceiling(minMax_scaled[2]))
    bounds_rounded        <- bounds_scaled_rounded * 10^divideByPower

    ###### Establish the range of x ######
    x_range      <- bounds_rounded
    x_range_p10  <- x_range / 10^x_power10Max
    x_range_dif  <- x_range_p10[2] - x_range_p10[1]

    ### Determine unit of breaks in power of 10
    x_unit_p10     <- 0.5
    x_breaks_p10   <- seq(x_range_p10[1], x_range_p10[2], by = x_unit_p10)
    n_Ticks        <- x_breaks_p10 %>% length
    if(n_Ticks>nTicks){
      x_unit_p10   <- 1
      x_breaks_p10 <- seq(x_range_p10[1], x_range_p10[2], by = x_unit_p10)
      n_Ticks      <- x_breaks_p10 %>% length
      if(n_Ticks>nTicks){
        x_unit_p10   <- 2
        x_breaks_p10 <- seq(x_range_p10[1], x_range_p10[2], by = x_unit_p10)
        n_Ticks      <- x_breaks_p10 %>% length
      }
    }
    x_breaks       <- x_breaks_p10 * 10^x_power10Max
    # return(x_breaks)

    ### Create list to return
    return_list <- list(
      breaks    = x_breaks,
      limits    = df_minMax$value,
      bounds    = bounds_rounded,
      power10   = x_power10Max,
      power1000 = x_power1000,
      mod1000   = x_mod1000
    )

    return(return_list)
  }

####### fredi_slrInterp ######
fredi_slrInterp <- function(
    data_x,
    slr_x, ### slrScenario
    groupByCols
    # drivers_x ### driverScenario %>% filter(tolower(model_type)=="slr") %>% select(-c("model_type"))
    # drivers_x, ### driverScenario %>% filter(tolower(model_type)=="slr") %>% select(-c("model_type"))
    ){
  
  #data_x <-df_slrImpacts
  #slr_x = slrScenario
  #groupByCols=slrGroupByCols
  
  names_slr      <- data_x %>% names; #names_slr %>% print
  ### Summary columns
  slrSumCols     <- c("scaled_impacts")
  n_slrSumCols   <- slrSumCols %>% length
  slrMutCols     <- c("lower_model", "upper_model")

  ### Info names
  ### "year", "driverValue", "lower_model" , "upper_model", "lower_slr" ,  "upper_slr"
  data_xAdj      <- slr_x; rm("slr_x")
  names_slrAdj   <- data_xAdj %>% names; #names_slrAdj %>% print
  other_slrCols  <- names_slrAdj[which(names_slrAdj!="year")]
  join_slrCols   <- c(groupByCols, "year") ### sectorprimary, includeaggregate
  join_cols0     <- c("driverValue", "year")

  ### Format values
  data_xAdj      <- data_xAdj %>% mutate_at(.vars=c(all_of(slrMutCols)), as.character)

  ### Join with slrInfo and convert columns to character
  data_xAdj      <- data_xAdj %>% mutate(equal_models = lower_model == upper_model)
  data_x         <- data_x    %>% left_join(data_xAdj, by=all_of(join_cols0))
  data_x %>% filter(!is.na(scaled_impacts)) %>% nrow %>% print
  rm("data_xAdj")

  ### Filter to conditions
  data_xEqual    <- data_x %>% filter( equal_models) %>% select(-c("equal_models"));
  data_xOther    <- data_x %>% filter(!equal_models) %>% select(-c("equal_models"));
  data_x %>% names %>% print
  data_x$model %>% unique %>% print
  data_x$model_dot %>% unique %>% print
  data_x$lower_model %>% unique %>% print
  data_x$upper_model %>% unique %>% print
  c(nrow(data_xEqual), nrow(data_xOther)) %>% print
  rm("data_x")
  ### Process observations that are equal
  if(nrow(data_xEqual)){
    ### Filter observations that are zeros only and make the summary column values zero
    data_xEqual0    <- data_xEqual %>% filter(lower_model=="0cm") %>% filter(model_dot=="30cm")
    data_xEqual1    <- data_xEqual %>% filter(lower_model!="0cm") %>% filter(model_dot==lower_model)
    c(nrow(data_xEqual0), nrow(data_xEqual1)) %>% print
    rm("data_xEqual")
    ### For observations that are zeros only and make the summary column values zero
    data_xEqual0    <- data_xEqual0 %>% mutate_at(.vars=c(all_of(slrSumCols)), function(y){0})
    ### Bind values back together
    data_xEqual     <- data_xEqual0 %>% rbind(data_xEqual1)
    rm("data_xEqual0", "data_xEqual1")
    ### Rename the model_dot, select appropriate columns
    data_xEqual     <- data_xEqual %>% mutate(model_dot="Interpolation")
    # data_xEqual %>% names%>% print
    data_xEqual     <- data_xEqual %>% select(c(all_of(names_slr)))
  } ### End if length(which_equal) > 0

  ### Observations that are greater than zero
  if(nrow(data_xOther)){
    ### Lower and upper column names and new names
    lowerSumCols   <- slrSumCols %>% paste("lower", sep="_")
    upperSumCols   <- slrSumCols %>% paste("upper", sep="_")
    ### Filter lower model_dot observations to those with a lower model_dot value == "0 cm" and others and drop model_dot column
    data_xLower0   <- data_xOther %>% filter(lower_model=="0cm")
    data_xLower1   <- data_xOther %>% filter(lower_model!="0cm")
    data_xUpper    <- data_xOther %>% filter(model_dot==upper_model ) %>% select(-c("model_dot"))
    rm("data_xOther")
    ### Rename columns
    data_xLower0   <- data_xLower0 %>% rename_with(~lowerSumCols[which(slrSumCols==.x)], .cols=slrSumCols)
    data_xLower1   <- data_xLower1 %>% rename_with(~lowerSumCols[which(slrSumCols==.x)], .cols=slrSumCols)
    data_xUpper    <- data_xUpper  %>% rename_with(~upperSumCols[which(slrSumCols==.x)], .cols=slrSumCols)
    # rm("lowerSumCols", "upperSumCols")
    ### Convert values for observations with a lower model_dot value =="0 cm" to zero
    data_xLower0   <- data_xLower0 %>% mutate_at(.vars=c(all_of(lowerSumCols)), function(y){0})
    # ### Convert values for observations with a lower model_dot value =="0 cm" to zero
    # data_xLower0   <- data_xLower0 %>% mutate_at(.vars=c(all_of(slrSumCols)), function(y){0})
    ### Filter values
    data_xLower0   <- data_xLower0 %>% filter(model_dot=="30 cm"    )
    data_xLower1   <- data_xLower1 %>% filter(model_dot==lower_model)
    data_xLower    <- data_xLower0 %>% rbind(data_xLower1) %>% select(-c("model_dot"))
    rm("data_xLower0", "data_xLower1")
    ### Join upper and lower data frames
    data_xOther    <- data_xLower %>% select(-model,-scenario_id) %>% left_join(data_xUpper)#, by = c(all_of(join_slrCols[-5:-6])))
    rm("data_xLower", "data_xUpper")

    ### Calculate the new value
    # data_xOther %>% names %>% print
    slrLowerVals  <- data_xOther[, lowerSumCols]
    slrUpperVals  <- data_xOther[, upperSumCols]
    slrOtherAdj   <- data_xOther[, "adj_slr"]  %>% as.vector
    slrNewFactors <- (slrUpperVals - slrLowerVals) * (1 - slrOtherAdj)
    slrNewValues  <- slrLowerVals + slrNewFactors
    data_xOther[,slrSumCols] <- slrNewValues
    rm("slrLowerVals", "slrUpperVals", "slrOtherAdj", "slrNewFactors", "slrNewValues")
    rm("lowerSumCols", "upperSumCols")
    ### When finished, drop columns and mutate model_dot column
    # data_xOther %>% names %>% print
    data_xOther <- data_xOther %>% mutate(model_dot="Interpolation")
    data_xOther <- data_xOther %>% select(c(all_of(names_slr)))

  } ### End if (nrow(data_xOther) > 0)

  ### Bind SLR averages together
  # c(nrow(data_xEqual), nrow(data_xOther)) %>% print
  data_x <- data_xEqual %>% rbind(data_xOther)
  # data_x %>% nrow %>% print
  rm("data_xEqual", "data_xOther")

  return(data_x)
}
