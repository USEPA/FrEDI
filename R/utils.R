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


  ###### Adjust Elasticity ######
  if(!is.null(elasticity)){
    if(is.numeric(elasticity)){
      df_x   <- df_x %>% mutate(exp0 = elasticity)
    }
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
  extend_from = NULL, ### Maximum value for model type to extend from, if not missing
  extend_to = NULL, ### Extend last points for x
  unitScale = NULL ### Scale between values
){
  ###### Defaults ######
  unitScale <- ifelse(is.null(unitScale),        1, unitScale)
  # extend_to      <- ifelse(is.null(extend_to     ),        1, unitScale)
  ###### Group data ######
  x$group_id <- x[,groupCol]
  x$xIn      <- x[,    xCol]
  x$yIn      <- x[,    yCol]
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
  df_0     <- data.frame(xIn = 0, yIn = 0)
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

      # extrapolate %>% print
      ### Whether to extend values
      extrapolate <- (xIn_max == extend_from) & (extend_from!=extend_to)
      # extrapolate %>% print
      if(extrapolate){
        ### Extend values out to the specified value
        ### - Find linear relationship between last two points
        ### - Interpolate the last few values
        df_ref_i  <- df_i[len_i + -1:0,]
        # df_ref_i %>% print
        ### Get linear trend
        lm_i      <- lm(yIn~xIn, data=df_ref_i)
        ### Extend values
        df_new_i  <- data.frame(
          xIn = seq(xIn_max + unitScale, extend_to, unitScale)
        ) %>%
          mutate(
            yIn = xIn * lm_i$coefficients[2] + lm_i$coefficients[1]
          )
        # df_new_i %>% print

        ### Bind the new observations with the other observations
        df_i <- df_i %>% rbind(df_new_i)
      }

      ###### Linear Interpolation ######
      ### Create a piece-wise linear interpolation function using approxfun and defaults
      ###    rule = 1 (Returns NA for x-values outside range)
      ###    ties = mean (take the average of multiple values)
      fun_i <- approxfun(x = df_i$xIn, y = df_i$yIn, method = "linear", rule = 1)

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
  groupCol = c("sector", "adaptation", "model_type", "impactType", "impactYear") ### Column(s) to use for grouping
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
  # default_groupCols <- c("sector", "adaptation", "model_type", "impactType", "impactYear", "region")
  default_groupCols <- c("sector", "adaptation", "model_type", "impactType", "impactYear", "region", "year", "model", "yvar")
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

  ### Sea level rise information
  assign("slr_df", rDataList[["slr_cm"]])
  df_slr_years <- slr_df$year %>% unique

  slr_df       <- slr_df %>%
    mutate(model  = model %>% as.character) %>%
    mutate(model2 = model %>% factor(levels = slr_levels, labels=slr_labels)) %>%
    mutate(order  = model2 %>% as.numeric) %>%
    arrange_at(.vars=c("order", "year")) %>%
    mutate(model = model2 %>% as.character) %>%
    select(-c(model2)) %>%
    as.data.frame

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
      message("\t", "Averaging 'yCol' values...")
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

  ### Initialize an empty dataframe
  df_return  <- data.frame()

  for(i in 1:n_years){
    ### Get year and gmsl in cm
    year_i  <- data$year[i]
    gmsl_i  <- data$yValue[i]

    ### Filter to slr observations for that year
    ### Arrange by driver value
    df_slr_i <- slr_df %>% filter(year == year_i)

    ### Figure out which values match
    which_slrs  <- gmsl_i %>% fun_getNeighbors(values=df_slr_i, col="driverValue")
    # c_slrs      <- which_slrs$model
    lower_i     <- which_slrs %>% filter(type=="lower")
    upper_i     <- which_slrs %>% filter(type=="upper")

    ### Make a dataframe with the new info
    df_i <- data.frame(
      year   = year_i,
      yValue = gmsl_i) %>%
      mutate(
        lower_model  = lower_i$model[1],
        upper_model  = upper_i$model[1],
        lower_slr = lower_i$driverValue[1],
        upper_slr = upper_i$driverValue[1]
    )

    ### Add to data frame
    df_return <- df_return %>% rbind(df_i)
  } ### End iterate over years

  ### Rename yValue
  df_return <- df_return %>% rename_at(.vars=c(all_of(newColName_y)), ~oldColName_y)

  return(df_return)

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

