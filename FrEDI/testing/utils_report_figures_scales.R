###### get_p10Labels ######
### Get power of 10 labels
get_p10Labels <- function(
    values,
    type0 = c("p10") ### Power 10, power 1000, "p1000"
){
  ### Data tible
  vals0 <- values; rm("values")
  # dfx   <- tibble(value=vals0, type=type0)
  dfx   <- tibble(value=vals0)
  dfx   <- dfx |> rename_at(.vars=c("value"), ~c(type0))
  ### Tibble to match to
  c_p10 <- seq(0, 21)
  df0   <- tibble(p10=c_p10)
  df0   <- df0 |> mutate(p1000 = p10 %/% 3)
  df0   <- df0 |> mutate(p1000 = p10 %/% 3)
  df0   <- df0 |> mutate(label = case_when(
    p1000 >= 0 & p1000 < 1 ~ "",
    p1000 >= 1 & p1000 < 2 ~ "Thousands",
    p1000 >= 2 & p1000 < 3 ~ "Millions",
    p1000 >= 3 & p1000 < 4 ~ "Billions",
    p1000 >= 4 & p1000 < 5 ~ "Trillions",
    p1000 >= 5 & p1000 < 6 ~ "Quadillions",
    p1000 >= 6 & p1000 < 7 ~ "Quintillions",
    .default = ""
  ))
  ### Join data
  dfx   <- dfx |> left_join(df0, by=c(type0))
  ### Return
  return(dfx)
}

###### fun_getScale ######
### This function creates a set of breaks for a particular column
### It returns a list of breaks, the power of 10, and the limits
fun_getScale <- function(
    data,
    scaleCol = "driverValue",
    nTicks   = 5
){
  ###### Defaults ######
  # if(is.null(scaleCol)) scaleCol <- "driverValue"
  ### Default is not to zero out in case there are negative numbers
  # if(is.null(zero))     zero <- F
  # if(is.null(nTicks)){nTicks <- 5}

  ###### Min/Max Values ######
  data <- data
  xMin <- data[[scaleCol]] |> min(na.rm=T)
  xMax <- data[[scaleCol]] |> max(na.rm=T)
  # c(xMin, xMax)|> print()

  ### Set minimum to zero unless the minimum is less than zero
  ### Does the series have negative values? If not, zero out.
  # zero     <- ifelse(lowValue < 0, F, zero)
  if(xMin > 0){xMin <- 0}
  if(xMax < 0){xMax <- 0}

  ###### Bounds
  bound_min <- xMin |> floor()
  bound_max <- xMax |> ceiling()

  ###### Min/Max Tibble ######
  ### Limit names, values, bounds, etc
  df_minMax <- tibble(
    name  = c("min", "max"),
    value = c(xMin, xMax),
    bound = c(bound_min, bound_max),
    boundType = c("floor", "ceiling")
  )

  ###### Log10 Values ######
  ### Absolute value, Power of 10 and y-scale info
  df_minMax <- df_minMax |> mutate(bound_abs = bound |> abs())
  ### Calculate log 10 and replace values of infinity with 0
  df_minMax <- df_minMax |> mutate(log10 = bound_abs |> log10())
  df_minMax <- df_minMax |> mutate(log10 = log10 |> abs())
  df_minMax <- df_minMax |> mutate(log10 = log10 |> na_if(Inf))
  df_minMax <- df_minMax |> mutate(log10 = log10 |> replace_na(0))

  ###### Power-10 Values ######
  ### Then get the floor of the log10 value
  df_minMax <- df_minMax |> mutate(p10   = log10 |> floor())
  df_minMax <- df_minMax |> mutate(p1000 = log10 |> floor())
  ### Get maximum power of 10, then scale to zero for negative numbers
  ### Integer division of power of 10 by 3 to get number of thousands
  ### Then get the modulus of the thousands
  x_p10Max  <- df_minMax[["p10"]] |> max(na.rm=T)
  x_p1000   <- x_p10Max  %/% 3
  x_mod1000 <- x_p10Max  %% 3

  # df_minMax |> print(); x_p1000 |> print()

  # df_minMax$value|> print()
  # df_minMax$bound_abs|> print()
  # x_p1000|> print()
  ### Rounded bounded values (round to 1 essentially)
  divideByPower         <- x_p10Max - 1
  minMax_scaled         <- df_minMax[["value"]] / 10**divideByPower
  bound_scaled_min      <- minMax_scaled[1] |> floor()
  bound_scaled_max      <- minMax_scaled[2] |> ceiling()
  bounds_scaled_rounded <- c(bound_scaled_min, bound_scaled_max)
  bounds_rounded        <- bounds_scaled_rounded * 10**divideByPower
  # minMax_scaled|> print()
  # bounds_rounded|> print()

  ###### Range ######
  x_range      <- bounds_rounded
  x_range_p10  <- x_range / 10**x_p10Max
  # x_range_dif  <- x_range_p10[2] - x_range_p10[1]
  x_p10_min    <- x_range_p10[1] |> floor()
  x_p10_max    <- x_range_p10[1] |> ceiling()
  x_p10_dif    <- x_p10_max - x_p10_min
  # x_range_p10 |> print(); x_p10_dif |> print()

  ### Determine unit of breaks in power of 10
  # x_range_p10|> print()
  x_unit_p10     <- 0.5
  # x_unit_p10     <- x_p10_dif
  # x_range_p10|> print()
  x_breaks_p10   <- seq(x_range_p10[1], x_range_p10[2], by = x_unit_p10)
  # x_breaks_p10   <- seq(x_p10_min, x_p10_max, by = x_unit_p10)
  n_Ticks        <- x_breaks_p10 |> length()

  ### Check if number of ticks greater than threshold number
  cond1          <- n_Ticks > nTicks
  if(cond1){
    x_unit_p10   <- 1
    x_breaks_p10 <- seq(x_range_p10[1], x_range_p10[2], by = x_unit_p10)
    n_Ticks      <- x_breaks_p10 |> length()
  } ### End if(n_Ticks>nTicks)
  ### Check again
  cond1          <- n_Ticks > nTicks
  if(cond1){
    x_unit_p10   <- 2
    x_breaks_p10 <- seq(x_range_p10[1], x_range_p10[2], by = x_unit_p10)
    n_Ticks      <- x_breaks_p10 |> length()
  }
  ### Number of breaks
  x_breaks       <- x_breaks_p10 * 10**x_p10Max
  # return(x_breaks)

  # ### Add a zero value
  # if(xMin < 0 & xMax > 0){
  #   whichBelowZero <- (x_breaks < 0)|> which()
  #   whichAboveZero <- (x_breaks > 0)|> which()
  #
  #   ### Add zero
  #   x_breaks <- c(x_breaks[whichBelowZero], 0, x_breaks[whichAboveZero])
  # }

  ###### Return List ######
  ### Create list to return
  return_list <- list(
    breaks    = x_breaks,
    limits    = df_minMax[["value"]],
    bounds    = bounds_rounded,
    p10       = x_p10Max,
    p1000     = x_p1000,
    mod1000   = x_mod1000
    # mod1000   = x_mod1000,
    # label     = x_lab
  )

  ###### Return ######
  return(return_list)
} ### End fun_getScale()


###### get_colScale ######
### Get X Scale Values
get_colScale <- function(
    df0, ### Data frame
    col0   = "xCol", ### Column to use
    nTicks = 5 ### Number of ticks
){
  ###### Initialize list
  list0    <- list()
  ###### X Scales
  x_info   <- df0 |> fun_getScale(scaleCol = col0, nTicks=nTicks)
  ###### Modify values
  x_p1000  <- x_info[["p1000"]]
  x_p10    <- x_p1000 * 3
  x_denom  <- 10**x_p10
  # x_info[["breaks"]] |> print()
  x_breaks <- x_info[["breaks"]] / x_denom
  x_limits <- x_info[["limits"]] / x_denom

  ###### Add Labels ######
  # x_lab    <- x_p1000
  x_lab    <- x_p1000 |> get_p10Labels(type="p1000")
  x_lab    <- x_lab[["label"]][1]
  # x_p10 |> print(); x_p1000 |> print(); x_lab |> print()

  ###### Update list
  list0[["scale" ]] <- x_info
  list0[["p10"   ]] <- x_p10
  list0[["p1000" ]] <- x_p1000
  list0[["denom" ]] <- x_denom
  list0[["breaks"]] <- x_breaks
  list0[["limits"]] <- x_limits
  list0[["label" ]] <- x_lab
  ###### Return
  return(list0)
} ### End get_colScale()


###### fun_limitsByGroup ######
### This function summarizes data for a particular group
### It returns a dataframe of results with the groups, a column called "summary_type", and the summarized value "summary_value"
fun_limitsByGroup <- function(
    data,
    groupCols = c("sector"),
    sumCols   = c("annual_impacts"),
    type      = c("min", "max"),
    silent    = FALSE,
    msg       = ""
){
  ###### Defaults ######
  ### Whether to message
  print_msg  <- !silent
  msg0       <- msg
  msg1       <- "\t" |> paste0(msg0)
  if(print_msg){ msg0 |> paste0("Running fun_limitsByGroup()...") |> message()}
  ###### Data ######
  names0     <- data |> names()
  sumCols0   <- sumCols
  groupCols0 <- groupCols
  ###### Check for Summary Columns ######
  ### Check if the summary columns are present
  hasSumCols   <- (sumCols %in% names0)
  sumCols      <- sumCols[ hasSumCols]
  naSumCols    <- sumCols[!hasSumCols]
  nSumCols     <- hasSumCols |> length()
  ### Conditions
  anySumCols   <- hasSumCols |> any()
  multSumCols  <- nSumCols > 1
  hasNaSumCols <- length(naSumCols) > 0
  ### Messaging
  msgSumCols   <-
    ### If no summary columns
    if     (!anySumCols ){
      if(print_msg){msg1 |> paste0("Summary columns ", paste(sumCols0, collapse=", "), " not present in data...") |> message()}
      if(print_msg){msg1 |> paste0("Exiting...") |> message()}
      return()
    } ### End if(!hasSumCols)
  else if(hasNaSumCols){
    if(print_msg){msg1 |> paste0("Summary columns ", paste(naSumCols, collapse=", "), " not present in data...") |> message()}
  }
  ### IF multiple columns
  # if(multSumCols){
  #   if(print_msg){ "\t" |> paste0("More than one summary column provided. Summarizing the first column only...") |> message()}
  #   sumCols <- sumCols[1]
  # }

  ###### Check for Group Columns ######
  ### Check if the group columns is present
  hasGroupCols   <- (groupCols %in% names0)
  groupCols      <- groupCols[ hasGroupCols]
  naGroupCols    <- groupCols[!hasGroupCols]
  nGroupCols     <- hasGroupCols |> length()
  ### Conditions
  anyGroupCols   <- hasGroupCols |> any()
  multGroupCols  <- nGroupCols > 1
  hasNaGroupCols <- length(naGroupCols) > 0
  ### If no grouping columns
  if     (!anyGroupCols ){
    if(print_msg){msg1 |> paste0("Group columns ", paste(groupCols0, collapse=", "), " not present in data...") |> message()}
    if(print_msg){msg1 |> paste0("Exiting...") |> message()}
    return()
  } ### End if(!hasGroupCols)
  else if(hasNaGroupCols){
    if(print_msg){msg1 |> paste0("Group columns ", paste(naGroupCols, collapse=", "), " not present in data...") |> message()}
  }

  ###### Summarize Values ######
  ### Message the user
  if(print_msg){msg1 |> paste0("Summarizing values for columns ", paste(sumCols, collapse=", "), "...") |> message()}
  if(print_msg){msg1 |> paste0("Grouping by columns ", paste(groupCols, collapse=", "), "...") |> message()}
  ### Summarize values by sector
  lim_bySector <- data |>
    group_by_at(c(groupCols)) |>
    summarise_at(.vars = c(sumCols), .funs = c(type), na.rm=T) |>
    gather(key = "summary_type", value = "summary_value", -c(all_of(groupCols))) |>
    ungroup()
  # df_limBySector |> print()

  ###### Spread ######
  ### Spread & calculate the spread
  lim_bySector <- lim_bySector |> spread(key="summary_type", value="summary_value")
  lim_bySector <- lim_bySector |> mutate(spread=max - min)

  ###### Arrange ######
  ### Arrange the sectors & get their order
  # arrange0     <- groupCols    |> c("max", "spread")
  arrange0     <- c("max", "spread")
  lim_bySector <- lim_bySector |> arrange_at(.vars = c(arrange0), desc)
  # lim_bySector |> print()

  ###### Get Group Orders ######
  for(group_i in groupCols){
    ### Get unique values in arranged order
    val_i <- lim_bySector[[group_i]] |> unique()
    ### Factor values & get order
    ord_i <- "order" |> paste0("_", group_i)
    fac_i <- group_i |> paste0("_", "factor")
    lim_bySector[[fac_i]] <- lim_bySector[[group_i]] |> factor(levels=val_i)
    lim_bySector[[ord_i]] <- lim_bySector[[fac_i  ]] |> as.numeric()
  } ### End for(group_i in groupCols)

  ###### Arrange Again ######
  ### Arrange the sectors & get their order
  # arrange0     <- groupCols    |> c("max", "spread")
  arrange0     <- "order" |> paste0("_", groupCols)
  lim_bySector <- lim_bySector |> arrange_at(.vars = c(arrange0))
  lim_bySector <- lim_bySector |> mutate(order = row_number())
  # lim_bySector |> print()

  ### Return value
  if(print_msg){ msg1 |> paste0("...Finished.") |> message()}
  return(lim_bySector)
}


###### get_sector_plotInfo ######
### Get sector plot info
get_sector_plotInfo <- function(
    df0, ### Data
    yCol      = "yCol",
    byType    = FALSE,
    groupCols = c("sector"),
    nCol      = 4,
    silent    = TRUE
){
  ###### Initialize Return List ######
  list0  <- list()

  ###### Grouping Columns ######
  if(byType){
    group0    <- c("impactYear", "variant", "impactType")
    groupCols <- groupCols[!(groupCols %in% group0)]
    groupCols <- groupCols |> c(group0)
  } ### End if(byType)

  ###### Get Value Ranges ######
  # df0 |> glimpse()
  df_sectorInfo <- df0 |> fun_limitsByGroup(
    sumCols   = yCol,
    groupCols = groupCols,
    silent    = silent
  )
  # df_sectorInfo |> print()

  ###### Get Value Lists & Lengths ######
  ### Get number of sectors and calculate columns
  doSector   <- "sector"     %in% groupCols
  doYears    <- "impactYear" %in% groupCols
  doVariants <- "variant"    %in% groupCols
  doTypes    <- "impactType" %in% groupCols
  ### Sectors
  if(doSector){
    c_sectors <- df_sectorInfo[["sector_factor"]] |> levels() |> as.character()
    n_sectors <- c_sectors |> length()
    list0[["cSectors" ]] <- c_sectors |> as.character()
    list0[["nSectors" ]] <- n_sectors
  }
  ### Impact Years
  if(doYears){
    c_impYears <- df_sectorInfo[["impactYear_factor"]] |> levels() |> as.character()
    n_impYears <- c_impYears |> length()
    list0[["cImpYears"]] <- c_impYears |> as.character()
    list0[["nImpYears"]] <- n_impYears
  }
  ### Variants
  if(doVariants){
    c_variants <- df_sectorInfo[["variant_factor"]] |> levels() |> as.character()
    n_variants <- c_variants |> length()
    list0[["cVariants"]] <- c_variants |> as.character()
    list0[["nVariants"]] <- n_variants
  }
  ### Impact Types
  if(doTypes){
    c_impTypes <- df_sectorInfo[["impactType_factor"]] |> levels() |> as.character()
    n_impTypes <- c_impTypes |> length()
    list0[["cImpTypes"]] <- c_impTypes |> as.character()
    list0[["nImpTypes"]] <- n_impTypes
  }

  ###### Number of Rows & Columns ######
  ### Initialize rows & columns
  nCol      <- nCol
  nRow      <- n_sectors %/% nCol
  nRow      <- (nRow == 0) |> ifelse(1, nRow)
  ### Get Number of Rows & Columns
  if(byType){ nCol <- n_variants}
  if(byType){ nRow <- n_impTypes}
  ### Add to list
  list0[["nCol"]] <- nCol
  list0[["nRow"]] <- nRow

  ###### Min/Max Info ######
  ### Also figure out sector positions in the list of plots
  if(byType){
    df_sectorInfo <- df_sectorInfo |> mutate(plotCol = order_variant   )
    df_sectorInfo <- df_sectorInfo |> mutate(plotRow = order_impactType)
  } ### End if(byType)
  else{
    df_sectorInfo <- df_sectorInfo |> mutate(plotRow = ((order - 1) %/% nCol) + 1)
    df_sectorInfo <- df_sectorInfo |> mutate(plotCol = ((order - 1) %%  nCol) + 1)
  } ### End else

  # df_sectorInfo |> print()

  ### Get maximum and minimum values by plot row and combine
  df_minMax   <- df_sectorInfo |>
    group_by_at(.vars=c("plotRow")) |>
    summarize(min = min(min), max=max(max)) |>
    ungroup()
  ### Gather values
  df_minMax   <- df_minMax |> gather(key="summary_type", value = "summary_value", -c("plotRow"))
  # df_minMax[["summary_value"]] |> print()

  ###### Return List ######
  ### Return list
  list0 <- list(minMax     = df_minMax    ) |> c(list0)
  list0 <- list(sectorInfo = df_sectorInfo) |> c(list0)
  ### Return
  return(list0)
}

