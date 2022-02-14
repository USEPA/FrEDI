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
    
    # c(xMin, xMax) %>% print
    ### Set minimum to zero unless the minimum is less than zero
    # ### Does the series have negative values? If not, zero out.
    # lowValue <- df_minMax$limit_rounded[1]
    # zero     <- ifelse(lowValue < 0, F, zero)
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
    
    # df_minMax$value %>% print
    # df_minMax$bound_abs %>% print
    # x_power1000 %>% print
    ### Rounded bounded values (round to 1 essentially)
    divideByPower         <- x_power10Max - 1
    minMax_scaled         <- df_minMax$value / 10^divideByPower
    bounds_scaled_rounded <- c(floor(minMax_scaled[1]), ceiling(minMax_scaled[2]))
    bounds_rounded        <- bounds_scaled_rounded * 10^divideByPower
    
    # minMax_scaled %>% print
    # bounds_rounded %>% print
    ###### Establish the range of x ######
    x_range      <- bounds_rounded
    x_range_p10  <- x_range / 10^x_power10Max
    x_range_dif  <- x_range_p10[2] - x_range_p10[1]
    
    # x_range %>% print
    ### Determine unit of breaks in power of 10
    # x_range_p10 %>% print
    x_unit_p10     <- 0.5
    # x_range_p10 %>% print
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
    
    # ### Add a zero value
    # if(xMin < 0 & xMax > 0){
    #   whichBelowZero <- (x_breaks < 0) %>% which
    #   whichAboveZero <- (x_breaks > 0) %>% which
    #   
    #   ### Add zero
    #   x_breaks <- c(x_breaks[whichBelowZero], 0, x_breaks[whichAboveZero])
    # }
    
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