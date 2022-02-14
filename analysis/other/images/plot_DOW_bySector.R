###### plot_DOW_bySector ######
### This function plots degrees of warming for a particular sector
plot_DOW_bySector <- function(
  data,
  sector,
  xCol = "driverValue",
  yCol = "annual_impacts",
  zero = F,
  plotIndex = 1,
  plotCol   = 1,
  xBreaks = NULL,
  yBreaks = NULL,
  xLimits = NULL,
  yLimits = NULL,
  title = "Impacts by Degrees of Warming",
  xtitle = expression("Degrees of Warming ("*degree*C*")"),
  ytitle  = "Impacts (Billions, $2015)",
  legend_title = "Model",
  nTicks = 5,
  silent = F,
  theme = "bw" ### Default theme vs black and white
){
  ### Defaults
  if(is.null(xCol)) xCol <- "driverValue"
  if(is.null(yCol)) yCol <- "annual_impacts"
  if(is.null(plotIndex)) plotIndex <- 1
  if(is.null(plotCol)) plotCol <- 1
  if(is.null(nTicks)){nTicks <- 5}
  if(is.null(title)){title <- "Impacts by Degrees of Warming"}
  ### Whether to message
  if(is.null(silent)){silent <- F}
  print_msg <- !silent
  if(print_msg) message("Running plot_DOW_bySector():")

  ### initialize x_breaks and y_breaks
  x_breaks <- xBreaks
  y_breaks <- yBreaks
  x_lims   <- xLimits
  y_lims   <- yLimits

  ### Filter to sector
  df_sector   <- data %>% filter(sector==sector) %>% as.data.frame

  ### Figure out the plot info
  ggtitle_sector  <- title
  subtitle_sector <- sector

  if(is.null(xtitle      )){xtitle       <- expression("Degrees of Warming ("*degree*C*")")}
  if(is.null(ytitle      )){ytitle       <- "Impacts (Billions, $2015)"}
  if(is.null(legend_title)){legend_title <- "Model"}

  ### Add plot and legend titles to the first plot only
  if(plotIndex!=1){
    legend_title <- xtitle <- ytitle <- ""
  }

  ###### Standardize column names ######
  ### Standardize columns and scale
  # df_sector %>% names %>% print
  # c(xCol, yCol) %>% print
  df_sector[,"xCol"] <- df_sector[, xCol]
  df_sector[,"yCol"] <- df_sector[, yCol]
  # df_sector <- df_sector %>% select(-c(all_of(xCol), all_of(yCol)))

  # "got here" %>% print
  ##### Scales for X-axis ######
  if(is.null(x_breaks)){
    if(df_sector[, yCol] %>% is.na %>% all){
      if(print_msg) message("\t", "No non-missing values for ", xCol, " for ", sector_i, "...")
    } else{
      ###### Get X Breaks ######
      # x_scaleInfo <- df_sector %>% fun_getScale(scaleCol = "xCol", zero = zero, nTicks = nTicks)
      # "got here" %>% print
      x_scaleInfo <- df_sector %>% fun_getScale(scaleCol = "xCol", nTicks = nTicks)
      # "got here" %>% print
      x_breaks    <- x_scaleInfo$breaks
      # "got here" %>% print
      x_lims      <- x_scaleInfo$limits
      # "got here" %>% print
      x_p10       <- x_scaleInfo$power1000 * 3
    }
  }
  else{
    ### Order the breaks
    x_breaks <- x_breaks[x_breaks %>% order]
    ### Create x limits from breaks if is.null(x_lims) and message user if values are outside bounds
    if(is.null(x_lims)){
      x_lims <- c(x_breaks[1], last(x_breaks))
      x_min  <- df_sector$xCol %>% min(na.rm=T)
      x_max  <- df_sector$xCol %>% max(na.rm=T)
      if(x_min < x_lims[1]){
        if(print_msg) message("\t", "Warning: minimum value of ", xCol, " is below the minimum in x_breaks...")
      }
      if(x_max > x_lims[2]){
        if(print_msg) message("\t", "Warning: maximum value of ", xCol, " is below the minimum in x_breaks...")
      }
    }
    ### Get power of 10
    # "got here" %>% print
    x_p10_vals <- x_lims %>% abs
    x_p10_vals[which(x_p10_vals==0)] <- 1
    x_p10_vals <- x_p10_vals  %>% log10 %>% max(na.rm=T) %>% floor
    x_p10      <- (x_p10_vals %/% 3) * 3
  }
  # "got here" %>% print
  denom_x    <- 10^x_p10

  ###### Scales for Y-axis ######
  if(is.null(y_breaks)){
    if(df_sector[, yCol] %>% is.na %>% all){
      if(print_msg) message("\t", "No non-mising values for ", yCol, " for ", sector_i, "...")
    } else{
      # "got here" %>% print
      ###### Get Y Breaks ######
      # y_scaleInfo <- df_sector %>% fun_getScale(scaleCol = "yCol", zero = zero, nTicks = nTicks)
      y_scaleInfo <- df_sector %>% fun_getScale(scaleCol = "yCol", nTicks = nTicks)
      y_breaks    <- y_scaleInfo$breaks
      y_lims      <- y_scaleInfo$limits
      y_p10       <- y_scaleInfo$power1000 * 3

    }
  }
  else{
    ### Order the breaks
    y_breaks <- y_breaks[y_breaks %>% order]
    ### Create y limits from breaks if is.null(y_lims) and message user if values are outside bounds
    if(is.null(y_lims)){
      y_lims <- c(y_breaks[1], last(y_breaks))
      y_min  <- df_sector$yCol %>% min(na.rm=T)
      y_max  <- df_sector$yCol %>% max(na.rm=T)
      if(y_min < y_lims[1]){
        if(print_msg) message("\t", "Warning: minimum value of ", yCol, " is below the minimum in x_breaks...")
      }
      if(y_max > y_lims[2]){
        if(print_msg) message("\t", "Warning: maximum value of ", yCol, " is below the minimum in x_breaks...")
      }
    }
    ### Get power of 10
    y_p10_vals <- y_lims %>% abs
    y_p10_vals[which(y_p10_vals==0)] <- 1
    y_p10_vals <- y_p10_vals %>% log10 %>% max(na.rm=T) %>% floor
    y_p10      <- (y_p10_vals %/% 3) * 3
  }
  denom_y     <- 10^y_p10
  # "got here" %>% print

  ###### Create the plot ######
  # df_sector %>% names %>% print
  # c(denom_x, denom_y) %>% print
  # c(denom_x, denom_y) %>% is.numeric %>% print
  # df_sector[,c("xCol", "yCol")] %>% lapply(is.numeric) %>% unlist %>% print
  p_sector  <- df_sector %>%
    mutate(xCol = xCol / denom_x, yCol = yCol / denom_y) %>%
    ggplot(aes(x = xCol, y=yCol)) +

    ### Add Geoms
    geom_line(aes(color = model)) +
    geom_point(aes(color = model, shape=model)) +

    ### Add Scales
    scale_color_discrete(legend_title) +
    scale_shape_discrete(legend_title)

  ###### Add themes and title ######
  # p_sector    <- p_sector +
  #   ggtitle(ggtitle_sector, subtitle_sector) +
  #   theme(panel.background = element_rect(fill="white")) +
  #   theme(panel.grid = element_line(color="lightgrey")) +
  #   theme(axis.line = element_line(color="darkgrey")) +
  #   theme(plot.title = element_text(hjust = 0.5, size=12)) +
  #   theme(plot.subtitle = element_text(hjust = 0.5, size=9))
  p_sector    <- p_sector +
    ggtitle(ggtitle_sector, subtitle_sector) +

    theme(plot.title = element_text(hjust = 0.5, size=12)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size=9))

  ###### Change theme ######
  themeSpec <- ifelse(is.null(theme), "default", theme)
  if(themeSpec=="bw"){
    p_sector    <- p_sector +
      theme(panel.background = element_rect(fill="white")) +
      theme(panel.grid = element_line(color="lightgrey")) +
      theme(axis.line = element_line(color="darkgrey"))
  }


  ###### Add scales ######
  p_sector    <- p_sector + scale_x_continuous(xtitle, limits = x_lims / denom_x, breaks = x_breaks / denom_x)
  p_sector    <- p_sector + scale_y_continuous(ytitle, limits = y_lims / denom_y, breaks = y_breaks / denom_y)

  ###### If plotIndex, remove some plot elements #####
  if(plotIndex>1){
    p_return <- p_sector +
      theme(
        plot.title = element_blank(),
        legend.position = "none",
        axis.title = element_blank()
      )
  } else{
    p_return <- p_sector +
      theme(
        legend.position = "bottom"
      )
  }

  ### Only plot tick marks for y-labels if plotCol == 1
  ### Make y-labels white if not in the first column
  if(plotCol>1){
    p_return <- p_return + theme(axis.text.y = element_text(color="white"))
  }

  ## Return the plot
  if(print_msg) message("Finished.")
  return(p_return)

}
