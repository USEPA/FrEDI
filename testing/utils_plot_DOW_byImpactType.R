###### plot_DOW_byImpactType
### This function plots degrees of warming by sector, variant, impact year, and type
plot_DOW_byImpactType <- function(
    data,
    sector0,
    variant0,
    impYear0,
    impType0,
    infoList0, ### Dataframe with sector info...output from get_sector_plotInfo
    xCol    = "xCol", ### X-Column,
    yCol    = "yCol", ### Y-Column,
    xInfo   = NULL , ### xScale...outputs of get_colScale
    refPlot = FALSE, ### Whether to do a ref plot
    nTicks  = 5,
    silent  = TRUE,
    years0  = c(2010:2100),
    repo0   = "FrEDI",
    options = list(
      title      = "Impacts by Degrees of Warming",
      # subtitle   = NULL,
      xTitle     = expression("Degrees of Warming (°C)"),
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    ) ### End options
){
  ###### Messaging ######
  print_msg  <- !silent
  if(print_msg) "Running plot_DOW_byImpactType()..." |> message()

  ###### Format Data ######
  ###### ** Filter Data ######
  ### Filter data
  df0        <- data |> filter(sector     == sector0)
  df0        <- df0  |> filter(impactYear == impYear0)
  df0        <- df0  |> filter(variant    == variant0)
  df0        <- df0  |> filter(impactType == impType0)
  # df0 |> glimpse()

  ###### Values
  ###### ** Model Types ######
  # type0 |> print()
  type0      <- df0  |> pull(model_type) |> unique()
  typeLC0    <- type0 |> tolower()
  do_gcm     <- "gcm" %in% typeLC0
  do_slr     <- "slr" %in% typeLC0
  ### Whether to do marsh migration
  doMarsh    <- (sector0 %in% "Marsh Migration (Primary)") | (impType0 %in% "Lost Marsh Area")

  ###### Axis Scales & Breaks ######
  ###### ** Sector Info ######
  info0      <- infoList0[["sectorInfo"]] |> filter(sector==sector0)
  index0     <- info0[["sector_order"]][1]
  row0       <- info0[["plotRow"     ]][1]
  col0       <- info0[["plotCol"     ]][1]

  ###### ** X Breaks ######
  do_xInfo   <- xInfo |> is.null()
  if(do_xInfo) {
    xInfo <- xInfo |> getXAxisScale(
      xCol    = xCol,
      yrUnit  = 20,
      nTicks  = nTicks
    ) ### End getXAxisScale
  } ### End if(do_xInfo)
  ### Assign to objects
  x_limits   <- xInfo[["limits"]]
  x_breaks   <- xInfo[["breaks"]]
  x_denom    <- xInfo[["denom" ]]
  # x_breaks |> print()

  ###### ** Y-Breaks ######
  yInfo      <- infoList0[["minMax"]] |> filter(plotRow == row0)
  yInfo      <- yInfo |> mutate(sector = sector0)
  yInfo      <- yInfo |> get_colScale(col0="summary_value", nTicks=nTicks)
  # if(doMarsh) yInfo <- df0 |> get_colScale(col0="physical_impacts", nTicks=nTicks)
  ### Additional info
  y_p10      <- yInfo[["p10"   ]]
  y_denom    <- yInfo[["denom" ]]
  y_breaks   <- yInfo[["breaks"]]
  y_limits   <- yInfo[["limits"]]
  y_label    <- yInfo[["label" ]]
  # if(sector0 == "Marine Fisheries"){
  #   y_limits <- c(-0.12, .07)
  #   y_breaks <- seq(-0.1, 0.1, by=0.04)
  # }
  # y_label |> print(); y_breaks |> print()

  ### Labeling
  y_prelabel <- (y_label == "") |> ifelse("", ", ")
  # y_label    <- "" |> paste0("(", y_label, y_prelabel, ")")
  y_label    <- y_label |> paste0(y_prelabel, "$2015")
  y_label    <- "Impacts (" |> paste0(y_label, ")")
  # y_p10 |> print(); y_denom |> print(); y_breaks |> print()

  ###### ** Mutate Data ######
  # "got here" |> print(); x_denom |> print(); y_denom |> print()
  df0[[xCol]] <- df0[[xCol]] / x_denom
  df0[[yCol]] <- df0[[yCol]] / y_denom

  ###### Plot Setup ######
  ###### ** Plot Options ######
  # plotOpts0   <- typeLC0 |> get_scaledImpactPlotTitles(options=options)
  plotOpts0   <- typeLC0 |> get_scaledImpactPlotTitles(options=options, repo0=repo0)
  title0      <- plotOpts0[["title"     ]]
  xTitle      <- plotOpts0[["xTitle"    ]]
  yTitle      <- plotOpts0[["yTitle"    ]]
  lgdLbl      <- plotOpts0[["lgdTitle"  ]]
  lgdPos      <- plotOpts0[["lgdPos"    ]]
  heights     <- plotOpts0[["heights"   ]]
  margins     <- plotOpts0[["margins"   ]]
  mUnit       <- plotOpts0[["marginUnit"]]
  nameBrk     <- plotOpts0[["nameBrk"   ]]
  theme0      <- plotOpts0[["theme"     ]]
  # xTitle |> print()
  ### Conditionals
  hasLgdPos   <- !(lgdPos  |> is.null())
  hasMargins  <- !(margins |> is.null())
  hasTheme    <- !(theme0  |> is.null())
  hasNameBrk  <- !(nameBrk |> is.null())
  # xTitle |> print()


  ###### ** Plot Aesthetics ######
  ###### ** Titles
  # title0    <- impType0
  # subtitle0 <- variant0
  title0    <- "Impact Type: " |> paste0(impType0)
  subtitle0 <- "Variant: "     |> paste0(variant0)
  ###### ** Aesthetics
  ### Text size
  titleSize <- 11
  subSize   <- 10
  axisSize  <- 9
  ### Text orientation
  hjust0    <- 0.5
  ### Geom alpha
  alpha0    <- 0.80
  ### Legend position,  direction, title position, and nrows
  lgdPos0   <- "bottom"
  lgdDir0   <- "vertical"
  lgdTit0   <- "left"
  lgdNrow   <- 3
  ### Theme: whether to do black and white
  doBW      <- theme0 == "bw"

  ###### ** Legend Scales ######
  shapeLvls <- df0 |> pull(model) |> unique() |> sort()
  numShapes <- shapeLvls |> length()
  shapeVals <- 1:numShapes
  colorVals <- fun_manual_colors()

  ###### Special Cases ######
  ### Plot Marsh Migration (Primary), Impact Type: "Lost Marsh Area" separately
  # doMarsh   <- (sector0 %in% "Marsh Migration (Primary)") | (impType0 %in% "Lost Marsh Area")
  # if(doMarsh) {
  #   xTitle <- xTitle |> str_replace("Impacts", "Physical Impacts")
  #   xTitle <- xTitle |> str_replace(", \\$2015", "")
  #   xTitle |> print()
  # } ### End if(doMarsh)


  ###### Add in Model info ######
  if(do_gcm)  {
    ### Get model info
    select0   <- c("model_id", "maxUnitValue")
    rename0   <- c("model_id")
    rename1   <- c("model")
    co_models <- "co_models" |> get_frediDataObj("frediData")
    co_models <- co_models |> filter((modelType |> tolower()) == "gcm")
    co_models <- co_models |> select(all_of(select0))
    co_models <- co_models |> rename_at(c(rename0), ~rename1)
    rm(select0, rename0, rename1)
    ### Join model info with df0
    join0     <- c("model")
    df0       <- df0 |> left_join(co_models, by=c(join0))
    rm(join0)
  } ### if(do_gcm)

  ###### Create Plot ######
  ###### ** Split Data ######
  ### Add Geoms
  if(do_gcm) {
    ### Plot these values as lines
    df0_1  <- df0 |> filter((driverValue <= maxUnitValue))
    ### Plot these values as dashed lines
    df0_2  <- df0 |> filter((driverValue >= maxUnitValue))
  } else{
    ### Plot these values as lines
    df0_1  <- df0
    ### Plot these values as points
    df0_2  <- df0 |> filter(year %in% x_breaks)
  } ### End if(do_gcm)

  ###### ** Initialize Plot #####
  ### Initialize plot
  plot0  <- df0_1 |> ggplot(aes(
    x     = .data[[xCol]],
    y     = .data[[yCol]],
    color = .data[["model"]]
  )) ### End aes/ggplot

  ###### ** Plot First Set ######
  ### Add geomline
  plot0  <- plot0 + geom_line (alpha=alpha0)

  ###### Plot Second Set ######
  ### If GCM: plot second set of values as dashed lines
  ### If SLR: plot second set of values as points
  if(do_gcm) {
    plot0  <- plot0 + geom_line (
      data = df0_2, aes(
        x     = .data[[xCol]],
        y     = .data[[yCol]],
        color = .data[["model"]]
      ), ### End aes
      linetype = "dashed",
      alpha    = alpha0
    ) ### End geom_line
  } else{
    plot0  <- plot0 + geom_point(
      data = df0_2, aes(
        x     = .data[[xCol]],
        y     = .data[[yCol]],
        color = .data[["model"]]
      ), ### End aes
      alpha = alpha0
    ) ### End geom_point
  } ### End if(do_gcm)
  ### Remove data
  rm(df0_1, df0_2)


  ###### ** Plot Title ######
  ### Add title
  plot0     <- plot0 + ggtitle(title0, subtitle0)
  ### Add title aesthetics
  plot0     <- plot0 + theme(plot.title    = element_text(hjust=hjust0, size=titleSize))
  plot0     <- plot0 + theme(plot.subtitle = element_text(hjust=hjust0, size=subSize))

  ###### ** Plot Axes ######
  ### Add axis scales
  plot0     <- plot0 + scale_x_continuous(xTitle, breaks = x_breaks, limits = x_limits)
  # plot0     <- plot0 + scale_y_continuous(yTitle)
  plot0     <- plot0 + scale_y_continuous(y_label)
  ### Add axis aesthetics
  plot0     <- plot0 + theme(axis.title.x  = element_text(hjust=hjust0, size=axisSize))
  plot0     <- plot0 + theme(axis.title.y  = element_text(hjust=hjust0, size=axisSize))

  ###### ** Plot Legend ######
  ### Add legend scales
  plot0     <- plot0 + scale_color_manual(lgdLbl, values=colorVals)
  ### Add legend aesthetics
  plot0     <- plot0 + theme(legend.box      = lgdDir0)
  plot0     <- plot0 + theme(legend.position = lgdPos0)
  plot0     <- plot0 + guides(color = guide_legend(title.position=lgdTit0, ncol=lgdNrow, byrow=TRUE))
  plot0     <- plot0 + guides(shape = guide_legend(title.position=lgdTit0, ncol=lgdNrow, byrow=TRUE))

  ###### ** Plot Theme ######
  ### Theme
  if(hasTheme) {
    if(doBW) plot0 <- plot0 + theme_bw()
    else     plot0 <- plot0 + theme0
  } ### End if(hasTheme)

  ###### ** Plot Margins ######
  ### Margins
  if(hasMargins) {
    plot0 <- plot0 + theme(plot.margin = margin(
      t = margins[1],  ### Top margin
      r = margins[2],  ### Right margin
      b = margins[3],  ### Bottom margin
      l = margins[4],  ### Left margin
      unit = mUnit
    )) ### End theme
  } ### End if(hasMargins)


  ###### Return ######
  ### Return the plot
  if(print_msg) message("...Finished.")
  return(plot0)
}

###### End Script ######
