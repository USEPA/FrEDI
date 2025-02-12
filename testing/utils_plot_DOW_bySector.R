###### plot_DOW_bySector
### Get plot_DOW_byModelType_sector
plot_DOW_bySector <- function(
    sector0,           ### Plot index number
    df0,               ### Sector data
    infoList0,         ### Dataframe with sector info...output from get_sector_plotInfo
    xCol     = "xCol", ### X-Column,
    yCol     = "yCol", ### Y-Column,
    xInfo    = NULL  , ### xScale...outputs of get_colScale
    refPlot  = F     , ### Whether to do a ref plot
    nTicks   = 5     ,
    repo0    = "FrEDI",
    options  = list(
      title      = "Impacts by Degrees of Warming",
      # subtitle   = NULL,
      xTitle     = expression("Degrees of Warming (°C)"),
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      nameBreak  = 18  , ### Sector name break
      theme      = NULL
    ) ### End options
){
  ###### Format Data ######
  df0        <- df0  |> filter(sector==sector0)
  type0      <- df0  |> pull(model_type) |> unique()
  typeLC0    <- type0 |> tolower()
  do_gcm     <- "gcm" %in% typeLC0
  do_slr     <- "slr" %in% typeLC0

  ###### Axis Scales & Breaks ######
  ###### ** Sector Info ######
  info0      <- infoList0[["sectorInfo"]] |> filter(sector==sector0)
  index0     <- info0[["sector_order"]][1]
  row0       <- info0[["plotRow"     ]][1]
  col0       <- info0[["plotCol"     ]][1]

  ###### ** X Breaks ######
  do_xInfo   <- xInfo |> is.null()
  if(do_xInfo) xInfo <- df0 |> get_colScale(col0=xCol, nTicks=nTicks)
  x_denom    <- xInfo[["denom" ]]
  x_breaks   <- xInfo[["breaks"]]
  x_limits   <- xInfo[["limits"]]
  # x_breaks |> print()
  # x_breaks |> print()
  ###### ** Y Breaks ######
  yInfo      <- infoList0[["minMax"]] |> filter(plotRow == row0)
  yInfo      <- yInfo |> mutate(sector=sector0)
  yInfo      <- yInfo |> get_colScale(col0="summary_value", nTicks=nTicks)
  # yInfo      <- yInfo |> get_colScale(col0="summary_value", nTicks=nTicks)
  ### Additional info
  y_p10      <- yInfo[["p10"   ]]
  y_denom    <- yInfo[["denom" ]]
  y_breaks   <- yInfo[["breaks"]]
  y_limits   <- yInfo[["limits"]]
  y_label    <- yInfo[["label" ]]
  # # y_label |> print(); y_breaks |> print()
  # # ### Labeling
  # y_prelabel <- (y_label == "") |> ifelse("", ", ")
  # # y_label    <- "" |> paste0("(", y_label, y_prelabel, ")")
  # y_label    <- y_label |> paste0(y_prelabel, "$2015")
  # # y_p10 |> print(); y_denom |> print(); y_breaks |> print()

  ###### ** Mutate Data ######
  # x_denom |> print(); y_denom |> print()
  # df0[[xCol]] <- df0[[xCol]] / x_denom
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
  ### Has plot options
  hasTheme    <- !(theme0  |> is.null())
  hasMargins  <- !(margins |> is.null())
  hasMUnits   <- !(mUnit   |> is.null())
  ### Overwrite some values
  # yTitle0     <- y_label
  subtitle0   <- sector0   |> format_sectorNames(thresh0=nameBrk)

  ###### ** Plot Aesthetics ######
  ###### ** Titles
  # title0    <- impType0
  # subtitle0 <- variant0
  # title0    <- "Impact Type: " |> paste0(impType0)
  # subtitle0 <- "Variant: "     |> paste0(variant0)
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
  ### Legend position = bottom if refPlot; otherwise, don't show
  # lgdPos0   <- "bottom"
  lgdPos0   <- refPlot |> ifelse("bottom", "none")
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

  ###### Add in Model info ######
  if(do_gcm)  {
    ### Get model info
    renameAt0 <- c("model_label")
    renameTo0 <- c("model")
    select0   <- renameTo0   |> c("maxUnitValue")
    co_models <- "co_models" |> get_frediDataObj("frediData")
    co_models <- co_models |> filter((modelType |> tolower()) == "gcm")
    co_models <- co_models |> rename_at(c(renameAt0), ~renameTo0)
    co_models <- co_models |> select(all_of(select0))
    rm(renameAt0, renameTo0, select0)
    ### Join model info with df0
    join0     <- c("model")
    df0       <- df0 |> left_join(co_models, by=c(join0))
    rm(join0)
  } ### if(do_gcm)

  ###### Split Data ######
  ### Split the data into values for plotting lines, dashed lines, and points
  # # df0 |> names() |> print()
  # # df0 |> glimpse()
  if (do_gcm) {
    ### Plot these values as lines
    df0_1  <- df0 |> filter((driverValue <= maxUnitValue))
    ### Plot these values as dashed lines
    df0_2  <- df0 |> filter((driverValue >= maxUnitValue))
    # df0_1 |> glimpse(); df0_2 |> glimpse()
  } else {
    ### Plot these values as lines
    df0_1  <- df0
    ### Plot these values as points
    df0_2  <- tibble()
    # df0_2  <- df0 |> filter(year %in% x_breaks)
  } ### End if(do_gcm)


  ###### Create Plot ######
  ###### ** Initialize Plot #####
  # df0_1 |> glimpse()
  # plot0     <- ggplot()
  plot0     <- df0_1 |> ggplot(aes(
    x     = .data[[xCol]],
    y     = .data[[yCol]],
    color = .data[["model"]]
  )) + geom_line (alpha=alpha0) ### End aes/ggplot

  ###### ** Plot First Set ######
  # ### Add geomline
  # plot0  <- plot0 + geom_line (alpha=alpha0)

  ###### ** Plot Second Set ######
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
      # alpha    = alpha0 - 10
    ) ### End geom_line
  } ### End if(do_gcm)
  ### Remove data
  rm(df0_1, df0_2)

  ###### ** Plot Title ######
  ### Add title
  plot0       <- plot0 + ggtitle(title0, subtitle0)
  ### Add title aesthetics
  plot0       <- plot0 + theme(plot.title    = element_text(hjust=hjust0, size=12))
  plot0       <- plot0 + theme(plot.subtitle = element_text(hjust=hjust0, size=9))

  ###### ** Plot Axes ######
  ### Add axes
  # c(xTitle) |> print(); c(x_limits) |> print(); c(x_breaks) |> print();
  # c(y_label) |> print(); c(y_limits) |> print(); c(y_breaks) |> print();
  yTitle0     <- y_label
  plot0       <- plot0 + scale_x_continuous(xTitle, limits=x_limits, breaks=x_breaks)
  plot0       <- plot0 + scale_y_continuous(yTitle0, limits=y_limits, breaks=y_breaks)
  ### Add axes aesthetics
  plot0       <- plot0 + theme(axis.title   = element_text(size=8))

  ###### ** Plot Legend ######
  ### Add legend scales
  plot0       <- plot0 + scale_color_manual(lgdLbl, values=colorVals, breaks=shapeLvls)
  ### Add legend aesthetics
  plot0       <- plot0 + theme(legend.box = lgdDir0)
  plot0       <- plot0 + theme(legend.position = lgdPos0)
  plot0       <- plot0 + guides(color = guide_legend(title.position=lgdTit0, ncol=lgdNrow, byrow=TRUE))
  plot0       <- plot0 + guides(shape = guide_legend(title.position=lgdTit0, ncol=lgdNrow, byrow=TRUE))

  ###### ** Drop elements ######
  plot0       <- plot0 + theme(plot.title   = element_blank())
  plot0       <- plot0 + theme(axis.title.x = element_blank())

  ###### If plotIndex>1, remove some plot elements
  ###### White out axis text if column > 1
  if (col0 > 1) {
    plot0 <- plot0 + theme(axis.text.y  = element_text(color="white"))
    plot0 <- plot0 + theme(axis.title.y = element_text(color="white"))
    plot0 <- plot0 + theme(axis.title.x = element_blank())
  } ### End if(col0 > 1)

  ###### ** Add Theme ######
  if(hasTheme){
    # theme0 |> print()
    doBW <- theme0 == "bw"
    if (doBW) plot0 <- plot0 + theme_bw()
    else      plot0 <- plot0 + theme0
  } ### End if(hasTheme)

  ###### ** Add Margins ######
  if (hasMargins) {
    # margins |> print(); mUnit |> print()
    margin0 <- margin(
      t    = margins[1],  ### Top margin
      r    = margins[2],  ### Right margin
      b    = margins[3],  ### Bottom margin
      l    = margins[4],  ### Left margin
      unit = mUnit
    ) ### End margin
    ### Add to plot
    plot0 <- plot0 + theme(plot.margin = margin0)
  } ### End if(hasMargins)

  ###### Return ######
  ### Return the plots
  return(plot0)
}

###### End of Script ######



