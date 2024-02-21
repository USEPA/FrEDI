
###### plot_DOW_bySector ######
### Get plot_DOW_byModelType_sector
plot_DOW_bySector <- function(
    sector0,           ### Plot index number
    df0,               ### Sector data
    infoList0,         ### Dataframe with sector info...output from get_sector_plotInfo
    xCol     = "xCol", ### X-Column,
    yCol     = "yCol", ### Y-Column,
    xInfo    = NULL  , ### xScale...outputs of get_colScale
    refPlot  = F     , ### Whether to do a ref plot
    nTicks   = 5,
    options  = list(
      title      = "Impacts by Degrees of Warming",
      # subtitle   = NULL,
      xTitle     = expression("Degrees of Warming (°C)"),
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ###### Data ######
  df0        <- df0  |> filter(sector==sector0)
  type0      <- df0[["model_type"]] |> unique()
  do_gcm     <- (type0 |> tolower()) == "gcm"
  ###### Sector info ######
  info0      <- infoList0[["sectorInfo"]] |> filter(sector==sector0)
  index0     <- info0[["sector_order"]][1]
  row0       <- info0[["plotRow"     ]][1]
  col0       <- info0[["plotCol"     ]][1]
  ###### Breaks info ######
  ###### ** X Breaks ######
  do_xInfo   <- is.null(xInfo)
  if(do_xInfo){xInfo <- df0 |> get_colScale(col0 = xCol, nTicks=nTicks)}
  x_scale    <- xInfo[["scale" ]]
  x_p10      <- xInfo[["p10"   ]]
  x_denom    <- xInfo[["denom" ]]
  x_breaks   <- xInfo[["breaks"]]
  x_limits   <- xInfo[["limits"]]
  # x_scale |> print()
  # x_breaks |> print()
  ###### ** Y Breaks ######
  y_info     <- infoList0[["minMax"]] |> filter(plotRow == row0)
  y_info     <- y_info |> mutate(sector=sector0)
  y_info     <- y_info |> get_colScale(col0="summary_value", nTicks=nTicks)
  # y_info     <- y_info |> get_colScale(col0="summary_value", nTicks=nTicks)
  ### Additional info
  y_scale    <- y_info[["scale" ]]
  # y_scale |> names() |> print()
  y_p10      <- y_info[["p10"   ]]
  y_denom    <- y_info[["denom" ]]
  y_breaks   <- y_info[["breaks"]]
  y_limits   <- y_info[["limits"]]
  y_label    <- y_info[["label" ]]
  # y_label |> print()
  # y_breaks |> print()
  # ### Labeling
  y_prelabel <- (y_label == "") |> ifelse("", ", ")
  # y_label    <- "" |> paste0("(", y_label, y_prelabel, ")")
  y_label    <- y_label |> paste0(y_prelabel, "$2015")
  # y_p10 |> print(); y_denom |> print(); y_breaks |> print()

  ###### Mutate Data ######
  # x_denom |> print(); y_denom |> print()
  df0[[xCol]] <- df0[[xCol]] / x_denom
  df0[[yCol]] <- df0[[yCol]] / y_denom

  ###### Plot Options ######
  theme0      <- options[["theme"      ]]
  margins0    <- options[["margins"    ]]
  mUnit0      <- options[["marginsUnit"]]
  xTitle0     <- options[["xTitle"     ]]
  yTitle0     <- options[["yTitle"     ]]
  yTitle0     <- y_label
  lgdTitle0   <- options[["lgdTitle"   ]]
  ggtitle0    <- options[["title"      ]]
  subtitle0   <- sector0
  ### Has plot options
  hasTheme    <- !is.null(theme0  )
  hasMargins  <- !is.null(margins0)
  hasMUnits   <- !is.null(mUnit0  )
  ### If units or no
  if(!hasMUnits){mUnit0 <- "cm"}

  ###### Add in Model info ######
  if(do_gcm)  {
    ### Get model info
    select0   <- c("model_id", "maxUnitValue")
    rename0   <- c("model_id")
    rename1   <- c("model")
    co_models <- "co_models" |> get_frediDataObj("frediData")
    co_models <- co_models |> filter((modelType |> tolower()) == "gcm")
    co_models <- co_models |> select(all_of(select0))
    co_models <- co_models |> rename_at(vars(rename0), ~rename1)
    rm(select0, rename0, rename1)
    ### Join model info with df0
    join0     <- c("model")
    df0       <- df0 |> left_join(co_models, by=c(join0))
    rm(join0)
  } ### if(do_gcm)

  ###### Create the plot ######
  # # df0 |> names() |> print()
  # # df0 |> glimpse()
  # plot0       <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[["model"]]))
  # # plot0       <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], group=interaction(sector, model)))

  ### Add Geoms
  # plot0       <- plot0 + geom_line (aes(color = model))
  # # plot0       <- plot0 + geom_point(aes(color = model))
  # plot0       <- plot0 + geom_point(aes(color = model, shape=model))
  if(do_gcm){
    ### Separate GCM values
    ### Plot these values as lines
    df0_1  <- df0 |> filter((maxUnitValue < 6 & driverValue <= maxUnitValue) | maxUnitValue >=6)
    ### Plot these values as points
    df0_2  <- df0 |> filter((maxUnitValue < 6 & driverValue >= maxUnitValue))
    ### Initialize plot
    plot0  <- ggplot()
    ### Plot values as lines
    plot0  <- plot0 + geom_line (data = df0_1, aes(x = .data[[xCol]], y = .data[[yCol]], color=.data[["model"]]), alpha=0.65)
    ### Plot values as points
    plot0  <- plot0 + geom_point(data = df0_2, aes(x = .data[[xCol]], y = .data[[yCol]], color=.data[["model"]], shape = .data[["model"]]), alpha=0.65)
  } else{
    ### Initialize plot
    plot0  <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[["model"]]))
    ### Add geoms
    plot0  <- plot0 + geom_line (aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[["model"]]), alpha=0.65)
    plot0  <- plot0 + geom_point(aes(x=.data[[xCol]], y=.data[[yCol]], color=.data[["model"]], shape = model), alpha=0.65)
  } ### End if(do_gcm)

  ### Add Scales
  plot0       <- plot0 + scale_color_discrete(lgdTitle0)
  # plot0       <- plot0 + scale_shape_discrete(lgdTitle0)
  shapeLvls   <- df0[["model"]] |> unique() |> sort()
  numShapes   <- shapeLvls |> length()
  shapeVals   <- c(1:numShapes)
  # shapeLvls |> print()
  # plot0       <- plot0 + scale_shape_discrete(lgdTitle0)
  plot0       <- plot0 + scale_shape_manual(lgdTitle0, breaks=shapeLvls, values=shapeVals)
  plot0       <- plot0 + scale_x_continuous(xTitle0, limits = x_limits, breaks = x_breaks)
  plot0       <- plot0 + scale_y_continuous(yTitle0, limits = y_limits, breaks = y_breaks)

  ###### Add titles ######
  plot0       <- plot0 + ggtitle(ggtitle0, subtitle0)
  # plot0       <- plot0 + theme(panel.background = element_rect(fill="white"))
  # plot0       <- plot0 + theme(panel.grid = element_line(color="lightgrey"))
  # plot0       <- plot0 + theme(axis.line = element_line(color="lightgrey"))
  plot0       <- plot0 + theme(plot.title    = element_text(hjust = 0.5, size=12))
  plot0       <- plot0 + theme(plot.subtitle = element_text(hjust = 0.5, size=9))

  ###### Format Plot ######
  ### Legend position = bottom if refPlot; otherwise, don't show
  lgdPos0     <- refPlot |> ifelse("bottom", "none")
  plot0       <- plot0 + theme(legend.box = "vertical")
  plot0       <- plot0 + guides(color = guide_legend(title.position="top", ncol=4, byrow=TRUE))
  plot0       <- plot0 + guides(shape = guide_legend(title.position="top", ncol=3, byrow=TRUE))
  # plot0       <- plot0 + theme(legend.direction = "vertical")
  plot0       <- plot0 + theme(legend.position = lgdPos0)
  plot0       <- plot0 + theme(plot.title   = element_blank())
  plot0       <- plot0 + theme(axis.title   = element_text(size=8))
  plot0       <- plot0 + theme(axis.title.x = element_blank())
  ###### If plotIndex>1, remove some plot elements
  ###### White out axis text if column > 1
  if(col0 > 1){
    plot0 <- plot0 + theme(axis.text.y  = element_text(color="white"))
    plot0 <- plot0 + theme(axis.title.y = element_text(color="white"))
    plot0 <- plot0 + theme(axis.title.x = element_blank())
  } ### End if(col0 > 1)


  ### Theme
  if(hasTheme){
    # theme0 |> print()
    if(theme0=="bw"){plot0 <- plot0 + theme_bw()}
    else            {plot0 <- plot0 + theme0    }
  } ### End if(hasTheme)
  ### Margins
  if(hasMargins){
    # margins0 |> print(); mUnit0 |> print()
    margin0 <- margin(
      t    = margins0[1],  # Top margin
      r    = margins0[2],  # Right margin
      b    = margins0[3],  # Bottom margin
      l    = margins0[4],  # Left margin
      unit = mUnit0
    )
    ### Add to plot
    plot0 <- plot0 + theme(plot.margin = margin0)
  } ### End if(hasMargins)

  ### Return the plots
  return(plot0)
}





