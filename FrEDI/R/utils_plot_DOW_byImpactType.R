###### plot_DOW_byImpactType ######
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
    options = list(
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
  ###### Messaging ######
  print_msg <- !silent
  if(print_msg){ "Running plot_DOW_byImpactType()..." |> message()}

  ###### Data ######
  df0        <- data |> filter(sector     == sector0)
  df0        <- df0  |> filter(impactYear == impYear0)
  df0        <- df0  |> filter(variant    == variant0)
  df0        <- df0  |> filter(impactType == impType0)
  type0      <- df0[["model_type"]] |> unique()
  # df0 |> glimpse()

  ###### Model Types ######
  # type0 %>% print
  do_gcm    <- "gcm" %in% (type0 |> tolower())
  do_slr    <- "slr" %in% (type0 |> tolower())

  ###### Sector info ######
  info0      <- infoList0[["sectorInfo"]] |> filter(sector==sector0)
  index0     <- info0[["sector_order"]][1]
  row0       <- info0[["plotRow"     ]][1]
  col0       <- info0[["plotCol"     ]][1]

  ###### Breaks info ######
  ###### ** X Breaks ######
  do_xInfo   <- is.null(xInfo)
  if(do_xInfo){
    if(xCol == "year"){
      x_limits   <- c(2010, 2090)
      x_breaks <- seq(x_limits[1] - 10, x_limits[2] + 10, by = 20)
      x_denom  <- 1
    } ### End if(xCol == "year")
    else              {
      x_info     <- df0 |> get_colScale(col0=xCol, nTicks = 5)
      x_scale    <- x_info[["scale" ]]
      x_p10      <- x_info[["p10"   ]]
      x_denom    <- x_info[["denom" ]]
      x_breaks   <- x_info[["breaks"]]
      x_limits   <- x_info[["limits"]]
    } ### End else(xCol == "year")
  } ### End if(do_xInfo)
  else{
    x_scale    <- xInfo[["scale" ]]
    x_p10      <- xInfo[["p10"   ]]
    x_denom    <- xInfo[["denom" ]]
    x_breaks   <- xInfo[["breaks"]]
    x_limits   <- xInfo[["limits"]]
  }
  # x_scale |> print(); x_breaks |> print()
  # x_breaks |> print()
  # "got here" |> print()
  ###### ** Y-Breaks ######
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
  # if( sector0=="Marine Fisheries"){
  #   y_limits <- c(-0.12, .07)
  #   y_breaks <- seq(-0.1, 0.1, by=0.04)
  # }
  # y_label |> print(); y_breaks |> print()
  # ### Labeling
  y_prelabel <- (y_label == "") |> ifelse("", ", ")
  # y_label    <- "" |> paste0("(", y_label, y_prelabel, ")")
  y_label    <- y_label |> paste0(y_prelabel, "$2015")
  y_label    <- "Impacts (" |> paste0(y_label, ")")
  # y_p10 |> print(); y_denom |> print(); y_breaks |> print()

  ###### Mutate Data ######
  # "got here" |> print(); x_denom |> print(); y_denom |> print()
  df0[[xCol]] <- df0[[xCol]] / x_denom
  df0[[yCol]] <- df0[[yCol]] / y_denom

  # ###### Plot Options ######
  ###### Defaults ######
  ### Defaults
  def_titles  <- list(GCM="Impacts by Degrees of Warming", SLR="Impacts by GMSL (cm)")
  def_xTitles <- list(GCM=expression("Degrees of Warming (°C)"), SLR="GMSL (cm)")
  def_lgdLbls <- list(GCM="Model", SLR="Year")
  def_margins <- list(GCM=c(0, 0, .15, 0), SLR=c(0, .2, .15, 0))
  ### Values
  title0      <- options[["title"     ]]
  xTitle      <- options[["xTitle"    ]]
  yTitle      <- options[["yTitle"    ]]
  lgdLbl      <- options[["lgdTitle"  ]]
  lgdPos      <- options[["lgdPos"    ]]
  heights     <- options[["heights"   ]]
  margins     <- options[["margins"   ]]
  mUnit       <- options[["marginUnit"]]
  theme0      <- options[["theme"     ]]
  ### Plot options
  hasTitle    <- !(is.null(title0  ))
  hasXTitle   <- !(is.null(xTitle  ))
  hasYTitle   <- !(is.null(yTitle  ))
  hasLgdLbl   <- !(is.null(lgdLbl  ))
  hasLgdPos   <- !(is.null(lgdPos  ))
  hasHeights  <- !(is.null(heights ))
  hasMargins  <- !(is.null(margins ))
  hasMUnits   <- !(is.null(mUnit   ))
  hasTheme    <- !(is.null(theme0  ))
  ### Defaults: Default Heights Below
  def_title   <- do_gcm |> ifelse(def_titles [["GCM"]], def_titles [["SLR"]])
  def_xTitle  <- do_gcm |> ifelse(def_xTitles[["GCM"]], def_xTitles[["SLR"]])
  def_margin  <- do_gcm |> ifelse(def_margins[["GCM"]], def_margins[["SLR"]])
  def_lgdLbl  <- do_gcm |> ifelse(def_lgdLbls[["GCM"]], def_lgdLbls[["SLR"]])
  def_lgdPos  <- "top"
  def_yTitle  <- "Impacts ($2015)"
  def_mUnit   <- "cm"
  def_theme   <- NULL
  ### Values: Height Values Below
  if(!hasTitle  ){title0  <- def_title }
  if(!hasXTitle ){xTitle  <- def_xTitle}
  if(!hasYTitle ){yTitle  <- def_yTitle}
  if(!hasLgdLbl ){lgdLbl  <- def_lgdLbl}
  if(!hasMargins){margins <- def_margin}
  if(!hasMUnits ){mUnit   <- def_mUnit }
  if(!hasTheme  ){theme0  <- def_theme }
  # xTitle |> print()
  ###### Standardize column names ######
  title0    <- "Impact Type: " |> paste0(impType0)
  subtitle0 <- "Variant: "     |> paste0(variant0)
  # title0    <- impType0
  # subtitle0 <- variant0

  ###### Create the plot ######
  plot0     <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]]))

  ### Add Geoms
  plot0     <- plot0 + geom_line (aes(color = model))
  if(do_slr){df_points0 <- df0 |> filter(year %in% x_breaks)}
  else      {df_points0 <- df0}
  plot0     <- plot0 + geom_point(data=df_points0, aes(color = model, shape=model))

  ### Add Scales
  # plot0     <- plot0 + scale_shape_discrete(lgdTitle0)
  shapeLvls <- df0[["model"]] |> unique() |> sort()
  numShapes <- shapeLvls |> length()
  shapeVals <- c(1:numShapes)
  # shapeLvls |> print()
  # plot0     <- plot0 + scale_shape_discrete(lgdLbl)
  plot0     <- plot0 + scale_shape_manual(lgdLbl, breaks=shapeLvls, values=shapeVals)
  plot0     <- plot0 + scale_color_discrete(lgdLbl)
  # plot0     <- plot0 + scale_shape_discrete(lgdLbl)

  ###### Adjust legend title ######
  if(hasLgdPos){plot0  <- plot0  + guides(color = guide_legend(title.position = lgdPos))}

  ###### Add themes and title ######
  plot0     <- plot0 + ggtitle(title0, subtitle0)

  ###### Add scales ######
  plot0     <- plot0 + scale_x_continuous(xTitle, breaks = x_breaks, limits = x_limits)
  # plot0     <- plot0 + scale_y_continuous(yTitle)
  plot0     <- plot0 + scale_y_continuous(y_label)

  ###### Adjust Appearance ######
  # plot0     <- plot0 + theme(panel.background = element_rect(fill="white"))
  # plot0     <- plot0 + theme(panel.grid = element_line(color="lightgrey"))
  # plot0     <- plot0 + theme(plot.title    = element_text(hjust = 0.5, size=11))
  plot0     <- plot0 + theme(plot.title    = element_text(hjust = 0.5, size=11))
  plot0     <- plot0 + theme(plot.subtitle = element_text(hjust = 0.5, size=10))
  # plot0     <- plot0 + theme(axis.title.x  = element_text(hjust = 0.5, size=9, color="white"))
  plot0     <- plot0 + theme(axis.title.x  = element_text(hjust = 0.5, size=9))
  plot0     <- plot0 + theme(axis.title.y  = element_text(hjust = 0.5, size=9))
  plot0     <- plot0 + theme(legend.position = "bottom")

  ###### Plot Index #####
  ###### If plotIndex, remove some plot elements
  # if(col0 > 1){
  #   plot0 <- plot0 + theme(plot.title = element_blank())
  #   # plot0 <- plot0 + theme(axis.title = element_blank())
  #   # plot0 <- plot0 + theme(axis.title.x = element_blank())
  #   plot0 <- plot0 + theme(axis.title.x = element_blank())
  # }

  ###### Add Themes & Margins ######
  ### Theme
  if(hasTheme  ){
    if(theme=="bw"){plot0 <- plot0 + theme_bw()}
    else           {plot0 <- plot0 + theme0}
  } ### End if(hasTheme  )
  ### Margins
  if(hasMargins){
    plot0 <- plot0 + theme(plot.margin = margin(
      t = margins[1],  # Top margin
      r = margins[2],  # Right margin
      b = margins[3],  # Bottom margin
      l = margins[4],  # Left margin
      unit = mUnit
    ))
  } ### End if(hasMargins)


  ###### Return ######
  ### Return the plot
  if(print_msg){ message("...Finished.")}
  return(plot0)
}
