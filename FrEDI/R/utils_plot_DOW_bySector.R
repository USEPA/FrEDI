### Get Plot element
get_cowplot_element <- function(
    plot0,  ### Reference plot
    type0, ### Plot element type: spacer, xlab-b, ylab-l
    return_all = FALSE
){
  ### Get first list element
  obj0     <- plot0[[1]]
  type0 |> print()
  ### Get cowplot component
  # doComp   <- type0 %in% c("spacer", "xlab-b", "ylab-l")
  doSpace  <- type0 %in% c("spacer")
  doComp   <- type0 %in% c("xlab-b", "ylab-l")
  doTitle  <- type0 %in% c("title" )
  doLegend <- type0 %in% c("legend")
  doUnlist <- type0 %in% c("spacer")

  ### Get element
  # plot_spacer   <- (ref_plot[[1]] %>% cowplot::get_plot_component(pattern="spacer", return_all=T))[[1]] %>% (function(x){ggplotify::as.grob(x)})
  # plot_xTitle   <- (ref_plot[[1]] %>% cowplot::get_plot_component(pattern="xlab-b", return_all=F)) %>% (function(x){ggplotify::as.grob(x)})
  if     (doComp  ){elem0 <- obj0 |> cowplot::get_plot_component(pattern=type0, return_all=return_all)}
  else if(doSpace ){elem0 <- (obj0 |> cowplot::get_plot_component(pattern=type0, return_all=return_all))[[1]]}
  else if(doTitle ){elem0 <- obj0 |> cowplot::get_title ()}
  else if(doLegend){elem0 <- obj0 |> ggpubr::get_legend()}
  else             {elem0 <- list()}
  ### If unlist, get first list element
  # if(doUnlist){elem0 <- elem0[[1]]}
  ### Mutate to grob
  if(doComp|doSpace  ){grob0 <- ggplotify::as.grob(elem0)}
  else        {grob0 <- elem0}
  # [[1]] %>% (function(x){ggplotify::as.grob(x)})
  ### Return
  return(grob0)
} ### End get_cowplot_element

### Get list of cowplot elements
get_cowplot_elements <- function(
    plot0, ### Plot object
    types0 = c("spacer", "xlab-b", "ylab-l", "title", "legend")
){
  ### Get types
  types0   <- types0 |> tolower()
  ### Which types
  doSpacer <- "spacer" %in% types0
  doXlab   <- "xlab-b" %in% types0
  doYlab   <- "ylab-l" %in% types0
  doTitle  <- "title"  %in% types0
  doLegend <- "legend" %in% types0
  ### Initiate list
  list0  <- list()
  ### Add list elements
  if(doSpacer){list0[["spacer"]] <- get_cowplot_element(plot0=plot0, type0="spacer", return_all=TRUE )}
  if(doXlab  ){list0[["xlab-b"]] <- get_cowplot_element(plot0=plot0, type0="xlab-b", return_all=FALSE)}
  if(doYlab  ){list0[["ylab-l"]] <- get_cowplot_element(plot0=plot0, type0="ylab-l", return_all=FALSE)}
  if(doTitle ){list0[["title" ]] <- get_cowplot_element(plot0=plot0, type0="title" , return_all=TRUE )}
  if(doLegend){list0[["legend"]] <- get_cowplot_element(plot0=plot0, type0="legend", return_all=TRUE )}

  ### Return
  return(list0)
}

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
  hasTheme   <- !is.null(theme0  )
  hasMargins <- !is.null(margins0)
  hasMUnits  <- !is.null(mUnit0  )
  ### If units or no
  if(!hasMUnits){mUnit0 <- "cm"}

  ###### Create the plot ######
  # df0 %>% names() %>% print()
  # df0 %>% glimpse()
  plot0      <- df0 |> ggplot(aes(x=.data[[xCol]], y=.data[[yCol]]))

  ### Add Geoms
  plot0      <- plot0 + geom_line (aes(color = model))
  plot0      <- plot0 + geom_point(aes(color = model, shape=model))

  ### Add Scales
  plot0      <- plot0 + scale_color_discrete(lgdTitle0)
  plot0      <- plot0 + scale_shape_discrete(lgdTitle0)
  plot0      <- plot0 + scale_x_continuous(xTitle0, limits = x_limits, breaks = x_breaks)
  plot0      <- plot0 + scale_y_continuous(yTitle0, limits = y_limits, breaks = y_breaks)

  ###### Add titles ######
  plot0      <- plot0 + ggtitle(ggtitle0, subtitle0)
    # plot0      <- plot0 + theme(panel.background = element_rect(fill="white"))
    # plot0      <- plot0 + theme(panel.grid = element_line(color="lightgrey"))
    # plot0      <- plot0 + theme(axis.line = element_line(color="lightgrey"))
  plot0      <- plot0 + theme(plot.title    = element_text(hjust = 0.5, size=12))
  plot0      <- plot0 + theme(plot.subtitle = element_text(hjust = 0.5, size=9))

  ###### Format Plot ######
  ### Legend position = bottom if refPlot; otherwise, don't show
  lgdPos0    <- refPlot |> ifelse("bottom", "none")
  plot0      <- plot0 + theme(legend.position = lgdPos0)
  plot0      <- plot0 + theme(plot.title   = element_blank())
  plot0      <- plot0 + theme(axis.title   = element_text(size=8))
  plot0      <- plot0 + theme(axis.title.x = element_blank())
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





