###### plot_DOW_byModelType ######
### This function plots degrees of warming for a particular type of model
plot_DOW_byModelType <- function(
    data,
    modelType = "GCM",
    xCol      = "driverValue",
    yCol      = "annual_impacts",
    nCol      = 4,
    nTicks    = 5,
    options   = list(
      title      = NULL,
      xTitle     = NULL,
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      heights    = NULL,
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    ),
    silent    = F
){
  ###### Defaults ######
  ### Whether to message
  print_msg <- !silent
  if(print_msg) message("Running plot_DOW_byModelType()...")

  ### Model Type
  # modelType %>% print
  do_gcm    <- "gcm" %in% (modelType |> tolower())
  do_slr    <- "slr" %in% (modelType |> tolower())

  ###### Data ######
  # data |> glimpse()
  df_x      <- data |> filter(model_type == modelType)
  # df_x |> glimpse()
  # df_x[["sector"]] |> unique() |> print()
  # df_x |> nrow() |> print()

  ###### Plot Option Defaults ######
  ### Defaults
  def_titles  <- list(GCM="Impacts by Degrees of Warming", SLR="Impacts by GMSL (cm)")
  def_xTitles <- list(GCM=expression("Degrees of Warming (°C)"), SLR="GMSL (cm)")
  def_lgdLbls <- list(GCM="Model", SLR="Year")
  def_margins <- list(GCM=c(0, 0, .15, 0), SLR=c(0, .2, .15, 0))
  ### Values
  title       <- options[["title"     ]]
  xTitle      <- options[["xTitle"    ]]
  yTitle      <- options[["yTitle"    ]]
  lgdLbl      <- options[["lgdTitle"  ]]
  heights     <- options[["heights"   ]]
  margins     <- options[["margins"   ]]
  mUnit       <- options[["marginUnit"]]
  theme0      <- options[["theme"     ]]
  ### Plot options
  hasTitle    <- !(is.null(title   ))
  hasXTitle   <- !(is.null(xTitle  ))
  hasYTitle   <- !(is.null(yTitle  ))
  hasLgdLbl   <- !(is.null(lgdLbl  ))
  hasHeights  <- !(is.null(heights ))
  hasMargins  <- !(is.null(margins ))
  hasMUnits   <- !(is.null(mUnit   ))
  hasTheme    <- !(is.null(theme0  ))
  ### Defaults: Default Heights Below
  def_title   <- do_gcm |> ifelse(def_titles [["GCM"]], def_titles [["SLR"]])
  def_xTitle  <- do_gcm |> ifelse(def_xTitles[["GCM"]], def_xTitles[["SLR"]])
  def_lgdLbl  <- do_gcm |> ifelse(def_lgdLbls[["GCM"]], def_lgdLbls[["SLR"]])
  def_margin  <- do_gcm |> ifelse(def_margins[["GCM"]], def_margins[["SLR"]])
  def_yTitle  <- "Impacts ($2015)"
  def_mUnit   <- "cm"
  def_theme   <- NULL
  ### Values: Height Values Below
  if(!hasTitle  ){title   <- def_title }
  if(!hasXTitle ){xTitle  <- def_xTitle}
  if(!hasYTitle ){yTitle  <- def_yTitle}
  if(!hasLgdLbl ){lgdLbl  <- def_lgdLbl}
  if(!hasMargins){margins <- def_margin}
  if(!hasMUnits ){mUnit   <- def_mUnit }
  if(!hasTheme  ){theme0  <- def_theme }
  # title |> print(); def_xTitle |> print()
  ### Update plot options
  plotOpts0    <- list(
    title      = title,
    xTitle     = xTitle,
    yTitle     = yTitle,
    lgdTitle   = lgdLbl,
    margins    = margins,
    marginUnit = mUnit,
    theme      = theme0
  )

  ###### Standardize column names ######
  # df_x[["xCol"]] <- df_x[[xCol]]
  # df_x[["yCol"]] <- df_x[[yCol]]

  ###### Get Sector Info ######
  infoList0     <- df_x |> get_sector_plotInfo(yCol=yCol, nCol=nCol, silent=silent)
  df_info       <- infoList0[["sectorInfo"]]
  df_minMax     <- infoList0[["minMax"    ]]
  c_sectors     <- infoList0[["cSectors"  ]]
  n_sectors     <- infoList0[["nSectors"  ]]
  nRow          <- infoList0[["nRow"      ]]
  # df_info |> print()

  ###### Get X Scale ######
  ###### X Scales
  x_info     <- df_x |> get_colScale(col0=xCol, nTicks = 5)
  x_scale    <- x_info[["scale" ]]
  x_p10      <- x_info[["p10"   ]]
  x_denom    <- x_info[["denom" ]]
  x_breaks   <- x_info[["breaks"]]
  x_limits   <- x_info[["limits"]]
  # x_p10 |> print(); x_denom |> print(); x_breaks |> print()
  # "got here" |> print()

  ###### Create Plot List ######
  ### Get maximum and minimum values by plot row
  ### scaleCol=summary_value
  # # lgdLbl |> print()
  plotList_x <- c_sectors %>% map(function(.x){
    plot_x <- .x |> plot_DOW_bySector(
      df0       = df_x,      ### Sector data
      infoList0 = infoList0, ### Dataframe with sector info...output from get_sector_plotInfo
      xCol      = xCol, ### X-Column,
      yCol      = yCol, ### Y-Column,
      xInfo     = x_info, ### xScale...outputs of get_colScale
      refPlot   = FALSE , ### Whether to do a ref plot
      nTicks    = nTicks,
      options   = plotOpts0
    )
    ### Return the plots
    return(plot_x)
  })
  # "got here" |> print()

  ### Add list names
  # # x |> length() |> print()
  plotList_x  <- plotList_x |> addListNames(c_sectors)

  ###### Get Reference Plot ######
  refPlot_x   <- c_sectors[1] |> plot_DOW_bySector(
    df0       = df_x,      ### Sector data
    infoList0 = infoList0, ### Dataframe with sector info...output from get_sector_plotInfo
    xCol      = xCol, ### X-Column,
    yCol      = yCol, ### Y-Column,
    xInfo     = x_info, ### xScale...outputs of get_colScale
    refPlot   = TRUE  , ### Whether to do a ref plot
    nTicks    = nTicks,
    options   = plotOpts0
  )
  # return(refPlot_x)

  ###### Get Plot Elements ######
  ### Get plot elements
  # title |> print(); yTitle |> print(); xTitle |> print()
  plot_spacer   <- ggplot() + theme_void()
  plot_title    <- text_grob(title , color = "black", face = "bold", size = 14, hjust=0.5)
  plot_yTitle   <- text_grob(yTitle, color = "black", rot  = 90)
  plot_xTitle   <- text_grob(xTitle, color = "black")
  plot_legend   <- refPlot_x |> ggpubr::get_legend()
  # return(list(plots=plotList_x, lgd=plot_legend))
  plotList0     <- list(plots=plotList_x, lgd=plot_legend)
  rm(plotList_x, plot_legend)

  ### Arrange plot grid and elements
  nCol |> print(); nRow |> print()
  ### Rel widths
  widths0       <- rep(1, nCol)
  heights0      <- rep(1, nRow)
  ### Set heights
  plot_heights0  <- c(0.2, nRow + 1, 1)
  # nRow          <- nRow - 1

  ###### Arrange Main Grid ######
  ### Arrange main grid and add plot_xTitle
  ### Add spacer and add plot_yTitle
  main_grid      <- ggarrange(plotlist=plotList0[["plots"]], ncol=nCol, nrow=nRow, legend="none", align="h", common.legend = T, widths=widths0, heights=heights0)
  # main_grid      <- main_grid |> annotate_figure(left = plot_yTitle, bottom = plot_xTitle)
  main_grid      <- main_grid |> annotate_figure(bottom = plot_xTitle)
  main_grid      <- ggarrange(plotlist=list(
    x=plot_spacer,
    y=main_grid,
    z=plot_spacer
    ), ncol=3, nrow=1, legend="none", align="h", widths=c(0.2, nCol * 2, 0.2), heights=rep(1, 3))
  main_grid      <- main_grid |> annotate_figure(left = plot_yTitle)
  # main_list      <- list(spacer=plot_spacer, plots=main_grid, lgd = plotList0[["lgd"]])
  ###### Arrange Plot ######
  main_plot      <- ggarrange(
    # plotlist=main_list,
    plotlist=list(
      # x=ggplot() + theme_void(),
      x=plot_spacer,
      y=main_grid,
      z=plotList0[["lgd"]]
    ),
    ncol=1,
    heights=plot_heights0
  )
  main_plot <- main_plot |> annotate_figure(top=plot_title)
  return(main_plot)

  ###### Return######
  ### Return
  if(print_msg){ message("Finished.")}
  return(main_plot)

}
