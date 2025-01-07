###### plot_DOW_byModelType
### This function plots degrees of warming for a particular type of model
plot_DOW_byModelType <- function(
    data,
    modelType = "GCM",
    xCol      = "driverValue",
    yCol      = "annual_impacts",
    nCol      = 4,
    nTicks    = 5,
    silent    = F,
    repo0     = "FrEDI",
    options   = list(
      title      = NULL,
      xTitle     = NULL,
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      heights    = NULL,
      margins    = c(0, 1, .15, 1),
      marginUnit = "cm",
      nameBreak  = 18  , ### Sector name break
      theme      = NULL
    ) ### End options
){
  ###### Defaults ######
  ### Whether to message
  print_msg <- !silent
  if(print_msg) message("Running plot_DOW_byModelType()...")

  ###### Values ######
  ### Model Type
  # type0 |> print()
  typeLC0   <- modelType |> tolower()
  repo0     <- repo0 |> tolower()
  do_gcm    <- "gcm" %in% typeLC0
  do_slr    <- "slr" %in% typeLC0
  # modelType |> print(); do_gcm |> print(); do_slr |> print()

  ###### Format Data ######
  ###### ** Filter Data ######
  # data |> glimpse()
  df_x      <- data |> filter(model_type == modelType)
  # df_x |> glimpse()
  # df_x[["sector"]] |> unique() |> print()
  # df_x |> nrow() |> print()

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

  ######  ** Format Sector Names ######
  # refSectors <- df_x |> pull(sector) |> unique()
  # # refSectors |> print()
  # newSectors <- refSectors |> format_sectorNames(thresh0=nameBrk)
  # df_x       <- df_x |> mutate(sector = sector |> factor(levels=refSectors, labels=newSectors))

  ###### ** Get Sector Info ######
  infoList0  <- df_x |> get_sector_plotInfo(yCol=yCol, nCol=nCol, silent=silent)
  df_info    <- infoList0[["sectorInfo"]]
  df_minMax  <- infoList0[["minMax"    ]]
  c_sectors  <- infoList0[["cSectors"  ]]
  n_sectors  <- infoList0[["nSectors"  ]]
  nRow       <- infoList0[["nRow"      ]]
  # df_info |> print()

  ###### ** Get X Scale ######
  # xInfo      <- df_x |> get_colScale(col0=xCol, nTicks=5)
  xInfo      <- df_x |> get_colScale(col0=xCol, nTicks=nTicks)
  x_denom    <- xInfo[["denom" ]]
  x_breaks   <- xInfo[["breaks"]]
  x_limits   <- xInfo[["limits"]]
  # x_denom |> print(); x_breaks |> print()
  # "got here" |> print()

  ###### Create Plot List ######
  ### Get maximum and minimum values by plot row
  ### scaleCol=summary_value
  # # lgdLbl |> print()
  plotList_x <- c_sectors |> map(function(.x){
    plot_x <- .x |> plot_DOW_bySector(
      df0       = df_x  , ### Sector data
      infoList0 = infoList0, ### Dataframe with sector info...output from get_sector_plotInfo
      xCol      = xCol  , ### X-Column,
      yCol      = yCol  , ### Y-Column,
      xInfo     = xInfo , ### xScale...outputs of get_colScale
      refPlot   = FALSE , ### Whether to do a ref plot
      nTicks    = nTicks, ### Number of tick marks in scale
      options   = plotOpts0
    ) ### End plot_DOW_bySector
    # plot_x |> print()
    ### Return the plots
    return(plot_x)
  })
  # "got here" |> print()

  ### Add list names
  # # x |> length() |> print()
  plotList_x <- plotList_x |> set_names(c_sectors)

  ###### ** Create Reference Plot ######
  ### Create a reference plot
  refPlot_x  <- c_sectors[1] |> plot_DOW_bySector(
    df0       = df_x,   ### Sector data
    infoList0 = infoList0, ### Dataframe with sector info...output from get_sector_plotInfo
    xCol      = xCol  , ### X-Column,
    yCol      = yCol  , ### Y-Column,
    xInfo     = xInfo , ### xScale...outputs of get_colScale
    refPlot   = TRUE  , ### Whether to do a ref plot
    nTicks    = nTicks, ### Number of ticks to use in scale
    options   = plotOpts0
  ) ### End plot_DOW_bySector
  # return(refPlot_x)

  ###### ** Common Plot Elements ######
  ### Get common plot elements from reference plot
  # title0 |> print(); yTitle |> print(); xTitle |> print()
  plot_spacer   <- ggplot() + theme_void()
  plot_title    <- text_grob(title0, color="black", face="bold", size =14, hjust=0.5)
  plot_yTitle   <- text_grob(yTitle, color="black", rot=90)
  plot_xTitle   <- text_grob(xTitle, color="black")
  # plot_xTitle |> print()
  plot_legend   <- refPlot_x |> ggpubr::get_legend()
  # return(list(plots=plotList_x, lgd=plot_legend))
  plotList0     <- list(plots=plotList_x, lgd=plot_legend)
  # rm(plotList_x, plot_legend)

  ###### ** Plot Heights ######
  ### Arrange plot grid and elements
  # nCol |> print(); nRow |> print()
  ### Rel widths
  # widths0       <- rep(1, nCol)
  # heights0      <- rep(1, nRow)
  ### Set heights
  plot_heights0 <- c(0.2, nRow + 1, 1)
  # nRow |> print(); nCol |> print()

  ###### ** Arrange Main Grid ######
  ### Arrange main grid and add plot_xTitle
  ### Add spacer and add plot_yTitle
  main_grid     <- ggarrange(plotlist=plotList_x, ncol=nCol, nrow=nRow, legend="none", align="h", common.legend=T)
  main_grid     <- main_grid |> annotate_figure(bottom=plot_xTitle)
  ### Add spacer
  plotList0     <- list(x=plot_spacer, y=main_grid, z=plot_spacer)
  main_grid     <- ggarrange(plotlist=plotList0, ncol=3, nrow=1, legend="none", align="h", widths=c(0.2, nCol * 2, 0.2))
  main_grid     <- main_grid |> annotate_figure(left=plot_yTitle)
  ### Add Legend
  plotList0     <- list(x=plot_spacer, y=main_grid, w=plot_spacer, z=plot_legend, u=plot_spacer)
  main_plot     <- ggarrange(plotlist=plotList0, ncol=1, heights=c(0.2, nRow + 1, 0.1, 1, 0.1))
  main_plot     <- main_plot |> annotate_figure(top=plot_title)

  ###### Return ######
  ### Return
  if(print_msg) message("Finished.")
  return(main_plot)
} ### End function

###### End Script ######
