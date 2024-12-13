###### plot_DOW_byImpactType
### This function plots degrees of warming by sector, variant, impact year, and type
plot_DOW_byImpactTypes <- function(
    data,
    sector,
    xCol      = "driverValue",
    yCol      = "annual_impacts",
    modelType = "GCM",
    nTicks    = 5,
    silent    = TRUE,
    repo0     = "FrEDI",
    options   = list(
      title      = "Impacts by Degrees of Warming",
      # subtitle   = NULL,
      xTitle     = expression("Degrees of Warming (°C)"),
      yTitle     = "Impacts ($2015)",
      lgdTitle   = "Model",
      nameBreak  = 18, ### Sector name break
      margins    = c(0, 0, .15, 0),
      marginUnit = "cm",
      theme      = NULL
    )
){
  ###### Messaging ######
  print_msg <- !silent
  if(print_msg) "Running plot_DOW_byImpactType()..." |> message()

  ###### Values ######
  ### Model Type
  # type0 |> print()
  typeLC0   <- type0 |> tolower()
  repo0     <- repo0 |> tolower()
  do_gcm    <- "gcm" %in% typeLC0
  do_slr    <- "slr" %in% typeLC0
  # modelType |> print(); do_gcm |> print(); do_slr |> print()


  ###### Format Data ######
  ###### ** Filter Data ######
  ### Filter to sector and convert to data frame
  # data |> glimpse()
  # data[["sector"]] |> unique() |> print(); modelType |> print()
  sector0   <- sector; rm(sector)
  df0       <- data  ; rm(data  )
  df0       <- df0 |> filter(model_type == modelType)
  df0       <- df0 |> filter(sector     == sector0  )
  # df0 |> glimpse()

  ###### ** Sector Info ######
  ### Get sector info
  # df0 |> glimpse()
  infoList0  <- df0 |> get_sector_plotInfo(yCol=yCol, byType=TRUE, silent=silent)
  df_info    <- infoList0[["sectorInfo"]]
  df_minMax  <- infoList0[["minMax"    ]]
  nRow       <- infoList0[["nRow"      ]]
  nCol       <- infoList0[["nCol"      ]]
  ### Unique values
  c_sectors  <- infoList0[["cSectors" ]]
  c_impYears <- infoList0[["cImpYears"]]
  c_variants <- infoList0[["cVariants"]]
  c_impTypes <- infoList0[["cImpTypes"]]
  c_models   <- df0[["model"]] |> unique()
  ### Numbers
  n_sectors  <- c_sectors  |> length()
  n_impYears <- c_impYears |> length()
  n_variants <- c_variants |> length()
  n_impTypes <- c_impTypes |> length()
  n_models   <- c_models   |> length()
  # c_impYears |> print(); c_impTypes |> print(); c_variants |> print()
  # n_impYears |> print(); n_impTypes |> print(); n_variants |> print()

  ###### ** Factor Model ######
  ### Factor model levels
  df0        <- df0 |> mutate(model = model |> factor(levels=c_models))


  ###### Plot Setup ######
  ###### ** X Breaks ######
  do_xInfo   <- xInfo |> is.null()
  if(do_xInfo) {
    xInfo <- xInfo |> getXAxisScale(
      xCol    = xCol,
      maxYear = 2100,
      yrUnit  = 20,
      nTicks  = nTicks
    ) ### End getXAxisScale
  } ### End if(do_xInfo)
  ### Assign to objects
  x_limits   <- xInfo[["limits"]]
  x_breaks   <- xInfo[["breaks"]]
  x_denom    <- xInfo[["denom" ]]

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


  ###### Reference Plot ######
  ###### ** Create Reference Plot ######
  ### Reference plots
  refPlot0   <- df0 |> plot_DOW_byImpactType(
    sector0   = sector0,
    impYear0  = c_impYears[1],
    impType0  = c_impTypes[1],
    variant0  = c_variants[1],
    infoList0 = infoList0, ### Dataframe with sector info...output from get_sector_plotInfo
    xCol      = xCol  , ### X-Column,
    yCol      = yCol  , ### Y-Column,
    xInfo     = xInfo, ### xScale...outputs of get_colScale
    refPlot   = TRUE  , ### Whether to do a ref plot
    silent    = silent,
    options   = plotOpts0
  ) ### End plot_DOW_byImpactType
  # refPlot0 |> print()

  ### Add guide to legend
  lgdCols   <- case_when(
    n_variants <= 1 ~ 2 ,
    n_variants == 2 ~ 3,
    .default = 4
  ) ### End case_when
  # lgdCols |> print()

  ### Add guides to reference plot
  refPlot0  <- refPlot0 + guides(color=guide_legend(ncol=lgdCols))

  ###### ** Common Plot Elements ######
  ### Get common plot elements from reference plot
  spacer0   <- ggplot() + theme_void()
  legend0   <- refPlot0 |> ggpubr::get_legend()
  grobLgd0  <- ggarrange(plotlist=list(legend=legend0))

  ###### Create Plot List ######
  ###### ** Create Impact Years plots ######
  listYears0 <- c_impYears |> map(function(impYear_i){
    ###### ** Create impact type plots ######
    listTypes_i <- c_impTypes |> map(function(impType_j){
      ### Figure out min/max across all variants for an impact type to get the y-scale
      listVars_j <- c_variants |> map(function(variant_k){
        ###### ** Create variant plot ######
        plot_k <- df0 |> plot_DOW_byImpactType(
          sector0   = sector0,
          impYear0  = impYear_i,
          impType0  = impType_j,
          variant0  = variant_k,
          infoList0 = infoList0, ### Dataframe with sector info...output from get_sector_plotInfo
          xCol      = xCol  , ### X-Column,
          yCol      = yCol  , ### Y-Column,
          xInfo     = xInfo , ### xScale...outputs of get_colScale
          refPlot   = FALSE , ### Whether to do a ref plot
          silent    = silent,
          options   = plotOpts0
        ) ### End plot_DOW_byImpactType

        ### White out impact type label
        plot_k <- plot_k + theme(plot.title = element_text(color="white"))
        ### Add spacer to the right
        list_k <- list(plot=plot_k, spacer1=spacer0)
        grid_k <- ggarrange(plotlist=list_k, nrow=1, ncol=2, common.legend=T, legend="none", widths=c(1, 0.1))
        ### Add spacer to the left
        list_k <- list(spacer1=spacer0, plot=grid_k)
        grid_k <- ggarrange(plotlist=list_k, nrow=1, ncol=2, common.legend=T, legend="none", widths=c(0.1, 1))
        # "got here..." |> print()
        ### Return plot
        return(grid_k)
      })

      ### Name the plots
      listVars_j  <- listVars_j |> set_names(c_variants)
      # return(listVars_j)

      ### Arrange the other plots in a grid
      plotGrid_j <- ggarrange(plotlist=listVars_j, nrow=1, ncol=nCol, common.legend=T, legend="none")
      # return(plotGrid_j)

      ###### ** Annotate Variant Plots ######
      ### Labels on top
      ### Longest impact type: "Acute Myocardial Infarction"
      # title_j      <- "Impact Type: " |> paste0(impType_j)
      # impTitle_j  <- impType_j
      typeTitle_j  <- "Impact Type: " |> paste0(impType_j)
      grobType_j   <- text_grob(typeTitle_j, face="italic", size=11)
      plotList_j   <- list(spacer1=spacer0, plots=plotGrid_j, spacer2=spacer0)
      plotGrid_j   <- ggarrange(plotlist=plotList_j, nrow=3, ncol=1, common.legend=T, legend="none", heights=c(0.01, 1, 0.05))
      plotGrid_j   <- plotGrid_j |> annotate_figure(top=grobType_j)

      ### Return
      return(plotGrid_j)
    })

    ###### ** Annotate Impact Year Plots ######
    ### Name the plots
    # listTypes_i |> length() |> print(); c_impTypes |> print()
    listTypes_i <- listTypes_i |> set_names(c_impTypes)
    # return(listTypes_i)

    ### Arrange plot list
    plotGrid_i   <- ggarrange(plotlist=listTypes_i, ncol=1, nrow=nRow, common.legend=T, legend="none")

    ### Add spacer to the top
    yearTitle_i <- "Impact Year: " |> paste0(impYear_i)
    grobYear_i  <- text_grob(yearTitle_i, face="plain", size=13)
    plotList_i  <- list(spacer1=spacer0, plot=plotGrid_i)
    plotGrid_i  <- ggarrange(plotlist=plotList_i, nrow=2, ncol=1, common.legend=T, legend="none", heights=c(0.01, 1))
    plotGrid_i  <- plotGrid_i |> annotate_figure(top=grobYear_i)

    ### Heights for spacer
    h_spacer1  <- 0.05
    h_spacer2  <- 0.05
    h_plots    <- n_impTypes * (1 + 0.07)
    h_legend   <- 0.5
    h_spacers  <- c(h_spacer1, h_plots, h_spacer1, h_legend, h_spacer1/4)
    ### Add Plot Title & Y Title
    # plotList_i <- list(spacer1=spacer0, plot=plotGrid_i, legend=legend0)
    # plotList_i <- list(spacer1=spacer0, plot=plotGrid_i, legend=grobLgd0)
    plotList_i <- list(spacer1=spacer0, plot=plotGrid_i, spacer2=spacer0, legend=grobLgd0, spacer1=spacer0)
    plotGrid_i <- ggarrange(plotlist=plotList_i, nrow=5, ncol=1, legend="none", heights=h_spacers)
    title0_i   <- sector0
    grobTit_i  <- text_grob(title0_i, color="black", size=12, face="bold", hjust=0.5)
    plotYTit_i <- text_grob(yTitle  , color="black", rot =90)
    plotGrid_i <- plotGrid_i |> annotate_figure(top=grobTit_i, left=plotYTit_i)
    # return(plotGrid_i)

    ### Add Note
    # # note_i     <- "Note: Figure scale varies by impact type"
    # note_i     <- create_fig_scale_note(ntypes=n_impTypes, nvars=n_variants)
    # doNote_i   <- !(note_i == "")
    # if(doNote_i){
    #   plotNote_i <- text_grob(note_i, face = "italic", size=10, hjust=.93)
    #   plotGrid_i <- plotGrid_i |> annotate_figure(bottom = plotNote_i)
    # } ### End if(doNote_i)

    ###### ** Return Impact Year Plots ######
    return(plotGrid_i)
  })
  ### Name the plots
  listYears0 <- listYears0 |> set_names(c_impYears)

  ###### Return ######
  ### Return the plot
  if(print_msg) message("Finished.")
  return(listYears0)
}

###### End Script ######
