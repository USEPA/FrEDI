###### plot_DOW_byImpactType ######
### This function plots degrees of warming by sector, variant, impact year, and type
plot_DOW_byImpactTypes <- function(
    data,
    sector,
    xCol      = "driverValue",
    yCol      = "annual_impacts",
    modelType = "GCM",
    nTicks    = 5,
    silent    = TRUE,
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
  if(print_msg){ "Running plot_DOW_byImpactType()..." |> message()}
  ### Model Type
  # modelType %>% print
  do_gcm    <- "gcm" %in% (modelType |> tolower())
  do_slr    <- "slr" %in% (modelType |> tolower())
  # modelType |> print(); do_gcm |> print(); do_slr |> print()

  ###### Format Data ######
  ### Filter to sector and convert to data frame
  # data |> glimpse()
  # data[["sector"]] |> unique() |> print(); modelType |> print()
  sector0   <- sector; rm(sector)
  df0       <- data  ; rm(data  )
  df0       <- df0 |> filter(model_type == modelType)
  df0       <- df0 |> filter(sector     == sector0  )
  # df0 |> glimpse()

  ###### Defaults ######
  ### Defaults
  def_titles  <- list(GCM="Impacts by Degrees of Warming", SLR="Impacts by GMSL (cm)")
  def_xTitles <- list(GCM=expression("Degrees of Warming (°C)"), SLR="GMSL (cm)")
  def_lgdLbls <- list(GCM="Model", SLR="Scenario")
  def_margins <- list(GCM=c(0, 0, .15, 0), SLR=c(0, .2, .15, 0))
  ### Defaults: Default Heights Below
  def_title   <- do_gcm |> ifelse(def_titles [["GCM"]], def_titles [["SLR"]])
  def_xTitle  <- do_gcm |> ifelse(def_xTitles[["GCM"]], def_xTitles[["SLR"]])
  def_margin  <- do_gcm |> ifelse(def_margins[["GCM"]], def_margins[["SLR"]])
  def_lgdLbl  <- do_gcm |> ifelse(def_lgdLbls[["GCM"]], def_lgdLbls[["SLR"]])
  def_lgdPos  <- "top"
  def_yTitle  <- "Impacts ($2015)"
  def_mUnit   <- "cm"
  def_theme   <- NULL
  def_nameBrk <- 18
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
  nameBrk     <- options[["nameBreak" ]]
  # xTitle |> print()
  ### Plot options
  hasTitle    <- !(is.null(title0 ))
  hasXTitle   <- !(is.null(xTitle ))
  hasYTitle   <- !(is.null(yTitle ))
  hasLgdLbl   <- !(is.null(lgdLbl ))
  hasLgdPos   <- !(is.null(lgdPos ))
  hasHeights  <- !(is.null(heights))
  hasMargins  <- !(is.null(margins))
  hasMUnits   <- !(is.null(mUnit  ))
  hasTheme    <- !(is.null(theme0 ))
  hasNameBrk  <- !(is.null(nameBrk))
  ### Values: Height Values Below
  if(!hasTitle  ){title0  <- def_title  }
  if(!hasXTitle ){xTitle  <- def_xTitle }
  if(!hasYTitle ){yTitle  <- def_yTitle }
  if(!hasLgdLbl ){lgdLbl  <- def_lgdLbl }
  if(!hasMargins){margins <- def_margin }
  if(!hasMUnits ){mUnit   <- def_mUnit  }
  if(!hasTheme  ){theme0  <- def_theme  }
  if(!hasNameBrk){nameBrk <- def_nameBrk}
  # title0 |> print(); def_xTitle |> print()
  # xTitle |> print()
  ### Update plot options
  plotOpts0    <- list(
    title      = title0,
    xTitle     = xTitle,
    yTitle     = yTitle,
    lgdTitle   = lgdLbl,
    margins    = margins,
    marginUnit = mUnit,
    theme      = theme0
  )

  ###### Get Sector Info ######
  infoList0     <- df0 |> get_sector_plotInfo(yCol=yCol, byType=TRUE, silent=silent)
  df_info       <- infoList0[["sectorInfo"]]
  df_minMax     <- infoList0[["minMax"    ]]
  nRow          <- infoList0[["nRow"      ]]
  nCol          <- infoList0[["nCol"      ]]
  ### Unique values
  c_sectors     <- infoList0[["cSectors" ]]
  c_impYears    <- infoList0[["cImpYears"]]
  c_variants    <- infoList0[["cVariants"]]
  c_impTypes    <- infoList0[["cImpTypes"]]
  c_models      <- df0[["model"]] |> unique()
  ### Numbers
  n_sectors     <- c_sectors  |> length()
  n_impYears    <- c_impYears |> length()
  n_variants    <- c_variants |> length()
  n_impTypes    <- c_impTypes |> length()
  n_models      <- c_models   |> length()
  # c_impYears |> print(); c_impTypes |> print(); c_variants |> print()
  # n_impYears |> print(); n_impTypes |> print(); n_variants |> print()

  ###### Factor Model ######
  df0 <- df0 |> mutate(model = model |> factor(levels=c_models))

  ###### Plot Title Info ######
  ### Default for now
  # x_denom <- y_denom <- 1

  ###### Get X Breaks ######
  if(xCol == "year"){
    x_limits <- c(2010, 2090)
    x_breaks <- seq(x_limits[1] - 10, x_limits[2] + 10, by = 20)
    x_denom  <- 1
    x_info   <- NULL
    # x_info   <- list()
    # x_info[["denom" ]] <- x_denom
    # x_info[["breaks"]] <- x_breaks
    # x_info[["limits"]] <- x_limits
  } ### End if(xCol == "year")
  else              {
    x_info     <- df0 |> get_colScale(col0=xCol, nTicks = 5)
    x_scale    <- x_info[["scale" ]]
    x_p10      <- x_info[["p10"   ]]
    x_denom    <- x_info[["denom" ]]
    x_breaks   <- x_info[["breaks"]]
    x_limits   <- x_info[["limits"]]
  } ### End else(xCol == "year")

  # ###### Format Sector Names ######
  # refSectors <- df0[["sector"]] |> unique()
  # newSectors <- refSectors |> format_sectorNames(thresh0 = nameBrk)
  # df0        <- df0 |> mutate(sector = sector |> factor(levels=refSectors, labels=newSectors))

  ###### Reference Plot ######
  ### Reference plots
  refPlot0   <- df0 |> plot_DOW_byImpactType(
    sector0   = sector0,
    impYear0  = c_impYears[1],
    impType0  = c_impTypes[1],
    variant0  = c_variants[1],
    infoList0 = infoList0, ### Dataframe with sector info...output from get_sector_plotInfo
    xCol      = xCol,   ### X-Column,
    yCol      = yCol,   ### Y-Column,
    xInfo     = x_info, ### xScale...outputs of get_colScale
    refPlot   = TRUE, ### Whether to do a ref plot
    silent    = silent,
    options   = plotOpts0
  )
  # refPlot0 |> print()

  ### Add guide to legend
  lgdCols   <- case_when(
    n_variants <= 1 ~ 2 ,
    n_variants == 2 ~ 3,
    .default = 4
  )
  # lgdCols |> print()
  refPlot0  <- refPlot0 + guides(color=guide_legend(ncol=lgdCols))

  ###### Common Plot Elements ######
  spacer0   <- ggplot() + theme_void()
  legend0   <- refPlot0 |> ggpubr::get_legend()
  # # "got here..." |> print()
  grobLgd0  <- ggarrange(plotlist=list(legend=legend0))

  ###### Create Plot List ######
  ### Iterate over Impact Years
  listYears0 <- c_impYears |> map(function(impYear_i){
    listTypes_i <- c_impTypes |> map(function(impType_j){
      ### Figure out min/max across all variants for an impact type to get the y-scale
      listVars_j <- c_variants |> map(function(variant_k){
        ###### Create the plot ######
        plot_k <- df0 |> plot_DOW_byImpactType(
          sector0   = sector0,
          impYear0  = impYear_i,
          impType0  = impType_j,
          variant0  = variant_k,
          infoList0 = infoList0, ### Dataframe with sector info...output from get_sector_plotInfo
          xCol      = xCol,   ### X-Column,
          yCol      = yCol,   ### Y-Column,
          xInfo     = x_info, ### xScale...outputs of get_colScale
          refPlot   = FALSE, ### Whether to do a ref plot
          silent    = silent,
          options   = plotOpts0
        )

        ### White out impact type label
        plot_k <- plot_k + theme(plot.title = element_text(color="white"))
        ### Add spacer to the right
        list_k <- list(plot=plot_k, spacer1=spacer0)
        grid_k <- ggarrange(plotlist=list_k, nrow=1, ncol=2, common.legend=T, legend="none", widths =c(1, 0.1))
        ### Add spacer to the left
        list_k <- list(spacer1=spacer0, plot=grid_k)
        grid_k <- ggarrange(plotlist=list_k, nrow=1, ncol=2, common.legend=T, legend="none", widths =c(0.1, 1))
        # "got here..." |> print()
        ### Return plot
        return(grid_k)
      })

      ### Name the plots
      listVars_j  <- listVars_j |> addListNames(c_variants)
      # return(listVars_j)
      # "got here1..." |> print()

      ### Arrange the other plots in a grid
      plotGrid_j <- ggarrange(plotlist=listVars_j, nrow=1, ncol=nCol, common.legend=T, legend="none")
      # return(plotGrid_j)

      ###### Annotate Plots ######
      ### Labels on top
      ### Longest impact type: "Acute Myocardial Infarction"
      # title_j      <- "Impact Type: " |> paste0(impType_j)
      # impTitle_j  <- impType_j
      typeTitle_j  <- "Impact Type: " |> paste0(impType_j)
      grobType_j   <- text_grob(typeTitle_j, face="italic", size=11)
      plotList_j   <- list(spacer1=spacer0, plots=plotGrid_j, spacer2=spacer0)
      plotGrid_j   <- ggarrange(plotlist=plotList_j, nrow=3, ncol=1, common.legend=T, legend="none", heights=c(0.01, 1, 0.01))
      plotGrid_j   <- plotGrid_j |> annotate_figure(top=grobType_j)

      ### Return
      return(plotGrid_j)
    })

    ### Name the plots
    # listTypes_i |> length() |> print(); c_impTypes |> print()
    listTypes_i <- listTypes_i |> addListNames(c_impTypes)
    # "got here3..." |> print()
    # return(listTypes_i)

    ### Arrange plot list
    plotGrid_i   <- ggarrange(plotlist=listTypes_i, ncol=1, nrow=nRow, common.legend=T, legend="none")

    ### Add spacer to the top
    # "got here2..." |> print()
    yearTitle_i <- "Impact Year: " |> paste0(impYear_i)
    grobYear_i  <- text_grob(yearTitle_i, face="plain", size=13)
    plotList_i  <- list(spacer1=spacer0, plot=plotGrid_i)
    plotGrid_i  <- ggarrange(plotlist=plotList_i, nrow=2, ncol=1, common.legend=T, legend="none", heights=c(0.01, 1))
    plotGrid_i  <- plotGrid_i |> annotate_figure(top=grobYear_i)

    ### Add Plot Title & Y Title
    # plotList_i <- list(spacer1=spacer0, plot=plotGrid_i, legend=legend0)
    plotList_i <- list(spacer1=spacer0, plot=plotGrid_i, legend=grobLgd0)
    # plotGrid_i <- ggarrange(plotlist=plotList_i, nrow=2, ncol=1, common.legend=T, legend="none", heights=c(0.1, n_impTypes, 0.25))
    plotGrid_i <- ggarrange(plotlist=plotList_i, nrow=3, ncol=1, legend="none", heights=c(0.01, n_impTypes, 0.2))
    title0_i   <- sector0
    grobTit_i  <- text_grob(title0_i, color="black", size=14, face="bold", hjust=0.5)
    plotYTit_i <- text_grob(yTitle, color = "black", rot  = 90)
    plotGrid_i <- plotGrid_i |> annotate_figure(top=grobTit_i, left=plotYTit_i)
    # "got here4..." |> print()
    # return(plotGrid_i)

    ### Add Note
    # note_i     <- "Note: Figure scale varies by impact type"
    note_i     <- create_fig_scale_note(ntypes=n_impTypes, nvars=n_variants)
    doNote_i   <- !(note_i == "")
    if(doNote_i){
      plotNote_i <- text_grob(note_i, face = "italic", size=10, hjust=.93)
      plotGrid_i <- plotGrid_i |> annotate_figure(bottom = plotNote_i)
    }
    ###### Return Impact Type Plot ######
    return(plotGrid_i)
  })
  ### Name the plots
  listYears0 <- listYears0 |> addListNames(c_impYears)

  ###### Return ######
  ### Return the plot
  if(print_msg) message("Finished.")
  return(listYears0)
}
