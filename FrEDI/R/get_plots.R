###### Documentation ######
#' Create plots of FrEDI outputs
#'
#' @description
#' `This function creates plots for the summarized FrEDI outputs. [FrEDI::get_plots()] returns a list with heatmaps for model types present in the data (GCMs and SLR scenarios) and ribbon plots, by sector.
#'
#' @param data Data frame of summarized outputs produced by [FrEDI::run_fredi()]. Do not change column names of the [FrEDI::run_fredi()] output before running [FrEDI::get_plots()].
#' @param column A character string indicating the name of the column in the data for which to create plots.
#' @param undiscounted A `TRUE/FALSE` value indicating whether the values in column represent undiscounted values or discounted values (i.e., present values). Defaults to `undiscounted=TRUE`.
#' @param plotTypes Character string or character vector indicating which types of plots to produce. Options are `c("heatmaps", "ribbon", "all")`. Set `plotTypes="all"` (default) to produce both types of plots.
#' @param save A `TRUE/FALSE` value indicating whether to save results. If a directory value is supplied (i.e., `!is.null(directory)`), defaults to `save=TRUE`. Otherwise, default is `save=FALSE`.
#' @param directory A character string indicating the location of a directory in which to save the plot objects. No default (i.e., `directory=NULL`).
#' @param groupVars A character vector indicating columns to use for grouping. Defaults to `groupVars=c("sector", "variant")`
#'
#' @details
#' This function creates plots for the summarized FrEDI outputs. [FrEDI::get_plots()] returns a list with heatmaps for model types present in the data (GCMs and SLR scenarios) and annual results for all sectors and variants. Results from FrEDI must be summed across impact types before using [FrEDI::get_plots()] (use [FrEDI::run_fredi()] with the defaults or run `aggregate_impacts(aggLevels="impactType")` on the output from [FrEDI::run_fredi()]).
#'
#' By default, [FrEDI::get_plots()] plots results from the `"annual_impacts"` column (or users can specify a column name in the data with `column`).
#'
#' The argument `undiscounted` is used by [FrEDI::get_plots()] for plot labels and in file and directory names for saving results.
#'
#' [FrEDI::get_plots()] produces heatmaps (`plotTypes="heatmaps"`) for the outputs of FrEDI and/or plots the average value and range of impacts as a time series (`plotTypes="ribbon"`) for each sector-variant-region combination. Users can specify which plot types to produce by setting `plotTypes`. Set `plotTypes="all"` (default) to produce both heat maps and annual results or specify a single type (`plotTypes="heatmaps"` and `plotTypes="ribbon"`, respectively).
#'
#' The heatmaps display the numeric values in the specified column (e.g., `"annual_impacts"`) as a grid of colored pixels. Each row in the grid corresponds to a sector-variant combination (e.g., "Coastal Properties, No Adaptation"), while columns in the grid correspond to years. In other words, the heatmaps display the relative intensity of the impacts of a sector and variant compared to others. The colors in the heatmap are a gradient ranging from dark blue (impacts with values below zero) to dark red (impacts with values above zero), with a midpoint at zero (missing values appear as grey pixels). The scale of the gradient is determined from the underlying data, with the darkest points corresponding to the minimum and maximum values.
#'
#' If temperature-driven (GCM sectors) and SLR-driven (SLR sectors) sectors are both present in the data, [FrEDI::get_plots()] will produce a separate heatmap for each. Each heatmap displays panels for each region (stacked vertically) and underlying models (organized horizontally).
#'
#' Setting (`plotTypes="ribbon"`) plots the annual impacts as time series. For temperature-driven sectors, the model average is plotted as a line and the range of model values (minimum and maximum) are plotted as a ribbon plot. For the SLR-driven sectors, the interpolated impacts are plotted as a line. For sectors with multiple variants, impacts for individual variants are displayed in separate panels (organized horizontally).
#'
#' If `save=TRUE` and the user supplies a path to a directory (i.e., `!is.null(directory)`), [FrEDI::get_plots()] will try to save results to the specified directory. Separate directories are created within the specified directory for heatmaps and ribbon plots.
#'
#' @return
#' [FrEDI::get_plots()] returns a nested list of plots:
#'
#' \tabular{ll}{
#' \strong{List Index} \tab \strong{Description} \cr
#' `heatmaps` \tab List of heatmaps with list elements for all unique values for model types present in data (i.e., `"GCM"` for results calculated with temperature as the driver value and `"SLR"` for results calculated with GMSL as the driver value.\cr
#' `ribbon` \tab List of lists of annual results. List of annual plots contains a list of sectors. Each sector contains a list of nested regional plots (`regional`, with all regions except national results and `national`, for national results). \cr
#' }
#'
#'
#' @examples
#' ### Create temperature binning scenario
#' df_tempExOut <- get_fredi(aggLevels="none", pv=TRUE, silent=TRUE)
#'
#' ### Aggregate temperature binning summary across multiple columns
#' agg_tempExOut <- df_tempExOut |> aggregate_impacts(columns=c("annual_impacts", "discounted_impacts"))
#'
#' ### Create list of plots for aggregated results
#' agg_plotList <- agg_tempExOut |> get_plots()
#'
#' ### Create list of heatmaps for regional values only:
#' reg_plotList <- agg_tempExOut |> filter(region!="National Total") |> get_plots(plotTypes="heatmaps")
#'
#' ### Create list of annual plots for national values only:
#' nation_plotList <- agg_tempExOut |> filter(region=="National Total") |> get_plots(plotTypes="annual")

#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'
#' @export
#' @md
#'
###### get_plots ######
### This function creates a list of plots from the temperature binning output.
### This function assumes that results have already been aggregated.
get_plots <- function(
  data         = NULL,
  column       = "annual_impacts", ### Column for annual impacts
  undiscounted = TRUE, ### True or false, affects labels
  plotTypes    = "all", ### Types of plots,
  save         = FALSE, ### Whether to save results
  directory    = NULL, ### Path to base image directory
  groupVars    = c("sector", "variant")
  # column       = NULL, ### Column for annual impacts
  # undiscounted = NULL, ### True or false, affects labels
  # plotTypes    = NULL, ### Types of plots,
  # save         = NULL, ### Whether to save results
  # directory    = NULL, ### Path to base image directory
  # dpi          = NULL, ### Image resolution, default = 150
  # groupVars    = c("sector", "variant")
){
  ###### Plot Types ######
  ### Also accepts "all"
  def_plotTypes <- c("heatmaps", "ribbon")
  plotTypesList <- def_plotTypes
  ### Lower case
  if(is.null(plotTypes)){
    plotTypesList <- def_plotTypes
  } else{
    plotTypesList <- plotTypes |> tolower()
    if("all" %in% plotTypesList){
      plotTypesList <- def_plotTypes
    } else if("none" %in% plotTypesList){
      if("all" %in% plotTypesList){
        plotTypesList <- c()
      }
    }
  }
  ### Whether to do heatmaps or annual
  doHeat <- "heatmaps" %in% plotTypesList
  doAnn  <- "ribbon"   %in% plotTypesList

  ###### Load Config File ######
  ### Assign data objects to objects in this namespace
  for(i in 1:length(fredi_config)){
    assign(names(fredi_config)[i], fredi_config[[i]])
  } ### End iterate over i
  for(i in 1:length(rDataList     )) assign(names(rDataList)[i], rDataList[[i]])

  ###### Columns and renaming ######
  ### Default for column is annual impacts
  # column <- ifelse(is.null(column), "annual_impacts", column)
  default_column <- "annual_impacts"
  if(is.null(column)){
    column <- default_column
  }
  ### Keep track of the data names, filter to the standardized data names, then add a dummy column for the column to plot
  data   <- data |>
    (function(y){
      y$valueColumn <- y[,column]
      return(y)
    })()

  ###### Parameter Type ######
  ### Whether results are discounted or not
  undiscounted <- ifelse(is.null(undiscounted), T, undiscounted)

  ###### Save Behavior ######
  ### Default for save is false unless
  if(is.null(save)){
    save <- ifelse(is.null(directory), F, T)
  }

  ###### Subdirectory names ######
  ### Names of directory as uppercase/lowercase
  discount_uc   <- ifelse(undiscounted, "Undiscounted", "Discounted")
  discount_lc   <- discount_uc |> tolower()

  message("In get_plots():")
  ###### Prepare directories #####
  ### Create directories if they don't exist
  if(save & !is.null(directory)){
    ### Directory names for parent image directory, then heatmaps and annual
    ### Try to create directories for images
    # results_dir       <- directory   |> file.path(img_suffix, sep="/")
    results_dir       <- directory
    heat_dir          <- results_dir |> file.path("heatmaps", sep="/")
    ann_dir           <- results_dir |> file.path("ribbon", sep="/")

    ### Check if the directories exist:
    dir_exists        <- directory   |> dir.exists()
    resultsDir_exists <- results_dir |> dir.exists()
    heatDir_exists    <- heat_dir    |> dir.exists()
    ribDir_exists     <- ann_dir     |> dir.exists()

    ### If the directory doesn't exist, try to create the directory and image directory and then check again to see if it exists
    if(!dir_exists){
      message("Creating specified directory to save results...")
      ### Try to create all directories
      try_dir             <- try(directory   |> dir.create(), silent=T)
      try_resultsDir      <- try(results_dir |> dir.create(), silent=T)
      if(doHeat) try_heat <- try(heat_dir    |> dir.create(), silent=T)
      if(doAnn)  try_rib  <- try(ann_dir     |> dir.create(), silent=T)

      ### then check if they exist
      dir_exists          <- directory   |> dir.exists()
      resultsDir_exists   <- results_dir |> dir.exists()
      heatDir_exists      <- heat_dir    |> dir.exists()
      ribDir_exists       <- ann_dir     |> dir.exists()
    } else{
      if(!resultsDir_exists){
        ### Try to create directories
        try_resultsDir      <- try(results_dir |> dir.create(), silent=T)
        if(doHeat) try_heat <- try(heat_dir    |> dir.create(), silent=T)
        if(doAnn)  try_rib  <- try(ann_dir     |> dir.create(), silent=T)

        ### then check if they exist
        resultsDir_exists   <- results_dir |> dir.exists()
        heatDir_exists      <- heat_dir    |> dir.exists()
        ribDir_exists       <- ann_dir     |> dir.exists()
      } else{
        ### Check for the heatmaps directory
        if(!heatDir_exists & doHeat){
          try_heat       <- try(heat_dir |> dir.create(), silent=T)
          heatDir_exists <- heat_dir |> dir.exists()
        } ### End if heatmap directory doesn't exist

        ### Check for the ribbon directory
        if(!ribDir_exists & doAnn){
          try_rib        <- try(ann_dir |> dir.create(), silent=T)
          ribDir_exists  <- ann_dir |> dir.exists()
        } ### End if ribbon directory doesn't exist
      } ### End if results directory doesn't exist
    }  ### End if directory doesn't exist
  } ### End if directory is not null

  ###### Data Information ######
  ### Sectors, models, regions, combinations
  sectorsList    <- data$sector |> unique()
  numSectors     <- sectorsList |> length()

  modelTypesList <- data$model_type |> unique()
  numModels      <- modelTypesList |> length()

  regionsList    <- data$region |> unique()
  numRegions     <- regionsList |> length()

  ### Unique sectors, variants, impact types
  ### impact types
  impactTypes    <- data$impactType |> unique()
  numImpactTypes <- impactTypes |> length()

  ### Unique Groups
  if(is.null(groupVars)){groupVars <- c("sector", "variant")}
  # groupVars      <- c("sector", "variant")
  # groupVarLabels <- c("Sector", "Variant")
  groupVarLabels <- stringr::str_to_title(groupVars)
  if(numImpactTypes > 1){
    groupVars      <- c(groupVars, "impactType")
    groupVarLabels <- c(groupVarLabels, "Impact Type")
  }


  ###### Image Constants ######
  getPlots_constants <- fredi_config$get_plots
  for(i in 1:length(getPlots_constants)){
    assign(names(getPlots_constants)[i], getPlots_constants[[i]])
  }
  # ### Defaults for regional images
  ### DPI not needed for vector graphics
  def_img_device  <- "pdf"
  def_img_unit    <- "in"

  ### Heights and Widths
  ### Heat Maps: ~28 for GCM, ~10 for SLR
  # def_heat_width   <- 16
  # base_heat_ht_per <- 0.13 ### Per region and group
  base_heat_ht     <- 3 + (numRegions - 2) * base_heat_ht_per
  #
  # ### Annual damage images, by region and sector
  # ### Different heights for regions, national (originally 6 for regions, 4.5 for national).
  base_rib_ht         <- 4
  base_rib_width_per  <- 1.5 ### Per variant
  base_rib_per        <- 0.75 ### Per region
  base_rib_width      <- 4

  ###### Create Results List ######
  list_plotOuts     <- list()

  ###### Plot Labels ######
  ### Base label for impacts and unit
  base_plot_label <- discount_uc |> paste("Annual Impacts")
  base_unitStart  <- "("
  base_unitEnd    <- "2015$)"
  ylab_unitEnd    <- base_unitEnd

  ###### Get Heat Maps ######
  if(doHeat){
    message("\t", "Creating heat maps of impacts...")
    ### Adjust data: remove national total for heatmaps
    ### Non NA values
    # dataHeat         <- data |> filter(region!="National Total")
    dataHeat         <- data
    non_na_heat      <- which(!is.na(dataHeat[,column])) |> length()

    ###### Get Limits ######
    if(non_na_heat>0){
      heat_scaleInfo <- dataHeat |> fun_getScale(scaleCol = "valueColumn")
      heat_power1000 <- heat_scaleInfo$power1000

      which_globUnit <- which(df_units$log10mod3<=heat_power1000)

      if(length(which_globUnit) > 0){
        ### Scale
        globMax_unit   <- df_units |> filter(log10mod3==max(df_units$log10mod3[which_globUnit]))
        scale_global   <- globMax_unit$unitValue |> max()
        unit_global    <- globMax_unit$unitLabel[which(globMax_unit$unitValue==scale_global)]
        ### Adjust data
        dataHeat <- dataHeat |> mutate(valueColumn = valueColumn/scale_global)
        ### Adjust unit scale label
        if(unit_global!=""){
          ylab_unitEnd <- paste0(unit_global, ", ", base_unitEnd)
        }
      }
    }

    c_heatMapYears     <- seq(minYear, maxYear, by = 5)

    ##### Y Label
    impactLab_heat       <- base_plot_label |> paste(base_unitStart) |> paste0(ylab_unitEnd)
    # yLab_heat            <- groupVarLabels |> paste(base_unitStart, collapse=", ")
    groupLab_heat        <- groupVarLabels |> paste(collapse = ", " )
    yLab_heat            <- impactLab_heat |> paste0(", by ", groupLab_heat)

    ### Iterate over model types
    for(modelType_i in modelTypesList){
      ### Data for Model
      # data_model_i       <- data |> filter(model_type == modelType_i)

      # data_model_i <- data |> filter(model_type == modelType_i)
      data_model_i <- dataHeat |> filter(model_type == modelType_i)

      ### Refactor model names
      if(tolower(modelType_i)=="slr"){
        # refModelList <- c(paste(c(0, 30, seq(50, 250, by=50)), "cm"), "Interpolation")
        # data_model_i       <- data_model_i |> mutate(model = model |> factor(levels=refModelList))
        refModelList   <- c("Interpolation")
        data_model_i   <- data_model_i |> mutate(model = model |> as.character() |> factor(levels=refModelList))
        # data_model_i |> filter(!is.na(valueColumn)) |> filter(year > 2090) |> nrow() |> print()
        # (data_model_i |> filter(!is.na(valueColumn)) |> filter(year > 2090))$valueColumn |> range() |> print()
      } else{
        data_model_i    <- data_model_i |> mutate( model = model |> as.character())
        models_i        <- data_model_i$model |> as.character() |> unique()
        is_aveCol_i     <- (models_i == "Average" | models_i == "Model Average")
        ### Put average columns last
        modelLevels_i   <- models_i[which(!is_aveCol_i)] |> c(models_i[which(is_aveCol_i)])
        # models_i |> print(); modelLevels_i |> print()
        data_model_i    <- data_model_i |> mutate(model = model |> as.character() |> factor(levels=modelLevels_i))

        # data_model_i |> filter(!is.na(valueColumn)) |> filter(model%in% c("GCM Ensemble",  "MRI-CGCM3")) |> nrow() |> print()
        # (data_model_i |> filter(!is.na(valueColumn)) |> filter(model%in% c("GCM Ensemble",  "MRI-CGCM3")))$valueColumn |> range(na.rm=T) |> print()
      }



      ### Unique sectors, variants, impact types
      unique_combos_heat  <- data_model_i |> group_by_at(.vars = groupVars) |> summarize(n=n())
      numCombos_heat      <- unique_combos_heat |> nrow()
      data_model_i$group_name     <- (1:nrow(data_model_i)) |> lapply(function(j){
        # data_model_i[j, groupVars] |> paste(collapse="\n\t")
        data_model_i[j, groupVars] |> paste(collapse=", ")
      }) |> unlist()
      rm("unique_combos_heat")
      # numCombos_heat |> print()

      ### Height Info
      fig_heat_ht_model  <- base_heat_ht + base_heat_ht_per * numCombos_heat * numRegions
      assign(paste("fig_heat_ht", tolower(modelType_i), sep="_"), fig_heat_ht_model)
      # fig_heat_ht_model |> print()


      ### Model Title
      # p_title <- paste("Heat Map of", base_plot_label, "(by", modelType_i, "and NCA Region)")
      p_title_i_add <- NULL; if(modelType_i=="SLR"){p_title_i_add <- "Scenario"}
      p_title_i_obj <- c("Heat Map of", base_plot_label, "(by", modelType_i, p_title_i_add, "and NCA Region)")
      p_title       <- paste(p_title_i_obj, collapse = " ")

      ### Model Plot
      p_model <- data_model_i |>
        # arrange(desc(group_name)) |>
        # mutate(valueColumn = valueColumn / (10^3)^heat_power1000 ) |>
        # filter(year %in% seq(2010, 2090, by=5)) |>
        filter(year %in% c_heatMapYears) |>
        mutate(group_name = group_name |> factor()) |>
        mutate(group_name = group_name |> factor(levels=rev(levels(group_name)))) |>
        ggplot(., aes(x=year, y=group_name, fill=valueColumn)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(impactLab_heat, low="darkblue", high="darkred") +
        scale_x_continuous("Year", breaks=list_years_by20, limits = c(minYear, maxYear)) +
        scale_y_discrete(yLab_heat) +
        facet_grid(region~model) +
        theme(
          axis.text.x = element_text(angle = 90),
          legend.position = "bottom",
          # legend.key.width = unit(def_heat_lgd_w, "cm"),
          axis.text = element_text(size=8)
        ) +
        ggtitle(p_title)

      ### Add to outputs list
      list_plotOuts[["heatmaps"]][[modelType_i]] <- p_model
    }
    ### Remove data
    rm("dataHeat")
  }

  ###### Ribbon Plots ######
  ###### Get Model Averages
  if(doAnn){
    message("\t", "Creating ribbon plots of annual damages...")


    ###### Get Plots of Model Averages ######
    ### Create plot lists for model averages

    ###### Get Plots for Each Sector ######
    for(sector_i in sectorsList){
      # sector_i |> print()
      ### Initialize plot list and titles
      plot0_list     <- list()
      base_rib_title <- sector_i
      ylab_unitEnd_i <- base_unitEnd ### Base unit

      ### Filter to data and get number of variants
      data0_i        <- data |> filter(sector==sector_i) |>
        filter(model!="Average") |>
        filter(model!="Model Average") |>
        as.data.frame()


      model_type_i <- (data0_i$model_type |> unique())[1] ### Refactor model names
      ### Number of models
      models_i        <- data0_i$model |> unique()
      n_models_i      <- models_i |> length()

      if(tolower(model_type_i)=="slr"){
        refModelList   <- c("Interpolation")
        data0_i        <- data0_i |> mutate(model = model |> factor(levels=refModelList))
      } else{
        # models_i        <- data0_i$model |> unique()
        is_aveCol_i     <- (models_i == "Average" | models_i == "Model Average")
        ### Put average columns last
        modelLevels_i   <- models_i[which(!is_aveCol_i)] |> c(models_i[which(is_aveCol_i)])
        data0_i         <- data0_i |> mutate(model = model |> factor(levels=modelLevels_i))
      }

      ### Get model averages for undiscounted a
      ### Annual undiscounted: convert to millions and then get averages
      if(tolower(model_type_i)=="gcm"){
      # if(n_models_i>1){
        data0_i    <- data0_i |>
          get_annual_model_stats(yVar = "valueColumn", groupCol = c(groupVars, "region", "year"))
        has_non_na_i       <- (data0_i |> filter(!is.na(modelMax)) |> nrow()) > 0
      } else{
        has_non_na_i       <- (data0_i |> filter(!is.na(valueColumn)) |> nrow()) > 0
      }



      ### Get the maximum value, its log10 value, its floor, modulus, and remainder
      if(has_non_na_i){
        # if(n_models_i>1){
        if(tolower(model_type_i)=="gcm"){
          ann_scaleInfo <- data0_i |>
            gather(key="valueType", value="valueColumn", c("modelMin", "modelAve", "modelMax")) |>
            fun_getScale(scaleCol = "valueColumn")
        } else{
          ann_scaleInfo <- data0_i |> fun_getScale(scaleCol = "valueColumn")
        }

        ann_power1000 <- ann_scaleInfo$power1000

        ### Subset the unit data frame
        which_i_unit     <- which(df_units$log10mod3<=ann_power1000)

        ### If there is a scalar that exists, scale the data
        if(length(which_i_unit) > 0){
          max_i_unit     <- df_units |> filter(log10mod3==max(df_units$log10mod3[which_i_unit]))
          scale_i        <- max_i_unit$unitValue |> max()
          unit_i         <- max_i_unit$unitLabel[which(max_i_unit$unitValue==scale_i)]

          # ann_power1000 |> print()
          # scale_i |> print()
          ### Scale the data
          # if(n_models_i>1){
          if(tolower(model_type_i)=="gcm"){
            data0_i        <- data0_i |> mutate(modelAve = modelAve/scale_i, modelMin=modelMin/scale_i, modelMax=modelMax/scale_i)
            # data0_i        <- data0_i |> mutate_at(.vars=c("modelAve", "modelMin", "modelMax"), function(y){y/scale_i})
          } else{
            data0_i        <- data0_i |> mutate(valueColumn = valueColumn/scale_i)
          }

          ylab_unitEnd_i <- paste0(unit_i, ", ", base_unitEnd)
        }

        ### Plot label for y
        yLab_unit_i <- paste0(base_unitStart, ylab_unitEnd_i)
        yLab0_reg_i <- base_plot_label |> paste(yLab_unit_i)
        yLab0_nat_i <- base_plot_label |> paste(yLab_unit_i)


        ### Get regional plots)
        # if(n_models_i>1){
        if(tolower(model_type_i)=="gcm"){
          plot0_reg_i <- data0_i |>
            filter(region!="National Total") |>
            ggplot() +
            geom_ribbon( aes(x=year, ymin=modelMin, ymax = modelMax, fill=region), alpha=0.25) +
            geom_line(aes(x=year, y=modelAve, colour=region, linetype=model), size = 0.5, alpha=0.85)
        } else{
          plot0_reg_i <- data0_i |>
            filter(region!="National Total") |>
            ggplot() +
            geom_line(aes(x=year, y=valueColumn, colour=region, linetype=model), size = 0.5, alpha=0.85)
        }

        plot0_reg_i <- plot0_reg_i +
          # data0_i |>
          # filter(region!="National Total") |>
          #
          # ggplot() +
          # geom_ribbon( aes(x=year, ymin=modelMin, ymax = modelMax, fill=region), alpha=0.25) +
          # geom_line(aes(x=year, y=modelAve, colour=region, linetype=model), size = 0.5, alpha=0.85) +

          facet_grid(region~variant) +

          scale_x_continuous("Year", breaks = list_years_by10, limits = c(minYear, maxYear)) +
          scale_y_continuous(yLab0_reg_i) +
          scale_color_discrete("Region", guide = guide_legend(nrow=3)) +
          scale_fill_discrete("Region", guide = guide_legend(nrow=3)) +
          scale_linetype_discrete("Model") +

          theme(
            axis.text       = element_text(size = 8),
            axis.text.y     = element_text(angle = 90),
            legend.position = "bottom", legend.direction = "vertical",
            legend.text     = element_text(size=9)
          ) +

          ggtitle(base_plot_label, paste(base_rib_title, "(by NCA Region)"))

        ### Add plots to list
        list_plotOuts[["ribbon"]][[sector_i]][["regional"]] <- plot0_reg_i

        ### Filter to national, add ribbon and line
        ### Facet the plot, and scales
        if("National Total" %in% regionsList){
          if(tolower(model_type_i)=="gcm"){
            plot0_nat_i <- data0_i |> filter(region=="National Total") |>

              ggplot() +
              geom_ribbon(aes(x=year, ymin=modelMin, ymax = modelMax), fill = "grey24", alpha=0.25) +
              geom_line(aes(x=year, y=modelAve, linetype=model), size = 0.5, colour = "grey24", alpha=0.85)
          } else{
            plot0_nat_i <- data0_i |> filter(region=="National Total") |>

              ggplot() +
              geom_line(aes(x=year, y=valueColumn, linetype=model), size = 0.5, colour = "grey24", alpha=0.85)
          }

          plot0_nat_i <- plot0_nat_i +
            # data0_i |> filter(region=="National Total") |>
            #
            # ggplot() +
            # geom_ribbon(aes(x=year, ymin=modelMin, ymax = modelMax), fill = "grey24", alpha=0.25) +
            # geom_line(aes(x=year, y=modelAve, linetype=model), size = 0.5, colour = "grey24", alpha=0.85) +

            facet_grid(region~variant) +

            scale_x_continuous("Year", breaks = list_years_by10, limits = c(minYear, maxYear)) +
            scale_y_continuous(yLab0_nat_i) +
            scale_linetype_discrete("Model") +

            theme(
              axis.text       = element_text(size = 8),
              axis.text.y     = element_text(angle = 90),
              legend.position = "bottom", legend.direction = "vertical",
              legend.text     = element_text(size=9)
            ) +

            ggtitle(base_plot_label, paste(base_rib_title, "(National Total)"))

          ### Add plot to list
          list_plotOuts[["ribbon"]][[sector_i]][["national"]] <- plot0_nat_i
        }
      } ### End iteration over variants
    } ### End iteration over sectors
    ### Remove data
    # rm("data_modelStats0")
  }
  ###### Save images ######
  ### Start the conditional again
  if(save){
    ### Save images and plot list if the results directory exists
    if(resultsDir_exists){
      message("\t", "Saving results...")
      fpath_data <- results_dir |> file.path("outputPlots.RData")
      list_plotOuts |> save(file=fpath_data)

      ###### Save heat maps ######
      ### Save heatmaps if the directory exists
      if(heatDir_exists & doHeat){
        message("\t\t", "Saving heatmaps...")
        heatMaps <- list_plotOuts[["heatmaps"]] |> names()
        ### Iterate Over Model Types
        for(modelType_i in heatMaps){
          ### File name and path
          fName_i <- discount_lc |>
            paste("impacts_as_heatmap_by", modelType_i, sep="_") |>
            paste(def_img_device, sep=".")
          fPath_i <- heat_dir |> file.path(fName_i)
          ### Plot i
          plot_i  <- list_plotOuts[["heatmaps"]][[modelType_i]]
          ### Plot height
          if(!is.null(plot_i)){
            ### Plot height
            height_i <- ifelse(modelType_i=="SLR", fig_heat_ht_slr, fig_heat_ht_gcm)

            ### Save the plot if it exists
            if("ggplot" %in% class(plot_i) & !is.null(plot_i)){
              ### Try to save the file
              try_heat <- try(
                fPath_i |>
                  ggsave(
                    plot   = plot_i,
                    device = def_img_device,
                    width  = def_heat_width,
                    height = height_i,
                    units  = def_img_unit,
                    # dpi    = dpi,
                    limitsize = F
                  ),
                silent=T); #dev.off();
              ### Let the system sleep
              Sys.sleep(0.5)
            }
          }
        } ### End iterate over model types
      } ### End if heatmap exists
      ### Let system rest
      # Sys.sleep(0.1)

      ###### Save sector-specific plots ######
      ### Save ribbon plots if the directory exists
      if(ribDir_exists & doAnn){
        message("\t\t", "Saving ribbon plots...")
        sectorPlots <- list_plotOuts[["ribbon"]] |> names()
        for(sector_i in sectorPlots){
          plotList0_i <- list_plotOuts[["ribbon"]][[sector_i]]
          ### Number of variants
          num_variant_i <- (data |> filter(sector == sector_i))$variant |> unique() |> length()
          def_rib_width <- base_rib_width + base_rib_width_per * num_variant_i

          ### Regions for j
          regions_i   <- plotList0_i |> names()

          for(region_j in regions_i){
            ### Plot k
            plot_j  <- plotList0_i[[region_j]]
            if(!is.null(plot_j)){
              ### File names and shorten
              fPath_j <- sector_i |>
                paste(region_j, sep="_") |>
                paste(def_img_device, sep=".") |>
                (function(y){gsub("/", "", y)})()
              fPath_j <- ann_dir |> file.path(fPath_j) |>
                (function(k){gsub(" and ", "", k)})() |>
                (function(k){gsub("Variant", "", k)})() |>
                (function(k){gsub(" ", "", k)})()

              ### Plot height
              num_reg_i           <- ifelse(region_j=="national", 1, numRegions)
              fig_rib_ht          <- base_rib_ht + num_reg_i * base_rib_per
              fig_rib_width       <- base_rib_width + num_variant_i * base_rib_width_per

              ### Plot exists
              if("ggplot" %in% class(plot_j) & !is.null(plot_j)){
                ### Save plot
                try_j   <- try(
                  fPath_j |>
                    ggsave(plot   = plot_j,
                           device = def_img_device,
                           width  = fig_rib_width,
                           height = fig_rib_ht,
                           units  = def_img_unit,
                           limitsize = F
                    ),
                  silent=F);
              }
              ### Let the system sleep
              Sys.sleep(0.1)
            } ### End if region_j is not null
          } ### End for k in regions
          ### Sleep after each sector
          # Sys.sleep(0.5)
        } ### End for i in 1:numSectors
      }
    } else{ ### If directory doesn't exist
      message("Cannot open directory to save images...")
    } ### End if directory exists
  } ### End if save

  message("Finished.")
  ###### Return Object ######
  return(list_plotOuts)
} ### End function
