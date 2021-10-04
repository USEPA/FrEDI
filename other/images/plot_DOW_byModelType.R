###### plot_DOW_byModelType ######
### This function plots degrees of warming for a particular type of model

plot_DOW_byModelType <- function(
  data,
  modelType = "GCM",
  xCol = "driverValue",
  yCol = "annual_impacts",
  ncol = 4,
  nTicks = 5,
  # title = "Impacts by Degrees of Warming",
  title = NULL,
  silent = F,
  heights = NULL,
  margins = NULL,
  marginUnit = "cm",
  theme = "bw" ### Default theme vs black and white
){
  ### Defaults
  if(is.null(xCol)){xCol <- "driverValue"}
  if(is.null(yCol)){yCol <- "annual_impacts"}
  if(is.null(nTicks)){nTicks <- 5}

  ### Whether to message
  if(is.null(silent)){silent <- F}
  print_msg <- !silent
  if(print_msg) message("Running plot_DOW_byModelType():")



  ### Plot margins
  if(is.null(margins)){marginUnit <- NULL}
  ###### Get Number of Models ######
  # modelType %>% print
  if(is.null(modelType)){modelType <- "GCM"}
  # modelType %>% print

  # title <- NULL
  default_titles <- list(GCM="Impacts by Degrees of Warming", SLR="Impacts by GMSL (cm)")
  # # default_titles %>% names %>% print
  if(is.null(title)){
    which_title <- which(tolower(names(default_titles)) == tolower(modelType))
    title <- default_titles[[which_title]]

    if(is.null(title)) title <- "Impacts by Degrees of Warming"
  }
  xTitle       <- NULL
  legend_title <- NULL
  if(modelType == "SLR"){
    xTitle       <- "GMSL (cm)"
    legend_title <- "Year"
  }

  df_x <- data %>%
    filter(model_type == modelType) %>%
    mutate(sector = sector %>% as.character) %>%
    as.data.frame

  # data %>% nrow %>% print
  # modelType %>% print
  # df_x %>% nrow %>% print

  ### Standardize column names
  df_x[,"xCol"] <- df_x[,xCol]
  df_x[,"yCol"] <- df_x[,yCol]

  ###### Get Maximum Values ######
  df_minMaxBySector <- df_x %>%
    fun_limitsByGroup(sumByCol = "yCol", silent=silent) %>%
    as.data.frame

  # df_minMaxBySector %>% print
  ### Separate out minimum and maximum values
  df_minBySector <- df_minMaxBySector %>%
    filter(summary_type=="min") %>%
    rename(min = summary_value) %>%
    select(c("sector", "min"))

  df_maxBySector <- df_minMaxBySector %>%
    filter(summary_type=="max") %>%
    rename(max = summary_value)

  # df_minBySector %>% print
  # df_maxBySector %>% print
  ### Separate out maximum values
  df_sectorInfo <- df_maxBySector %>%
    ### Add minimum values in
    left_join(df_minBySector, by = "sector") %>%
    ### Arrange the observations by annual impacts
    arrange_at(.vars = c("max"), desc) %>%
    ### Get the order of the sectors
    (function(x){
      x %>%
        mutate(sector_factor = sector %>% factor(levels=x$sector)) %>%
        mutate(sector_order  = sector_factor %>% as.numeric)
    }); rm("df_minBySector", "df_maxBySector")

  # df_sectorInfo %>% print

  ### Get number of sectors and calculate columns
  c_sectors      <- df_sectorInfo$sector
  n_sectors      <- c_sectors %>% length

  if(is.null(ncol)){ncol <- 4}
  nrow <- (n_sectors %/% ncol) + 1
  # nrow %>% print
  # df_sectorInfo$sector_order %>% unique %>% print
  ### Also figure out sector positions in the list of plots
  df_sectorInfo <- df_sectorInfo %>%
    mutate(
      # plotRow = ((sector_order - 1) %/% nrow) + 1,
      # plotCol = ((sector_order - 1) %% ncol) + 1
      plotRow = ((sector_order - 1) %/% ncol) + 1,
      plotCol = ((sector_order - 1) %% nrow) + 1
    )
  # df_sectorInfo %>% print
  ### Get maximum and minimum values by plot row and combine
  df_minByRow <- df_sectorInfo %>%
    group_by(plotRow) %>%
    summarize_at(.vars=c("min"), min)

  df_maxByRow <- df_sectorInfo %>%
    group_by(plotRow) %>%
    summarize_at(.vars=c("max"), max)

  df_minMax   <- df_minByRow %>%
    left_join(df_maxByRow, by=c("plotRow")); rm("df_minByRow", "df_maxByRow")

  df_minMax_gather <- df_minMax %>%
    gather(key="summary_type", value = "summary_value", -c("plotRow"))
  # df_minMax_gather$summary_value %>% print

  x_scale_info <- df_x %>% mutate(sector=c_sectors[1]) %>% fun_getScale(scaleCol = "xCol")


  x_p10       <- x_scale_info$power1000 * 3
  # x_p10 %>% print
  x_denom     <- 10^x_p10
  # x_denom %>% print
  x_breaks    <- x_scale_info$breaks / x_denom
  x_limits    <- x_scale_info$limits / x_denom

  # x_breaks %>% print

  ### Get maximum and minimum values by plot row
  plotList_x    <- 1:(n_sectors + 1) %>%
    lapply(function(i){
      j             <- ifelse(i == 1, 1, i - 1) #; j %>% print
      sector_i      <- c_sectors[j] %>% as.character#; sector_i %>% print

      info_sector_i <- df_sectorInfo %>% filter(as.character(sector)==sector_i) %>% as.data.frame
      df_sector_i   <- df_x  %>% filter(as.character(sector)==sector_i) %>% as.data.frame

      index_i       <- info_sector_i$sector_order[1]
      row_i         <- info_sector_i$plotRow[1]
      col_i         <- info_sector_i$plotCol[1]

      # row_i %>% print
      # df_sectorInfo$plotRow %>% unique %>% print
      # y_breaks_info_i <- df_minMax_gather %>%
      y_breaks_info_i <- df_sectorInfo %>%
        filter(plotRow == row_i)
      # y_breaks_info_i$sector %>% unique %>% print
      # y_breaks_info_i <- df_sectorInfo %>%
      #   filter(plotRow == row_i) %>%
      y_breaks_info_i <- y_breaks_info_i %>%
        mutate(sector = sector %>% as.character) %>%
        mutate(sector = sector_i) %>%
        select(-c("summary_type")) %>%
        gather(key="summary_type", value="summary_value", c("min", "max")) %>%
        as.data.frame %>%
        fun_getScale(scaleCol = "summary_value")



      y_p10_i       <- y_breaks_info_i$power1000 * 3
      y_denom_i     <- 10^y_p10_i
      y_breaks_i    <- y_breaks_info_i$breaks / y_denom_i
      y_limits_i    <- y_breaks_info_i$limits / y_denom_i

      ### Create the plot
      # df_sector_i %>% names %>% print
      plot_i        <- df_sector_i %>%
        mutate(xCol = xCol / x_denom, yCol = yCol / y_denom_i) %>%
        plot_DOW_bySector(
          sector    = sector_i,
          plotIndex = i,
          plotCol   = col_i,
          xCol      = "xCol",
          yCol      = "yCol",
          xBreaks   = x_breaks,
          yBreaks   = y_breaks_i,
          xLimits   = x_limits,
          yLimits   = y_limits_i,
          xtitle    = xTitle,
          legend_title = legend_title,
          title     = title,
          silent    = silent,
          theme     = theme
        )

      ### Adjust plot spacing
      if(!is.null(margins)){
        plot_i        <- plot_i + theme(plot.margin = unit(margins, marginUnit))
      }

      ### Return the plots
      return(plot_i)
    }) %>%
    ### Name the plots
    (function(x){
      # x %>% length %>% print
      new_x_names <- c("Reference", c_sectors)
      names(x)    <- new_x_names
      return(x)
    })


  ### Separate reference plot from other plots
  is_ref     <- names(plotList_x)=="Reference"
  ref_plot   <- plotList_x[is_ref]
  plot_list1 <- plotList_x[!is_ref]

  # return(plotList_x)

  ### Get plot elements
  plot_spacer   <- (ref_plot[[1]] %>% cowplot::get_plot_component(pattern="spacer", return_all=T))[[1]] %>% (function(x){ggplotify::as.grob(x)})
  plot_xTitle   <- (ref_plot[[1]] %>% cowplot::get_plot_component(pattern="xlab-b", return_all=F)) %>% (function(x){ggplotify::as.grob(x)})
  plot_yTitle   <- (ref_plot[[1]] %>% cowplot::get_plot_component(pattern="ylab-l", return_all=F)) %>% (function(x){ggplotify::as.grob(x)})
  plot_title    <-  ref_plot[[1]] %>% cowplot::get_title()
  plot_legend   <-  ref_plot[[1]] %>% cowplot::get_legend()


  ### Arrange the other plots in a grid
  main_grid <- plot_grid(
    plotlist      = plot_list1,
    ncol          = ncol,
    align         = "h"
  )

  # ### Arrange plot grid and elements
  row_0     <- plot_grid(plot_title)
  row_1     <- plot_grid(plot_yTitle, main_grid, rel_widths = c(1, 10), nrow = 1)
  row_2     <- plot_grid(plot_spacer, plot_xTitle, rel_widths = c(1, 10), nrow = 1)
  row_3     <- plot_grid(plot_legend)

  ### Set heights
  h_main      <- 6*nrow/4
  def_heights <- c(0.4, h_main, 0.25, 0.75)*nrow/4

  # heights = NULL
  if(!is.null(heights)){
    plot_heights <- heights
  } else{
    plot_heights <- def_heights
  }

  main_plot_list <- list(row_0, row_1, row_2, row_3)

  # return(main_plot_list)
  main_plot <- plot_grid(
    plotlist = main_plot_list,
    ncol = 1,
    rel_heights = plot_heights
  )

  if(print_msg) message("Finished.")
  return(main_plot)

}
