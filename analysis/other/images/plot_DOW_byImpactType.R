###### plot_DOW_byImpactType ######
### This function plots degrees of warming by sector, adaptation, impact year, and type
plot_DOW_byImpactType <- function(
  data,
  sector,
  xCol = "driverValue",
  yCol = "annual_impacts",
  title = "Impacts by Degrees of Warming",
  modelType = "GCM",
  # zero = F,
  # xBreaks = NULL,
  # yBreaks = NULL,
  # legend_title = "Model",
  # xtitle = "default",
  nTicks = 5,
  heights = NULL,
  silent = F,
  theme = "bw" ### Default theme vs black and white
){
  ###### Defaults ######
  if(is.null(xCol   )){xCol <- "driverValue"}
  if(is.null(yCol   )){yCol <- "annual_impacts"}
  if(is.null(title  )){title <- "Impacts by Degrees of Warming"}
  if(is.null(heights)){heights <- NULL }

  ###### Messaging ######
  if(is.null(silent)){silent <- F}
  print_msg <- !silent
  if(print_msg) message("Running plot_DOW_byType():")

  ###### initialize x_breaks and y_breaks ######
  # if(is.null(plotIndex)) plotIndex <- 1
  # if(is.null(plotCol)) plotCol <- 1
  if(is.null(nTicks)){nTicks <- 5}

  default_titles <- list(GCM="Impacts by Degrees of Warming", SLR="Impacts by GMSL (cm)")
  # default_titles %>% names %>% print
  if(is.null(title)){
    which_title <- which(tolower(names(default_titles)) == tolower(modelType))
    title <- default_titles[[which_title]]
    if(is.null(title)) title <- "Impacts by Degrees of Warming"
  }

  # ### Default is not to zero out in case there are negative numbers

  # xtitle <- expression("Degrees of Warming ("*degree*C*")")
  xtitle       <- NULL
  legend_title <- NULL
  if(modelType == "SLR"){
    xtitle       <- "Year"
    # xtitle       <- "GMSL (cm)"
    # xtitle       <- "GMSL (cm)"
    # legend_title <- "Year"
    legend_title <- "Sweet et al. SLR Scenario"
  }

  ### Spec for changing theme
  themeSpec <- ifelse(is.null(theme), "default", theme)

  ###### Standardize data ######
  ### Filter to sector and convert to data frame
  df_sector   <- data %>%
    filter(sector==sector) %>%
    as.data.frame

  ### Standardize columns
  # df_sector   <- df_sector %>%
  #   mutate(xCol = df_sector[,xCol]) %>%
  #   mutate(yCol = df_sector[,yCol])
  df_sector[,"xCol"]   <- df_sector[,xCol]
  df_sector[,"yCol"]   <- df_sector[,yCol]

  ###### Data Min/Max ######
  # c_sectors <- df_sector %>%
  df_minMax   <- df_sector %>%
    group_by_at(.vars=c("impactYear", "impactType", "adaptation")) %>%
    summarize_at(.vars=c("yCol"), .funs = c("min", "max"), na.rm=T)

  ###### Other Data Info ######
  ### Sector
  x_sector <- sector
  ### Impact Years
  c_years <- df_sector$impactYear %>% unique
  n_years <- c_years %>% length
  # c_years %>% print

  ###### Summary by Impact Type ######
  ### Plot Rows
  df_minMax_types <- df_minMax %>%
    # group_by_at(.vars=c("impactType", "impactYear", "adaptation")) %>%
    group_by_at(.vars=c("impactType")) %>%
    summarize_at(.vars=c("max"), max, na.rm=T) %>%
    rename(impactType_max = max) %>%
    ### Arrange the observations by maximum value
    arrange_at(.vars = c("impactType_max"), desc) %>%
    ### Get the order of the impact types
    (function(x){
      x %>%
        mutate(type_order  = impactType %>% factor(levels=x$impactType) %>% as.numeric)
    })

  c_types     <- df_minMax_types$impactType
  n_types     <- c_types %>% length
  # c_types %>% print

  ###### Summary by Adaptation ######
  ### Get number of adaptations, impact types, impact years
  ### Plot columns
  df_minMax_adapt <- df_minMax %>%
    group_by_at(.vars=c("adaptation")) %>%
    summarize_at(.vars=c("max"), max, na.rm=T) %>%
    rename(adaptType_max = max) %>%
    ### Arrange the observations by maximum value
    arrange_at(.vars = c("adaptType_max"), desc) %>%
    ### Get the order of the adaptations
    (function(x){
      x %>%
        mutate(adapt_order  = adaptation %>% factor(levels=x$adaptation) %>% as.numeric)
    })

  c_adapt     <- df_minMax_adapt$adaptation
  n_adapt     <- c_adapt %>% length
  # c_adapt %>% print

  ###### Join Summary Info to Min/Max ######
  ### Add orders to the main sector data
  df_minMax   <- df_minMax %>%
    left_join(df_minMax_adapt, by = "adaptation") %>%
    left_join(df_minMax_types, by = "impactType")


  ###### Plot Title Info ######
  ### Figure out the plot info
  ggtitle_sector  <- title

  # ytitle          <- "Impacts (Billions, $2015)"
  ytitle          <- "Impacts\n(Billions, $2015)"
  # xtitle          <- expression("Degrees of Warming ("*degree*C*")")
  legend_title    <- "Model"

  ### Default for now
  # denom_x <- denom_y <- 1


  ###### Get X Breaks ######
  if(xCol=="year"){
    x_breaks <- seq(2020, 2080, by=20)
    x_lims   <- c(2010, 2090)
    denom_x  <- 1
  } else{
  x_scaleInfo <- df_sector %>% fun_getScale(scaleCol = "xCol", nTicks = nTicks)
  x_p10       <- x_scaleInfo$power1000 * 3
  # denom_x     <- 1
  denom_x     <- 10^x_p10
  # # denom_x     <- 1
  #
  x_breaks    <- x_scaleInfo$breaks / denom_x
  x_lims      <- x_scaleInfo$limits / denom_x
  # x_scaleInfo$power10 %>% print
  # denom_x %>print

  # xCol %>% print
  # x_breaks %>% print
  # df_sector$xCol %>% range(na.rm=T) %>% print

}
  ###### Standardize column names ######
  ### Iterate over Impact Years
  plotList_years <- (1:n_years) %>%
    lapply(function(i){
      year_i      <- c_years[i]

      # paste("Year: ", year_i) %>% print
      plotList_types  <- 1:n_types %>%
        lapply(function(j){
          type_j    <- c_types[j]
          info_j    <- df_minMax %>% filter(impactType == type_j)
          ggtitle_j <- paste(x_sector, type_j, sep = ": ")

          # paste("Type: ", type_j) %>% print
          ### Figure out min/max across all adaptations for an impact type to get the y-scale
          plotList_adapt <- 1:(n_adapt + 1) %>%
            lapply(function(k){
              k1         <- ifelse(k == 1, 1, k - 1)
              adapt_k    <- c_adapt[k1]
              subtitle_k <- adapt_k

              # paste("Adaptation: ", adapt_k) %>% print
              ### Filter data
              df_k    <- df_sector %>%
                filter(impactYear == year_i) %>%
                filter(impactType == type_j) %>%
                filter(adaptation == adapt_k) %>%
                as.data.frame

              # df_k %>%
              # df_k %>% filter(!is.na(yCol)) %>% nrow %>% print
              ###### Get Y Breaks ######
              y_scaleInfo <- df_k %>% fun_getScale(scaleCol = "yCol", nTicks = nTicks)
              y_p10       <- y_scaleInfo$power1000 * 3
              denom_y     <- 10^y_p10

              y_breaks    <- y_scaleInfo$breaks / denom_y
              y_lims      <- y_scaleInfo$limits / denom_y
              ### Zero out the limits
              # y_lims %>% print
              if(y_lims[1] >= 0){
                if(y_lims[1] != 0){
                  y_lims[1] <- 0
                }
                ### Round the upper value
                y_power10 <- y_lims[2] %>% log10 %>% floor
                y_max     <- (y_lims[2] / 10^y_power10) %>% ceiling
                y_breaks  <- seq(y_lims[1], y_max, length.out=5) * 10^y_power10
              }
              else if(y_lims[2] <= 0){
                ### Zero the maximum value out
                if(y_lims[2] != 0){
                  y_lims[2] <- 0
                }
                # y_lims %>% print
                ### Round the lower value
                y_power10 <- (-y_lims[1]) %>% log10 %>% floor
                y_min     <- (y_lims[1] / 10^y_power10) %>% floor
                y_breaks  <- seq(y_min, y_lims[2], length.out=5) * 10^y_power10
              }



              ###### Create the plot ######
              plot_k  <- df_k %>%
                mutate(xCol = xCol / denom_x, yCol = yCol / denom_y) %>%
                ggplot(aes(x = xCol, y=yCol)) +

                ### Add Geoms
                geom_line(aes(color = model), alpha=0.7) +
                # geom_point(aes(color = model, shape=model), alpha=0.7) +
                geom_point(
                  data=df_k %>% filter(year %in% seq(2010, 2090, by=5)),
                  aes(color = model, shape=model), alpha=0.7) +

                ### Add Scales
                scale_color_discrete(legend_title) +
                scale_shape_discrete(legend_title)

              ###### Add themes and title ######
              plot_k    <- plot_k +
                ggtitle(ggtitle_j, subtitle_k) +
                # theme(panel.background = element_rect(fill="white")) +
                # theme(panel.grid       = element_line(color="lightgrey")) +
                # theme(axis.line        = element_line(color="darkgrey")) +
                # theme(axis.title       = element_text(size=9))
                # theme(plot.title       = element_text(hjust = 0.5, size=12)) +
                # theme(plot.subtitle    = element_text(hjust = 0.5, size=10)) +
                theme(axis.title       = element_text(size=9)) +
                theme(plot.title       = element_text(hjust = 0.5, size=14)) +
                theme(plot.subtitle    = element_text(hjust = 0.5, size=11)) +
                theme(legend.position  = "bottom", legend.direction = "horizontal")

              ###### Change theme ######

              if(themeSpec=="bw"){
                plot_k    <- plot_k +
                  theme(panel.background = element_rect(fill="white")) +
                  theme(panel.grid = element_line(color="lightgrey")) +
                  theme(axis.line = element_line(color="darkgrey"))
              }


              ###### Add scales ######
              plot_k    <- plot_k + scale_x_continuous(xtitle, breaks = x_breaks, limits = x_lims)
              plot_k    <- plot_k + scale_y_continuous(ytitle, breaks = y_breaks, limits = y_lims)
              # plot_k    <- plot_k +

              ###### If plotIndex, remove some plot elements #####
              if(k>1){
                plot_k <- plot_k +
                  theme(
                    plot.title = element_blank()
                    # plot.title = element_blank(),
                    # axis.title.x = element_blank()
                  )
              }

              ### Return plot
              return(plot_k)
            }) %>%
            ### Name the plots
            (function(x){
              # x %>% length %>% print
              new_x_names <- c("Reference", c_adapt)
              names(x)    <- new_x_names
              return(x)
            })


          ### Separate reference plot from other plots
          is_ref_adapt    <- names(plotList_adapt)=="Reference"
          ref_plot_adapt  <- plotList_adapt[is_ref_adapt]
          plot_list_adapt <- plotList_adapt[!is_ref_adapt]

          # return(plotList_adapt)

          ### Get plot elements
          plot_spacer_adapt   <- (
            ref_plot_adapt[[1]] %>% cowplot::get_plot_component(pattern="spacer", return_all=T)
          )[[1]] %>% (function(x){ggplotify::as.grob(x)})
          plot_xTitle_adapt   <- (
            ref_plot_adapt[[1]] %>% cowplot::get_plot_component(pattern="xlab-b", return_all=F)
          ) %>% (function(x){ggplotify::as.grob(x)})
          # plot_yTitle_adapt   <- (
          #   ref_plot_adapt[[1]] %>% cowplot::get_plot_component(pattern="ylab-l", return_all=F)
          # ) %>% (function(x){ggplotify::as.grob(x)})
          plot_title_adapt    <-  ref_plot_adapt[[1]] %>% cowplot::get_title()
          plot_legend_adapt   <-  ref_plot_adapt[[1]] %>% cowplot::get_legend()


          ### Arrange the other plots in a grid
          main_grid_adapt <-

            ggarrange(
              plotlist      = plot_list_adapt,
              nrow          = 1, ncol = n_adapt,
              common.legend = T,
              legend = "none",
              widths   = rep(1, n_adapt)
            )

          # ### Arrange plot grid and elements
          # row_1_adapt     <- plot_grid(plot_yTitle_adapt, main_grid_adapt, rel_widths = c(1, 10), nrow = 1)
          row_1_adapt     <- main_grid_adapt
          # row_2_adapt     <- plot_grid(plot_spacer_adapt, plot_xTitle_adapt, rel_widths = c(1, 10), nrow = 1)
          row_2_adapt     <- plot_grid(plot_spacer_adapt, nrow = 1)
          title_adapt     <- plot_grid(plot_title_adapt)


          ### Set heights
          # h_main      <- 1*n_types/2
          h_main      <- 1*n_types/2
          # def_heights_adapt <- c(0.2, 1, 0.1)
          def_heights_adapt <- c(0.2, h_main, 0.1)*n_types/2
          heights_adapt     <- def_heights_adapt

          main_plot_adapt   <- ggarrange(
            title_adapt,
            row_1_adapt,
            row_2_adapt,
            ncol = 1,
            common.legend = T,
            legend = "none",
            heights = heights_adapt
          )

          # main_plot_adapt  <- ggarrange(
          #   main_plot_adapt0,
          #   row_3_adapt,
          #   ncol = 1,
          #   heights = c(sum(def_heights_adapt), 0.1)
          # )

          main_plot_list    <- list(
            plot  = main_plot_adapt,
            # title = title_adapt
            lgd   = plot_legend_adapt,
            spacer = plot_spacer_adapt
          )
          #
          # ### Return
          # return(main_plot_list)


        }) %>%
        ### Name the plots
        (function(x){
          names(x)    <- c_types
          return(x)
        })

      ### Separate reference plot from other plots
      names_list_types <- plotList_types %>% names
      # names_list_types %>% print

      ### Get plot legend
      ref_plot_types    <- plotList_types[[1]]
      plot_title_types  <- ref_plot_types$title
      plot_legend_types <- ref_plot_types$lgd

      plot_list_types <- list()
      for(m in 1:length(plotList_types)){
        type_m      <- c_types[m]
        plot_m      <- plotList_types[[type_m]]$plot
        plot_list_types[[type_m]] <- plot_m
      }

      main_plot_types <- ggarrange(
        plotlist = plot_list_types,
        ncol = 1,
        common.legend = T,
        legend = "bottom",
        legend.grob = plot_legend_types
      )
      return(main_plot_types)

    }) %>%
    ### Name the plots
    (function(x){
      names(x)    <- c_years
      return(x)
    })

  if(print_msg) message("Finished.")
  ## Return the plot
  return(plotList_years)
}
