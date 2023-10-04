###### plot_byDegree ######
### This function plots all sectors together for a particular year and specified adaptation
plot_byDegree <- function(
  data,
  sumByCol = "annual_impacts",
  driverCol = "driverValue",
  year = 2020,
  ncol = 4,
  nTicks = 5,
  # titles = list(GCM="Impacts by Degrees of Warming", SLR="Impacts by GMSL (cm)"),
  title   = "Impacts by Degrees of Warming",
  heights = list(GCM = NULL, SLR = NULL),
  silent = F
){
  # "got here" %>% print
  if(is.null(sumByCol )){sumByCol  <- "annual_impacts"}
  if(is.null(driverCol)){driverCol <- "driverValue"}
  if(is.null(year     )){year      <- 2020}
  if(is.null(ncol     )){ncol      <- 4}
  if(is.null(nTicks   )){nTicks    <- 5}
  if(is.null(silent   )){silent    <- F}
  
  if(is.null(title    )){title    <- "Impacts by Degrees of Warming"}
  # default_titles <- list(GCM="Impacts by Degrees of Warming", SLR="Impacts by GMSL (cm)")
  # if(is.null(titles   )){titles    <- default_titles}
  
  plot_primary <- T
  
  # "got here" %>% print
  ### Whether to message
  print_msg <- !silent
  if(print_msg) message("Running plot_byDegree():")
  
  ### Get number of model types
  data         <- data %>% as.data.frame 
  c_modelTypes <- data$model_type %>% unique 
  n_modelTypes <- c_modelTypes %>% length
  
  # c_modelTypes %>% print
  
  ### Default heights
  def_heights <- list(GCM = NULL, SLR = NULL)
  if(!is.null(heights)){
    if(!is.list(heights)){
      list_heights <- def_heights
      for(i in 1:n_modelTypes){
        list_heights[[c_modelTypes[i]]] <- heights
      } ### End for
    } ### End if
    else{
      list_heights <- heights
    } 
  } else{
    list_heights <- def_heights
  }
  
  
  ###### Filter data ######
  df_x <- data %>% 
    filter(year == year) %>%
    filter(plot_primary==plot_primary)
  
  ### Plot labels
  df_labels <- data.frame(
    model_type = c("GCM", "SLR"), 
    label = c("By GCM", "By SLR Scenario")
  ) %>% 
    filter(model_type %in% c_modelTypes) %>%
    as.data.frame
  
  ###### Get plot grids ######
  plotList_models <-  1:n_modelTypes %>% 
    lapply(function(i){
      
      modelType_i <- c_modelTypes[i]
      heights_i   <- list_heights[[i]]
      label_i     <- df_labels$label[i]
      
      # if(!is.list(titles)){
      #   title   <- titles[i]
      # } else{
      #   title   <- title[[modelType_i]]
      #   if(is.null(title)){
      #     title   <- title[[i]]
      #   }
      # }
      
      title_i     <- paste(title, label_i)

      
      
      if(i>1){
        if(print_msg) message("\t", "Plotting by degree for ", modelType_i, "...")
      }
      
      data_i  <- df_x %>% filter(model_type==modelType_i)
      plot_i  <- data_i %>% 
        plot_DOW_byModelType(
          modelType = modelType_i,
          ncol   = ncol, 
          nTicks = nTicks, 
          # title  = title_i,
          heights = heights_i,
          silent  = T
        )
    }) %>%
    ### Name the plots
    (function(x){
      names(x)    <- c_modelTypes
      return(x)
    })
  
  ### 
  if(print_msg) message("Finished.")
  return(plotList_models)
}