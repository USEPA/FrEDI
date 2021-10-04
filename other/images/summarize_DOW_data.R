###### summarize_DOW_data ######
### This function summarizes data to use with plot_byDegree
summarize_DOW_data <- function(
  data,
  year = 2020,
  primary = T,
  sumByCol = "annual_impacts",
  bySector = T,
  otherGroupVars = c("adaptation", "impactType", "impactYear"),
  impactYear = NULL,
  silent = F
){
  ###### Set defaults ######
  if(is.null(year    )) year     <- 2020
  if(is.null(primary )) primary  <- T
  if(is.null(bySector)) bySector <- T
  if(is.null(sumByCol)) sumByCol <- "annual_impacts"
  if(is.null(otherGroupVars)) otherGroupVars <- c("adaptation", "impactType", "impactYear")

  ### Messaging
  if(is.null(silent)) silent     <- F
  print_msg <- !silent


  ###### Prep data #######
  ### Keep only observations for specified reference year
  ### Drop model averages
  data <- data %>%
    filter(model!="Average") %>%
    as.data.frame

  # data %>% nrow %>% print

  #### Filter to specific year
  ref_year <- year
  data <- data %>% filter(year == ref_year) %>% as.data.frame
  # if(bySector){
  #   data <- data %>% filter(year == ref_year) %>% as.data.frame
  #   # data$year %>% unique %>% length %>% print
  # }


  ### Standardize the column name
  data$yCol <- data[,sumByCol]

  data      <- data %>%
    select(-c(all_of(sumByCol))) %>%
    mutate(is_NA = yCol %>% is.na)

  #### Names
  data_names0 <- data %>% names

  ###### Summarize by Region ######
  ### Drop national totals if present
  if("region" %in% data_names0){
    data <- data %>% filter(region!="National Total")
  }
  c_regions   <- data$region %>% unique
  n_regions   <- c_regions %>% length

  # n_regions %>% print
  ### Main group vars
  main_groupVars  <- c("sector", "model_type", "model", "driverValue")
  which_main     <- (main_groupVars %in% data_names0) %>% which
  main_groupVars <- main_groupVars[which_main]

  ### Figure out which factor columns are in the data
  other_groupVars <- otherGroupVars
  which_other     <- (other_groupVars %in% data_names0) %>% which
  other_groupVars <- other_groupVars[which_other]

  # data %>% nrow %>% print
  ###### Summarize by Impact Year ######
  ref_impactYear <- impactYear
  if(("impactYear" %in% other_groupVars) & bySector){
    c_impYears   <- data$impactYear %>% unique
    n_impYears   <- c_impYears %>% length
    if(n_impYears>1){
      if(print_msg) message("\t", "More than one impact year present...")
      if(is.null(impactYear)){
        which_not_interp <- (c_impYears!="Interpolation") %>% which
        impactYear       <- c_impYears[1]
      }

      if(print_msg) message("\t", "Summarizing values for impact year", impactYear, " ...")
    }

    data <- data %>%
      filter(impactYear=="Interpolation" | impactYear == ref_impactYear) %>%
      mutate(impactYear==c_impYears) %>%
      mutate(impactYear = impactYear %>% as.character %>% as.numeric) %>%
      filter(year == impactYear)
    n_impYears   <- c_impYears %>% length
  }
  # "got here" %>% print
  # data %>% filter(is.na(yCol)) %>% nrow %>% print

  ###### Get primary values ######
  primeValue    <- primary * 1
  if(("primary" %in% data_names0) & bySector){
    data <- data %>% filter(primary==primeValue) %>% as.data.frame
  }

  ###### Summarize by Impact Type ######
  ### Impact Type
  if(("impactType" %in% other_groupVars) & bySector){
    impactType_groupVars <- c(other_groupVars[which(other_groupVars!="impactType")], main_groupVars)

    #### Count number of impact types
    count_impactTypes <-  data %>%
      group_by_at(c(all_of(impactType_groupVars), "region")) %>%
      summarize(n=n(), .groups="keep")

    n_impTypes   <- count_impactTypes$n %>% max
    # n_impTypes %>% print
    # data$year %>% unique %>% print

    if(n_impTypes>1){
      if(print_msg) message("\t", "More than one impact type present...")
      if(print_msg) message("\t\t", "Summing values across impact types...")

      data <- data %>%
        (function(x){
          # data %>% names %>% print
          #### Join with other data
          x <- x %>% left_join(count_impactTypes, by = c(impactType_groupVars, "region"))
          ### Summarize by impact type
          x_impactTypes <- x %>%
            group_by_at(.vars=c(all_of(impactType_groupVars), "region", "n")) %>%
            summarize_at(.vars = c("yCol", "is_NA"), sum, na.rm = T) %>%
            mutate(
              is_NA = is_NA < n,
              is_NA = is_NA * 1,
              is_NA = is_NA %>% na_if(0)
            ) %>%
            mutate(yCol = yCol * is_NA)
            # mutate(yCol = yCol * is_NA) %>%
            # mutate(yCol = yCol %>% replace_na(0))

          # "got here"
          # x_impactTypes %>% filter(!is.na(yCol)) %>% nrow %>% print

          ### Summarize national values
          x_national  <- x_impactTypes %>%
            group_by_at(.vars=c(all_of(impactType_groupVars))) %>%
            summarize_at(.vars = c("yCol", "is_NA"), sum, na.rm = T) %>%
            mutate(
              is_NA = is_NA < n_regions,
              is_NA = is_NA * 1,
              is_NA = is_NA %>% na_if(0)
            ) %>%
            mutate(yCol = yCol * is_NA) %>%
            # mutate(yCol = yCol %>% replace_na(0)) %>%
            mutate(region = "National Total") %>%
            mutate(impactType = "all")

          return(x_national)
        })
    } ### End if n_impTypes > 1
  } ### End if impactType in data
  # "got here" %>% print
  # data %>% filter(!is.na(yCol)) %>% nrow %>% print

  ###### Summarize by Adaptation ######
  if(("adaptation" %in% other_groupVars) & bySector){
    adapt_groupVars <- c(other_groupVars[which(other_groupVars!="adaptation")], main_groupVars)

    #### Count number of adaptations
    #### Count number of impact types
    count_adapt <-  data %>%
      group_by_at(.vars=c(all_of(adapt_groupVars), "region")) %>%
      summarize(n=n(), .groups="keep")

    n_adapt   <- count_adapt$n %>% max

    if(n_adapt>1){
      if(print_msg) message("\t", "More than one adaptation present...")
      if(print_msg) message("\t\t", "Averaging values across adaptations...")


      data <- data %>%
        (function(x){

          #### Join with other data
          x <- x %>% left_join(count_adapt, by = c(adapt_groupVars, "region"))

          ### Summarize by impact type
          x_adapt <- x %>%
            group_by_at(.vars=c(all_of(adapt_groupVars), "region", "n")) %>%
            summarize_at(.vars = c("yCol", "is_NA"), sum, na.rm = T) %>%
            mutate(
              is_NA = is_NA < n,
              is_NA = is_NA * 1,
              is_NA = is_NA %>% na_if(0)
            ) %>%
            mutate(yCol = yCol * is_NA)
            # mutate(yCol = yCol * is_NA) %>%
            # mutate(yCol = yCol %>% replace_na(0))

          ### Summarize national values
          x_national  <- x_adapt %>%
            group_by_at(.vars=c(all_of(adapt_groupVars))) %>%
            summarize_at(.vars = c("yCol", "is_NA"), sum, na.rm = T) %>%
            mutate(
              is_NA = is_NA < n_regions,
              is_NA = is_NA * 1,
              is_NA = is_NA %>% na_if(0)
            ) %>%
            mutate(yCol = yCol * is_NA) %>%
            # mutate(yCol = yCol %>% replace_na(0)) %>%
            mutate(region = "National Total") %>%
            mutate(adaptation = "Average")

          return(x_national)
        })
    } ### End if n_impTypes > 1
  } ### End if impactType in data
  # "got here" %>% print
  # data %>% filter(!is.na(yCol)) %>% nrow %>% print


  ###### Summarize By Sector ######
  all_group_vars <- c(main_groupVars, other_groupVars)
  # c_regions      <- (data %>% filter(region!="National Total"))$region %>% unique
  # n_regions      <- c_regions %>% length
  # c_regions%>% print
  # n_regions%>% print
  if(bySector){
    data <- data %>%
      group_by_at(.vars = c(all_of(main_groupVars))) %>%
      summarize_at(.vars = c("yCol", "is_NA"), sum, na.rm = T) %>%
      mutate(
        is_NA = is_NA < n_regions,
        is_NA = is_NA * 1,
        is_NA = is_NA %>% na_if(0)
      ) %>%
      mutate(yCol = yCol * is_NA) %>%
      # mutate(yCol = yCol %>% replace_na(0)) %>%
      mutate(region = "National Total")

    # data %>% filter(is.na(yCol)) %>% nrow %>% print
  } else{
    # all_group_vars %>% print
    # data %>% nrow %>% print


    data <- data %>%
      as.data.frame %>%
      ungroup %>%
      # group_by_at(.vars = c(all_of(all_group_vars))) %>%
      group_by_at(.vars = c("sector", "model_type", "model", "driverValue", "impactYear", "impactType", "adaptation")) %>%
      summarize_at(.vars = c("yCol", "is_NA"), sum, na.rm = T)


    data %>% filter(sector=="Extreme Temperature") %>% filter(!is.na(yCol)) %>% nrow %>% print
    (data %>% filter(sector=="Extreme Temperature"))$is_NA %>% max(na.rm=T) %>% print

    data <- data %>%
      mutate(
        is_NA = is_NA < n_regions,
        is_NA = is_NA * 1,
        is_NA = is_NA %>% na_if(0)
      ) %>%
      mutate(yCol = yCol * is_NA) %>%
      # mutate(yCol = yCol %>% replace_na(0)) %>%
      mutate(region = "National Total")


    # data <- data %>%
    #   group_by_at(.vars = c(all_of(all_group_vars))) %>%
    #   summarize_at(.vars = c("yCol", "is_NA"), sum, na.rm = T) %>%
    #   mutate(
    #     is_NA = is_NA < n_regions,
    #     is_NA = is_NA * 1,
    #     is_NA = is_NA %>% na_if(0)
    #   ) %>%
    #   mutate(yCol = yCol * is_NA) %>%
    #   # mutate(yCol = yCol %>% replace_na(0)) %>%
    #   mutate(region = "National Total")
    # data %>% filter(!is.na(yCol)) %>% nrow %>% print
  }

  ###### Return ######
  return_df             <- data %>% ungroup %>% as.data.frame
  return_df[, sumByCol] <- return_df$yCol
  return_df             <- return_df %>% select(-c("yCol", "is_NA")) %>% as.data.frame
  return(return_df)
}
