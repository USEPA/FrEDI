###### summarize_DOW_data
### This function summarizes data to use with plot_byDegree
summarize_DOW_data <- function(
    data,
    bySector   = TRUE,
    year       = 2020,
    primary    = TRUE,
    sumCol     = "annual_impacts",
    groupVars  = c("variant", "impactType", "impactYear"),
    impactYear = "Interpolation",
    silent     = FALSE
){
  ###### Set defaults ######
  ### Messaging
  print_msg <- !silent

  ###### Prep data #######
  ### Keep only observations for specified reference year
  ### Drop model averages
  # data      <- data |> filter(model != "Average")
  # data      <- data |> filter(model %in% c("Average", "Model Average"))
  data      <- data |> filter(!(model %in% c("Average", "Model Average")))

  #### Filter to specific year
  ref_year  <- year
  data      <- data |> filter(year == ref_year)
  c_years   <- data |> pull(year) |> unique()
  n_years   <- c_years |> length()

  ### Standardize the column name
  # drop0     <- sumCol
  # data[["yCol"]] <- data[[sumCol]]
  # # data |> names() |> print()
  # data      <- data |> select(-all_of(drop0))
  # rm(drop0)
  data      <- data |> rename_at(c(sumCol), ~c("yCol"))
  data      <- data |> mutate(is_NA = yCol |> is.na())

  #### Names
  names0      <- data |> names()

  ### Values
  c_regions   <- data |> pull(region) |> unique()
  c_impYears  <- data |> pull(impactYear) |> unique()
  c_impTypes  <- data |> pull(impactType) |> unique()
  c_variants  <- data |> pull(variant) |> unique()

  ### Number of values
  n_regions   <- c_regions  |> length()
  n_impYears  <- c_impYears |> length()
  n_impTypes  <- c_impTypes |> length()
  n_variants  <- c_variants |> length()
  # n_regions |> print()

  ### Whether to check values
  hasRegions  <- (n_regions  > 1) & bySector
  hasImpYears <- (n_impYears > 1) & bySector
  hasImpTypes <- (n_impTypes > 1) & bySector
  hasVariants <- (n_variants > 1) & bySector

  ### Whether to aggregate values
  doImpTypes  <- hasImpTypes & !bySector

  ### Main group vars
  main_groupVars <- c("sector", "model_type", "model", "driverValue")
  # which_main     <- (main_groupVars %in% names0) |> which()
  # main_groupVars <- main_groupVars[which_main]
  main_groupVars <- main_groupVars |> get_matches(y=names0, matches=T)

  ### Figure out which factor columns are in the data
  # groupVars      <- groupVars
  # which_other    <- (groupVars %in% names0) |> which()
  # groupVars      <- groupVars[which_other]
  groupVars      <- groupVars |> get_matches(y=names0, matches=T)
  # data |> nrow() |> print()

  ###### Summarize by Region ######
  ### Drop national totals if present
  hasNat         <- "National Total" %in% c_regions
  doNat          <- !hasNat
  if(hasNat) data <- data |> filter(region == "National Total")
  else       data <- data |> filter(region != "National Total")
  # x_nat  <- x_nat |> mutate(region = "National Total")

  ###### Summarize by Impact Year ######
  impactYear0    <- impactYear
  multImpYears   <- length(impactYear) > 1
  if(hasImpYears) {
    if(multImpYears) {
      hasImpYr <- !(impactYear |> is.null())
      if(!hasImpYr) impactYear0 <- impactYear0[1]
      if(print_msg) message("\t", "More than one impact year present...")
      if(print_msg) message("\t", "Using impact year ", impactYear, "...")
    } ### End if(multImpYears)
    ### Filter data
    data       <- data |> filter(impactYear == impactYear0)
  } ### End if(hasImpYears)
  # "got here" |> print()
  # data |> filter(is.na(yCol)) |> nrow() |> print()

  ###### Summarize by Impact Type ######
  ### Impact Type
  if(doImpTypes){
    # impTypeGroups0 <- c(groupVars[which(groupVars!="impactType")])
    impTypeGroups0 <- groupVars |> get_matches(y="impactType", matches=F)
    impTypeGroups0 <- impTypeGroups0 |> c(main_groupVars) |> c("region") |> unique()
    # impTypeGroups0 |> print()
    #### Count number of impact types
    group0         <- c(impTypeGroups0) |> unique()
    count_impTypes <- data |>
      group_by_at(c(group0)) |>
      summarize(n=n(), .groups="drop")
    n_impTypes     <- count_impTypes |> pull(n) |> max()
    rm(group0)
    # count_impTypes |> glimpse()
    # n_impTypes |> print()
    # data |> pull(year) |> unique |> print()

    if(n_impTypes>1){
      if(print_msg) {
        message("\t", "More than one impact type present...")
        message("\t\t", "Summing values across impact types...")
      } ### End if(print_msg)
      ### Join with counts
      join0  <- c(impTypeGroups0) |> unique()
      data   <- data |> left_join(count_impTypes, by=join0)
      rm(join0, count_impTypes)

      ### Get national summary
      data   <- data |> (function(x){
        # data |> names() |> print()
        ### Summarize by impact type
        group0 <- c(impTypeGroups0) |> c("n") |> unique()
        x_imp  <- x |> sum_with_na(
          group0    = group0,
          col0      = "yCol",
          threshCol = "n",
          drop      = TRUE
        ) ### End sum_with_na
        # "got here"
        # x_imp |> filter(!is.na(yCol)) |> nrow() |> print()
        drop0  <- c("n")
        x_imp  <- x_imp |> select(-all_of(drop0))
        rm(group0, drop0)

        ### Summarize national values
        if(doNat) {
          group0 <- impTypeGroups0[!(impTypeGroups0 %in% c("region"))]
          group0 <- group0 |> c("threshold") |> unique()
          x_nat  <- x_imp  |> mutate(threshold=n_regions)
          x_nat  <- x_nat  |> sum_with_na(
            group0    = group0,
            col0      = "yCol",
            threshCol = "threshold",
            drop      = TRUE
          ) ### End sum_with_na
          ### Drop columns
          drop0  <- c("threshold")
          x_imp  <- x_imp |> select(-all_of(drop0))
          rm(group0, drop0)
          ### Add region
          x_imp  <- x_imp |> mutate(impactType = "All")
          x_imp  <- x_imp |> mutate(region     = "National Total")
        } ### End if(doNat)
        return(x_imp)
      })()
    } ### End if n_impTypes > 1
  } ### End if impactType in data

  ###### Summarize By Sector ######
  all_group_vars <- c(main_groupVars, groupVars)
  c_regions      <- data |> pull(region) |> unique()
  n_regions      <- c_regions |> length()
  group0 <- all_group_vars
  # c_regions |> print(); n_regions |> print()

  ### Summarize national values
  group0 <- group0 |> c("threshold") |> unique()
  data   <- data   |> mutate(threshold=n_regions)
  # data |> glimpse()
  data   <- data   |> sum_with_na(
    group0    = group0,
    col0      = "yCol",
    threshCol = "threshold",
    drop      = TRUE
  ) ### End sum_with_na
  # data |> glimpse()
  ### Drop columns
  drop0  <- c("threshold")
  data   <- data |> select(-all_of(drop0))
  rm(group0, drop0)

  ### Additional columns
  data   <- data |> mutate(region = "National Total")
  # data |> filter(is.na(yCol)) |> nrow() |> print()

  ###### Return ######
  # data |> glimpse()
  # drop0  <- c("yCol")
  # data[[sumCol]] <- data[["yCol"]]
  # data   <- data |> select(-all_of(drop0))
  data   <- data |> rename_at(c("yCol"), ~sumCol)
  return(data)
}

###### End of script ######
