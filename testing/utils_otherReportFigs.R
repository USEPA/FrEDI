### Other report figure utilities
### Scenario functions ----------------------
### Function to get national scenario from results
getNationalScenario_fromResults <- function(df0){
  ### Filter to a sector
  sector0  <- df0 |> pull(sector) |> unique()
  df0      <- df0 |> filter(sector %in% sector0)
  rm(sector0)
  ### Select columns and get distinct values
  select0  <- c("year", "national_pop", "gdp_usd", "gdp_percap")
  df0      <- df0 |> select(all_of(select0))
  df0      <- df0 |> unique()
  rm(select0)
  ### Return
  gc()
  return(df0)
}

### Get state population scenario from results
getPopScenario_fromResults <- function(df0){
  ### Filter to a sector
  sector0  <- df0 |> pull(sector) |> unique()
  df0      <- df0 |> filter(sector %in% sector0)
  rm(sector0)
  ### Select columns and get distinct values
  select0  <- c("region", "state", "postal", "year", "pop")
  df0      <- df0 |> select(all_of(select0))
  df0      <- df0 |> unique()
  rm(select0)
  ### Return
  gc()
  return(df0)
}

### Get driver scenario from results
getDriverScenario_fromResults <- function(df0){
  ### Filter to a sector
  sector1  <- df0 |> filter((model_type |> tolower()) %in% "gcm") |> pull(sector) |> unique()
  sector2  <- df0 |> filter((model_type |> tolower()) %in% "slr") |> pull(sector) |> unique()
  df0      <- df0 |> filter(sector %in% c(sector1, sector2))
  rm(sector1, sector2)
  ### Select columns and get distinct values
  select0  <- c("model_type", "year", "driverType", "driverUnit", "driverValue")
  df0      <- df0 |> select(all_of(select0))
  df0      <- df0 |> unique()
  rm(select0)
  ### Return
  gc()
  return(df0)
}

### Summarize Values --------------------------
### Summarize region population scenario from state population
summarizeRegPop <- function(df0){
  ### Select columns and get distinct values
  group0   <- c("region", "year")
  sum0     <- c("pop")
  df0      <- df0 |> group_by_at(c(group0)) |> summarize_at(c(sum0), sum, na.rm=T) |> ungroup()
  rm(group0, sum0)
  ### Return
  gc()
  return(df0)
}

### Summarize values by region
sumByRegion <- function(
    df0,
    group0 = c("sector", "variant", "impactType", "impactYear", "model", "model_type", "region", , "sectorprimary", "includeaggregate", "year", "gdp_usd"),
    cols0  = c("annual_impacts")
){
  ### Get columns
  names0 <- df0    |> names()
  group0 <- group0 |> get_matches(y=names0)
  cols0  <- cols0  |> get_matches(y=names0)
  ### Summarize over regions
  df0    <- df0    |> group_by_at(c(group0)) |> summarize_at(c(cols0), sum, na.rm=T) |> ungroup()
  ### Return
  return(df0)
}

### Function to scale impacts by gdp
calcScaledImpacts <- function(
    df0,
    cols0 = "annual_impacts"
){
  ### Columns
  colsM    <- cols0 |> paste0("_multiplier")
  colsS    <- cols0 |> paste0("_scaled")
  ### Calculate multiplier and then apply multiplier
  ### (1 + annual_impacts / gdp_usd)**(-1)
  df0[,colsM] <- df0[,cols0]
  df0[,colsM] <- df0[["gdp_usd"]] / (1 + df0[,colsM])
  df0[,colsS] <- df0[,cols0] * df0[,colsM]
  rm(colsM, colsS)
  ### Return
  gc()
  return(df0)
}


### Calculate per capita impaacts
calcPerCap <- function(
    df0,
    cols0 = "annual_impacts",
    pop0  = "pop"
){
  ### Columns
  cols1    <- cols0 |> paste0("_percap")
  ### Calculate per capita values
  df0[,cols1] <- df0[,cols0]
  df0         <- df0 |> mutate_at(c(cols1), function(x, y=df0[[pop0]]){x/y})
  rm(cols0, cols1)
  ### Return
  gc()
  return(df0)
}

### Format Data ---------------------------
### Function to replace ATS data in df1 with adjusted results in df2
replaceATSdata <- function(
    df1, ### listAgg2[[name]]
    df2, ### listATS[[name]][["all"]]
    sector1 = "ATS Temperature-Related Mortality"
){
  df1 <- df1 |> filter(!(sector %in% sector1))
  df1 <- df1 |> bind_rows(df2); rm(df2)
  return(df1)
}

### Create list containing ATS data, adjusted by suicides
getAdjustedATSlist <- function(
    ### List of data frames to adjust (must contain at least ATS and Suicides)
    list0   = list(none=tibble(), agg=tibble(), all=tibble()),
    sector1 = "ATS Temperature-Related Mortality",
    sector2 = "Suicide",
    join0   = c("state", "model", "year"),
    ### Single vector or list of columns with same number of named elements as list0
    cols0   = c("physical_impacts", "annual_impacts")
){
  ### Names
  names0   <- list0 |> names()

  ### Filter to specific sectors
  sectors0 <- sector1 |> c(sector2)
  list0    <- list0   |> map(function(data0){data0 |> filter(sector %in% sectors0)}) |> set_names(names0)
  rm(sectors0)

  ### Get list of cols
  doCols0  <- !(cols0 |> is.list())
  if(doCols) cols0 <- names0 |> map(function(name_i, cols_i=cols0){cols_i}) |> set_names(names0)

  ### Adjust columns for agg
  hasAgg0  <- "agg" %in% (names0 |> tolower())
  if(hasAgg0) {
    which1 <- (names0 |> tolower()) %in% "agg"
    cols1  <- cols0[[which1]]
    phys1  <- cols1 |> str_detect("physical")
    cols1  <- drop1[!phys1]
    cols0[[which1]] <- cols1
    rm(which1, cols1, phys1)
  } ### End if(hasAgg0)

  ### Adjust data for sector2
  list0    <- list(data0=list0, cols0=cols0) |> pmap(function(data0, cols0){
    data0 |> adjustSector1_bySector2(sector1=sector1, sector2=sector2, cols0=cols0)
    }) |> set_names(names0)

  ### Drop sector2 and just keep sector 1
  list0    <- list0   |> map(function(data0){data0 |> filter(sector %in% sector1)}) |> set_names(names0)

  ### Return
  gc()
  return(list0)
}
# ### Create list containing ATS data, adjusted by suicides: old version
# getAdjustedATSlist2 <- function(
#     df1     = listResults0[[name0]],
#     df2     = listAgg1[[name0]],
#     df3     = listAgg2[[name0]],
#     names0  = c("none", "agg", "all"),
#     sector1 = "ATS Temperature-Related Mortality",
#     sector2 = "Suicide",
#     join0   = c("state", "model", "year"),
#     cols0   = c("physical_impacts", "annual_impacts")
# ){
#   ### Names
#   names0   <- list0 |> names()
#
#   ### Filter to specific sectors
#   sectors0 <- sector1 |> c(sector2)
#   df1      <- df1 |> filter(sector %in% sectors0)
#   df2      <- df2 |> filter(sector %in% sectors0)
#   df3      <- df3 |> filter(sector %in% sectors0)
#   rm(sectors0)
#
#   ### Adjust data for sector2
#   cols1    <- cols0 |> get_matches(y="physical_impacts", matches=F)
#   df1      <- df1 |> adjustSector1_bySector2(sector1=sector1, sector2=sector2, cols0=cols0)
#   df2      <- df2 |> adjustSector1_bySector2(sector1=sector1, sector2=sector2, cols0=cols0)
#   df3      <- df3 |> adjustSector1_bySector2(sector1=sector1, sector2=sector2, cols0=cols1)
#   rm(cols1)
#
#   ### Drop sector2 and just keep sector 1
#   df1    <- df1 |> filter(sector %in% sector1)
#   df2    <- df2 |> filter(sector %in% sector1)
#   df3    <- df3 |> filter(sector %in% sector1)
#
#   ### Add to list
#   list0   <- list()
#   list0[[names0[1]]] <- df1; rm(df1)
#   list0[[names0[2]]] <- df2; rm(df2)
#   list0[[names0[3]]] <- df3; rm(df3)
#   rm(names0)
#
#   ### Return
#   gc()
#   return(list0)
# }


### Select plot data, calculate scaled values
formatPlotData <- function(
    df0, ### listAgg2[[name]]
    cols0    = c("annual_impacts")
){
  cols1 <- cols0 |> map(paste0, c("", "_scaled")) |> unlist()
  df0   <- df0   |> calcScaledImpacts(cols0=cols0)
  df0   <- df0   |> calcPerCap  (cols0=cols1)
  rm(cols1, cols0)
  return(df0)
}


### Function to multiply columns in a tibble by a constant
mutate_multiplyConstant <- function(df0, cols0, k0){
  df0 |> mutate_at(c(cols0), function(x, y=k0){x * y})
}

### Add category count
addCategoryCount <- function(
    df0,
    health0 = c("health", "atsHealth"),
    group0  = c("lab1")
){
  ### Summarize number in groups and join summary with df1
  move0   <- c("nSectors")
  sort0   <- move0 |> c(group0)
  df1     <- df0 |> group_by_at(c(group0)) |> summarize(nSectors=n(), .groups="drop")
  df0     <- df0 |> select(-any_of(move0)) |> left_join(df1, by=group0)
  # df0 |> glimpse()
  df0     <- df0 |> relocate(all_of(move0), .before="lab0")
  df0     <- df0 |> arrange_at(c(sort0), desc)
  rm(move0, sort0, df1)
  ### Mutate lab1 and lab2
  df0     <- df0 |> mutate(lab2 = df0[[group0]] |> paste0(" (", nSectors, ")"), .after=all_of(group0))
  ### Return
  return(df0)
}

### Function to join with sector labels
join_sectorLabels <- function(
    df0,
    join0   = c("sector"),
    health0 = c("health", "atsHealth"), ### Which health categories to include (also: cilHealth, or health)
    other0  = FALSE, ### Whether to keep other
    df1     = fun_colorSectors()
){
  ### Filter out some categories
  filter0 <- c("lab0")
  vals0   <- c("na", "health", "cilHealth", "atsHealth") |> get_matches(y=health0, matches=F)
  if(!other0) vals0 <- vals0 |> c("other")
  df1     <- df1 |> filter_at(c(filter0), function(x, y=vals0){x |> get_matches(y=vals0, matches=F, type="matches")})
  df1     <- df1 |> addCategoryCount(health0=health0)
  # df1 |> glimpse()

  ### Join with sector info
  join0   <- c("sector")
  to0     <- c("year")
  move0   <- df1 |> names() |> get_matches(y=join0, matches=FALSE)
  df0     <- df0 |> left_join(df1, by=join0)
  # df0     <- df0 |> addCategoryCount(health0=health0)
  df0     <- df0 |> relocate(all_of(move0), .before=all_of(to0))
  df0     <- df0 |> rename_at(c("lab2"), ~c("category"))
  rm(join0, move0, df1, to0)

  ### Return
  return(df0)
}



### Stacked Bar------------------------------
### Function to format data for stacked bar plot by sector
##### Pre-process and organize
#### - Option A - without error bars
####     * Annual Impacts for Baseline
####       - Filter relevant data for three years of interest and assign category to each sector
####       - Summarize annual impacts
####       - Filter to to national
format_stackedBar_bySector <- function(
    df0,
    cols0   = c("annual_impacts"),
    join0   = c("sector"),
    group0  = c("category"),
    health0 = c("health", "atsHealth"),
    other0  = FALSE, ### Whether to keep other
    # yrs0    = c(2050, 2070, 2090),
    df1     = fun_colorSectors()
    # df1     = fun_sectorLabels()
){
  ### Filter data
  ### Mutate year as factor
  # filter0 <- yrs0
  # df0     <- df0 |> getFilterDf(filters0="year", years0=yrs0)
  df0     <- df0 |> mutate(year = year |> as.factor())

  ### Join with sector info and drop missing values
  df0     <- df0 |> join_sectorLabels(join0=join0, health0=health0, other0=other0, df1=df1)
  df0     <- df0 |> filter(!lab0 |> is.na())
  # hTot0   <- df0 |> filter(lab0 %in% health0)
  rm(health0, other0, df1, join0)

  ### Order from largest to smallest
  # sort0   <- c("year") |> c(cols0, group0)
  sort0   <- c(cols0, group0)
  df1     <- df0
  df1     <- df1 |> group_by_at(c(group0)) |> summarize_at(c(cols0), sum, na.rm=T) |> ungroup()
  df1     <- df1 |> arrange_at(c(sort0))
  lvls0   <- df1 |> pull(category)
  rm(sort0)

  ### Factor category
  sort0   <- c(group0)
  df0     <- df0 |> mutate(category = category |> factor(levels=lvls0))
  df0     <- df0 |> arrange_at(c(sort0))
  rm(group0, lvls0, sort0)

  ### Return
  return(df0)
}

### Function to plot stacked bar chart by sector
plot_stackedBar_bySector <- function(
    df0,
    yCol0     = "annual_impacts",
    title0    = "Annual U.S. Climate-Driven Damages",
    subTitle0 = "Mean Damages by Year and Category (Trillions $)\nSubset of Climate-Related Impacts",
    lgdTitle0 = "Sector Categories \n(Number of sectors in category)",
    xLab0  = "Year",
    yLab0  = "Damages ($2015)",
    # yLab0  = "Damages (Trillions, $2015)",
    # fill0  = rainbow_6(),
    theme0 = theme_stackedBar(),
    theme1 = NULL,
    unit0  = "Trillions"
){
  ### Fill breaks
  dfFill0 <- df0 |> select(category, color0) |> unique()

  ### Initial plot
  p0      <- df0 |> ggplot()

  ### Add geom
  p0      <- p0 + geom_bar(
    aes(fill=category, y=.data[[yCol0]], x=year),
    position = "stack",
    stat     = "identity",
    alpha    = 0.7
  ) ### End geom_bar

  ### Format titles and labels
  str0    <- "\\("
  str1    <- str0  |> paste0(unit0, ", ")
  yLab0   <- yLab0 |> str_replace(str0, str1)
  ### Add scales
  p0      <- p0 + scale_y_continuous(yLab0)
  p0      <- p0 + scale_x_discrete  (xLab0)
  p0      <- p0 + scale_fill_manual (
    lgdTitle0,
    breaks = dfFill0 |> pull(category) |> rev(),
    values = dfFill0 |> pull(color0) |> rev()
  ) ### End scale_fill_manual

  ### Add themes
  p0      <- p0 + theme0
  p0      <- p0 + theme1

  ### Return
  return(p0)
}


### Box-Whisker -----------------------------------
### Format data for box and whisker plot
format_boxWhisker <- function(
    df0,
    # yrs0 = 2090,
    cols0   = c("annual_impacts"),
    join0   = c("sector"),
    # k0      = 1e-9,
    health0 = c("health", "atsHealth"),
    other0  = FALSE, ### Whether to keep other
    df1     = fun_colorSectors()
){
  ### Filter data
  # ### Mutate year as factor
  # filter0 <- yrs0
  # df0     <- df0 |> filter(year %in% yrs0)

  ### Join with sector info and drop missing values
  df0     <- df0 |> join_sectorLabels(join0=join0, health0=health0, other0=other0, df1=df1)
  df0     <- df0 |> filter(!lab0 |> is.na())
  df0$category |> unique() |> print()
  rm(health0, other0, df1, join0)

  # ### Adjust values top trillions
  # df0     <- df0 |> mutate(value = annual_impacts * k0)

  ### Find order of sectors and categories, largest to smallest, then sort
  group1  <- c("sector")
  sum1    <- c(cols0)
  sort1   <- c(sum1, group1)
  df1     <- df0
  df1     <- df1 |> group_by_at(c(group1)) |> summarize_at(c(sum1), mean, na.rm=T) |> ungroup()
  df1     <- df1 |> arrange_at(c(sort1))
  df1     <- df1 |> arrange(row_number() |> desc())
  lvls1   <- df1 |> pull(all_of(group1))
  ### Factor category and arrange
  df0     <- df0 |> mutate_at(c(group1), factor, levels=lvls1)
  df0     <- df0 |> arrange_at(c(group1))
  rm(group1, sum1, sort1, lvls1, df1)

  ### Find order of sectors and categories, largest to smallest, then sort
  group2  <- c("category")
  sum2    <- c(cols0)
  sort2   <- c(sum2, group2)
  df2     <- df0
  df2     <- df2 |> group_by_at(c(group2)) |> summarize_at(c(sum2), sum, na.rm=T) |> ungroup()
  df2     <- df2 |> arrange_at(c(sort2))
  df2     <- df2 |> arrange(row_number() |> desc())
  lvls2   <- df2 |> pull(all_of(group2))
  ### Factor category and sector and arrange
  df0     <- df0 |> mutate_at(c(group2), factor, levels=lvls2)
  df0     <- df0 |> arrange_at(c(group2))
  rm(group2, sum2, sort2,lvls2, df2)

  ### Arrange
  # # sort0   <- c("sector", "category")
  # sort0   <- c("category", "sector")
  # df0     <- df0 |> arrange_at(c(sort0))

  ### Return
  return(df0)
}


### Function to create a box and whisker plot
plot_basicBoxWhisker <- function(
    df0,
    sectors0  = c("ATS Temperature-Related Mortality"),
    yCol0     = c("annual_impacts"),
    title0    = "U.S. Annual Climate-Driven Damages in 2090",
    subTitle0 = "By sector, colored by sector category (subset of all climate-related impacts)",
    lgdTitle0 = "Sector Category",
    xLab0     = "",
    yLab0  = "Damages (Billions, $2015)",
    # yLab0  = "Damages ($2015)",
    # yLims0 = c(0, 4000),
    yLims0 = NULL,
    xLabs0 = function(x) str_wrap(x, width=25),
    yLabs0 = scales::dollar_format(),
    # fill0  = rainbow_6() |> rev(),
    unit0  = "",
    theme0 = theme_basicBoxWhisker(),
    df1    = fun_colorSectors()
){
  ### Fill breaks
  dfFill0 <- df0 |> select(category, color0) |> unique()

  ### Filter data to sector(s)
  filter0 <- c("sector")
  vals0   <- sectors0
  df0     <- df0 |> filter_at(c(filter0), function(x, y=vals0){(x %in% y)})
  rm(filter0, vals0)

  ### Print range and adjust y limits if necessary
  df0 |> pull(all_of(yCol0)) |> range(na.rm=T) |> print()

  ### Factor values
  mutate0 <- c("sector")
  lvls0   <- df1 |> pull(sector)
  lbls0   <- df1 |> pull(graphLabel)
  df0     <- df0 |> mutate_at(c(mutate0), factor, levels=lvls0, labels=lbls0)
  # df0     <- df0 |> mutate_at(c(mutate0), as.character)
  rm(mutate0, lvls0, lbls0)

  ### Group values
  group0  <- c("sector", "category")
  # group0  <- c("category")
  sum0    <- c("value")
  sort0   <- c(sum0, "category")
  df0     <- df0 |> mutate(value = df0 |> pull(all_of(yCol0)))
  df0     <- df0 |> group_by_at(c(group0)) |> summarize_at(c(sum0), mean, na.rm=T) |> ungroup()
  rm(group0, sum0)

  ### Format titles and labels
  # str0    <- "\\("
  # str1    <- str0  |> paste0(unit0, ", ")
  # yLab0   <- yLab0 |> str_replace(str0, str1)

  ### Initial plot
  p0      <- df0 |> ggplot(aes(x=reorder(sector, value, mean, na.rm=TRUE), y=value))

  ### Add geoms and flip coordinates
  p0      <- p0 + geom_bar(aes(fill = category), stat="identity", alpha=0.6, width=0.7)
  p0      <- p0 + coord_flip()

  ### Add scales
  p0      <- p0 + scale_x_discrete  (xLab0, labels=xLabs0)
  p0      <- p0 + scale_y_continuous(yLab0, sec.axis=dup_axis(), limits=yLims0, labels=yLabs0)
  # p0      <- p0 + scale_fill_manual (lgdTitle0, values=fill0)
  p0      <- p0 + scale_fill_manual (
    lgdTitle0,
    breaks = dfFill0 |> pull(category),
    values = dfFill0 |> pull(color0)
  ) ### End scale_fill_manual

  ### Add themes
  p0      <- p0 + theme0

  ### Return
  return(p0)
}

### Function to divide basicBoxWhisker plots into sector categories
plot_basicBoxWhisker2 <- function(
    df0,
    yCol0      = "annual_impacts",
    sectorList = list(
      a=c("ATS Temperature-Related Mortality"),
      b=c("Climate-Driven Changes in Air Quality", "Labor", "Rail", "Roads", "Suicide",
                "Transportation Impacts from High Tide Flooding", "Wildfire", "Wind Damage")
    ), ### End list
    # lims0     = NULL,
    # lims0     = list(sector1=c(0, 4000), sector2=c(NA, 600), other=c(NA, 40)),
    title0    = "U.S. Annual Climate-Driven Damages in 2090",
    subTitle0 = "By sector, colored by sector category (subset of all climate-related impacts)",
    # subTitle0 = "By sector, colored by sector category",
    # fill0     = list(a=rainbow_6() |> rev(), b=rainbow_6() |> rev(), c=rainbow_6()[-4] |> rev()),
    lgdTitle0 = "Sector Category",
    xLab0     = "",
    yLab0  = "Damages ($2015)",
    # yLims0 = c(0, 4000),
    yLims0 = NULL,
    xLabs0 = function(x) str_wrap(x, width=25),
    yLabs0 = scales::dollar_format(),
    theme0 = theme_basicBoxWhisker()
){
  ### Sector names
  sectors0  <- df0 |> pull(sector) |> unique()
  other0    <- sectors0 |> get_matches(y=sectorList |> unlist() |> unique(), matches=F)
  sectorList[["other"]] <- other0
  sectNames <- sectorList |> names()
  sectLbls  <- letters[sectNames |> length() |> seq_len()]

  ### Plot list
  pList0    <- sectNames |> map(function(
    name_i,
    sectors_i = sectorList[[name_i]]
    # sectors_i = sectorList[[name_i]],
    # fill_i    = fill0[[name_i]]
  ){
    df0 |> plot_basicBoxWhisker(
      sectors0  = sectors_i,
      title0    = title0,
      subTitle0 = subTitle0,
      xLab0     = xLab0,
      yLab0     = yLab0,
      xLabs0    = xLabs0,
      yLabs0    = yLabs0,
      yLims0    = yLims0,
      # fill0     = fill_i,
      theme0    = theme0
    ) ### End plot_basicBoxWhisker
  }) |> set_names(sectLbls)

  ### Return
  return(pList0)
}



### Regions ----------------------------
### Function to get remaining
arrangeTibbleVals <- function(
    df0,    ### Sorted data frame
    sort0   = "annual_impacts_percap", ### Column to sort on
    # desc0   = TRUE  ### Sort descending or not
    desc0   = TRUE , ### Sort descending or not
    rownum  = FALSE  ### Get row number
){
  ### Sort
  if(desc0)  {df0 <- df0 |> arrange_at(c(sort0), desc)}
  else       {df0 <- df0 |> arrange_at(c(sort0))}
  ### Add row number
  if(rownum) {df0 <- df0 |> mutate(RowNum = row_number())}
  ### Return
  return(df0)
}

### Function to get remaining
get_piePlotRemaining <- function(
    df0, ### Sorted data frame
    maxVal  = 4, ### Threshold
    valCol  = "RowNum", ### Column to check for threshold
    nameCol = "sector",
    newName = "Remaining"
){
  ### Add row number
  df0     <- df0 |> mutate(RowNum = row_number())
  ### Add row number and adjust value in name column
  df0     <- df0 |> mutate(checkVal = RowNum <= maxVal)
  df0     <- df0 |> mutate(nameVal  = df0[[nameCol]] )
  df0     <- df0 |> mutate(nameVal  = case_when(checkVal ~ nameVal, .default=newName))
  df0[[nameCol]] <- df0 |> pull(nameVal)
  rm(maxVal, valCol, nameCol, newName)

  ### Drop columns
  drop0   <- c("checkVal", "nameVal")
  df0     <- df0 |> select(-all_of(drop0))
  rm(drop0)

  ### Return
  return(df0)
}

### Add column with total value
add_colTotal <- function(
    df0,
    col0   = c("annual_impacts_percap"),
    col1   = c("total")
){
  ### Calculate total and add to a column
  total0  <- df0 |> pull  (all_of(col0)) |> sum(na.rm=T)
  df0[[col1]] <- total0
  ### Return
  return(df0)
}

### Calculate pie plot locations
calc_piePlotPositions <- function(
    df0,
    col0   = c("annual_impacts_percap"),
    col1   = "regTot" ### Column to store total value
){
  ### Mutate values:
  ### - Compute ratio to total
  ### - Compute the cumulative percentages (top of each rectangle)
  ### - Compute the bottom of each rectangle
  ### - Compute label position
  ### - Compute (a )good label(s)
  # df0 |> pull  (all_of(col0)) |> sum(na.rm=T) |> print()
  df0     <- df0 |> add_colTotal(col0=col0, col1=col1)
  df0     <- df0 |> mutate(fraction = df0[[col0]] / df0[[col1]])
  ### - Calculate ymax
  df0     <- df0 |> mutate(ymax     = fraction |> cumsum())
  ### Get head of ymax and calculate ymin
  yMins0  <- df0 |> pull  (ymax) |> head(n=-1)
  df0     <- df0 |> mutate(ymin     = 0 |> c(yMins0))
  ### Get label position
  df0     <- df0 |> mutate(labelPosition = (ymax + ymin) / 2)
  ### Return
  return(df0)
}

### Function to format values for regional pie plot
calc_regPieMargin <- function(
    region0,
    df0,
    fun0     = "mean", ### Summary function
    col0     = c("annual_impacts_percap"),
    col1     = c("total"), ### Column with total in it
    group0   = c("region", "sector", "color1", "darkop", "col1"),
    nameCol  = c("sector"),
    newName  = "Remaining",
    colorCol = "color1",
    newColor = "#D3D3D3",
    doRemain = TRUE,
    maxVal   = 4
){
  ### Add groups
  group0  <- group0 |> c(col1) |> unique()

  ### Filter to region
  df0     <- df0 |> filter(region %in% region0)

  ### Group and summarize
  ### Filter to impacts greater than zero and arrange descending
  df0     <- df0 |> group_by_at(c(group0)) |> summarize_at(c(col0), .funs=fun0, na.rm=T) |> ungroup()
  df0     <- df0 |> filter_at(c(col0), function(x){x >= 0})
  # df0     <- df0 |> arrange_at(c(col0), desc)
  df0     <- df0 |> arrangeTibbleVals(sort0=col0, desc0=T, rownum=T)
  # df0$sector |> unique() |> print()

  ### Adjust sector to remaining
  if(doRemain) {
    # df0     <- df0 |> mutate(RowNum = row_number())
    df0     <- df0 |> get_piePlotRemaining(
      maxVal  = maxVal,
      valCol  = "RowNum",
      nameCol = nameCol,
      newName = newName
    ) ### End get_piePlotRemaining
  } ### End if(doRemain)

  ### Adjust color and drop row number
  # df0$sector |> unique() |> print()
  df0 <- df0 |> mutate_at(c(colorCol), function(x, y=df0[[nameCol]]){
    case_when(y %in% newName ~ newColor, .default=x)
  })

  ### Drop RowNum, group, summarize, filter, and arrange
  drop0   <- c("RowNum")
  df0     <- df0 |> select(-any_of(drop0))
  df0     <- df0 |> group_by_at(c(group0)) |> summarize_at(c(col0), .funs=fun0, na.rm=T) |> ungroup()
  df0     <- df0 |> filter_at (c(col0), function(x){x >= 0})
  df0     <- df0 |> arrangeTibbleVals(sort0=col0, desc0=T, rownum=T)
  # df0 |> names() |> get_matches(y="RowNum") |> print()
  # df0     <- df0 |> arrange_at(c(col0), desc)
  # df0     <- df0 |> mutate_at(c(nameCol), as.factor)

  ### Add region total
  ### Get label position and then add labels
  col2    <- "regTot"
  df0     <- df0 |> calc_piePlotPositions(col0=col0, col1=col2)

  ### Add labels
  total0  <- df0 |> pull(all_of(col1)) |> unique()
  regTot0 <- df0 |> pull(all_of(col2)) |> unique()
  regPct0 <- (regTot0 / total0 * 1e2)
  # c(total0, regTot0, regPct0) |> print()
  df0     <- df0 |> mutate(label    = (fraction*1e2) |> round(0) |> format(nsmall=0) |> paste0("%"))
  df0     <- df0 |> mutate(label2   = regPct0 |> round(1) |> format(nsmall=1) |> paste0("%") )

  ### Return
  return(df0)
}



### Format region pie plots
### Add in a legend
format_regPieChart <- function(
    df0,
    col0    = c("annual_impacts_percap"),
    col1    = c("total"),
    join0   = c("sector"),
    group0  = c("region", "sector"),
    health0 = c("health", "atsHealth"),
    other0  = FALSE, ### Whether to keep other
    naStr0  = c(NA, NaN, Inf, -Inf, "NA", "NaN"),
    nameCol  = c("sector"),
    newName  = "Remaining",
    colorCol = "color1",
    darkop0  = TRUE,
    newColor = "#D3D3D3",
    df1      = fun_colorSectors()
){
  ### Adjust groups
  group0  <- group0 |> c(colorCol) |> unique()
  if(darkop0) group0 <- group0 |> c("darkop")

  ### Format data: Filter out NA values and filter to values >= 0
  filter0 <- c(col0)
  sort0   <- col0 |> c("sector")
  # df0     <- df0  |> filter_at(c(filter0), function(x, y=naStr0){!(x %in% y)})
  df0     <- df0 |> filter_at(c(filter0), function(x, y=naStr0){x |> get_matches(y=naStr0, matches=F, type="matches")})
  df0     <- df0 |> filter_at(c(col0), function(x){x >= 0})

  ### Join with sector info and drop missing values
  df0     <- df0 |> join_sectorLabels(join0=join0, health0=health0, other0=other0, df1=df1)
  df0     <- df0 |> filter(!lab0 |> is.na())
  rm(health0, other0, df1, join0)

  ### Get total value
  ### Calculate value for all regions
  # df0 |> pull(all_of(col0)) |> sum(na.rm=T) |> print()
  df0     <- df0 |> add_colTotal(col0=col0, col1=col1)
  # df0 |> pull(all_of(col1)) |> unique() |> print()

  ### Iterate over regions
  region0 <- df0     |> pull(region) |> unique()
  df0     <- region0 |> map(
    calc_regPieMargin,
    df0=df0, fun0="sum", col0=col0, group0=group0,
    nameCol=nameCol, newName=newName, newColor=newColor, colorCol=colorCol, doRemain=T
  ) |> bind_rows()
  # df0 |> glimpse()

  ### Calculate value for all regions
  dfAll0  <- df0   |> mutate(region = "All")
  dfAll0  <- "All" |> calc_regPieMargin(
    df0=dfAll0, fun0="mean", col0=col0, group0=group0,
    nameCol=nameCol, newName=newName, newColor=newColor, colorCol=colorCol, doRemain=F
  ) ### End calc_regPieMargin
  # df0 |> glimpse()

  ### Adjust values
  y0      <- "Sectors outside of the top 4 by region"
  y1      <- 1
  # dfAll0  <- dfAll0 |> mutate_at(c(nameCol), function(x){case_when(x %in% newName ~ y0, .default=x)})
  # dfAll0  <- dfAll0 |> mutate_at(c(col0), function(x){case_when(x %in% newName ~ y1, .default=x)})
  df0 <- df0 |> mutate_at(c(nameCol), function(x, y=df0[[nameCol]]){
    case_when(x %in% newName ~ y0, .default=x)
  }) |> mutate_at(c(col0), function(x, y=df0[[nameCol]]){
    case_when(y %in% newName ~ y1, .default=x)
  })
  rm(y0, y1)

  ### Bind values and adjust color for remaining
  df0     <- df0 |> bind_rows(dfAll0)
  rm(dfAll0)

  ### Return
  return(df0)
}



### Function to plot region pie margins
# scale_fill_brewer(palette = 'Greys',
#                   guide = guide_legend(reverse=TRUE),
#                   labels=function(x) sprintf("$%1.0f", as.double(x)))+
plot_regPieMargin <- function(
    region0,
    df0,
    col0      = c("annual_impacts_percap"),
    lgdTitle0 = "Top 4 Sectors",
    xLims0    = c(2, 4),
    xMax0     = 4,
    xMin0     = 3,
    colorCol  = "color1",
    darkop0   = TRUE,
    theme0    = theme_void()
){
  ### Filter data
  df0     <- df0 |> filter(region %in% region0)
  df0     <- df0 |> mutate(value = df0[[col0]])

  ### Arrange and factor data
  sort0   <- c("RowNum")
  df0     <- df0 |> arrange_at(c(sort0), desc)
  df0     <- df0 |> mutate_at(c("sector"), as.factor)

  ### Get title
  label0 <- df0     |> pull(label2) |> unique()
  title0 <- region0 |> paste(label0)

  ### Fill breaks
  select0 <- c("region", "sector") |> c(colorCol)
  if(darkop0) df0 <- df0 |> mutate_at(c(colorCol), function(x, y=df0[["darkop"]]){paste0(x, y)})
  dfFill0 <- df0 |> select(all_of(select0)) |> unique()
  # dfFill0 |> glimpse()

  ### Initialize plot
  p0      <- df0 |> ggplot(aes(ymax=ymax, ymin=ymin, xmax=xMax0, xmin=xMin0))

  ### Add geoms
  p0      <- p0 + geom_rect(aes(fill=reorder(sector, value)))
  p0      <- p0 + geom_text(x=4.25, aes(y=labelPosition, label=""), size=5)

  ### Format coords
  p0      <- p0 + coord_polar(theta="y")

  ### Add scales
  p0      <- p0 + scale_fill_manual (
    lgdTitle0,
    breaks = dfFill0 |> pull(sector) |> rev(),
    values = dfFill0 |> pull(all_of(colorCol)) |> rev()
    # breaks = dfFill0[["sector"]] |> rev(),
    # values = dfFill0[[colorCol]] |> rev()
  ) ### End scale_fill_manual
  p0      <- p0 + xlim(xLims0)

  ### Add tgheme and labels
  p0      <- p0 + theme_void()
  p0      <- p0 + labs(fill=lgdTitle0, title=title0)

  ### Return plot
  return(p0)
}


### Delta Functions ---------------------------
#### Join Impacts, calculate deltas
### Columns that are different between the two dataframes:
### Columns that are the same across the two dataframes:
joinDeltaResults <- function(
    list0, ### List with named elements containing two data frames to join
    diff0 = c("driverValue", "annual_impacts"), ### Columns that differ between data frames that we want to keep
    join0 = c("sector", "variant", "state", "model", "year"), ### Columns that we want to join on
    drop0 = c("impactYear", "impactType", "region", "postal", "model_type", "sectorprimary", "includeaggregate") ### Columns that we want to drop from one of the data frames
){
  ### Names
  lNames0 <- list0  |> names()
  lDiff0  <- lNames0 |> map(function(name_i){name_i |> paste0("_", diff0)}) |> set_names(lNames0)
  ### Rename columns
  df1     <- list0[[1]]
  df2     <- list0[[2]]
  ### Drop columns from one data frame and join
  select2 <- c(join0, diff0)
  df2     <- df2 |> select(all_of(select2))
  rm(select2)
  ### Rename columns
  df1     <- df1 |> rename_at(c(diff0), ~lDiff0[[1]])
  df2     <- df2 |> rename_at(c(diff0), ~lDiff0[[2]])
  ### Join data
  select2 <- c(join0, diff0)
  df0     <- df1 |> left_join(df2, by=join0)
  rm(df1, df2)
  ### Mutate data
  delta0  <- diff0 |> paste0("_delta")
  pct0    <- diff0 |> paste0("_deltaPct")
  df0[,delta0] <- df0[,lDiff0[[1]]] - df0[,lDiff0[[2]]]
  df0[,pct0  ] <- df0[,delta0     ] / df0[,lDiff0[[1]]] * 1e2
  ### Return
  return(df0)
}


### Utilities  -------------------------
### Function to load data
funLoadData <- function(path0, name0="obj_i"){
  newEnv      <- new.env()
  expr0       <- substitute(a |> load(envir=b), list(a=path0, b=newEnv))
  expr0 |> eval()
  obj0        <- name0 |> get(envir=newEnv, inherits=F)
  rm(newEnv)
  return(obj0)
}

### Write a function to format limits
formatPlotBreaks <- function(vals0, round0, n.breaks0=6){
  nonNA0   <- vals0  |> get_matches(y=c(NA, NaN, Inf, -Inf), matches=F)
  hasVals0 <- nonNA0 |> length()
  ### Return if there are no non-missing values
  if(!hasVals0) return()
  ### Otherwise, multiply values by tens and get limits
  p10     <- round0 + 1
  tens0   <- 10**(p10)
  vals1   <- nonNA0 * tens0
  lims0   <- nonNA0  |> range(na.rm=T)
  ### Take the floor and ceiling of the limits and then divide by tens
  lims0   <- c(lims0 |> floor(), lims0 |> ceiling())
  lims0   <- lims0 / tens0
  vals1   <- vals1 / tens0
  ### Zero out values
  less0   <- lims0[1] < 0
  more0   <- lims0[2] > 0
  doZero0 <- !(less0 & more0)
  if(doZero0) {
    if(less0) lims0 <- lims0[1] |> c(0)
    if(more0) lims0 <- 0 |> c(lims0[2])
  } else {
    abs0  <- lims0 |> abs()
    max0  <- abs0  |> max()
    lims0 <- c(-max0, max0)
  } ### End if(doZero0)
  ### Get a sequence of breaks
  vals0 <- seq(lims0[1], lims0[2], length.out=n.breaks0 + 1)
  vals0 <- vals0 |> round(round0)
  ### Return
  return(vals0)
}

### Themes  -------------------------
# theme_min0       <- theme_minimal()
### Stacked bar theme
theme_stackedBar <- function(theme0=theme_minimal()){
  ### Axis title and text
  theme0 <- theme0 +
    theme(axis.text    = element_text(size   = 11)) +
    theme(axis.title   = element_text(size   = 14)) +
    theme(axis.title.x = element_text(margin = margin(t= 5 , r= 0, b= 0, l= 0))) +
    theme(axis.title.y = element_text(margin = margin(t= 0 , r=10, b= 0, l= 0)))

  ### Legend title and text
  theme0 <- theme0 +
    theme(legend.text  = element_text(size   = 10)) +
    theme(legend.title = element_text(size   = 10, face="bold"))

    ### Plot title and text
    theme0 <- theme0 +
    theme(plot.title   = element_text(size   = 16, face="bold")) +
    theme(plot.title.position = "plot")

  ### Return(return)
  return(theme0)
}

### Basic box and whisker
theme_basicBoxWhisker <- function(
    theme0     = theme_minimal(),
    legend.key = NULL
){
  ### Axis title and text
  theme0 <- theme0 +
    theme(axis.text    = element_text(size   = 12)) +
    theme(axis.title   = element_text(size   = 12)) +
    theme(axis.title.x = element_text(margin = margin(t= 5 , r= 0, b= 0, l= 0)))

  ### Legend title and text
  theme0 <- theme0 +
    theme(legend.position   = "none") +
    # theme(legend.key        = element_blank()) +
    theme(legend.key        = legend.key) +
    theme(legend.background = element_blank())

  ### Plot title and text
  theme0 <- theme0 +
    theme(plot.title   = element_text(size   = 16, face="bold"))

  ### General margins
  theme0 <- theme0 +
    theme(plot.margin  = margin(l= 5, r= 5))

  ### Return(return)
  return(theme0)
}

### Theme for region map
### Basic box and whisker
theme_regMap <- function(
    theme0=theme_minimal(),
    legend.key = NULL
){
  ### Axis title and text
  theme0 <- theme0 +
    theme(axis.text    = element_text(size   = 12)) +
    theme(axis.title   = element_text(size   = 12)) +
    theme(axis.title.x = element_text(margin = margin(t= 5 , r= 0, b= 0, l= 0))) +
    theme(axis.ticks   = element_blank()) +
    theme(axis.text    = element_blank())

  ### Legend title and text
  theme0 <- theme0 +
    theme(legend.title      =  element_text(size=12)) +
    theme(legend.text       =  element_text(size=10)) +
    theme(legend.position   = "right") +
    # theme(legend.key        = element_blank()) +
    theme(legend.key        = legend.key) +
    theme(legend.key.size   = unit(1, "cm")) +
    theme(legend.background = element_blank()) +
    theme(legend.box.margin = margin(t= 1, l= 1))

  ### Plot title and text
  theme0 <- theme0 +
    theme(plot.title   = element_text(size   = 16, face="bold"))

  ### Panel stuff
  theme0 <- theme0 +
    theme(panel.background  = element_rect(linewidth=0.5, linetype="solid", colour="white", fill="white")) +
    theme(panel.grid.major  = element_line(linewidth=0.5, linetype="solid", colour="white")) +
    theme(panel.grid.minor  = element_line(linewidth=0.5, linetype="solid", colour="white"))

  ### Plot margin
  theme0 <- theme0 +
    theme(plot.margin  = margin(l = 5, r = 5))

  ### Return
  return(theme0)
}

theme_state_mortal <- function(
    theme0 = theme_minimal()
){
  ### Axis stuff
  theme0 <- theme0 +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.text.x  = element_blank()) +
    theme(axis.text.y  = element_blank())

  #### Legend stuff
  theme0 <- theme0 +
    theme(legend.title      = element_text(size=10)) +
    theme(legend.text       = element_text(size=9)) +
    theme(legend.position   = "right") +
    theme(legend.box.margin = margin(t = 1, l = 1)) +
    theme(legend.key.size   = unit(1, "cm"))

  ### Panel stuff
  theme0 <- theme0 +
    theme(panel.background = element_rect(linewidth=0.5, linetype="solid", colour="white", fill="white")) +
    theme(panel.grid.major = element_line(linewidth=0.5, linetype="solid", colour="white")) +
    theme(panel.grid.minor = element_line(linewidth=0.5, linetype="solid", colour="white"))

  ### Return
  return(theme0)
}


theme_state <- function(
    theme0 = theme_minimal()
){
  ### Axis stuff
  theme0 <- theme0 +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.text.x  = element_blank()) +
    theme(axis.text.y  = element_blank())

  ### Legend stuff
  theme0 <- theme0 +
    theme(legend.title      = element_text(size=10)) +
    theme(legend.text       = element_text(size=9)) +
    theme(legend.position   = "right") +
    theme(legend.box.margin = margin(t = 1, l = 1)) +
    theme(legend.key.size   = unit(0.5, "cm"))

  ### Panel stuff
  theme0 <- theme0 +
    theme(panel.background = element_rect(linewidth=0.5, linetype="solid", colour="white", fill="white")) +
    theme(panel.grid.major = element_line(linewidth=0.5, linetype="solid", colour="white")) +
    theme(panel.grid.minor = element_line(linewidth=0.5, linetype="solid", colour="white"))

  ### Return
  return(theme0)
}

### Color stuff -------------------------

#### Function to create list of sectors
fun_sectorTypes <- function(x0=1){
  list0 <- list()
  list0[["labor"    ]] <- c("Labor")
  list0[["ag"       ]] <- c("CIL Agriculture")
  list0[["eco"      ]] <- c("Marine Fisheries", "Winter Recreation", "Water Quality")
  list0[["elec"     ]] <- c("Electricity Demand and Supply", "Electricity Transmission and Distribution")
  list0[["infra"    ]] <- c("Rail", "Roads", "Urban Drainage", "Transportation Impacts from High Tide Flooding",
                            "Coastal Properties", "Inland Flooding", "Wind Damage")
  list0[["health"   ]] <- c("Climate-Driven Changes in Air Quality", "Valley Fever", "Wildfire", "Southwest Dust",
                            "CIL Crime", "Suicide", "Vibriosis")
  list0[["atsHealth"]] <- c("ATS Temperature-Related Mortality")
  list0[["cilHealth"]] <- c("CIL Temperature-Related Mortality")
  # list0[["atsHealth"]] <- c("ATS Temperature-Related Mortality") |> c(list0[["health"]])
  # list0[["cilHealth"]] <- c("CIL Temperature-Related Mortality") |> c(list0[["health"]])
  return(list0)
}


#### Function to match graph labels to sectors
fun_sectorLabels <- function(
    col0      = c("graphLabel"),
    join0     = c("sector"),
    sectors0  = c("ATS Temperature-Related Mortality", "CIL Temperature-Related Mortality", "Climate-Driven Changes in Air Quality",
                  "Electricity Demand and Supply", "Electricity Transmission and Distribution", "Marsh Migration (Primary)",
                  "Marsh Migration (Secondary)", "Transportation Impacts from High Tide Flooding"),
    labels0   = c("ATS Temp Mortality", "CIL Temp Mortality", "Climate-Driven AQ",
                  "Elec. Demand & Supply", "Elec. Trans. & Distr.", "Marsh Migration (P)",
                  "Marsh Migration (S)", "Transport. Impacts HTF"),
    physical0 = c("Climate-Driven Changes in Air Quality", "Asphalt Roads", "ATS Temperature-Related Mortality",
                  "CIL Crime", "CIL Temperature-Related Mortality", "Extreme Temperature",
                  "Labor", "Lyme Disease", "Marsh Migration (Secondary)", "Outdoor Recreation",
                  # "Rail", "Roads",
                  "Southwest Dust", "Suicide", "Valley Fever", "Vibriosis", "Wildfire"),
    variants0 = c("Climate-Driven Changes in Air Quality", "ATS Temperature-Related Mortality*",  "CIL Agriculture",
                  "CIL Temperature-Related Mortality", "Coastal Properties", "Electricity Transmission and Distribution",
                  "Extreme Temperature*", "Forestry Loss", "Transportation Impacts from High Tide Flooding",
                  "Marsh Migration (Primary)", "Marsh Migration (Secondary)", "Outdoor Recreation", "Rail", "Roads"),
    df0       = "co_sectors" |> get_frediDataObj(listSub="frediData") |> select(c("sector_id", "sector_label")) |> rename_at(c("sector_label"), ~c("sector"))
){
  ### Check length
  nSectors0 <- sectors0 |> length()
  nLabels0  <- labels0  |> length()
  pass0     <- nSectors0 == nLabels0
  if(!pass0) {
    msgPass0 <- paste0("Error: Length of sectors0 and labels0 must match! Exiting...") |> message()
    return(msgPass0)
  } ### End if(!pass0)

  ### Create a tibble with sectors and new labels
  df1     <- tibble(sector = sectors0)
  df1     <- df1 |> mutate(newLabs = labels0)
  # df1 |> glimpse(); df0 |> glimpse()

  ### Merge and arrange data
  ### Then replace NA values
  ### Rename
  join0   <- c("sector")
  from0   <- c("newLabs")
  to0     <- c(col0)
  df0     <- df0 |> left_join(df1, by=join0)
  df0     <- df0 |> arrange_at(c(join0))
  df0     <- df0 |> mutate(newLabs = case_when(newLabs |> is.na() ~ sector, .default=newLabs))
  df0     <- df0 |> rename_at(c(from0), ~to0)
  rm(join0, from0, to0)

  ### Return
  return(df0)
}



#### Function to add sector attributes
# physical0 = c("Climate-Driven Changes in Air Quality", "Asphalt Roads", "ATS Temperature-Related Mortality",
#               "CIL Crime", "CIL Temperature-Related Mortality", "Extreme Temperature",
#               "Labor", "Lyme Disease", "Marsh Migration (Secondary)", "Outdoor Recreation",
#               # "Rail", "Roads",
#               "Southwest Dust", "Suicide", "Valley Fever", "Vibriosis", "Wildfire")
# variants0 = c("Climate-Driven Changes in Air Quality", "ATS Temperature-Related Mortality*",  "CIL Agriculture",
#               "CIL Temperature-Related Mortality", "Coastal Properties", "Electricity Transmission and Distribution",
#               "Extreme Temperature*", "Forestry Loss", "Transportation Impacts from High Tide Flooding",
#               "Marsh Migration (Primary)", "Marsh Migration (Secondary)", "Outdoor Recreation", "Rail", "Roads")
addSectorAttribute <- function(
    df0       = fun_sectorLabels(),
    col0      = c("hasPhysical"), ### New column
    filter0   = c("sector_id"),
    sectors0  = c("Climate-Driven Changes in Air Quality", "Asphalt Roads", "ATS Temperature-Related Mortality",
                  "CIL Crime", "CIL Temperature-Related Mortality", "Extreme Temperature",
                  "Labor", "Lyme Disease", "Marsh Migration (Secondary)", "Outdoor Recreation",
                  # "Rail", "Roads",
                  "Southwest Dust", "Suicide", "Valley Fever", "Vibriosis", "Wildfire")
){
  ### Check if sector is in sectors0
  ### Rename column
  filter0 <- c("sector")
  from0   <- c("newCol")
  to0     <- c(col0)
  vals0   <- df0 |> pull(all_of(filter0))
  # df0     <- df0 |> mutate(idCol  = vals0)
  # df0     <- df0 |> mutate(newCol = idCol %in% sectors0)
  df0     <- df0 |> mutate(newCol  = vals0 %in% sectors0)
  df0     <- df0 |> rename_at(c(from0), ~to0)
  rm(filter0, from0, to0, vals0)

  ### Return
  return(df0)
}



#### Create a tibble with info
addSectorTypes <- function(
    # sectors0 = get_sectorInfo(),
  df0    = fun_sectorLabels(),
  types0 = fun_sectorTypes()
){
  ### Initialize df and make copies
  # df0      <- tibble(sector = sectors0)
  df1      <- df0
  df2      <- df0

  ### Add columns to data checking if sector is in the column
  idCols0  <- df0    |> names()
  newCols0 <- types0 |> names() |> sort()
  # newCols0 |> print()
  df1      <- newCols0 |> map(function(
    col_i,
    list_i = types0,
    df_i   = df1
  ){
    ### Initialize columns
    # col_i |> print()
    vals_i <- types0[[col_i]]
    # vals_i |> print()
    # df_i    <- df_i |> mutate_at(c(col_i), function(x, y1=vals1_i, y2=vals2_i){case_when(y1 %in% y2 ~ 1, .default=x)})
    # df_i    <- df_i |> mutate(newCol = case_when(sector %in% vals_i ~ 1, .default=newCol))
    df_i    <- df_i |> mutate(newCol = case_when(sector %in% vals_i ~ 1, .default=NA))
    df_i    <- df_i |> rename_at(c("newCol"), ~col_i)
    rm(col_i, vals_i)
    ### Return
    return(df_i)
  }) |> reduce(left_join, by=idCols0)
  # return(list1)

  # ### Join data
  # df1      <- df1 |> reduce(left_join, by=idCols0)
  # rm(join0)
  # # df1 |> glimpse()

  ### Pivot longer
  ### Filter NA values
  filter0 <- c("value0")
  drop0   <- filter0
  df1     <- df1 |> pivot_longer(cols=-all_of(idCols0), names_to ="lab0", values_to="value0")
  df1     <- df1 |> filter_at(c(filter0), function(x){!(x |> is.na())})
  df1     <- df1 |> select(-all_of(drop0))
  # df1     <- df1 |> distinct()
  # df1 |> glimpse()
  rm(filter0, drop0)

  ### Bind sectors without representation
  filter0 <- c("sector")
  sort0   <- c("lab0", "sector")
  vals0   <- df1 |> pull(sector) |> unique()
  df2     <- df2 |> filter_at(c(filter0), function(x, y=vals0){!(x %in% y)})
  df2     <- df2 |> mutate(lab0 = "other")
  df0     <- df1 |> rbind(df2)
  df0     <- df0 |> arrange_at(c(sort0))
  rm(filter0, sort0, vals0, df1, df2)

  # ### Summarize number in groups and join summary with df1
  # group0  <- c("lab0")
  # join0   <- group0
  # move0   <- c("nSectors")
  # before0 <- group0
  # sort0   <- c("nSectors", "lab0")
  # df1     <- df0 |> group_by_at(c(group0)) |> summarize(nSectors=n(), .groups="drop")
  # df0     <- df0 |> left_join(df1, by=join0)
  # df0     <- df0 |> relocate(all_of(move0), .before=all_of(before0))
  # df0     <- df0 |> arrange_at(c(sort0))
  # rm(group0, join0, move0, before0, sort0, df1)

  ### Mutate lab1 and lab2
  # lvls0   <- c("labor", "ag", "eco", "elec", "infra", "health", "cilHealth", "atsHealth") |> c("na")
  # lbls0   <- c("Labor", "Agriculture", "Ecosystems + Recreation", "Electricity", "Infrastructure") |>  c("Health" |> rep(3)) |> c("NA")
  lvls0   <- c("labor", "ag", "eco", "elec", "infra", "health", "cilHealth", "atsHealth")
  lbls0   <- c("Labor", "Agriculture", "Ecosystems + Recreation", "Electricity", "Infrastructure") |>  c("Health" |> rep(3))
  df0     <- df0 |> mutate(lab1 = lab0 |> factor(levels=lvls0, labels=lbls0) |> as.character())
  # df0     <- df0 |> mutate(lab2 = lab1 |> paste0(" (", nSectors, ")"))
  rm(lvls0, lbls0)

  ### Return
  return(df0)
}



#### Colors
c("#f2938c", "#d54309", "#c05600", "#ffbc78", "#97d4ea", "#86b98e", "#4d8055", "#446443", "#1a4480", "#936f38")
rainbow_6      <- function(x=1){c("#f2938c", "#d54309", "#ffbc78", "#4d8055", "#97d4ea", "#1a4480")}
rainbow_7      <- function(x=1){c("#f2938c", "#d54309", "#ffbc78", "#86b98e", "#4d8055", "#97d4ea", "#1a4480")}
rainbow_8      <- function(x=1){c("#EA7580FF", "#F6A1A5FF", "#F8CD9CFF", "#86b98e", "#4d8055", "#1BB6AFFF", "#088BBEFF", "#172869FF")}
rainbow_10      <- function(x=1){}
# colors_7      <-               c("#f2938c", "#d54309", "#ffbc78", "#86b98e", "#4d8055","#97d4ea", "#1a4480")
blues_7        <- function(x=1){c("#d9e8f6", "#aacdec", "#73b3e7", "#005ea2", "#0050d8", "#1a4480", "#162e51")}

##### Specify opacity/darkness
fun_darkop     <- function(x=1){"B3"}

### Remaining color
# "#D3D3D3"

#### Create a tibble with info
fun_colorSectors <- function(
    df0     = addSectorTypes(),
    colors6 = rainbow_6(),
    colors7 = rainbow_7(),
    colors8 = rainbow_8(),
    blues7  = blues_7(),
    dark0   = "B3"
){
  ### Add dark option
  df0     <- df0 |> mutate(darkop = dark0)

  ### Add color
  # lvls0   <- c("labor", "ag", "eco", "elec", "infra", "health", "cilHealth", "atsHealth") |> c("na")
  # lbls0   <- c(colors6[4], colors6[2], colors6[1], colors6[3], colors6[5]) |>  c(colors6[6] |> rep(3)) |> c("NA")
  lvls0   <- c("labor", "ag", "eco", "elec", "infra", "health", "cilHealth", "atsHealth")
  lbls0   <- c(colors6[4], colors6[2], colors6[1], colors6[3], colors6[5]) |>  c(colors6[6] |> rep(3))
  df0     <- df0 |> mutate(color0 = lab0   |> factor(levels=lvls0, labels=lbls0) |> as.character())
  rm(lvls0, lbls0)

  ### Add colors for region annual impact plots
  lvls0   <- c("ATS Temperature-Related Mortality", "Climate-Driven Changes in Air Quality",
               "Transportation Impacts from High Tide Flooding", "Wildfire", "Rail", "Wind Damage",
               "Roads", "Labor", "Suicide", "Southwest Dust"
               )
  lbls0   <- c("#f2938c", "#d54309", "#c05600", "#ffbc78", "#97d4ea", "#86b98e", "#4d8055", "#446443", "#1a4480", "#936f38")
  df0     <- df0 |> mutate(color1 = sector |> factor(levels=lvls0, labels=lbls0) |> as.character())
  rm(lvls0, lbls0)

  ### Return
  return(df0)
}


### End Script  -------------------------
