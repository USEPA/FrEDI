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

### Function to scale impacts by gdp
scaleImpacts <- function(
    df0,
    cols0 = "annual_impacts"
){
  ### Columns
  colsM    <- cols0 |> paste0("_multiplier")
  colsS    <- cols0 |> paste0("_scaled")
  ### Calculate multiplier and then apply multiplier
  ### (1 + annual_impacts / gdp_usd)**(-1)
  df0[,colsM] <- df0[,cols0]
  df0      <- df0 |> mutate_at(c(colsM), function(x, y=df0[["gdp_usd"]]){((1 + x) / y)**(-1)})
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
    df2  ### listATS[[name]][["all"]]
){
  df1 <- df1 |> filter(!(sector %in% "ATS Temperature Mortality"))
  df1 <- df1 |> bind_rows(df2); rm(df2)
  return(df1)
}

### Select plot data, calculate scaled values
formatPlotData <- function(
    df0, ### listAgg2[[name]]
    cols0    = c("annual_impacts")
){
  cols1 <- cols0 |> map(paste0, c("", "_scaled")) |> unlist()
  df0   <- df0   |> scaleImpacts(cols0=cols0)
  df0   <- df0   |> calcPerCap  (cols0=cols1)
  rm(cols1, cols0)
  return(df0)
}


### Function to multiply columns in a tibble by a constant
mutate_multiplyConstant <- function(df0, cols0, k0){
  df0 |> mutate_at(c(cols0), function(x, y=k0){x * y})
}

### Function to join with sector labels
join_sectorLabels <- function(
    df0,
    join0   = c("sector"),
    health0 = c("health", "atsHealth"), ### Which health categories to include (also: cilHealth, or health)
    other0  = FALSE, ### Whether to keep other
    df1     = fun_colorSectors()
    # df1   = fun_sectorLabels() |> addSectorTypes(types0=fun_sectorTypes()) |> fun_colorSectors()
){
  ### Filter out some categories
  filter0 <- c("lab0")
  # vals0   <- c("na", "health") |> c(health0)
  vals0   <- c("na", "health", "cilHealth", "atsHealth") |> get_matches(y=health0, matches=F)
  # vals0   <- c("na", "health", "cilHealth", "atsHealth") |> get_matches(y=health0, matches=F)
  if(!other0) vals0 <- vals0 |> c("other")
  df1     <- df1 |> filter_at(c(filter0), function(x, y=vals0){x |> get_matches(y=vals0, matches=F, type="matches")})

  ### Join with sector info
  join0   <- c("sector")
  to0     <- c("year")
  move0   <- df1 |> names() |> get_matches(y=join0, matches=FALSE)
  df0     <- df0 |> left_join(df1, by=join0)
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
    breaks = dfFill0 |> pull(category),
    values = dfFill0 |> pull(color0)
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
  rm(health0, other0, df1, join0)

  # ### Adjust values top trillions
  # df0     <- df0 |> mutate(value = annual_impacts * k0)

  ### Find order of sectors and categories, largest to smallest, then sort
  group1  <- c("sector")
  sum1    <- c(cols0)
  sort1   <- c(sum1, group1)
  df1     <- df0
  df1     <- df1 |> group_by_at(c(group1)) |> summarize_at(c(sum1), sum, na.rm=T) |> ungroup()
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
    # fill0  = colors_6 |> rev(),
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
  sum0    <- c("value")
  df0     <- df0 |> mutate(value = df0 |> pull(all_of(yCol0)))
  df0     <- df0 |> group_by_at(c(group0)) |> summarize_at(c(sum0), mean, na.rm=T) |> ungroup()
  rm(group0, sum0)

  ### Format titles and labels
  # str0    <- "\\("
  # str1    <- str0  |> paste0(unit0, ", ")
  # yLab0   <- yLab0 |> str_replace(str0, str1)

  ### Initial plot
  p0      <- df0 |> ggplot(aes(
    x    = reorder(sector, value, mean, na.rm=TRUE),
    y    = value
  ))

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
      sector1=c("ATS Temperature-Related Mortality"),
      sector2=c("Climate-Driven Changes in Air Quality", "Labor", "Rail", "Roads", "Suicide",
                "Transportation Impacts from High Tide Flooding", "Wildfire", "Wind Damage")
    ), ### End list
    # lims0     = NULL,
    # lims0     = list(sector1=c(0, 4000), sector2=c(NA, 600), other=c(NA, 40)),
    title0    = "U.S. Annual Climate-Driven Damages in 2090",
    subTitle0 = "By sector, colored by sector category (subset of all climate-related impacts)",
    # subTitle0 = "By sector, colored by sector category",
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
      # fill0     = fill0,
      theme0    = theme0
    ) ### End plot_basicBoxWhisker
  }) |> set_names(sectLbls)

  ### Return
  return(pList0)
}



### Regions ----------------------------
### Summarize values by region
sumByRegion <- function(
    df0,
    group0 = c("sector", "variant", "impactType", "impactYear", "model", "model_type", "region", , "sectorprimary", "includeaggregate", "year", "gdp_usd"),
    cols0  = c("annual_impacts")
){
  ### Summarize over regions
  df0   <- df0  |> group_by_at(c(group0)) |> summarize_at(c(cols0), sum, na.rm=T) |> ungroup()
  # ### Columns
  # cols1 <- cols0 |> map(paste0, c("", "_scaled")) |> unlist()
  # ### Summarize data, scale impacts, get per capita impacts
  # # df0  <- df0 |> getFilterDf(filters0="national", reverse0=T)
  # df0   <- df0  |> scaleImpacts(cols0=cols0)
  # df0   <- df0  |> calcPerCap(cols0 = cols1)
  ### Return
  return(df0)
}

### Function to format values for regional pie plot
calc_regPieMargin <- function(
    region0,
    df0,
    col0   = c("annual_impacts_percap"),
    group0 = c("region", "sector", "color0", "darkop"),
    colorR = "#D3D3D3"
){
  ### Filter to region
  df0     <- df0 |> filter(region %in% region0)

  ### Make a copy to calculate total impacts
  # dfCopy0 <- df0
  totImp0 <- df0 |> pull(all_of(col0)) |> sum(na.rm=T)

  ### Group and summarize
  ### Filter to impacts greater than zero and arrange descending
  df0     <- df0 |> group_by_at(c(group0)) |> summarize_at(c(col0), sum, na.rm=T) |> ungroup()
  df0     <- df0 |> filter_at(c(col0), function(x){x >= 0})
  df0     <- df0 |> arrange_at(c(col0), desc)
  df0$sector |> unique()
  ### Add row number and adjust sector
  df0     <- df0 |> mutate(RowNum = row_number())
  df0     <- df0 |> mutate(sector = case_when(RowNum <= 4 ~ sector, .default="Remaining"))
  df0     <- df0 |> mutate(sector = sector |> as.factor())
  # df0$sector |> unique() |> print()

  ### Group and summarize
  ### Filter to impacts greater than zero and arrange descending
  df0     <- df0 |> group_by_at(c(group0)) |> summarize_at(c(col0), sum, na.rm=T) |> ungroup()
  df0     <- df0 |> filter_at(c(col0), function(x){x >= 0})
  df0     <- df0 |> arrange_at(c(col0), desc)

  ### Factor sector and add color
  df0     <- df0 |> mutate(color0 = case_when(sector %in% "Remaining" ~ colorR, .default=color0))
  # df0$sector |> unique() |> print()
  ### Mutate values:
  ### - Compute ratio to total and get ymin, etc.
  ### - Compute the cumulative percentages (top of each rectangle)
  ### - Compute the bottom of each rectangle
  ### - Compute label position
  ### - Compute (a )good label(s)
  sumVal0 <- df0 |> pull(all_of(col0)) |> sum(na.rm=T)
  df0     <- df0 |> mutate(fraction = df0[[col0]] / sumVal0)
  df0     <- df0 |> mutate(ymax     = fraction |> cumsum())
  yMins0  <- df0 |> pull(ymax) |> head(n=-1)
  df0     <- df0 |> mutate(ymin     = 0 |> c(yMins0))
  df0     <- df0 |> mutate(labelPosition = (ymax + ymin) / 2)
  df0     <- df0 |> mutate(label    = (fraction*1e2) |> round(0) |> format(nsmall=0) |> paste0("%"))
  df0     <- df0 |> mutate(label2   = (sumVal0 / totImp0 * 1e2) |> round(1) |> format(nsmall=1) |> paste0("%"))

  ### Return
  return(df0)
}



### Format region pie plots
format_regPieChart <- function(
    df0,
    col0    = c("annual_impacts_percap"),
    join0   = c("sector"),
    group0  = c("region", "sector", "color0", "darkop"),
    health0 = c("health", "atsHealth"),
    other0  = FALSE, ### Whether to keep other
    naStr0  = c(NA, NaN, Inf, -Inf, "NA", "NaN"),
    colorR  = "#D3D3D3",
    df1     = fun_colorSectors()
){
  ### Format data
  filter0 <- c(col0)
  sort0   <- col0 |> c("sector")
  # df0     <- df0  |> filter_at(c(filter0), function(x, y=naStr0){!(x %in% y)})
  df0     <- df0 |> filter_at(c(filter0), function(x, y=naStr0){x |> get_matches(y=naStr0, matches=F, type="matches")})
  df0     <- df0 |> arrange_at(c(sort0))

  ### Join with sector info and drop missing values
  df0     <- df0 |> join_sectorLabels(join0=join0, health0=health0, other0=other0, df1=df1)
  df0     <- df0 |> filter(!lab0 |> is.na())
  rm(health0, other0, df1, join0)

  ### Iterate over regions
  region0 <- df0     |> pull(region) |> unique()
  df0     <- region0 |>
    map(calc_regPieMargin, df0=df0, col0=col0, group0=group0, colorR=colorR) |>
    bind_rows()

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
    theme0    = theme_void()
){
  ### Filter data
  df0    <- df0 |> filter(region %in% region0)
  df0    <- df0 |> mutate(value = df0[[col0]])

  ### Get title
  label0 <- df0     |> pull(label2) |> unique()
  title0 <- region0 |> paste(label0)

  ### Fill breaks
  dfFill0 <- df0 |> select(region, sector, color0) |> unique()
  # dfFill0 |> glimpse()

  ### Initialize plot
  p0      <- df0 |> ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(sector, value)))

  ### Add geoms
  p0      <- p0 + geom_rect()
  p0      <- p0 + geom_text(x=4.25, aes(y=labelPosition, label=""), size=5)

  ### Format coords
  p0      <- p0 + coord_polar(theta="y")

  ### Add scales
  p0      <- p0 + scale_fill_manual (
    lgdTitle0,
    # breaks = dfFill0 |> pull(sector),
    # values = dfFill0 |> pull(color0)
    breaks = dfFill0 |> pull(sector) |> rev(),
    values = dfFill0 |> pull(color0) |> rev()
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
  lNames0 <- list0 |> names()
  lDiff0  <- lNames0 |> map(paste0, "_", diff0) |> set_names(lNames0)
  ### Rename columns
  df1     <- list0[[1]] |> rename_at(c(diff0), ~lDiff0[[1]])
  df2     <- list0[[2]] |> rename_at(c(diff0), ~lDiff0[[2]])
  ### Drop columns
  select2 <- c(join0, diff0)
  df2     <- df2 |> select(all_of(select2))
  rm(select2)
  ### Join data
  df0     <- df1 |> left_join(df2, by=join0)
  rm(df1, df2)
  ### Mutate data
  delta0  <- diff0 |> paste0("_delta")
  pct0    <- diff0 |> paste0("_deltaPct")
  df0[[delta0]] <- df0[[lDiff0[[1]]]] - df0[[lDiff0[[2]]]]
  df0[[pct0  ]] <- df0[[delta0]] / df0[[lDiff0[[2]]]] * 1e2
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

  ### Summarize number in groups and join summary with df1
  group0  <- c("lab0")
  join0   <- group0
  move0   <- c("nSectors")
  before0 <- group0
  sort0   <- c("nSectors", "lab0")
  df1     <- df0 |> group_by_at(c(group0)) |> summarize(nSectors=n(), .groups="drop")
  df0     <- df0 |> left_join(df1, by=join0)
  df0     <- df0 |> relocate(all_of(move0), .before=all_of(before0))
  df0     <- df0 |> arrange_at(c(sort0))
  rm(group0, join0, move0, before0, sort0, df1)

  ### Mutate lab1 and lab2
  # lvls0   <- c("labor", "ag", "eco", "elec", "infra", "health", "cilHealth", "atsHealth") |> c("na")
  # lbls0   <- c("Labor", "Agriculture", "Ecosystems + Recreation", "Electricity", "Infrastructure") |>  c("Health" |> rep(3)) |> c("NA")
  lvls0   <- c("labor", "ag", "eco", "elec", "infra", "health", "cilHealth", "atsHealth")
  lbls0   <- c("Labor", "Agriculture", "Ecosystems + Recreation", "Electricity", "Infrastructure") |>  c("Health" |> rep(3))
  df0     <- df0 |> mutate(lab1 = lab0 |> factor(levels=lvls0, labels=lbls0) |> as.character())
  df0     <- df0 |> mutate(lab2 = lab1 |> paste0(" (", nSectors, ")"))
  rm(lvls0, lbls0)

  ### Return
  return(df0)
}

#### Colors
rainbow_6      <- function(x=1){c("#f2938c", "#d54309", "#ffbc78", "#4d8055", "#97d4ea", "#1a4480")}
rainbow_7      <- function(x=1){c("#f2938c", "#d54309", "#ffbc78", "#86b98e", "#4d8055", "#97d4ea", "#1a4480")}
rainbow_8      <- function(x=1){c("#EA7580FF", "#F6A1A5FF", "#F8CD9CFF", "#86b98e", "#4d8055", "#1BB6AFFF", "#088BBEFF", "#172869FF")}
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
  ### Add color
  # lvls0   <- c("labor", "ag", "eco", "elec", "infra", "health", "cilHealth", "atsHealth") |> c("na")
  # lbls0   <- c(colors6[4], colors6[2], colors6[1], colors6[3], colors6[5]) |>  c(colors6[6] |> rep(3)) |> c("NA")
  lvls0   <- c("labor", "ag", "eco", "elec", "infra", "health", "cilHealth", "atsHealth")
  lbls0   <- c(colors6[4], colors6[2], colors6[1], colors6[3], colors6[5]) |>  c(colors6[6] |> rep(3))
  df0     <- df0 |> mutate(color0 = lab0   |> factor(levels=lvls0, labels=lbls0) |> as.character())
  rm(lvls0, lbls0)

  ### Add another color and dark option
  lvls0   <- c("Labor", "CIL Agriculture", "Rail", "Roads", "Southwest Dust", "Wildfire", "Wind Damage",
               "ATS Temperature-Related Mortality", "Climate-Driven Changes in Air Quality",
               "Transportation Impacts from High Tide Flooding")
  lbls0   <- c(colors7[5], colors7[4], colors7[3], colors7[1], "#936f38", "#c05600", "#446443",
               colors7[7], colors7[6], colors7[2])
  df0     <- df0 |> mutate(color1 = lab0   |> factor(levels=lvls0, labels=lbls0) |> as.character())
  df0     <- df0 |> mutate(darkop = dark0)
  rm(lvls0, lbls0)


  ### Return
  return(df0)
}


### End Script  -------------------------
