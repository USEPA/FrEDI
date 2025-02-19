### Other report figure utilities

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

### Summarize region population scenario from state population
summarizeRegPop <- function(df0){
  ### Select columns and get distinct values
  group0   <- c("region", "year")
  sum0     <- c("pop")
  df0      <- df0 |> group_by_at(c(group0)) |> summarize_at(c(sum0)) |> ungroup()
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
  df0[[colsM]] <- (1 + df0[[cols0]] / df0[["gdp_usd"]])**(-1)
  df0[[colsS]] <- df0[[cols0]] * df0[[colsM]]
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
  df0[[colsS]] <- df0[[cols0]] / df0[[pop0]]
  rm(cols0, cols1)
  ### Return
  gc()
  return(df0)
}


### Function to format data for stacked bar plot by sector
##### Pre-process and organize
#### - Option A - without error bars
####     * Annual Impacts for Baseline
####       - Filter relevant data for three years of interest and assign category to each sector
####       - Summarize annual impacts
####       - Filter to to national
format_stackedBar_bySector <- function(
    df0,
    df1  = fun_colorSectors(),
    yrs0 = c(2050, 2070, 2090),
    k0   = 1e-12
){
  ### Filter data
  ### Mutate year as factor
  filter0 <- yrs0
  df0     <- df0 |> filter(year %in% yrs0)
  df0     <- df0 |> mutate(year = year |> as.factor())

  ### Join with sector info
  join0   <- c("sector")
  move0   <- df1 |> names() |> get_matches(y=join0, matches=FALSE)
  df0     <- df0 |> left_join(df1, by=join0)
  df0     <- df0 |> relocate(all_of(move0), .after=all_of(after0))
  df0     <- df0 |> rename_at(c("lab2"), ~c("category"))

  ### Filter out some categories
  filter0 <- c("lab0")
  vals0   <- c("na", "health", "cilHealth")
  df0     <- df0 |> filter_at(c(filter0), function(x, y=vals0){!(x %in% y)})

  ### Adjust values top trillions
  df0     <- df0 |> mutate(value = annual_impacts * k0)

  ### Order from largest to smallest
  group0  <- c("category")
  sum0    <- c("value")
  sort0   <- c(sum0, group0)
  df1     <- df0
  df1     <- df1 |> group_by_at(c(group0)) |> summarize_at(c(sum0), sum, na.rm=T) |> ungroup()
  df1     <- df1 |> arrange_at(c(sort0))
  rm(group0, sum0, sort0)

  ### Factor category
  lvls0   <- df1 |> pull(category)
  sort0   <- c("category")
  df0     <- df0 |> mutate(category = category |> factor(levels=lvls0))
  df0     <- df0 |> arrange_at(c(sort0))

  ### Return
  return(df0)
}


### Format data for box and whisker plot
format_boxWhisker <- function(
    df0,
    df1  = fun_colorSectors(),
    yrs0 = 2090,
    k0   = 1e-9
){
  ### Filter data
  ### Mutate year as factor
  filter0 <- yrs0
  df0     <- df0 |> filter(year %in% yrs0)

  ### Join with sector info
  join0   <- c("sector")
  move0   <- df1 |> names() |> get_matches(y=join0, matches=FALSE)
  df0     <- df0 |> left_join(df1, by=join0)
  df0     <- df0 |> relocate(all_of(move0), .after=all_of(after0))
  df0     <- df0 |> rename_at(c("lab2"), ~c("category"))

  ### Filter out other
  ### Filter out some categories
  filter0 <- c("lab0")
  vals0   <- c("na", "health", "cilHealth")
  df0     <- df0 |> filter_at(c(filter0), function(x, y=vals0){!(x %in% y)})

  ### Adjust values top trillions
  df0     <- df0 |> mutate(value = annual_impacts * k0)

  ### Find order of sectors and categories, largest to smallest, then sort
  group0  <- c("sector")
  sum0    <- c("value")
  sort0   <- c(sum0, group0)
  df1     <- df0
  df1     <- df1 |> group_by_at(c(group0)) |> summarize_at(c(sum0), sum, na.rm=T) |> ungroup
  df1     <- df1 |> arrange_at(c(sort0), desc)
  lvls0   <- df1 |> pull(all_of(group0))
  ### Factor category and arrange
  df0     <- df0 |> mutate_at(c(group0), factor, levels=lvls0)
  df0     <- df0 |> arrange_at(c(group0))
  rm(group0, sum0, sort0, lvls0, df1)


  ### Find order of sectors and categories, largest to smallest, then sort
  group0  <- c("category")
  sum0    <- c("value")
  sort0   <- c(sum0, group0)
  df1     <- df0
  df1     <- df1 |> group_by_at(c(group0)) |> summarize_at(c(sum0), sum, na.rm=T) |> ungroup
  df1     <- df1 |> arrange_at(c(sort0))
  lvls0   <- df1 |> pull(all_of(group0))
  ### Factor category and arrange
  df0     <- df0 |> mutate_at(c(group0), factor, levels=lvls0)
  df0     <- df0 |> arrange_at(c(group0))
  rm(group0, sum0, sort0, lvls0, df1)

  ### Arrange
  # sort0   <- c("sector", "category")
  sort0   <- c("category", "sector")
  df0     <- df0 |> arrange_at(c(group0))

  ### Return
  return(df0)
}


### Function to create a box and whisker plot
plot_basicBoxWhisker <- function(
    df0,
    df1       = fun_colorSectors(),
    sectors0  = c("ATS Temperature-Related Mortality"),
    title0    = "U.S. Annual Climate-Driven Damages in 2090",
    subTitle0 = "By sector, colored by sector category (subset of all climate-related impacts)",
    lgdTitle0 = "",
    xLab0  = "",
    # yLab0  = "Damages (Billions, $2015)",
    yLab0  = "Damages ($2015)",
    yLims0 = c(0, 4000),
    xLabs0 = function(x) str_wrap(x, width=25),
    yLabs0 = scales::dollar_format(),
    fill0  = colors_6 |> rev(),
    theme0 = theme_basicBoxWhisker(),
    unit0  = "Billions"
){
  ### Filter data to sector(s)
  filter0 <- c("sector")
  vals0   <- sectors0
  df0     <- df0 |> filter_at(c(filter0), function(x, y=vals0){(x %in% y)})
  rm(filter0, vals0)

  ### Print range and adjust y limits if necessary
  df0 |> pull(value) |> range(na.rm=T) |> print()

  ### Factor values
  mutate0 <- c("sector")
  lvls0   <- df1 |> pull(sector)
  lbls0   <- df1 |> pull(graphLabel)
  df0     <- df0 |> mutate_at(c(mutate0), factor, levels=lvls0, labels=lbls0)
  df0     <- df0 |> mutate_at(c(mutate0), as.character)
  rm(mutate0, lvls0, lbls0)

  ### Group values
  group0  <- c("sector", "category")
  sum0    <- c("value")
  df0     <- df0 |> group_by(c(group0)) |>
    summarize_at(c(sum0), mean, na.rm=TRUE) |> ungroup()
  rm(group0, sum0)

  ### Format titles and labels
  str0    <- "\\("
  str1    <- str0  |> paste0(unit0, ", ")
  yLab0   <- yLab0 |> str_replace(str0, str1)

  ### Initial plot
  p0      <- df0 |> ggplot() + geom_bar(
    aes(
      x    = reorder(sector, value, mean, na.rm=TRUE),
      y    = value,
      fill=category
    ), ### End aes
    # position = "stack",
    stat     = "identity",
    alpha    = 0.6,
    width    = 0.7
  ) ### End geom_bar

  ### Flip axes
  p0      <- p0 + coord_flip()

  ### Add scales
  p0      <- p0 + scale_y_continuous(sec.axis=dup_axis(), limits=yLims0, labels=yLabs0)
  p0      <- p0 + scale_x_discrete  (xLab0, labels=xLabs0)
  p0      <- p0 + scale_fill_manual (lgdTitle0, values=fill0)

  ### Add themes
  p0      <- p0 + theme0

  ### Return
  return(p0)
}

### Function to divide basicBoxWhisker plots into sector categories
plot_basicBoxWhisker2 <- function(
    df0,
    sectorList = list(
      sector1=c("ATS Temperature-Related Mortality"),
      sector2=c("Climate-Driven Changes in Air Quality", "Labor", "Rail", "Roads", "Suicide",
                "Transportation Impacts from High Tide Flooding", "Wildfire", "Wind Damage")
    ), ### End list
    lims0     = list(),
    # lims0     = list(sector1=c(0, 4000), sector2=c(NA, 600), other=c(NA, 40)),
    title0    = "U.S. Annual Climate-Driven Damages in 2090",
    subTitle0 = "By sector, colored by sector category (subset of all climate-related impacts)",
    # subTitle0 = "By sector, colored by sector category",
    lgdTitle0 = "Sector Category",
    unit0     = "Billions",
    fill0     = colors_6 |> rev(),
    theme0    = theme_basicBoxWhisker()
){
  ### Sector names
  sectors0  <- df0 |> pull(sector) |> unique()
  other0    <- sectors0 |> get_matches(y=sectorList |> unlist() |> unique(), matches=F)
  sectorList[["other"]] <- other0
  sectNames <- sectorList |> names()

  ### Plot list
  pList0    <- sectNames |> map(function(
    name_i,
    sectors_i = sectorList[[name_i]],
    lims_i    = lims0[[name_i]]
  ){
    df0 |> plot_basicBoxWhisker(
      sectors0  = sectors_i,
      title0    = title0,
      subTitle0 = subTitle0,
      yLims0    = lims0,
      fill0     = fill0,
      theme0    = theme0,
      unit0     = unit0
    ) ### End plot_basicBoxWhisker
  }) |> set_names()

  ### Return
  return(pList0)
}


### Function to format values for regional pie plot
calc_regPieMargin <- function(
    region0,
    df0 = df_regPieMargin,
    df1 = dfSectLbls
){
  # ### Filter data
  # filter0 <- c("impacts_percap")
  # vals0   <- c(NA, NaN, "NA", "NaN")
  # df0     <- df0 |> filter(region %in% region0)
  # df0     <- df0 |> filter_at(c(filter0), function(x, y=vals0){!(x %in% y)})
  # rm(filter0, vals0)

  ### Make a copy to calculate total impacts
  # dfCopy0 <- df0
  totImp0 <- df0 |> pull(impacts_percap) |> sum(na.rm=T)

  ### Group and summarize
  ### Filter to impacts greater than zero and arrange descending
  group0  <- c("sector", "color_f")
  sum0    <- c("impacts_percap")
  df0     <- df0 |> group_by_at(c(group0)) |> summarize_at(c(sum0), sum, na.rm=T) |> ungroup()
  df0     <- df0 |> filter_at(c(sum0), function(x){x >= 0})
  df0     <- df0 |> arrange_at(c(sum0), desc=TRUE)
  rm(group0, sum0)

  ### Add row number and sector
  df0     <- df0 |> mutate(RowNum = row_number())
  df0     <- df0 |> mutate(sector = case_when(RowNum <= 4 ~ sector, .default="Remaining"))

  ### Group and summarize
  ### Filter to impacts greater than zero and arrange descending
  group0  <- c("sector")
  sum0    <- c("impacts_percap")
  df0     <- df0 |> group_by_at(c(group0)) |> summarize_at(c(sum0), sum, na.rm=T) |> ungroup()
  df0     <- df0 |> filter_at(c(sum0), function(x){x >= 0})
  df0     <- df0 |> arrange_at(c(sum0), desc=TRUE)
  rm(group0, sum0)

  ### Factor sector and add color
  df0     <- df0 |> mutate(sector = sector |> as.factor())
  df0     <- df0 |> mutate(sector = case_when(RowNum <= 4 ~ sector, .default="Remaining"))

  ### Join with color labels
  select0 <- c("sector", "lab0", "color0")
  filter0 <- c("lab0")
  vals0   <- c("health", "cilHealth")
  join0   <- c("sector")
  df1_0   <- df1 |> filter_at(c(filter0), function(x, y=vals0){!(x %in% y)})
  df0     <- df0 |> left_join(df1, by=join0)
  rm(filter0, vals0, join0, df1_0)

  ### Mutate values:
  ### - Compute ratio to total and get ymin, etc.
  ### - Compute the cumulative percentages (top of each rectangle)
  ### - Compute the bottom of each rectangle
  ### - Compute label position
  ### - Compute (a )good label(s)
  sumVal0 <- df0 |> pull(impacts_percap) |> sum(na.rm=T)
  df0     <- df0 |> mutate(fraction = impacts_percap / sumVal0)
  df0     <- df0 |> mutate(ymax     = fraction |> cumsum())
  yMins0  <- df0 |> pull(ymax) |> head(n=-1)
  df0     <- df0 |> mutate(ymin     = 0 |> c(yMins0))
  df0     <- df0 |> mutate(labelPosition = (ymax + ymin) / 2)
  df0     <- df0 |> mutate(label    = (fraction*1e2) |> round(0) |> format(nsmall=0) |> paste0("%"))
  df0     <- df0 |> mutate(label2   = (sumVal0 / totImp0 * 1e2) |> round(1) |> format(nsmall=1) |> paste0("%"))

  ### Return
  return(df0)
}

### Function to plot region pie margins
### Function to make plot
plot_regPieMargin <- function(
    region0,
    df0,
    df1 = dfSectLbls,
    lgdTitle0 = "Top 4 Sectors",
    xLims0    = c(2, 4),
    theme0    = theme_void()
){
  ### Filter data
  df0    <- df0 |> filter(region %in% region0)

  ### Get title
  label0 <- df0     |> pull(label2) |> unique()
  title0 <- region0 |> paste0(label0)

  ### Initialize plot
  p0 <- df0 |> ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(sector, impacts_percap)))

  ### Add geoms
  p0 <- p0 + geom_rect()
  p0 <- p0 + geom_text(x=4.25, aes(y=labelPosition, label=""), size=5)

  ### Format coords
  p0 <- p0 + coord_polar(theta="y")

  ### Add scales
  # p0 <- p0 + scale_fill_manual(values =rev(p_temp$color_f))
  p0 <- p0 + scale_fill_manual(values=p_temp |> pull(sector) |> rev(), values=p_temp |> pull(color_f) |> rev())
  p0 <- p0 + xlim(xLims0)

  ### Add tgheme and labels
  p0 <- p0 + theme_void()
  p0 <- p0 + labs(fill=lgdTitle0, title=title0)

  ### Return plot
  return(p0)
}
