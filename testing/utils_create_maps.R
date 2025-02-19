### Function to adjust suicides
adjustSector1_bySector2 <- function(
    df0,
    sector1 = "ATS Temperature-Related Mortality",
    sector2 = "Suicide",
    join0   = c("state", "model", "year"),
    cols0   = c("annual_impacts")
){
  ### Columns
  cols2   <- cols0 |> paste0("_sector2")

  ### Separate data
  df1     <- df0 |> filter(sector %in% sector1)
  df2     <- df0 |> filter(sector %in% sector2)
  df0     <- df0 |> filter(!(sector %in% sector1))
  # rm(sector1, sector2)

  ### Format data for second sector
  ### - Drop and rename some columns for join
  ### - Suicide has only one each of variant, impact type, and impact year
  ### - Then join to ATS data
  # dropS   <- c("sector", "variant", "sectorprimary", "includeaggregate")
  select1 <- join0 |> c(cols0)
  df2     <- df2   |> select(all_of(select2))
  df2     <- df2   |> rename_at(c(cols0), ~cols2)
  df2     <- df2   |> unique()
  # return(list(df1=df1, df2=df3))

  ### Format data for first sector, then join with data for second sector
  df1     <- df1 |> left_join(df2, by=join1)
  rm(join1, df2)
  df1 |> is.na() |> sum() |> print()

  ### - Subtract sector 2 impacts from sector 1 impacts, then drop sector 2 impacts
  drop1   <- cols2
  df1[[cols0]] <- df1[[cols0]] - df1[[cols2]]
  df1     <- df1 |> select(-any_of(drop1))
  df1 |> is.na() |> sum() |> print()
  rm(drop1)

  ### Now rbind original results
  df0     <- df0 |> rbind(df1)
  rm(df1)

  ### Return
  gc()
  return(df0)
}

### Get state polygons
getStatePolygons <- function(x0=1){
  str0 <- "state"
  df0  <- str0 |> ggplot2::map_data()
  df0  <- df0  |> rename_at(c("region", "subregion"), ~c("state_lc", "state_subregion"))
  return(df0)
}

### Function to add data to a map
addData2Map <- function(
    df0,  ### E.g. frediAdjSum
    join0 = c("state_lc"),
    gons0 = getStatePolygons()
){
  ### Add impact data to state map data
  df0   <- df0 |> mutate(state_lc=state |> tolower())
  gons0 <- gons0 |> left_join(df0, by="state_lc") |> relocate(c("state_lc"), .after="state")
  gons0 |> pull(state_lc) |> unique() |> print()
  ### Filter data
  gons0 <- gons0 |> filter(!(state |> is.na()))
  gons0 |> pull(state_lc) |> unique() |> print()
  ### Return
  return(gons0)
}

### Map theme
getMapTheme <- function(x=1){
  theme0 <- theme_minimal() +
    ### Legends
    theme(legend.title      = element_text(size=10)) +
    theme(legend.text       = element_text(size=9)) +
    theme(legend.position   = "right") +
    theme(legend.key.size   = unit(1, "cm")) +
    theme(legend.box.margin = margin(t = 1, l = 1)) +
    ### Axis ticks
    theme(axis.ticks.x=element_blank()) +
    theme(axis.ticks.y=element_blank()) +
    theme(axis.text.x = element_blank()) +
    theme(axis.text.y = element_blank()) +
    ### Panel
    theme(panel.background = element_rect(linewidth=0.5, linetype="solid", colour="white", fill="white")) +
    theme(panel.grid.major = element_line(linewidth=0.5, linetype="solid", colour="white")) +
    theme(panel.grid.minor = element_line(linewidth=0.5, linetype="solid", colour="white"))
  return(theme0)
}

### Function to plot a state map
plotStateMap <- function(
    df0,
    col0      = "annual_impacts",
    # lims0     = list(),
    lims0     = c(-5, 325),
    colors0   = list(
      low      = "#010d5e",
      mid      = "white",
      high     = "#701201",
      na.value = "grey",
      n.breaks = 6
    ), ### End list
    outline   = "gray8",
    xLab0     = "",
    yLab0     = "",
    lgdLab0   = "Total Impacts\nbillion USD",
    ggTitle0  = "Annual Climate-Driven Damages in 2090 by State",
    subTitle0 = "Subset of Climate-Related Impacts",
    theme0    = getMapTheme()
){
  plot0 <- df0 |> ggplot(aes(long, lat, group=group))
  ### Create plot 1
  plot0 <- plot0 +
    geom_polygon(aes(fill=.data[[col0]]), color=outlines) +
    scale_fill_gradient2(
      name     = lgdLabs0[[1]],
      limits   = lims0[[1]],
      low      = colors0[["low"]],
      mid      = colors0[["mid"]],
      high     = colors0[["high"]],
      na.value = colors0[["na.value"]],
      n.breaks = colors0[["n.breaks"]],
      guide    = guide_colorsteps(ticks=TRUE, ticks.linewidth=1, show.limits=TRUE)
    ) ### End scale_fill_gradient2
  plot0 <- plot0 + theme0
  plot0 <- plot0 + xlab(xLabs0[[1]]) + ylab(yLabs0[[1]])
  plot0 <- plot0 + ggtitle(ggTitle0[[1]], subTitle0[[1]])
  return(plot0)
}

### Function to map the plotStateMap function over a set of columns
map2StateMap <- function(
    df0,
    cols0     = list(p1="annual_impacts", p2="annual_impacts_percap"),
    names0    = list(p1="totals", p2="percap"),
    # lims0     = list(),
    lims0     = list(p1=c(-5, 325), p2=c(0, 1.5)),
    # lims0     = list(p1=c(5, 540), p2=c(0.75, 2.2)),
    colors0   = list(
      low      = "white",
      mid      = "white",
      high     = "#DD8047",
      na.value = "grey",
      n.breaks = 6
    ), ### End list
    outlines  = "gray8",
    xLabs0    = list(p1="", p2=""),
    yLabs0    = list(p1="", p2=""),
    lgdLabs0  = list(p1="Total Impacts\nbillion USD", p2="Total Impacts\nbillion USD\nper 100,000\nindividuals"),
    ggTitle0  = list(p1="Annual Climate-Driven Damages in 2090 by State", p2=""),
    subTitle0 = list(p1="Subset of Climate-Related Impacts", p2="Subset of Climate-Related Impacts, Per 100,000 people"),
    theme0    = getMapTheme(),
    doGrid0   = TRUE
){
  ### Initialize list and plot
  # list0 <- list()
  names0 <- names0 |> unlist()
  plot0  <- df0 |> ggplot(aes(long, lat, group = group))
  ### Create plot 1
  list0  <- cols0 |> length() |> seq_len() |> map(function(i, col_i=cols0[[i]]){
    df0 |> plotStateMap(
      col0      = col_i,
      lims0     = lims0[[i]],
      colors0   = colors0, ### End list
      outline   = outline,
      xLab0     = xLabs0[[i]],
      yLab0     = yLabs0[[i]],
      lgdLab0   = lgdLabs0[[i]],
      ggTitle0  = ggTitle0[[i]],
      subTitle0 = subTitle0[[i]],
      theme0    = theme0
    ) ### End plotStateMap
  }) |> set_names(names0)
  # ### Add plot 1 to list
  # list0[["totals"]] <- plot1

  ### Arrange the plots in a grid
  if(doGrid0) plot0 <- ggarrange(plotlist=list0, nrow=2)

  ### Return
  return(plot0)
}


getSectorMaps <- function(
    df0,
    cols0     = list(p1="annual_impacts", p2="annual_impacts_percap"),
    names0    = list(p1="totals", p2="percap"),
    # lims0     = list(),
    # lims0     = list(p1=c(-5, 325), p2=c(0, 1.5)),
    # lims0     = list(p1=c(5, 540), p2=c(0.75, 2.2)),
    colors0   = list(
      low      = "white",
      mid      = "white",
      high     = "#DD8047",
      na.value = "grey",
      n.breaks = 6
    ), ### End list
    outlines  = "gray8",
    xLabs0    = list(p1="", p2=""),
    yLabs0    = list(p1="", p2=""),
    lgdLabs0  = list(p1="Total Impacts\nbillion USD", p2="Total Impacts\nbillion USD\nper 100,000\nindividuals"),
    ggTitle0  = list(p1="Climate-Driven Damages in 2090 by State for sectori", p2=""),
    subTitle0 = list(p1="sectori Impacts", p2="sectori Impacts, Per 100,000 people"),
    theme0    = getMapTheme(),
    doGrid0   = TRUE
){
  ### List of sectors
  sectors0 <- df0 |> pull(sector) |> unique()
  maps0    <- sectors0 |> map(function(
    sector_i,
    df_i = df0 |> filter(df)
  ){
    df0 |>
      filter(sector %in% sector_i) |>
      map2StateMap(
        cols0     = cols0,
        names0    = names0,
        # lims0     = lims0,
        colors0   = colors0,
        outlines  = outlines,
        xLabs0    = xLabs0,
        yLabs0    = yLabs0,
        lgdLabs0  = lgdLabs0,
        ggTitle0  = ggTitle0  |> map(str_replace, pattern="sectori", replacement=sector_i),
        subTitle0 = subTitle0 |> map(str_replace, pattern="sectori", replacement=sector_i),
        theme0    = theme0,
        doGrid0   = doGrid0
      ) ### End map2StateMap
  }) |> set_names(sectors0)

  ### Return
  return(maps0)
}
