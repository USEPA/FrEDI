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
  select2 <- join0 |> c(cols0)
  df2     <- df2   |> select(all_of(select2))
  df2     <- df2   |> rename_at(c(cols0), ~cols2)
  df2     <- df2   |> unique()
  rm(select2)
  # return(list(df1=df1, df2=df3))

  ### Format data for first sector, then join with data for second sector
  df1     <- df1 |> left_join(df2, by=join0)
  rm(join0, df2)
  df1 |> is.na() |> sum() |> print()

  ### - Subtract sector 2 impacts from sector 1 impacts, then drop sector 2 impacts
  drop1   <- cols2
  df1[,cols0] <- df1[,cols0] - df1[,cols2]
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

### Get state map
getStateMap <- function(str0="state"){
  # str0 <- "state"
  map0 <- str0 |> maps::map(fill=T, plot=FALSE)
  return(map0)
}

### Get state polygons
getStatePolygons <- function(str0="state"){
  # str0 <- "state"
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
  df0   <- df0   |> mutate(state_lc = state |> tolower(), .after="state")
  gons0 <- gons0 |> left_join(df0, by="state_lc", relationship="many-to-many")
  # gons0 |> pull(state_lc) |> unique() |> print()
  ### Filter data
  gons0 <- gons0 |> filter(!(state |> is.na()))
  # gons0 |> pull(state_lc) |> unique() |> print()
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
    theme(axis.ticks.x = element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.text.x  = element_blank()) +
    theme(axis.text.y  = element_blank()) +
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
      gradType = "2", ### Or "n"
      n.breaks = NULL
      # n.breaks = 6
    ), ### End list
    outline   = "gray8",
    xLab0     = "",
    yLab0     = "",
    lgdLab0   = "Total Impacts\nbillion USD",
    ggTitle0  = "Annual Climate-Driven Damages in 2090 by State",
    subTitle0 = "Subset of Climate-Related Impacts",
    # n.breaks0 = 6,
    round0    = 1,
    symb0     = "$",
    theme0    = getMapTheme()
){
  ### Edit limits
  hasLims0 <- lims0 |> length()
  if(hasLims0) {
    lims0 |> print()
    lims0[1] <- lims0[1] |> floor()
    lims0[2] <- lims0[2] |> ceiling()
    lims0 |> print()
  } ### End if(hasLims0)

  ### Colors
  clrCols <- colors0 |> names()
  clrCols |> print()
  colors0 <- colors0 |>
    as.list() |> set_names(clrCols) |> map(function(x0){
      x0 |> is.na() |> ifelse(NULL, x0)
    }) |> set_names(clrCols)
  # colors0 |> print()

  ### Gradient
  gradType <- colors0[["gradType"]]
  gradType |> print()
  doGrad   <- !(gradType |> is.null())
  doGradN  <- doGrad |> ifelse(!(gradType %in% "2"), F)

  ### Print values
  # col0  |> print()
  df0   |> pull(all_of(col0)) |> range(na.rm=T) |> print()
  # lims0 |> print()

  ### Initialize plot
  plot0 <- df0 |> ggplot(aes(long, lat, group=group))
  ### Add geom
  plot0 <- plot0 + geom_polygon(aes(fill=.data[[col0]]), color=outline)
  ### Add colors
  if(doGrad) {
    if(doGradN) {
      rm(plot0)
      clrs0 <- c(colors0[["low"]], colors0[["mid"]], colors0[["high"]])
      # vals0 <- lims0 |> c(0) |> sort()
      df0   <- df0 |>
        mutate(colVal   = df0 |> pull(all_of(col0))) |>
        mutate(colAbs   = colVal |> abs()) |>
        mutate(colLog10 = colAbs |> log10() |> na_if(-Inf) |> replace_na(0)) |>
        mutate(colAdj   = colLog10 * colAbs / colVal ) |>
        mutate(colAdj   = colAdj |> na_if(-Inf) |> na_if(Inf) |> replace_na(0))

      ### Initialize plot
      plot0 <- df0 |> ggplot(aes(long, lat, group=group))
      ### Add geom
      plot0 <- plot0 + geom_polygon(aes(fill=.data[[col0]]), color=outline)
      ### Add gradient
      plot0 <- plot0 + scale_fill_gradientn(
        # name     = lgdLab0[[1]],
        name     = lgdLab0[[1]],
        limits   = lims0,
        colors   = clrs0,
        # values   = vals0 |> scales::rescale(),
        na.value = colors0[["na.value"]],
        n.breaks = colors0[["n.breaks"]],
        guide    = guide_colorsteps(ticks=TRUE, ticks.linewidth=1, show.limits=TRUE),
        oob      = scales::squish
      ) ### End scale_fill_gradient2
    } else{
      plot0 <- plot0 + scale_fill_gradient2(
        # name     = lgdLab0[[1]],
        name     = lgdLab0[[1]],
        limits   = lims0,
        low      = colors0[["low"]],
        mid      = colors0[["mid"]],
        high     = colors0[["high"]],
        na.value = colors0[["na.value"]],
        n.breaks = colors0[["n.breaks"]],
        guide    = guide_colorsteps(ticks=TRUE, ticks.linewidth=1, show.limits=TRUE),
        oob      = scales::squish
      ) ### End scale_fill_gradient2
    } ### End if(doGradN)
  } ### End if(doGrad)

  ### Add themes, labels, titles
  plot0 <- plot0 + theme0
  plot0 <- plot0 + xlab(xLab0[[1]]) + ylab(yLab0[[1]])
  plot0 <- plot0 + ggtitle(ggTitle0[[1]], subTitle0[[1]])

  ### Return
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
      gradType = "2", ### Or "n"
      n.breaks = NULL
      # n.breaks = 6
    ) |> (function(listX){
      list(p1=listX, p2=listX)
    })(), ### End list
    outlines  = "gray8",
    xLabs0    = list(p1="", p2=""),
    yLabs0    = list(p1="", p2=""),
    lgdLabs0  = list(p1="Total Impacts\nbillion USD", p2="Total Impacts\nbillion USD\nper 100,000\nindividuals"),
    ggTitle0  = list(p1="Annual Climate-Driven Damages in 2090 by State", p2=""),
    subTitle0 = list(p1="Subset of Climate-Related Impacts", p2="Subset of Climate-Related Impacts, Per 100,000 people"),
    # n.breaks0 = 8,
    round0    = 1,
    symb0     = "$",
    theme0    = getMapTheme(),
    doGrid0   = TRUE
){
  ### Names & values
  names0  <- names0 |> unlist()
  nList0  <- names0 |> length()
  nSymb0  <- symb0  |> length()
  if(nSymb0 < nList0) {
    nSymb0 <- nList0 - nSymb0
    symb0  <- symb0 |> c(symb0[1] |> rep(nSymb0))
  } ### End if(nSymb0 < nList0)
  # names0 <- names0[1]
  # names0 |> print()

  ### Initialize list and plot
  # list0 <- list()
  # "got here2" |> print()
  # plot0  <- df0 |> ggplot(aes(long, lat, group=group))

  ### Create plot 1
  # "got here3" |> print()
  # df0 |> glimpse()
  list0  <- cols0 |> length() |> seq_len() |> map(function(i, col_i=cols0[[i]]){
    df0 |> plotStateMap(
      col0      = col_i,
      lims0     = lims0[[i]],
      colors0   = colors0[[i]], ### End list
      # colors0   = colors0, ### End list
      outline   = outlines,
      xLab0     = xLabs0[[i]],
      yLab0     = yLabs0[[i]],
      lgdLab0   = lgdLabs0[[i]],
      ggTitle0  = ggTitle0[[i]],
      subTitle0 = subTitle0[[i]],
      n.breaks0 = n.breaks0,
      round0    = round0,
      symb0     = symb0[[i]],
      theme0    = theme0
    ) ### End plotStateMap
  }) |> set_names(names0)
  # ### Add plot 1 to list
  # list0[["totals"]] <- plot1
  # "got here4" |> print()

  ### Arrange the plots in a grid
  if(doGrid0) plot0 <- ggarrange(plotlist=list0, nrow=2)
  else        return(list0)
  # "got here5" |> print()

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


### Function to format region plot
format_regionMapData <- function(
    df0, ### NCA Regions
    df1, ### Region data
    map0 = getStateMap()
){
  ### Separate values into state and subregion
  vals0 <- map0$names
  dfM   <- tibble(polyName = vals0)
  dfM   <- dfM |> separate(
    col    = polyName,
    into   = c("state_lc", "subregion"),
    sep    = ":",
    fill   = "right",
    remove = FALSE
  ) ### End separate
  ### Join in region info
  dfM   <- dfM |> left_join(df0, by="state_lc")
  # return(dfM)
  ### Add row names
  # rownames(dfM) <- vals0

  ### Create spatial dataframe from map
  map0  <- map0 |>
    sf::st_as_sf(coords=c("long", "lat")) |>
    sf::st_make_valid() |>
    sf::st_transform(4326) |>
    sf::st_transform(3857) |>
    mutate(state_lc = ID)

  ### Left join region values
  map0  <- map0 |>
    left_join(dfM, by="state_lc") |>
    sf::st_make_valid()
  rm(dfM)

  ### Summarize over region |>
  map0  <- map0 |>
    group_by_at(c("region")) |>
    sf::st_make_valid() |>
    summarise()

  ### Add in region-level data
  map0  <- map0 |>
    left_join(df1, by="region") |>
    sf::st_make_valid() |>
    group_by_at(c("region"))
  rm(df1)

  ### Return
  return(map0)
}


### Function to plot region maps
map_regionMapData <- function(
    df0,   ### Outputs of format_regionMapData
    col0   = "annual_impacts_percap",
    k0     = 1e-3,
    title0 = "2090 Regional Climate-Driven Damages Per Capita",
    sub0   = NULL,
    lgd0   = "Damages per Capita\n(Thousands USD)",
    xlab0  = NULL,
    ylab0  = NULL,
    spf0   = "$%1.0f",
    pal0   = "Greys",
    theme0 = getMapTheme()
){
  ### Mutate data
  df0 <- df0 |>
    arrange_at(c(col0)) |>
    mutate_at(c(col0), function(x, y=k0){x * y})
  ### Initialize plot
  p0  <- df0 |>
    ggplot(group=group)+
    geom_sf(aes(fill=(!!sym(col0)) |> factor()), lwd=0.8, color="black")
  ### Add fill
  p0  <- p0 + scale_fill_brewer(
    lgd0,
    palette = pal0,
    guide   = guide_legend(reverse=TRUE),
    labels  = function(x) spf0 |> sprintf(x |> as.double())
    # labels  = function(x) "$%1.0f" |> sprintf(x |> as.double())
  ) ### End scale_fill_brewer
  ### Add fill
  p0  <- p0 + labs(x=xlab0, y=ylab0)
  ### Add labels
  p0  <- p0 + ggtitle(title0, sub0)
  ### Return
  return(p0)
}
