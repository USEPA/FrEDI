###### aggregate_impacts ######
### Created 2021.02.08.
#' Summarize and aggregate impacts from [FrEDI::run_fredi()] (calculate national totals, average across models, sum impact types, and interpolate between impact year estimates)
#'
#' @description
#' Summarize and aggregate impacts from [FrEDI::run_fredi()] (calculate national totals, average across models, sum impact types, and interpolate between impact estimate years).
#'
#' @param data      Data frame of results FrEDI (outputs from [FrEDI::run_fredi()])
#' @param columns   Character vector of columns for which to aggregate results (defaults to `columns = c( "physical_impacts", "annual_impacts")`).
#' @param aggLevels Levels of aggregation at which to summarize data: one or more of `c("national", "modelAverage", "impactYear", "impactType", "all" )`. Defaults to all levels (i.e., `aggLevels = "all"`). Note that, if `"impacttype"` is in `aggLevels` (e.g., `aggLevels = "all"`), column `"physical_measure"` is dropped from the `groupCols` and column `"physical_impacts"` is dropped from `columns`. This is because aggregating over impact types for some sectors requires summing costs over different types of physical impacts, so reporting the physical impacts would be nonsensical.
#' @param groupCols Character vector indicating which columns to use for grouping. Defaults to `groupCols = c("sector", "variant", "impactYear", "impactType", "model_type", "model", "sectorprimary", "includeaggregate", "physicalmeasure", "region", "state", "postal")`. Note that the `"variant"` column referred to below contains information about the variant or adaptation name (or `“N/A”`), as applicable.
#'
#' @details
#' This function can be used to aggregate and summarize the FrEDI results to levels of aggregation specified by the user (passed to `aggLevels`). Users can specify all aggregation levels at once by specifying `aggLevels = "all"` (default) or no aggregation levels (`aggLevels = "none"`). Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels= "national"`), averaging across model types and models (`aggLevels = "modelAverage"`), summing over all impact types (`aggLevels = "impactType"`), and interpolating between impact year estimates (`aggLevels = "impactYear"`).
#'
#'
#' Before aggregating impacts for national totals and/or model averages, [FrEDI::aggregate_impacts()] will drop any pre-summarized results (i.e., values for which `region = "National Total"` and/or for which `model = "Average"`, respectively) that are already present in the data and then re-summarize results at those respective levels.
#'
#' If users specify `aggLevels = "none"`, [FrEDI::aggregate_impacts()] returns the data frame passed to the `data` argument.
#'
#' If users specify `aggLevels = "all"` or other combinations of aggregation levels, the [FrEDI::aggregate_impacts()] function uses performs the following calculations using the grouping columns specified by the `groupCols` argument: `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, `"model"`, `"sectorprimary"`, `"includeaggregate"`, `"physicalmeasure"`, and `"year"`.
#'
#' \tabular{ll}{
#' \strong{Aggregation Level} \tab \strong{Description} \cr
#' *`impactyear`* \tab To aggregate over impact years, [FrEDI::aggregate_impacts()] first separates results for sectors with only one impact year estimate (i.e., `impactYear = "N/A"`) from from observations with multiple impact year estimates (i.e., sectors with results for both `impactYear = "2010"` and `impactYear = "2090"`). For these sectors with multiple impact years, physical impacts and annual costs (columns `"physical_impacts"` and `"annual_impacts"`) are linearly interpolated between impact year estimates. For any model run years above 2090, annual results for sectors with multiple impact years return the 2090 estimate. The interpolated values are then row-bound to the results for sectors with a single impact year estimate, and column `impactYear` set to `impactYear = "Interpolation"` for all values. If `"impactyear"` is included in `aggLevels` (e.g., `aggLevels = "all"`), [FrEDI::aggregate_impacts()] aggregates over impact years before performing other types of aggregation. \cr
#'
#' *`modelaverage`* \tab To aggregate over models for temperature-driven sectors, [FrEDI::aggregate_impacts()] averages physical impacts and annual costs (columns `"physical_impacts"` and `"annual_impacts"`, respectively) across all GCM models present in the data. [FrEDI::aggregate_impacts()] drops the column `"model"` from the grouping columns when averaging over models. Averages exclude observations with missing values. However, If all values within a grouping are missing, the model average is set to `NA`. The values in column `"model"` are set to `"Average"` for model averages and the model averages data frame is then row-bound to the main results data frame. For SLR-driven sectors, there is no need for additional model aggregation; these values already have `model = "Interpolation"`. If `"modelaverage"` is included in `aggLevels` (e.g., `aggLevels = "all"`), [FrEDI::aggregate_impacts()] first aggregates over impact years  (if `"impactyear"` present in `aggLevels` or if `aggLevels = "all"`) before aggregating over models.\cr
#'
#' *`national`* \tab To aggregate values to the national level, [FrEDI::aggregate_impacts()] sums physical impacts and annual costs (columns `"physical_impacts"` and `"annual_impacts"`, respectively) across all states present in the data. [FrEDI::aggregate_impacts()] drops the columns `"region"`, `"state"`, and `"postal"` when summing over states and regions. Years which have missing column data for all states return as `NA`. Values for columns `"region"`, `"state"`, and `"postal"` are set to `"National Total"`, `All`, and `US`, respectively. The data frame with national totals is then row-bound to the main results data frame. If `"national"` is included in `aggLevels` (e.g., `aggLevels = "all"`), [FrEDI::aggregate_impacts()] first aggregates over impact years and/or models (if `"impactyear"` and/or `"modelaverage"` are present in `aggLevels` or if `aggLevels = "all"`) before aggregating over models.\cr
#'
#' *`impacttype`* \tab To aggregate values over impact types, [FrEDI::aggregate_impacts()] sums annual impacts (column `"annual_impacts"`) across all impact types for each sector. [FrEDI::aggregate_impacts()] drops the column `"impactType"` and `"physicalmeasure"` from the grouping columns when summing over impact types. Years which have missing column data for all impact types return as `NA`. All values in column `"impactType"` are set to `"all"`. Aggregating over impact types, drops columns related to physical impacts (i.e., columns `"physicalmeasure"` and `"physical_impacts"`). These columns are dropped since aggregating over impact types for some sectors requires summing costs over different types of physical impacts, so reporting the physical impacts would be nonsensical.\cr
#' }
#'
#' After aggregating values, [FrEDI::aggregate_impacts()] joins the data frame of impacts with information about `"driverType"`, `"driverUnit"`, `"driverValue"`, `"gdp_usd"`, `"national_pop"`, `"gdp_percap"`, and `"state_pop"`.
#'
#'
#' @examples
#' ### Create temperature binning scenario
#' df_results1 <- run_fredi(aggLevels="none", silent=TRUE)
#'
#' ### Aggregate temperature binning summary across multiple columns
#' df_results2 <- df_results1 |> aggregate_impacts(columns=c("annual_impacts"), aggLevels="all")
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'
#' @export
#' @md
#'
### This function aggregates outputs produced by temperature binning
aggregate_impacts <- function(
    data,             ### Data frame of outputs from temperature binning
    aggLevels  = c("national", "modelaverage", "impacttype", "impactyear"),  ### Levels of aggregation
    sumCols    = c("physical_impacts", "annual_impacts"), ### Columns to aggregate
    groupCols  = c("sector", "variant", "impactType", "impactYear", "region", "state", "postal", "model_type", "model") |>
      c("includeaggregate", "sectorprimary"),
    silent     = TRUE
){
  ### Messaging ----------------
  ### Not used currently; preserving it in messaging logicals for the future
  msgUser      <- !silent
  msg0         <- function(lvl0=1){c("\t") |> rep(lvl0) |> paste(collapse = c(""))}


  #### Aggregation Levels ----------------
  ### Types of summarization to do: default
  ### Aggregation levels
  aggList0   <- aggList0  |> tolower()
  aggLevels  <- aggLevels |> tolower()
  aggNone0   <- "none" %in% aggLevels
  aggAll0    <- "all"  %in% aggLevels
  aggLvlsN   <- 0
  ### If none specified, no aggregation (only SLR interpolation)
  ### Otherwise, aggregation depends on length of agg levels
  if      (aggNone0 ) {
    aggLevels <- "none"
    msg0 (1) |> paste0("No aggregation levels specified...")
    msg0 (1) |> paste0("Returning data...", "\n")
    paste0("Finished.", "\n") |> message()
  } else if (aggAll0) {
    aggLevels <- aggList0
  } else{
    aggLevels <- aggLevels |> get_matches(y=aggList0)
    aggLvlsN  <- aggLevels |> length()
  } ### End if (aggAll0 )
  doAgg      <- aggAll0 | aggLvlsN
  rm(aggList0, aggNone0, aggAll0, aggLvlsN)

  ### Aggregate to impact years or national...reduces the columns in output
  doNat    <- "national"     %in% aggLevels
  doMAves  <- "modelaverage" %in% aggLevels
  doIType  <- "impacttype"   %in% aggLevels
  doIYear  <- "impactyear"   %in% aggLevels


  ### Values ----------------
  #### Conditionals ----------------
  # hasGroupCols <- !(groupCols |> is.null())
  #### Columns ----------------
  condCols0    <- c("includeaggregate", "sectorprimary")
  sectVarCols0 <- c("sector", "variant")
  iTypeCol0    <- c("impactType")
  iYearCol0    <- c("impactYear")
  mTypeCol0    <- c("model_type")
  modCol0      <- c("model")
  regCols0     <- c("region", "state", "postal")
  yrCol0       <- c("year")
  popCol0      <- c("pop")
  natPopCol0   <- c("national_pop")
  gdpCols0     <- c("gdp_usd", "gdp_percap")
  driverCols0  <- c("driverType", "driverUnit", "driverValue")
  baseIDCol0   <- c("baseID")

  #### Scalar columns
  annSumCol0   <- c("annual_impacts")
  physSumCol0  <- c("physical_impacts")
  scaledSumCol <- c("scaled_impacts")
  physMeasCol0 <- c("physicalmeasure")
  mainGroups   <- c(sectVarCols0, iTypeCol0, iYearCol0, regCols0, mTypeCol0, modCol0, condCols0)

  #### Values ----------------
  ### Years in data
  ### "Average", "Model Average"
  years0       <- data |> pull(all_of(yrCol0)) |> unique()
  mTypes0      <- data |> pull(all_of(mTypeCol0)) |> unique()
  natLbls0     <- list(region="National Total", state="All", postal="US")
  natLbl0      <- c("National Total")
  modAveLbl0   <- c("Average")
  doGcm        <- "gcm" %in% (mTypes0 |> tolower())

  ### Format Data ----------------
  ### Ungroup
  ### If "national" aggregation, filter out national totals
  ### If modelAverage %in% aggLevels, filter out model averages
  # data |> glimpse()
  data         <- data |> ungroup()
  oNames0      <- data |> names()
  names0       <- oNames0
  if(doNat  ){data <- data |> filter(!(region %in% natLbls0$region))}
  if(doMAves){data <- data |> filter(!(model  %in% modAveLbl0))}

  ### Grouping and Summary Columns ----------------
  #### Grouping columns
  groupCols    <- groupCols |> aggImpacts_adjustColumns(
    names0  = names0 , ### Names of data
    doNat   = doNat  , ### Aggregate over national
    doIType = doIType, ### Aggregate over impact types
    type0   = "group", ### Or sum
    msg0    = msg0()
  ) ### End aggImpacts_adjustColumns
  ### Message user
  msg0(1) |> paste0("Grouping by groupCols = c(", groupCols |> paste(collapse=", "), ")...") |> message()

  #### Summary Columns
  sumCols      <- sumCols |> aggImpacts_adjustColumns(
    names0  = names0 , ### Names of data
    doNat   = doNat  , ### Aggregate over national
    doIType = doIType, ### Aggregate over impact types
    type0   = "group", ### Or sum
    msg0    = msg0()
  ) ### End aggImpacts_adjustColumns
  ### Message user
  hasSumCols   <- sumCols |> length()
  if(!hasSumCols){msg0() |> paste0("Exiting...") |> message()}
  else           {msg0(1) |> paste0("Summarizing over sumCols = c(", sumCols |> paste(collapse=", "), ")...") |> message()}
  rm(hasSumCols)

  #### Select Columns from Data
  ### Add scenario columns
  select0      <- c(groupCols, yrCol0, sumCols)
  dropCols0    <- names0 |> get_matches(y=select0, matches=F)
  data         <- data |> select(-all_of(dropCols0))
  names0       <- data |> names()
  rm(select0, dropCols0)



  ### Scenarios ----------------
  #### Base scenario ----------------
  ### Associated Columns
  # data |> glimpse()
  baseGroups     <- mainGroups |> get_matches(y=groupCols)
  baseCols       <- c(mTypeCol0, regCols0, yrCol0, natPopCol0, gdpCols0, popCol0, driverCols0)
  baseInfo   <- mTypes0 |> map(function(
    typeX,
    colX    = mTypeCol0,
    groupsX = baseGroups,
    colsX   = baseCols
  ){
    data |>
      filter_at(c(colX), function(x){x %in% typeX}) |>
      get_uniqueDf0(group0=groupsX, cols0=colsX, type0="first")
  }) |> bind_rows()

  #### National Scenario ----------------
  ### If doNat, add national scenario
  if(doNat) {
    baseInfo <- mTypes0 |> map(function(
      typeX,
      colX    = mTypeCol0,
      groupsX = baseGroups,
      colsX   = baseCols
    ){
      baseInfo |>
        mutate(region = natLbls0$region) |>
        mutate(state  = natLbls0$state ) |>
        mutate(postal = natLbls0$postal) |>
        filter_at(c(colX), function(x){x %in% typeX}) |>
        get_uniqueDf0(group0=groupsX, cols0=colsX, type0="first")
    }) |> bind_rows() |>
      bind_rows(baseInfo)
  } ### End if(doNat)

  #### Other Info ----------------
  otherCols      <- c(sectVarCols0, mTypeCol0, condCols0) |> (function(
    colsX,
    colsY   = baseCols,
    groupsX = groupColumns
  ){
    ### Columns to exclude
    if(doIType) colsY <- colsY |> c(iTypeCol0)
    if(doIYear) colsY <- colsY |> c(iYearCol0)
    if(doNat  ) colsY <- colsY |> c(regCols0 )
    if(doMAves) colsY <- colsY |> c(modCol0  )
    ### Get unique values
    colsY    <- colsY |> unique()
    ### Get matches
    groupsX  <- groupsX |> get_matches(y=colsY, matches=F)
    ### Columns
    colsX    <- colsX |> c(groupsX) |> unique()
    ### Return
    return(colsX)
  })()
  ### Group data
  ### Get group keys
  groupCols     <- groupCols |> c(baseIDCol0)
  data          <- data |> group_by_at(c(otherCols)) |> mutate(baseID=cur_group_id())
  otherInfo <- data |> group_keys() |> mutate(baseID = data |> pull(baseID) |> unique())

  ### Aggregation ----------------
  #### Select Data
  # if(msgUser & doAgg){message("Aggregating impacts...")}
  ### Select appropriate columns
  # groupCols |> c(yrCol0, sumCols) |> print()
  select0     <- groupCols |> c(yrCol0, sumCols)
  data        <- data  |> select(all_of(select0))
  rm(select0)

  #### Impact Years ----------------
  ### Separate into years after 2090 and before 2090
  if(doIYear){
    if(msgUser){msg0 (1) |> paste0("Interpolating between impact year estimates...") |> message()}
    data <- interpolate_impYear(
      col0    = iYearCol0,
      group0  = groupCols,
      sum0    = sumCols,
      naStr0  = c("N/A"),
      newStr0 = c("Interpolation")
    ) ### End interpolate_impYear
  } ### if(doIYear)


  #### Model Averages ----------------
  ### Average values across models
  if(doMAves){
    if(msgUser){msg0 (1) |> paste0("Calculating model averages...") |> message()}
    if(doGcm) {
      ### Ungroup first
      data <- data |> calc_modelAves(
        cols0  = modCol0,
        group0 = groupCols,
        sum0   = sumCols,
        lbl0   = c("Average"),
        fun0   = c("mean"),
        slrStr = c("slr"),
        yrCol0 = c("year"),
        na.rm  = TRUE
      ) ### End calc_modelAves
    } ### End if(doGcm)
  } ### End if "model" %in% aggLevels


  #### National Totals ----------------
  if(doNat){
    if(msgUser){msg0 (1) |> paste0("Calculating national totals...") |> message()}
    ### Ungroup first
    data <- data |> sum_national(
      cols0  = regCols0,
      group0 = groupCols,
      sum0   = sumCols,
      lbls0  = natLbls0,
      fun0   = c("sum"),
      na.rm  = TRUE
    ) ### End sum_national
  } ### End if national


  #### Impact Types ----------------
  ### Summarize by Impact Type
  if(doIType){
    if(msgUser){msg0 (1) |> paste0("Summing across impact types...") |> message()}
    ### Ungroup first
    data  <- data |> sum_impType(
      data,  ### Grouped data
      cols0  = iTypeCol0,
      group0 = groupCols,
      sum0   = sumCols,
      lbl0   = c("all"),
      fun0   = c("sum"),
      naStr0 = c("N/A", "NA"),
      na.rm  = TRUE
    ) ### End sum_impType
  } ### End if impactType in aggLevels

  ### Join data ----------------
  #### Other Info
  join0    <- c(baseIDCol0)
  namesD   <- data      |> names()
  colsOth  <- otherInfo |> names() |> get_matches(namesD, matches=F) |> c(join0)
  data     <- data      |> left_join(otherInfo |> select(all_of(colsOth)), by=join)
  rm(join0, otherInfo)

  #### Base Scenario
  # data |> glimpse(); baseInfo |> glimpse()
  namesD   <- data     |> names()
  join0    <- baseCols |> get_matches(namesD, matches=T)
  data     <- data     |> left_join(baseInfo, by=join0)
  rm(join0)

  ### Format Data ----------------
  ### Column indices of columns used in ordering
  data     <- data |> select(any_of(names0))

  ### Return ----------------
  ### Message, clear unused memory, return
  # paste0("\n", "Finished", ".") |> message()
  gc()
  return(data)
}

