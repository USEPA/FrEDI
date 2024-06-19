###### aggregate_impacts ######
### Created 2021.02.08.
#' Summarize and aggregate impacts from [FrEDI::run_fredi()] (calculate national totals, average across models, sum impact types, and interpolate between impact year estimates)
#'
#' @description
#' Summarize and aggregate impacts from [FrEDI::run_fredi()] (calculate national totals, average across models, sum impact types, and interpolate between impact estimate years).
#'
#' @param data      Data frame of results FrEDI (outputs from [FrEDI::run_fredi()])
#' @param columns   Character vector of columns for which to aggregate results (defaults to `columns = c( "physical_impacts", "annual_impacts")`).
#' @param aggLevels Levels of aggregation at which to summarize data: one or more of `c("national", "modelAverage", "impactYear", "impactType", "all" )`. Defaults to all levels (i.e., `aggLevels = "all"`). Note that, if `"impacttype"` is in `aggLevels` (e.g., `aggLevels = "all"`), column `"physical_measure"` is dropped from the `groupByCols` and column `"physical_impacts"` is dropped from `columns`. This is because aggregating over impact types for some sectors requires summing costs over different types of physical impacts, so reporting the physical impacts would be nonsensical.
#' @param groupByCols Character vector indicating which columns to use for grouping. Defaults to `groupByCols = c("sector", "variant", "impactYear", "impactType", "model_type", "model", "sectorprimary", "includeaggregate", "physicalmeasure", "region", "state", "postal")`. Note that the `"variant"` column referred to below contains information about the variant or adaptation name (or `“N/A”`), as applicable.
#'
#' @details
#' This function can be used to aggregate and summarize the FrEDI results to levels of aggregation specified by the user (passed to `aggLevels`). Users can specify all aggregation levels at once by specifying `aggLevels = "all"` (default) or no aggregation levels (`aggLevels = "none"`). Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels= "national"`), averaging across model types and models (`aggLevels = "modelAverage"`), summing over all impact types (`aggLevels = "impactType"`), and interpolating between impact year estimates (`aggLevels = "impactYear"`).
#'
#'
#' Before aggregating impacts for national totals and/or model averages, [FrEDI::aggregate_impacts()] will drop any pre-summarized results (i.e., values for which `region = "National Total"` and/or for which `model = "Average"`, respectively) that are already present in the data and then re-summarize results at those respective levels.
#'
#' If users specify `aggLevels = "none"`, [FrEDI::aggregate_impacts()] returns the data frame passed to the `data` argument.
#'
#' If users specify `aggLevels = "all"` or other combinations of aggregation levels, the [FrEDI::aggregate_impacts()] function uses performs the following calculations using the grouping columns specified by the `groupByCols` argument: `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"region"`, `"state"`, `"postal"`, `"model_type"`, `"model"`, `"sectorprimary"`, `"includeaggregate"`, `"physicalmeasure"`, and `"year"`.
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
    aggLevels   = c("national", "modelaverage", "impacttype", "impactyear"),  ### Levels of aggregation
    columns     = c("physical_impacts", "annual_impacts"), ### Columns to aggregate
    groupByCols = c("sector", "variant", "impactType", "impactYear") |>
      c("region", "state", "postal") |>
      c("model_type", "model") |>
      c("includeaggregate", "sectorprimary"),
    silent      = TRUE
){
  ###### Defaults ######
  ### Not used currently; preserving it in messaging logicals for the future
  msgUser      <- !silent
  msg0         <- function(lvl0=1){c("\t") |> rep(lvl0) |> paste(collapse = c(""))}
  yearCol0     <- c("year")
  # summaryCols  <- columns; rm(columns)
  ####### By State  ######
  byState      <- c("state") %in% (data |> names())
  if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  popCol0      <- byState |> ifelse("state_pop", "reg_pop")
  # byState |> print()

  ###### Format Data ######
  ### Ungroup
  data         <- data |> ungroup() #; names(data) |> print()
  # data |> glimpse()
  ###### Years Info ######
  ### Years in data
  # c_npdRefYear <- 2090
  c_dataYears  <- data[[yearCol0]] |> unique()

  ###### Aggregation Levels  ######
  ### Types of summarization to do: default
  # aggList0     <- c("national", "modelaverage", "impactyear", "impacttype", "all")
  aggList0     <- c("national", "modelaverage", "impacttype", "impactyear")
  null_aggLvls <- aggLevels |> is.null()
  aggLevels    <- aggLevels |> tolower()
  aggNone      <- "none" %in% aggLevels
  aggAll       <- "all"  %in% aggLevels
  if(null_aggLvls | aggAll){
    aggLevels <- aggList0
  } else if(aggNone){
    aggLevels <- c()
    if(msgUser){
      msg0 (1) |> paste0("No aggregation levels specified...")
      msg0 (1) |> paste0("No aggregation levels specified...")
      msg0 (1) |> paste0("Returning data...", "\n")
      paste0("Finished.", "\n") |> message()
    } ### End if msgUser
    return(data)
  } else{
    aggLevels <- aggLevels[aggLevels %in% aggList0]
  } ### End else
  ### Check if aggregation required
  requiresAgg <- aggLevels |> length() > 0

  ###### Aggregation Level Options  ######
  ### Aggregate to impact years or national...reduces the columns in output
  aggImpYear  <- "impactyear"   %in% aggLevels
  aggImpTypes <- "impacttype"   %in% aggLevels
  aggNational <- "national"     %in% aggLevels
  aveModels   <- "modelaverage" %in% aggLevels

  ### Filter data
  ### If "national" aggregation, filter out national totals
  if(aggNational){data <- data |> filter(region!="National Total")}
  ### If modelAverage %in% aggLevels, filter out model averages
  if(aveModels  ){data <- data |> filter(!(model %in% c("Average", "Model Average")))}

  ###### Get FrEDI Data Objects ######
  ### Load info tables from sysdata.rda
  co_models   <- "co_models"   |> get_frediDataObj("frediData")
  co_sectors  <- "co_sectors"  |> get_frediDataObj("frediData")
  co_variants <- "co_variants" |> get_frediDataObj("frediData")

  ###### - Formate SLR Info ######
  co_slrs     <- co_models |> filter(modelType=="slr")
  co_slrs     <- co_slrs   |> mutate_at(c("model_dot", "model_label"), as.character)

  ###### - Format Sector Info ######
  rename0     <- c("sector_id", "sector_label", "modelType")
  rename1     <- c("sector_id", "sector", "model_type")
  co_sectors  <- co_sectors |> select(all_of(rename0))
  co_sectors  <- co_sectors |> rename_at(c(rename0), ~rename1)
  co_sectors  <- co_sectors |> mutate_at(c("model_type"), toupper)
  rm(rename0, rename1)

  ###### - Format Variant Info ######
  ### Format variants
  rename0     <- c("sector_id", "variant_label")
  rename1     <- c("sector_id", "variant")
  select0     <- c(rename1) #|> c("sectorprimary", "includeaggregate")
  co_variants <- co_variants |> rename_at(c(rename0), ~rename1)
  co_variants <- co_variants |> select(all_of(select0))
  rm(rename0, rename1, select0)
  ### Combine sector and variant info
  join0          <- "sector_id"
  co_sectorVars  <- co_sectors |> left_join(co_variants, by =join0)
  co_sectorVars  <- co_sectorVars |> select(-all_of(join0))
  rm(join0, co_variants)

  ###### Grouping Columns  ######
  ### Use default group by columns if none specified...otherwise, check which are present
  groupByCols0 <- c("sector", "variant", "impactType", "impactYear")
  groupByCols0 <- groupByCols0 |> c("region") |> c(stateCols0)
  groupByCols0 <- groupByCols0 |> c("model_type", "model")
  groupByCols0 <- groupByCols0 |> c("sectorprimary", "includeaggregate")
  if(groupByCols |> is.null()){groupByCols <- groupByCols0}
  rm(groupByCols0)
  # ### Convert grouping columns to character
  # data          <- data |> mutate_at(c(groupByCols), as.character)
  ### Check if columns for grouping are there
  isPresent0   <- groupByCols %in% (data |> names())
  hasNaCols0   <- (!isPresent0) |> which() |> length() > 0
  ### Message user if some columns aren't present
  if(hasNaCols0 & msgUser){
    msg0(1) |> paste0("Warning: groupByCols = c(", paste(groupByCols[!isPresent0], collapse = ", "), ") not present...") |> message()
    msg0(2) |> paste0("Grouping by remaining columns...") |> message()
  } ### End
  ### Adjust to desired values
  groupByCols  <- groupByCols[isPresent0]
  rm(hasNaCols0, isPresent0)
  # groupByCols |> print()

  ### Drop some columns from grouping columns
  if(aggImpTypes){
    scalarCols  <- c("physScalar", "physAdj", "damageAdj", "econScalar", "econAdj", "econMultiplier") |> paste("Name")
    # scalarCols  <- c("physScalar", "physAdj", "damageAdj", "econScalar", "econAdj", "econMultiplier")
    # scalarCols  <- scalarCols0 |> map(~.x |> paste0(c(scalarSuffix0))) |> unlist()
    dropCols    <- c("physicalmeasure") |> c(scalarCols) |> c(popCol0, "national_pop")
    isDropCol   <- groupByCols %in% dropCols
    hasDropCols <- isDropCol |> any()
    ### If hasDropCols
    if(hasDropCols){
      ### Drop levels
      groupByCols  <- groupByCols |> (function(y){y[!(y %in% dropCols)]})()
    } ### End if(hasDropCols)

    ### If message user
    if(hasDropCols & msgUser){
      ### Message user
      msg0 (1) |> paste0(
        "Warning: cannot group by columns = c(`",
        groupByCols[isDropCol] |> paste(collapse="`, `"),
        "`) when 'impacttype' in aggLevels!") |> message()
      msg0 (2) |> paste0(
        "Dropping columns = c(`",
        groupByCols[isDropCol] |> paste(collapse="`, `"),
        "`) from grouping columns..."
      ) |> message()
    }### End if(msgUsr)
    ### Remove extra names
    rm(scalarCols, dropCols, isDropCol, hasDropCols)
  } ### End if(aggImpTypes)

  ###### Summary Columns  ######
  ### Columns to summarize
  # if(!is.null(columns)){summaryCols <- columns}
  summaryCols0 <- c("annual_impacts")
  nullSumCols  <- columns |> is.null()
  if(nullSumCols) {
    summaryCols <- summaryCols0
  } else {
    summaryCols <- columns
  } ### End else
  isPresent0   <- summaryCols %in% (data |> names())
  hasNaCols0   <- (!isPresent0) |> which() |> length() > 0
  ### Message user if some columns aren't present
  if(hasNaCols0 & msgUser){
    msg0 (1) |> paste0("Warning: columns = c(", paste(summaryCols[!isPresent0], collapse = ", "), ") not present...") |> message()
    msg0 (2) |> paste0("Aggregating values in columns = c(", paste(summaryCols[isPresent0], collapse = ", "), "...") |> message()
  } ### End if no sum columns present
  ### Drop missing columns
  summaryCols <- summaryCols[isPresent0]
  rm(summaryCols0, nullSumCols, isPresent0, hasNaCols0)

  ### Drop some columns from summary columns
  if(aggImpTypes){
    scalarCols  <- c("physScalar", "physAdj", "damageAdj", "econScalar", "econAdj", "econMultiplier") |> paste("Value")
    scalarCols  <- scalarCols |> c("physScalar", "econScalar", "physEconScalar")
    scalarCols  <- c("c0", "c1", "exp0", "year0") |> c(scalarCols)
    dropCols    <- c("physical_impacts")
    isDropCol   <- summaryCols %in% dropCols
    hasDropCols <- isDropCol |> any()
    ### If hasDropCols drop columns
    if(hasDropCols){
      ### Drop levels
      summaryCols  <- summaryCols |> (function(y){y[!(y %in% dropCols)]})()
    } ### End if(hasDropCols)

    ### If, message user
    if(hasDropCols & msgUser){
      ### Message user
      msg0 (1) |> paste0(
        "Warning: cannot aggregate columns = c(`",
        summaryCols[isDropCol] |> paste(collapse="`, `"),
        "`) when 'impacttype' in aggLevels!") |> message()
      msg0 (2) |> paste0(
        "Dropping columns = c(`",
        summaryCols[isDropCol] |> paste(collapse="`, `"),
        "`) from summary columns...") |> message()
    } ### End if(hasDropCols)

    ### Remove extra names
    rm(scalarCols, dropCols, isDropCol, hasDropCols)
  } ### End if(aggImpTypes)

  ### Drop some columns if certain aggLevels present
  if(aggImpTypes ){
    dropCols    <- c("scaled_impacts")
    isDropCol   <- summaryCols %in% dropCols
    hasDropCols <- isDropCol |> any()
    ### If hasDropCols, message user
    if(hasDropCols){
      ### Drop levels
      summaryCols  <- summaryCols |> (function(y){y[!(y %in% dropCols)]})()
    } ### End if(hasDropCols)

    ###
    if(msgUser){
      ### Message user
      msg0 (1) |> paste0(
        "Warning: cannot aggregate columns = c(`",
        summaryCols[!isDropCol] |> paste(collapse="`, `"),
        "`) when aggLevels != 'none'") |> message()
      msg0 (2) |> paste0(
        "Dropping columns = c(`",
        summaryCols[ isDropCol] |> paste(collapse="`, `"),
        "`) from summary columns...") |> message()
    } ### End if(msgUser)
    ### Remove extra names
    rm(dropCols, isDropCol, hasDropCols)
  } ### End if(aggImpTypes)

  ### Get single summary column
  summaryCol1 <- summaryCols[1]

  ### Number of summary columns
  num_sumCols <- summaryCols |> length()
  if(!num_sumCols){
    msg0 (1) |> paste0("Warning: no columns to aggregate!") |> message()
    msg0 (2) |> paste0("Exiting...") |> message()
    return(data)
  } ### End if(!num_sumCols)

  ###### Format Columns  ######
  ### Make sure all summary values are numeric
  mutate0       <- c("sectorprimary", "includeaggregate")
  chrCols0      <- groupByCols |> (function(y){y[!(y %in% c(mutate0))]})()
  numCols0      <- summaryCols |> c(mutate0)
  numCols0      <- numCols0    |> c("gdp_usd", "national_pop", "gdp_percap")
  numCols0      <- numCols0    |> c("driverValue") |> c(popCol0) |> c(yearCol0)
  numCols0      <- numCols0    |> unique()
  # data          <- data |> mutate_at(c(chrCols0), as.character)
  data          <- data |> mutate_at(c(numCols0), as.character)
  data          <- data |> mutate_at(c(numCols0), as.numeric  )
  rm(mutate0)

  ###### Standardize Columns  ######
  ### Associated Columns
  # data |> glimpse()
  baseCols      <- c("year", "gdp_usd", "national_pop", "gdp_percap")
  regPopCols    <- c("year", "region") |> c(stateCols0) |> c(popCol0) |> unique()
  natPopCols    <- c("year", "region") |> c("national_pop")
  driverCols    <- c("year", "model_type", "driverType", "driverUnit", "driverValue")

  ### Get names in names
  names0        <- data |> names()
  baseCols      <- baseCols   |> (function(y){y[(y %in% names0)]})()
  regPopCols    <- regPopCols |> (function(y){y[(y %in% names0)]})()
  natPopCols    <- natPopCols |> (function(y){y[(y %in% names0)]})()
  driverCols    <- driverCols |> (function(y){y[(y %in% names0)]})()
  rm(names0)

  # ### Add state_pop column
  # if(aggNational){
  #   groupByCols <- groupByCols |> c(regPopCols) |> unique()
  #   summaryCols <- summaryCols |> (function(y){y[!(y %in% groupByCols)]})()
  # } ### End if(aggNational)

  ### List of standardized columns
  standardCols  <- c(groupByCols, baseCols, regPopCols, natPopCols) |> unique()
  standardCols  <- standardCols |> c(driverCols, summaryCols) |> unique()
  scenarioCols  <- standardCols |> (function(y){y[!(y %in% c(groupByCols, yearCol0, summaryCols))]})()
  data          <- data |> select(any_of(standardCols))#; names(data) |> print

  ###### Base Scenario Info  ######
  ### Some values are the same for all runs and regions...separate those values
  baseScenario  <- data |>
    group_by_at(c(baseCols)) |>
    summarize(n=n(), .groups="keep") |> ungroup() |>
    select(-c("n"))
  ### Regional population
  regionalPop  <- data |>
    group_by_at(c(regPopCols)) |>
    summarize(n=n(), .groups="keep") |> ungroup() |>
    select(-c("n"))
  # baseCols |> print(); baseScenario |> glimpse()
  # regPopCols |> print(); regionalPop |> glimpse()
  ### Create national population scenario from the base scenario
  nationalPop  <- baseScenario |>
    mutate(region = "National Total") |>
    select(all_of(natPopCols)) |>
    rename_at(c("national_pop"), ~popCol0)
  if(byState){nationalPop <- nationalPop |> mutate(state="All", postal="US")}
  ### Driver Scenario
  driverScenario <- data |>
    group_by_at(c(driverCols)) |>
    summarize(n=n(), .groups="keep") |> ungroup() |>
    select(-c("n"))



  ###### Aggregation  ######
  # if(msgUser & requiresAgg){message("Aggregating impacts...")}
  ### Select appropriate columns
  df_agg <- data  |> select(-all_of(scenarioCols))
  rm(data)
  # df_agg |> nrow() |> print(); df_agg |> head() |> glimpse()

  ###### ** Impact Years ######
  ### Separate into years after 2090 and before 2090
  if(aggImpYear){
    if(msgUser){msg0 (1) |> paste0("Interpolating between impact year estimates...") |> message()}
    ### Ungroup first
    df_agg        <- df_agg |> ungroup()
    ### Group by columns
    # groupCols0    <- groupByCols |> (function(y){y[!(y %in% c("impactYear", yearCol0))]})()
    group0        <- groupByCols |> (function(y){y[!(y %in% c("impactYear", yearCol0))]})()
    group0        <- group0 |> c("year")
    ### Impact years
    impactYears   <- c(2010, 2090) |> as.character()
    cImpYear1     <- impactYears[1]
    cImpYear2     <- impactYears[2]
    nImpYear1     <- cImpYear1 |> as.numeric()
    nImpYear2     <- cImpYear2 |> as.numeric()

    ### Separate data into years > 2090, years <= 2090
    c_cutoff_yr   <- 2090
    df_aggImp_1   <- df_agg |> filter(year <= c_cutoff_yr)
    df_aggImp_2   <- df_agg |> filter(year >  c_cutoff_yr)
    rm(df_agg)

    ### Then do the post-2090 results
    ### Exclude 2010 results
    df_agg        <- df_aggImp_2   |> filter(impactYear != cImpYear1) |> mutate(impactYear="Interpolation")
    rm(df_aggImp_2)
    ### Process pre-2090:
    ### Separate out observations without impact years
    df_naYears    <- df_aggImp_1 |> filter(!(impactYear %in% impactYears)) |> mutate(impactYear="Interpolation")

    ### New upper and lower column names
    sumCols2010   <- summaryCols |> paste0("_", "2010")
    sumCols2090   <- summaryCols |> paste0("_", "2090")

    ### Filter to impact year in impact years
    df_impYears   <- df_aggImp_1 |> filter(impactYear %in% impactYears)
    nrow_impYrs   <- df_impYears |> nrow()
    rm(df_aggImp_1)
    ### For nrow_impYrs > 0
    if(nrow_impYrs){
      ### Filter to other lower models and then bind with the zero values, drop model column
      df2010      <- df_impYears |> filter(impactYear == cImpYear1) |> select(-c("impactYear"))
      df2090      <- df_impYears |> filter(impactYear == cImpYear2) |> select(-c("impactYear"))
      ### Update summary columns
      df2010[,sumCols2010] <- df2010[,summaryCols]
      df2090[,sumCols2090] <- df2090[,summaryCols]
      ### Drop summary columns from 2010
      df2010      <- df2010 |> select(-all_of(summaryCols))

      ### Join upper and lower data frames and calculate the numerator, denominator, and adjustment factor
      df_impYears <- df2090 |> left_join(df2010, by=c(group0))
      # df2090 |> glimpse(); df2010 |> glimpse(); df_impYears |> glimpse()
      rm(df2090, df2010)

      ### Add Impact year numerator and denominator
      df_impYears <- df_impYears |> mutate(numer_yr = year - nImpYear1)
      df_impYears <- df_impYears |> mutate(denom_yr = nImpYear2 - nImpYear1)
      df_impYears <- df_impYears |> mutate(adj_yr   = numer_yr / denom_yr )

      ### Iterate over summary columns
      for(i in 1:num_sumCols){
        ### Upper/lower
        col_i       <- summaryCols[i]
        col_i_2010  <- col_i |> paste0("_", "2010")
        col_i_2090  <- col_i |> paste0("_", "2090")

        ### Calculate numerator and denominator
        df_impYears[["new_factor"]] <- df_impYears[[col_i_2090]] - df_impYears[[col_i_2010]]
        df_impYears[["new_value" ]] <- df_impYears[[col_i_2010]]
        # df_slrOther |> names() |> print()

        ### Update the new value
        oldCol_i    <- col_i       |> c()
        newCol_i    <- "new_value" |> c()
        ### Mutate and rename
        select0    <- c(col_i, "new_factor")
        select1    <- c(col_i_2010, col_i_2090)
        df_impYears <- df_impYears |> mutate(new_value = new_value + new_factor * adj_yr)
        df_impYears <- df_impYears |> select(-all_of(select0))
        df_impYears <- df_impYears |> rename_at(c(newCol_i), ~oldCol_i)
        df_impYears <- df_impYears |> select(-all_of(select1))
        ### Remove values
        rm(i, col_i, col_i_2010, col_i_2090, oldCol_i, newCol_i, select0)
      } ### End for(i in 1:num_sumCols)
      ### Add new factor and drop columns
      select0     <- c("numer_yr", "denom_yr", "adj_yr")
      df_impYears <- df_impYears  |> mutate(impactYear="Interpolation")
      df_impYears <- df_impYears  |> select(-all_of(select0))
      rm(select0)
    } ### End if(nrow_impYrs)
    rm(impactYears, cImpYear1, cImpYear2, sumCols2010, sumCols2090, c_cutoff_yr)
    ### Add back into values without NA years
    ### Join post 2090 results with earlier results
    df_aggImp_1   <- df_impYears |> rbind(df_naYears) |> mutate(impactYear="Interpolation")
    df_agg        <- df_agg      |> rbind(df_aggImp_1)
    rm(df_impYears, df_naYears, df_aggImp_1, group0)
  } ### if(aggImpYear)
  # paste0("Finished impact year interpolation: ", nrow(df_agg)) |> print(); df_agg |> head() |> glimpse()
  # "got here1" |> print()

  ###### ** Model Averages ######
  # groupByCols |> print(); df_agg |> glimpse()
  ### Average values across models
  if(aveModels){
    modelAveMsg   <- "Calculating model averages..."
    if(msgUser){msg0 (1) |> paste0(modelAveMsg) |> message()}
    ### Ungroup first
    df_agg        <- df_agg |> ungroup()
    # df_agg        <- df_agg |> mutate_at(c("model"), as.character) |> ungroup()
    ### Group by columns
    group0        <- groupByCols |> (function(y){y[!(y %in% c("model", yearCol0))]})()
    group0        <- group0 |> c("year")
    # group0 |> print()
    ### Separate model types
    df_gcm        <- df_agg |> filter(model_type |> tolower() == "gcm")
    df_agg        <- df_agg |> filter(model_type |> tolower() != "gcm")
    do_gcm        <- df_gcm |> nrow() > 0
    ### Calculate GCM model averages
    if(do_gcm){
      ### Calculate number of non missing values
      df_modelAves <- df_gcm |> (function(w){
        w |> mutate(not_isNA = 1 * (!(w[[summaryCol1]] |> is.na())))
      })()
      ### Group data, sum data, calculate averages, and drop NA column
      sum0         <- summaryCols |> c("not_isNA")
      df_modelAves <- df_modelAves |>
        group_by_at(c(group0)) |>
        summarize_at(c(sum0), sum, na.rm=T) |> ungroup()
      rm(sum0)
      ### Adjust for non-missing values
      df_modelAves <- df_modelAves |> mutate(not_isNA = not_isNA |> na_if(0))
      # df_modelAves[,summaryCols] <- df_modelAves[,summaryCols] / df_modelAves[["not_isNA"]]
      df_modelAves <- df_modelAves |> (function(x){
        x[,summaryCols] <- x[,summaryCols] / x[["not_isNA"]]; return(x)
      })()
      ### Drop columns
      df_modelAves <- df_modelAves |> select(-c("not_isNA"))
      ### Mutate models
      df_modelAves <- df_modelAves |> mutate(model = "Average")
      ### Add observations back in
      # df_agg <- df_agg |> rbind(df_modelAves)
      df_gcm        <- df_gcm |> rbind(df_modelAves)
      rm( df_modelAves)
    } ### End if nrow(df_gcm)
    ### Bind GCM and SLR results
    df_agg <- df_gcm |> rbind(df_agg)
    rm(df_gcm, group0)
  } ### End if "model" %in% aggLevels
  # paste0("Finished model aggregation: ", nrow(df_agg)) |> print();
  # df_agg |> glimpse()
  # "got here2" |> print()

  ###### ** National Totals ######
  if(aggNational){
    if(msgUser){msg0 (1) |> paste0("Calculating national totals...") |> message()}
    ### Ungroup first
    df_agg      <- df_agg |> ungroup()
    ### Grouping columns
    # group0      <- groupByCols |> (function(y){y[!(y %in% c("region", stateCols0, popCol0, yearCol0))]})()
    group0      <- groupByCols |> (function(y){y[!(y %in% c("region", stateCols0, popCol0, yearCol0))]})()
    group0      <- group0 |> c("year")
    ### Calculate number of non missing values
    df_national <- df_agg |> (function(w){
      w |> mutate(not_isNA = 1 * (!(w[[summaryCol1]] |> is.na())))
    })()
    ### Group data, sum data, calculate averages, and drop NA column
    # sum0        <- summaryCols |> c(popCol0) |> c("not_isNA")
    sum0        <- summaryCols |> c("not_isNA")
    # df_national |> glimpse()
    df_national <- df_national |>
      group_by_at(c(group0)) |>
      summarize_at(vars(sum0), sum, na.rm=T) |> ungroup()
    rm(sum0)
    ### Adjust non-missing values
    df_national <- df_national |> mutate(not_isNA = (not_isNA > 0) * 1)
    df_national <- df_national |> mutate(not_isNA = not_isNA |> na_if(0))
    df_national <- df_national |> (function(x){
      x[, summaryCols] <- x[, summaryCols] * x[["not_isNA"]]; return(x)
    })()
    ### Drop columns, adjust values
    df_national <- df_national |> select(-c("not_isNA"))
    df_national <- df_national |> mutate(region="National Total")
    ### Join with National Pop
    # join0       <- natPopCols |> (function(y){y[!(y %in% c("national_pop", popCol0))]})()
    # df_national <- df_national |> left_join(nationalPop, by = c(join0))
    if(byState){
      df_national   <- df_national |> mutate(state ="All")
      df_national   <- df_national |> mutate(postal="US")
    } ### End if(byState)

    ### Add back into regional values and bind national population to impact types
    # df_agg |> glimpse(); df_national |> glimpse()
    df_agg      <- df_agg |> rbind(df_national);

    ### Add national to total populations
    # regionalPop |> glimpse(); nationalPop |> glimpse()
    regionalPop <- regionalPop |> rbind(nationalPop)
    ### Remove values
    rm(df_national, group0)
  } ### End if national
  # paste0("Finished national totals: ", nrow(df_agg)) |> print; df_agg |> head |> glimpse
  # "got here3" |> print()

  ###### ** Impact Types ######
  ### Summarize by Impact Type
  if(aggImpTypes){
    if(msgUser){msg0 (1) |> paste0("Summing across impact types...") |> message()}
    ### Ungroup first
    df_agg  <- df_agg |> ungroup()
    ### Grouping columns
    group0  <- groupByCols |> (function(y){y[!(y %in% c("impactType", yearCol0))]})()
    group0  <- group0 |> c("year")
    ### Separate into observations that have a single impact type and those with multiple impacts
    ### Rename impact type for those with one impact
    df_imp1 <- df_agg |> filter(impactType!="N/A")
    df_agg  <- df_agg |> filter(impactType=="N/A") |> mutate(impactType="all")

    ### Summarize at impact types: Count number of impact types
    df_imp1 <- df_imp1 |> (function(w){
      w |> mutate(not_isNA = 1 * (!(w[[summaryCol1]] |> is.na())))
    })()
    ### Calculate number of observations
    sum0    <- summaryCols |> c("not_isNA")
    df_imp1 <- df_imp1 |>
      group_by_at(c(group0)) |>
      summarize_at(c(sum0), sum, na.rm=T) |> ungroup()
    rm(sum0)
    ### Adjust values & drop column
    df_imp1 <- df_imp1 |> mutate(not_isNA = (not_isNA > 0) * 1)
    df_imp1 <- df_imp1 |> mutate(not_isNA = not_isNA |> na_if(0))
    df_imp1 <- df_imp1 |> (function(x){
      x[, summaryCols] <- x[, summaryCols] * x[["not_isNA"]]; return(x)
    })()
    ### Drop columns and mutate values
    df_imp1 <- df_imp1 |> select(-c("not_isNA"))
    df_imp1 <- df_imp1 |> mutate(impactType="all") #|> as.data.frame()
    ### Bind values
    df_agg  <- df_agg |> rbind(df_imp1)
    rm(df_imp1)
    # "aggregate_impacts: got here5" |> print()
  } ### End if impactType in aggLevels
  # "got here4" |> print()

  ###### Join Base Scenario Info ######
  ### Join base scenario with driver scenario
  ### Join base scenario with population scenario
  join0         <- c(yearCol0)
  arrange0      <- c("region")  |> c(stateCols0) |> c("model_type") |> c(yearCol0)
  df_base       <- baseScenario |> left_join(driverScenario, by=c(join0), relationship="many-to-many")
  df_base       <- df_base |> left_join(regionalPop   , by=c(join0))
  df_base       <- df_base |> arrange_at(c(arrange0))
  rm(regionalPop, baseScenario, driverScenario)
  rm(join0, arrange0)

  ### Join base scenario with aggregated info
  # "got here5" |> print()
  ### Names
  names0        <- df_agg        |> names(); #aggNames |> print()
  names1        <- co_sectorVars |> names()
  join0         <- co_sectorVars |> names() |> (function(y){y[y %in% names0]})()
  join1         <- df_base       |> names() |> (function(y){y[y %in% c(names0, names1)]})()
  df_agg        <- df_agg |> left_join(co_sectorVars, by=c(join0))
  df_agg        <- df_agg |> left_join(df_base      , by=c(join1))
  rm(names0, names1, join0, join1)

  ###### Format Columns ######
  # ###### Reformat sectorprimary and includeaggregate, which were converted to character
  # mutate0         <- c("sectorprimary", "includeaggregate")
  # mutate0         <- mutate0[mutate0 %in% names(df_agg)]
  # doMutate        <- mutate0 |> length() > 0
  # if(doMutate){df_agg <- df_agg |> mutate_at(c(mutate0), as.numeric)}
  # if(doMutate){df_agg <- df_agg |> mutate_at(c(mutate0), as.numeric)}
  mutate0       <- baseCols |> c(popCol0) |> c("driverValue")
  mutate0       <- mutate0  |> c(summaryCols)
  mutate0       <- mutate0  |> c("sectorprimary", "includeaggregate")
  mutate0       <- mutate0  |> unique()
  mutate0       <- mutate0  |> (function(y){y[y %in% (df_agg |> names())]})()
  doMutate      <- (mutate0 |> length()) > 0
  if(doMutate){df_agg <- df_agg |> mutate_at(c(mutate0), as.numeric)}

  ###### Order Columns ######
  ### Order the data frame and ungroup
  ### Column indices of columns used in ordering
  # df_agg |> names() |> print(); groupByCols |> print()
  arrange0      <- groupByCols |> c(yearCol0) |> unique()
  df_agg        <- df_agg |> arrange_at(c(arrange0))
  df_agg        <- df_agg |> select(any_of(standardCols))
  df_agg        <- df_agg |> ungroup()

  ###### Return ######
  ### Return object
  # if(msgUser) message("\n", "Finished...")
  return(df_agg)
}

