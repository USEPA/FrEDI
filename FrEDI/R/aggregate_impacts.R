###### aggregate_impacts ######
### Created 2021.02.08.
#' Summarize and aggregate impacts from FrEDI (calculate national totals, average across models, sum impact types, and interpolate between impact year estimates)
#'
#' @description
#' Summarize and aggregate impacts from FrEDI (calculate national totals, average across models, sum impact types, and interpolate between impact estimate years).
#'
#' @param data      Data frame of results FrEDI (outputs from [FrEDI::run_fredi()])
#' @param columns   Character vector of columns for which to aggregate results (defaults to `columns=c("annual_impacts"`)).
#' @param aggLevels Levels of aggregation at which to summarize data: one or more of `c("national", "modelAverage", "impactYear", "impactType", "all")`. Defaults to all levels (i.e., `aggLevels="all"`).
#' @param groupByCols Character vector indicating which columns to use for grouping. Defaults to `groupByCols=c("sector", "variant", "impactYear", "impactType", "model_type", "model", "region")`. Note that the `"variant"` column referred to below contains information about the variant (or adaptation) name or `“N/A”`, as applicable.
#'
#' @details
#' This post-processing helper function aggregates and summarizes the FrEDI results to levels of aggregation specified by the user (passed to `aggLevels`). Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels="national"`), averaging across model types and models (`aggLevels="modelAverage"`), summing over all impact types (`aggLevels="impactType"`), and interpolating between impact year estimates (`aggLevels="impactYear"`). Users can specify all aggregation levels at once by specifying `aggLevels="all"` (default).
#' This post-processing helper function aggregates and summarizes temperature binning results to levels of aggregation specified by the user (passed to `aggLevels`). Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels="national"`), averaging across model types and models (`aggLevels="modelaverage"`), summing over all impact types (`aggLevels="impacttype"`), and interpolate between impact year estimates (`aggLevels="impactyear"`). Users can specify all aggregation levels at once by specifying `aggLevels="all"` (default) or no aggregation levels (`aggLevels="none"`).
#'
#' Before aggregating impacts for national totals and/or model averages, [FrEDI::aggregate_impacts()] will drop any pre-summarized results  (i.e., values for which `region="National Total"` and/or for which `model="average"`, respectively) that are already present in the data `and then reaggregate at those levels.
#'
#' For each of the `aggLevels`, [FrEDI::aggregate_impacts()] performs the following calculations (note that the `"variant"` column referred to below contains information about the variant (or adaptation) name or `“N/A”`, as applicable):
#'
#' \tabular{ll}{
#' \strong{Aggregation Level} \tab \strong{Description} \cr
#' `national` \tab Annual values are summed across all regions present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`,  `"impactYear"`, `"model_type"`, `"model"`, and `"year"`) and summed across regions. Years which have missing column data for all regions return as `NA`. The rows of the data frame of national values (with column `region="National Total"`) are then added as rows to the results. \cr
#' `modelaverage` \tab For temperature-driven sectors, annual results are averaged across all GCM models present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"model_type"`, `"region"`, and `"year"` and averaged across models (SLR impacts are estimated as an interpolation between SLR scenarios). Averages exclude missing values. Years which have missing column data for all models return as `NA`. The rows of model averages (with column `model="Average"` are then added as rows to the results data frame. \cr
#' `impacttype` \tab Annual results are summed across all impact types by sector present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`,`"model_type"`, `"model"`, `"region"`, and `"year"` and summed across impact types. Mutates column `impactType="all"` for all values. Years which have missing column data for all impact types return as `NA`. If results are aggregated across impact types, information about physical impacts (columns `"physicalmeasure"` and `"physical_impacts"`) are dropped.\cr
#' `impactyear` \tab Annual results for sectors with only one impact year estimate (i.e., `impactYear = "N/A"`) are separated from those with multiple impact year estimates. Sectors with multiple impact years have separate results for impact years 2010 and 2090. For these sectors, annual results are linearly interpolated between impact year estimates. For any model run years above 2090, annual results for sectors with multiple impact years return the 2090 estimate. The interpolated values are bound back to the results for sectors with a single impact year estimate, and column `impactYear` set to `impactYear="Interpolation"` for all values. \cr
#' }
#'
#' Note that [FrEDI::aggregate_impacts()] drops columns not used in grouping or aggregation.
#'
#' @return
#' The output of [FrEDI::run_fredi()] is an R data frame object containing annual average impacts, by year (2010-2090), summarized at the specified aggregation and grouping levels.
#'
#' @examples
#' ### Create temperature binning scenario
#' df_tempExOut <- run_fredi(aggLevels="none", pv=TRUE, silent=TRUE)
#'
#' ### Aggregate temperature binning summary across multiple columns
#' agg_tempExOut <- df_tempExOut |> aggregate_impacts(columns=c("annual_impacts", "discounted_impacts"))
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
    aggLevels   = c("national", "modelaverage", "impactyear", "impacttype"),  ### Levels of aggregation
    columns     = c("annual_impacts"), ### Columns to aggregate
    groupByCols = c("sector", "variant", "impactYear", "impactType", "model_type", "model", "region")
){
  ###### Defaults ######
  ### Not used currently; preserving it in messaging logicals for the future
  msgUser        <- T

  ###### Set Values ######
  ###### - Args ######
  null_groupCols <- groupByCols |> is.null()

  ####### By State  ######
  byState        <- c("state") %in% groupByCols
  if(byState){stateCols0 <- c("state", "postal")} else{stateCols0 <- c()}
  popCol0        <- byState |> ifelse("state_pop", "reg_pop")
  # byState |> print()

  ###### Format Data ######
  ### Ungroup
  data           <- data |> ungroup() #; names(data) |> print()
  # data |> glimpse()
  ###### Years Info ######
  ### Years in data
  # c_npdRefYear <- 2090
  c_dataYears    <- data$year |> unique()
  # has_plus2090vals <- (c_dataYears[which(c_dataYears > c_npdRefYear)] |> length()) > 0

  ###### Get FrEDI Data Objects ######
  co_models      <- "co_models"   |> get_frediDataObj("frediData")
  co_sectors     <- "co_sectors"  |> get_frediDataObj("frediData")
  co_variants    <- "co_variants" |> get_frediDataObj("frediData")

  ###### - Formate SLR Info ######
  # assign("slr_cm", rDataList[["slr_cm"]])
  # assign("co_models", rDataList[["co_models"]])
  co_slrs        <- co_models |> filter(modelType=="slr")
  co_slrs        <- co_slrs   |> mutate_at(.vars=c("model_dot", "model_label"), as.character)
  # co_slrs <- co_slrs   |> as.data.frame()

  ###### - Format Sector Info ######
  rename0        <- c("sector_id", "sector_label", "modelType")
  rename1        <- c("sector_id", "sector", "model_type")
  co_sectors     <- co_sectors |> select(all_of(rename0))
  co_sectors     <- co_sectors |> rename_at(c(rename0), ~rename1)
  co_sectors     <- co_sectors |> mutate_at(c("model_type"), toupper)
  rm("rename0", "rename1")

  ###### - Format Variant Info ######
  ### Get input scenario info: co_inputScenarioInfo
  ### Load variant info table from sysdata.rda
  ### Format variants
  rename0        <- c("sector_id", "variant_label")
  rename1        <- c("sector_id", "variant")
  select0        <- c(rename1) |> c("sectorprimary", "includeaggregate")
  co_variants    <- co_variants |>  rename_at(c(rename0), ~rename1)
  co_variants    <- co_variants |> select(c(all_of(select0)))
  rm("rename0", "rename1", "select0")

  ### Combine sector and variant info
  join0          <- "sector_id"
  co_sectorVars  <- co_sectors |> left_join(co_variants, by =join0)
  co_sectorVars  <- co_sectorVars |> select(-c(all_of(join0)))
  rm(join0)

  ###### Groups Columns  ######
  ### Filter to columns in data and mutate those columns to character
  def_groupByCols <- c("sector", "variant", "impactType", "impactYear", "region") |>
    c(stateCols0) |>
    c("model_type", "model", "sectorprimary", "includeaggregate")
  ### Use default group by columns if none specified...otherwise, check which are present
  if(null_groupCols){groupByCols <- def_groupByCols}
  ### Check if columns for grouping are there
  names_data0   <- data |> names()
  is_groupByCol <- groupByCols %in% names_data0
  which_notPresentGroups <- (!is_groupByCol) |> which()
  ### Message user if some columns aren't present
  if(length(which_notPresentGroups) > 0){
    groupByCols <- groupByCols[which(is_groupByCol)]
    if(msgUser){
      message("\t", "Warning: groupByCols = c(", paste(groupByCols[which_notPresentGroups], collapse = ", "), ") not present...")
      message("\t\t", "Grouping by remaining columns...")
    }
  }

  ### Add sector primary and include aggregate
  # newGroupCols <- c("sectorprimary", "includeaggregate") |>
  newGroupCols <- c("physicalmeasure") |>
    (function(y){y[which(!(y %in% groupByCols))]})() |>
    (function(y){y[which(y %in% names_data0)]})()
  ### If length(newGroupCols)>0, add them
  if(newGroupCols |> length() > 0){groupByCols <- groupByCols |> c(newGroupCols)}
  ### Remove extra names
  rm("is_groupByCol", "which_notPresentGroups", "newGroupCols")

  ###### Summary Columns  ######
  ### Columns to summarize
  # if(!is.null(columns)){summaryCols <- columns}
  def_summaryCols <- c("annual_impacts")
  if(columns |> is.null()) {summaryCols <- def_summaryCols}
  else                     {summaryCols <- columns}
  is_sumByCol <- summaryCols %in% names_data0
  which_notPresentSums <- (!is_sumByCol) |> which()
  ### Message user if some columns aren't present
  if(which_notPresentSums |> length() > 0){
    summaryCols <- summaryCols[which(is_sumByCol)]
    if(msgUser){
      paste0("\t", "Warning: columns = c(", paste(summaryCols[which_notPresentSum], collapse = ", "), ") not present...") |> message()
      paste0("\t\t", "Aggregating results for columns c(", paste(summaryCols, collapse=", "),")...") |> message()
    } ### End if message user
  } ### End if no sum columns present
  rm("is_sumByCol", "which_notPresentSums")
  summaryCol1     <- summaryCols[!(summaryCols %in% "physical_impacts")][1]
  ### Add physical impacts summary column
  newSumCols      <- c("physical_impacts") |>
    (function(y){y[which(!(y %in% summaryCols))]})() |>
    (function(y){y[which  (y %in% names_data0) ]})()
  ### If length(newGroupCols)>0, add them
  if(newSumCols |> length() > 0){summaryCols <- summaryCols |> c(newSumCols)}
  ### Number of summary columns
  num_summaryCols <- summaryCols |> length()
  data            <- data |> mutate_at(.vars=c(groupByCols), as.character)

  ###### Aggregation Levels  ######
  ### Types of summarization to do: default
  # aggList0     <- c("national", "modelaverage", "impactyear", "impacttype", "all")
  aggList0     <- c("national", "modelaverage", "impactyear", "impacttype")
  null_aggLvls <- aggLevels |> is.null()
  aggLevels    <- aggLevels |> tolower()
  aggNone      <- "none" %in% aggLevels
  aggAll       <- "all"  %in% aggLevels
  if(null_aggLvls | aggAll){
    aggLevels <- aggList0
  } else if(aggNone){
    aggLevels <- c()
    if(msgUser){
      paste0("\t", "No aggregation levels specified...", "\n") |>
        paste0("\t", "No aggregation levels specified...", "\n") |>
        paste0("\t", "Returning data...", "\n") |>
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

  ###### Standardize Columns  ######
  ### Associated Columns
  # data |> glimpse()
  baseCols      <- c("year", "gdp_usd", "national_pop", "gdp_percap")
  regPopCols    <- c("year", "region") |> c(stateCols0) |> c(popCol0) |> unique()
  natPopCols    <- c("year", "region") |> c("national_pop")
  driverCols    <- c("year", "model_type", "driverType", "driverUnit", "driverValue")
  ### Get names in names
  names0        <- data |> names()
  baseCols      <- baseCols   |> (function(y){y[which(y %in% names0)]})()
  regPopCols    <- regPopCols |> (function(y){y[which(y %in% names0)]})()
  natPopCols    <- natPopCols |> (function(y){y[which(y %in% names0)]})()
  driverCols    <- driverCols |> (function(y){y[which(y %in% names0)]})()
  rm(names0)

  ### List of standardized columns
  standardCols  <- c(groupByCols, baseCols, regPopCols, natPopCols) |> unique()
  standardCols  <- standardCols |> c(driverCols, summaryCols) |> unique()
  scenarioCols  <- standardCols[!(standardCols %in% c(groupByCols, summaryCols, "year"))] |> unique()
  data          <- data |> select(c(any_of(standardCols)))#; names(data) |> print

  ###### Base Scenario Info  ######
  ### Some values are the same for all runs and regions...separate those values
  baseScenario  <- data |>
    group_by_at(.vars=c(baseCols)) |>
    summarize(n=n(), .groups="keep") |> ungroup() |>
    select(-c("n"))
  ### Regional population
  regionalPop  <- data |>
    group_by_at(.vars=c(regPopCols)) |>
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
    group_by_at(.vars=c(driverCols)) |>
    summarize(n=n(), .groups="keep") |> ungroup() |>
    select(-c("n"))

  ###### Aggregation  ######
  # if(requiresAgg){
  #   if(msgUser) message("Aggregating impacts...")
  #   }
  ###### Save a copy of the data

  ### Select appropriate columns
  df_agg <- data  |> select(-c(all_of(scenarioCols)))
  # df_agg |> nrow() |> print(); df_agg |> head() |> glimpse()
  rm(data)

  ###### Impact Years ######
  ### Separate into years after 2090 and before 2090
  if(aggImpYear){
    if(msgUser){paste0("\t", "Interpolating between impact year estimates...") |> message()}
    ### Ungroup first
    df_agg        <- df_agg |> ungroup()
    # summaryCol1 <- summaryCols[1]
    ### Group by columns
    groupCols0    <- groupByCols[which(groupByCols != "impactYear" )]
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
    rm("df_agg")

    ### Then do the post-2090 results
    ### Exclude 2010 results
    df_agg        <- df_aggImp_2   |> filter(impactYear != cImpYear1) |> mutate(impactYear="Interpolation")
    rm("df_aggImp_2")
    ### Process pre-2090:
    ### Separate out observations without impact years
    df_naYears    <- df_aggImp_1 |> filter(!(impactYear %in% impactYears)) |> mutate(impactYear="Interpolation")

    ### New upper and lower column names
    sumCols2090   <- paste(summaryCols, "2090", sep="_")
    sumCols2010   <- paste(summaryCols, "2010", sep="_")

    ### Filter to impact year in impact years
    df_impYears   <- df_aggImp_1 |> filter(impactYear %in% impactYears)
    nrow_impYrs   <- df_impYears |> nrow()
    rm("df_aggImp_1")
    ### For nrow_impYrs > 0
    if(nrow_impYrs){
      ### Filter to other lower models and then bind with the zero values, drop model column
      df2090      <- df_impYears |> filter(impactYear == cImpYear2) |> select(-c("impactYear"))
      df2090[,sumCols2090] <- df2090[,summaryCols]

      ### Drop summary columns from 2010
      df2010      <- df_impYears |> filter(impactYear == cImpYear1) |> select(-c("impactYear"))
      df2010[,sumCols2010] <- df2010[,summaryCols]
      df2010      <- df2010 |> select(-c(all_of(summaryCols)))

      ### Join upper and lower data frames and calculate the numerator, denominator, and adjustment factor
      df_impYears <- df2090 |> left_join(df2010, by=c(groupCols0, "year"))
      rm("df2090", "df2010")

      ### Add Impact year numerator and denominator
      df_impYears <- df_impYears |> mutate(numer_yr = year - nImpYear1)
      df_impYears <- df_impYears |> mutate(denom_yr = nImpYear2 - nImpYear1)
      df_impYears <- df_impYears |> mutate(adj_yr   = numer_yr / denom_yr )

      ### Iterate over summary columns
      for(i in 1:num_summaryCols){
        ### Upper/lower
        col_i       <- summaryCols[i]
        col_i_2010  <- col_i |> paste("2010", sep="_")
        col_i_2090  <- col_i |> paste("2090", sep="_")

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
        df_impYears <- df_impYears |> select(-c(all_of(select0)))
        df_impYears <- df_impYears |> rename_at(.vars=c(newCol_i), ~oldCol_i)
        df_impYears <- df_impYears |> select(-c(all_of(select1)))
        ### Remove values
        rm("i", "col_i", "col_i_2010", "col_i_2090", "oldCol_i", "newCol_i", select0)
      } ### End for(i in 1:num_summaryCols)
      ### Add new factor and drop columns
      df_impYears <- df_impYears  |> mutate(impactYear="Interpolation")
      df_impYears <- df_impYears  |> select(-c("numer_yr", "denom_yr", "adj_yr"))
    } ### End if(nrow_impYrs)
    rm("impactYears", "cImpYear1", "cImpYear2", "sumCols2010", "sumCols2090")
    rm("groupCols0", "c_cutoff_yr")
    ### Add back into values without NA years
    ### Join post 2090 results with earlier results
    df_aggImp_1   <- df_impYears  |> rbind(df_naYears) |> mutate(impactYear="Interpolation")
    df_agg        <- df_agg |> rbind(df_aggImp_1)
    rm("df_impYears", "df_naYears", "df_aggImp_1")
  } ### if(aggImpYear)
  # paste0("Finished impact year interpolation: ", nrow(df_agg)) |> print(); df_agg |> head() |> glimpse()
  # "got here1" |> print()

  ###### Model Averages ######
  ### Average values across models
  if(aveModels){
    modelAveMsg   <- "Calculating model averages..."
    if(msgUser){paste0("\t", modelAveMsg) |> message()}
    ### Ungroup first
    df_agg        <- df_agg |> ungroup()
    df_agg        <- df_agg |> mutate_at(.vars=c("model"), as.character) |> ungroup()
    ### Group by columns
    groupCols0    <- groupByCols[which(groupByCols != "model" )]
    ### Separate model types
    df_gcm        <- df_agg |> filter(tolower(model_type)=="gcm")
    df_slr        <- df_agg |> filter(tolower(model_type)=="slr")
    do_gcm        <- df_gcm |> nrow() > 0
    rm("df_agg")
    ### Calculate GCM model averages
    if(do_gcm){
      # ### Names of agg impacts
      # names_gcms   <- df_gcm |> names()
      ### Calculate number of non missing values
      df_modelAves <- df_gcm |> (function(w){
        w |> mutate(not_isNA = 1 * (!(w[[summaryCol1]] |> is.na())))
      })()
      ### Group data, sum data, calculate averages, and drop NA column
      group0       <- groupByCols[which(groupByCols != "model" )] |> c("year")
      sum0         <- summaryCols |> c("not_isNA")
      df_modelAves <- df_modelAves |>
        group_by_at(c(group0)) |>
        summarize_at(.vars=c(sum0), sum, na.rm=T) |> ungroup()
      rm(group0, sum0)
      ### Adjust for non-missing values
      df_modelAves <- df_modelAves |> mutate(not_isNA = not_isNA |> na_if(0))
      # df_modelAves[,summaryCols] <- df_modelAves[,summaryCols] / df_modelAves[["not_isNA"]]
      df_modelAves <- df_modelAves |> (function(x){
        x[,summaryCols] <- x[,summaryCols] / x$not_isNA; return(x)
      })()
      ### Drop columns
      df_modelAves <- df_modelAves |> select(-c("not_isNA"))
      ### Mutate models
      df_modelAves <- df_modelAves |> mutate(model = "Average")

      ### Add observations back in
      # df_agg <- df_agg |> rbind(df_modelAves)
      df_gcm        <- df_gcm |> rbind(df_modelAves)
      rm( "df_modelAves")
    } ### End if nrow(df_gcm)
    ### Bind GCM and SLR results
    df_agg <- df_gcm |> rbind(df_slr)
    rm( "df_gcm", "df_slr")
  } ### End if "model" %in% aggLevels
  # paste0("Finished model aggregation: ", nrow(df_agg)) |> print(); df_agg |> head() |> glimpse()
  # "got here2" |> print()

  ###### National Totals ######
  if(aggNational){
    if(msgUser){paste0("\t", "Calculating national totals...") |> message()}
    ### Ungroup first
    df_agg        <- df_agg |> ungroup()
    ### Calculate number of non missing values
    df_national   <- df_agg |> (function(w){
      w |> mutate(not_isNA = 1 * (!(w[[summaryCol1]] |> is.na())))
    })()
    ### Group data, sum data, calculate averages, and drop NA column
    group0        <- groupByCols[!(groupByCols %in% c("region", stateCols0))] |> c("year")
    sum0          <- summaryCols |> c("not_isNA")
    df_national   <- df_national |>
      group_by_at(c(group0)) |>
      summarize_at(vars(sum0), sum, na.rm=T) |> ungroup()
    rm(group0, sum0)
    ### Adjust non-missing values
    df_national   <- df_national |> mutate(not_isNA = (not_isNA > 0) * 1)
    df_national   <- df_national |> mutate(not_isNA = not_isNA |> na_if(0))
    df_national   <- df_national |> (function(x){
      x[, summaryCols] <- x[, summaryCols] * x[["not_isNA"]]; return(x)
    })()
    ### Drop columns, adjust values
    df_national   <- df_national |> select(-c("not_isNA"))
    df_national   <- df_national |> mutate(region="National Total")
    if(byState){
      df_national   <- df_national |> mutate(state ="All")
      df_national   <- df_national |> mutate(postal="US")
    } ### End if(byState)

    ### Add back into regional values and bind national population to impact types
    # df_agg |> glimpse(); df_national |> glimpse()
    df_agg        <- df_agg |> rbind(df_national);
    # regionalPop |> glimpse(); nationalPop |> glimpse()

    ### Add national to total populations
    # regionalPop |> glimpse(); nationalPop |> glimpse()
    regionalPop   <- regionalPop   |> rbind(nationalPop)
    ### Remove values
    rm("df_national")
  } ### End if national
  # paste0("Finished national totals: ", nrow(df_agg)) |> print; df_agg |> head |> glimpse
  # "got here3" |> print()

  ###### Impact Types ######
  ### Summarize by Impact Type
  if(aggImpTypes){
    if(msgUser){paste0("\t", "Summing across impact types...") |> message()}
    ### Ungroup first
    df_agg        <- df_agg |> ungroup()
    ### Drop some columns and update values
    drop0         <- c("physicalmeasure", "physical_impacts")
    groupByCols   <- groupByCols[which(!(groupByCols %in% c(drop0)))]
    summaryCols   <- summaryCols[which(!(summaryCols %in% c(drop0)))]
    standardCols  <- standardCols[which(!(standardCols %in% c(drop0)))]
    df_agg        <- df_agg |> select(-c(any_of(drop0)))
    rm(drop0)
    ### Names
    # namesAgg0      <- df_agg |> names()
    ### Separate into observations that have a single impact type and those with multiple impacts
    ### Rename impact type for those with one impact
    df_imp1       <- df_agg |> filter(impactType!="N/A")
    df_imp0       <- df_agg |> filter(impactType=="N/A") |> mutate(impactType="all")
    rm("df_agg")

    ### Summarize at impact types: Count number of impact types
    df_imp1 <- df_imp1 |> (function(w){
      w |> mutate(not_isNA = 1 * (!(w[[summaryCol1]] |> is.na())))
    })()
    ### Calculate number of observations
    group0        <- groupByCols[which(!(groupByCols %in% c("impactType")))] |> c("year")
    sum0          <- summaryCols |> c("not_isNA")
    df_imp1       <- df_imp1 |>
      group_by_at(.vars=c(group0)) |>
      summarize_at(.vars=c(sum0), sum, na.rm=T) |> ungroup()
    rm(group0, sum0)
    ### Adjust values & drop column
    df_imp1       <- df_imp1 |> mutate(not_isNA = (not_isNA > 0) * 1)
    df_imp1       <- df_imp1 |> mutate(not_isNA = not_isNA |> na_if(0))
    df_imp1       <- df_imp1 |> (function(x){
      x[, summaryCols] <- x[, summaryCols] * x[["not_isNA"]]; return(x)
    })()
    ### Drop columns and mutate values
    df_imp1       <- df_imp1 |> select(-c("not_isNA"))
    df_imp1       <- df_imp1 |> mutate(impactType="all") #|> as.data.frame()
    ### Bind values
    df_agg        <- df_imp0 |> rbind(df_imp1)
    rm("df_imp0", "df_imp1")
    # "aggregate_impacts: got here5" |> print()
  } ### End if impactType in aggLevels
  # "got here4" |> print()

  ###### Join Base Scenario Info ######
  ### Join Base Scenario Back In
  ### Join national info with population
  # "aggregate_impacts: got here6" |> print()
  # df_base      |> head() |> glimpse() |> print()
  join0           <- c("year")
  arrange0        <- c("model_type", "region") |> c(stateCols0) |> c("year")
  regionalPop     <- regionalPop  |> mutate(year = year |> as.numeric())
  df_base         <- baseScenario |> mutate(year = year |> as.numeric())
  df_base         <- df_base      |> left_join(regionalPop   , by=c(join0))
  df_base         <- df_base      |> left_join(driverScenario, by=c(join0))
  df_base         <- df_base      |> arrange_at(arrange0)
  rm("regionalPop", "baseScenario", "driverScenario", join0, arrange0)
  # "got here5" |> print()

  # df_base |> dim() |> print() ### 1470 rows, 13 columns
  ### Names
  aggNames        <- df_agg |> names(); #aggNames |> print()
  svNames         <- co_sectorVars |> names(); #svNames |> print()
  # svJoin          <- c("model_type", "sector", "variant")
  svJoin0         <- c("model_type", "sector", "variant")
  svJoin1         <- c("year", "region") |> c(stateCols0) |> c("model_type")
  svDrop          <- svNames[which((svNames %in% aggNames) & !(svNames %in% svJoin0))]
  df_return       <- df_agg    |> left_join(co_sectorVars |> select(-c(all_of(svDrop))), by = c(svJoin0))
  df_return       <- df_return |> left_join(df_base, by = c(svJoin1))
  rm("df_agg", svNames, "svDrop", "svJoin0", "svJoin1")
  ###### Reformat sectorprimary and includeaggregate, which were converted to character
  mutate0         <- c("sectorprimary", "includeaggregate")
  mutate0         <- mutate0[mutate0 %in% names(df_return)]
  doMutate        <- mutate0 |> length() > 0
  if(doMutate){df_return <- df_return |> mutate_at(c(mutate0), as.numeric)}
  if(doMutate){df_return <- df_return |> mutate_at(c(mutate0), as.numeric)}

  ###### Order Columns ######
  ### Order the data frame and ungroup
  ### Column indices of columns used in ordering
  arrange0        <- df_return |> names() |> c("year") |> unique()
  standardCols    <- standardCols[standardCols %in% arrange0    ]
  arrange0        <- arrange0    [arrange0     %in% standardCols]
  df_return       <- df_return |> arrange_at(c(arrange0))
  df_return       <- df_return |> select(any_of(standardCols))
  df_return       <- df_return |> ungroup()

  ###### Return ######
  ### Return object
  # if(msgUser) message("\n", "Finished...")
  return(df_return)
}

