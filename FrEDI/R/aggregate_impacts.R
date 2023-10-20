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
    groupByCols = c("sector", "variant", "impactYear", "impactType", "model_type", "model", "region"), ### Columns to group by
    mode = "all"
){


  ###### Defaults ######
  ### Not used currently; preserving it in messaging logicals for the future
  msgUser <- T
  ### Modes...specifically SLR interpolation only
  xMode   <- ifelse(is.null(mode), "all", mode) |> tolower()

  ###### Ungroup Data ######
  data    <- data |> ungroup() #; names(data) |> print()

  ###### Years Info ######
  ### Years in data
  # c_npdRefYear <- 2090
  c_dataYears  <- data$year |> unique()
  # has_plus2090vals <- (c_dataYears[which(c_dataYears > c_npdRefYear)] |> length()) > 0

  ###### SLR Info ######
  # assign("slr_cm", rDataList[["slr_cm"]])
  assign("co_models", rDataList[["co_models"]])
  co_slrs <- co_models |> filter(modelType=="slr")
  co_slrs <- co_slrs   |>  mutate_at(.vars=c("model_dot", "model_label"), as.character)
  co_slrs <- co_slrs   |> as.data.frame()

  ###### Load Sector Info ######
  name_dfSectors   <- "co_sectors"
  sectorCols_old   <- c("sector_id", "sector_label", "modelType")
  sectorCols_new   <- c("sector_id", "sector", "model_type")

  assign(name_dfSectors, rDataList[[name_dfSectors]])

  co_sectors  <- co_sectors |> select(all_of(sectorCols_old))
  co_sectors  <- co_sectors |> rename_with(~sectorCols_new, .cols=sectorCols_old)
  # co_sectors  <- co_sectors |> (function(y){names(y) <- c(sectorCols_new); return(y)})
  co_sectors  <- co_sectors |> mutate(model_type = model_type |> toupper())
  rm("sectorCols_old", "sectorCols_new")

  ###### Load Variant Info ######
  ### Get input scenario info: co_inputScenarioInfo
  ### Load variant info table from sysdata.rda
  name_dfVariant    <- "co_variants"
  assign(name_dfVariant, rDataList[[name_dfVariant]])
  ### Format variants
  variantCols_old   <- c("sector_id", "variant_label")
  variantCols_new   <- c("sector_id", "variant")
  variantCols_other <- c("sectorprimary", "includeaggregate")
  co_variants  <- co_variants |> select(c(all_of(variantCols_old), all_of(variantCols_other)))
  # co_variants  <- co_variants |> (function(y){names(y) <- c(variantCols_new, variantCols_other); return(y)})
  co_variants  <- co_variants |>  rename_with(~variantCols_new, .cols=variantCols_old)
  rm("variantCols_old", "variantCols_new", "variantCols_other")

  ### Combine sector and variant info
  co_sector_variants <- co_sectors |> left_join(co_variants, by = "sector_id") |> select(-c("sector_id"))


  ###### Groups Columns  ######
  ### Filter to columns in data and mutate those columns to character
  default_groupByCols <- c("sector", "variant", "impactYear", "impactType", "model_type", "model", "region")
  ### Use default group by columns if none specified...otherwise, check which are present
  if(is.null(groupByCols)){
    groupByCols <- default_groupByCols
  }
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
  newGroupCols <- c("sectorprimary", "includeaggregate", "physicalmeasure") |>
    (function(y){y[which(!(y %in% groupByCols))]})() |>
    (function(y){y[which(y %in% names_data0)]})()
  ### If length(newGroupCols)>0, add them
  if(length(newGroupCols)>0){
    groupByCols <- groupByCols |> c(newGroupCols)
  }
  ### Remove extra names
  rm("is_groupByCol", "which_notPresentGroups", "newGroupCols")

  ###### Summary Columns  ######
  ### Columns to summarize
  # if(!is.null(columns)){summaryCols <- columns}
  default_summaryCols <- c("annual_impacts")
  if(is.null(columns)) {summaryCols <- default_summaryCols}
  else                 {summaryCols <- columns}

  is_sumByCol <- summaryCols %in% names_data0
  which_notPresentSums <- (!is_sumByCol) |> which()
  ### Message user if some columns aren't present
  if(length(which_notPresentSums) > 0){
    summaryCols <- summaryCols[which(is_sumByCol)]
    if(msgUser){
      message("\t", "Warning: columns = c(", paste(summaryCols[which_notPresentSum], collapse = ", "), ") not present...")
      message("\t\t", "Aggregating results for columns c(", paste(summaryCols, collapse=", "),")...")
    }
  }
  rm("is_sumByCol", "which_notPresentSums")
  summaryCol1         <- summaryCols[1]
  ### Add physical impacts summary column
  newSumCols <- c("physical_impacts") |>
    (function(y){y[which(!(y %in% summaryCols))]})() |>
    (function(y){y[which(y %in% names_data0)]})()
  ### If length(newGroupCols)>0, add them
  if(length(newSumCols)>0){
    summaryCols <- summaryCols |> c(newSumCols)
  }
  ### Number of summary columns
  num_summaryCols <- summaryCols |> length()
  data            <- data |> mutate_at(.vars=c(all_of(groupByCols)), as.character)

  ###### Aggregation Levels  ######
  ### Types of summarization to do: default
  aggList0 <- c("national", "modelaverage", "impactyear", "impacttype", "all")
  if(!is.null(aggLevels)){
    aggLevels <- aggLevels |> tolower()
    ### If the user specified none, return the data
    if("none" %in% aggLevels){
      aggLevels <- c()
      if(msgUser){
        message("\t", "No aggregation levels specified...")
        message("\t", "Returning data...")
        message("Finished.")
      }
      return(data)
    }
    ### If the user specified all, use all the aggregation levels
    if("all"  %in% aggLevels) aggLevels <- aggList0
  } else{
    aggLevels <- aggList0
  }

  if(xMode=="slrinterpolation"){
    aggLevels <- "modelaverage"
  }

  requiresAgg <- length(aggLevels) > 0

  ###### Aggregation Level Options  ######
  ### Aggregate to impact years or national...reduces the columns in output
  impactYearsAgg <- ifelse("impactyear"   %in% aggLevels, T, F)
  nationalAgg    <- ifelse("national"     %in% aggLevels, T, F)
  impactTypesAgg <- ifelse("impacttype"   %in% aggLevels, T, F)
  modelAveAgg    <- ifelse("modelaverage" %in% aggLevels, T, F)


  ###### Standardize Columns  ######
  ### Associated Columns
  baseScenarioCols   <- c("year", "gdp_usd", "national_pop", "gdp_percap") |> (function(y){y[which(y %in% names(data))]})()
  regionalPopCols    <- c("year", "region", "reg_pop") |> (function(y){y[which(y %in% names(data))]})()
  nationalPopCols    <- c("year", "region", "national_pop") |> (function(y){y[which(y %in% names(data))]})()
  driverScenarioCols <- c("year", "model_type", "driverType", "driverUnit", "driverValue") |> (function(y){y[which(y %in% names(data))]})()


  ### List of standardized columns
  standardCols       <- c(baseScenarioCols, regionalPopCols, nationalPopCols) |> unique()
  # standardCols       <- c(standardCols) |> unique()
  # summaryCols        <- c(summaryCols) |> unique()
  ### Standardize columns
  standardCols <- c(groupByCols, standardCols, driverScenarioCols, summaryCols) |> unique()


  ### If "national" aggregation, filter out national totals
  if(nationalAgg){
    data <- data |> filter(region!="National Total")
  }
  ### If modelAverage %in% aggLevels, filter out model averages
  if(modelAveAgg){
    data <- data |> filter(!(model %in% c("Average", "Model Average")))
  }
  data <- data[,(names(data) %in% standardCols)]#; names(data) |> print

  ###### Base Scenario Info  ######
  ### Some values are the same for all runs and regions...separate those values
  sectorsList   <- data$sector |> unique()
  sector0       <- sectorsList |> first()
  variant0      <- (data |> filter(sector==sector0))$variant |> unique() |> first()
  region0       <- (data |> filter(sector==sector0))$region |> unique() |> first()
  model0        <- (data |> filter(sector==sector0))$model |> unique() |> first()
  impactType0   <- (data |> filter(sector==sector0))$impactType |> unique() |> first()
  ### Base Scenario and regional population
  baseScenario  <- data |> filter(sector == sector0, variant == variant0, region == region0, model == model0, impactType == impactType0)
  regionalPop   <- data |> filter(sector == sector0, variant == variant0, model == model0, impactType == impactType0)

  ### Filter to impact types
  if(impactTypesAgg){
    impactType0  <- baseScenario$impactType |> unique() |> first()
    baseScenario <- baseScenario |> filter(impactType == impactType0)
    regionalPop  <- regionalPop  |> filter(impactType == impactType0)
  }
  ### Filter to impact years
  if(impactYearsAgg){
    impactYear0  <- baseScenario$impactYear |> unique() |> first()
    baseScenario <- baseScenario |> filter(impactYear == impactYear0)
    regionalPop  <- regionalPop  |> filter(impactYear == impactYear0)
  }
  ### Select columns
  ### Base Scenario, regional population, national population
  baseScenario <- baseScenario |> select(all_of(baseScenarioCols))
  regionalPop  <- regionalPop  |> select(all_of(regionalPopCols))
  ### Create national population scenario from the base scenario
  nationalPop  <- baseScenario |>
    mutate(region = "National Total") |>
    select(all_of(nationalPopCols)) |>
    rename(reg_pop=national_pop)

  ###### Driver Scenario ######
  ### Get unique model types, sectors, variants, and models
  names_x        <- data |> names()
  modelTypesList <- data$model_type |> unique()
  driverScenario <- modelTypesList |>
    lapply(function(model_type_i){
      ### Filter to sector
      df_i      <- data |> filter(model_type==model_type_i)
      sector_i  <- df_i$sector |> unique() |> first()
      df_i      <- df_i |> filter(sector == sector_i)
      ### Filter to variant
      variant_i <- df_i$variant |> unique() |> first()
      df_i      <- df_i |> filter(variant == variant_i)
      ### Filter to impact type
      if("impactType" %in% names_x){
        type_i    <- df_i$impactType |> unique() |> first()
        df_i      <- df_i |> filter(impactType == type_i)
      }
      ### Filter to region
      if("region" %in% names_x){
        region_i  <- df_i$region |> unique() |> first()
        df_i      <- df_i |> filter(region == region_i)
      }
      ### Filter to model
      if("model" %in% names_x){
        model_i  <- df_i$model |> unique() |> first()
        df_i     <- df_i |> filter(model == model_i)
      }
      ### Filter to impact year
      if("impactYear" %in% names_x){
        year_i  <- df_i$impactYear |> unique() |> first()
        df_i     <- df_i |> filter(impactYear == year_i)
      }
      ### Select columns
      df_i <- df_i |> select(all_of(driverScenarioCols))
      ### Return
      return(df_i)
    }) |> (function(x){do.call(rbind, x)})()
  # driverScenario |> dim() |> print()

  ###### Aggregation  ######
  # if(requiresAgg){
  #   if(msgUser) message("Aggregating impacts...")
  #   }
  ###### Save a copy of the data
  scenarioCols <- c(baseScenarioCols, regionalPopCols, nationalPopCols, driverScenarioCols) |> unique() |>
    (function(y){y[which(!(y %in% c(groupByCols, "year")))]})() #; scenarioCols |> print()
  ### Select appropriate columns
  df_aggImpacts <- data  |> select(-c(all_of(scenarioCols)))
  # df_aggImpacts |> nrow() |> print(); df_aggImpacts |> head() |> glimpse()

  ###### Impact Years ######
  ### Separate into years after 2090 and before 2090
  if(impactYearsAgg){
    if(msgUser) message("\t", "Interpolating between impact year estimates...")
    ### Ungroup first
    df_aggImpacts <- df_aggImpacts |> ungroup()
    # summaryCol1 <- summaryCols[1]
    ### Group by columns
    groupCols0    <- groupByCols[which(groupByCols != "impactYear" )]
    ### Impact years
    impactYears   <- c(2010, 2090) |> as.character()
    impactYear1   <- impactYears[1]
    impactYear2   <- impactYears[2]

    ### Separate data into years > 2090, years <= 2090
    c_cutoff_yr   <- 2090
    df_aggImp_1   <- df_aggImpacts |> filter(year <= c_cutoff_yr)
    df_aggImp_2   <- df_aggImpacts |> filter(year >  c_cutoff_yr)
    rm("df_aggImpacts")

    ### Then do the post-2090 results
    ### Exclude 2010 results
    df_aggImpacts <- df_aggImp_2   |> filter(impactYear != impactYear1) |> mutate(impactYear="Interpolation")
    rm("df_aggImp_2")
    ### Process pre-2090:
    ### Separate out observations without impact years
    df_naYears    <- df_aggImp_1 |> filter(!(impactYear %in% impactYears)) |> mutate(impactYear="Interpolation")

    ### New upper and lower column names
    new_2090SummaryCols <- paste(summaryCols, "2090", sep="_")
    new_2010SummaryCols <- paste(summaryCols, "2010", sep="_")

    ### Filter to impact year in impact years
    df_impYears <- df_aggImp_1 |> filter(impactYear %in% impactYears)
    nrow_impYrs <- df_impYears |> nrow()
    rm("df_aggImp_1")
    ### For nrow_impYrs > 0
    if(nrow_impYrs){
      ### Filter to other lower models and then bind with the zero values, drop model column
      df2090 <- df_impYears |> filter(impactYear == impactYear2) |> select(-c("impactYear"))
      df2090 <- df2090      |> (function(y){
        y <- y |> as.data.frame()
        y[,new_2090SummaryCols] <- y[,summaryCols]
        return(y)
      })()

      ### Drop summary columns from 2010
      df2010 <- df_impYears |> filter(impactYear == impactYear1) |> select(-c("impactYear"))
      df2010 <- df2010      |> (function(y){
        y <- y |> as.data.frame()
        y[,new_2010SummaryCols] <- y[,summaryCols]
        return(y)
      })() |>
        select(-c(all_of(summaryCols)))

      ### Join upper and lower data frames and calculate the numerator, denominator, and adjustment factor
      df_impYears <- df2090 |> left_join(df2010, by=c(groupCols0, "year"))
      rm("df2090", "df2010")
      ### Add Impact year numerator and denominator
      df_impYears <- df_impYears |> mutate(numer_yr = year-as.numeric(impactYear1))
      df_impYears <- df_impYears |> mutate(denom_yr = as.numeric(impactYear2)-as.numeric(impactYear1))
      df_impYears <- df_impYears |> mutate(adj_yr   = numer_yr/denom_yr)

      ### Iterate over summary columns
      for(i in 1:num_summaryCols){
        ### Upper/lower
        col_i      <- summaryCols[i]
        col_i_2010 <- col_i |> paste("2010", sep="_")
        col_i_2090 <- col_i |> paste("2090", sep="_")

        ### Calculate numerator and denominator
        df_impYears <- df_impYears |> as.data.frame()

        df_impYears$new_factor <- df_impYears[,col_i_2090] - df_impYears[,col_i_2010]
        df_impYears$new_value  <- df_impYears[,col_i_2010]
        # df_slrOther |> names() |> print()

        ### Update the new value
        oldCol_i <- col_i       |> c()
        newCol_i <- "new_value" |> c()
        ### Mutate and rename
        df_impYears <- df_impYears |> mutate(new_value = new_value + new_factor * adj_yr)
        df_impYears <- df_impYears |> select(-c(all_of(col_i), "new_factor"))
        df_impYears <- df_impYears |> rename_with(~oldCol_i[which(newCol_i==.x)], .cols=newCol_i)
        df_impYears <- df_impYears |> select(-c(all_of(col_i_2010), all_of(col_i_2090)))
        rm("i", "col_i", "col_i_2010", "col_i_2090", "oldCol_i", "newCol_i")
      } ### End for(i in 1:num_summaryCols)

      df_impYears <- df_impYears  |> mutate(impactYear="Interpolation")
      df_impYears <- df_impYears  |> select(-c("numer_yr", "denom_yr", "adj_yr"))
    }
    rm("impactYears", "impactYear1", "impactYear2", "new_2010SummaryCols", "new_2090SummaryCols")
    rm("groupCols0", "c_cutoff_yr")
    ### Add back into values without NA years
    ### Join post 2090 results with earlier results
    df_aggImp_1   <- df_impYears   |> rbind(df_naYears) |> mutate(impactYear="Interpolation")
    df_aggImpacts <- df_aggImpacts |> rbind(df_aggImp_1)
    rm("df_impYears", "df_naYears", "df_aggImp_1")
  }
  # paste0("Finished impact year interpolation: ", nrow(df_aggImpacts)) |> print(); df_aggImpacts |> head() |> glimpse()

  ###### Model Averages ######
  ### Average values across models
  if(modelAveAgg){
    modelAveMsg <- ifelse(xMode=="slrinterpolation", "Interpolating SLR impacts..." , "Calculating model averages...")
    if(msgUser) message("\t", modelAveMsg)
    ### Ungroup first
    df_aggImpacts <- df_aggImpacts |> mutate_at(.vars=c("model"), as.character) |> ungroup()
    ### Group by columns
    groupCols0    <- groupByCols[which(groupByCols != "model" )]
    ### Separate model types
    df_gcm        <- df_aggImpacts |> filter(tolower(model_type)=="gcm")
    df_slr        <- df_aggImpacts |> filter(tolower(model_type)=="slr")
    rm("df_aggImpacts")
    ###### GCM #######
    if(nrow(df_gcm)){
      ### Names of agg impacts
      names_gcms   <- df_gcm |> names()
      ### Calculate number of non missing values
      ### Group data, sum data, calculate averages, and drop NA column
      df_modelAves <- df_gcm |> (function(w){
        w$not_isNA <- (!is.na(w[,summaryCol1] |> as.vector()))*1
        return(w)
      })()
      df_modelAves <- df_modelAves |>
        group_by_at(c(all_of(groupCols0), "year")) |>
        summarize_at(.vars=c(all_of(summaryCols), "not_isNA"), sum, na.rm=T) |> ungroup()
      df_modelAves <- df_modelAves |> mutate(not_isNA = not_isNA |> na_if(0)) |>
        as.data.frame() |> (function(x){
          x[,summaryCols] <- x[,summaryCols] / x$not_isNA
          return(x)
        })()
      df_modelAves <- df_modelAves |> select(-c("not_isNA"))
      df_modelAves <- df_modelAves |> mutate(model = "Average")

      ### Add observations back in
      # df_aggImpacts <- df_aggImpacts |> rbind(df_modelAves)
      df_gcm        <- df_gcm |> rbind(df_modelAves)
      rm("names_gcms", "df_modelAves")
    } ### End if nrow(df_gcm)
    ### Bind GCM and SLR results
    df_aggImpacts <- df_gcm |> rbind(df_slr)
    rm( "df_gcm", "df_slr", "groupCols0")
  } ### End if "model" %in% aggLevels
  # paste0("Finished model aggregation: ", nrow(df_aggImpacts)) |> print(); df_aggImpacts |> head() |> glimpse()

  ###### National Totals ######
  if(nationalAgg){
    if(msgUser) message("\t", "Calculating national totals...")
    ### Ungroup first
    df_aggImpacts <- df_aggImpacts |> ungroup()
    ### Group by columns
    groupCols0    <- groupByCols[which(groupByCols != "region" )]
    ### Filter to national values and not national values
    ### Calculate number of non missing values
    df_national   <- df_aggImpacts |> (function(w){
      w <- w |> as.data.frame()
      w$not_isNA <- (!is.na(w[,summaryCol1]))*1
      return(w)
    })()
    ### Group data, sum data, calculate averages, and drop NA column
    df_national   <- df_national |>
      group_by_at(c(all_of(groupCols0), "year")) |>
      summarize_at(vars(all_of(summaryCols), not_isNA), sum, na.rm=T) |> ungroup()
    df_national   <- df_national |> mutate(not_isNA = (not_isNA>=1)*1)
    df_national   <- df_national |> mutate(not_isNA = not_isNA |> na_if(0))
    df_national   <- df_national |> (function(x){
      x[, summaryCols] <- x[, summaryCols]*x$not_isNA; return(x)
    })()
    df_national   <- df_national |> select(-c("not_isNA"))
    df_national   <- df_national |> mutate(region="National Total")
    ### Add back into regional values and bind national population to impact types
    df_aggImpacts <- df_aggImpacts |> rbind(df_national);
    regionalPop   <- regionalPop   |> rbind(nationalPop)
    ### Remove values
    rm("df_national", "nationalPop", "groupCols0")
  } ### End if national
  # paste0("Finished national totals: ", nrow(df_aggImpacts)) |> print; df_aggImpacts |> head |> glimpse
  # "got here1" |> print

  ###### Impact Types ######
  ### Summarize by Impact Type
  if(impactTypesAgg){
    if(msgUser) message("\t", "Summing across impact types...")
    ### Ungroup first
    df_aggImpacts <- df_aggImpacts |> ungroup()
    ### Group by columns
    dropCols0     <- c("physicalmeasure", "physical_impacts")
    df_aggImpacts <- df_aggImpacts |> (function(y){
      names_y <- y |> names()
      names_y <- names_y[which(!(names_y %in% dropCols0))]
      y       <- y |> select(all_of(names_y))
      return(y)
    })()
    ### Names
    # namesAgg0      <- df_aggImpacts |> names()
    ### Columns
    groupByCols   <- groupByCols[which(!(groupByCols %in% c(dropCols0)))]
    summaryCols   <- summaryCols[which(!(summaryCols %in% c(dropCols0)))]
    summaryCol1   <- summaryCols[1]
    standardCols  <- standardCols[which(!(standardCols %in% c(dropCols0)))]
    ### GroupByCols
    groupCols0    <- groupByCols[which(!(groupByCols %in% c("impactType")))]
    # nGroupCols0   <- groupCols0 |> length()

    ### Separate into observations that have a single impact type and those with multiple impacts
    ### Rename impact type for those with one impact
    df_aggImpacts1 <- df_aggImpacts |> filter(impactType=="N/A") |> mutate(impactType="all")
    df_aggImpactsN <- df_aggImpacts |> filter(impactType!="N/A")
    # "aggregate_impacts: got here2" |> print()
    ### Remove df_aggImpacts
    rm("df_aggImpacts")

    ### Summarize at impact types: Count number of impact types
    df_aggImpactsN <- df_aggImpactsN |> (function(w){
        w <- w |> as.data.frame()
        w$not_isNA <- (!is.na(w[,summaryCol1]))*1
        return(w)
      })()
    ### Calculate number of observations
    df_aggImpactsN <- df_aggImpactsN |>
      group_by_at(.vars=c(all_of(groupCols0), "year")) |>
      summarize_at(.vars=c(all_of(summaryCols), "not_isNA"), sum, na.rm=T) |>
      as.data.frame() |> ungroup()

    # "aggregate_impacts: got here3" |> print()
    df_aggImpactsN <- df_aggImpactsN |> mutate(not_isNA = (not_isNA > 0)*1)
    df_aggImpactsN <- df_aggImpactsN |> mutate(not_isNA = not_isNA |> na_if(0))
    df_aggImpactsN <- df_aggImpactsN |> (function(x){
        x[, summaryCols] <- x[, summaryCols]*x$not_isNA; return(x)
      })()
    df_aggImpactsN <- df_aggImpactsN |> select(-c("not_isNA"))
    df_aggImpactsN <- df_aggImpactsN |> mutate(impactType="all") |> as.data.frame()
    # "aggregate_impacts: got here4" |> print()
    ### Add to impacts
    # df_aggImpacts <- df_oneImpact |> rbind(df_nImpacts) |> mutate(impactType="all")
    # rm("df_oneImpact", "df_nImpacts")
    df_aggImpacts <- df_aggImpacts1 |> rbind(df_aggImpactsN)
    rm("df_aggImpacts1", "df_aggImpactsN", "groupCols0")
    # "aggregate_impacts: got here5" |> print()
  } ### End if impactType in aggLevels

  ###### Join Base Scenario Info with Aggregated Data ######
  ### Join national info with population
  # "aggregate_impacts: got here6" |> print()
  # df_base      |> head() |> glimpse() |> print()
  regionalPop     <- regionalPop  |> mutate(year = year |> as.numeric())
  df_base         <- baseScenario |> mutate(year = year |> as.numeric())
  df_base         <- df_base      |> left_join(regionalPop   , by=c("year"))
  df_base         <- df_base      |> left_join(driverScenario, by=c("year"))
  rm("regionalPop", "baseScenario", "driverScenario")

  # df_base |> dim() |> print() ### 1470 rows, 13 columns
  ### Names
  aggNames        <- df_aggImpacts |> names(); #aggNames |> print()
  svNames         <- co_sector_variants |> names(); #svNames |> print()
  svJoin          <- c("model_type", "sector", "variant")
  svDrop          <- svNames[which((svNames %in% aggNames) & !(svNames %in% svJoin))]
  # "aggregate_impacts: got here7" |> print()

  df_return       <- df_aggImpacts |> left_join(co_sector_variants |> select(-c(all_of(svDrop))), by = c(all_of(svJoin)))
  df_return       <- df_return     |> left_join(df_base , by = c("year", "region", "model_type"))
  rm("df_aggImpacts", "svDrop", "svJoin", "svNames")
  ###### Reformat sectorprimary and includeaggregate, which were converted to character
  c_aggColumns    <- c("sectorprimary", "includeaggregate") |> (function(y){y[which(y %in% names(df_return))]})()
  if(length(c_aggColumns)){
    df_return     <- df_return |> mutate_at(.vars=c(all_of(c_aggColumns)), as.numeric)
  }

  ###### Order Columns ######
  ### Order the data frame and ungroup
  ### Column indices of columns used in ordering
  return_names  <- df_return |> names()
  orderColNames <- c(groupByCols, "year") |> (function(y){y[which(y %in% return_names)]})() #; "got here10" |> print()
  df_return     <- df_return |> arrange_at(.vars=c(all_of(orderColNames)))

  ###### Return ######
  ### Grouping columns, driver columns, scenario columns
  ### Make sure data is ungrouped and a data frame object
  df_return <- df_return |> select( all_of(standardCols))
  df_return <- df_return |> ungroup() |> as.data.frame()

  ### Return object
  # if(msgUser) message("\n", "Finished...")
  return(df_return)
}

