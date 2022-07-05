###### aggregate_impacts ######
### Created 2021.02.08.
#' Summarize and aggregate impacts from FrEDI (calculate national totals, average across models, sum impact types, and interpolate between impact year estimates)
#'
#' @description
#' Summarize and aggregate impacts from FrEDI (calculate national totals, average across models, sum impact types, and interpolate between impact estimate years).
#'
#' @param data      Dataframe of results FrEDI (outputs from [FrEDI::run_fredi()])
#' @param columns   Character vector of columns for which to aggregate results (defaults to `columns=c("annual_impacts"`)).
#' @param aggLevels Levels of aggregation at which to summarize data: one or more of `c("national"`, `"modelAverage"`, `"impactYear"`, `"impactType"`, `"all")`. Defaults to all levels (i.e., `aggLevels="all"`).
#' @param groupByCols Character vector indicating which columns to use for grouping. Defaults to `groupByCols=``c("sector"`, `"variant"`, `"impactYear"`, `"impactType"`, `"model_type"`, `"model"`, `"region")`. Note that the `"variant"` column referred to below contains information about the adaptation or variant name or `“N/A”`, as applicable.
#'
#' @details
#' This post-processing helper function aggregates and summarizes the FrEDI results to levels of aggregation specified by the user (passed to `aggLevels`). Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels="national"`), averaging across model types and models (`aggLevels="modelAverage"`), summing over all impact types (`aggLevels="impactType"`), and interpolate between impact year estimates (`aggLevels="impactYear"`). Users can specify all aggregation levels at once by specifying `aggLevels="all"` (default).
#' This post-processing helper function aggregates and summarizes temperature binning results to levels of aggregation specified by the user (passed to `aggLevels`). Users can specify a single aggregation level or multiple aggregation levels by passing a single character string or character vector to `aggLevels`. Options for aggregation include calculating national totals (`aggLevels="national"`), averaging across model types and models (`aggLevels="modelaverage"`), summing over all impact types (`aggLevels="impacttype"`), and interpolate between impact year estimates (`aggLevels="impactyear"`). Users can specify all aggregation levels at once by specifying `aggLevels="all"` (default) or no aggregation levels (`aggLevels="none"`).
#'
#' Before aggregating impacts for national totals and/or model averages, [FrEDI::aggregate_impacts()] will drop any pre-summarized results  (i.e., values for which `region="National Total"` and/or for which `model="average"`, respectively) that are already present in the data `and then reaggregate at those levels.
#'
#' For each of the `aggLevels`, [FrEDI::aggregate_impacts()] performs the following summarization (note that the `"variant"` column referred to below contains information about the adaptation or variant name or `“N/A”`, as applicable):
#' \tabular{ll}{
#' \strong{Aggregation Level} \tab \strong{Description} \cr
#' `national` \tab Annual values are summed across all regions present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`,  `"impactYear"`, `"model_type"`, `"model"`, and `"year"`) and summed across regions. Years which have missing column data for all regions return as `NA`. The rows of the dataframe of national values (with column `region="National Total"`) are then added as rows to the results. \cr
#' `modelaverage` \tab For temperature-driven sectors, annual results are averaged across all GCM models present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`, `"model_type"`, `"region"`, and `"year"` and averaged across models (SLR impacts are estimated as an interpolation between SLR scenarios). Averages exclude missing values. Years which have missing column data for all models return as `NA`. The rows of model averages (with column `model="Average"` are then added as rows to the results dataframe. \cr
#' `impactType` \tab Annual results are summed across all impact types by sector present in the data. I.e., data is grouped by columns `"sector"`, `"variant"`, `"impactType"`, `"impactYear"`,`"model_type"`, `"model"`, `"region"`, and `"year"` and summed across impact types. Mutates column `impactType="all"` for all values. Years which have missing column data for all impact types return as `NA`. If results are aggregated across impact types, information about physical impacts (columns `"physicalmeasure"` and `"physical_impacts"`) are dropped.\cr
#' `impactYear` \tab Annual results for sectors with only one impact year estimate (i.e., `impactYear == "N/A"`) are separated from those with multiple impact year estimates. For sectors with multiple impact years (i.e. 2010 and 2090 socioeconomic runs), annual results are interpolated between impact year estimates for applicable sectors  i.e., data is grouped by columns `"sector", "variant", "impactType, "model_type", "model", "region", "year"` and interpolated across years with the 2010 run assigned to year 2010 and the 2090 run assigned to year 2090. The interpolated values are bound back to the results for sectors with a single impact year estimate, and column `impactYear` set to `impactYear="Interpolation"` for all values. \cr
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
#' agg_tempExOut <- df_tempExOut %>% aggregate_impacts(columns=c("annual_impacts", "discounted_impacts"))
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'
#' @export
#' @md
#'
### This function aggregates outputs produced by temperature binning
aggregate_impacts <- function(
  data,             ### Dataframe of outputs from temperature binning
  aggLevels   = c("national", "modelaverage", "impactyear", "impacttype"),  ### Levels of aggregation
  columns     = c("annual_impacts"), ### Columns to aggregate
  groupByCols = c("sector", "variant", "impactYear", "impactType", "model_type", "model", "region"), ### Columns to group by
  mode = "all"
){
  ###### Defaults ######
  ### Not used currently; preserving it in messaging logicals for the future
  msgUser <- T
  ### Modes...specifically SLR interpolation only
  xMode   <- ifelse(is.null(mode), "all", mode) %>% tolower

  ###### Ungroup Data ######
  data    <- data %>% ungroup #; names(data) %>% print

  ###### SLR Info ######
  # assign("slr_cm", rDataList[["slr_cm"]])
  assign("co_models", rDataList[["co_models"]])
  co_slrs <- co_models %>% filter(modelType=="slr") %>%
    mutate_at(.vars=c("model_dot", "model_label"), as.character) %>%
    as.data.frame

  ###### Load Sector Info ######
  name_dfSectors   <- "co_sectors"
  sectorCols_old   <- c("sector_id", "sector_label", "modelType")
  sectorCols_new   <- c("sector_id", "sector", "model_type")

  assign(name_dfSectors, rDataList[[name_dfSectors]])

  co_sectors  <- co_sectors %>%
    select(all_of(sectorCols_old)) %>%
    (function(y){
      names(y) <- c(sectorCols_new)
      return(y)
    }) %>%
    mutate(model_type = model_type %>% toupper())


  ###### Load Variant Info ######
  ### Get input scenario info: co_inputScenarioInfo
  ### Load variant info table from sysdata.rda
  name_dfVariant    <- "co_variants"
  assign(name_dfVariant, rDataList[[name_dfVariant]])
  ### Format variants
  variantCols_old   <- c("sector_id", "variant_label")
  variantCols_new   <- c("sector_id", "variant")
  variantCols_other <- c("sectorprimary", "includeaggregate")
  co_variants  <- co_variants %>%
    select(c(variantCols_old, variantCols_other)) %>%
    (function(y){
      names(y) <- c(variantCols_new, variantCols_other)
      return(y)
    })

  ### Combine sector and variant info
  co_sector_variants <- co_sectors %>%
    left_join(co_variants, by = "sector_id") %>%
    select(-c("sector_id"))


  ###### Groups Columns  ######
  ### Filter to columns in data and mutate those columns to character
  default_groupByCols <- c("sector", "variant", "impactYear", "impactType", "model_type", "model", "region")
  ### Use default group by columns if none specified...otherwise, check which are present
  if(is.null(groupByCols)){
    groupByCols <- default_groupByCols
  }
  ### Check if columns for grouping are there
  names_data0   <- data %>% names
  is_groupByCol <- groupByCols %in% names_data0
  which_notPresentGroups <- (!is_groupByCol) %>% which
  ### Message user if some columns aren't present
  if(length(which_notPresentGroups) > 0){
    groupByCols <- groupByCols[which(is_groupByCol)]
    if(msgUser){
      message("\t", "Warning: groupByCols = c(", paste(groupByCols[which_notPresentGroups], collapse = ", "), ") not present...")
      message("\t\t", "Grouping by remaining columns...")
    }
  }

  ### Add sector primary and include aggregate
  # newGroupCols <- c("sectorprimary", "includeaggregate") %>%
  newGroupCols <- c("sectorprimary", "includeaggregate", "physicalmeasure") %>%
    (function(y){y[which(!(y %in% groupByCols))]}) %>%
    (function(y){y[which(y %in% names_data0)]})
  ### If length(newGroupCols)>0, add them
  if(length(newGroupCols)>0){
    groupByCols <- groupByCols %>% c(newGroupCols)
  }

  ### Remove extra names
  rm("is_groupByCol", "which_notPresentGroups", "newGroupCols")

  ###### Summary Columns  ######
  ### Columns to summarize
  # if(!is.null(columns)){summaryCols <- columns}
  default_summaryCols <- c("annual_impacts")
  if(is.null(columns)){
    summaryCols <- default_summaryCols
  } else{
    summaryCols <- columns
  }

  is_sumByCol <- summaryCols %in% names_data0
  which_notPresentSums <- (!is_sumByCol) %>% which
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
  newSumCols <- c("physical_impacts") %>%
    (function(y){y[which(!(y %in% summaryCols))]}) %>%
    (function(y){y[which(y %in% names_data0)]})
  ### If length(newGroupCols)>0, add them
  if(length(newSumCols)>0){
    summaryCols <- summaryCols %>% c(newSumCols)
  }
  ### Number of summary columns
  num_summaryCols     <- summaryCols %>% length
  data        <- data %>% mutate_at(.vars=c(all_of(groupByCols)), as.character)

  ###### Aggregation level  ######
  ### Types of summarization to do: default
  aggList0 <- c("national", "modelaverage", "impactyear", "impacttype", "all")
  if(!is.null(aggLevels)){
    aggLevels <- aggLevels %>% tolower()
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
  baseScenarioCols   <- c("year", "gdp_usd", "national_pop", "gdp_percap") %>% (function(y){y[which(y %in% names(data))]})
  regionalPopCols    <- c("year", "region", "reg_pop") %>% (function(y){y[which(y %in% names(data))]})
  nationalPopCols    <- c("year", "region", "national_pop") %>% (function(y){y[which(y %in% names(data))]})
  driverScenarioCols <- c("year", "model_type", "driverType", "driverUnit", "driverValue") %>% (function(y){y[which(y %in% names(data))]})


  ### List of standardized columns
  standardCols       <- c(baseScenarioCols, regionalPopCols, nationalPopCols) %>% unique
  # standardCols       <- c(standardCols) %>% unique
  # summaryCols        <- c(summaryCols) %>% unique
  ### Standardize columns
  standardCols <- c(groupByCols, standardCols, driverScenarioCols, summaryCols) %>% unique


  ### If "national" aggregation, filter out national totals
  if(nationalAgg){
    data <- data %>% filter(region!="National Total")
  }
  ### If modelAverage %in% aggLevels, filter out model averages
  if(modelAveAgg){
    data <- data %>% filter(!(model %in% c("Average", "Model Average")))
  }
  data <- data[,(names(data) %in% standardCols)]#; names(data) %>% print

  ###### Base Scenario Info  ######
  ### Some values are the same for all runs and regions...separate those values
  sectorsList   <- data$sector %>% unique
  sector0       <- sectorsList %>% first
  variant0      <- (data %>% filter(sector==sector0))$variant %>% unique %>% first
  region0       <- (data %>% filter(sector==sector0))$region %>% unique %>% first
  model0        <- (data %>% filter(sector==sector0))$model %>% unique %>% first
  impactType0   <- (data %>% filter(sector==sector0))$impactType %>% unique %>% first
  ### Base Scenario and regional population
  baseScenario  <- data %>% filter(sector == sector0, variant == variant0, region == region0, model == model0, impactType == impactType0)
  regionalPop   <- data %>% filter(sector == sector0, variant == variant0, model == model0, impactType == impactType0)

  ### Filter to impact types
  if(impactTypesAgg){
    impactType0  <- baseScenario$impactType %>% unique %>% first
    baseScenario <- baseScenario %>% filter(impactType == impactType0)
    regionalPop  <- regionalPop  %>% filter(impactType == impactType0)
  }
  ### Filter to impact years
  if(impactYearsAgg){
    impactYear0  <- baseScenario$impactYear %>% unique %>% first
    baseScenario <- baseScenario %>% filter(impactYear == impactYear0)
    regionalPop  <- regionalPop  %>% filter(impactYear == impactYear0)
  }
  ### Select columns
  ### Base Scenario, regional population, national population
  baseScenario <- baseScenario %>% select(all_of(baseScenarioCols))
  regionalPop  <- regionalPop  %>% select(all_of(regionalPopCols))
  ### Create national population scenario from the base scenario
  nationalPop  <- baseScenario %>%
    mutate(region = "National Total") %>%
    select(all_of(nationalPopCols)) %>%
    rename(reg_pop=national_pop)

  ###### Driver Scenario ######
  ### Get unique model types, sectors, variants, and models
  modelTypesList <- data$model_type %>% unique
  # data$model_type %>% unique %>% print
  sectors1       <- modelTypesList %>%
    lapply(function(model_type_i){
      (data %>% filter(model_type==model_type_i))$sector %>% unique %>% first
    }) %>% unlist %>% c()
  variants1   <- sectors1 %>%
    lapply(function(sector_i){
      (data %>% filter(sector==sector_i))$variant %>% unique %>% first
    }) %>% unlist%>% c()
  # impactTypes1   <- sectors1 %>%
  #   lapply(function(sector_i){
  #     (data %>% filter(sector==sector_i))$impactType %>% unique %>% first
  #   }) %>% unlist%>% c()
  models1     <- sectors1 %>%
    lapply(function(sector_i){
      (data %>% filter(sector==sector_i))$model %>% unique %>% first
    }) %>% unlist %>% c()

  ###### Driver scenario
  driverScenario <- 1:length(sectors1) %>%
    lapply(function(i){
      data %>% filter(sector == sectors1[i], variant == variants1[i], region == region0, model == models1[i])
    }) %>% (function(x){do.call(rbind, x)})

  ### Filter to impact types
  # if(impactTypesAgg){
  impactTypes1  <- sectors1 %>%
    lapply(function(sector_i){
      (driverScenario %>% filter(sector==sector_i))$impactType %>% unique %>% first
    }) %>% unlist
  driverScenario  <- 1:length(sectors1) %>%
    lapply(function(i){
      driverScenario %>% filter(sector==sectors1[i]) %>% filter(impactType == impactTypes1[i])
    }) %>% (function(x){do.call(rbind, x)})
  # }

  ### Filter to impact years
  # if(impactYearsAgg){
  impactYear1  <- sectors1 %>%
    lapply(function(sector_i){
      (driverScenario %>% filter(sector==sector_i))$impactYear %>% unique %>% first
    }) %>% unlist
  driverScenario  <- 1:length(sectors1) %>%
    lapply(function(i){
      driverScenario %>% filter(sector==sectors1[i]) %>% filter(impactYear == impactYear1[i])
    }) %>% (function(x){do.call(rbind, x)})
  # }


  ### Select columns
  driverScenario <- driverScenario %>% select(all_of(driverScenarioCols))

  ###### Aggregation  ######
  # if(requiresAgg){
  #   if(msgUser) message("Aggregating impacts...")
  #   }
  ###### Save a copy of the data
  scenarioCols <- c(baseScenarioCols, regionalPopCols, nationalPopCols, driverScenarioCols) %>% unique %>%
    (function(y){y[which(!(y %in% c(groupByCols, "year")))]}) #; scenarioCols %>% print
  ### Select appropriate columns
  df_aggImpacts <- data  %>% select(-c(all_of(scenarioCols)))

  ###### Impact Years ######
  if(impactYearsAgg){
    if(msgUser) message("\t", "Interpolating between impact year estimates...")
    # summaryCol1 <- summaryCols[1]

    ### Group by columns
    groupCols_impYears <- groupByCols[which(groupByCols != "impactYear" )]

    impactYears <- c(2010, 2090) %>% as.character
    impactYear2 <- impactYears[2]
    impactYear1 <- impactYears[1]

    ### Separate out observations without impact years
    df_naYears <- df_aggImpacts %>% filter(!(impactYear %in% impactYears)) %>% mutate(impactYear="Interpolation")

    ### New upper and lower column names
    new_2090SummaryCols <- paste(summaryCols, "2090", sep="_")
    new_2010SummaryCols <- paste(summaryCols, "2010", sep="_")


    df_impYears <- df_aggImpacts %>% filter(impactYear %in% impactYears)
    rm("df_aggImpacts")
    nrow_impYrs <- df_impYears %>% nrow


    if(nrow_impYrs > 0){

      ### Filter to other lower models and then bind with the zero values, drop model column
      df2090 <- df_impYears %>% filter(impactYear == impactYear2) %>%
        select(-c("impactYear")) %>%
        (function(y){
          y <- y %>% as.data.frame
          y[,new_2090SummaryCols] <- y[,summaryCols]
          return(y)
        })

      ### Drop summary columns from 2010
      df2010 <- df_impYears %>% filter(impactYear == impactYear1) %>%
        select(-c("impactYear")) %>%
        (function(y){
          y <- y %>% as.data.frame
          y[,new_2010SummaryCols] <- y[,summaryCols]
          return(y)
        }) %>%
        select(-c(all_of(summaryCols)))

      ### Join upper and lower data frames and calculate the numerator, denominator, and adjustment factor
      df_impYears <- df2090 %>%
        left_join(df2010, by=c(groupCols_impYears, "year")) %>%
        ### Add Impact year numerator and denominator
        mutate(numer_yr = year-as.numeric(impactYear1)) %>%
        mutate(denom_yr = as.numeric(impactYear2)-as.numeric(impactYear1)) %>%
        mutate(adj_yr    = numer_yr/denom_yr)

      rm("df2090", "df2010")

      ### Iterate over summary columns
      for(i in 1:num_summaryCols){
        col_i  <- summaryCols[i]

        ### Upper/lower
        col_i_2010 <- col_i %>% paste("2010", sep="_")
        col_i_2090 <- col_i %>% paste("2090", sep="_")

        ### Calculate numerator and denominator
        df_impYears <- df_impYears %>% as.data.frame

        df_impYears$new_factor <- df_impYears[,col_i_2090] - df_impYears[,col_i_2010]
        df_impYears$new_value  <- df_impYears[,col_i_2010]

        # df_slr_notEqual %>% names %>% print

        ### Update the new value
        oldCol_i <- col_i %>% c()
        newCol_i <- "new_value" %>% c()

        df_impYears <- df_impYears %>%
          mutate(new_value = new_value + new_factor * adj_yr) %>%
          select(-c(all_of(col_i), "new_factor")) %>%
          rename_with(~oldCol_i[which(newCol_i==.x)], .cols=newCol_i) %>%
          select(-c(all_of(col_i_2010), all_of(col_i_2090)))
      } ### End for(i in 1:num_summaryCols)

      df_impYears <- df_impYears  %>%
        mutate(impactYear="Interpolation") %>%
        select(-c("numer_yr", "denom_yr", "adj_yr"))
    }

    ### Add back into regional values
    df_aggImpacts <- df_impYears %>% rbind(df_naYears) %>% mutate(impactYear="Interpolation")

    rm("df_impYears", "df_naYears")
  }

  ###### Model Averages ######
  ### Average values across models
  if(modelAveAgg){
    modelAveMsg <- ifelse(xMode=="slrinterpolation", "Interpolating SLR impacts..." , "Calculating model averages...")
    if(msgUser) message("\t", modelAveMsg)
    ### Group by columns
    groupCols_modelAverage <- groupByCols[which(groupByCols != "model" )]

    for(model_type_i in modelTypesList){
      ###### GCM #######
      if(tolower(model_type_i)=="gcm"){

        ### Names of agg impacts
        names_aggImpacts   <- df_aggImpacts %>% names

        ### Calculate number of non missing values
        ### Group data, sum data, calculate averages, and drop NA column
        df_modelAves     <- df_aggImpacts %>%
          filter(model_type == model_type_i) %>%
          (function(w){
            w <- w %>% as.data.frame
            w$not_isNA <- (!is.na(w[,summaryCol1]))*1
            return(w)
          }) %>%
          group_by_at(c(all_of(groupCols_modelAverage), "year")) %>%
          summarize_at(.vars=c(all_of(summaryCols), "not_isNA"), sum, na.rm=T) %>%
          as.data.frame %>%
          mutate(not_isNA = not_isNA %>% na_if(0)) %>%
          (function(x){
            x[,summaryCols] <- x[,summaryCols]/x$not_isNA
            return(x)
          }) %>%
          select(-not_isNA) %>%
          mutate(model = "Average") %>%
          ungroup %>%
          as.data.frame

        ### Add observations back in
        df_aggImpacts <- df_aggImpacts %>% rbind(df_modelAves)
      }
      ###### SLR #######
      else{
        ### Filter to SLR sectors
        df_slr <- df_aggImpacts %>%
          mutate_at(.vars=c("model"), as.character) %>%
          filter(model_type == model_type_i)

        ### Separate into interpolated & not interpolated
        df_slr_interp   <- df_slr %>%
          filter(model=="Interpolation") %>%
          as.data.frame

        df_slr_notInterp <- df_slr %>%
          filter(model!="Interpolation") %>%
          as.data.frame

        nrow_slr        <- df_slr %>% nrow
        nrow_interp     <- df_slr_interp %>% nrow
        nrow_notInterp  <- df_slr_notInterp %>% nrow
        # c(nrow_slr, nrow_interp, nrow_notInterp) %>% print

        ### Filter driver scenario to slr
        slrScenario    <- driverScenario %>% filter(model_type=="SLR") %>% select(-c("model_type"))

        ### Check if there is more than one model level
        if(nrow_interp== 0 & nrow_notInterp > 0){
          df_slr         <- df_slr_notInterp
          slrSummaryCols <- summaryCols
          num_slrSumCols <- slrSummaryCols %>% length

          ### Get information on the driver scenario
          ### Names from slr_Interp_byYear c("driverValue", "lower_model", "upper_model", "lower_slr", "upper_slr")
          df_slrInfo         <- slrScenario %>% slr_Interp_byYear

          names_df_slrInfo   <- df_slrInfo %>% names
          other_slrGroupCols <- names_df_slrInfo[which(names_df_slrInfo!="year")]
          names_aggImpacts   <- df_aggImpacts %>% names

          df_slr <- df_slr %>% left_join(df_slrInfo, by=c("year"))

          ### Data where upper model = lower model
          equal_slr_models <- (df_slr$lower_model == df_slr$upper_model)
          which_equal      <-  equal_slr_models %>% which
          which_notEqual   <- (!equal_slr_models) %>% which


          ### Initialize dataframes
          df_slr_equal     <- df_slr[which_equal,]
          df_slr_notEqual  <- df_slr[which_notEqual,]

          ### Filter out observations that are equal
          if(length(which_equal) > 0){
            ### Filter observations that are zeros only and make the summary column values zero
            slr_equal0    <- df_slr_equal %>% filter(lower_model=="0 cm")

            if(nrow(slr_equal0) > 0){
              c_slr_equal0 <- slr_equal0$model %>% as.character %>% unique

              slr_equal0    <- slr_equal0 %>%
                ### Filter to the lower model results
                filter(model==c_slr_equal0[1]) %>%
                ### Make values zero
                (function(y){
                  y[,slrSummaryCols] <- 0; return(y)
                })
            }

            ### For those that are equal but not equal to zero, filter to the appropriate model
            slr_equal_not0 <- df_slr_equal %>%
              filter(lower_model!="0 cm") %>%
              filter(model==lower_model)

            ### Join values back together and add the model info
            df_slr_equal <- slr_equal0 %>%
              rbind(slr_equal_not0) %>%
              mutate(model="Interpolation")

            if("physical_impacts" %in% names_aggImpacts){
              df_slr_equal <- df_slr_equal %>% mutate(physical_impacts = NA)
            }
            df_slr_equal <- df_slr_equal %>% select(c(all_of(names_aggImpacts)))

            rm("slr_equal0")
          } ### End if length(which_equal) > 0

          ### Observations that are greater than zero
          if(length(which_notEqual) > 0){
            df_slr_notEqual <- df_slr[which_notEqual,] %>%
              mutate_at(.vars=c("model", "lower_model", "upper_model"), as.character)

            ### New upper and lower column names
            new_lowerSummaryCols <- paste(slrSummaryCols, "lower", sep="_")
            new_upperSummaryCols <- paste(slrSummaryCols, "upper", sep="_")

            ### Lower and upper dataframes
            ### Filter observations with a lower value of "0 cm" and convert values to zero
            slr_lower0    <- df_slr_notEqual %>% filter(lower_model=="0 cm")

            if(nrow(slr_lower0)>0){
              slr_lower0 <- slr_lower0 %>%
                (function(y){
                  y[,new_lowerSummaryCols] <- 0
                  return(y)
                }) %>%
                ### Filter to the 30cm model results
                filter(model=="30 cm")
            }

            ### Filter to other lower models and then bind with the zero values, drop model column
            slr_lower <- df_slr_notEqual %>% filter(model==lower_model & lower_model!= "0 cm") %>%
              (function(y){
                y <- y %>% as.data.frame
                y[,new_lowerSummaryCols] <- y[,slrSummaryCols]
                return(y)
              })

            slr_lower <- slr_lower %>% rbind(slr_lower0) %>% select(-c("model"))


            ### Filter to upper model values, drop model column
            slr_upper <- df_slr_notEqual %>% filter(model==upper_model) %>% select(-c("model"))
            slr_upper <- slr_upper %>% rename_with(~new_upperSummaryCols[which(slrSummaryCols==.x)], .cols=slrSummaryCols)


            ### Join upper and lower data frames and calculate the numerator, denominator, and adjustment factor
            df_slr_notEqual <- slr_lower %>%
              left_join(slr_upper, by=c(groupCols_modelAverage, "year", other_slrGroupCols)) %>%
              mutate(denom_slr  = upper_slr - lower_slr) %>%
              mutate(numer_slr  = upper_slr - driverValue) %>%
              mutate(adj_slr    = numer_slr/denom_slr) %>%
              as.data.frame

            rm("slr_lower0", "slr_lower", "slr_upper", "new_lowerSummaryCols", "new_upperSummaryCols")

            ###### Iterate over summary columns ######
            ### Iterate over summary columns
            for(i in 1:num_slrSumCols){
              col_i      <- slrSummaryCols[i]

              ### Upper/lower
              lowerCol_i <- col_i %>% paste("lower", sep="_")
              upperCol_i <- col_i %>% paste("upper", sep="_")

              ### Calculate numerator and denominator
              df_slr_notEqual <- df_slr_notEqual %>% as.data.frame

              df_slr_notEqual$new_factor <- df_slr_notEqual[,upperCol_i] - df_slr_notEqual[,lowerCol_i]
              df_slr_notEqual$new_value  <- df_slr_notEqual[,lowerCol_i]

              ### Update the new value
              oldCol_i <- col_i %>% c()
              newCol_i <- "new_value" %>% c()
              df_slr_notEqual <- df_slr_notEqual %>%
                mutate(new_value = new_value + new_factor * (1 - adj_slr)) %>%
                select(-c(all_of(col_i))) %>%
                rename_with(~oldCol_i[which(newCol_i==.x)], .cols=newCol_i)
            } ### End for(i in 1:num_slrSumCols)

            ### When finished, drop columns and mutate model column
            df_slr_notEqual <- df_slr_notEqual %>% mutate(model="Interpolation")

            df_slr_notEqual <- df_slr_notEqual %>% select(c(all_of(names_aggImpacts))) %>%
              mutate(model_type = model_type_i)

          } ### End if(nrow_interp== 0 & nrow_notInterp > 0)

          ### Bind slr averages together
          df_slr <- df_slr_equal %>% rbind(df_slr_notEqual)

          ### Add other observations back in
          df_aggImpacts <- df_aggImpacts %>%
            filter(tolower(model_type)!="slr") %>%
            filter(model!="Interpolation") %>%
            rbind(df_slr)

          rm("df_slr", "df_slr_equal", "df_slr_notEqual")
        }
        ### Else (nrow_interp== 0 & nrow_notInterp > 0)
        else{
          df_aggImpacts_gcm <- df_aggImpacts %>% filter(tolower(model_type) != tolower(model_type_i))
          df_aggImpacts_slr <- df_aggImpacts %>%
            filter(tolower(model_type) == tolower(model_type_i)) %>%
            filter(model=="Interpolation")

          df_aggImpacts     <- df_aggImpacts_gcm %>% rbind(df_aggImpacts_slr)
          rm("df_slr", "df_aggImpacts_gcm", "df_aggImpacts_slr")

        } ### ### End else(nrow_interp== 0 & nrow_notInterp > 0)
      } ### End if if(model_type_i=="gcm")
    } ### End if model_type_i %in% c_models
  } ### End if "model" %in% aggLevels

  ###### National Totals ######
  if(nationalAgg){
    if(msgUser) message("\t", "Calculating national totals...")
    ### Group by columns
    groupCols_national <- groupByCols[which(groupByCols != "region" )]
    ### Filter to national values and not national values
    df_national     <- df_aggImpacts %>%
      ### Calculate number of non missing values
      ### Group data, sum data, calculate averages, and drop NA column
      (function(w){
        w <- w %>% as.data.frame
        w$not_isNA <- (!is.na(w[,summaryCol1]))*1
        return(w)
      }) %>%
      group_by_at(c(all_of(groupCols_national), "year")) %>%
      summarize_at(vars(all_of(summaryCols), not_isNA), sum, na.rm=T) %>%
      mutate(not_isNA = (not_isNA>=1)*1) %>%
      mutate(not_isNA = not_isNA %>% na_if(0)) %>%
      (function(x){
        x[, summaryCols] <- x[, summaryCols]*x$not_isNA
        return(x)
      }) %>%
      select(-not_isNA) %>%
      mutate(region="National Total") %>%
      ungroup
    ### Add back into regional values and bind national population to impact types
    df_aggImpacts <- df_aggImpacts %>% rbind(df_national);
    regionalPop   <- regionalPop %>% rbind(nationalPop)
    ### Remove values
    rm("df_national")
  } ### End if national

  ###### Impact Type ######
  ### Summarize by Impact Type
  if(impactTypesAgg){
    if(msgUser) message("\t", "Summing across impact types...")

    ### Group by columns
    impactType_dropCols     <- c("physicalmeasure", "physical_impacts")
    df_aggImpacts           <- df_aggImpacts %>%
      (function(y){
        names_y <- y %>% names
        names_y <- names_y[which(!(names_y %in% impactType_dropCols))]
        y       <- y %>% select(all_of(names_y))
        return(y)
      })

    names_aggImpacts      <- df_aggImpacts %>% names

    groupByCols   <- groupByCols[which(!(groupByCols %in% c(impactType_dropCols)))]
    summaryCols   <- summaryCols[which(!(summaryCols %in% c(impactType_dropCols)))]
    summaryCol1   <- summaryCols[1]
    standardCols  <- standardCols[which(!(standardCols %in% c(impactType_dropCols)))]

    groupCols_impactTypes <- groupByCols[which(!(groupByCols %in% c("impactType")))]
    n_groupCols_impTypes  <- groupCols_impactTypes %>% length

    ### Summarize at impact types: Count number of impact types
    df_nImpTypes       <- df_aggImpacts %>%
      group_by_at(.vars=c(all_of(groupCols_impactTypes), "impactType", "year")) %>%
      summarize(nTypes = n(), .groups="keep") %>%
      group_by_at(.vars=c(all_of(groupCols_impactTypes), "impactType")) %>%
      summarize(nTypes = n(), .groups="keep") %>%
      group_by_at(.vars=c(all_of(groupCols_impactTypes))) %>%
      summarize(nTypes = n(), .groups="keep")


    ### Add info back in
    df_aggImpacts <- df_aggImpacts %>% left_join(df_nImpTypes, by=groupCols_impactTypes)

    df_oneImpact  <- df_aggImpacts %>%
      filter(nTypes==1) %>% select(-c("nTypes")) %>%
      mutate(impactType="all")

    df_nImpacts   <- df_aggImpacts %>%
      filter(nTypes!=1) %>% select(-c("nTypes")) %>%
      (function(w){
        w <- w %>% as.data.frame
        w$not_isNA <- (!is.na(w[,summaryCol1]))*1
        return(w)
      }) %>%
      group_by_at(.vars=c(all_of(groupCols_impactTypes), "year")) %>%
      summarize_at(.vars=c(all_of(summaryCols), "not_isNA"), sum, na.rm=T) %>%
      as.data.frame %>%
      mutate(
        not_isNA = (not_isNA > 0)*1,
        not_isNA = not_isNA %>% na_if(0)
      ) %>%
      (function(x){
        x[, summaryCols] <- x[, summaryCols]*x$not_isNA
        return(x)
      }) %>%
      select(-not_isNA) %>%
      mutate(impactType="all") %>%
      as.data.frame

    ### Add to impacts
    df_aggImpacts <- df_oneImpact %>% rbind(df_nImpacts) %>% mutate(impactType="all")
    rm("df_oneImpact", "df_nImpacts")
  } ### End if impactType in aggLevels

  ###### Join Base Scenario Info with Aggregated Data ######
  ### Join national info with population
  df_base   <- baseScenario %>% left_join(regionalPop, by=c("year"))

  df_agg_names      <- df_aggImpacts %>% names
  sectorVariant_names <- co_sector_variants %>% names
  sectorVariant_join  <- c("model_type", "sector", "variant")
  sectorVariant_drop  <- sectorVariant_names[which((sectorVariant_names %in% df_agg_names) & !(sectorVariant_names %in% sectorVariant_join))]

  df_return <- df_aggImpacts %>%
    left_join(co_sector_variants %>% select(-c(all_of(sectorVariant_drop))), by = sectorVariant_join) %>%
    left_join(driverScenario, by = c("year", "model_type")) %>%
    left_join(df_base      , by = c("year", "region"))

  ###### Reformat sectorprimary and includeaggregate, which were converted to character
  c_aggColumns <- c("sectorprimary", "includeaggregate") %>% (function(y){y[which(y %in% names(df_return))]})
  if(length(c_aggColumns)>0){
    df_return     <- df_return %>% mutate_at(.vars=c(all_of(c_aggColumns)), as.numeric)
  }

  ###### Order Columns ######
  ### Order the data frame and ungroup
  ### Column indices of columns used in ordering
  return_names  <- df_return %>% names
  orderColNames <- c(groupByCols, "year") %>% (function(y){y[which(y %in% return_names)]}) #; "got here10" %>% print
  df_return     <- df_return %>% arrange_at(.vars=c(all_of(orderColNames)))

  ###### Return ######
  ### Grouping columns, driver columns, scenario columns
  ### Make sure data is ungrouped and a dataframe object
  df_return <- df_return %>%
    select( all_of(standardCols)) %>%
    ungroup %>% as.data.frame

  ### Return object
  # if(msgUser) message("\n", "Finished...")
  return(df_return)
}

