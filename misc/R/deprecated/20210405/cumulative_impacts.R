###### cumulative_impacts ######
### Created 2021.02.08.
#' Calculate cumulative values of impacts produced from temperature- and SLR- binning
#'
#' @description
#' #' Calculate cumulative values of impacts produced from temperature- and SLR- binning for specified group levels (e.g., sector, adaptation, impact type, impact estimate year, model, and region combinations)
#'
#' @param data        Dataframe of results from temperature binning (e.g., outputs from `tempBin()`, processed by `present_values()`.
#' @param column      Name of column for which to calculate cumulative impacts (defaults to `column="discounted_impacts"`.
#' @param groupLevels Names of columns containing levels at which to group the results for calculating cumulative impacts. Can be a single column name or a character vector of multiple column names (e.g., `groupLevels = c("sector", "adaptation", "impactType", "impactYear", "model_type", "model", "region")`). .
#'
#' @details
#' This post-processing helper function calculates cumulative impacts at specified group levels. Combine with `present_values()` to calculate cumulative discounted impacts over time.
#'
#' @return
#' Returns a dataframe with the original dimensions, plus an additional column (`cumulative_impacts`), with the cumulative values of impacts from the specified column.
#'
#' @examples
#' ### Load summary of temperature binning results for the default scenarios
#' load(defaultSummary)
#' ### Calculate present values for the summarized results
#' pvSummary <- defaultSummary %>% present_values(column="annual_impacts")
#' ### Calculate cumulative value of present values
#' sumPV <- pvSummary %>% cumulative_impacts(column="discounted_impacts")
#'
#'
#'
### This function calculates cumulative impacts of present values
cumulative_impacts <- function(
  data,
  column = NULL, ###which column to sum...default = discounted_impacts
  groupLevels = NULL ### Where to group
){
  ###### Defaults ######
  ### Data names
  # names_data <- data %>% names
  ### Config files
  for(i in 1:length(tempBin_config)){
    assign(names(tempBin_config)[i], tempBin_config[[i]])
  } ### End iterate over i

  ### Data names
  # names_data <- data %>% names
  if(is.null(groupLevels)){
    groupLevels <- groupLevels0
  }
  ### Subset to those in the data
  groupLevels  <- groupLevels[which(groupLevels %in% names(data))]
  numGroupCols <- groupLevels %>% length
  # groupLevels %>% print

  ###### Create groups ######
  ### Create groups and a group key
  for(i in 1:numGroupCols){
    group_i <- groupLevels[i]
    if(i == 1){
      data$group_id <- data[,group_i]
    } else{
      data$group_id <- paste(data$group_id, data[,group_i], sep="_")
    }
  }
  ### Group by the groups
  data <- data %>% group_by(group_id) %>% mutate(group_key = group_id)

  ### Get a list of groups
  df_sums <- data %>%
    group_map(function(.x, .y, .keep=T){
      ### Data
      df_i    <- .x #%>% as.data.frame

      ### Get sums
      sums_i  <- cumsum(df_i[, column])

      ### Add results back
      df_i    <- df_i %>% mutate(cumulative_impacts = sums_i[, 1])
      return(df_i)
      ### End group map function
    }) %>%
    ### Bind all values
    (function(y){
      do.call(rbind, y)
    }) %>%
    ungroup %>%
    select(-group_key) %>%
    as.data.frame

  ###### Return ######
  return(df_sums)
}



