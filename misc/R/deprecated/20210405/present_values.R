###### present_values ######
### Created 2021.02.08.
#' Calculate present values of temperature binning results
#'
#' @description
#' This post-processing helper function calculates present values for a specified column (`column`) in a dataframe of temperature binning results (`data`), using a user specified base year (`baseYear`) and annual discount rate (`rate`).
#'
#' @param data Dataframe of results from temperature binning (e.g., outputs from `tempBin()`, `get_reports()`, or `aggregate_impacts()`.
#' @param baseYear Base year used in calculating present value of annual impacts (defaults to `baseYear=2010`).
#' @param rate Average annual discount rate used in calculating present value of annual impacts (defaults to `rate=0.03`).
#' @param column Column name in data for which to calculate present values (i.e., discounted impacts). Defaults to `column="annual_impacts"`.
#'
#' @return
#' Returns a dataframe with the original dimensions, plus an additional column (`discounted_impacts`) with the present values of impacts from the specified column.
#'
#' @examples
#' ### Load summary of temperature binning results for the default scenarios
#' load(defaultSummary)
#' ### Calculate present values for the summarized results
#' pvSummary <- defaultSummary %>% present_values()
#'
#' @export
#'
#'
#'
#'
#'
### This function calculates present values
present_values <- function(
  data,
  baseYear  = NULL, ### Default = 2010
  rate      = NULL, ### Ratio, defaults to 0.03
  column    = NULL ### Which column to use
){
  ###### Defaults ######
  ### Config files
  for(i in 1:length(tempBin_config)){
    assign(names(tempBin_config)[i], tempBin_config[[i]])
  }
  ### Default base year and rate defined in config
  baseYear <- ifelse(is.null(baseYear), baseYear0, baseYear)
  rate     <- ifelse(is.null(rate), rate0, rate)

  ###### Format Data ######
  data <- data %>% as.data.frame

  ###### Present Value ######
  ### Calculate present value and cumulative value
  df_pvs <- data %>%
    mutate(
      discountRate   = rate,
      discountYear   = baseYear,
      discountFactor = 1 / (1 + discountRate)^(year - discountYear),
      valueColumn    = data[,column]
    ) %>%
    mutate(
      discounted_impacts   = valueColumn * discountFactor
    ) %>%
    ### Rename some columns
    rename(baseYear = discountYear) %>%
    (function(x){
      names_x <- names(x)
      which_x <- which(names_x == column)
      names(x)[which_x] <- column
      return(x)
    }) %>%
    ### Drop other columns
    select(-c("discountFactor", "valueColumn"))

  ###### Return ######
  return(df_pvs)
}


