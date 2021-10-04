#' Create and save reports of results from `tempBin()`.
#'
#' @description
#' This function creates and saves reports of temperature binning outputs produced by `tempBin()`. `get_reports()` aggregates (i.e., sums) results across impact types, reduces the size of the output dataframe, and calculates average annual and cumulative discounted impacts. Returns a list of reports, with an option to save results to a specified directory.
#'
#' @param data Dataframe of outputs produced by `tempBin()`. Do not change column names of `tempBin()` output before running `get_reports()`.
#' @param baseYear Base year to use for calculating present values of outputs (defaults to `baseYear=2010`).
#' @param rate Annual discount rate to use for calculating present values of outputs. Defaults to a 3% annual discount rate (i.e., `rate=0.03`).
#' @param directory A character string indicating the location of a directory in which to save the report objects. No default (i.e., `directory=NULL`).
#' @param save TRUE/FALSE value indicating whether to save results. If a directory value is supplied (i.e., `!is.null(directory)`), defaults to `save=TRUE`. Otherwise, default is `save=FALSE`.
#'
#' @details
#' This function creates and saves reports of temperature binning outputs produced by `tempBin()`. The input passed to `data` must have the same column names as those output from `tempBin()`. `get_reports()` is a wrapper for several helper functions, including `aggregate_impacts()`, `present_values()`, and `cumulative_impacts()`.
#'
#' `get_reports()` processes the outputs of `tempBin()` by:
#'
#' \itemize{
#'      \item Calculating average annual and cumulative discounted impacts from annual impacts using the functions `present_values()`, and `cumulative_impacts()`, respectively (see \code{\link{present_values}} and \code{\link{cumulative_impacts}}). The resulting dataframe is returned in the list element `"annual"`.
#'      \item Summarizing the large dataframe of impacts by 1) aggregating (i.e., summing) annual impacts across impact types using `aggregate_impacts()` and 2) reducing the size of the output dataframe by filtering results to 5-year increments between 2010 and 2090 (see \code{\link{aggregate_impacts}}). The resulting dataframe is passed to the results list element `"summary"`.
#' }
#'
#' `get_reports()` returns a list of dataframes, with an option to save results to a specified directory. If `save="TRUE"` and `direction` is not `NULL`, the results list is saved to the specified directory as an RDATA object (`"all_outputs.RData"`), and a CSV file of the summarized results (`"annual_outputs_summary.csv"`).
#'
#' @return
#' `get_reports()` returns a list of reports:
#'
#' \describe{
#'      \item{annual}{Dataframe of temperature-binned results, with additional columns for annual discounted impacts and cumulative discounted impacts.}
#'      \item{summary}{Dataframe of temperature-binned results, aggregated over all impact types per sector and filtered to 5-year increments.}
#' }
#' @examples
#' ### Load temperature-binning results for default scenarios
#' load(defaultResults)
#' ### Create results list
#' resultsSummary <- defaultResults %>% get_reports(get_reports(baseYear=2015, rate=0.03, save=FALSE))
#' ### Separate results summary from the results list
#' defaultSummary <- resultsSummary$summary
#'
#'
#'
#'
#'
### Created 2021.02.19. Last update: 2021.02.26.
### Updated order of operations for creating reports.
### This function creates a report as a list of objects from the temperature binning output.
get_reports <- function(
  data      = NULL,
  baseYear  = NULL, ### Default = 2010
  rate      = NULL, ### Default = 0.03
  directory = NULL, ###
  save      = NULL
){
  ###### Load Config File ######
  ### Assign data objects to objects in this namespace
  for(i in 1:length(tempBin_config)){
    assign(names(tempBin_config)[i], tempBin_config[[i]])
  } ### End iterate over i

  ###### Defaults ######
  ### Default for save is false unless
  if(is.null(save)){
    if(is.null(directory)){
      save <- F
    } else if(!is.null(directory)){
      save <- T
    }
  }

  ###### Prepare directory ######
  if(!is.null(directory)){
    ### Check if the directory exists
    dir_exists <- directory %>% dir.exists
    ### If the directory doesn't exist, try to create the directory and image directory and then check again to see if it exists
    if(!dir_exists){
      try_dir    <- try(directory %>% dir.create, silent=T)
      dir_exists <- directory %>% dir.exists
      if(!dir_exists){
        message("Could not create directory.")
      }
    }
  } else{
    dir_exists <- F
  }

  ###### Discounting Constants ######
  ### Defaults for discounting (move to config file)
  if(is.null(baseYear)){baseYear <- baseYear0} ### 2015
  if(is.null(rate    )){rate     <- rate0} ### 0.03

  ###### Sectors ######
  sectorsList <- (data %>% filter(!is.na(sector)))$sector %>% unique
  numSectors  <- sectorsList %>% length

  ###### Calculate Present Values ######
  ### Calculate discounted impacts (i.e., present values)
  data  <- data %>%
    present_values(
      baseYear = baseYear,
      rate     = rate,
      column   = "annual_impacts"
    )
  ### Cumulative impacts
  # data_pvSum <- data_pv %>%
  data <- data %>%
    cumulative_impacts(
      column="discounted_impacts"
    )

  ###### Create Results List ######
  all_outputs <- list(annual = data)

  # data %>% names %>% print
  ###### Subset to 5-year intervals ######
  data_agg    <- data %>%
    aggregate_impacts(
      aggLevels = c("impactType", "model.average", "national"),
      columns   = c("annual_impacts", "discounted_impacts", "cumulative_impacts")
      ) %>%
    filter(year %in% list_years_by5)
  ### Add aggregated outputs to outputs list
  all_outputs[["summary"]] <- data_agg

  ###### Save results ######
  ### Start the conditional again
  if(save){
    if(dir_exists){
      message("Saving results...")

      ### File paths
      fpath_data <- directory %>% paste("all_outputs.RData", sep="/")
      fpath_agg  <- directory %>% paste("annual_outputs_summary.csv", sep="/")

      ### Save data list
      all_outputs %>% save(file=fpath_data)

      ### Save aggregated data
      data_agg %>% write.csv(file=fpath_agg, row.names=F)

    } else{ ### End if directory exists
      message("Cannot open directory for results...")
    }
  } ### End if save

  ### Return object
  return(all_outputs)
}
