###### fun_limitsByGroup ######
### This function summarizes data for a particular group
### It returns a dataframe of results with the groups, a column called "summary_type", and the summarized value "summary_value"
fun_limitsByGroup <- function(
  data,
  groupByCols = c("sector"),
  sumByCol = "annual_impacts",
  type = c("min", "max"),
  silent = F
  
){
  
  ###### Defaults ######
  if(is.null(groupByCols)){
    groupByCols <- c("sector")
  }
  ### Whether to message
  if(is.null(silent)){silent <- F}
  print_msg <- !silent
  if(print_msg) message("Running fun_limitsByGroup():")
  ###### Check for Summary Column ######
  if(is.null(sumByCol)){
    sumByCol <- "annual_impacts"
  } else{
    if(length(sumByCol) > 1){
      if(print_msg) message("\t", "More than one summary column provided. Summarizing the first column only...")
      sumByCol <- sumByCol[1]
    }
  }
  ### Message the user
  if(print_msg) message("\n\t", "Summarizing values for ", sumByCol, "...")
  ### Check if the column is present
  has_sumByCol <- sumByCol %in% names(data)
  if(!has_sumByCol){
    if(print_msg) message("\n\t", "Column ", sumByCol, " not present in data...", "\n", "Exiting...")
    return()
  }
  ###### Check for Group By Columns ######
  data_names      <- data %>% names
  num_groupCols   <- groupByCols %>% length
  has_groupCols   <- (groupByCols %in% data_names)
  if(length(which(has_groupCols))!=num_groupCols){
    if(length(which(!has_groupCols)) == num_groupCols){
      if(print_msg) message("\t", "No grouping columns present in data.", "\n", "Exiting...")
      return()
    } else{
      groupCols_notInData <- groupByCols[which(!has_groupCols)]
      groupByCols <- groupByCols[which(has_groupCols)]
      if(print_msg) message("\t", "groupByCols ", paste(groupCols_notInData, collapse = ", "), " not present in data...")
    }
  }
  
  ### Set Type and Message the user
  if(is.null(type)){type <- c("min", "max")}
  if(print_msg) message("\t", "Getting ", paste(type, collapse = ", ")," values by ", paste(groupByCols, collapse = ", "), " combination...")
  if(print_msg) message("\t", "Grouping by columns ", paste(groupByCols, collapse = ", "), "...")
  
  ###### Get Maximum Values ######
  lim_bySector <- data %>% as.data.frame %>%
    ### Get the maximum value for sector
    group_by_at(all_of(groupByCols)) %>%
    summarise_at(.vars = all_of(sumByCol), .funs = all_of(type), na.rm=T) %>%
    gather(key = "summary_type", value = "summary_value", -c(all_of(groupByCols)))
  
  ### Return value
  if(print_msg) message("Finished.")
  return(lim_bySector)
}