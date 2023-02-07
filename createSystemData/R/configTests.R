#' test_loadData
#'
#' @param data_list A list of named tables to be examined
#' @param outPath A filepath to where test files are saved
#' @param save  A TRUE/FALSE option to save files or not
#' @param return A TRUE/FALSE option to return the test_table
#'
#' @return Returns a table object if "return" option is TRUE
#' @export
#'
#' @examples
dataInfo_test <- function(
    dataList = list(),
    outPath  = ".",
    csvName  = "dataInfo_test",
    save     = TRUE, ### Whether to save results to file
    return   = TRUE  ### Whether to return results
) {
  ### List of names
  listNames <- dataList %>% names
  
  ###### Initial Table Info ######
  ### Get table of info
  df_info   <- tibble(table = listNames)
  ### Count number of columns in each table
  df_info   <- df_info %>% mutate(num_cols    = listNames %>% map(~ (dataList[[.]] %>% ncol)) %>% unlist)
  ### Count number of rows in each table
  df_info   <- df_info %>% mutate(num_rows    = listNames %>% map(~ (dataList[[.]] %>% nrow)) %>% unlist)
  ### Count number of distinct rows in each table
  df_info   <- df_info %>% mutate(unique_rows = listNames %>% map(~ (dataList[[.]] %>% distinct %>% nrow)) %>% unlist)
  ### Count number of missing values 
  df_info   <- df_info %>% mutate(cols_wAllNA = listNames %>% map(~ (dataList[[.]] %>% has_nonNA_values)) %>% unlist)
  
  ###### Check Tests ######
  ### Check number of columns with some non-missing values is equal to the number of columns 
  df_info   <- df_info %>% mutate(na_flag  =   1 * (cols_wAllNA > 0))
  #### Check if each table has duplicate values: Number of rows should equal number of unique rows
  ### List of tables to make exceptions for
  except0   <- c("data_scaledImpacts")
  df_info   <- df_info %>% mutate(has_dups = case_when((row_count == unique_rows) ~ F, (Table.Name %in% except0) ~ F))
  ### Check whether all tests are passed
  df_info   <- df_info %>% mutate(passed   = case_when((has_dups == T | na_flag == T) ~ F, (has_dups == F & na_flag == F) ~ T))
  ### Mutate logicals to numeric
  mutate0   <- c("has_dups", "passed")
  df_info   <- df_info %>% mutate_at(.vars=c(all_of(mutate0)), as.numeric)
  ### Remove intermediates
  rm("except0", "mutate0")
  
  ### Print Out tables if there are any values that don't pass
  df_flags  <- df_info %>% filter(passed==0)
  numFlags  <- df_flags %>% nrow
  hasFlags  <- hasFlags > 0
  
  ### Message user
  msg_flags <- ifelse(hasFlags, "Some tables don't pass", "All tables pass") %>% paste0("...")
  msg_flags %>% message
  rm("msg_flags")
  
  ### Print flagged table results
  if_flags  <- hasFlags %>% if_else(
    true  = {df_flags %>% glimpse},
    false = {}
  )
  
  ### Save Option Outputs ####
  ifSave   <- save %>% if_else(
    true = {
      "Saving data checks" %>% paste0("...") %>% message
      outDir    <- outPath %>% file.path("data_tests")
      outExt    <- "." %>% paste0("csv")
      # csvName   <- "loadData_tests" 
      csvName   <- csvName %>% paste0(outExt)
      outFile   <- outDir %>% file.path(csvName)
      rm("outExt", "csvName")
      ### Check if outDir exists and, if not, create one
      odExists  <- outDir %>% dir.exists()
      if(!odExists){outDir %>% dir.create(showWarnings = F)}
      rm("odExists")
      ## Save the test results
      df_info  %>% write_csv(outFile)
    },
    false = {}
  ) ### End if_else
  
  ## Return options ####
  ifReturn <- return %>% if_else(
    true  = return(df_info),
    false = return(NULL)
  )
}
