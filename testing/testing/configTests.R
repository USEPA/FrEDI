require(tidyverse)
###### Dimension Tests ######
#' test_loadData
#'
#' @param data_list A list of named tables to be examined
#' @param outPath A filepath to where test files are saved
#' @param save  A TRUE/FALSE option to save files or not
#' @param return A TRUE/FALSE option to return the test_table
#'
#' @return Returns a table object if "return" option is TRUE

dataInfo_test <- function(
    dataList = list(),
    outPath  = ".",
    csvName  = "dataInfo_test",
    save     = TRUE, ### Whether to save results to file
    return   = TRUE  ### Whether to return results
) {
  #dataList <- list_reshapeData
  ###### List Info ######
  ### List names
  ### Length of list and list names
  listNames <- dataList  |> names()
  lenList   <- dataList  |> length()
  lenName   <- listNames |> length()
  # c(lenList, lenName, lenList==lenName) |> print()

  ###### List Object Types ######
  ### Get info on object types and add names
  ### Simplify list of types
  # cTypes    <- c("data.frame", "list", "character", "numeric")
  listTypes <- listNames |> map(~ (dataList[[.]] |> class()))
  ### Add names back to list
  listTypes <- listTypes |> set_names(listNames)
  # listTypes[1] |> print
  ### Simplify types
  listTypes <- listNames |> map(~ case_when(
    ("data.frame" %in% listTypes[[.]]) ~ "data.frame",
    ("list"       %in% listTypes[[.]]) ~ "list",
    ("character"  %in% listTypes[[.]]) ~ "character",
    ("numeric"    %in% listTypes[[.]]) ~ "numeric",
    TRUE ~ "N/A"
  ))
  ### Add names back to list
  listTypes <- listTypes |> (function(x){names(x) <- listNames; return(x)})()
  # c(length(listTypes), names(listTypes) |> length) |> print()

  ###### Initial Table Info ######
  ### Initialize table of info...make methods specific to class
  ### Get class of table object
  df_info   <- tibble(table = listNames)
  df_info   <- df_info |> mutate(itemClass = listTypes |> unlist())

  ### Count number of columns in each table
  ### Count number of rows in each table
  ### Count number of distinct rows in each table
  ### Count number of missing values

  ### Expressions
  num_cols    <- listNames |> map(~ .x |> fun_nCol(a=listTypes, b=dataList)) |> unlist()
  num_rows    <- listNames |> map(~ .x |> fun_nRow(a=listTypes, b=dataList)) |> unlist()
  unique_rows <- listNames |> map(~ .x |> fun_nUnq(a=listTypes, b=dataList)) |> unlist()
  cols_wAllNA <- listNames |> map(~ .x |> fun_nNna(a=listTypes, b=dataList)) |> unlist()

  ### Add to df_info
  df_info   <- df_info |> mutate(num_cols    = num_cols)
  df_info   <- df_info |> mutate(num_rows    = num_rows)
  df_info   <- df_info |> mutate(unique_rows = unique_rows)
  df_info   <- df_info |> mutate(cols_wAllNA = cols_wAllNA)

  ###### Check Tests ######
  ### Check number of columns with some non-missing values is equal to the number of columns
  df_info   <- df_info |> mutate(na_flag  =   1 * (cols_wAllNA > 0))
  #### Check if each table has duplicate values: Number of rows should equal number of unique rows
  ### List of tables to make exceptions for
  except0   <- c("data_scaledImpacts")
  df_info   <- df_info |> mutate(has_dups = case_when((itemClass == "list") ~ F, (num_rows == unique_rows) ~ F, (table %in% except0) ~ F))
  ### Check whether all tests are passed
  df_info   <- df_info |> mutate(passed   = case_when((itemClass == "list") ~ T, (has_dups == T | na_flag == T) ~ F, (has_dups == F & na_flag == F) ~ T))
  ### Mutate logicals to numeric
  mutate0   <- c("has_dups", "passed")
  df_info   <- df_info |> mutate_at(.vars=c(mutate0), as.numeric)
  ### Remove intermediates
  rm(except0, mutate0)

  ### Print Out tables if there are any values that don't pass
  df_flags  <- df_info  |> filter(passed==0)
  numFlags  <- df_flags |> nrow()
  hasFlags  <- numFlags > 0

  ### Message user
  "Checking tables..." |> message()
  msg_flags <- ifelse(hasFlags, "Some tables don't pass", "All tables pass") |> paste0("...")
  "\t" |> paste0(msg_flags) |> message()
  rm(msg_flags)

  ### Print flagged table results
  if(hasFlags){df_flags |> glimpse()}

  ### Save Option Outputs ####
  if(save){
    "Saving data checks" |> paste0("...") |> message()
    outDir    <- outPath |> file.path("data_tests")
    outExt    <- "." |> paste0("csv")
    # csvName   <- "loadData_tests"
    csvName   <- csvName |> paste0(outExt)
    outFile   <- outDir |> file.path(csvName)
    rm(outExt, csvName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir |> dir.exists()
    if(!odExists){outDir |> dir.create(showWarnings = F)}
    rm(odExists)
    ## Save the test results
    df_info  |> write_csv(outFile)
  } ### End if_else

  ## Return options ####
  "Finished." |> message()
  if(return) {return(df_info)}

  ### End Function
}
