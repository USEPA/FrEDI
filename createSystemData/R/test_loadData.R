#' test_DataList
#' test_DataList serves as a function to compare the dimensions of 
#' two lists of dataframes and check for possible duplicated rows. 
#' These datalists are expected to be related such the second one is 
#' based on the first. Function options include the ability to save 
#' outupts to a file and also return the output.
#' 
#' @param data_list1 a list of named dataframes for comparison
#' @param data_list2 a list of named dataframes for comparison
#' @param outPath path to where test table will be written
#' @param save TRUE/FALSE option to save outputs that have been flagged for cheking, to csv
#' @param return TRUE/FALSE option to return outputs to environment
#'
#' @return
#' @export
#'
#' @examples
test_DataList <- function(
    data_list1 = NULL,
    data_list2 =NULL,
    outPath = getwd(),
    fileName = "Table",
    save = TRUE,
    return = TRUE){
  

  # Read in the first list of data frames ####
  dat_1 <- tibble(
    # get names of each table
    tab_name = names(data_list1),
    # count number of rows
    count_rows_one = data_list1 %>%
      names(.) %>%
      map( ~ nrow(data_list1[[.]])) %>%
      unlist(),
    # count number of unique rows
    unique_rows_one = data_list1 %>%
      names(.) %>%
      map( ~ nrow(distinct(data_list1[[.]]))) %>%
      unlist(),
    # count number of columns
    count_cols_one = data_list1 %>%
      names(.) %>%
      map( ~ ncol(data_list1[[.]])) %>%
      unlist(),
    # count number of unique columns
    unique_cols_one = data_list1 %>%
      names(.) %>%
      map( ~ ncol(distinct(data_list1[[.]]))) %>%
      unlist()
  )
  # Read in second list of data frames ####
  dat_2 <- tibble(
    # get nanes of each table
    tab_name = names(data_list2),
    # count number of rows
    count_rows_two = data_list2 %>%
      names(.) %>%
      map( ~ nrow(data_list2[[.]])) %>%
      unlist(),
    # count number of unique rows
    unique_rows_two = data_list2 %>%
      names(.) %>%
      map( ~ nrow(distinct(data_list2[[.]]))) %>%
      unlist(),
    # count number of columns
    count_cols_two = data_list2 %>%
      names(.) %>%
      map( ~ ncol(data_list2[[.]])) %>%
      unlist(),
    # count number of unique columns
    unique_cols_two = data_list2 %>%
      names(.) %>%
      map( ~ ncol(distinct(data_list2[[.]]))) %>%
      unlist()
  )
  
  # Create Summary table of test elements ####
  test_tab <- left_join(dat_1, dat_2) %>%
    mutate(
      dims_same = case_when(
      ## Check if the tables have the same number of columns and rows
        count_rows_one == count_rows_two &
          count_cols_one == count_cols_two ~  TRUE,
          count_rows_one != count_rows_two |
            count_cols_one != count_cols_two ~  FALSE
      ),
      ## check if each table has duplicate values
      ## Number of rows should equal number of unique rows
      dup_vals = case_when(count_rows_one == unique_rows_one &
                           count_rows_two == unique_rows_two~ FALSE,
                           TRUE ~ TRUE)
    ) %>%
    mutate(
      ## Check if values are the same in each table
      ## Currently checks if each table is exactly identical (should customize)
      vals_same = tab_name %>%
             map( ~ identical(data_list1[.], data_list2[.])) %>% unlist(),
      ## Create a check flag for tables that need to be checked
           check_flag = case_when(
             dims_same ==FALSE & vals_same == FALSE & dup_vals==TRUE ~ TRUE
           )
      )
  
  ### print out files if check indicated
  
  # Find table names that are flagged to be checked ####
  check_flags <- test_tab %>%
    filter(check_flag == TRUE) %>%
    select(tab_name)
  
  
  # find the anti_join (rows that are different) of the flagged tables ####
  try(diff_tables <- test_tab %>%
    filter(check_flag == TRUE) %>%
    select(tab_name) %>%
    map(~anti_join(data_list1[[.]],data_list2[[.]])),
    silent = TRUE)
  
  # Save Option Outputs ####
  ## 
  if_else(
    save == TRUE,
    true = {
      message("Saving Data Checks");
      ## Save the Summary test table
      write_csv(test_tab,file.path(outPath,paste0("test_table_",fileName,".csv")));
      if(nrow(check_flags)>0){
        message("Creating flagged datasets1");
        test_tab %>%
          filter(check_flag == TRUE) %>%
          select(tab_name) %>%
          map(~ write_csv(data_list1[[.]], paste0(outPath,"/", .,fileName, "_dat1.csv")))
      };
      ## Save the flagged datasets
      if(nrow(check_flags)>0){
        message("Creating flagged datasets2");
        test_tab %>%
          filter(check_flag == TRUE) %>%
          select(tab_name) %>%
          map(~ write_csv(data_list2[[.]], paste0(outPath,"/", .,fileName, "_dat2.csv")))
      };
      ## Save the anti_join table
      if(exists("diff_tables")){diff_tables %>%
          message("Creating flagged anti_join table");
          test_tab %>%
          names(.) %>%
          map(~ write_csv(diff_tables[[.]], paste0(outPath,"/", .,fileName, "_difftables.csv")))}
    },
    false = NULL
  )
  
  ## Return options ####
  if_else(
    return == TRUE,
    return(test_tab),
    false = return(NULL)
  )
  
}



