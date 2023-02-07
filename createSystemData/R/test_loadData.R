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
test_loadData <- function(
    data_list = NULL,
    outPath = getwd(),
    save = TRUE,
    return = TRUE) {
  ## Function to check if column has at least one non NA value
  check_na <- function(x) {
    tibble(
      n_na = colSums(is.na(x)),
      n_rows = nrow(x),
      na_all = n_rows > n_na
    ) %>%
      filter(na_all == FALSE) %>%
      nrow()
  }


  # Read in the first list of data frames ####
  dat_att <- tibble(
    # get names of each table
    Table.Name = names(data_list),
    # count number of rows
    row_count = data_list %>%
      names(.) %>%
      map(~ nrow(data_list1[[.]])) %>%
      unlist(),
    # count number of unique rows
    unique_rows = data_list %>%
      names(.) %>%
      map(~ nrow(distinct(data_list[[.]]))) %>%
      unlist(),
    # count number of columns
    col_count = data_list %>%
      names(.) %>%
      map(~ ncol(data_list1[[.]])) %>%
      unlist(),
    # find if there is at least one non-missing value in each column
    miss_val_check = data_list %>%
      names(.) %>%
      map(~ check_na(data_list[[.]])) %>%
      unlist(),
  )


  # Create Summary table of test elements ####

  test_tab <- dat_att %>%
    mutate(
      ## check if each table has duplicate values
      ## Number of rows should equal number of unique rows
      dup_vals = case_when(
        row_count == unique_rows ~ FALSE,
        Table.Name == "data_scaledImpacts" ~ FALSE,
      ),
      miss_val_check = case_when(
        miss_val_check == 0 ~ FALSE,
        miss_val_check > 0 ~ TRUE
      )
    ) %>%
    mutate(
      ## Create test_pass function
      test_pass = case_when(
        dup_vals == TRUE | miss_val_check == TRUE ~ FALSE,
        dup_vals == FALSE & miss_val_check == FALSE ~ TRUE
      )
    )


  ### Print Out tables if


  # Save Option Outputs ####
  ##
  if_else(
    save == TRUE,
    true = {
      message("Saving Data Checks")
      outdir <- file.path(outPath, "test_data")
      tmp_dir <- dir.create(outdir, showWarnings = FALSE);
      ## Save the Summary test table
      write_csv(test_tab, file.path(outdir, "loadData_tests.csv"))
      if (any(test_tab$test_pass == FALSE)) {
        ##
        message("Creating flagged datasets")
        test_tab %>%
          filter(check_flag == TRUE) %>%
          select(Table.Name) %>%
          map(~ write_csv(data_list[[.]], file.path(outdir, paste0(., "_loadData_test.csv"))))
      }
    },
    false = message("All datasets pass")
  )

  ## Return options ####
  if_else(
    return == TRUE,
    return(test_tab),
    false = return(NULL)
  )
}
