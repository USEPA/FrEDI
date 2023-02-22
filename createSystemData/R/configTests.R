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
  
  dataList <- list_reshapeData 
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
  df_info   <- df_info %>% mutate(has_dups = case_when((num_rows == unique_rows) ~ F, (table %in% except0) ~ F))
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
  hasFlags  <- numFlags > 0
  
  ### Message user
  msg_flags <- ifelse(hasFlags, "Some tables don't pass", "All tables pass") %>% paste0("...")
  msg_flags %>% message
  rm("msg_flags")
  
  ### Print flagged table results
  if(hasFlags){df_flags %>% glimpse}
  
### Save Option Outputs ####
  if(save){
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
    }
    
     ### End if_else
  
  ## Return options ####
  if(return){
    return(df_info)
  }
  
}




#' configTest_newSectors
#'
#' @param new_data 
#' @param old_data_file 
#' @param xslxName 
#' @param save 
#' @param return 
#'
#' @return
#' @export
#'
#' @examples
configTest_newSectors <- function(
    new_data = NULL,
    old_data_file = projectPath %>% file.path("createSystemData", "data", "sysdata.rda"),
    xslxName = "configTest_newSectors_results.xlsx",
    save    = T,
    return  = T){
  
  #require(openxlsx)
  # require(tidyverse)
  
  #new_data <- list_systemData0
  
  old_data_file %>% load(envir = globalenv(),verbose = T)
  old_data <- rDataList
  
  table_test_status <- new_data$testDev %>%
    rename(table_test ="Changes.if.new.sector.added") %>%
    select(-Column2)
  
  ### Separating yes/maybe,no in case we want to have differenttests for each in the future
  table_yes <- table_test_status %>%
    filter(table_test == "Yes")
  
  table_maybe <- table_test_status %>%
    filter(table_test == "Maybe")
  
  table_no <- table_test_status %>%
    filter(table_test == "No")
  
  
  ##### FOR NO TEST GET DIMESNSION AND IDENTICAL CHECK ###############
  table_no <- table_no %>%
    mutate(num_cols_new = Table.Name %>% map(~ (new_data[[.]] %>% ncol)) %>% unlist)
  
  table_no <- table_no %>%
    mutate(num_rows_new = Table.Name %>% map(~ (new_data[[.]] %>% nrow)) %>% unlist)
  
  table_no <- table_no %>%
    mutate(num_cols_old = Table.Name %>% map(~ (old_data[[.]] %>% ncol)) %>% unlist)
  
  table_no <- table_no %>%
    mutate(num_rows_old = Table.Name %>% map(~ (old_data[[.]] %>% nrow)) %>% unlist)
  
  table_no <- table_no %>%
    mutate(ident = Table.Name %>% map(~ (identical(new_data[[.]],old_data[[.]]))))
  
  table_no <- table_no %>%
    mutate(print = case_when(
      num_rows_new == num_rows_old & num_cols_new == num_cols_old & ident == TRUE ~ 0,
      TRUE ~ 1
    ))
  
  ##### FOR MAYBE TEST LOOK AT DIMENSION AND PRINT OUT FILE ###########
  
  table_maybe <- table_maybe %>%
    mutate(num_cols_new = Table.Name %>% map(~ (new_data[[.]] %>% ncol)) %>% unlist)
  
  table_maybe <- table_maybe %>%
    mutate(num_rows_new = Table.Name %>% map(~ (new_data[[.]] %>% nrow)) %>% unlist)
  
  table_maybe <- table_maybe %>%
    mutate(num_cols_old = Table.Name %>% map(~ (old_data[[.]] %>% ncol)) %>% unlist)
  
  table_maybe <- table_maybe %>%
    mutate(num_rows_old = Table.Name %>% map(~ (old_data[[.]] %>% nrow)) %>% unlist)
  
  table_maybe <- table_maybe %>%
    mutate(ident = Table.Name %>% map(~ (identical(new_data[[.]],old_data[[.]]))))
  
  table_maybe <- table_maybe %>%
    mutate(print = case_when(
      num_rows_new == num_rows_old & num_cols_new == num_cols_old & ident == TRUE ~ 0,
      TRUE ~ 1
    ))
  
  
  ##### FOR YES TEST LOOK AT DIMENSION AND PRINT OUT FILES and differences###########
  
  table_yes <- table_yes %>%
    mutate(num_cols_new = Table.Name %>% map(~ (new_data[[.]] %>% ncol)) %>% unlist)
  
  table_yes <- table_yes %>%
    mutate(num_rows_new = Table.Name %>% map(~ (new_data[[.]] %>% nrow)) %>% unlist)
  
  table_yes <- table_yes %>%
    mutate(num_cols_old = Table.Name %>% map(~ (old_data[[.]] %>% ncol)) %>% unlist)
  
  table_yes <- table_yes %>%
    mutate(num_rows_old = Table.Name %>% map(~ (old_data[[.]] %>% nrow)) %>% unlist)
  
  table_yes  <- table_yes %>%
    mutate(ident = Table.Name %>% map(~ (identical(new_data[[.]],old_data[[.]]))))
  
  table_yes <- table_yes %>%
    mutate(print = case_when(
      num_rows_new == num_rows_old & num_cols_new == num_cols_old & ident == TRUE ~ 0,
      TRUE ~ 1
    ))
  
  
  
  ###### COMBINE TABLES and write to xlsx #####
  
  test_table <- full_join(table_no,table_maybe) %>% full_join(table_yes)
  
  test_wb <- createWorkbook()
  addWorksheet(test_wb, sheetName = "Test_table")
  writeDataTable(test_wb,sheet = "Test_table",test_table)
  
  print_tables <- test_table %>% filter(print == 1)
  
  ### Write the diff tables to xlsx table
  walk(
    print_tables$Table.Name,
    function(x){
      new <- new_data[[x]]
      old <- old_data[[x]]
      diff <- anti_join(new,old)
      addWorksheet(test_wb,sheetName = paste0(x,"_diff"))
      writeDataTable(test_wb,sheet = paste0(x,"_diff"),diff)
    }
  )
  
  
  
  
  
  ##### ADD PLOTS ##############
  addWorksheet(test_wb, sheetName = "plots")
  
  
  temp  <- tibble(temp = -1:11)
  
  impact_func <- new_data$list_impactFunctions
  
  ## Execute the impact functions across all sectors
  out <- temp %>%
    map_df(~ impact_func %>% map_df(exec,.x)) %>%
    gather(scenario_id,impact_val)
  
  
  
  ############ For now This is specific to Vibrio. NEed to generlize to any new sector
  ## Need to get sector_name of new sector and apply the data filter for that name.
  ## would need to add by variant to if a sector has it
  
  ## vibrio regional values
  vib_reg <- out %>%
    separate(col = scenario_id , into = c("sector_name","variant","impactYear","impactType","model_type","model_dot","region_dot"), sep = "_") %>%
    filter(sector_name == "Vibrio") %>%
    mutate( temp = rep(-1:11,90)) %>%
    group_by(sector_name,variant,impactYear,impactType,model_type,region_dot,model_dot,temp) %>%
    summarize(imp_val = sum(impact_val))
  
  
  plot_sect_reg <- vib_reg %>%
    group_by(sector_name,impactYear,impactType,model_type) %>%
    do(plots=ggplot(data=.) +
         aes(x = temp,y =imp_val, color = region_dot) +
         geom_line(aes(group = region_dot), linewidth = 0.7) +
         facet_wrap(vars(model_dot),scales = "free") +
         ggtitle(paste0(unique(.$sector_name),"-",unique(.$impactType))) +
         scale_x_continuous(breaks=c(-1:11)) +
         xlab("Degrees of Warming (°C)") + # for the x axis label
         ylab(case_when(unique(.$impactType) == 'DirectMedCost' ~"Direct Medical Cost",
                        unique(.$impactType) == 'LostDays' ~ "Lost Days",
                        unique(.$impactType) == 'Mortality' ~"Mortality"))
       
    )
  
  print(plot_sect_reg$plots[[1]])
  insertPlot(test_wb,  "plots", width = 20, height = 16.5, fileType = "png", units = "cm")
  
  print(plot_sect_reg$plots[[2]])
  insertPlot(test_wb, "plots", xy = c("L", 2), width = 20, height = 16.5, fileType = "png", units = "cm")
  
  print(plot_sect_reg$plots[[3]])
  insertPlot(test_wb, "plots", xy = c("V", 2), width = 20, height = 16.5, fileType = "png", units = "cm")
  
  
  
  
  vib_tot<-vib_reg %>%
    group_by(sector_name,variant,impactType,impactYear,model_type,model_dot,temp) %>%
    summarize(imp_val = sum(imp_val))
  
  plot_tot <- vib_tot %>%
    group_by(sector_name,impactYear,impactType,model_type) %>%
    do(plots=ggplot(data=.) +
         aes(x = temp,y =imp_val, color = model_dot) +
         geom_line(linewidth = 0.7) +
         ggtitle(paste0(unique(.$sector_name),"-",unique(.$impactType))," National Total") +
         scale_x_continuous(breaks=c(-1:11)) +
         xlab("Degrees of Warming (°C)") + # for the x axis label
         ylab(case_when(unique(.$impactType) == 'DirectMedCost' ~"Direct Medical Cost",
                        unique(.$impactType) == 'LostDays' ~ "Lost Days",
                        unique(.$impactType) == 'Mortality' ~"Mortality"))
    )
  
  print(plot_tot$plots[[1]])
  insertPlot(test_wb, "plots", xy = c("A", 35), width = 20, height = 16.5, fileType = "png", units = "cm")
  
  print(plot_tot$plots[[2]])
  insertPlot(test_wb, "plots", xy = c("L", 35), width = 20, height = 16.5, fileType = "png", units = "cm")
  
  print(plot_tot$plots[[3]])
  insertPlot(test_wb, "plots", xy = c("V", 35), width = 20, height = 16.5, fileType = "png", units = "cm")
  
  
  
  
  saveWorkbook(test_wb, xslxName ,overwrite=TRUE)
  
  ### Ned to add "Save" option interaction
  
  ## Return options ####
  if_else(
    return == TRUE,
    return(test_table),
    false = return(NULL)
  )
  
}


