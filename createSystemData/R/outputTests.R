
###### General Output Tests ######

general_output_test <- function(
    new_output,
    outPath   = ".",
    xlsxName  = "testResults_general_output.xlsx",
    save      = TRUE,
    return    = TRUE,
    overwrite = TRUE
){
  
  ##### Prepare the Data ######
  # load Default Data
  data("defaultResults")
  
  #new_output <- defaultResults %>%
  #              filter(sector != "Air Quality")
  save_list <- list()
  
  #### Data Table Names ######
  ### Names of objects to save
  c_out0 <- "general_tests"
  c_diff0   <- "tests_diffs"
  
  
  #### General Dimension Checks ####
  data_list <- list(defaultResults,new_output)
    names(data_list) <- c("old_dat", "new_dat")
       
  general_test <- data_list %>% dataInfo_test(
                  csvName = "output_testResults",
                  save    = FALSE,
                  return  = return_test
                  )
  
  save_list[["general_test"]] <- general_test
  
  ### Write dimension checks to workbook
  if(save){
    wbook0  <- createWorkbook()
    wbook0 %>% addWorksheet(sheetName = c_out0)
    wbook0 %>% writeDataTable(sheet = c_out0, x = general_test)
  }
  
  
  #####  Find Differences between table  #######
  
  table_anti_join <- anti_join(defaultResults,new_output)
  
  save_list[["table_difference"]] <- table_anti_join
  
  if(save){
    wbook0 %>% addWorksheet(sheetName = c_diff0)
    wbook0 %>% writeDataTable(sheet = c_diff0, table_anti_join)
  }

    
  #### Save Write Workbook Option ####
  if(save){
    "Saving output test results" %>% paste0("...") %>% message
    outDir    <- outPath %>% file.path("data_tests")
    outFile   <- outDir  %>% file.path(xlsxName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir  %>% dir.exists()
    if(!odExists){outDir %>% dir.create(showWarnings = F)}
    rm("odExists")
    ### Save the workbook
    wbook0  %>% saveWorkbook(file=outFile, overwrite=overwrite)
    ### Remove workbook
    rm("wbook0")
  } ### End if(save)
  
  
  ## Return options ####
  if(return) {
    return(save_list)
  } ### End return
  
  
  
}
