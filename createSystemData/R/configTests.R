
###### Dimension Tests ######
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
  
  #dataList <- list_reshapeData 
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
  } ### End if_else
  
  ## Return options ####
  if(return) {return(df_info)}
  
  ### End Function
}

###### General Configuration Tests ######
general_config_test <- function(
    reshapedData   = NULL, ### List of reshaped data
    configuredData = NULL, ### List of configured data
    # reshapedFile   = "." %>% file.path("data_tests", "reshapedData_testResults.csv"), ### File name of reshaped data test
    outPath        = ".",
    outFile        = "testResults_general_config.xlsx",
    save   = TRUE,
    return = TRUE
){
  ###### Initialize save list ######
  ### Initialize list for saving values
  saveList <- list()
  
  ###### Reshaped Data ######
  ### Check if reshaphedData exists
  has_data0  <- !is.null(reshapedData)
  # has_file0  <- !is.null(reshapedFile)
  ### If reshapedData exists, check if it's the correct class
  if(has_data0) {
    class0   <- reshapedData %>% class
    is_list0 <- "list" %in% class0
    ### If reshapedData is not a list, message the user
    if(!is_list0) {
      "`reshapedData` must be of class \`list\`..." %>% message
      "\t" %>% paste0("Exiting", "...", "\n") %>% message
      return()
    } ### End if(!is_list0) 
    else          {
      reshape0 <- reshapedData %>% dataInfo_test(save = F, return = T)
    } ### End else(!is_list0) 
    rm("class0", "is_list0")
  } ### End if(has_reshape0)
  else           {reshape0 <- data.frame()}
  # ### If no reshapedData passed to argument, try to load from file
  # else          {
  #   expr0    <- reshapedFile %>% read.csv()
  #   reshape0 <- try(expr=expr0 %>% eval, silent=T)
  #   class0   <- reshape0 %>% class
  #   is_df0   <- "data.frame" %in% class0
  #   ### Exit if unsuccessful
  #   if(!is_df0) {
  #     "Could not load file at `reshapedFile=\`" %>% paste0(reshapedFile, "\'`...") %>% message
  #     "\t" %>% paste0("Exiting", "...", "\n") %>% message
  #   }
  #   rm("expr0", "class0", "is_df0")
  # } ### End else(has_data0) 
  # ### Remove intermediate objects
  # rm("has_data0", "has_file0")
  ### Add table to list
  saveList[["reshapedData_base_test"]] <- reshape0
  
  ###### Configured Data ######
  ### Check if configuredData exists
  has_data0  <- !is.null(configuredData)
  # has_file0  <- !is.null(configuredFile)
  ### If reshapedData exists, check if it's the correct class
  if(has_data0) {
    class0   <- configuredData %>% class
    is_list0 <- "list" %in% class0
    ### If configuredData is not a list, message the user
    if(!is_list0) {
      "`configuredData` must be of class \`list\`..." %>% message
      "\t" %>% paste0("Exiting", "...", "\n") %>% message
      return()
    } ### End if(!is_list0) 
    else          {
      configure0 <- configuredData %>% dataInfo_test(save = F, return = T)
    } ### End else(!is_list0) 
    rm("class0", "is_list0")
  } ### End if(has_reshape0)
  else           {configure0 <- data.frame()}
  # ### If no configuredData passed to argument, try to load from file
  # else          {
  #   expr0      <- configuredFile %>% read.csv()
  #   configure0 <- try(expr=expr0 %>% eval, silent=T)
  #   class0     <- reshape0 %>% class
  #   is_df0     <- "data.frame" %in% class0
  #   ### Exit if unsuccessful
  #   if(!is_df0) {
  #     "Could not load file at `configuredFile=\`" %>% paste0(configuredFile, "\'`...") %>% message
  #     "\t" %>% paste0("Exiting", "...", "\n") %>% message
  #   }
  #   rm("expr0", "class0", "is_df0")
  # } ### End else(has_data0) 
  # ### Remove intermediate objects
  # rm("has_data0", "has_file0")
  ### Add table to list
  saveList[["configuredData_base_test"]] <- configure0
  
  ###### Default Values ######
  ### Items from fredi_config: 
  listConfig0   <- configuredData[["fredi_config"]]
  c_defaults0   <- c("aggList0" , "minYear", "maxYear", "baseYear0", "rate0")
  c_defaults1   <- c("aggLevels", "minYear", "maxYear", "baseYear" , "rate" )
  n_defaults0   <- listConfig0 %>% names
  w_defaults0   <- (n_defaults0 %in% c_defaults0) %>% which
  ### Filter to specified items, update names
  listConfig1   <- listConfig0[w_defaults0]
  ### Create table
  defaultsList  <- listConfig1 %>% names %>% lapply(function(
    name_i, 
    list0 = listConfig1
  ){
    val0_i <- list0[[name_i]]
    type_i <- val0_i %>% class %>% paste(collapse=", ")
    val1_i <- val0_i %>% paste(collapse=", ")
    df_i   <- tibble(parameter=name_i, class=type_i, value=val_i)
    return(df_i)
  }) %>% (function(x){do.call(rbind, x)})
  ### Update parameter names
  defaultsList  <- defaultsList %>% mutate(parameter=parameter %>% factor(c_defaults0, c_defaults1))
  defaultsList  <- defaultsList %>% mutate(parameter=parameter %>% as.character)
  ### Add table to list
  saveList[["defaultParameters"]] <- defaultsList
  
  ###### Default Plots ######
  ### Create list for default plots
  defaultPlots <- list()
  ### GDP Plot
  gdp_plot <- configuredData[["co_defaultScenario"]] %>% 
    filter(region=="Midwest") %>% 
    ggplot() +
    geom_line(aes(x = year, y = gdp_usd))
  # print(gdp_plot)
  # insertPlot(init_test_wb,  "default_plots", width = 6, height = 4.5, fileType = "png", units = "in")
  defaultPlots[["gdp_plot"]] <- gdp_plot
  
  ### Pop plot
  pop_plot <- configuredData[["co_defaultScenario"]] %>% 
    ggplot() +
    geom_line(aes(x = year, y = reg_pop, color = region), alpha = 0.75)
  # print(pop_plot)
  # insertPlot(init_test_wb,  "default_plots", xy = c("K", 2), width = 16, height = 10, fileType = "png", units = "cm")
  defaultPlots[["pop_plot"]] <- gdp_plot
  
  ### Temp plot
  temp_plot <- configuredData[["co_defaultTemps"]] %>% 
    ggplot() +
    geom_line(aes(x = year, y = temp_C_global))
  # print(temp_plot)
  # insertPlot(init_test_wb,  "default_plots", xy = c("A", 25), width = 16, height = 10, fileType = "png", units = "cm")
  defaultPlots[["temp_plot"]] <- gdp_plot
  
  
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
  } ### End if_else
  
  
} ### End general_config_test


###### New Sector Configuration Tests ######

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
    newData = NULL,
    refDataFile = "." %>% file.path("data", "sysdata.rda"),
    # sector_id = "",
    xslxName  = "configTest_newSectors_results.xlsx",
    save      = T,
    return    = T
){
  ### Load ref data
  refData   <- refDataFile %>% load(verbose = T)
  refData   <- refData[["rDataList"]]
  
  ### Create table of status, rename and drop some columns
  ### Mutate values for changes_expected
  levels0   <- c("No", "Maybe", "Yes")
  mutate0   <- c("changes_expected")
  select0   <- c("Column2")
  df_status <- newData[["testDev"]]
  df_status <- df_status %>% rename_at(.vars=c("Changes.if.new.sector.added"), ~mutate0)
  df_status <- df_status %>% mutate_at(.vars=c(all_of(mutate0)), factor, levels=levels0)
  df_status <- df_status %>% select(-c(all_of(select0)))
  rm("mutate0", "select0")
  
  ###### Check values ######
  ### Could filter on `table_test` columns if different tests required in the future
  ### When no changes are expected still get dimensions and check that values are identical
  df_status <- df_status %>% mutate(numCols_new = Table.Name %>% map(~ (newData[[.]] %>% ncol)) %>% unlist)
  df_status <- df_status %>% mutate(numRows_new = Table.Name %>% map(~ (newData[[.]] %>% nrow)) %>% unlist)
  df_status <- df_status %>% mutate(numCols_ref = Table.Name %>% map(~ (refData[[.]] %>% ncol)) %>% unlist)
  df_status <- df_status %>% mutate(numRows_ref = Table.Name %>% map(~ (refData[[.]] %>% nrow)) %>% unlist)
  df_status <- df_status %>% mutate(sameDims    = 1*(numCols_new == numCols_ref) & (numRows_new == numRows_ref))
  df_status <- df_status %>% mutate(sameVals    = 1*(Table.Name %>% map(~(identical(newData[[.]],refData[[.]])))))
  df_status <- df_status %>% mutate(hasDiffs    = 1*(!sameDims | !sameVals))
  
  ###### Arrange values ######
  arrange0  <- c("changes_expected", "hasDiffs", "sameDims", "sameVals", "Table.Name")
  df_status <- df_status %>% arrange_at(.vars=c(all_of(arrange0)))
  rm("arrange0")
  
  ###### Create workbook ######
  ### Create workbook
  ### Add worksheet with test info
  wbook0  <- createWorkbook()
  sheet0  <- "config_newSector_tests"
  wbook0 %>% addWorksheet(sheetName = sheet0)
  wbook0 %>% writeDataTable(sheet = sheet0, df_status)
  
  ###### Print tables ######
  ### Filter to tables with differences
  ### Print tables with differences
  df_diff <- df_status %>% filter(print == 1)
  names0  <- df_diff$Table.Name
  ### Write the diff tables to xlsx table
  names0 %>% walk(function(
    name_i, 
    new0=newData[[name_i]], 
    ref0=refData[[name_i]]
  ){
    ### Get difference
    diff0  <- new0 %>% anti_join(ref0)
    sheet0 <- name_i %>% paste("diff", sep="_")
    ### Add worksheet
    wbook0 %>% addWorksheet(sheetName = sheet0)
    wbook0 %>% writeDataTable(sheet = sheet0,diff)
  }) ### End function(name_i), end walk
  
  ###### New impact functions ######
  ### Figure out which functions are new and then filter to those functions
  ### Function lists
  newFunList  <- newData[["list_impactFunctions"]]
  refFunList  <- refData[["list_impactFunctions"]]
  ### Names
  newFunNames <- newFunList %>% names
  refFunNames <- refFunList %>% names
  ### New names and new list
  funNames    <- newFunNames[!(newFunNames %in% refFunNames)]
  funList     <- newFunList[funNames]
  ### Remove intermediate values
  rm("newFunList", "newFunNames", "refFunList", "refFunNames")
  
  ###### Scaled impacts ######
  ### Create temperature scenario
  ### Execute the impact functions across new sectors, then gather values
  df_temps    <- tibble(temp_C = -1:11)
  df_vals     <- df_temps %>% map_df(~ funList %>% map_df(exec,.x))
  df_vals     <- df_vals  %>% gather(key="scenario_id",value="scaled_impact")
  rm("df_temps")
  
  ### Separate scenario_id into components
  into0       <- c("sector", "variant", "impactYear", "impactType", "model_type", "model_dot", "region_dot")
  df_vals     <- df_vals %>% separate(col = scenario_id , into = c(all_of(into0)), sep = "_")
  ### Filter to new sector_id
  # df_vals0    <- df_vals0 %>% filter(sector==sector_id)
  # df_vals0    <- df_vals0 %>% rename_at(.vars=c("sector"), ~sector_id)
  
  ### Get unique sectors
  sectors0    <- df_vals$sector %>% unique
  
  ### Regional plots
  plots0  <- sectors0 %>% lapply(function(
    sector_i, 
    data_i = df_vals %>% filter(sector==sector_i)
  ){
    paste0("Plotting scaled impacts for sector \'", sector_i, "\'", "...") %>% message
    ### Get unique variants
    variants_i <- data_i$variant %>% unique
    # nVariants  <- variants_i %>% length
    plots_i    <- variants_i %>% lapply(function(
    variant_j,
    data_j = data_i %>% filter(variant==variant_j)
    ){
      ### Initialize plot list
      plots_j <- list()
      ### Regional plots
      ### Groups
      groups_j <- c("sector", "variant", "impactYear", "impactType", "model_dot", "region_dot") 
      plots_j[["regional"]]  <- data_j %>% ggplot() +
        geom_line(aes(x = temp_C, y =scaled_impacts, color = region_dot), alpha = 0.7, linewidth = 0.7)
      ### Add facets
      plots_j  <- plots_j + facet_wrap(facets=c("model_dot", "impactType"),scales = "free") 
      ### Add scales
      plots_j  <- plots_j + scale_x_continuous("Degrees of Warming (°C)", breaks=df_temps$temp_C)
      plots_j  <- plots_j + scale_y_continuous(unique(.$impactType))
      ### Add title
      plots_j  <- plots_j + ggtitle(paste0(unique(.$sector),"-",unique(.$impactType)))
      
      ### National plots
      # plots_j[["national"]]
      
      ### Return plot
      return(plots_j)
    })
    ### Add names
    names(plots_i) <- variants_i
    ### Return
    return(plots_i)
  }) %>%
    ### Add names
    (function(i, sectors_i=sectors0){names(i) <- sectors_i; return(i)})
  
  
  
  ### Iterate over each sector and add worksheet for each sector
  sectors0 %>% walk(function(
    sector_i, 
    plots_i = plots0
    ){
    ### Add worksheet
    sheet_i <- "plots" %>% paste0("_", sector_i)
    wbook0 %>% addWorksheet(sheetName = sheet_i)
    wbook0 %>% writeDataTable(sheet = sheet_i,diff)
    
    ### Plot vals
    width_i  <- 20
    height_i <- 16.5
    ftype_i  <- "png"
    units_i  <- "cm"
    ### Get variants
    variants_i <- plots_i %>% names
    nVar_i     <- variants_i %>% length
    ### Add regional plots
    for(j in 1:nVar_i){
      variant_j <- variants_i[j]
      plots_j   <- plots_i[[plot_j]]
      col_j     <- 11 * (j - 1) + 1
      row_j     <- 11 * (j - 1) + 2
      xy_j      <- c(col_j, row_j)
      print(plots_j[["regional"]])
      wbook0 %>% insertPlot(sheet = sheet_i, width = width_i, height = height_i, fileType = ftype_i, units = units_i)
    }
    
  }) ### End function, end walk
  
  # print(plot_sect_reg$plots[[1]])
  # insertPlot(test_wb,  "plots", width = 20, height = 16.5, fileType = "png", units = "cm")
  # 
  # print(plot_sect_reg$plots[[2]])
  # insertPlot(test_wb, "plots", xy = c("L", 2), width = 20, height = 16.5, fileType = "png", units = "cm")
  # 
  # print(plot_sect_reg$plots[[3]])
  # insertPlot(test_wb, "plots", xy = c("V", 2), width = 20, height = 16.5, fileType = "png", units = "cm")
  
  ###### Save workbook
  saveWorkbook(test_wb, xslxName ,overwrite=TRUE)
  
  ### Ned to add "Save" option interaction
  
  ## Return options ####
  if_else(
    return == TRUE,
    return(test_table),
    false = return(NULL)
  )
  
}


