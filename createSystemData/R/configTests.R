
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
  ###### List Info ######
  ### List names
  ### Length of list and list names
  listNames <- dataList  %>% names
  lenList   <- dataList  %>% length
  lenName   <- listNames %>% length
  # c(lenList, lenName, lenList==lenName) %>% print
  
  ###### List Object Types ######
  ### Get info on object types and add names
  ### Simplify list of types
  # cTypes    <- c("data.frame", "list", "character", "numeric")
  listTypes <- listNames %>% map(~ (dataList[[.]] %>% class))
  ### Add names back to list
  listTypes <- listTypes %>% (function(x){names(x) <- listNames; return(x)})
  # listTypes[1] %>% print
  ### Simplify types
  listTypes <- listNames %>% map(~ case_when(
    ("data.frame" %in% listTypes[[.]]) ~ "data.frame",
    ("list"       %in% listTypes[[.]]) ~ "list",
    ("character"  %in% listTypes[[.]]) ~ "character",
    ("numeric"    %in% listTypes[[.]]) ~ "numeric",
    TRUE ~ "N/A"
  ))
  ### Add names back to list
  listTypes <- listTypes %>% (function(x){names(x) <- listNames; return(x)})
  # c(length(listTypes), names(listTypes) %>% length) %>% print
  
  ###### Initial Table Info ######
  ### Initialize table of info...make methods specific to class
  ### Get class of table object
  df_info   <- tibble(table = listNames)
  df_info   <- df_info %>% mutate(itemClass = listTypes %>% unlist)
  
  ### Count number of columns in each table
  ### Count number of rows in each table
  ### Count number of distinct rows in each table
  ### Count number of missing values 
  
  ### Expressions
  num_cols    <- listNames %>% map(~ .x %>% fun_nCol(a=listTypes, b=dataList)) %>% unlist
  num_rows    <- listNames %>% map(~ .x %>% fun_nRow(a=listTypes, b=dataList)) %>% unlist
  unique_rows <- listNames %>% map(~ .x %>% fun_nUnq(a=listTypes, b=dataList)) %>% unlist
  cols_wAllNA <- listNames %>% map(~ .x %>% fun_nNna(a=listTypes, b=dataList)) %>% unlist
  
  ### Add to df_info
  df_info   <- df_info %>% mutate(num_cols    = num_cols)
  df_info   <- df_info %>% mutate(num_rows    = num_rows)
  df_info   <- df_info %>% mutate(unique_rows = unique_rows)
  df_info   <- df_info %>% mutate(cols_wAllNA = cols_wAllNA)
  
  ###### Check Tests ######
  ### Check number of columns with some non-missing values is equal to the number of columns 
  df_info   <- df_info %>% mutate(na_flag  =   1 * (cols_wAllNA > 0))
  #### Check if each table has duplicate values: Number of rows should equal number of unique rows
  ### List of tables to make exceptions for
  except0   <- c("data_scaledImpacts")
  df_info   <- df_info %>% mutate(has_dups = case_when((itemClass == "list") ~ F, (num_rows == unique_rows) ~ F, (table %in% except0) ~ F))
  ### Check whether all tests are passed
  df_info   <- df_info %>% mutate(passed   = case_when((itemClass == "list") ~ T, (has_dups == T | na_flag == T) ~ F, (has_dups == F & na_flag == F) ~ T))
  ### Mutate logicals to numeric
  mutate0   <- c("has_dups", "passed")
  df_info   <- df_info %>% mutate_at(.vars=c(all_of(mutate0)), as.numeric)
  ### Remove intermediates
  rm("except0", "mutate0")
  
  ### Print Out tables if there are any values that don't pass
  df_flags  <- df_info  %>% filter(passed==0)
  numFlags  <- df_flags %>% nrow
  hasFlags  <- numFlags > 0
  
  ### Message user
  "Checking tables..." %>% message
  msg_flags <- ifelse(hasFlags, "Some tables don't pass", "All tables pass") %>% paste0("...")
  "\t" %>% paste0(msg_flags) %>% message
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
  "Finished." %>% message
  if(return) {return(df_info)}
  
  ### End Function
}

###### General Configuration Tests ######
general_config_test <- function(
    reshapedData   = NULL, ### List of reshaped data
    configuredData = NULL, ### List of configured data
    # reshapedFile   = "." %>% file.path("data_tests", "reshapedData_testResults.csv"), ### File name of reshaped data test
    outPath   = ".",
    xlsxName  = "testResults_general_config.xlsx",
    save      = TRUE,
    return    = TRUE,
    overwrite = TRUE, ### Whether to overwrite an existing file,
    fredi_config = NULL ### fredi_config list object
){
  ###### Initialize List ######
  ### Initialize list for saving values
  ### Initialize list for default plots
  saveList   <- list()
  listPlots  <- list()
  
  ###### Data Names ######
  ### Data Names
  rshpName0  <- "reshapedData_base_test"
  cfigName0  <- "configuredData_base_test"
  defParam0  <- "defaultParameters"
  defPlots0  <- "defaultPlots"
  
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
  } ### End if(has_data0) 
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
  saveList[[rshpName0]] <- reshape0
  
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
  ### Add table to list
  saveList[[cfigName0]] <- configure0
  
  ###### Default Values ######
  # "got here" %>% print
  ### Items from fredi_config: 
  listConfig0   <- fredi_config
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
    df_i   <- tibble(parameter=name_i, class=type_i, value=val1_i)
    return(df_i)
  }) %>% (function(x){do.call(rbind, x)})
  ### Update parameter names
  defaultsList  <- defaultsList %>% mutate(parameter=parameter %>% factor(c_defaults0, c_defaults1))
  defaultsList  <- defaultsList %>% mutate(parameter=parameter %>% as.character)
  ### Add table to list
  saveList[[defParam0]] <- defaultsList
  
  ###### Create Excel Workbook ######
  ### Create Excel workbook if save
  if(save) {
    ### Open workbook
    wbook0  <- createWorkbook()
    ### Sheet Names
    names0  <- saveList %>% names
    for(name_i in names0) {
      wbook0 %>% addWorksheet(sheetName = name_i)
      wbook0 %>% writeDataTable(sheet = name_i, saveList[[name_i]])
      rm("name_i")
    } ### End for(name_i in names0) 
    rm("names0")
  }
  
  ###### Default Plots ######
  ### Plot values
  lab_tmp0  <- expression("CONUS Degrees of Warming ("~degree*C*")")
  lab_yrs0  <- "Year"
  brk_yrs0  <- seq(2010, 2300, by=20)
  ### Temp plot
  temp_plot <- configuredData[["temp_default"]] %>% 
    ggplot() +
    geom_line(aes(x = year, y = temp_C_conus)) +
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0) +
    scale_y_continuous(lab_tmp0) + 
    ggtitle("Default SLR Scenario")
  ### SLR plot
  slr_plot  <- configuredData[["slr_default"]] %>% 
    ggplot() +
    geom_line(aes(x = year, y = slr_cm)) +
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0) +
    scale_y_continuous("SLR (cm)") + 
    ggtitle("Default SLR Scenario")
  ### GDP Plot: Convert to Billions
  gdp_plot <- configuredData[["gdp_default"]] %>% 
    mutate(gdp_usd = gdp_usd / 1e9) %>% 
    ggplot() +
    geom_line(aes(x = year, y = gdp_usd)) + 
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0) + 
    scale_y_continuous("U.S. National GDP (2015$, billions)") + 
    ggtitle("Default GDP Scenario")
  ### Pop plot
  pop_plot <- configuredData[["pop_default"]] %>% 
    mutate(reg_pop = reg_pop / 1e6) %>% 
    ggplot() +
    geom_line(aes(x = year, y = reg_pop, color = region), alpha = 0.75) +
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0) + 
    scale_y_continuous("Regional Population (millions)") + 
    scale_color_discrete("Region") +
    ggtitle("Default Population Scenario")
  ### Add plots to list
  listPlots[["temp_plot"]] <- temp_plot
  listPlots[["slr_plot" ]] <- slr_plot
  listPlots[["gdp_plot" ]] <- gdp_plot
  listPlots[["pop_plot" ]] <- pop_plot
  ### Add plot list to saveList
  saveList[[defPlots0]] <- listPlots
  
  
  ###### Add Plots to Excel Workbook
  if(save) {
    ### Add worksheet
    sheet0  <- defPlots0
    wbook0 %>% addWorksheet(sheetName = sheet0)
    ### Plot info
    fType0  <- "png"
    units0  <- "in"   
    ### Add plots
    ### Temperature
    listPlots[["temp_plot"]] %>% print()
    wbook0 %>% insertPlot(sheet0, xy = c(1 , 2 ), width = 6, height = 4.5, fileType = fType0, units = units0)
    ### SLR
    listPlots[["slr_plot"]] %>% print()
    wbook0 %>% insertPlot(sheet0, xy = c(12, 2 ), width = 6, height = 4.5, fileType = fType0, units = units0)
    ### GDP
    listPlots[["gdp_plot"]] %>% print()
    wbook0 %>% insertPlot(sheet0, xy = c(1 , 25), width = 6, height = 4.5, fileType = fType0, units = units0)
    ### Population
    listPlots[["pop_plot"]] %>% print()
    wbook0 %>% insertPlot(sheet0, xy = c(12, 25), width = 8, height = 4.5, fileType = fType0, units = units0)
  }
  rm("sheet0", "fType0", "units0")
  
  
  ###### Save Option Outputs ######
  if(save){
    "Saving data checks" %>% paste0("...") %>% message
    outDir    <- outPath %>% file.path("data_tests")
    # outExt    <- "." %>% paste0("xlsx")
    # xlsxName  <- xlsxName %>% paste0(outExt)
    outFile   <- outDir %>% file.path(xlsxName)
    rm("xlsxName")
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir %>% dir.exists()
    if(!odExists){outDir %>% dir.create(showWarnings = F)}
    rm("odExists")
    ### Save the workbook
    wbook0  %>% saveWorkbook(file=outFile, overwrite=overwrite)
    ### Remove workbook
    rm("wbook0")
  } ### End if(save)
  
  ###### Return ######
  if(return) {
    return(saveList)
  } ### End if(return)
  
} ### End general_config_test


###### New Sector Configuration Tests ######
#' configTest_newSectors
#'
#' @param newData 
#' @param refDataFile 
#' @param outPath
#' @param xslxName 
#' @param save 
#' @param return 
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
newSectors_config_test <- function(
    newData = NULL,
    refDataFile = "." %>% file.path("data", "sysdata.rda"),
    # sector_id = "",
    outPath   = ".",
    xslxName  = "testResults_newSectors_config.xlsx",
    save      = T,
    return    = T,
    overwrite = T
){
  ###### Initialize Save List ######
  saveList  <- list()
  
  ###### Data Names ######
  ### Names of objects to save
  c_config0 <- "tests"
  c_diff0   <- "tests_diffs"
  c_impact0 <- "scaledImpacts_values"
  c_plots0  <- "scaledImpacts_plots"
  
  ###### Load Reference Data ######
  ### Load ref data
  newEnv <- new.env()
  refDataFile %>% load(verbose = F, envir=newEnv)
  # ls(envir=newEnv) %>% print
  refData <- "rDataList" %>% get(envir=newEnv, inherits = F)
  # ls() %>% print; refData %>% names %>% print
  refFunList <- refData[["list_impactFunctions"]]
  rm("newEnv")
  # return(refData)
  
  ###### Format New Data ######
  newFunList <- newData[["list_impactFunctions"]]
  # return(refData)
  
  ###### Table Info ######
  ### Create table of status, rename and drop some columns
  ### Mutate values for changes_expected
  levels0   <- c("No", "Maybe", "Yes")
  mutate0   <- c("changes_expected")
  df_status <- newData[["testDev"]]
  df_status <- df_status %>% rename_at(.vars=c("Changes.if.new.sector.added"), ~mutate0)
  df_status <- df_status %>% mutate_at(.vars=c(all_of(mutate0)), factor, levels=levels0)
  rm("mutate0", "levels0")
  
  ###### Compare New & Ref Data ######
  ###### ** Get Test Info ######
  ### Get test info for new and old data
  newTests  <- newData %>% dataInfo_test(save = F, return = T)
  refTests  <- newData %>% dataInfo_test(save = F, return = T)
  ### Select appropriate columns and join old and new test info
  join0     <- c("table")
  sum0      <- c("num_cols", "num_rows")
  rename0   <- c("numCols" , "numRows" )
  select0   <- join0 %>% c("itemClass", sum0)
  select1   <- join0 %>% c(sum0)
  suffix0   <- c("_new", "_ref")
  ### Select columns
  newTests  <- newTests %>% select(c(all_of(select0)))
  refTests  <- refTests %>% select(c(all_of(select1)))
  ### Rename columns
  newTests  <- newTests %>% rename_at(.vars=c(all_of(sum0)), ~rename0)
  refTests  <- refTests %>% rename_at(.vars=c(all_of(sum0)), ~rename0)
  ### Join old and new 
  df_tests  <- newTests %>% left_join(refTests, by=c(all_of(join0)), suffix=suffix0)
  rm("join0", "sum0", "select0", "select1", "rename0"); rm("newTests", "refTests")
  
  ###### ** Join Tests and Test Info ######
  ### Join df_tests with df_status
  join0     <- c("Table.Name")
  rename0   <- c("table")
  ### Check number of rows before
  dim0      <- c(nrow(df_status), nrow(df_tests))
  ### Rename columns and join columns
  df_tests  <- df_tests  %>% rename_at(.vars=c(all_of(rename0)), ~join0)
  df_status <- df_status %>% left_join(df_tests, by=c(all_of(join0)))
  ### Check number of rows before
  dim1      <- c(nrow(df_status), nrow(df_tests))
  all0      <- (dim1 == dim0) %>% all
  rm("join0", "rename0", "all0"); rm("df_tests")
  # "got here2" %>% print; 
  
  ###### ** Compare Values ######
  ### Could filter on `table_test` columns if different tests required in the future
  ### When no changes are expected still get dimensions and check that values are identical
  # df_status %>% names %>% print
  df_status <- df_status %>% mutate(sameDims = 1 * ((numCols_new == numCols_ref) & (numRows_new == numRows_ref)))
  # df_status <- df_status %>% mutate(sameVals = Table.Name %>% map(~ifelse(
  #   (refData[[.]] %>% is.null) | newData[[.]] %>% is.null) | ("list" %in% class(newData[[.]])),
  #   yes = NA,
  #   no  = newData[[.]] %>% identical(refData[[.]])
  # )) %>% unlist)
  # df_status <- df_status %>% mutate(hasDiffs = 1 * (!sameDims | !sameVals))
  checkVals <- df_status %>% nrow %>% seq_len %>% lapply(function(i, df1 = newData, df2 = refData){
    name_i  <- df_status[["Table.Name"]][i]
    df1_i   <- df1[[name_i]]
    df2_i   <- df2[[name_i]]
    
    ### Check whether to check values
    skip_i  <- ("list" %in% class(df1_i)) | df1_i %>% is.null | df2_i %>% is.null
    check_i <- !skip_i
    ### Initialize return value
    y_i     <- NA
    if(check_i) {y_i <- 1 * identical(df1_i, df2_i)}
    return(y_i)
  }) %>% unlist
  # checkVals %>% print
  df_status <- df_status %>% mutate(sameVals = checkVals)
  df_status <- df_status %>% mutate(hasDiffs = 1 * (!sameDims | !sameVals))
  rm("checkVals")
  
  ###### ** Arrange Test Results ######
  ### Arrange values and add to save list
  arrange0  <- c("changes_expected", "hasDiffs", "sameDims", "sameVals", "Table.Name")
  df_status <- df_status %>% arrange_at(.vars=c(all_of(arrange0)))
  saveList[[c_config0]] <- df_status 
  rm("arrange0")
  
  ###### Create Workbook ######
  ### Create workbook if(save)
  ### Add worksheet with test info
  if(save){
    wbook0  <- createWorkbook()
    sheet0  <- c_config0
    wbook0 %>% addWorksheet(sheetName = sheet0)
    wbook0 %>% writeDataTable(sheet = sheet0, x = df_status)
    rm("sheet0")
  }
  
  ###### Print Test Results ######
  ### Filter to tables with differences and add to list and workbook
  df_diff <- df_status %>% filter(hasDiffs == 1)
  saveList[[c_diff0]] <- df_diff
  # df_diff %>% glimpse
  
  ### Iterate over names of tables with differences:
  ### - Add tables with differences to list
  ### - Write tables with differences to xlsx workbook
  names0  <- df_diff[["Table.Name"]]
  names0 %>% walk(function(
    name_i, 
    new0=newData[[name_i]], 
    ref0=refData[[name_i]]
  ){
    ### Worksheet/list name
    sheet0 <- name_i %>% paste("diff", sep="_")
    
    ### Get difference
    join0  <- new0   %>% names %>% (function(y, z=ref0){y[(y %in% names(z))]})
    diff0  <- new0   %>% anti_join(ref0, by=c(all_of(join0)))
    rm("join0")
    
    ### Add table to list
    saveList[[sheet0]] <- diff0
    
    ### Add worksheet and write data table if(save)
    if(save) {
      wbook0 %>% addWorksheet(sheetName = sheet0)
      wbook0 %>% writeDataTable(sheet = sheet0, diff0)
    } ### End if(save) 
  }) ### End function(name_i), end walk
  
  ###### New Sector Results ######
  ###### ** New Impact Functions ######
  ### Figure out which functions are new and then filter to those functions
  ### Function lists
  ### Names
  newFunNames <- newFunList %>% names
  refFunNames <- refFunList %>% names
  ### New names and new list
  funNames    <- newFunNames[!(newFunNames %in% refFunNames)]
  funList     <- newFunList[funNames]
  funLength   <- funList %>% length
  ### Remove intermediate values
  rm("newFunList", "newFunNames", "refFunList", "refFunNames")

  ###### ** Scaled Impacts: Values ######
  if(funLength){
    ### Create temperature scenario
    df_temps    <- tibble(temp_C = -1:11)
    ### Execute the impact functions across new sectors, then gather values
    df_vals     <- df_temps %>% map_df(~ funList %>% map_df(exec,.x))
    ### Add temperatures
    df_vals     <- df_vals  %>% mutate(temp_C = df_temps$temp_C)
    ### Gather values
    idCols0     <- c("temp_C")
    df_vals     <- df_vals  %>% gather(key="scenario_id",value="scaled_impact", -c(all_of(idCols0)))
    # %>% mutate( temp_C = rep(-1:11,length(scaled_impact)/length(df_temps$temp_C)))
    rm("df_temps")

    ### Separate scenario_id into components
    into0       <- c("sector", "variant", "impactYear", "impactType", "model_type", "model_dot", "region_dot")
    df_vals     <- df_vals %>% separate(col = scenario_id , into = c(all_of(into0)), sep = "_")
    ### Filter to new sector_id
    # df_vals0    <- df_vals0 %>% filter(sector==sector_id)
    # df_vals0    <- df_vals0 %>% rename_at(.vars=c("sector"), ~sector_id)

    ### Arrange and add scaled impacts to list of items to save
    arrange0    <- c("sector", "variant", "impactYear", "impactType", "model_type", "model_dot", "region_dot")
    df_vals     <- df_vals %>% arrange_at(.vars=c(all_of(arrange0)))
    saveList[[c_impact0]] <- df_vals
    rm("arrange0")

    ### Add worksheet and write data table if(save)
    if(save) {
      sheet0 <- c_impact0
      wbook0 %>% addWorksheet(sheetName = sheet0)
      wbook0 %>% writeDataTable(sheet = sheet0, df_vals)
    } ### End if(save)
  }

  ###### ** Scaled Impacts: Plots ######
  if(funLength){
    ### Get unique sectors
    sectors0    <- df_vals$sector %>% unique
    ### Regional plots
    plots0      <- sectors0 %>% lapply(function(
    sector_i,
    data_i = df_vals %>% filter(sector==sector_i)
    ){
      paste0("Plotting scaled impacts for sector \'", sector_i, "\'", "...") %>% message
      ### Get unique variants
      variants_i <- data_i$variant %>% unique
      ### Get unique models
      models_i   <- data_i$model_dot %>% unique
      nModels_i  <- models_i %>% length
      ### Get unique impact types
      impTypes_i <- data_i$impactType %>% unique
      nImp_i     <- impTypes_i %>% length
      # nVariants  <- variants_i %>% length
      plots_i    <- variants_i %>% lapply(function(
    variant_j,
    data_j = data_i %>% filter(variant==variant_j)
      ){
        ### yColumn
        yCol_j   <- "scaled_impact"
        ### Initialize scalar labels
        lvl_y0   <- 10**c(0, 3, 6, 9)
        lbl_y0   <- c("") %>% c(paste0(", ", c("Thousands", "Millions", "Billions")))
        ### Get maximum y value, get scalar, label for y
        max_y0   <- data_j[[yCol_j]] %>% abs %>% max(na.rm=T)
        scale_y  <- max_y0 %>% (function(x){case_when(
          x > lvl_y0[4] ~ lvl_y0[4],
          x > lvl_y0[3] ~ lvl_y0[3],
          x > lvl_y0[2] ~ lvl_y0[2],
          TRUE ~ 1
        )})

        ###### Adjust data
        data_j = data_j %>% mutate_at(.vars=c(all_of(yCol_j)), function(x){x / scale_y})

        ###### Plot values
        ### X label, breaks
        lab_x0   <- expression("CONUS Degrees of Warming ("*~degree*C*")")
        brk_x0   <- (-1:6)*2
        ### Y label, breaks
        unit_y0  <- scale_y %>% factor(lvl_y0, lbl_y0) %>% as.character
        lab_y0   <- yCol_j %>% str_split(pattern="_") %>% unlist %>% paste(collapse=" ") %>% str_to_title
        lab_y0   <- paste0("(", lab_y0, unit_y0, ")")
        # brk_yrs0  <- seq(2010, 2300, by=20)

        ### Initialize plot list
        plots_j  <- list()

        ### Regional plots
        ### Groups
        groups_j <- c("sector", "variant", "impactYear", "impactType", "model_dot", "region_dot","temp_C")
        reg_j    <- data_j %>%
          group_by_at(.vars=c(all_of(groups_j))) %>%
          ggplot() +
          geom_line(aes(x=temp_C, y=scaled_impact, color=region_dot), alpha = 0.7)
        # ### Add facets
        # reg_j    <- reg_j + facet_wrap(facets=c("model_dot", "impactType"),scales = "free")
        reg_j    <- reg_j + facet_wrap(facets=c("impactType", "model_dot"), scales = "free", nrow=nImp_i)
        ### Add scales
        reg_j    <- reg_j + scale_x_continuous(lab_x0, breaks=brk_x0)
        reg_j    <- reg_j + scale_y_continuous(lab_y0)
        reg_j    <- reg_j + scale_color_discrete("Region")
        reg_j    <- reg_j + theme(legend.position = "bottom")
        ### Add title
        reg_j    <- reg_j + ggtitle(paste0(reg_j$data$sector %>% unique, " -- ","Regional"))
        ### Add regional plot
        plots_j[["regional"]] <- reg_j
        rm("reg_j", "groups_j")

        ### National plots
        # plots_j[["national"]]
        ### Regional plots
        ### Groups
        groups_j <- c("sector", "variant", "impactYear", "impactType", "model_dot","temp_C")
        nat_j    <- data_j %>%
          group_by_at(.vars=c(all_of(groups_j))) %>%
          summarize_at(.vars=c("scaled_impact"),.funs = sum) %>%
          ggplot() +
          geom_line(aes(x = temp_C, y = scaled_impact, color = model_dot), alpha = 0.7)

        ### Add facets
        nat_j    <- nat_j + facet_wrap(facets=c("impactType"),scales = "free", ncol=nImp_i)
        ### Add scales
        nat_j    <- nat_j + scale_x_continuous(lab_x0, breaks=brk_x0)
        nat_j    <- nat_j + scale_y_continuous(lab_y0)
        nat_j    <- nat_j + scale_color_discrete("Model/GCM")
        nat_j    <- nat_j + theme(legend.position = "bottom")
        ### Add title
        nat_j    <- nat_j + ggtitle(paste0(nat_j$data$sector %>% unique," -- ",nat_j$data$impactType %>% unique, " (Region Sums)"))
        ### Add regional plot
        plots_j[["national"]] <- nat_j
        rm("nat_j", "groups_j")

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

    ### Add plots to list of items to save
    saveList[[c_plots0]] <- plots0

    ### Iterate over each sector and add worksheet for each sector
    if(save){
      sectors0 %>% walk(function(sector_i, plots_i = plots0[[sector_i]]){
        ### Add worksheet
        sheet_i <- "plots" %>% paste0("_", sector_i)
        wbook0 %>% addWorksheet(sheetName = sheet_i)
        #wbook0 %>% writeDataTable(sheet = sheet_i,diff)

        ### Sector values
        models_i   <- df_vals    %>% filter(sector==sector_i) %>% select(c("model_dot" )) %>% unique %>% as.vector
        nModels_i  <- models_i   %>% length
        ### Impact type values
        impType_i  <- df_vals    %>% filter(sector==sector_i) %>% select(c("impactType")) %>% unique %>% as.vector
        nImp_i     <- impType_i  %>% length
        ### Get variants
        variants_i <- plots_i    %>% names
        nVar_i     <- variants_i %>% length

        ### Plot strings
        ftype_i  <- "png"
        units_i  <- "cm"
        ### Plot dimensions by unit
        cUnit     <- 6
        ### Regional
        widthR_i  <- cUnit * 3 * nModels_i + cUnit
        heightR_i <- cUnit * 2 * nImp_i    + cUnit
        # c(widthR_i, heightR_i) %>% print
        ### National
        widthN_i  <- widthR_i
        heightN_i <- cUnit * 2
        # c(widthN_i, heightN_i) %>% print

        ### Plot multipliers by columns
        htReg_i  <- 12 * nImp_i
        htNat_i  <- 20
        # c(htReg_i, htNat_i) %>% print

        ### Add regional plots
        for(j in 1:nVar_i){
          variant_j <- variants_i[j]
          plots_j   <- plots_i[[j]]

          ### Columns
          col_j   <- 1

          ### Regional plot
          # regPlot_j <- plots_j[[j]][["regional"]]
          regPlot_j <- plots_j[["regional"]]
          # regPlot_j <- regPlot_j + facet_wrap(facets=c("impactType", "model_dot"), scales = "free", nrow=nVar_i)
          rowReg_j  <- htReg_i * (j - 1) + htNat_i * (j - 1) + 2
          xyReg_j   <- col_j %>% c(rowReg_j)
          regPlot_j %>% print
          wbook0 %>% insertPlot(sheet = sheet_i, xy = xyReg_j, width = widthR_i, height = heightR_i, fileType = ftype_i, units = units_i)

          ### National plots
          natPlot_j <- plots_j[["national"]]
          # natPlot_j <- natPlot_j + facet_wrap(facets=c("impactType", "model_dot"), scales = "free", nrow=nVar_i)
          rowNat_j  <- rowReg_j + htReg_i + htNat_i + cUnit
          xyNat_j   <- c(col_j, rowNat_j)
          natPlot_j %>% print
          wbook0 %>% insertPlot(sheet = sheet_i, xy = xyNat_j, width = widthN_i, height = heightN_i, fileType = ftype_i, units = units_i)
          rm("natPlot_j", "rowNat_j", "xyNat_j")
          rm("regPlot_j", "rowReg_j", "xyReg_j")

          ### Delete intermediate values
        } ### End for(j in 1:nVar_i)
      }) ### End function, end walk
    } ### End if(save)
  } ### End if(funLength)

  ###### Save Workbook ######
  if(save){
    "Saving new sector results" %>% paste0("...") %>% message
    outDir    <- outPath %>% file.path("data_tests")
    outFile   <- outDir  %>% file.path(xslxName)
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
    return(saveList)
  } ### End return
  
} ### End function


###### End of Page ######


