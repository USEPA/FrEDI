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
  df_info   <- df_info %>% mutate_at(.vars=c(mutate0), as.numeric)
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
### Function to initialize plots list
initialize_gen_plotsList <- function(x=1){
  list0 <- list()
  # list0[["temp"]] <- list(plotName="Temp Plot", xy=c(1 , 2 ))
  # list0[["slr" ]] <- list(plotName="SLR Plot" , xy=c(12, 2 ))
  # list0[["gdp" ]] <- list(plotName="GDP Plot" , xy=c(1 , 25))
  # list0[["pop" ]] <- list(plotName="Pop Plot" , xy=c(12, 25))
  list0[["temp"]] <- list(plotName="Temp Plot", xy=c(1, 2))
  list0[["slr" ]] <- list(plotName="SLR Plot" , xy=c(1, 2))
  list0[["gdp" ]] <- list(plotName="GDP Plot" , xy=c(1, 2))
  list0[["pop" ]] <- list(plotName="Pop Plot" , xy=c(1, 2))
  return(list0)
}
### Configuration plots function

### Add general configuration plot to workbook
add_gen_plot <- function(
    wbook, 
    # sheet     = 1,
    # wbook     = openxlsx::createWorkbook(),
    outPath   = ".",
    plotName  = "temp",
    plotsList = initialize_gen_plotsList()
){
  ###### Info
  plotNames <- c("temp", "slr", "gdp", "pop")
  # plotNames <- plotsList %>% names()
  
  ### Plot info
  path0   <- outPath
  fType0  <- "png"
  units0  <- "in"  
  dpi0    <- 200
  width0  <- 6.0
  height0 <- 4.5
  
  list0  <- plotsList[[plotName]]
  plot0  <- list0[["plot"]]
  xy0    <- list0[["xy"]]
  # plotsList %>% names %>% print; 
  # list0 %>% names %>% print; 
  # xy0 %>% print 
  # c(sheet0, fType0, units0, sheet0, xy0[1], xy0[2], width0, height0, units0)  %>% print
  file0  <- "tmp_" %>% paste0(plotName, ".png")
  fpath0 <- path0  %>% file.path(file0)
  # fpath0 %>% print(); plot0 %>% print()
  
  ### Add worksheet
  # sheet0  <- sheet; rm("sheet")
  sheet0 <- list0[["plotName"]]
  wbook %>% addWorksheet(sheetName = sheet0)
  # sheet0 %>% print
  
  ### Temporarily Save Plots
  ggsave(
    filename = file0, 
    path     = path0, 
    plot     = plot0, 
    device   = fType0, 
    dpi      = dpi0,
    width    = width0, 
    height   = height0, 
    units    = units0
  )
  ### Add plots to workbook
  insertImage(
    wb       = wbook,
    sheet    = sheet0,
    file     = fpath0,
    startCol = xy0[1],
    startRow = xy0[2],
    width    = width0,
    height   = height0,
    units    = units0
  )
  # ### Delete temporary file
  # fpath0 %>% file.remove()
  # rm("plotName", "plot0", "xy0", "file0", "path0", "fpath0")
  ### Return
  return(wbook)
} ### End: for plotName

### Configuration Test
general_config_test <- function(
    reshapedData   = NULL, ### List of reshaped data
    configuredData = NULL, ### List of configured data
    # reshapedFile   = "." %>% file.path("data_tests", "reshapedData_testResults.csv"), ### File name of reshaped data test
    outPath   = ".",
    xlsxName  = "generalConfig_testResults.xlsx",
    save      = TRUE,
    return    = TRUE,
    overwrite = TRUE, ### Whether to overwrite an existing file,
    fredi_config = NULL ### fredi_config list object
){
  ###### Create Workbook ######
  if(save){
    outDir    <- outPath %>% file.path("data_tests")
    outFile   <- outDir  %>% file.path(xlsxName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir  %>% dir.exists()
    if(!odExists){outDir %>% dir.create(showWarnings = F)}
    rm("odExists")
    
    ### Create Excel workbook
    wbook0    <- createWorkbook()
  } ### End if(save)
  
  ###### Initialize List ######
  ### Initialize list for saving values
  ### Initialize list for default plots
  saveList   <- list()
  listPlots  <- initialize_gen_plotsList()
  
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
  
  ###### Add Data to Excel Workbook ######
  ### Add Data to Excel workbook if save
  if(save) {
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
  lim_yrs0  <- c(2000, 2300)
  brk_yrs0  <- seq(lim_yrs0[1], lim_yrs0[2], by=20)
  
  ### Temp plot
  temp_plot <- configuredData[["temp_default"]] %>% 
    ggplot() +
    geom_line(aes(x = year, y = temp_C_conus)) +
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits = lim_yrs0) +
    scale_y_continuous(lab_tmp0) + 
    ggtitle("Default Temperature Scenario")
  ### SLR plot
  slr_plot  <- configuredData[["slr_default"]] %>% 
    ggplot() +
    geom_line(aes(x = year, y = slr_cm)) +
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits = lim_yrs0) +
    scale_y_continuous("SLR (cm)") + 
    ggtitle("Default SLR Scenario")
  ### GDP Plot: Convert to Billions
  gdp_plot <- configuredData[["gdp_default"]] %>% 
    mutate(gdp_usd = gdp_usd / 1e12) %>% 
    ggplot() +
    geom_line(aes(x = year, y = gdp_usd)) + 
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits = lim_yrs0) + 
    scale_y_continuous("U.S. National GDP (2015$, trillions)") + 
    ggtitle("Default GDP Scenario")
  ### Pop plot
  pop_plot <- configuredData[["pop_default"]] %>% 
    mutate(reg_pop = reg_pop / 1e6) %>% 
    ggplot() +
    geom_line(aes(x = year, y = reg_pop, color = region), alpha = 0.75) +
    scale_x_continuous(lab_yrs0, breaks=brk_yrs0, limits = lim_yrs0) + 
    scale_y_continuous("Regional Population (millions)") + 
    # theme(axis.text.x = element_text(angle=90)) +
    theme(legend.position = "bottom") +
    scale_color_discrete("Region") +
    ggtitle("Default Population Scenario")
  
  ### Add plots to list
  listPlots[["temp"]] <- list(plot=temp_plot) %>% c(listPlots[["temp"]])
  listPlots[["slr" ]] <- list(plot=slr_plot ) %>% c(listPlots[["slr" ]])
  listPlots[["gdp" ]] <- list(plot=gdp_plot ) %>% c(listPlots[["gdp" ]])
  listPlots[["pop" ]] <- list(plot=pop_plot ) %>% c(listPlots[["pop" ]])
  ### Add plot list to saveList
  saveList[[defPlots0]] <- listPlots
  
  
  ###### Add Plots to Excel Workbook ######
  if(save) {
    ### Add worksheet
    names0  <- listPlots %>% names
    # sheet0  <- defPlots0
    # wbook0 %>% addWorksheet(sheetName = sheet0)
    
    ### Add Plots
    for(name_i in names0){
      # wbook0 <- 
      add_gen_plot(
        plotsList = listPlots,
        plotName  = name_i,
        wbook     = wbook0,
        # sheet     = sheet0,
        outPath   = outDir
      )
    } ### End: for name_i
  } ### End if(save)
  
  
  ###### Save Option Outputs ######
  ### Save the workbook
  ### Remove workbook
  if(save){
    ### Save workbook
    "Saving data tests" %>% paste0("...") %>% message
    wbook0  %>% saveWorkbook(file=outFile, overwrite=overwrite)
    rm("wbook0")
    ### Remove temporary image files
    files0 <- outDir %>% list.files(pattern="tmp_", full.names = T)
    files0 %>% file.remove()
  } ### End if(save)
  
  ###### Return ######
  if(return) {
    return(saveList)
  } ### End if(return)
  
} ### End general_config_test

###### New Sector Plot Function ######
### Make regional or national variant plot
make_variant_plot  <- function(
    variant0,
    sector0,
    df0
){
  ### Scales
  # lab_x0 <- xScale[["lab"]]; brk_x0 <- xScale[["brk"]]; lim_x0 <- xScale[["lim"]]
  # lab_y0 <- yScale[["lab"]]; brk_y0 <- yScale[["brk"]]; lim_y0 <- yScale[["lim"]]
  ### Filter data
  df0      <- df0 %>% filter(sector  == sector0)
  df0      <- df0 %>% filter(variant == variant0)
  nImp0    <- df0[["impactType"]] %>% unique() %>% length()
  
  ### Get unique models & impact types
  models0  <- df0[["model_dot" ]] %>% unique
  impacts0 <- df0[["impactType"]] %>% unique
  ### Number of models & impacts
  nModels0 <- models_i %>% length
  nImp0    <- impacts_i %>% length
  # nVariants  <- variants_i %>% length
  
  ### Plot info
  title0   <- sector0 %>% paste0(" -- ","Regional")
  yCol0    <- "scaled_impact"
  groups0  <- c("sector", "variant", "impactYear", "impactType", "model_dot", "region_dot","temp_C")
  
  ### Initialize scalar labels
  lvl_y0   <- 10**c(0, 3, 6, 9)
  lbl_y0   <- c("") %>% c(paste0(", ", c("Thousands", "Millions", "Billions")))
  ### Get maximum y value, get scalar, label for y
  max_y0   <- df0[[yCol0]] %>% abs %>% max(na.rm=T)
  scale_y  <- max_y0 %>% (function(x){case_when(
    x > lvl_y0[4] ~ lvl_y0[4],
    x > lvl_y0[3] ~ lvl_y0[3],
    x > lvl_y0[2] ~ lvl_y0[2],
    TRUE ~ 1
  )})
  
  ###### Adjust data
  df0      <- df0 %>% mutate_at(.vars=c(yCol0), function(x){x / scale_y})
  
  ###### Plot values
  ### X label, breaks
  lab_x0   <- expression("CONUS Degrees of Warming ("*~degree*C*")")
  brk_x0   <- (-1:6)*2
  ### Y label, breaks
  unit_y0  <- scale_y %>% factor(lvl_y0, lbl_y0) %>% as.character
  lab_y0   <- yCol0 %>% str_split(pattern="_") %>% unlist %>% paste(collapse=" ") %>% str_to_title
  lab_y0   <- paste0("(", lab_y0, unit_y0, ")")
  # brk_yrs0  <- seq(2010, 2300, by=20)
  
  ### Plot
  plot0    <- df0 %>%
    group_by_at(.vars=c(groups0)) %>%
    ggplot() +
    geom_line(aes(x=temp_C, y=scaled_impact, color=region_dot), alpha = 0.7)
  # ### Add facets
  # plot0    <- plot0 + facet_wrap(facets=c("model_dot", "impactType"),scales = "free")
  plot0    <- plot0 + facet_wrap(facets=c("impactType", "model_dot"), scales = "free", nrow=nImp0)
  ### Add scales
  plot0    <- plot0 + scale_x_continuous(lab_x0, breaks=brk_x0, limits=NULL)
  plot0    <- plot0 + scale_y_continuous(lab_y0)
  plot0    <- plot0 + scale_color_discrete("Region")
  plot0    <- plot0 + theme(legend.position = "bottom")
  ### Add title
  plot0    <- plot0 + ggtitle(title0)
  
  ### Return
  return(plot0)
}
### Make list of plots for a sector
make_sector_plots <- function(
    sector0,
    df0
){
  paste0("Plotting scaled impacts for sector \'", sector_i, "\'", "...") %>% message
  ### Regional plots
  variants0   <- (df0 %>% filter(sector=sector0))[["variant"]] %>% unique
  plots0      <- variants0 %>% lapply(make_variant_plot(x, sector0=sector0, df0=df0))
  ### Add names
  names(plots0) <- variants0
  ### Return
  return(plots0)
}

#### Save sector plots
add_sector_plot <- function(
    wbook, 
    # sheet     = 1,
    # wbook     = openxlsx::createWorkbook(),
    sector0,
    variant0,
    outPath   = ".",
    df0, ### Data
    plotsList = list()
){
  ### Plot info
  fType0  <- "png"
  units0  <- "cm"   
  cUnit0  <- 6 ### Columns per unit
  col0    <- 1
  path0   <- outPath
  
  ### Sector values
  df0       <- df0    %>% filter(sector==sector_i)
  ### Unique models & impacts
  models0   <- df0[["model_dot"]] %>% unique
  impacts0  <- df0[["impactType"]] %>% unique
  ### Numbers of unique models & impacts
  nModels0  <- models0   %>% length
  nImp0     <- impacts0  %>% length
  ### Get variants
  variants_i <- plots_i    %>% names
  nVar_i     <- variants_i %>% length
  
  ### Heights & Widths
  ### Regional
  width0  <- cUnit0 * 3 * nModels0 + cUnit0
  height0 <- cUnit0 * 2 * nImp0    + cUnit0
  # c(widthR_i, heightR_i) %>% print
  
  ### Plot multipliers by columns
  # height0  <- 12 * nImp0
  
  ### Plot
  plot0    <- plotsList[["sector0"]][["variant0"]]
  
  ### Add worksheet
  sheet0   <- "plots" %>% paste(sector0, variant0, sep="_")
  wbook %>% addWorksheet(sheetName = sheet0)
  
  ### File names
  file0  <- "tmp_" %>% paste0(sector0, "_", variant0, ".png")
  fpath0 <- path0 %>% file.path(file_0)
  ### Temporarily Save Plots
  file_j %>% ggsave(plot=plot0, device=fType0, path=path0, width=width0, height=height0, units=units0)
  ### Add plots to workbook
  wbook %>% insertImage(sheet=sheet0, file=fpath0, startCol=2, startRow=1, width= width0, height=height0, units=units0)
  
  ### Iterate over each sector and add worksheet for each sector
  if(save){
    
    
    
    ### Iterate over sectors
    sectors0 %>% walk(function(sector_i, plots_i = plots0[[sector_i]]){
      ### Add worksheet
      sheet_i <- 
        #wbook0 %>% writeDataTable(sheet = sheet_i,diff)
        
        
        
        ### Add Plots
        for(j in 1:nVar_i){
          variant_j <- variants_i[j]
          ### Plots
          plot_j   <- plots_i[[j]]
          
          
          ### Delete temporary files
          pathReg_j %>% file.remove()
          ### Delete intermediate values
          rm("j", "variant_j", "plots_j")
          rm("regPlot_j", "rowReg_j", "fileReg_j", "pathReg_j")
          rm("natPlot_j", "rowNat_j", "fileNat_j", "pathNat_j")
        } ### End for(j in 1:nVar_i)
    }) ### End function(sector_i), end walk
  } ### End if(save)
} ### End if(funLength)

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
    newData   = NULL,
    refDataFile = "." %>% file.path("data", "sysdata.rda"),
    # sector_id = "",
    outPath   = ".",
    xslxName  = "newSectorsConfig_testResults.xlsx",
    save      = T,
    return    = T,
    overwrite = T
){
  ###### Create Workbook ######
  if(save){
    outDir    <- outPath %>% file.path("data_tests")
    outFile   <- outDir  %>% file.path(xlsxName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir  %>% dir.exists()
    if(!odExists){outDir %>% dir.create(showWarnings = F)}
    rm("odExists")
    
    ### Create Excel workbook
    wbook0    <- createWorkbook()
  } ### End if(save)
  
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
  df_status <- df_status %>% mutate_at(.vars=c(mutate0), factor, levels=levels0)
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
  newTests  <- newTests %>% rename_at(.vars=c(sum0), ~rename0)
  refTests  <- refTests %>% rename_at(.vars=c(sum0), ~rename0)
  ### Join old and new 
  df_tests  <- newTests %>% left_join(refTests, by=c(join0), suffix=suffix0)
  rm("join0", "sum0", "select0", "select1", "rename0"); rm("newTests", "refTests")
  
  ###### ** Join Tests and Test Info ######
  ### Join df_tests with df_status
  join0     <- c("Table.Name")
  rename0   <- c("table")
  ### Check number of rows before
  dim0      <- c(nrow(df_status), nrow(df_tests))
  ### Rename columns and join columns
  df_tests  <- df_tests  %>% rename_at(.vars=c(rename0), ~join0)
  df_status <- df_status %>% left_join(df_tests, by=c(join0))
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
  df_status <- df_status %>% arrange_at(.vars=c(arrange0))
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
    diff0  <- new0   %>% anti_join(ref0, by=c(join0))
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
    df_vals     <- df_vals %>% arrange_at(.vars=c(arrange0))
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
    sectors0    <- df_vals[["sector"]] %>% unique
    ### Regional plots
    listPlots0  <- sectors0 %>% lapply(make_sector_plots(x, df0 = df_vals))
    names(listPlots0) <- sectors0
    
    ### Add plots to list of items to save
    saveList[[c_plots0]] <- listPlots0
    
    ### Iterate over each sector and add worksheet for each sector
    if(save){
      for(sector_i in sectors0){
        variants0 <-  (df_vals %>% filter(sector==sector_i))[["variant"]] %>% unique
        for(variant_j in variants0){
          add_sector_plot(
            wbook     = wbook0, 
            sector0   = sector_i,
            variant0  = variant_j,
            outPath   = ".",
            df0       = df_vals, ### Data
            plotsList = listPlots0
          )
        } ### End for variant_j
      } ### End for sector_i
    } ### End if(save)
  } ### End if(funLength)
  
  ###### Save Workbook ######
  if(save){
    "Saving new sector results" %>% paste0("...") %>% message
    wbook0  %>% saveWorkbook(file=outFile, overwrite=overwrite)
    rm("wbook0")
  } ### End if(save)
  
  
  ## Return options ####
  if(return) {
    return(saveList)
  } ### End return
  
} ### End function


###### End of Page ######

