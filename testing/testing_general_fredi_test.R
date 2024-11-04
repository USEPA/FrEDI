
###### General Output Tests ######
### Output test for Main FrEDI
general_fredi_test <- function(
    newOutputs,
    refOutputs,
    outPath   = ".",
    xlsxName  = "fredi_general_testResults.xlsx",
    save      = TRUE,
    return    = TRUE,
    overwrite = TRUE
){
  ###### Initialize Lists ######
  save_list   <- list()
  data_list   <- list()

  #### Data Table Names ######
  ### Data names
  c_ref0      <- "refData"
  c_new0      <- "newData"
  ### Names of objects to save
  c_genTest0  <- "dataInfo_test"
  c_diff0     <- "outputs_diffs"
  c_sectorNA0 <- "sector_NA"

  #### Sector NA Check ####
  #### Check if each sector has at least one non-NA-value
  ### For each sector group, check if each sector has nonNA values
  sect_na_check <- newOutputs |> has_nonNA_values_df(groups0="sector", col0="annual_impacts")

  ### If there are any sector groups with all NA values, filter to that sector and save to output list
  has_no_nonNA_values <- (sect_na_check |> nrow()) > 0
  if(has_no_nonNA_values) {
    save_list[[c_sectorNA0]] <- sectorNA
  } else {
    save_list[[c_sectorNA0]] <- tibble("All Sectors have some non-missing and non-zero values")
  } ### End else


  ###### Load Reference Data ######
  # ### Load previous Default Data to compare to new outputs
  # ### 0. Names for reference data
  # ### 1. Create new environment
  # ### 2. Load data into new environment
  # ### 3. Remove intermediate objects
  # dName0      <- "defaultResults" ### Name of data to load
  # dPkg0       <- "FrEDI"          ### Name of package to load data from (i.e., FrEDI)
  # newEnv      <- new.env()
  # expr0       <- substitute(data(d, package=dPkg0, envir=newEnv), list(d=dName0))
  # expr0 |> eval()
  # refOutputs  <- dName0 |> get(envir=newEnv, inherits = F)
  # rm(newEnv)
  # # refOutputs |> glimpse()


  ###### Add Data to List ######
  ### Create a list of new data and reference data
  data_list[[c_ref0]] <- refOutputs
  data_list[[c_new0]] <- newOutputs

  ###### General Dimension Checks ######
  ### Get general dimension test and add to save list
  df_dataInfo <- data_list |> dataInfo_test(save=F, return=T)
  save_list[[c_genTest0]] <- df_dataInfo

  ###### Compare Tables #######
  ### Get anti-join between the two tables: Join using all names
  ### Add table to save_list
  # join0     <- newOutputs |> names()
  join0     <- newOutputs |> names() |> get_matches(y=refOutputs |> names())
  df_diffs1 <- newOutputs |> anti_join(refOutputs, by=c(join0)) |> mutate(antiJoin = "New to Ref")
  df_diffs2 <- refOutputs |> anti_join(newOutputs, by=c(join0)) |> mutate(antiJoin = "Ref to New")
  df_diffs  <- df_diffs1 |> rbind(df_diffs2)
  save_list[[c_diff0]] <- df_diffs
  rm(join0, df_diffs1, df_diffs2)

  ###### Create Excel Workbook ######
  ### Create workbook if save=T
  ### Iterate over names in save_list, adding worksheets and saving tables
  if(save) {
    ### Create workbook
    wbook0  <- createWorkbook()
    ### Names
    c_save0 <- save_list |> names()
    ### Iterate over items in save_list: add worksheets and save tables
    for(name_i in c_save0) {
      wbook0 |> addWorksheet(sheetName=name_i)
      wbook0 |> writeDataTable(sheet=name_i, x=save_list[[name_i]], tableName=name_i)
    } ### End for(name_i in c_save0)
  } ### End if(save)


  ###### Save Workbook ####
  if(save){
    "Saving output test results" |> paste0("...") |> message()
    # outDir    <- outPath |> file.path("data_tests")
    outDir    <- outPath
    outFile   <- outDir  |> file.path(xlsxName)
    ### Check if outDir exists and, if not, create one
    odExists  <- outDir  |> dir.exists()
    if(!odExists){outDir |> dir.create(showWarnings = F)}
    rm(odExists)
    ### Save the workbook
    wbook0 |> saveWorkbook(file=outFile, overwrite=overwrite)
    ### Remove workbook
    rm(wbook0)
  } ### End if(save)


  ## Return options ####
  if(return) {
    return(save_list)
  } ### End return

} ### End function
