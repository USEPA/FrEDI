###### check_and_create_path ######
### Check and create path to save data object
check_and_create_path <- function(
    fpath     = ".",
    createDir = TRUE
){
  ### Check directory exists
  exists0   <- fpath |> dir.exists()
  ### Create directory if it doesn't exist
  create0   <- !exists0
  create1   <- create0 & createDir
  ### Create
  if(create1) {
    "Directory does not exist. " |> message()
    "\t" |> paste0("Creating directory...") |> message()
    fpath |> dir.create()
    "\t" |> paste0("...Directory created.") |> message()
  } ### End if(create1)
} ### End check_and_create_path

###### save_data ######
### Utility function to help save a data object
save_data <- function(
    obj0, ### Data object
    fpath     = ".", ### File path
    fname     = "data",
    ftype     = "csv", ### CSV or RData
    createDir = TRUE,  ### Create directory if it doesn't exist
    row.names = FALSE
){
  ### Directory name
  fdir      <- fpath; rm(fpath)
  ### Check directory and create it if it  doesn't exist
  createDir <- fdir |> check_and_create_path(createDir=createDir)
  ### File name
  ftype     <- ftype |> tolower()
  fname     <- fname |> paste0(".") |> paste0(ftype)
  fpath     <- fdir  |> file.path(fname)
  ### Which type to save
  doCsv     <- ftype == "csv"
  doRda     <- ftype %in% c("rda", "rdata", "rds")
  ### Message user
  "Writing data to " |> paste0(ftype, " file...") |> message()
  ### Save CSV
  if(doCsv){saved0 <- obj0 |> write.csv(file = fpath, row.names = row.names) |> try()}
  else     {saved0 <- obj0 |> save(file = fpath) |> try()}
  ### Message user
  "...Finished." |> message()
  ### Return
} ### End save_data

###### save_image ######
### Utility function to help save an image to file
save_image <- function(
    obj0, ### Data object
    fpath     = ".", ### File path
    fname     = "data",
    device    = "pdf", ### CSV or RData
    options   = list(
      height = 8,
      width  = 6.9,
      res    = 200,
      units  = "in"
    ),
    createDir = TRUE ### Whether to create directory if it doesn't exist
){
  ### Plot options
  dev0      <- device |> tolower()
  h0        <- options[["height"]]
  w0        <- options[["height"]]
  res0      <- options[["res"]]
  units0    <- options[["units"]]
  ### File name
  fdir      <- fpath
  fname     <- fname |> paste0(".") |> paste0(dev0)
  ### Create directory if it doesn't exist
  createDir <- fdir  |> check_and_create_path(createDir=createDir)
  fpath     <- fdir  |> file.path(fname)

  ### Save Image
  saved0    <- ggsave(fname, plot = obj0, device=dev0, path=fdir, width=w0, height=h0, units=units0)
  ### Return
} ### End save_image

###### save_appendix_figures ######
### Wrapper function to help save appendix figures to file
save_appendix_figures <- function(
    plotList,
    df0,      ### Dataframe used to create plots
    modelType = "GCM", ### Or SLR
    fpath     = ".",
    device    = "pdf",
    res       = 200,
    units     = "in",
    createDir = TRUE ### Whether to create directory if it doesn't exist
){
  ### Create directory if it doesn't exist
  fdir      <- fpath; rm("fpath")
  fdir      <- fdir |> file.path("images")
  created0  <- fdir  |> check_and_create_path(createDir=createDir)
  ### Prepare data
  df0       <- df0 |> filter(model_type == modelType)
  list0     <- plotList[[modelType]]
  ### Unique values
  names0    <- list0  |> names()
  sectors0  <- names0 |> map(function(.x){str_split(string=.x, pattern="_")[[1]][1]}) |> unlist() |> unique()
  refYears0 <- names0 |> map(function(.x){str_split(string=.x, pattern="_")[[1]][2]}) |> unlist() |> unique()
  # names0 |> print(); sectors0 |> print(); refYears0 |> print(); impYears0 |> print()

  ### Iterate over sectors
  names0 |> map(function(.x){
    ### Plot .x
    list_x    <- list0[[.x]]
    .x |> print(); #list_x |> names() |> print()
    # .x |> c(list_x |> names()) |> print()
    # "got here" |> print()
    ### Split name into sector and ref year
    sector_x  <- .x |> map(function(.y){str_split(string=.y, pattern="_")[[1]][1]}) |> unlist() |> unique()
    refYear_x <- .x |> map(function(.y){str_split(string=.y, pattern="_")[[1]][2]}) |> unlist() |> unique()
    fname_x   <- sector_x |> paste0("_refYear_", refYear_x)
    ### Filter to data
    df_x      <- df0 |> filter(sector == .x)
    ### Unique sector values
    # impYears0 <- list0[[1]] |> names()
    c_years <- list_x |> names()
    # c_years <- df_x[["impactYear"]] |> unique()
    c_types <- df_x[["impactType"]] |> unique()
    c_vars  <- df_x[["variant"   ]] |> unique()

    # c_types |> print(); c_types |> print(); c_vars |> print();
    ### Number of values
    n_years <- c_years |> length()
    n_types <- c_types |> length()
    n_vars  <- c_vars  |> length()
    ### Plot heights
    h_x     <- n_types * 2.5 + 2.5
    w_x     <- n_vars  * 3.0 + 1.5
    h_x     <- h_x * 4
    w_x     <- w_x * 4
    ### Plot options
    units_x <- units; #rm(units)
    res_x   <- res  ; #rm(res  )
    dev_x   <- device |> tolower()
    opts_x  <- list(
      height = h_x,
      width  = w_x,
      res    = res_x,
      units  = units_x
    ) ### End options
    # "got here" |> print()
    ### Iterate over impact years
    saved_x <- c_years |> walk(function(.y){
      .y |> print()
      plot_y  <- list_x[[.y]]
      fname_y <- fname_x |> paste0("_impYear_", .y, ".", dev_x)
      # "got here1" |> print()
      saved_y <- plot_y |> save_image(
        fpath     = fdir , ### File path
        fname     = fname_y,
        device    = dev_x,
        createDir = createDir,
        options   = opts_x
      ) ### End save_image
      # "got here2" |> print()
    }) ### End map(function(.y))
  }) ### End map(function(.z))
  ### Return
} ### End save_appendix_figures


###### save_fig7_images ######
### Save impacts by degree function
### Wrapper function to help save Fig 7 figures to file
save_fig7_images <- function(
    plotList,   ### Plot object
    modelType = "GCM", ### Or SLR
    fpath     = ".",
    device    = "pdf",
    units     = "in",
    createDir = TRUE ### Whether to create directory if it doesn't exist
){
  ### Create directory if it doesn't exist
  fdir      <- fpath; rm("fpath")
  createDir <- fdir  |> check_and_create_path(createDir=createDir)

  ### Other values
  type0  <- modelType |> tolower()
  ### Figure options
  h0     <- ("gcm" %in% type0) |> ifelse(9, 4.5)
  w0     <- 6.9
  dev0   <- device |> tolower()
  # type0 |> print(); ("gcm" %in% type0) |> print(); h0 |> print()

  ### Data info
  names0 <- plotList |> names()

  names0 |> walk(function(.x){
    plot_x <- plotList[[.x]]
    file_x <- .x |> paste0(".", device)
    ggsave(file_x, plot = plot_x, device=dev0, path=fdir, width=w0, height=h0, units=units)
  })
### Return
} ### End save_fig7_images
