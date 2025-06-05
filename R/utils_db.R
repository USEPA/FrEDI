##### HELPERS #####
load_frediDB <- function(){

  ### check if db has been unzipped
  zip <- system.file("extdata", "sysdata.zip", package="FrEDI")
  sys_db <- system.file("extdata", "sysdata", package="FrEDI")

  unzip_check <- sys_db == ""

  ## If needs to be unzipped, unzip it
  if(unzip_check){
    unzip(zipfile = zip, exdir = system.file("extdata", package="FrEDI"))
    fredi_db <- system.file("extdata","data","fredi","fredi_data", package="FrEDI")
    file.rename(fredi_db,
                file.path(system.file("extdata",package="FrEDI"),
                          "sysdata"))

    sys_db <- system.file("extdata","sysdata", package="FrEDI")
    unlink(file.path(system.file("extdata", package="FrEDI"),"data"), recursive = TRUE)
  }

  con <-  DBI::dbConnect(RSQLite::SQLite(), sys_db)
  
  return(con)
}

##### Queries #####
