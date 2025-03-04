##### HELPERS #####
load_frediDB <- function(){

  ### check if db has been unzipped
  zip <- system.file("extdata", "sysdata.zip", package="FrEDI")
  fredi_db <- system.file("extdata", "sysdata.db", package="FrEDI")

  unzip_check <- fredi_db == ""

  ## If needs to be unzipeed, un zip it
  if(unzip_check){
    unzip(zipfile = zip, exdir = system.file("extdata", package="FrEDI"))
    fredi_db <- system.file("extdata", "sysdata.db", package="FrEDI")
  }

  con <-  DBI::dbConnect(RSQLite::SQLite(), fredi_db)

  return(con)
}
##### Queries #####
