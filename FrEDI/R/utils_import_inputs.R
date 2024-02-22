###### fun_tryInput ######
### Created 2021.02.08. Last updated 2022.05.09
### This function attempts to load a user-specified input file
fun_tryInput <- function(
  filename = NULL,
  silent   = FALSE, ### Whether to message
  msg0      = "" ### Message prefix
){
  ###### Messaging ######
  msgUser <- ifelse(silent, FALSE, TRUE)
  msg0 <- ifelse(is.null(msg0), "", msg0)
  msg1 <- msg0 |> paste0("\t")
  msg2 <- msg1 |> paste0("\t")
  msg3 <- msg2 |> paste0("\t")

  ###### Initialize results ######
  return_list <- list()

  ###### Defaults ######
  ### Check if the file exists and try to load the file
  ### Set input to null if it doesn't exist
  if(!is.null(filename)){
    fileExists  <- filename |> file.exists()
    ### If the file exists, try loading the file and then check the class of the result
    if(fileExists){
      fileInput   <- try(filename |> read.csv(check.names = F), silent=T)
      classInput  <- fileInput |> class()
      inputExists <- ("data.frame" %in% classInput)

      ### If loading the inputs was successful
      if(inputExists){
        return_list[["fileInput"]]  <- fileInput
        return_list[["fileStatus"]] <- "loaded"
        return_list[["fileMsg"]]    <- "Inputs loaded."
      } else{
        return_list[["fileStatus"]] <- "other-error"
        return_list[["fileMsg"]]    <- "File exists but could not load the input file."
      }


    }  else{
      return_list[["fileStatus"]] <- "no-file"
      return_list[["fileMsg"]]    <- "Input file does not exist."
    }

    ### Message the user
    # message("\t", return_list[["fileMsg"]])
    if(msgUser){ msg0 |> paste0(return_list[["fileMsg"]]) |> message() }
  }

  ###### Return ######
  return(return_list)
}


###### rename_inputs ######
### Created 2021.02.08. Last updated 2021.02.08
rename_inputs <- function(
  data,
  new_names
){
  ### Get the length of the new names
  data_names   <- data |> names()
  num_names    <- new_names |> length()
  num_dataCols <- data |> ncol()

  if(num_dataCols>num_names){
    data <- data[,1:num_names]
  }

  names(data) <- new_names

  return(data)

}


###### check_inputs ######
### Check Input Ranges
### If input range is outside the range, return "flag" and row numbers of flagged values
### If input range is all inside the range, return "allgood"
check_inputs <- function(
  x,           ### Data column
  xmin = NULL, ### Minimum of range
  xmax = NULL  ### Maximum of range
){
  ###### Initialize Values ######
  ### Initialize flagged rows
  maxFlagRows <- minFlagRows <- c()

  ###### Check Minimum Value ######
  ### Which observations are below the minimum
  if(!is.null(xmin)) minFlagRows <- which(x < xmin)

  ###### Check Maximum Value ######
  ### Which observations are below the minimum
  if(!is.null(xmax)) maxFlagRows <- which(x > xmax)

  ###### Combine Any Flagged Rows ######
  flagRows   <- c(minFlagRows, maxFlagRows)

  ###### Summarize Flag Status ######
  has_flags  <- length(flagRows) > 0
  # flagStatus <- ifelse(has_flags, "flag", "allgood")

  ###### Return List ######
  flagList   <- list(flagged = has_flags, rows = flagRows)
  return(flagList)
}
