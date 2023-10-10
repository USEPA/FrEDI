# ## Function to check if column has at least one non NA value
# has_nonNA_values <- function(x) {
#   ### Check whether values in x are NA
#   x <- x %>% is.na
#   ### Calculate number of rows
#   y <- tibble(numRows = x %>% nrow)
#   ### Number of NA values
#   y <- y %>% mutate(numNA = x %>% colSums %>% nrow %>% if_else(is.null(.),0,.))
#   ### Whether all results are missing
#   y <- y %>% mutate(allNA = (numRows == numNA))
#   ### Filter to values with allNA
#   y <- y %>% filter(allNA)
#   ### Get number of rows %>%
#   z <- y %>% nrow
#   # z <- 1 * (z > 0)
#   ### Return
#   return(z)
# }

## Function to check if column has at least one non NA value
has_nonNA_values_df <- function(x) {
  ### Check whether values in x are NA
  x <- x %>% is.na
  ### Calculate number of rows
  y <- tibble(numRows = x %>% nrow)
  ### Number of NA values
  y <- y %>% mutate(numNA = x %>% colSums %>% nrow %>% is.null %>% if_else(.,0,1))
  ### Whether all results are missing
  y <- y %>% mutate(allNA = (numRows == numNA))
  ### Filter to values with allNA
  y <- y %>% filter(allNA)
  ### Get number of rows %>%
  z <- y %>% nrow
  # z <- 1 * (z > 0)
  ### Return
  return(z)
}

### Function to check non NA values for other types
has_nonNA_values_misc <- function(x) {
  class_x <- x %>% class
  len_x   <- x %>% length
  isList0 <- "list" %in% class_x
  if(isList0) {
    names_x    <- x %>% names
    hasNames_x <- !(names_x %>% is.null)
    if(hasNames_x) {y <- names_x %>% map(~ x[[.]] %>% class) %>% unlist}
    else           {y <- x       %>% map(~ . %>% class) %>% unlist}
    skip_y <- ("function" %in% y) | ("list" %in% y)
    # skip_y %>% print
    ### If y has functions
    if(skip_y) {y <- FALSE}
    else  {
      y0 <- x %>% map(~ . %>% unlist %>% is.na) %>% unlist
      # y0 %>% head %>% print
      y0 <- y0 %>% all(na.rm=TRUE)
      y1 <- x %>% map(~ .y %>% length) %>% unlist
      y  <- y0 & (y1 > 1)
    }
  } ### End if(isList0) 
  else        {y <- x %>% is.na %>% all(na.rm=TRUE)}
  ### Which observations
  which_x <- y %>% which
  ### Count NA values
  z       <- y %>% which %>% length
  ### Return
  return(z)
}

### For iterating over list names
### Calculating number of columns
fun_nCol <- function(z, a, b){
  # z %>% print
  # if_else("data.frame" %in% a[[z]], b[[z]] %>% ncol, 0 %>% as.integer)
  val1 <- "data.frame" %in% a[[z]]
  val2 <- b[[z]] %>% ncol
  val3 <- 0    %>% as.integer
  val2 <- val2 %>% is.null %>% ifelse(val3, val2)
  # val1 %>% print; val2 %>% print; val3 %>% print
  y <- if_else(val1, true=val2, false=val3)
  return(y)
}
### Calculating number of rows
fun_nRow <- function(z, a, b){
  # z %>% print
  # if_else("data.frame" %in% a[[z]], b[[z]] %>% nrow, b[[z]] %>% length)
  val1 <- "data.frame" %in% a[[z]]
  val2 <- b[[z]] %>% nrow
  val3 <- b[[z]] %>% length
  val2 <- val2   %>% is.null %>% ifelse(val3, val2)
  # val1 %>% print; val2 %>% print; val3 %>% print
  y <- if_else(val1, true=val2, false=val3)
  return(y)
}
### Calculating distinct rows or values
fun_nUnq <- function(z, a, b){
  # z %>% print
  # if_else("data.frame" %in% a[[z]], b[[z]] %>% distinct %>% nrow,  b[[z]] %>% distinct %>% length)
  class_z <- a[[z]]
  obj_z   <- b[[z]]
  do_df0  <- "data.frame" %in% class_z
  ### Values
  val1    <- do_df0
  ### What to do for data frames
  if(do_df0) {val2 <- obj_z %>% distinct %>% nrow}
  else       {val2 <- obj_z %>% unique %>% length}
  # val1 %>% print; val2 %>% print; val3 %>% print
  val3 <- val2
  y    <- if_else(val1, true=val2, false=val3)
  return(y)
}
### Calculating columns with all NA vales
fun_nNna <- function(z, a, b){
  # z %>% print
  # class_z <- a[[z]]
  # obj_z   <- b[[z]]
  # do_df0  <- "data.frame" %in% class_z
  # do_list <- "list"       %in% class_z
  ### Values
  # val1    <- do_df0
  val1 <- "data.frame" %in% a[[z]]
  ### What to do for data frames
  if(val1) {val2 <- b[[z]] %>% has_nonNA_values_df  }
  else     {val2 <- b[[z]] %>% has_nonNA_values_misc}
  # else if(do_list) {
  #   if(do_list){"got here3" %>% print}
  #   classes_z <- obj_z %>% map(~.%>% class) %>% unlist
  #   do_fun0   <- "function" %in% classes_z
  #   if(do_fun0) {val2 <- obj_z %>% length}
  #   else        {val2 <- obj_z %>% unique %>% length}
  # }
  val3       <- val2
  # if(!val1){val2 %>% class %>% print; val3 %>% class %>% print}
  y          <- if_else(val1, val2, val3)
  return(y)
}

