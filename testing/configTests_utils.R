### For iterating over list names in general config tests
###### General Values ######
### Calculating number of columns
fun_nCol <- function(
    z, ### Object name
    a, ### Object class
    b  ### List with objects
){
  # z |> print()
  ### Get values
  name0    <- z
  class0   <- a
  obj0     <- b
  ### Check if data frame
  do_df0   <- "data.frame" %in% class0
  ### Get numbers of columns or length of object
  nCol0    <- obj0 |> ncol() |> (function(x){(x |> is.null()) |> ifelse(NA, x)})()
  nCol0    <- case_when(do_df0 ~ nCol0, .default=obj0 |> length())
  return(nCol0)
} ### End fun_nCol

### Calculating number of rows
fun_nRow <- function(z, a, b){
  # z |> print()
  ### Get values
  name0    <- z
  class0   <- a
  obj0     <- b
  ### Check if data frame or list
  do_df0   <- "data.frame" %in% class0
  ### Get numbers of rows or length of object
  nRow0    <- obj0 |> nrow() |> (function(x){(x |> is.null()) |> ifelse(NA, x)})()
  nRow0    <- case_when(do_df0 ~ nRow0, .default=obj0 |> length())
  return(nRow0)
} ### End fun_nRow

### Calculating distinct rows or values
fun_nUnq <- function(z, a, b){
  ### Get values
  name0    <- z
  class0   <- a
  obj0     <- b
  ### Check if data frame or list
  do_df0   <- "data.frame" %in% class0
  do_list0 <- "list"       %in% class0
  ### What to do for data frames
  if     (do_df0  ) {nUnq0 <- obj0 |> distinct() |> nrow()}
  else if(do_list0) {nUnq0 <- obj0 |> names   () |> unique() |> length()}
  else              {nUnq0 <- obj0 |> unique  () |> length()}
  return(nUnq0)
} ### End fun_nUnq

# ### For iterating over list names
# ### Calculating number of columns
# fun_nCol <- function(z, a, b){
#   # z |> print()
#   # if_else("data.frame" %in% a[[z]], b[[z]] |> ncol(), 0 |> as.integer())
#   val1 <- "data.frame" %in% a[[z]]
#   val2 <- b[[z]] |> ncol()
#   val3 <- 0    |> as.integer()
#   val2 <- val2 |> is.null() |> ifelse(val3, val2)
#   # val1 |> print(); val2 |> print(); val3 |> print()
#   y    <- if_else(val1, true=val2, false=val3)
#   return(y)
# }
#
# ### Calculating number of rows
# fun_nRow <- function(z, a, b){
#   # z |> print()
#   # if_else("data.frame" %in% a[[z]], b[[z]] |> nrow(), b[[z]] |> length())
#   val1 <- "data.frame" %in% a[[z]]
#   val2 <- b[[z]] |> nrow()
#   val3 <- b[[z]] |> length()
#   val2 <- val2   |> is.null() |> ifelse(val3, val2)
#   # val1 |> print(); val2 |> print(); val3 |> print()
#   y    <- if_else(val1, true=val2, false=val3)
#   return(y)
# }
#
# ### Calculating distinct rows or values
# fun_nUnq <- function(z, a, b){
#   # z |> print()
#   # if_else("data.frame" %in% a[[z]], b[[z]] |> distinct() |> nrow(),  b[[z]] |> distinct() |> length())
#   class_z <- a[[z]]
#   obj_z   <- b[[z]]
#   do_df0  <- "data.frame" %in% class_z
#   ### Values
#   val1    <- do_df0
#   ### What to do for data frames
#   if(do_df0) {val2 <- obj_z |> distinct() |> nrow()}
#   else       {val2 <- obj_z |> unique()   |> length()}
#   # val1 |> print; val2 |> print; val3 |> print
#   val3 <- val2
#   y    <- if_else(val1, true=val2, false=val3)
#   return(y)
# }


###### All-NA Values ######
### Calculating columns with all NA vales
fun_allNA <- function(z, a, b){
  ### Get values
  name0    <- z
  class0   <- a
  obj0     <- b
  # name0 |> paste(class0, sep=", ") |> print()
  ### Check if data frame or list
  do_df0   <- "data.frame" %in% class0
  ### What to do for data frames
  if(do_df0) {nNna0 <- obj0 |> has_allNA_values_df()  }
  else       {nNna0 <- obj0 |> has_allNA_values_misc(a=class0)}
  return(nNna0)
} ### End fun_nNna

### Function to check if column has at least one non NA value
has_allNA_values_df <- function(df0) {
  ### Calculate number of rows
  nRow   <- df0 |> nrow()
  ### Check whether values in x are NA
  df0    <- df0 |> is.na()
  ### Number of NA values
  numNA  <- df0 |> colSums() |> nrow() |> (function(x){(x |> is.null()) |> ifelse(1, x)})()
  ### Whether all results are missing
  allNA  <- nRow == numNA
  ### Get number of rows %>%
  allNA  <- allNA |> which() |> length()
  ### Return
  return(allNA)
} ### End has_nonNA_values_df

### Function to check non NA values for other types
has_allNA_values_list <- function(list0) {
  names0    <- list0 |> names()
  hasNames0 <- !(names0 |> is.null())
  if(hasNames0) {y <- names0 |> map(~ list0[[.]] |> class()) |> unlist()}
  else          {y <- list0  |> map(~ . |> class()) |> unlist()}
  skip_y <- ("function" %in% y) | ("list" %in% y)
  # skip_y |> print()
  ### If y has functions
  if(skip_y) {
    allNA  <- FALSE
  } else  {
    isNA0 <- list0 |> map(~ . |> unlist() |> is.na()) |> unlist()
    # allNA |> head |> print()
    allNA <- isNA0    |> all(na.rm=TRUE)
    len0  <- list0 |> map(~ .y |> length()) |> unlist()
    allNA <- allNA & (len0 > 1)
  } ### End if(skip_y)
  ### Count NA values
  allNA   <- y |> which() |> length()
  ### Return
  return(allNA)
} ### End has_nonNA_values_misc

### Function to check non NA values for other types
has_allNA_values_misc <- function(b, a) {
  ### Get values
  class0   <- a
  obj0     <- b
  ### Check if list
  allNA    <- obj0 |> is.na() |> all(na.rm=TRUE)
  ### Which observations
  which0   <- allNA |> which()
  ### Count NA values
  allNA    <- which0 |> length()
  ### Return
  return(allNA)
} ### End has_nonNA_values_misc


###### Non-NA Values ######
### Calculating columns with all NA vales
fun_nNna <- function(z, a, b){
  ### Values
  val1 <- "data.frame" %in% a[[z]]
  ### What to do for data frames
  if(val1) {val2 <- b[[z]] |> has_nonNA_values()     }
  else     {val2 <- b[[z]] |> has_nonNA_values_misc()}
  val3       <- val2
  # if(!val1){val2 |> class() |> print(); val3 |> class() |> print()}
  y          <- if_else(val1, val2, val3)
  return(y)
}

### Function to check if column has at least one non NA value
has_nonNA_values <- function(
    x
) {
  ### Check whether values in x are NA
  x <- x |> is.na()
  ### Calculate number of rows
  y <- tibble(numRows = x |> nrow())
  ### Number of NA values
  y <- y |> mutate(numNA = x |> colSums() |> nrow() |> is.null() |> if_else(0, 1))
  ### Whether all results are missing
  y <- y |> mutate(allNA = (numRows == numNA))
  ### Filter to values with allNA
  y <- y |> filter(allNA)
  ### Get number of rows
  z <- y |> nrow()
  # z <- 1 * (z > 0)
  ### Return
  return(z)
} ### End has_nonNA_values_df

### Function to check if column has at least one non NA value
has_nonNA_values_df <- function(
    df0,
    groups0 = "sector",
    col0    = "annual_impacts"
) {
  ### Add counters
  df0   <- df0 |> mutate(numRows = 1)
  vals0 <- df0 |> pull(all_of(col0)) |> is.na()
  x     <- df0 |> mutate(numNA   = 1 * vals0)
  ### Summarize
  sum0  <- c("numRows", "numNA")
  y     <- x |> group_by_at(c(groups0)) |> summarize_at(c(sum0), sum)
  ### Whether all results are missing
  y     <- y |> mutate(allNA = (numRows == numNA))
  ### Filter to values with allNA
  z     <- y |> filter(allNA)
  ### Return
  return(z)
}

### Function to check non NA values for other types
has_nonNA_values_misc <- function(x) {
  class_x <- x |> class()
  len_x   <- x |> length()
  isList0 <- "list" %in% class_x
  if(isList0) {
    names_x    <- x |> names()
    hasNames_x <- !(names_x |> is.null())
    if(hasNames_x) {y <- names_x |> map(~ x[[.]] |> class()) |> unlist()}
    else           {y <- x       |> map(~ . |> class()) |> unlist()}
    skip_y <- ("function" %in% y) | ("list" %in% y)
    # skip_y |> print
    ### If y has functions
    if(skip_y) {
      y <- FALSE
    } else {
      y0 <- x |> map(~ . |> unlist() |> is.na()) |> unlist()
      # y0 |> head |> print
      y0 <- y0 |> all(na.rm=TRUE)
      y1 <- x |> map(~ .y |> length()) |> unlist()
      y  <- y0 & (y1 > 1)
    } ### End if(skip_y)
  } else {
    y <- x |> is.na() |> all(na.rm=TRUE)
  } ### End if(isList0)
  ### Which observations
  which_x <- y |> which()
  ### Count NA values
  z       <- y |> which() |> length()
  ### Return
  return(z)
}


###### End Script ######


