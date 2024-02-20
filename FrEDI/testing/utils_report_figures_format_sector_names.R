###### format_sectorNames ######
### Aggregate temperature scenario
format_sectorNames <- function(
    names0, ### Sector names
    thresh0 = 18
){
  names1 <- names0 |> map(fun_getLineBreaks, thresh=thresh0)
  names1 <- names1 |> unlist() |> trimws()
  names2 <- names1 |> standardize_lines()
  return(names2)
} ### End format_sectorNames()

###### fun_noStringValue ######
### Check for missing string value
fun_hasStringValue <- function(
    str0 = NULL
){
  ### Check if null
  isNull0 <- is.null(str0)
  naStr0  <- (!isNull0) |> ifelse(str0 |> is.na(), TRUE)
  hasStr0 <- (!(isNull0 & naStr0)) |> ifelse(!(str0==""), FALSE)
  ### Return
  return(hasStr0)
} ### End fun_hasStringValue

###### check_str1_in_str2 ######
### Function to check if one string is present in another
check_str2_in_str1 <- function(
    string =NULL,
    pattern=NULL,
    default=TRUE
){
  ### Check that string values exist
  hasStr1  <- fun_hasStringValue(str0=string)
  hasStr2  <- fun_hasStringValue(str0=pattern)
  hasBoth  <- hasStr1 & hasStr2
  # bothChar <-
  ### Check value
  if(hasBoth){
    # string |> print(); pattern |> print()
    match0  <- str_match(string=string, pattern=pattern)[1][1]
    isNa0   <- match0 |> is.na()
    match0  <- !isNa0
  } ### End if(hasBoth)
  else{match0 <- default}
  ### Return
  return(match0)
} ### End check_str2_in_str1

###### fun_compareStrings() ######
### This function compares strings and is used with the function fun_getLineBreaks
fun_compareStrings <- function(
    new_x  = NULL,
    sub_x  = NULL,
    word_x = NULL,
    next_x = NULL,
    thresh = 16
){
  ###### Defaults ######
  # if(is.null(thresh)){thresh <- 16}
  ###### Check values ######
  no_new_x  <- !(new_x  |> fun_hasStringValue())
  no_sub_x  <- !(sub_x  |> fun_hasStringValue())
  no_word_x <- !(word_x |> fun_hasStringValue())
  # no_sub_x |> print()

  ### Check if word is in new
  cond1     <- no_sub_x
  cond2     <- !no_sub_x & no_word_x
  # cond2     <- !no_sub_x & !no_word_x
  if     (cond1){
    sub_x       <- word_x
    wordInSub_x <- TRUE
  } ### End if (cond1)
  else if(cond2){
      sub_x       <- sub_x
      wordInSub_x <- TRUE
  } ### End else if(cond2)
  else          {
    wordInSub_x <- check_str2_in_str1(string=sub_x, pattern=word_x)
    if(!wordInSub_x){
      sub_x       <- c(sub_x, word_x) |> paste(collapse=" ")
      wordInSub_x <- TRUE
    } ### End if(!wordInSub_x)
    else             {wordInSub_x <- FALSE}
  } ### End else
  rm(cond1, cond2)

  ### Check values again
  no_new_x  <- !(new_x  |> fun_hasStringValue())
  no_sub_x  <- !(sub_x  |> fun_hasStringValue())
  no_word_x <- !(word_x |> fun_hasStringValue())

  ### Check if sub_x is in new_x
  cond1     <- no_new_x & no_sub_x
  cond2     <- no_new_x & !no_sub_x
  if     (cond1){subInNew_x <- TRUE}
  else if(cond2){subInNew_x <- FALSE}
  else          {subInNew_x <- check_str2_in_str1(string=new_x, pattern=sub_x)}
  rm(cond1, cond2)

  ### If subInNew_x, shift things forward and recheck values
  if(subInNew_x){
    new_x  <- new_x
    sub_x  <- NULL
  } ### End if(subInNew_x)
  ### Check values again
  no_new_x  <- !(new_x  |> fun_hasStringValue())
  no_sub_x  <- !(sub_x  |> fun_hasStringValue())
  no_word_x <- !(word_x |> fun_hasStringValue())

  ### Check if word is in new
  cond1     <- no_new_x
  cond2     <- !no_new_x & no_word_x
  if     (cond1){
    new_x       <- new_x
    wordInNew_x <- FALSE
  } ### End if(cond1)
  else if(cond2){
    word_x      <- NULL
    new_x       <- new_x
    wordInNew_x <- TRUE
  } ### End else if(cond2)
  else          {
    ### Check if word is in new
    new_x       <- new_x
    word_x      <- word_x
    wordInNew_x <- check_str2_in_str1(string=new_x, pattern=word_x)
  } ### End else
  rm(cond1, cond2)

  ### Check values
  no_new_x  <- !(new_x  |> fun_hasStringValue())
  no_sub_x  <- !(sub_x  |> fun_hasStringValue())
  no_word_x <- !(word_x |> fun_hasStringValue())
  # c("new_x: ", new_x, ", sub_x: ", sub_x, ", word_x: ", word_x) |> paste(collapse="") |> print()

  #### What to do if wordInNew_x versus not
  cond1     <- wordInNew_x
  if(cond1){
    new_x       <- new_x
    sub_x       <- NULL
    word_x      <- NULL
  } ### End if(cond1)
  else{
    ### Check if the sub is in x
    if(no_sub_x){
      subInNew_x  <- T
      wordInSub_x <- F
    } ### End if(no_sub_x)
    else if(no_new_x){
      subInNew_x  <- F
      wordInSub_x <- check_str2_in_str1(string=sub_x, pattern=word_x)
    } ### End else if(no_sub_x)
    else{
      subInNew_x  <- check_str2_in_str1(string=new_x, pattern=sub_x)
      wordInSub_x <- check_str2_in_str1(string=sub_x, pattern=word_x)
    } ### End else(no_sub_x)
    ### What to do about sub_x
    if(subInNew_x){
      if(wordInSub_x){
        new_x       <- new_x
        sub_x       <- NULL
        word_x      <- NULL
        sub_x       <- next_x
      } ### End if(subInNew_x)
      else{
        new_x       <- new_x
        sub_x       <- word_x
        word_x      <- NULL
      } ### End else(wordInSub_x)
    } ### End if(subInNew_x)
    else{
      if(wordInSub_x){
        sub_x       <- sub_x
        word_x      <- word_x
      } ### End if(wordInSub_x)
      else           {
        sub_x       <- c(sub_x, word_x) |> paste(collapse = " ")
        word_x      <- word_x
      } ### End else(wordInSub_x)
    } ### End else(subInNew_x)

  ### Check values
  no_new_x  <- !(new_x  |> fun_hasStringValue())
    no_sub_x  <- !(sub_x  |> fun_hasStringValue())
    no_word_x <- !(word_x |> fun_hasStringValue())

    ### Add word_x and check length
    tmp_x     <- c(sub_x, next_x) |> paste(collapse=" ")
    # no_tmp_x  <- ifelse(is.null(tmp_x), T, ifelse(tmp_x  == "", T, F))
    no_tmp_x  <- !(tmp_x  |> fun_hasStringValue())

    if(no_tmp_x){tmp_char <- 0}
    else        {tmp_char <- tmp_x |> nchar()}
    tmp_diff <- thresh - tmp_char

    cond_1    <- tmp_diff >= 0 & tmp_diff <= 1
    cond_2    <- tmp_diff < 0
    if(cond_1){
      new_x  <- c(new_x, tmp_x) |> paste(collapse="\n")
      sub_x  <- NULL
      word_x <- NULL
    } ### End cond_1
    else if(cond_2){
      new_x  <- c(new_x, sub_x) |> paste(collapse="\n")
      sub_x  <- next_x
      word_x <- NULL
    } ### End else if(cond_1)
    else{
      new_x  <- new_x
      sub_x  <- tmp_x
      word_x <- NULL
    } ### End else(cond_1)
  }  ### End else(cond1)

  ### Return
  return_list <- list(
    new  = new_x,
    sub  = sub_x,
    word = word_x
  )
  # return_list |> print()
  return(return_list)
} ### End fun_compareStrings

###### fun_getLineBreaks ######
### Good for ED&S, not for ET & D or HTFT
### This function splits a sector name into lines based on a specified space and threshold number of characters
fun_getLineBreaks <- function(
    x, ### Character string to split
    split = " ", ### Character string to split on
    thresh = 16, ### Max size of character string
    silent = T
){
  ###### Defaults ######
  ###### Get initial info about string
  nchar_x      <- x |> nchar()

  ###### Find info about the split character
  ### Get number of characters
  ### Find instances of the split character
  ### Find number of instances of the split string
  splitPos0   <- gregexpr(pattern = split, x) |> unlist()#; findSplit0
  numSplit0   <- splitPos0 |> length() ### ; numSplit0
  numWords0   <- numSplit0 + 1 ##; numWords0
  hasSplit0   <- numSplit0 > 0

  ###### Return word if no splits found
  x_new <- x
  if(hasSplit0){
    ###### Create a data frame with info ######
    ### Order of words, start and stop position, number of characters
    order0     <- 1:numWords0
    start0     <- 1 |> c(splitPos0 + 1)
    end0       <- c(splitPos0 - 1) |> c(nchar_x)
    ### Data frame
    df_x       <- tibble(word0 = x, order = order0, start=start0, end=end0)
    df_x       <- df_x |> mutate(word1  = word0 |> substr(start=start0, stop=end0))
    df_x       <- df_x |> mutate(nChar  = word1 |> nchar())
    # df_x |> print()
    ####### Figure out where to put new line characters ######
    ### x_new is new x, x_sub is temporary combination
    ### Initialize x_sub and x_new
    x_new <- NULL
    x_sub <- NULL

    ### Iterate over number of words
    for(i in 1:numWords0){
      ### Get new word and number of characters
      word_i <- df_x[["word1"]][i]
      next_i <- NULL
      last_i <- i == numWords0
      ### Iterate until i < numWords0
      if(!last_i){next_i  <- df_x[["word1"]][i + 1]}

      compareStr_i <- fun_compareStrings(
        new_x  = x_new,
        sub_x  = x_sub,
        word_x = word_i,
        next_x = next_i,
        thresh = thresh
      )

      x_new <- compareStr_i[["new"]]
      x_sub <- compareStr_i[["sub"]]

      ### Clear out sub
      isNullSub <- is.null(x_sub)
      if(last_i & !isNullSub){
          x_sep <- " "
          x_tmp <- c(x_new, x_sub) |> paste(collapse=x_sep)
          n_tmp <- x_tmp |> nchar()
          if(n_tmp > thresh){x_sep <- "\n"}
          x_new <- c(x_new, x_sub) |> paste(collapse=x_sep)
      } ### End if(last_i & !isNullSub)
    } ### End for(i in 1:numWords0)

    return(x_new)
  } ### End if(hasSplit0)

  return(x_new)
} ### End fun_getLineBreaks

###### paste_newLines ######
### Function to paste a string a certain number of new lines
repCollapse <- function(
    num0     = 1,    ### Number of times to repeat it
    str0     = "\n", ### String to repeat
    collapse = ""    ### String to use to collapse
){
  ### Replicate string
  str0 <- str0 |> rep(num0)
  ### Paste/collapse string
  str0 <- str0 |> paste(collapse=collapse)
  ### Return
  return(str0)
} ### End paste_newLines

###### get_newSectorLine ######
###### Get new sector line
get_newSectorLine <- function(
    index0 = 1,
    df0, ### df_words
    maxLines0 ### Max number of lines
    ){
  ### Number of lines
  lines_i   <- df0[["newlines"]][index0]
  word_i    <- df0[["word"    ]][index0]
  # df0[index0, ] |> print()
  add_i     <- maxLines0 - lines_i
  ### Whether there are lines to add
  has_i     <- add_i > 0
  # word_i |> print(); lines_i |> print(); add_i |> print()

  ### New word
  if(has_i){
    ### Lines to add
    str_i   <- add_i |> repCollapse()
    ### New string
    word_i  <- str_i |> paste0(word_i)
  } ### End else
  # ### New word
  # if(has_i){new_i <- word_i}
  # else     {
  #   # add_i <- rep("\n", add_lines) |> paste(collapse="")
  #   # new_word_i <- word_i |> paste0(add_line_i)
  #   mod_i   <- add_i %% 2
  #   ### Divisor
  #   div_i0  <- (add_i / 2) |> floor()
  #   ### Number of spacer lines to paste before & after
  #   ### Add most new lines before
  #   num_i1  <- (mod_i==0) |> ifelse(div_i, div_i + 1)
  #   num_i2  <- div_i
  #   ### Lines to add
  #   str_i1  <- num_i1 |> repCollapse()
  #   str_i2  <- num_i2 |> repCollapse()
  #   ### New string
  #   word_i  <- str_i1 |> paste0(word_i, str_i2)
  # } ### End else

  # word_i|> print()
  return(word_i)
} ### End get_newSectorLine

###### standardize_lines ######
### This function standardizes lines breaks
standardize_lines <- function(
    x, ### Character vector of words
    newline = "\n" ### Character string to split on
){
  ###### Defaults ######
  if(is.null(newline)){ newline <- "\n"}

  ###### Info about data ######
  num_words  <- x |> length()

  ###### Find info about the number of lines ######
  c_numLines <- x |> map(function(y){
    ### Get number of new lines
    y_findLines  <- gregexpr(pattern = newline, y) #; x_findLines
    y_numLines   <- y_findLines[[1]] |> length() ### ; numSplit0

    y_numLines   <- ifelse(y_findLines[[1]][1] == -1, 0, y_numLines)
    return(y_numLines)
  }) |> unlist()

  ### Maximum number of lines
  max_numLines <- c_numLines |> max(na.rm=T)

  # ###### Dataframes of words ######
  df_words <- tibble(word = x, newlines = c_numLines)
  # df_words |> print(); num_words |> print(); max_numLines |> print()
  new_words <- 1:num_words |> map(function(i){
    x_i <- i |> get_newSectorLine(
      df0       = df_words,
      maxLines0 = max_numLines
    )
    # x_i|> print()
    return(x_i)
  }) |> unlist()
  ### Return
  return(new_words)
} ### End standardize_lines

