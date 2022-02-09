###### standardize_lines ######
### This function standardizes lines breaks
standardize_lines <- function(
  x, ### Character vector of words
  newline = "\n" ### Character string to split on
){
  ###### Defaults ######
  if(is.null(newline)){ newline <- "\n"}

  ###### Info about data ######
  num_words <- x %>% length

  ###### Find info about the number of lines ######
  c_numLines <- x %>% lapply(function(y){
    ### Get number of new lines
    y_findLines  <- gregexpr(pattern = newline, y) #; x_findLines
    y_numLines   <- y_findLines[[1]] %>% length ### ; x_numSplit

    y_numLines   <- ifelse(y_findLines[[1]][1] == -1, 0, y_numLines)
    return(y_numLines)
  }) %>% unlist

  ### Maximum number of lines
  max_numLines <- c_numLines %>% max(na.rm=T)

  # c_numLines %>% print
  # max_numLines %>% print

  # ###### Dataframes of words ######
  df_words <- data.frame(
    word = x,
    newlines = c_numLines
  )

  new_words <- 1:num_words %>% lapply(function(i){
    lines_i   <- df_words$newlines[i]
    word_i    <- df_words$word[i]
    add_lines <- max_numLines - lines_i

    # word_i %>% print
    # lines_i %>% print
    # add_lines %>% print

    ### New word
    if(add_lines == 0){
      new_word_i <- word_i
    } else {
      ### Add only one line:
      if(add_lines==1){
        add_line_i <- rep("\n", add_lines) %>% paste(collapse="")
        new_word_i <- add_line_i %>% paste0(word_i)
      } else{
        ### Number of lines before
        divideBy2  <- add_lines / 2
        before_i   <- (divideBy2 %% 2) + (divideBy2 %/% 2)
        after_i    <- add_lines - before_i
        beforeLines_i   <- rep("\n", before_i) %>% paste(collapse="")
        afterLines_i   <- rep("\n", after_i) %>% paste(collapse="")
        new_word_i <- beforeLines_i %>% paste0(word_i) %>% paste0(afterLines_i)
      }


    }

    # new_word_i %>% print
    return(new_word_i)
  }) %>% unlist
  return(new_words)

}
