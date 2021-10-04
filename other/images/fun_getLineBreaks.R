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
  if(is.null(split )){ split  <- " "}
  if(is.null(thresh)){ thresh <- 16}
  if(is.null(silent)){ silent <- T}
  ###### Get initial info about string ######
  x_nchar      <- x %>% nchar #; x_nchar

  ###### Find info about the split character ######
  ### Get number of characters
  ### Find instances of the split character
  ### Find number of instances of the split string
  x_findSplit  <- gregexpr(pattern = split, x) #; x_findSplit
  x_numSplit   <- x_findSplit[[1]] %>% length ### ; x_numSplit
  x_numWords   <- x_numSplit + 1 ##; x_numWords
  x_splitPos   <- 1:x_numSplit %>% lapply(function(i){x_findSplit[i]}) %>% unlist ###; x_splitPos


  ###### Return word if no splits found
  if(x_numSplit == 0){
  # if(x_numSplit == 0 | x_nchar <= thresh){
    x_new <- x
    # x %>% print
  } else{
    # x_numSplit
    ###### Create a data frame with info ######
    ### Order of words, start and stop position, number of characters
    x_df_words   <- data.frame(
      order = 1:x_numWords,
      start = 1 %>% c(x_splitPos + 1),
      end   = c(x_splitPos - 1) %>% c(x_nchar)
    )
    ### Vector of words
    c_words <- x_df_words$order %>%
      lapply(function(i){
        substr(x, x_df_words$start[i], x_df_words$end[i])
      }) %>% unlist

    x_lastWord   <- c_words %>% last

    ### Add columns to x_df_words
    x_df_words <- x_df_words %>%
      mutate(word    = c_words) %>%
      mutate(numChar = nchar(word))

    ####### Figure out where to put new line characters ######
    ### x_new is new x, x_sub is temporary combination
    ### Initialize x_sub and x_new
    x_new <- NULL
    x_sub <- NULL

    ### Iterate over number of words
    for(i in 1:x_numWords){
      ### Get new word and number of characters
      word_i    <- x_df_words$word[i]
      next_word <- NULL
      ### Iterate until i < x_numWords
      if(i<x_numWords){
        i2         <- i + 1
        next_word  <- x_df_words$word[i2]
      }
      # else{next_word  <- x_df_words$word[i]}
      # i %>% print
      # paste("x_new:", x_new) %>% paste(paste("x_sub:", x_sub), collapse=", ") %>% paste(paste("word_i:", word_i), collapse=", ") %>% paste(paste("next_i:", next_word), collapse=", ") %>% print
      compareStr_i <- fun_compareStrings(
        new_x = x_new,
        sub_x = x_sub,
        word_x = word_i,
        next_x = next_word,
        thresh = thresh
      )

      x_new <- compareStr_i$new
      x_sub <- compareStr_i$sub

      # # x_sub %>% print
      #
      # if(is.null(x_sub)){
      # "here2" %>% print
      #
      #   match_word_i <- str_match(x_new, word_i)
      #   # match_word_i
      #   check_word_i <- match_word_i[1,1] %>% is.na
      #   x_new %>% print
      #   match_word_i %>% print
      #   check_word_i %>% print
      #
      #   if(check_word_i){
      #     x_sub <- word_i
      #   }
      # }

      # paste("x_new:", x_new) %>% paste(paste("x_sub:", x_sub), collapse=", ") %>% paste(paste("word_i:", word_i), collapse=", ") %>% paste(paste("next_i:", next_word), collapse=", ") %>% print

      ### Clear out sub
      if(i==x_numWords){
        if(!is.null(x_sub)){
          x_sep <- " "
          x_tmp <- c(x_new, x_sub) %>% paste(collapse=x_sep)
          n_tmp <- x_tmp %>% nchar
          if(n_tmp > thresh){x_sep <- "\n"}
          x_new <- c(x_new, x_sub) %>% paste(collapse=x_sep)
        }
      }


      # paste("x_new:", x_new) %>% paste(paste("x_sub:", x_sub), collapse=", ") %>% paste(paste("word_i:", word_i), collapse=", ") %>% paste(paste("next_i:", next_word), collapse=", ") %>% print
    }

    ### Trim blank spaces and return
    x_new <- x_new %>% trimws
    return(x_new)
  }

  ### Trim blank spaces and return
  x_new <- x_new %>% trimws
  return(x_new)
}
