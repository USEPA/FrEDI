###### fun_compareStrings() ######
### This function compares strings and is used with the function fun_getLineBreaks
fun_compareStrings <- function(
  new_x = NULL, sub_x = NULL, word_x = NULL, next_x = NULL, thresh = 16
){
  ###### Defaults ######
  if(is.null(thresh)){thresh <- 16}
  ###### Check values ######
  no_new_x  <- ifelse(is.null(new_x ), T, ifelse(new_x  == "", T, F))
  no_sub_x  <- ifelse(is.null(sub_x ), T, ifelse(sub_x  == "", T, F))
  no_word_x <- ifelse(is.null(word_x), T, ifelse(word_x == "", T, F))

  # no_sub_x %>% print
  ### Check if word is in new

  # new_x  <- new_x
  if(no_sub_x){
    sub_x       <- word_x
    word_in_sub <- T
  } else{
    if(no_word_x){
      sub_x       <- sub_x
      word_in_sub <- T
    } else{
      pattern_x   <- word_x
      # pattern_x  <- ifelse(word_x=="and", "\\a\\n\\d", word_x)
      word_in_sub <- !(str_match(pattern_x, word_x)[1,1] %>% is.na)
      if(!word_in_sub){
        sub_x       <- c(sub_x, word_x) %>% paste(collapse=" ")
        word_in_sub <- T
      }
    }
  }
  no_new_x  <- ifelse(is.null(new_x ), T, ifelse(new_x  == "", T, F))
  no_sub_x  <- ifelse(is.null(sub_x ), T, ifelse(sub_x  == "", T, F))
  no_word_x <- ifelse(is.null(word_x), T, ifelse(word_x == "", T, F))

  # c("new_x: ", new_x, ", sub_x: ", sub_x, ", word_x: ", word_x) %>% paste(collapse="") %>% print
  # ### Check if sub_x is in new_x
  # if(no_new_x & no_sub_x){
  #   # sub_in_new <- T
  #   sub_in_new <- F
  # } else if(no_new_x & !no_sub_x){
  #   sub_in_new <- F
  # } else{
  #   sub_in_new <- !(str_match(new_x, sub_x)[1,1] %>% is.na)
  # }
  ### Check if sub_x is in new_x
  if(no_new_x){
    if(no_sub_x) sub_in_new <- T
    else         sub_in_new <- F
  } else{
    if(no_sub_x) sub_in_new <- F
    else{
      # "here2" %>% print
      pattern_x  <- ifelse(sub_x=="and", "\\a\\n\\d", sub_x)
      sub_in_new <- !(str_match(new_x, pattern_x)[1,1] %>% is.na)
    }
  }

  ### If sub_in_new, shift things forward and recheck values
  if(sub_in_new){
    # "here" %>% print
    new_x  <- new_x
    sub_x  <- NULL
    # word_x <- next_x
  }
  # c("new_x: ", new_x, ", sub_x: ", sub_x, ", word_x: ", word_x) %>% paste(collapse="") %>% print

  ### Check values
  no_new_x  <- ifelse(is.null(new_x ), T, ifelse(new_x  == "", T, F))
  no_sub_x  <- ifelse(is.null(sub_x ), T, ifelse(sub_x  == "", T, F))
  no_word_x <- ifelse(is.null(word_x), T, ifelse(word_x == "", T, F))

  ### Check if word is in new
  if(no_new_x){
    # new_x       <- word_x
    # word_in_new <- T
    new_x       <- new_x
    word_in_new <- F
  } else{
    if(no_word_x){
      word_x      <- NULL
      # word_in_new <- c(new_x, word_x) %>%
      new_x       <- new_x
      word_in_new <- T
    } else{
      ### Check if word is in new
      new_x       <- new_x
      word_x      <- word_x

      # pattern_x   <- word_x
      pattern_x   <- ifelse(word_x=="and", "\\a\\n\\d", word_x)
      word_in_new <- !(str_match(new_x, pattern_x)[1,1] %>% is.na)
    }
  }
  ### Check values
  no_new_x  <- ifelse(is.null(new_x ), T, ifelse(new_x  == "", T, F))
  no_sub_x  <- ifelse(is.null(sub_x ), T, ifelse(sub_x  == "", T, F))
  no_word_x <- ifelse(is.null(word_x), T, ifelse(word_x == "", T, F))

  # c("new_x: ", new_x, ", sub_x: ", sub_x, ", word_x: ", word_x) %>% paste(collapse="") %>% print
  #### What to do if word_in_new versus not
  if(word_in_new){
    new_x       <- new_x
    sub_x       <- NULL
    word_x      <- NULL
  } else{
    ### Check if the sub is in x
    if(no_sub_x){
      sub_in_new  <- T
      word_in_sub <- F
    } else{
      if(no_new_x){
        sub_in_new  <- F
        pattern_x   <- word_x
        # pattern_x   <- " " %>% paste0(word_x)
        # pattern_x   <- ifelse(word_x=="and", "\\a\\n\\d", word_x)
        word_in_sub <- !(str_match(sub_x, pattern_x)[1,1] %>% is.na)
      } else{
        # pattern_x   <- sub_x
        pattern_x   <- " " %>% paste0(sub_x)
        # pattern_x   <- ifelse(sub_x=="and", "\\a\\n\\d", sub_x)
        sub_in_new  <- !(str_match(new_x, pattern_x)[1,1] %>% is.na)

        pattern_x   <- word_x
        # pattern_x   <- " " %>% paste0(word_x)
        # pattern_x   <- ifelse(word_x=="and", "\\a\\n\\d", word_x)
        word_in_sub <- !(str_match(sub_x, pattern_x)[1,1] %>% is.na)
        # # word_x %>% print
        # # sub_x %>% print
        # # pattern_x %>% print
        # new_x %>% print
        # sub_in_new %>% print
        # word_in_sub %>% print
        # str_match(sub_x, pattern_x) %>% print
      }
    }
    ### What to do about sub_x
    if(sub_in_new){
      if(word_in_sub){
        new_x       <- new_x
        sub_x       <- NULL
        word_x      <- NULL
        sub_x       <- next_x
      } else{
        new_x       <- new_x
        sub_x       <- word_x
        word_x      <- NULL
      }
    } else{
      if(word_in_sub){
        sub_x       <- sub_x
        # word_x      <- word_x
        word_x      <- NULL
      } else{
        sub_x       <- c(sub_x, word_x) %>% paste(collapse = " ")
        word_x      <- word_x
      }
    }

    ### Check values
    no_new_x  <- ifelse(is.null(new_x ), T, ifelse(new_x  == "", T, F))
    no_sub_x  <- ifelse(is.null(sub_x ), T, ifelse(sub_x  == "", T, F))
    no_word_x <- ifelse(is.null(word_x), T, ifelse(word_x == "", T, F))

    ### Add word_x and check length
    # sub_x %>% print
    # next_x %>% print
    tmp_x     <- c(sub_x, next_x) %>% paste(collapse=" ")
    no_tmp_x  <- ifelse(is.null(tmp_x), T, ifelse(tmp_x  == "", T, F))

    if(no_tmp_x){
      tmp_char <- 0
    } else{
      tmp_char <- tmp_x %>% nchar
    }
    tmp_diff <- thresh - tmp_char

    if(tmp_diff >= 0 & tmp_diff <= 1){
      new_x  <- c(new_x, tmp_x) %>% paste(collapse="\n")
      sub_x  <- NULL
      word_x <- NULL
      # next_x <- NULL
    } else if(tmp_diff < 0){
      new_x  <- c(new_x, sub_x) %>% paste(collapse="\n")
      sub_x  <- next_x
      word_x <- NULL
      # next_x <- NULL
    } else{
      new_x  <- new_x
      sub_x  <- tmp_x
      word_x <- NULL
      # word_x <- word_x
    }
  }

  ### Return
  return_list <- list(
    new  = new_x,
    sub  = sub_x,
    word = word_x
  )
  return(return_list)
}
