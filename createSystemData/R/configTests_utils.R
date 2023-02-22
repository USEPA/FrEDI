## Function to check if column has at least one non NA value
has_nonNA_values <- function(x) {
  ### Check whether values in x are NA
  x <- dataList$co_sectors
  x <- x %>% is.na
  ### Calculate number of rows
  y <- tibble(numRows = x %>% nrow)
  ### Number of NA values
  y <- y %>% mutate(numNA = x %>% colSums %>% nrow %>% if_else(is.null(.),0,.))
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
