###### convertTemps ######
#' Convert contiguous U.S. (CONUS) temperatures to global temperatures or vice versa
#'
#' @description
#' This pre-processing helper function converts a list of warming temperatures in degrees Celsius (`temps`) from global to CONUS (`from="global"`) or vice versa (`from="conus"`). The equations for converting between CONUS and global temperatures and back again are described elsewhere in this report.
#'
#' @param temps A numeric vector of CONUS or global temperatures in degrees Celsius.
#' @param from A character string (one of  `c("conus", "global")`), indicating whether users are converting from CONUS to global temperatures (`from="conus"`) or from global to CONUS (`from="global"`).
#'
#' Temperatures for the contiguous U.S. (CONUS) in degrees Celsius are converted to global temperatures (`convertTemps(temps, from="conus")`) in degrees Celsius by multiplying CONUS temperatures by a constant *1/k* (where *k=1.421*).
#'
#' Global temperatures in degrees Celsius are converted to CONUS temperatures (`convertTemps(temps, from="global")`) in degrees Celsius by multiplying global temperatures by a constant *k* (where *k=1.421*).
#'
#' @return
#' Outputs a numeric vector of temperatures in degrees Celsius.
#'
#' @examples
#' convertTemps(1:7, from = "global")
#' convertTemps(1:7, from = "conus")
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'
#' @export
#' @md
#'
#'
### This function converts a vector of temperatures from global to conus or vice versa
convertTemps <- function(
  temps, ### Vector of temperatures
  from  ### Type to convert from
){
  c0 <- 0 ### Update
  c1 <- 1.421 ### Update

  toType <- from |> tolower()
  if(from == "global"){
    temp_global <- temps
    new_temps   <- c1*temp_global + c0
  } else{
    temp_conus <- temps
    new_temps  <- (temp_conus - c0)/c1
  }

  ###### Return ######
  return(new_temps)
}

