###### temps2slr ######
### Created 2021.02.11. Updated with new GMSL calculation method 2021.06.02. Updated parameters in August 2021.
#' Convert global temperature change in degrees Celsius to global mean sea level rise (GMSL) in centimeters
#'
#' @description
#' This pre-processing helper function converts a vector of global temperatures to global mean sea level rise (GMSL). This function adapts the method described by Kopp et al. (2016) (see references, below).
#'
#' @param temps A numeric vector of global temperatures in degrees Celsius. The temperature series and corresponding years must begin in 2000 or earlier.
#' @param years A numeric vector of years (common era) corresponding to the temperatures provided to `temps`. The temperature series and corresponding years must begin in 2000 or earlier.
#'
#' @details
#' This function converts a temperature scenario (global temperatures in degrees Celsius) into an SLR scenario to use for estimate impacts of global mean sea level rise (GMSL) on affected sectors. [FrEDI::temps2slr()] implements the method described by Kopp et al., 2016, "Temperature-driven global sea-level variability in the Common Era" (see references, below).
#'
#' Kopp et al.'s "semiempirical" sea-level model relates the rate of global sea level (GSL) rise \eqn{\frac{dh}{dt}}{dh/dt} to global mean temperature \eqn{T(t)}, an equilibrium temperature \eqn{T_0(t)}{Te(t)}, and a small residual trend arising from the long-term response to earlier changes \eqn{\phi(t)}{\phi(t)}:
#'
#' \deqn{dh/dt = \alpha \cdot (T(t) - T_0(t)) + \phi(t)}{dh/dt = \alpha \cdot (T(t) - Te(t)) + \phi(t)}
#'
#' As explained by Kopp et al. (2016): "The first term describes the GSL response to earlier changes during the study period. The second term covers a small residual trend arising from the long-term response to earlier changes (i.e., deglaciation), which is very slowly decaying over millennia and of the order 0.1 mm/year in 2000 CE. It thus has a negligible effect on the modeled GSL rise during the 20th and 21st centuries" (Kopp et al., 2016, p. E1439).
#'
#' In the equation above, \eqn{T_0(t)}{Te(t)} and \eqn{\phi(t)} are functions of time, where:
#'
#' \deqn{\frac{dT_0(t)}{dt} = \frac{T(t) - Te(t)}{\tau_1}}{dTe(t)/dt = (T(t) - Te(t))/k1}
#'
#' And:
#'
#' \deqn{\frac{d\phi(t)}{dt} = \frac{\phi}{\tau_2}}{d\phi(t)/dt = p(t)/k2}
#'
#' And \eqn{\tau_1}{k1} and \eqn{\tau_2}{k2} are both constants.
#'
#' @return
#' Outputs a data frame with two columns: `year`, which has the years from the `years` input that fall within the range from 2000 through 2090 and `slr_cm` which has the GMSL in centimeters.
#'
#' @examples
#temps2slr(years = seq(2020, 2080, 10), temps =1:7)
#'
#' ### Path to example scenarios
#' scenariosPath <- system.file(package="FrEDI") |> file.path("extdata","scenarios")
#' ### View example scenario names
#'
#' scenariosPath |> list.files()
#'
#' ### Temperature Scenario File Name
#' tempInputFile <- scenariosPath |> file.path("GCAM_scenario.csv")
#'
#' ### Import example temperature scenario
#' example_inputsList <- import_inputs(tempfile = tempInputFile)
#'
#' ### Extract the example temperature scenario data frame from the list
#' ### Example has global temperatures in degrees Celsius
#' x_tempInput <- example_inputsList$tempInput
#'
#' ### Calculate global mean sea level rise in cm from global temperatures
#' x_slr <- temps2slr(temps=x_tempInput$temp_C, years=x_tempInput$year)
#'
#'
#' @references Environmental Protection Agency (EPA). 2021. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report EPA 430-R-21-004, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#' Kopp, Robert E., et al. (2016). Temperature-driven global sea-level variability in the Common Era. PNAS: E1434-E1441. Available at https://www.pnas.org/content/113/11/E1434
#'
#'
#' @export
#' @md
#'
### We used HadCrUT4 to determine the appropriate temperature offset between the actual temperature and the equilibrium temperature in 2000.
### Met Office Hadley Centre observations datasets. https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/download.html.
### Kopp Equations
### dh/dt = a*(T(t) - Te(t)) + c(t)
### dTe(t)/dt = (T(t) - Te(t))/tau1
### dc(t)/dt = c/tau2
###
### Kopp Constants
### Phi: (i.e., "c", above) is a temperature-independent rate term with e-folding time tau2. I.e., phi is the multi-millennial contribution to GMSL in mm/year.
### Scalar: Sensitivity of the GSL rate to a deviation of T(t) from an equilibrium temperature of Te(t).
### Alpha: The value obtained in Mann et al. posterior distribution (Kopp et al., 2016 Figure S5a for "a").
### Tau1: the timescale on which the actual temperature relaxes toward the equilibrium temperature. Value obtained  in Mann et al. posterior distribution (Kopp et al., 2016 Figure S5a for "tau").
### Tau2: e-folding time(scale) for phi. Value obtained  in Mann et al. posterior distribution (Kopp et al., 2016 Figure S5a for "tau_C").
### phi0       <- 0.14
### alpha      <- 4.0
### tau1       <- 174
### tau2       <- 4175
temps2slr <- function(
    temps,
    years,
    .kopp0  = "temps2slr" |> get_frediDataObj("fredi_config", "frediData"),
    .refYr0 = get_frediDataObj("co_modelTypes", "controlData") |> filter(model_type %in% "slr") |> pull(driverRefYear),
    .msg0   = 0
){
  ### Set up Environment ----------------
  #### Messaging ----------------
  # msg1    <- "\t"
  msgUser       <- !silent
  msgN          <- "\n"
  msg0          <- .msg0
  msg1          <- msg0 + 1
  msg2          <- msg0 + 2
  # if(msgUser)

  #### Kopp Constants ----------------
  # .kopp0     <- get_frediDataObj("fredi_config", "frediData")
  phi0       <- .kopp0[["phi0"]]
  alpha      <- .kopp0[["alpha"]]
  tau1       <- .kopp0[["tau1"]]
  tau2       <- .kopp0[["tau2"]]
  ### Other constants
  mm2cm      <- .kopp0[["mm2cm"]]
  eqTemp0    <- .kopp0[["eqTempOffset"]]


  ### Initialize Data ----------------
  ### Filter NA values and make sure values are numeric
  xCol0   <- "year"
  yCol0   <- "temp_C"
  yCol1   <- "slr_cm"
  mutate0 <- c("year", "temp_C")
  df0     <- tibble(year = years, temp_C = temps) |>
    mutate_at(c(xCol0, yCol0), as.numeric) |>
    filter_all(all_vars(!(. |> is.na()))) |>
    arrange_at(c(yrCol0))
  rm(mutate0)

  ### Check Data ----------------
  #### Duplicates ----------------
  ### Filter missing values, then get unique years
  dups0   <- df0 |>
    group_by_at(c(yrCol0)) |>
    summarize(n=n(), .groups="drop") |>
    filter(n > 1)

  ### Check that there are no duplicate rows
  hasDups <- dups0 |> nrow()
  if(hasDups){
    msg1 |> get_msgPrefix(newline=F) |> paste0("Warning: There are duplicate years in the inputs!") |> message()
    msg1 |> get_msgPrefix(newline=T) |> paste0("Exiting...") |> message()
    return()
  } ### End if(hasDups)

  ### Get distinct values
  df0     <- df0 |> distinct()
  years0  <- df0 |> pull(year)
  range0  <- years0 |> range()
  min0    <- range0[1]
  max0    <- range0[2]

  #### Ref Year ----------------
  ### Check for 2020 (year0)
  ### If 2020 not found, check for values above and below 2000
  ### If 2020 is still not found, message the user and exit
  ### Else, if there is a valid temperature series: Calculate temperatures
  hasRef0 <- min0 <= .refYr0 & max0 >= .refYr0
  if(!hasRef0) {
    msg1 |> get_msgPrefix(newline=F) |> paste0("Warning:") |> message()
    msg2 |> get_msgPrefix(newline=F) |> paste0("In 'temps2slr()': Missing values for the reference year ", .refYr0 , ".") |> message()
    msg3 |> get_msgPrefix(newline=F) |> paste0("The reference year ", .refYr0 , " must be present in the input (or the input must have values above and below the reference year).") |> message()
    msg1 |> get_msgPrefix(newline=T) |> paste0(msgN, "Enter a valid temperature series.") |> message()
    msg0 |> get_msgPrefix(newline=T) |> paste0(msgN, "Exiting...") |> message()
    return()
  } ### End if(!hasRef0)

  ### Interpolate Values ----------------
  # xOut0     <- min0:max0
  df0     <- years0 |> approx(
    y      = df0 |> pull(temp_C),
    xout   = min0:max0,
    method = "linear",
    rule   = 2
    ) |>
    as.data.frame() |>
    as.tibble() |>
    rename_at(c("x", "y"), ~c("year", "temp_C")) |>
    filter(year >= .refYr0)

  ### Calculate SLR from temperatures ----------------
  ### Initialize SLR columns
  df0     <- df0 |>
    mutate(equilTemp = NA, slr_mm = NA) |>
    mutate(yearFrom0 = year - .refYr0) |>
    mutate(phi = phi0 * exp(-yearFrom0 / tau2))
  # df0 |> glimpse();

  temp0   <- df0 |> filter(year == .refYr0) |> pull(temp_C)

  ### Iterate over series
  ### Calculate base values
  ### Equilibrium temps
  for(i in df0 |> pull(yearFrom0)){
    if(i == 0){
      ### Initialize temperature
      temp_C0        <- df0[["temp_C"]][i]
      df0[["equilTemp"]][i] <- temp_C0 - eqTemp0
      df0[["slr_mm"   ]][i] <- 0
    } else{
      df0[["equilTemp"]][i] <- df0[["equilTemp"]][i - 1] + ( df0[["temp_C"]][i] - df0[["equilTemp"]][i - 1] ) / tau1
      df0[["slr_mm"   ]][i] <- df0[["slr_mm"   ]][i - 1] + ( df0[["temp_C"]][i] - df0[["equilTemp"]][i] ) * alpha + df0[["phi"]][i]
    } ### End else
    rm(i)
  } ### End for(i in ind_x)

  ### GMSL in cm
  cols0   <- c("year", "slr_cm")
  df0     <- df0 |>
    mutate(slr_cm = slr_mm * mm2cm) |>
    select(all_of(cols0))

  ### Return ----------------
  return(df0)

}

