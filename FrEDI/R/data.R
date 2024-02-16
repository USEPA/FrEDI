###### defaultResults ######
# Default outputs of [FrEDI::run_fredi()]
#
# A dataframe containing the default outputs of [FrEDI::run_fredi()]
#
# @format A data frame with 173 016 rows and 18 columns:
# \describe{
#   \item{sector}{Name of the sector}
#   \item{variant}{Name of the adaptation or variant (values are sector-specific)}
#   \item{impactYear}{Name of the impact year ("2010", "2090", "N/A", or "Interpolation")}
#   \item{impactType}{Name of the impact type ("all" or sector-specific values)}
#   \item{region}{Name of the associated region ("Midwest", "Northeast", "Northern Plains", "Northwest", "Southeast", "Southern Plains", "Southwest", or "National Total")}
#   \item{model_type}{Type of model used to calculate impacts --- either "GCM" (for "Global Climate Model") or "SLR" for ("Sea Level Rise")}
#   \item{model}{Name of the GCM ("Average", "CanESM2", "CCSM4", "GCM Ensemble", "GFDL-CM3", "GISS-E2-R", "HadGEM2-ES", "MIROC5", "MRI-CGCM3") or SLR model ("Interpolation")}
#   \item{year}{Year of the output}
#   \item{driverValue}{Value for the associated driver type (in "degrees Celsius" for temperature and in "cm" for sea level rise)}
#   \item{driverUnit}{Unit for the associated driver type ("degrees Celsius" for temperature and "cm" for sea level rise)}
#   \item{driverType}{Associated driver type ("Temperature" or "GMSL (SLR)")}
#   \item{includeaggregate}{A `0` or `1` value indicating whether the variant is included in aggregation}
#   \item{sectorprimary}{A `0` or `1` value indicating whether the sector is a primary CIRA sector}
#   \item{gdp_usd}{National gross domestic product (GDP) for associated year in 2015 USD (U.S. dollars)}
#   \item{national_pop}{National population for associated year}
#   \item{gdp_per_cap}{National GDP per capita for associated year in 2015 USD (U.S. dollars) per capita}
#   \item{reg_pop}{Regional population for associated year}
#   \item{annual_impacts}{Annual impacts for associated year, region, sector, variant, impact year, impact type, and model}
#   \item{...}{...}
# }
# @source \url{https://epa.gov/cira/FrEDI/}
# "defaultResults"


###### gcamScenarios ######
#' Six driver scenarios that can be passed as inputs to [FrEDI::run_fredi_sv()]
#'
#' A dataframe containing six driver scenarios that can be passed as inputs to [FrEDI::run_fredi_sv()].
#'
#' The scenarios in this dataframe were created using Hector, a reduced-form global carbon-cycle climate model (Hartin et al., 2015) to model temperatures associated with emissions scenarios from the Global Change Analysis Model v5.3 (GCAM). The Global Change Analysis Model v5.3 (GCAM) is an open source model that represents the linkages between energy, water, land, climate and economic systems (Calvin et al., 2019). For more information on Hector and GCAM, see Appendix C of the FrEDI documentation.
#'
#' Calvin, K., Patel, P., Clarke, L., et al. 2019. GCAM v5.1: representing the linkages between energy, water, land, climate, and economic systems, Geosci. Model Dev., 12:677–698. https://doi.org/10.5194/gmd-12-677-2019.
#'
#' Hartin, C.A., Patel, P., Schwarber, A., Link, R.P. and Bond-Lamberty, B.P., 2015. A simple object-oriented and open-source model for scientific and policy analyses of the global climate system–Hector v1. 0. Geoscientific Model Development, 8(4), pp.939-955.
#'
#' @format A data frame with 546 rows and 4 columns:
#' \describe{
#'   \item{year}{Year}
#'   \item{temp_C}{CONUS temperature (in degrees Celsius) for the associated year and scenario}
#'   \item{slr_cm}{Global Mean Sea Level Rise (in centimeters) for the associated year and scenario}
#'   \item{scenario}{Associated scenario identifier}
#' }
#' @source \url{https://epa.gov/cira/FrEDI/}
"gcamScenarios"


###### popScenario ######
#' Population scenario to use as an input to [FrEDI::run_fredi_sv()]
#'
#' A dataframe containing a population scenario to be passed as an input to [FrEDI::run_fredi_sv()].
#'
#' This dataframe contains population projections at the regional level from the Integrated Climate and Land Use Scenarios version 2 (ICLUSv2) model (Bierwagen et al, 2010; EPA 2017) under the Median variant projection of United Nations (2015).
#'
#' Bierwagen, B., D. M. Theobald, C. R. Pyke, A. Choate, P. Groth, J. V. Thomas, and P. Morefield. 2010. “National housing and impervious surface scenarios for integrated climate impact assessments.” Proc. Natl. Acad. Sci. 107 (49): 20887–20892. https://doi.org/10.1073/pnas.1002096107.
#'
#' EPA. 2017. Multi-Model Framework for Quantitative Sectoral Impacts Analysis: A technical report for the Fourth National Climate Assessment. U.S. Environmental Protection Agency, EPA 430-R-17-001.
#'
#' United Nations. 2015. World population prospects: The 2015 revision. New York: United Nations, Department of Economic and Social Affairs, Population Division.
#'
#' @format A data frame with 133 rows and 3 columns:
#' \describe{
#'   \item{year}{Year}
#'   \item{region}{Region of U.S. ("Midwest", "Northeast", "Northern Plains", "Northwest", "Southeast", "Southern Plains", and "Southwest")}
#'   \item{reg_pop}{Regional population for associated region and year}
#' }
#' @source \url{https://epa.gov/cira/FrEDI/}
"popScenario"

