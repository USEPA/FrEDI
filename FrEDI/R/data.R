###### defaultResults ######
#' Default outputs of [FrEDI::run_fredi()]
#
#' A dataframe containing the default outputs of [FrEDI::run_fredi()]
#
#' @format A data frame with 775,179 rows and 20 columns:
#' \describe{
#'   \item{sector}{Name of the associated sector.}
#'   \item{variant}{Name of the associated variant or adaptation (values are sector-specific).}
#'   \item{impactYear}{Name of the impact year ("2010", "2090", "N/A", or "Interpolation").}
#'   \item{impactType}{Name of the impact type ("all" or sector-specific values).}
#'   \item{region}{Name of the associated region ("Midwest", "Northeast", "Northern Plains", "Northwest", "Southeast", "Southern Plains", "Southwest", or "National Total").}
#'   \item{state}{Name of the associated state (or District of Columbia): "Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",  "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",  "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas",  "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming". For the "National Total" region, `state = "All"`. For sectors with only has region-level data, `state = "N/A"`.}
#'   \item{postal}{Postal code abbreviation of the associated state (e.g., `postal="AL"` for `state="Alabama"`). The postal code abbreviation for the District of Columbia is `"DC"`. For the "National Total" region, `postal = "US"`. For sectors with only has region-level data, `postal = "N/A"`.}
#'   \item{model_type}{Type of model used to calculate impacts --- either "GCM" (for "Global Climate Model") or "SLR" for ("Sea Level Rise")}
#'   \item{model}{Name of the GCM ("Average", "CanESM2", "CCSM4", "GCM Ensemble", "GFDL-CM3", "GISS-E2-R", "HadGEM2-ES", "MIROC5", "MRI-CGCM3") or SLR model ("Interpolation")}
#'   \item{sectorprimary}{A `0` or `1` value indicating whether the sector is a primary CIRA sector (and whether it should be included when summing across sectors). `sectorprimary=1` if a primary sector and `sectorprimary=0` if not.}
#'   \item{includeaggregate}{A `0` or `1` value indicating whether the variant is the primary variant for the sector (and whether it should be included when summing across sectors). `includeaggregate=1` if the variant is a primary variant and `includeaggregate=0` if not).}
#'   \item{driverType}{Associated driver type ("Temperature" or "GMSL (SLR)").}
#'   \item{driverUnit}{Unit for the associated driver type ("degrees Celsius" for temperature and "cm" for sea level rise).}
#'   \item{driverValue}{Value for the associated driver type (in "degrees Celsius" for temperature and in "cm" for sea level rise).}
#'   \item{gdp_usd}{National gross domestic product (GDP) for associated year in 2015 USD (U.S. dollars).}
#'   \item{national_pop}{National population for associated year.}
#'   \item{gdp_per_cap}{National GDP per capita for associated year in 2015 USD (U.S. dollars) per capita.}
#'   \item{state_pop}{Population for the associated state (for state-level data) or region (for region-only data).}
#'   \item{annual_impacts}{Annual impacts for associated sector, variant, impact type, impact year, region, state, model type, model, and year.}
#' }
#' @source \url{https://epa.gov/cira/FrEDI/}
"defaultResults"


###### gcamScenarios ######
#' Six driver scenarios that can be passed as inputs to [FrEDI::run_fredi_sv()]
#'
#' A dataframe containing six driver scenarios that can be passed as inputs to [FrEDI::run_fredi_sv()].
#'
#' The scenarios in this dataframe were created using [Hector](https://jgcri.github.io/hector/), an open-source, reduced-form global carbon-cycle climate model (Hartin et al., 2015) to model temperatures associated with emissions scenarios from the Global Change Analysis Model v5.3 (GCAM). The Global Change Analysis Model v5.3 ([GCAM](https://gcims.pnnl.gov/modeling/gcam-global-change-analysis-model)) is an open source model that represents the linkages between energy, water, land, climate and economic systems (Calvin et al., 2019).
#'
#' Note that these temperature scenarios have been converted from the original global temperatures to CONUS temperatures using the [FrEDI::convertTemps] (with argument `from="global"`). The scenarios can therefore be passed directly to [FrEDI::run_fredi()] or [FrEDI::run_fredi_sv()]. *Please note that the `gcamScenarios` should be subset to any of the individual scenarios specified in the `scenario` column before passing to [FrEDI::run_fredi()] (e.g., `gcamScenarios |> dplyr::filter(scenario=="ECS_3.0_ref_0")` for the reference scenario).
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
#' Population scenario to use as an input to [FrEDI::run_fredi()]
#'
#' A dataframe containing a population scenario to be passed as an input to [FrEDI::run_fredi()].
#'
#' This dataframe contains population projections at the state level from the Integrated Climate and Land Use Scenarios version 2 (ICLUSv2) model (Bierwagen et al, 2010; EPA 2017) under the Median variant projection of United Nations (2015).
#'
#' Bierwagen, B., D. M. Theobald, C. R. Pyke, A. Choate, P. Groth, J. V. Thomas, and P. Morefield. 2010. “National housing and impervious surface scenarios for integrated climate impact assessments.” Proc. Natl. Acad. Sci. 107 (49): 20887–20892. https://doi.org/10.1073/pnas.1002096107.
#'
#' EPA. 2017. Multi-Model Framework for Quantitative Sectoral Impacts Analysis: A technical report for the Fourth National Climate Assessment. U.S. Environmental Protection Agency, EPA 430-R-17-001.
#'
#' United Nations. 2015. World population prospects: The 2015 revision. New York: United Nations, Department of Economic and Social Affairs, Population Division.
#'
#' @format A data frame with 14,259 rows and 5 columns:
#' \describe{
#'   \item{year}{Year}
#'   \item{region}{Region of U.S. ("Midwest", "Northeast", "Northern Plains", "Northwest", "Southeast", "Southern Plains", and "Southwest")}
#'   \item{state}{One of 48 contiguous U.S. states or the District of Columbia}
#'   \item{postal}{Postal code abbreviation associated with the state}
#'   \item{state_pop}{State population for associated region and year}
#' }
#' @source \url{https://epa.gov/cira/FrEDI/}
"popScenario"


###### popScenario_sv ######
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
#'
#' @source \url{https://epa.gov/cira/FrEDI/}
"popScenario_sv"
