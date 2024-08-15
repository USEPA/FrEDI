###### defaultResults ######
#' Default outputs of [FrEDI::run_fredi()]
#
#' A dataframe containing the default outputs of [FrEDI::run_fredi()]
#
#' @format A data frame with 1,501,500 rows and 20 columns:
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
#' Six driver scenarios that can be passed as inputs to [FrEDI::run_fredi()] and [FrEDI::run_fredi_sv()]
#'
#' `gcamScenarios` is a data frame object containing six driver scenarios that can be passed as inputs to [FrEDI::run_fredi()] and [FrEDI::run_fredi_sv()]. This data frame has five columns -- `year`, `temp_C_global`, `temp_C_conus`, `slr_cm`, `scenario`, and `model`:
#'
#' \describe{
#'   \item{year         }{The Common Era (CE) year associated with the observation.}
#'   \item{temp_C_global}{Global temperature (i.e., degrees of warming above the baseline year of 1995), in degrees Celsius, for the associated year and scenario.}
#'   \item{temp_C_conus }{Temperatures (i.e., degrees of warming above the baseline year of 1995) for the contiguous U.S. (CONUS), in degrees Celsius, for the associated year and scenario (calculated from global temperatures using [FrEDI::convertTemps(from="global")]).}
#'   \item{slr_cm       }{Global Mean Sea Level Rise (GMSL or SLR), in centimeters, for the associated year and scenario (calculated from global temperatures using [FrEDI::temps2slr()]).}
#'   \item{scenario     }{Associated scenario identifier (e.g., `"ECS_3.0_REF"`).}
#'   \item{model        }{A string (`"Hector_GCAM_v5.3"`) identifying the model (**Hector**, with **GCAM v5.3**) used in generating the global temperatures associated with each scenario.}
#' }
#'
#' The scenarios in this data frame were created using **Hector** with **GCAM v5.3**:
#'
#'    * [__Hector__](https://jgcri.github.io/hector/) is an open-source, reduced-form global carbon-cycle climate model (Hartin et al., 2015) used to model temperatures associated with emissions scenarios from the Global Change Analysis Model v5.3 (GCAM).
#'    * [__GCAM v5.3__](https://gcims.pnnl.gov/modeling/gcam-global-change-analysis-model) -- i.e., the Global Change Analysis Model v5.3 -- is an open source model that represents the linkages between energy, water, land, climate and economic systems (Calvin et al., 2019).
#'
#' Scenario identifiers in the `scenario` column of [FrEDI::gcamScenarios()] have the string `"ECS_3.0_REF_"` as a prefix, followed by a suffix indicating an emissions intensity associated with the scenario (e.g., `"20"`) -- for instance, the default scenario for [FrEDI::run_fredi()] and [FrEDI::run_fredi_sv()] is `"ECS_3.0_REF"`. Other scenarios include `"ECS_3.0_REF_20"`, `"ECS_3.0_REF_30"`, `"ECS_3.0_REF_50"`, `"ECS_3.0_REF_70"`, and `"ECS_3.0_REF_90"`.
#'
#' Users can use the scenarios in `gcamScenarios` as inputs to FrEDI or the FrEDI SV Module:
#'
#'    * Users can filter the `gcamScenarios` data frame to any of these six scenarios, which can then be passed directly to the [FrEDI::run_fredi()] function via a named element (`temp` and/or `slr`) in a list passed to the `inputsList` argument -- e.g., `run_fredi(inputsList=list(temp=gcamScenarios |> filter(scenario=="ECS_3.0_REF")))` will run using the default temperature scenario.
#'    * Any or all of the GCAM scenarios can be passed directly to the FrEDI SV module via the [FrEDI::run_fredi_sv()] function via a named list element (`temp` and/or `slr`) in a list passed to the `inputsList` argument -- e.g., `run_fredi_sv(inputsList=list(temp=gcamScenarios))` will run the SV module for all the GCAM scenarios provided in `gcamScenarios`.
#'    * `gcamScenarios` can also be combined with other provided scenarios (`gdpScenario`, `popScenario`) in function calls (e.g., `run_fredi(inputsList=list(temp=gcamScenarios |> filter(scenario=="ECS_3.0_REF"), gdp=gdpScenario, pop=popScenario))`) or user-provided data frames. For more information, see documentation for [FrEDI::run_fredi()], [FrEDI:run_fredi_sv()], [FrEDI:run_fredi_methane()], and [FrEDI::import_inputs()].
#'
#' Calvin, K., Patel, P., Clarke, L., et al. 2019. GCAM v5.1: representing the linkages between energy, water, land, climate, and economic systems, Geosci. Model Dev., 12:677–698. https://doi.org/10.5194/gmd-12-677-2019.
#'
#' Hartin, C.A., Patel, P., Schwarber, A., Link, R.P. and Bond-Lamberty, B.P., 2015. A simple object-oriented and open-source model for scientific and policy analyses of the global climate system–Hector v1. 0. Geoscientific Model Development, 8(4), pp.939-955.
#'
#' @format A data frame with 606 rows and 6 columns:
#' \describe{
#'   \item{year         }{Year}
#'   \item{temp_C_global}{Global temperatures, in degrees Celsius, for the associated year and scenario}
#'   \item{temp_C_conus }{Temperatures for the contiguous U.S. (CONUS), in degrees Celsius, for the associated year and scenario (converted from global temperatures using [FrEDI::convertTemps(from="global")])}
#'   \item{slr_cm       }{Global Mean Sea Level Rise (GMSL or SLR), in centimeters, for the associated year and scenario (calculated from global temperatures using [FrEDI::temps2slr()])}
#'   \item{scenario     }{Associated scenario identifier (e.g., `"ECS_3.0_REF"`)}
#'   \item{model        }{A string (`"Hector_GCAM_v5.3"`) identifying the model (**Hector**, with **GCAM v5.3**) used in generating the global temperatures associated with each scenario}
#' }
#' @source \url{https://epa.gov/cira/FrEDI/}
"gcamScenarios"


###### popScenario ######
#' U.S. state population scenario, which can be passed as an input to [FrEDI::run_fredi()], [FrEDI::run_fredi_sv()], and/or [FrEDI:run_fredi_methane()].
#'
#' `popScenario` is a data frame object that contains population projections for 50 U.S. states and the District of Columbia for the period from 2010 to 2100. Values for the 48 states and the District of Columbia comprising the contiguous U.S. (CONUS) are from the **Integrated Climate and Land Use Scenarios Version 2** (**ICLUSv2**) model (Bierwagen et al, 2010; EPA 2017) under the Median variant projection of United Nations (United Nations, 2015). Values for Alaska and Hawaii are from the U.S. Census Bureau.
#'
#' To use `popScenario` as an input to FrEDI, the FrEDI SV module, and/or the FrEDI Methane module, pass the data frame via the named element `pop` in a list passed to the `inputsList` argument in function calls to [FrEDI::run_fredi()], [FrEDI::run_fredi_sv()], and/or [FrEDI:run_fredi_methane()], respectively, e.g.: `run_fredi(inputsList=list(pop=popScenario))`. `popScenario` can also be combined with other provided scenarios (`gcamScenarios`, `gdpScenario`) in function calls (e.g., `run_fredi(inputsList=list(gdp=gdpScenario, pop=popScenario))`) or user-provided data frames. For more information, see documentation for [FrEDI::run_fredi()], [FrEDI:run_fredi_sv()], [FrEDI:run_fredi_methane()], and [FrEDI::import_inputs()].
#'
#'
#'
#' Bierwagen, B., D. M. Theobald, C. R. Pyke, A. Choate, P. Groth, J. V. Thomas, and P. Morefield. 2010. “National housing and impervious surface scenarios for integrated climate impact assessments.” Proc. Natl. Acad. Sci. 107 (49): 20887–20892. https://doi.org/10.1073/pnas.1002096107.
#'
#' EPA. 2017. Multi-Model Framework for Quantitative Sectoral Impacts Analysis: A technical report for the Fourth National Climate Assessment. U.S. Environmental Protection Agency, EPA 430-R-17-001.
#'
#' United Nations. 2015. World population prospects: The 2015 revision. New York: United Nations, Department of Economic and Social Affairs, Population Division.
#'
#' @format A data frame with 4,459 rows and 5 columns:
#' \describe{
#'   \item{region}{Region of the contiguous U.S. ("Midwest", "Northeast", "Northern Plains", "Northwest", "Southeast", "Southern Plains", and "Southwest") or Alaska or Hawaii.}
#'   \item{state }{One of 48 contiguous U.S. states or the District of Columbia, Alaska, or Hawaii}
#'   \item{postal}{Two-letter postal code abbreviation associated with the state (e.g., "AK" for Alaska)}
#'   \item{year  }{Year}
#'   \item{pop   }{State population for associated region and year}
#' }
#' @source \url{https://epa.gov/cira/FrEDI/}
"popScenario"


###### gdpScenario ######
#' Scenario with values for U.S. Gross Domestic Product (GDP), which can be passed as an input to [FrEDI::run_fredi()] or [FrEDI::run_fredi_methane()].
#'
#' `gdpScenario` is a data frame object containing values for U.S. GDP for the contiguous U.S. (CONUS) for the period from 2010 to 2100. Values are from the **MIT Economic Projection and Policy Analysis Version 6** (**EPPA v6**) model (Chen et al, 2015; EPA 2017).
#'
#' To use `gdpScenario` as an input to FrEDI and/or the FrEDI Methane module, pass the data frame via the named element `gdp` in a list passed to the `inputsList` argument in function calls to [FrEDI::run_fredi()] and/or [FrEDI:run_fredi_methane()], respectively, e.g.: `run_fredi(inputsList=list(gdp=gdpScenario))`. `gdpScenario` can also be combined with other provided scenarios (`gcamScenarios`, `popScenario`) in function calls (e.g., `run_fredi(inputsList=list(gdp=gdpScenario, pop=popScenario))`) or user-provided data frames. For more information, see documentation for [FrEDI::run_fredi()], [FrEDI:run_fredi_methane()], and [FrEDI::import_inputs()].
#'
#' Chen, Y.-H. H., S. Paltsev, J. M. Reilly, J. F. Morris, and M. H. Babiker. 2015. The MIT EPPA6 Model: conomic Growth, Energy Use, and Food Consumption. MIT Joint Program on the Science and Policy of Global Change, No. 278.
#'
#' EPA. 2017. Multi-Model Framework for Quantitative Sectoral Impacts Analysis: A technical report for the Fourth National Climate Assessment. U.S. Environmental Protection Agency, EPA 430-R-17-001.
#'
#'
#' @format A data frame with 91 rows and 2 columns:
#' \describe{
#'   \item{year   }{Year}
#'   \item{gdp_usd}{U.S. Gross Domestic Product for the contiguous U.S.}
#' }
#' @source \url{https://epa.gov/cira/FrEDI/}
"gdpScenario"
