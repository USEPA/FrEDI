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
#'    * `gcamScenarios` can also be combined with other provided scenarios (`gdpDefault`, `popDefault`) in function calls (e.g., `run_fredi(inputsList=list(temp=gcamScenarios |> filter(scenario=="ECS_3.0_REF"), gdp=gdpDefault, pop=popDefault))`) or user-provided data frames. For more information, see documentation for [FrEDI::run_fredi()], [FrEDI:run_fredi_sv()], [FrEDI:run_fredi_methane()], and [FrEDI::import_inputs()].
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


###### popDefault ######
#' U.S. state population scenario, which can be passed as an input to [FrEDI::run_fredi()], [FrEDI::run_fredi_sv()], and/or [FrEDI:run_fredi_methane()].
#'
#' `popDefault` is a data frame object that contains population projections for 50 U.S. states and the District of Columbia for the period from 2010 to 2100. Values for the 48 states and the District of Columbia comprising the contiguous U.S. (CONUS) are from the **Integrated Climate and Land Use Scenarios Version 2** (**ICLUSv2**) model (Bierwagen et al, 2010; EPA 2017) under the Median variant projection of United Nations (United Nations, 2015). Values for Alaska and Hawaii are from the U.S. Census Bureau.
#'
#' To use `popDefault` as an input to FrEDI, the FrEDI SV module, and/or the FrEDI Methane module, pass the data frame via the named element `pop` in a list passed to the `inputsList` argument in function calls to [FrEDI::run_fredi()], [FrEDI::run_fredi_sv()], and/or [FrEDI:run_fredi_methane()], respectively, e.g.: `run_fredi(inputsList=list(pop=popDefault))`. `popDefault` can also be combined with other provided scenarios (`gcamScenarios`, `gdpDefault`) in function calls (e.g., `run_fredi(inputsList=list(gdp=gdpDefault, pop=popDefault))`) or user-provided data frames. For more information, see documentation for [FrEDI::run_fredi()], [FrEDI:run_fredi_sv()], [FrEDI:run_fredi_methane()], and [FrEDI::import_inputs()].
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
#'   \item{region}{Region of the U.S.: one of `"Midwest"`, `"Northeast"`, `"Northern Plains"`, `"Northwest"`, `"Southeast"`, `"Southern Plains"`, or `"Southwest"` for CONUS states, or `"Alaska"` and `"Hawaii"` for Alaska and Hawaii, respectively.}
#'   \item{state }{U.S. states or the District of Columbia}
#'   \item{postal}{Two-letter postal code abbreviation associated with the state (e.g., `"AL"` for Alabama)}
#'   \item{year  }{Year}
#'   \item{pop   }{State population for associated region and year}
#' }
#' @source \url{https://epa.gov/cira/FrEDI/}
"popDefault"


###### gdpDefault ######
#' Scenario with values for U.S. Gross Domestic Product (GDP), which can be passed as an input to [FrEDI::run_fredi()] or [FrEDI::run_fredi_methane()].
#'
#' `gdpDefault` is a data frame object containing values for U.S. GDP for the contiguous U.S. (CONUS) for the period from 2010 to 2100. Values are from the **MIT Economic Projection and Policy Analysis Version 6** (**EPPA v6**) model (Chen et al, 2015; EPA 2017).
#'
#' To use `gdpDefault` as an input to FrEDI and/or the FrEDI Methane module, pass the data frame via the named element `gdp` in a list passed to the `inputsList` argument in function calls to [FrEDI::run_fredi()] and/or [FrEDI:run_fredi_methane()], respectively, e.g.: `run_fredi(inputsList=list(gdp=gdpDefault))`. `gdpDefault` can also be combined with other provided scenarios (`gcamScenarios`, `popDefault`) in function calls (e.g., `run_fredi(inputsList=list(gdp=gdpDefault, pop=popDefault))`) or user-provided data frames. For more information, see documentation for [FrEDI::run_fredi()], [FrEDI:run_fredi_methane()], and [FrEDI::import_inputs()].
#'
#' Chen, Y.-H. H., S. Paltsev, J. M. Reilly, J. F. Morris, and M. H. Babiker. 2015. The MIT EPPA6 Model: Economic Growth, Energy Use, and Food Consumption. MIT Joint Program on the Science and Policy of Global Change, No. 278.
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
"gdpDefault"



###### ch4Default ######
#' Scenario with changes in US methane concentrations for the years 2020 through 2100, which can be passed as an input to [FrEDI:run_fredi_methane()].
#'
#' `ch4Default` is a data frame object that contains projections with changes in annual US methane concentrations, at the national level, for the period from 2020 to 2100. Values for changes in annual US methane concentrations are provided in parts per billion by volume (ppbv) relative to a 1995-2006 baseline era. This scenario represents the SSP245 mean emissions pathway from RCMIP5, with methane concentrations produced using the FaIR model. For more information on the SSP245 mean emissions pathway and the FaIR model, see Nichols et al. (2020) and Leach, et al. (2021), respectively.
#'
#' To use `ch4Default` as an input to the FrEDI Methane module, pass the data frame via the named element `ch4` in a list passed to the `inputsList` argument in function calls to [FrEDI:run_fredi_methane()], e.g.: `run_fredi_methane(inputsList=list(ch4=ch4Default))`. `ch4Default` can also be combined with other provided scenarios (`gdpDefault`, `popDefault`) in function calls (e.g., `run_fredi(inputsList=list(gdp=gdpDefault, pop=popDefault))`) or user-provided data frames. For more information, see documentation for [FrEDI:run_fredi_methane()], and [FrEDI::import_inputs()].
#'
#' Leach, N. J., Jenkins, S., Nicholls, Z., Smith, C. J., Lynch, J., Cain, M., Walsh, T., Wu, B., Tsutsui, J., and Allen, M. R. 2021. FaIRv2.0.0: a generalized impulse response model for climate uncertainty and future scenario exploration, Geosci. Model Dev., 14, 3007--3036, https://doi.org/10.5194/gmd-14-3007-2021.
#'
#' Nicholls, Z. R. J., Meinshausen, M., Lewis, J., Gieseke, R., Dommenget, D., Dorheim, K., Fan, C.-S., Fuglestvedt, J. S., Gasser, T., Goluke, U., Goodwin, P., Hartin, C., Hope, A. P., Kriegler, E., Leach, N., Marchegiani, D., McBride, L. A., Quilcaille, Y., Rogelj, J., Xie, Z. (2020). Reduced Complexity Model Intercomparison Project Phase 1: introduction and evaluation of global-mean temperature response. Geoscientific Model Development. 13. 5175-5190. 10.5194/gmd-13-5175-2020.
#'
#' Environmental Protection Agency (EPA). Forthcoming. Technical Documentation on The Framework for Evaluating Damages and Impacts (FrEDI). Technical Report, EPA, Washington, DC. Available at <https://epa.gov/cira/FrEDI/>.
#'
#'  @format A data frame with 81 rows and 2 columns:
#' \describe{
#'   \item{year    }{Year}
#'   \item{CH4_ppbv}{Change in U.S. methane concentrations, in parts per billion by volume (ppbv), relative to a 1995-2006 baseline era.}
#' }
#' @source \url{https://epa.gov/cira/FrEDI/}
"gdpDefault"
