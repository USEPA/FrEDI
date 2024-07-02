### This file documents the R temperature binning package
#' README
#' FrEDI: The Framework for Evaluating Damages and Impacts
#'
#'
#' FrEDI is an R package being developed by the U.S. Environmental Protection Agency (EPA). The functions and data provided by this package can be used to estimate climate change impacts for the contiguous United States (CONUS) using the [Framework for Evaluating Damages and Impacts (FrEDI)](https://epa.gov/cira/FrEDI/), developed as part of EPA's [Climate Change Impacts and Risk Analysis](https://epa.gov/cira/) (CIRA) project. FrEDI contains R code that implement FrEDI and allow users to project impacts from climate change and sea level rise for a selected set of sectors.
#'
#'
#'
#' For help getting started with FrEDI, visit <https://usepa.github.io/FrEDI/articles/manual.html>.
#'
#' For additional package documentation, see <https://usepa.github.io/FrEDI/index.html>.
#'
#' For more information on the Framework and the CIRA project, visit <https://epa.gov/cira/>, especially <https://epa.gov/cira/FrEDI/>.
#'
#'
#'
#' @section Overview of Functions:
#'
#' The function [FrEDI::run_fredi()] provided in this package is the primary function implementing the [Framework for Evaluating Damages and Impacts (FrEDI)](https://epa.gov/cira/FrEDI/), developed by the U.S. EPA for projecting annual climate impacts. The main inputs to [FrEDI::run_fredi()] are climate scenarios (temperature in degrees Celsius, global mean sea level rise in centimeters) and socioeconomic scenarios (U.S. gross domestic product, state population).
#'
#' FrEDI also contains functions to assist in the pre-processing of input scenarios and the post-processing of outputs.
#'
#' * Pre-processing functions include [FrEDI::get_sectorInfo()], [FrEDI::import_inputs()], [FrEDI::convertTemps()], [FrEDI::temps2slr()].
#'      * [FrEDI::get_sectorInfo()] allows users to access a list of sectors within `FrEDI` and related sector information.
#'      * [FrEDI::import_inputs()] helps users in importing custom scenarios from user-specified comma-separated value (CSV) files.
#'      * [FrEDI::convertTemps()] helps users to convert between global mean temperature and temperatures for the contiguous United States (CONUS) (both in degrees Celsius).
#'      * [FrEDI::temps2slr()] helps users to estimate global mean sea level rise (GMSL, in centimeters) from global mean temperature in degrees Celsius.
#'
#' [FrEDI::aggregate_impacts()] is a post-processing helper function that helps users to aggregate and summarize the outputs of FrEDI. [FrEDI::aggregate_impacts()] can be used to calculate national totals, model averages, sum over impact types, and interpolate between multiple impact years (note that [FrEDI::run_fredi()] will automatically run [FrEDI::aggregate_impacts()] before returning outputs if the `aggLevels` argument is not `"none"`).
#'
#' Versions 2.3.0 and above include the `FrEDI` Social Vulnerability (SV) module for estimating impacts on socially vulnerable populations for select sectors. [FrEDI::get_sv_sectorInfo()] allows users to access a list of sectors within the FrEDI SV module and related sector information. The function [FrEDI::run_fredi_sv()] is the main function for the `FrEDI` SV module. [FrEDI::run_fredi_sv()] is designed to calculate impacts for a single sector at a time for a custom population scenario or one or more custom temperature or sea level rise scenarios. For more information on the data underlying the `FrEDI` SV module, see <https://www.epa.gov/cira/social-vulnerability-report/>.
#'
#'
#'
#' @section Overview of Package Contents:
#'
#' FrEDI consists of files in the following directories:
#' * __R__. Contains function definitions (files ending in `".R"`) and configuration files (ending in `".rda"`).
#' * __data__. Contains R Data files ending in `".rdb"`, `".rds"`, and `".rdx"`, containing data included with the package.
#' * __help__ and __html__. Contain documentation for functions available to the user, including function descriptions, lists of arguments and outputs, and examples. See `"html/00Index.html"` or the individual R help functions for more information about individual functions.
#' * __Meta__. Contains RDS files (ending in `".rds"`) with information about the package contents.
#' * __extdata__. __extdata/scenarios__ Contains four CSV files for users to test the function for importing data. For more information on importing scenarios for use with [FrEDI::run_fredi()], refer to documentation for the function [FrEDI::import_inputs()].
#'      * `"GCAM_scenario.csv"` contains a set of temperature scenarios that can be used with FrEDI, including the default temperature scenario used by both [FrEDI::run_fredi()] and [FrEDI::run_fredi_sv()]. Also see documentation for the [FrEDI::gcamScenarios] dataset for more information.
#'      * `"State ICLUS Population.csv"` contains the default state population scenario used by [FrEDI::run_fredi()] (see [FrEDI::popScenario] and [FrEDI::popScenario_sv]).
#'      * `"slr_from_GCAM.csv"` contains global mean sea level rise heights in centimeters (created from the reference temperature scenario).
#'  * __extdata/sv__ Contains files used by the `FrEDI` SV module to calculate impacts.
#'
#' The `FrEDI` package contains a loadable dataset with default results `defaultResults`, which contains annual impacts produced by [FrEDI::run_fredi()] for the with the default options and default scenarios (i.e., default temperature, GDP, and state population trajectories). Other loadable datasets provided by FrEDI are a set of driver scenarios ([FrEDI::gcamScenarios]) and a state population scenario ([FrEDI::popScenario]) for use with [FrEDI::run_fredi()] or [FrEDI::run_fredi_sv()], which can be loaded into the workspace using the `data()` function (e.g., `data(gcamScenarios)`).
#'
#' Typical use will involve `library(FrEDI)` or `require(FrEDI)`.
#'
#'
#'
#' @section Status:
#' Disclaimer: All code in this repository is being provided in a "draft" state and has not been reviewed or cleared by U.S. EPA. This status will be updated as models are reviewed.
#'
#'
#'
#' @section Dependencies:
#' FrEDI requires R (>= 4.2.0).
#'
#'
#' FrEDI depends on:
#' * [tidyverse] (Easily Install and Load the 'Tidyverse'). The official documentation for [tidyverse] can be found [here](https://cran.r-project.org/web/packages/tidyverse/index.html). [tidyverse] can be installed using `install.packages("tidyverse")`, or see [link](https://tidyverse.tidyverse.org/) for more information.
#' * [ggpubr] ('ggplot2' Based Publication Ready Plots). The official documentation for [ggpubr] can be found [here](https://cran.r-project.org/web/packages/ggpubr/index.html). [ggpubr] can be installed using `install.packages("ggpubr")`, or see [link](https://rpkgs.datanovia.com/ggpubr/) for more information.
#' * [openxlsx] (Read, Write and Edit `xlsx` Files). The official documentation for [openxlsx] can be found [here](https://cran.r-project.org/web/packages/openxlsx/index.html). [openxlsx] can be installed using `install.packages("openxlsx")`.
#'
#'
#'
#' @section License:
#' This repository is released under the MIT License.
#'
#'
#'
#' @section EPA Disclaimer:
#' The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
#'
#' By submitting a pull request to the GitHub and/or by installing this package, you make an agreement with EPA that you will not submit a claim of compensation for services rendered to EPA or any other federal agency. Further, you agree not to charge the time you spend developing software code related to this project to any federal grant or cooperative agreement.
#'
#' @keywords internal
"_PACKAGE"
#' @name FrEDI
#' @md
#> NULL
