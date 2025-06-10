# FrEDI 5.0.0

This release corresponds to FrEDI v5.0

* Bug Fixes: 
  - Rounding errors due to manual editing in excel of base data was fixed
  - Fixed GDP per capita scaling for sectors using GDP per capita as an economic multiplier/adjustment (including SLR sectors post-2100)
  
* Major updates: 
  - Data: Now stored as a SQLite database file in inst/extdata
  - FrEDI: New sectors Forestry, Learning Loss, Lyme Disease, Marsh Migration, and Outdoor Recreation were added (31 sectors total). 
  - FrEDI: New Air Quality Impact types were added to Air Quality, Southwest Dust, and Wild Fire sectors. 
  - FrEDI GHG: Now includes state level Mortality impacts
  - FrEDI GHG: New Morbidity sector impacts added

* Minor updates:
  - Data: Now includes FrEDI input testing scenarios
  - Data: Function `load_frediDB` introduced to load FrEDI database
  - FrEDI: Updates to figure utility functions and actions


# FrEDI v4.1.1

_See FrEDI v4.1 for full release notes_

* Bug fixes - n/a
* Updates - 
** Major updates: n/a
** Minor updates include: updated the `includeaggregate` flag for the suicide sector in `run_fredi()` and `run_fredi_sv()`, to alert users of an alternative sector aggregation approach to avoid possible double counting with the impacts in the default temperature-related mortality sector; updated R package documentation to reflect updates; updated github page
* Additional sectors added - n/a
* Documentation fixes - n/a
* New features - n/a

# FrEDI v4.1

This release corresponds to FrEDI v4.1, which accompanies the publication of the 2024 FrEDI Technical Documentation.

* Bug fixes - n/a
* Updates -
** Major updates: impacts and damages now calculated at the state-level (previously region-level) for all 25 impact-category sectors; FrEDI default max year was changed from 2090 to 2100
** Minor updates include: updated import_inputs() to accept population datafiles containing U.S. national, contiguous U.S., or U.S. state populations and set temperature values outside the allowed range (<=0C and >= 30C) to zero; updated run_fredi_sv() to accept state-level population inputs; updated R package documentation to reflect new features
* Additional sectors added - n/a
* Documentation fixes - minor updates to account for new default max year and state-level population input formatting features
* New features - FrEDI outputs include damages and impacts at the state-level (48 contiguous states and the District of Columbia) for all 25 impact category sectors; FrEDI default max year was changed from 2090 to 2100

# FrEDI v4.0.2 (peer-review)

Starting in February 2024, the FrEDI Technical Documentation and v4.0 of the FrEDI R package will be subject to a public review comment period and independent, external peer-review.
This release corresponds to FrEDI v4.0.2, the peer-review release version.

* Bug fixes - fixes to GDP input reference in and meta data documentation in run_fredi()
* Updates -
** Major updates include: impacts and damages now calculated at the state-level (previously region-level) for 17 impact-category sectors
** Minor updates include: n/a
* Additional sectors added - n/a
* Documentation fixes - n/a
* New features - FrEDI outputs include damages and impacts at the state-level (48 contiguous states and the District of Columbia) for 17 impact category sectors.

# FrEDI v4.0.1 (public review)

Starting in February 2024, the FrEDI Technical Documentation and v4.0 of the FrEDI R package will be subject to a public review comment period and independent, external peer-review.
This release corresponds to FrEDI v4.0.1, the public comment release version.

* Bug fixes - file paths in function documentation for run_fredi(), import_inputs(), and aggregate_impacts(); income elasticity for SLR sectors post-2090 in run_fredi() (no longer dependent on argument elasticity)
* Updates -
** Major updates include: impacts and damages now calculated at the state-level (previously region-level) for 12 impact-category sectors; population inputs for `run_fredi()` now requires state-level population instead of region-level inputs; damage function extrapolation approach for warmer temperatures for `run_fredi()` and `run_fredi_sv()`; default income elasticity (argument `elasticity` in `run_fredi()`) from 0.4 to 1.0; updated `run_fredi()` user inputs to remove options to calculate present values (arguments `pv`, `rate`, `baseYear`) and add option to output intermediate calculation columns
** Minor updates include: `magrittr` pipe operator `%>%` to the native pipe operator `|>`; Added monitoring of default `run_fredi()` parameter use; output columns from `aggregate_impacts` (added columns `“sectorprimary”` and `“includeaggregate”` for all aggregation levels and added columns `“physical_impacts”`, `“physicalmeasure”` for all aggregation levels except `aggLevels=”impacttype”`); removed option to output `run_fredi_sv()` module results to an Excel template
* Documentation fixes - this version corresponds to the February 2024 version of the FrEDI Technical Documentation (see https://www.epa.gov/cira/about-fredi). Function documentation updated to correspond to updates.
* Additional sectors added - n/a
* New features - FrEDI outputs include damages and impacts at the state-level (48 contiguous states and the District of Columbia) for 12 impact category sectors.

# FrEDI v3.4

* Bug fixes - minor fixes to inputs used in run_fredi() and run_fredi_sv()
* Updates - minor updates to run_fredi_sv() output template and documentation; run_fredi() input options 
* Documentation fixes - minor documentation fixes, e.g., global to conus temperature in import_inputs, updates to variant names, function documentation
* Additional sectors added - (temperature and weather impacts on) Suicide Incidence; (health and economic burden from changes in) Vibriosis cases
* New features - added components to create GitHub IO site (<https://usepa.github.io/FrEDI/>); new run_fredi() option to report out a list of input parameters at runtime; new user option to extend run_fredi() out to 2300

# FrEDI v2300_paper

* FrEDI code associated with Hartin et al., 2023 <https://egusphere.copernicus.org/preprints/2023/egusphere-2023-114/>

# FrEDI v3.0

* Bug fix - global to conus temperature in `import_inputs`
* Additional sector added - ATS Extreme Temperature Mortality
* New feature - added capability to perform distributional analysis of impacts among different populations for select sectors

# FrEDI v2.2.0

* Additional sectors added - Crime, Agriculture, and Marine Fisheries

# FrEDI v2.1.0

* Updates - extreme temperature mortality sector
* New feature - option for user supplied IEVSL

# FrEDI v2.0

Initial release of FrEDI that corresponds to the technical documentation 2021. <https://www.epa.gov/cira/fredi/>
