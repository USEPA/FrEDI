---
title: "FrEDI: An R package for estimating future climate impacts within the United States"
tags:
  - climate change
  - simple climate model
  - climate impacts
  - scenario analysis
  - vulnerability
authors:
  - name: Karen Noiva
    orcid: 0000-0002-4412-5302
    equal-contrib: true
    affiliation: 1 # "1, 2" (Multiple affiliations must be quoted)
  - name: Corinne Hartin
    equal-contrib: true # (This is how you can denote equal contributions between multiple authors)
    affiliation: 2
  - name: Erin McDuffie
    orcid: 0000-0002-6845-6077
    equal-contrib: true
  - name: Marcus Sarofim
    orcid: 0000-0001-7753-1676
    equal-contrib: true
  - name: Jacqueline Willwerth
    orcid: 0000-0002-6107-0315
    equal-contrib: true
  - name: Will Maddock
    orcid: 0000-0002-6107-0315
    equal-contrib: true
  - name: Anthony Gardella
    orcid: 0000-0003-4380-3412
    equal-contrib: true
affiliations:
 - name: "Industrial Economics, Incorporated, Cambridge, MA", USA
   index: 1
 - name: "Climate Change Division, Office of Atmospheric Programs, U.S. Environmental Protection Agency, Washington, DC", USA
   index: 2
date: 30 June 2023
bibliography: paper.bib
---

# Summary

tony_test. The Framework for Evaluating Damages and Impacts (`FrEDI`) is an open-source R package that quantifies economic and physical impacts from climate change in the contiguous United States (CONUS) resulting from future temperature change trajectories [@EPA:2021a]. `FrEDI` quantifies the magnitude of impacts; when, where, and to whom these impacts may occur; the types of impacts that will be most damaging; and the capacity of adaptation to reduce potential risks. `FrEDI`’s use of a temperature binning approach and flexible framework, designed for continuous updating and scope expansion, facilitates scenario analysis, comparison between studies, and communication of results, thereby serving as an alternative or complement to traditional scenario-based approaches to estimating climate change impacts.

# Statement of need

`FrEDI` employs a temperature binning approach that utilizes peer-reviewed studies of climate change impact models within the U.S. to develop relationships between climate-driven changes in physical driver variables (mean CONUS surface temperature change or global mean sea level rise [SLR]) and physical and/or economic impacts. `FrEDI` v3.4 currently includes 22 impact sectors across seven regions within CONUS and provides insight into differences in climate-driven impacts under various adaptation scenarios, such as historical adaptation, reactive adaptation, or proactive adaptation. `FrEDI` also contains a module that can be used to quantify impacts to socially vulnerable populations (e.g., Black, Indigenous, People of Color [or BIPOC], low income, etc.) across six sectors. `FrEDI` was developed using a transparent process and peer-reviewed methodologies, and it is designed as a flexible framework that is continually refined to reflect the current state of climate change impact science, including the incorporation of new impacts and adaptation options.

For sectoral impacts driven by temperature change, damages in `FrEDI` are calculated as functions of CONUS degrees of warming over time, relative to a 1986-2005 average temperature baseline. For sectoral impacts driven by sea level rise (i.e., coastal properties and high tide flooding and traffic), sea level-driven damages in a given year are calculated by interpolating between modeled damages at different sea level heights at that same point in time; this enables `FrEDI` to account for interactions between adaptation costs, increased coastal property values, and sea level rise over time [@EPA:2021a]. 

Damage functions are specific to each of the seven CONUS regions from the Fourth National Climate Assessment [@EPA:2021a] that are included in `FrEDI` – Midwest, Northeast, Northern Plains, Northwest, Southeast, Southern Plains, Southwest. For sectors where the underlying data support disaggregation of physical and economic impacts, `FrEDI` first quantifies physical impacts from damage functions and then applies an appropriate valuation method to calculate economic damages. For instance, `FrEDI` first calculates projections of temperature-related premature mortality or acres burned by wildfires and then applies willingness-to-pay (i.e., Value of a Statistical Life, for mortality) or response costs, respectively. For sectors that do not lend themselves to such disaggregation, damage functions tie together economic valuation with physical impacts (e.g., repair costs and economic damages for the electricity transmission and distribution impact sector). Annual results are then modified using year-specific multipliers that represent socioeconomic conditions (including population, gross domestic product [GPD], and socioeconomic composition) to produce a time series of impacts. 

FrEDI has the flexibility to use any custom warming scenario, coupled with socioeconomic projections (e.g., U.S. GPD and population). The R package provides functions to convert between global and CONUS degrees of warming and to calculate global SLR from global degrees of warming. `FrEDI` uses a linear relationship to convert between CONUS and global temperatures, with $\textrm{CONUS} (\degree \textrm{C}) = 1.42 \cdot \textrm{Global} (\degree \textrm{C})$ degrees of warming [@EPA:2021a]. This relationship between CONUS and global temperatures is stable across GCMs and over time, allowing the use of these available datapoints to develop a generalized relationship between global and CONUS temperature anomalies. Global SLR heights are calculated from global temperature using a semi-empirical method that estimates global sea level change based on a statistical synthesis of a global database of regional sea-level reconstructions from [@Kopp:2016]. Inputs for physical climate drivers must start in the year 2000 or earlier, and inputs for socioeconomic drivers must start in the year 2010 or earlier. All inputs can extend to or past the year 2090 (`FrEDI` has the option to extend past 2090 to 2300).

Users can also set other parameters, such as an income elasticity to capture changes in real economic values over time (used in estimating economic damages for some sectors, such as temperature-related mortality) and a discount rate and base year to estimate present values of economic damages. FrEDI outputs a data frame of annual physical and economic climate impacts over the period of analysis, with results broken down by sector, region, climate model, and impact type (e.g., temperature-related deaths from heat versus cold) for each sector-specific adaptation option or other variant (e.g., impacts with or without fertilization from carbon dioxide for the agriculture impact sector). The results also specify the temperature, GDP, and population drivers, relevant units associated with physical impacts (e.g., "Acres burned" for wildfire response), as well as flags for the sectors and variants that are recommended for a default analysis.

`FrEDI` also includes a Social Vulnerability (SV) module that that can be used to quantify physical climate impacts for six sectors to four socially vulnerable populations – specifically, individuals identifying as Black, Indigenous, People of Color (or BIPOC), those from low-income households, elderly (ages 65 and above), and adults without high school diplomas. The SV module calculates impacts using U.S. Census tract- or block-level damage functions before aggregating impacts to the CONUS-region level. The SV module provides an option to save outputs as an Excel workbook containing features to facilitate interpretation of the results. Damage functions for the SV module were developed using data from the 2021 EPA report on Climate Change and Social Vulnerability [@EPA:2021b].

`FrEDI`’s level of detail and flexibility provide a useful framework to efficiently, rapidly, and transparently explore a variety of future baseline trajectories and emission reduction policies that can complement the more traditional scenario-based analyses and outputs, such as those currently provided by Integrated Assessment Models [@Sarofim:2021]. `FrEDI` was designed to fill the current need of monetizing a broad range of climate-driven impacts in the U.S. across various emission/socioeconomic trajectories while doing so in a significantly shorter computational timeframe (e.g., seconds to produce results for all sectors and regions) relative to existing impact models. The `FrEDI` code is a flexible framework that can be continuously updated to reflect that latest available science on climate change impacts, and in current form facilitates exploration of structural uncertainty in sector-level impact estimates by allowing users to select from among alternative variants to impact estimation. `FrEDI` has been used in analyzing  climate change impacts including the regions, sectors, and vulnerable populations most at risk [@Hartin:2023] and in assessing benefits of staying below 2 degrees Celsius of warming in the White House’s Long-Term Strategy [@DOS:2021]. 

This flexible approach allows new sectors to be added to the `FrEDI` tool in a modular way, as new studies are published or new results become available. In support of these frequent updates, the FrEDI development environment includes a test suite and related test cases that reinforce the validation of model data against source data throughout the configuration process, enhancing model transparency and replicability.

`FrEDI` is available on the EPA GitHub repository at https://usepa.github.io/FrEDI/index.html. Source code can be downloaded and installed from https://github.com/USEPA/FrEDI using R GitHub tools. Alternatively, a user can download the compiled package and install the package manually using the `install.packages` function. Additional `FrEDI` Technical Documentation is available at: https://www.epa.gov/cira/fredi.

Figures can be included like this:
![Schematic of FrEDI components, from user inputs to model outputs.\label{fig:schematic}](fredi_schematic.png)
and referenced from text using \autoref{fig:schematic}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](fredi_schematic.png){ width=20% }

# Acknowledgements

The views presented in this manuscript are solely those of the authors and do not necessarily represent the views or policies of the U.S. Environmental Protection Agency. Support for Industrial Economics was provided under EPA contracts 47QFSA21D0002 and 140D0420A0002.The authors also wish to acknowledge research assistance and other analytic support from Hayley Kunkle and Charles Fant.

# References
