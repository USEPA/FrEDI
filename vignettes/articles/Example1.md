Example \#1
================
June, 2023

# Running FrEDI with Default Parameters

FrEDI is commonly used to project the annual, climate-driven physical
and economic impacts associated with user-defined trajectories of U.S.
temperature change (relative to a 1985-2005 baseline), population, and
GDP.

This vignette provides a simple example of how to run and analyze data
from FrEDI’s default scenario.

## **Overview:**

**This example script:**

1.  [Installs](#installFrEDI) the `FrEDI` R package from GitHub

2.  [Sets](#setFrEDI) `FrEDI` input and run parameters.

3.  [Runs](#runFrEDI) `FrEDI` with inputs specified in Step 2

4.  [Shows](#analyzeFrEDI) example types of analyses using `FrEDI`
    output data.

For more information about `FrEDI`, see the [About](About.html) page and
[FrEDI Technical Documentation](https://www.epa.gov/cira/fredi)

## Analysis Steps (Main `FrEDI`):

### Step 1. Install `FrEDI` R package

When installing for the first time, see [Installing & Running
FrEDI](manual.html) page.

Load package

``` r
library(FrEDI)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: ggplot2

    ## Loading required package: tidyr

    ## Loading required package: openxlsx

### Step 2. Set `FrEDI` Runtime parameters

First, use this chunk to specify & format input projections (r set to
`NULL` to use default projections)

Use this chunk to specify the input trajectories (temperature,
population, GDP) and runtime parameters for `FrEDI`.

``` r
# To run FrEDI for more than one scenario, the code below can be
# adapted into a loop to format the inputs for each scenario. 

#***********************************************
#1. Specify & Format Input Trajectories (temperature, population, U.S. GDP)

## Input Files
tempInputFile <- NULL
  # Description: csv file with time series of temperature relative to 1986-2005 average 
  # (units: degC, values: >=0)
  # data must start in 2000 or earlier and can be global or CONUS
  # If global --> must convert to CONUS temperature using the import_inputs() helper function
  # column 1 = 'year', column 2 = 'temp_C'

slrInputFile <- NULL 
  # Description: csv file with time series of global mean sea level rise relative to 2000
  # (units: cm, values: >= 0 and <= 250)
  # data must start in 2000 or earlier
  # column 1 = 'year', column 2 = 'slr_cm'
  # If NULL - slr is calculated from the input temperature trajectory

gdpInputFile <- NULL
  # Description: csv file with time series of Gross Domestic Product (units: 2015$, values: >= 0) 
  # data must start in 2000 or earlier
  # column 1 = 'year', column 2 = 'gdp_usd' 
  # If NULL - use default GDP trajectory

popInputFile <- NULL
  # Description: csv file with time series of annual NCA regional population (values >= 0) 
  # data must start in 2000 or earlier
  # column 1 = 'year', columns 2:x = depends on data format (popform)
  # If NULL - use default population trajectory (from ICLUS)


## Input Trajectory parameters
popformFlag = 'wide'   
  # Description: Use this to specify the format of the regional population data
  # Options: wide/long. 
  # Wide = columns 2-8 correspond to population in each NCA region. 
  # Long = column 2 = NCA region name ('region'), 
  # column 3 = population for that region ('reg_pop'). 
  # NCA region names: 'Midwest', 'Northeast', 'Northern.Plains', 'Northwest',
  #   'Southeast', 'Southern.Plains', 'Southwest'

temptypeflag <- 'global' 
  # Description: Use this to specify whether the input temperature is global or CONUS
  # import_inputs() will convert to global to CONUS temperature
  # Options: global (input is global T), conus (input is CONUS T)

## Use the import_inputs() helper function to format the input trajectories for use in FrEDI
inputs_list <- import_inputs(tempfile = tempInputFile,
                        slrfile = slrInputFile,
                        popfile = popInputFile,
                        gdpfile = gdpInputFile,
                        temptype = temptypeflag,
                        popform = popformFlag)

# print out how many custom input files were loaded 
# should be = 1 if using custom temperature only
if ( length(inputs_list) ==0 ){
  print('CHECK FILE LOCATIONS: No input Data Loaded')
} else {
  print( paste('Number of input files loaded:',length(inputs_list)))
}
```

    ## [1] "CHECK FILE LOCATIONS: No input Data Loaded"

Next, set FrEDI runtime parameters

``` r
thru2300Flag = FALSE
  # Purpose: 
  #   Specify whether to run FrEDI through 2100 (default) or extend to 2300
  # Default: FALSE (will run to 2100)

baseYearFlag <- NULL     
  # Purpose: 
  #   Specify base year for calculating present values of annual impacts
  # Default: 2010

SectorListFlag <- NULL  
  # Purpose: 
  #   Specify the vector of sectors to calculate results for
  # Default: report output for all sectors
  # See FrEDI:get_sectorInfo() for list of all sectors

aggLevelFlag <- c('national','modelaverage','impactyear')
  # Purpose: 
  #   Specify the desired level of results aggregation. For example,
  #   to report national total results across all underlying climate-model 
  #   damage functions, set the flag to c('national','modelaverage')
  # Options: at least one from c('national', 'modelaverage', 'impactyear',
  #   'impacttype', 'all'), or "none". 
  # Default: c('national', 'modelaverage', 'impactyear','impacttype')

pvFlag <- NULL 
  # Purpose: 
  #   Calculate the present value of annual monetized impacts
  # Options: TRUE/FALSE.
  # Default: FALSE

rateFlag <- NULL         
  # Purpose: 
  #   If pvFlag = TRUE, specify the annual constant discount rate used 
  #   to calculate present values
  # Default: 0.03

elasticityFlag <- 1   
  # Purpose: 
  #   Specify the income elasticity used to adjust the Value of a
  #   Statistical Life (VSL)
  # Options: any numeric value
  # Default: 


silentFlag <- NULL       
  # Purpose:
  #   Specify the level of messaging to the user
  # Options: TRUE/FALSE. 
  # Default: TRUE
```
