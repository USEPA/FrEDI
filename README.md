# FrEDI
The Framework for Evaluating Damages and Impacts (FrEDI)

Estimates economic damages and impacts from climate change to the US through the 21st century

## Installation

To install FrEDI for the first time:

`library("devtools")`

 ```
 withr::with_libpaths(  
 
    new = .libPaths()[1],  
    
    devtools::install_github(  
    
        repo = "USEPA/FrEDI",  
          subdir = "FrEDI",  
         type = "source",  
          force = TRUE  
          #ref = "branch" # this will install a particular branch of interest
        ))
```

`library("FrEDI")`

## Using FrEDI
 After loading FrEDI, point FrEDI to a particular temperature file of 
 interest formatted year, temp_c:
 `tempInputFile <- file.path("your_file.csv")`.  
 You also can add in files for gdp and population.

 Here we specify that the temperature inputs are global. This will
 allow FrEDI to convert those temperatures to CONUS level:
 
`inputs <- import_inputs(tempfile = tempInputFile, temptype = "global")`

 A simple command to run FrEDI using the inputs specified above : 
`df <- run_fredi(inputsList= inputs, aggLevels="all")`
                
 There are different levels to aggregate the data. By selecting "none",
 FrEDI will print out all of the information and the user can then aggregate after.

## Contributing to FrEDI

The FrEDI team welcomes and values community contributions, but please
see our [Contribution Guide](FrEDI/vignettes/articles/contributing.Rmd) and note
by contributing to this project, you agree to abide to our [Contributor
Code of Conduct](FrEDI/CODE_OF_CONDUCT.md)

 ----------------------------------------------------------------------------------
 For more information, refer to the documentation at https://usepa.github.io/FrEDI.
