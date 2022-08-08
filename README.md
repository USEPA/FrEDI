# FrEDI
The Framework for Evaluating Damages and Impacts (FrEDI)

Estimates economic damages and impacts from climate change to the US through the 21st century


`library("devtools")`

To install FrEDI for the first time:

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

 point FrEDI to a particular temperature file of interest
 formated year, temp_c
 
`tempInputFile <- file.path("your_file.csv")`  
 also can add in files for gdp and population

 here we specify that the temperature inputs are global. This will
 allow FrEDI to convert those temperatures to CONUS level
 
`inputs <- import_inputs(tempfile = tempInputFile,  
                        temptype = "global")`

 simple command to run FrEDI using the imputs specified above  
`df <- run_fredi(inputsList= inputs, 
                aggLevels="all")`
                
 there are different levels to aggregate the data. By selecting "none"
 FrEDI will print out all of the information and the user can then aggregate after
