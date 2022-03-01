require(usethis)
require(devtools)
require(tidyverse)
defaultResults <- run_fredi()
usethis::use_data(defaultResults, overwrite=T); rm("defaultResults")

c_impactListFilePath <- getwd() %>% file.path("..", "createSystemData", "data", "sv", "impactsLists")
c_impactListFiles    <- c_impactListFilePath %>% list.files; c_impactListFiles

# name_i <- "impactsList_airQuality"
file_i <- "impactsList_roads_proactiveAdapt" %>% paste0(".rda")
path_i <- c_impactListFilePath %>% file.path(file_i)
load(path_i)
usethis::use_data(impactsList_roads_proactiveAdapt, internal=F, overwrite=T)
rm(impactsList_roads_proactiveAdapt)

# for(file_i in c_impactListFiles[1]){
# # for(file_i in c_impactListFiles){
#   name_i <- gsub(".rda", "", file_i); name_i %>% print
#   path_i <- c_impactListFilePath %>% file.path(file_i)
#   load(path_i)
#   # eval(substitute(usethis::use_data(x, overwrite=y)), list(x=parse(text=name_i), y=TRUE))
#   eval(substitute(usethis::use_data(list=ls(pattern="impactsList_"), overwrite=y)), list(x=parse(text=name_i), y=TRUE))
#   eval(substitute(rm(x)), list(x=parse(text=name_i)))
# }

