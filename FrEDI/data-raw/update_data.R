### Save default results
require(usethis)
require(devtools)
require(tidyverse)
defaultResults <- run_fredi()
usethis::use_data(defaultResults, overwrite=T); rm("defaultResults")

c_impactListFilePath <- getwd() %>% file.path("..", "createSystemData", "data", "sv", "impactsLists")
c_impactListFiles    <- c_impactListFilePath %>% list.files; c_impactListFiles

for(i in 1:length(c_impactListFiles)){
  fileName_i    <- c_impactListFiles[i]
  inFilePath_i  <- c_impactListFilePath %>% file.path(fileName_i)
  outFilePath_i <- c_impactListFilePath %>% file.path("inst", "extdata", "sv", "impactLists", fileName_i)
  file.copy(from=inFilePath_i, to = outFilePath_i)
}

# for(file_i in c_impactListFiles[1]){
# # for(file_i in c_impactListFiles){
#   name_i <- gsub(".rda", "", file_i); name_i %>% print
#   path_i <- c_impactListFilePath %>% file.path(file_i)
#   load(path_i)
#   # eval(substitute(usethis::use_data(x, overwrite=y)), list(x=parse(text=name_i), y=TRUE))
#   eval(substitute(usethis::use_data(list=ls(pattern="impactsList_"), overwrite=y)), list(x=parse(text=name_i), y=TRUE))
#   eval(substitute(rm(x)), list(x=parse(text=name_i)))
# }

