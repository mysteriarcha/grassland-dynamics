## Setup script: loads all the necessary scripts and data

## Start by sourcing "transform_data.R". This is the last main script on the
#  libraries -> data loading -> data transformation pipeline, so as long as 
#  each step in the pipeline sources the previous one we are done:

setwd(here::here())

source("scripts/03_transform_data.R")

## Other sources of information as those coming mainly after an analysis are
#  loaded only by localized readRDS() calls within individual scripts. Hence 
#  if someone wants to focus only on the predictions or on the networks it's 
#  not necessary to load unnecessary information

function_files <- paste0("functions/", dir("functions/"))
for(i in function_files) source(i)
rm("function_files")