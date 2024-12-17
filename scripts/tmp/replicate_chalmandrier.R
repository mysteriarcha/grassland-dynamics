
## REPRODUCE LOIC CHALMANDRIER'S RESULTS

## Load Loic Chalmandrier's data

load("./Chalmandrier_interactions/data/data_ready.Rdata")
# This generates the following objects:
{
  ## a dataframe with trait, environmental, and species cover information.
  #  It would be a community matrix in long format, using species not as 
  #  columns but as observations (repeated through time):
  data
  
  ## The information used for the environmental part of "data":
  env_p
  
  ## The wide-format of the vegetation data part in "data". It's not in relative
  #  proportion values. It contains plots (with its repeated samples) as rows
  #  and species as columns. There are 15 species and 67 "plots" (less, as there
  #  are repeated plots through time)
  obs.comm
  
  ## A matrix of trait average values for each species for maximum height,
  #  SLA and "porosity"
  t.avg
  
  ## A list with the partial density functions of the species from an MClust
  #  algorithm done on the functional space. See details in "data_prep.R"
  pdf_species
}

source("./Chalmandrier_interactions/lib/traitspace_and_banquo.R")

source("./Chalmandrier_interactions/main/traitspace_gen.R")
