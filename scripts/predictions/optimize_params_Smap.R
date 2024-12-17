rm(list = ls())

## This script makes the predictions in the Convergent Cross Mapping 
#  algorithm based on the results obtained by the interactions_CCM.R script

library(pttstability); library(tidyverse)


source("interactions_CCM.R")
source("name_interactions.R")

T_Devin_wide <- readRDS("./data/RDS/T_Devin_wide.RDS")
F_Devin_wide <- readRDS("./data/RDS/F_Devin_wide.RDS")
P_Devin_wide <- readRDS("./data/RDS/P_Devin_wide.RDS")

T_interactions <- readRDS("./data/RDS/T_interactions.RDS")
F_interactions <- readRDS("./data/RDS/F_interactions.RDS")

# ?S_map_Sugihara1994

# Make a function to obtain the pattern of NAs delimiting different
# libraries at the F scale
source("./functions/libs_struct.R")

# Make function to obtain the Es for each focal species  as 1 + the number 
# of species significantly affecting it 
get_Es <- function(species, interactions){
  nms  <- rownames(interactions)
  inds <- str_detect(nms, species)
  sig_inters <- filter(interactions[inds, ], significant_BtoA) 
  E    <- 1 + NROW(sig_inters)
  return(E)
}

E_use   <- sapply(spp, get_Es, interactions = T_interactions)
F_E_use <- sapply(spp, get_Es, interactions = F_interactions)

source("./functions/my_smap_optimizer.R")


# 1. Large scale (T) ------------------------------------------------------

T_optimized_theta <- vector("list", length(spp))
for(i in seq_along(T_optimized_theta)){
  T_optimized_theta[[i]] <- tryCatch(
   my_Smap_optimizer(T_Devin_wide[[i]], E_use[[i]]),
   error = function(e) paste0("Error with the species: ", spp[[i]])
   )
}
names(T_optimized_theta) <- spp

T_E_opt <- integer(length(spp))
for(i in seq_along(T_E_opt)){
  T_E_opt[[i]] <- 
    tryCatch(
      which.min(T_optimized_theta[[i]]$RMSE), 
      error = function(e) paste0("Error with the species: ", spp[[i]] )
      )
  }

sum(as.numeric(T_E_opt[-c(37, 38, 54)]) < E_use[-c(37, 38, 54)])
# In "only" 28 cases the species are predicted by a smaller number
# of dimensions than we supposed based on species interactions

# We will use the default values in F_E_use for the E values that failed
# to be optimized
T_theta_opt <- numeric(length(spp))
for(i in seq_along(T_theta_opt)){
  T_theta_opt[[i]] <- 
    tryCatch(
      T_optimized_theta[[i]]$theta_opt[[which.min(T_optimized_theta[[i]]$theta_opt)]], 
      error = function(e) paste0("Error with the species: ", spp[[i]] )
  )
}

T_opt_params <- data.frame(E = T_E_opt, theta = T_theta_opt)
rownames(T_opt_params) <- spp

saveRDS(T_optimized_theta, "./data/RDS/T_optimized_theta.RDS")
saveRDS(T_opt_params, "./data/RDS/T_opt_params.RDS")


# 2. Medium-size scale (F) -----------------------------------------------------

F_optimized_theta <- vector("list", length(spp))
for(i in seq_along(F_optimized_theta)){
  F_optimized_theta[[i]] <- 
    tryCatch(
      my_Smap_optimizer(F_ts[[i]], F_E_use[[i]]),
      error = function(e) paste0("Error with the species: ", spp[[i]])
  )
}
names(F_optimized_theta) <- spp

F_E_opt <- integer(length(spp))
for(i in seq_along(F_E_opt)){
  F_E_opt[[i]] <- 
    tryCatch(
      which.min(F_optimized_theta[[i]]$RMSE), 
      error = function(e) paste0("Error with the species: ", spp[[i]])
  )
}

# The optimizer has failed for the species:
# 1. Inula oculus-christi
# 2. Iris pumila
# 3. Reseda lutea
# They are in positions 37, 38 and 54, respectively. 
# Let's try to fit them manually

theta_grid <- seq(0, 10, l=1e2)
sapply(
  theta_grid,
   function(x){
      ts <- F_ts[[54]] # Select the time series to model
      .E <- F_E_use[[54]] # Select the E to use for each species
      .X <- makeblock(ts, .E) # Make explicit matrix of embeddings

      # Now run the S-map function
      S_map_Sugihara1994(
        Y = ts,
        E = .E,
        theta = x,
        X = .X
      )
    }
  ) %>%
  t() %>%
  .[, "RMSE"] %>%
  unlist() %>%
  which.min() %>%
  theta_grid[.]

## This returns 0 whatever index for the three problematic species we use:
#  37, 38 or 54. Hence we are satisfied with our 0 default values
F_E_opt[c(37, 38, 54)] <- F_E_use[c(37, 38, 54)]
F_E_opt <- as.integer(F_E_opt)

F_theta_opt <- numeric(length(spp))
for(i in seq_along(F_theta_opt)){
  F_theta_opt[[i]] <- 
    tryCatch(
      F_optimized_theta[[i]]$theta_opt[[which.min(F_optimized_theta[[i]]$theta_opt)]], 
      error = function(e) paste0("Error with the species: ", spp[[i]])
  )
}
F_theta_opt[c(37, 38, 54)] <- 0
F_theta_opt <- as.numeric(F_theta_opt)

## Finally, make a table with the optimal Es and thetas:
F_opt_params <- data.frame(E = F_E_opt, theta = F_theta_opt)
rownames(F_opt_params) <- spp

saveRDS(F_optimized_theta, "./data/RDS/F_optimized_theta.RDS")
saveRDS(F_opt_params, "./data/RDS/F_opt_params.RDS")
