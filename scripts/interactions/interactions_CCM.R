# This script makes the list of pw time series and obtains
# the causality and strength of prediction correlation using
# (multispatial) Convergent Cross-Mapping: methods in Clark et al. (2015)

rm(list=ls())

# To obtain general information on the method, run the following line:
# ?multispatialCCM

# Load generic data (data-flow parameters, time series, etc.)
source("setup.R")

# Load the interaction names
source("scripts/name_interactions.R")

# Make the pairwise time series list
T_pw_ts          <- vector("list", NROW(spp_grid))
names(T_pw_ts)   <- nms_interactions
# Fill it in
for(i in seq_along(T_pw_ts)){
  T_pw_ts[[i]] <- T_Devin_wide[, spp_grid[i, ]]
}

# Obtain the correlation values for different embedding dimensions (E)
# from 1 to n_E
T_n_E <- 5L
{
  T_cors_mat <- matrix(0, nrow = length(spp), ncol = T_n_E)
  for(i in 1:T_n_E){
    T_cors_mat[, i] <-
      sapply(
        T_Devin_wide[-ncol(T_Devin_wide)],
        function(x) SSR_pred_boot(x, E = i)$rho
      )
  }
  
  T_E_use <- apply(T_cors_mat, 1, which.max)
  names(T_E_use) <- spp
}

# Now make the pairwise time series for the F-scale with the proper
# format for multispatialCCM (adding an NA between blocks)

# Like in the previous section, get the optimal E
F_n_E <- 5L
{
  F_cors_mat <- matrix(0, nrow = length(spp), ncol = F_n_E)
  for(i in 1:F_n_E){
    F_cors_mat[, i] <-
      sapply(
        F_Devin_wide[1:(ncol(F_Devin_wide)-2)],
        function(x) SSR_pred_boot(x, E = i)$rho
      )
  }
  
  F_E_use <- apply(F_cors_mat, 1, which.max)
  names(F_E_use) <- spp
}

# Make the function for the CCM_boot with the adequate E and bidirectional
# between the two species involded at each pairwise time series
source("functions/my_ccm_boot.R")

