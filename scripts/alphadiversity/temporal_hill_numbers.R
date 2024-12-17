# Compute the alpha diversity of each plot:
hill_nums_P <- matrix(0, nrow = NROW(P_Devin_wide_prop), ncol = 100L)
hill_nums_F <- matrix(0, nrow = NROW(F_Devin_wide_prop), ncol = 100L)
hill_nums_T <- matrix(0, nrow = n, ncol = 100L)

for(i in 1:NROW(hill_nums_P)){
  hill_nums_P[i, ] <- get_hill_nums(
    sp_ws = P_Devin_wide_prop[i, 1:n_common_spp],
    q = seq(0,3,l=1e2))
}

for(i in 1:NROW(hill_nums_F)){
  hill_nums_F[i, ] <- get_hill_nums(
    F_Devin_wide_prop[i, 1:n_common_spp],
    q = seq(0, 3, l=1e2))
}

for(i in 1:NROW(hill_nums_T)){
  hill_nums_T[i, ] <- get_hill_nums(
    T_Devin_wide_prop[i, 1:n_common_spp],
    q = seq(0, 3, l=1e2))
}