## Script to give interactions names and be able to retreive species names
#  from them

# species pairwise interactions
spp              <- colnames(T_Devin_wide)[1:(ncol(P_Devin_wide)-3)]
spp_grid         <- combn(spp, 2) |> t()
nms_interactions <- paste(spp_grid[,1], spp_grid[,2], sep = " & ")
