
T_opt_params <- readRDS("./data/RDS/T_opt_params.RDS")
F_opt_params <- readRDS("./data/RDS/F_opt_params.RDS")

# Get the interaction coefficients from a modified S-map.

## From Taken's theorem, to know how a species A ("focal species") interacts 
#  with B, C, D..., we can use, instead of the lagged observations of A for 
#  the embedding, the NON-lagged observations of the species B, C, D...
## We call the B, C, D... especies the "target species".

## Hence, given our normal S-map has a structure built over an X embedding matrix,
#  let's modify it, for each focal species, as built on the abundances of 
#  species it interacts with (target species)

#  1) Defining the focal species: "Teucrium chamaedrys" (the species with highest E)
focal_sp <- "Teucrium chamaedrys"

#  2) Definition of the embedding matrix with delayed values:
## In the default case (i.e. self-embedding), we would make:
.X <- makeblock(F_ts[[focal_sp]], F_opt_params[focal_sp, "E"])
## However, as we want to get the interspecific coefficients (i.e. from the 
#  target species to the focal species), we make find which are the actual
#  target species of our focal species:
target_spp <-
  F_interactions[str_detect(rownames(F_interactions), focal_sp), "significant_BtoA"] %>% 
  spp[.]
# And then build the matrix:
.X <- cbind(1, F_ts[[focal_sp]], as.matrix(F_ts[,target_spp]))

#  3) Now we can extract the interaction coefficients:
tmp_smap <-
  S_map_Sugihara1994(F_ts[[focal_sp]],
                     E =  F_opt_params[focal_sp, "E"],
                     theta = F_opt_params[focal_sp, "theta"],
                     lib = libs_struct,
                     X = .
  )

## In the C object we have a 1 + A + (B, C, D...) NCOL matrix, each column
#  being the intercept, the intraspecific, and the interspecific interactions.
#  In our case, Acinos arvensis just interacts with another species, so we can
#  extract this interaction as:

# list_Es_kawatsu <- vector("list", length(spp))
# for(i in seq_along(list_Es_kawatsu)){
#   list_Es_kawatsu[[i]] <- 
#     andsr::find_best_dim(F_ts[, i, drop = F], cols = 1, lib = libs_struct)
# }
# Es_kawatsu <- Reduce(rbind, list_Es_kawatsu)
# Es_kawatsu$spp <- spp
# Es_kawatsu <- select(Es_kawatsu, c(spp, everything()))
# saveRDS(Es_kawatsu, "./data/RDS/Es_kawatsu")
Es_kawatsu <- readRDS("./data/RDS/Es_kawatsu")

cor(Es_kawatsu$E, F_opt_params$E)
