
## Visualize-analyze interaction networks

{
  source("load_libraries.R")
  source("name_interactions.R")
  source("transform_data.R")
  source("interactions_CCM.R")
  source("functions/libs_struct.R")
}

library(igraph)
library(network)
library(ggnetwork)
library(ggnet)

{
  T_interactions <- readRDS("./data/RDS/T_interactions.RDS")
  F_interactions <- readRDS("./data/RDS/F_interactions.RDS")

  F_real_interactions <-
    F_interactions %>% 
    filter(significant_AtoB | significant_BtoA)
}

spp_merge <- function(spp1, spp2) paste0(spp1, " & ", spp2)

inters_table <-
  cbind(rep(spp, each = length(spp)), rep(spp, length(spp)))
inters_table <-
  inters_table[inters_table[,1] != inters_table[,2], ]
inters_vals <- matrix(ncol = 3, nrow = NROW(inters_table))

match_inters <- 
  function(
    pw_db = F_interactions, 
    inters_db = inters_table
    ){
  sppA    <- inters_db[,1]; sppB <- inters_db[,2]
  spp_int1 <- spp_merge(sppA, sppB)
  spp_int2 <- spp_merge(sppB, sppA)
  cnd <- spp_int1 %in% rownames(pw_db)
  
  # return(cnd)
  
  cor_mean <- ifelse(cnd,
                     pw_db[spp_int1,1],
                     pw_db[spp_int2,2])
  cor_sdev <- ifelse(cnd,
                     pw_db[spp_int1,3],
                     pw_db[spp_int2,4])
  cor_pval <- ifelse(cnd,
                     pw_db[spp_int1,5],
                     pw_db[spp_int2,6])
  
  return(data.frame("cor_mean" = cor_mean,
                    "cor_sdev" = cor_sdev,
                    "cor_pval" = cor_pval))
}

F_interactions_tidy <-
  cbind(
  "sppA" = inters_table[,1], 
  "sppB" = inters_table[, 2],
  match_inters()
  ) %>% 
  as.data.frame()

F_interactions_tidy_sig <- F_interactions_tidy %>% filter(cor_pval < .1)

make_incidence_mat <- function(
    inters = F_interactions_tidy, 
    p_val = .1
    ){
  
  spp_list <- unique(F_interactions_tidy$sppA)
  spp_n <- length(spp_list)
  mat <- matrix(0, nrow = spp_n, ncol = spp_n)
  colnames(mat) <- spp_list; rownames(mat) <- colnames(mat)
  
  for(i in spp_list){
    for(j in spp_list){
      if(j == i) next
      mat[i, j] <-
        if(
          (inters %>% filter(sppA == i & sppB == j) %>% .[["cor_pval"]]) < p_val
        ) inters %>% filter(sppA == i & sppB == j) %>% .[["cor_mean"]]
        else 0L
      # print(inters %>% filter(sppA == i & sppB == j) %>% .[["cor_pval"]])
    }
  }
  return(mat)
}


g <-
  network(
    make_incidence_mat(p_val = .1),
    matrix.type = "adjacency",
    ignore.eval = FALSE,
    names.eval = "weights"
  )


ggnet2(g, mode = "kamadakawai", label = TRUE)

