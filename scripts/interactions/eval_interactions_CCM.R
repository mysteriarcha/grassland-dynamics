## Script to make the evaluation of the interactions as set in 
#  interactions_CCM.R

source("scripts/interactions/interactions_CCM.R")

# RATHER USE PARALLELIZATION, IT TAKES VERY LONG FOR THE F SCALE
library(parallel)
library(foreach)
cl <- parallel::makeCluster(11L)
doParallel::registerDoParallel(cl)

T_pw_boots <- foreach(i = seq_len(length(T_pw_ts)), .combine="rbind") %dopar% {
  tryCatch(my_ccm_boot(pw_ts = T_pw_ts[[i]]), error = function(i) paste("error ocurred at:", i))
}
T_rhos <- 
  apply(T_pw_boots, 1, lapply, "[[", "rho") %>% 
  list2DF() %>%
  t()
T_rhos_mean <- T_rhos; T_rhos_sdev <- T_rhos
for(i in 1:nrow(T_rhos)){
  T_rhos_mean[i, 1] <- mean(unlist(T_rhos[i, 1]), na.rm = T)
  T_rhos_mean[i, 2] <- mean(unlist(T_rhos[i, 2]), na.rm = T)
  T_rhos_sdev[i, 1] <- sd(unlist(T_rhos[i, 1]), na.rm = T)
  T_rhos_sdev[i, 2] <- sd(unlist(T_rhos[i, 2]), na.rm = T)
}
T_interactions_pvalue <-
  apply(T_pw_boots, 1, function(x) ccmtest(x[[1]], x[[2]])) %>% 
  t()
T_significance <- (T_interactions_pvalue < .1)

T_interactions <-
  data.frame(
    mean_cor_AtoB = T_rhos_mean[,1]|> unlist(),
    mean_cor_BtoA = T_rhos_mean[,2]|> unlist(),
    sdev_cor_AtoB = T_rhos_sdev[,1]|> unlist(),
    sdev_cor_BtoA = T_rhos_sdev[,2]|> unlist(),
    p_value_AtoB  = T_interactions_pvalue[,1],
    p_value_BtoA  = T_interactions_pvalue[,2],
    significance_AtoB = T_significance[,1],
    significance_BtoA = T_significance[,2]
  )
rownames(T_interactions) <- nms_interactions

saveRDS(T_pw_boots, "./data/RDS/T_pw_boots.RDS")
saveRDS(T_interactions, "./data/RDS/T_interactions.RDS")

cat("T scale is complete. Results saved.\nProceed to analyze F scale (takes hours)\n")

F_pw_boots <-
  foreach(i = seq_len(length(T_pw_ts)), .combine="rbind") %dopar% {
    tryCatch(my_ccm_boot(pw_ts = F_pw_ts[[i]]), error = function(i) paste("error ocurred at:", i))
  }

F_rhos <- 
  apply(F_pw_boots, 1, lapply, "[[", "rho") %>% 
  list2DF() %>%
  t()
F_rhos_mean <- F_rhos; F_rhos_sdev <- F_rhos

for(i in 1:nrow(F_rhos)){
  F_rhos_mean[i, 1] <- mean(unlist(F_rhos[i, 1]), na.rm = T)
  F_rhos_mean[i, 2] <- mean(unlist(F_rhos[i, 2]), na.rm = T)
  F_rhos_sdev[i, 1] <- sd(unlist(F_rhos[i, 1]), na.rm = T)
  F_rhos_sdev[i, 2] <- sd(unlist(F_rhos[i, 2]), na.rm = T)
}

F_interactions_pvalue <-
  apply(F_pw_boots, 1, function(x) ccmtest(x[[1]], x[[2]])) %>% 
  t()

F_significance <- (F_interactions_pvalue < .1)

F_interactions <-
  data.frame(
    mean_cor_AtoB = F_rhos_mean[,1]|> unlist(),
    mean_cor_BtoA = F_rhos_mean[,2]|> unlist(),
    sdev_cor_AtoB = F_rhos_sdev[,1]|> unlist(),
    sdev_cor_BtoA = F_rhos_sdev[,2]|> unlist(),
    p_value_AtoB  = F_interactions_pvalue[,1],
    p_value_BtoA  = F_interactions_pvalue[,2],
    significant_AtoB = F_significance[,1],
    significant_BtoA = F_significance[,2]
  )
rownames(F_interactions) <- nms_interactions

saveRDS(F_pw_boots, "./data/RDS/F_pw_boots.RDS")
saveRDS(F_interactions, "./data/RDS/F_interactions.RDS")

parallel::stopCluster(cl)

cat("\nAll analyses are finished. Results saved in ./data/RDS/ as RDS objects")