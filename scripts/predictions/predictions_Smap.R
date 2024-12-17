
library(pttstability)

source("load_libraries.R")
source("name_interactions.R")
source("transform_data.R")
source("interactions_CCM.R")
source("functions/libs_struct.R")

T_opt_params <- readRDS("./data/RDS/T_opt_params.RDS")
F_opt_params <- readRDS("./data/RDS/F_opt_params.RDS")

T_Smap <- vector("list", length(spp))
for(i in seq_along(T_Smap)){
  T_Smap[[i]] <-
    S_map_Sugihara1994(T_Devin_wide[[i]],
                       E =  T_opt_params$E[i],
                       theta = T_opt_params$theta[i]
    )
}

names(T_Smap) <- spp
T_R2_Smap <- sapply(T_Smap, "[[", "R2")
T_R2_Smap[order(T_R2_Smap)]

# 16 spp have a negative R2, 7 of them smaller than -0.1:
# 1) Dianthus pontederae, 2) Euphorbia cyparissias, 3) Saxifraga tridactylites
# 4) Lotus corniculatus, 5) Inula oculus-christ, 6) Hesperis tristis,
# 7) Carex praecox

T_preds_Smap <- lapply(T_Smap, "[[", "Y_hat")

T_opt_params[names(T_R2_Smap[order(T_R2_Smap)]), ]


?S_map_Sugihara1994

F_Smap <- vector("list", length(spp))
for(i in seq_along(F_Smap)){
  .X <- makeblock(F_ts[[i]], F_opt_params$E[i])
  F_Smap[[i]] <-
    S_map_Sugihara1994(F_ts[[i]],
                       E =  F_opt_params$E[i],
                       theta = F_opt_params$theta[i],
                       lib = libs_struct,
                       X = .X
    )
}
names(F_Smap) <- spp
F_R2_Smap <- sapply(F_Smap, "[[", "R2")
F_R2_Smap[order(F_R2_Smap)]

# 4 spp have a negative R2:
# 1) Thymus praecox
# 2) Lotus corniculatus
# 3) Euphorbia cyparissias
# 4) Scabiosa ochroleuca

# Notably, Dianthus pontederae changes a lot its R2 value
# from very bad at T scale to moderately good at F scale

F_preds_Smap <- lapply(F_Smap, "[[", "Y_hat")


F_preds_Smap$`Seseli osseum` %>% na.omit() %>% plot(type = "b")
ggplot(F_ts %>% select(c(`Seseli osseum`, time, big_plot)), aes(x = time, y = `Seseli osseum`, color = big_plot)) + geom_path()

{
  ind_start <- libs_struct[, 'ind_start']
  F_ts_perturbed <- F_ts
  F_ts_perturbed[ind_start, ] <- 
    F_ts_perturbed[ind_start, ] + rexp(7, 10)
  
  F_Smap_perturbed <- vector("list", length(spp))
  for(i in seq_along(F_Smap_perturbed)){
    .X <- makeblock(F_ts_perturbed[[i]], F_opt_params$E[i])
    F_Smap_perturbed[[i]] <-
      S_map_Sugihara1994(F_ts_perturbed[[i]],
                         E =  F_opt_params$E[i],
                         theta = F_opt_params$theta[i],
                         lib = libs_struct,
                         X = .X
      )
  }
  names(F_Smap_perturbed) <- spp
  F_R2_Smap_perturbed <- sapply(F_Smap_perturbed, "[[", "R2")
  F_R2_Smap_perturbed[order(F_R2_Smap_perturbed)]
  F_preds_Smap_perturbed <- sapply(F_Smap_perturbed, "[[", "Y_hat")
}

spp

Reduce(c, F_preds_Smap) %>% 
  unlist() %>% 
  cbind(., 
        Reduce(c, F_preds_Smap_perturbed), 
        rep(spp, each = length(F_preds_Smap[[1]]))
  ) %>% 
  as_tibble() %>%
  rename(., "original_preds" = `.`, "perturbed_preds" = V2, "spp" = V3) %>% 
  mutate(original_preds = as.numeric(original_preds),
         perturbed_preds = as.numeric(original_preds)) %>% 
  filter(spp == "Arrhenatherum elatius") %>% 
  ggplot(aes(x = 1:258, y = original_preds)) +
  geom_point() +
  geom_path() +
  geom_point(aes(y = perturbed_preds), color = "red", shape = 2) +
  geom_path(aes(y = original_preds), color = "red") +
  theme_classic() +
  guides(color = "none")


my_RMSE_Smap <- function(spp, 
                          .scale = "T"){
  if(.scale == "T"){
  ts  <- T_Devin_wide
  params <- T_opt_params
  .ts <- ts[[spp]]
  .E  <- params[spp, "E"]
  .theta <- params[spp, "theta"]
  RMSE_vec <- numeric(20L)
  for(i in seq_along(RMSE_vec)){
    y <- .ts[-c(1:i)]
    # print(y)
    RMSE_vec[[i]] <- 
      S_map_Sugihara1994(
        Y = y,
        E = .E,
        theta = .theta
        )$RMSE
  }
  }
  if(.scale == "F"){
    ts <- F_ts
    params <- F_opt_params
    .ts <- ts[[spp]]
    .E  <- params[spp, "E"]
    .theta <- params[spp, "theta"]
    RMSE_vec <- numeric(20L)
    for(i in seq_along(RMSE_vec)){
      y  <- .ts[-c(1:i)]
      .X <- makeblock(y, .E)
      # print(y)
      RMSE_vec[[i]] <- 
        S_map_Sugihara1994(
          Y = y,
          E = .E,
          theta = .theta,
          X = .X,
          lib = libs_struct
        )$RMSE
    }
  }
  return(RMSE_vec)
}

sapply(spp, 
       function(x){
         tryCatch(my_RMSE_Smap(x), 
                  error = function(e) paste0("Error in: ", x))
         }
       )
# Anthriscus cerefolium is problematic

sapply(spp, 
       function(x){
         tryCatch(my_RMSE_Smap(x, .scale = "F"), 
                  error = function(e) paste0("Error in: ", x))
         }
      )
# No species gives any error at the F scale


dev.off()
par(mfrow = c(1, 2))

{
  target_sp <- "Euphorbia cyparissias"
  plot(my_RMSE_Smap(target_sp, "T"), type = "b", main = "T scale",
       ylab = paste0("S-map RMSE of ", target_sp))
  plot(my_RMSE_Smap(target_sp, "F"), type = "b", main = "F scale",
       ylab = paste0("S-map RMSE of ", target_sp))
}


## Adam's code
libs_struct_2 <- 
  rbind(
    c(1, 36),
    c(37, 72),
    c(73, 108),
    c(109, 144),
    c(145, 180),
    c(181, 216),
    c(217, 252)
  )
my_preds_Smap <- 
  function(ts, 
           .E, 
           .theta, 
           .lib = libs_struct_2,
           steps = 1){
    
    X <- makeblock(ts, 
                   .E
                   )
    # return(X)
    # for(i in 1:7){
    #   for(j in 1:2){
    #     .lib[i,j] <-
    #       if(i+j == 2) .lib[i,j]
    #     else if(i+j == 3) .lib[i,j] <- .lib[i,j]+1
    #     else if(i+j == 4) .lib[i,j] <- .lib[i,j]+2
    #     else if(i+j == 5) .lib[i,j] <- .lib[i,j]+3
    #     else if(i+j == 6) .lib[i,j] <- .lib[i,j]+4
    #     else if(i+j == 7) .lib[i,j] <- .lib[i,j]+5
    #     else if(i+j == 8) .lib[i,j] <- .lib[i,j]+6
    #     else if(i+j == 9) .lib[i,j] <- .lib[i,j]+7
    #   }
    # }
    # return(.lib)
    
    make_X_use <- function(X, 
                           steps = steps){
      
      E_cols <- NCOL(X)-1
      inputs <- 
        replicate(
          7,
          t(
            replicate(
              steps,
              c(1, rgamma(E_cols, shape=.5)
               )
          )
        )
      )
      
      X_list  <- vector("list", 7L)
      
      for(i in 1:7){
        init <- (i-1)*n+i; end <- i*n+(i-1)
        X_list[[i]]  <- X[init:end, ]
        X_list[[i]]  <- rbind(X_list[[i]],
                              inputs[,,i]
                              )
      }
      X_use <- Reduce(rbind, X_list)
      # return(X_use)
      
    }
    
    # return(.lib)
    # return(X)
    X_use <- make_X_use(X, steps)
    # return(X_use)
    .ts <- vector("list", 7L)
    for(i in 1:7){
      init <- (i-1)*n+i; end <- i*n+(i-1)
      .ts[[i]] <- c(ts[init:end], rep(NA, steps))
    }
    return(.ts)
    .ts   <- Reduce(c, .ts)
    
    # return(X_use)
    # return(.lib)
    return(.ts)
    preds <- S_map_Sugihara1994(
      Y = .ts,
      E = .E,
      theta = .theta,
      # lib = .lib,
      X = X_use
    )
    return(preds)
}

tmp <- my_preds_Smap(F_ts$`Achillea collina`,
              F_opt_params$E[1],
              F_opt_params$theta[1],
              .lib = NULL,
              steps = 1)

tmp$Y_hat

tmp2 <-
  S_map_Sugihara1994(
  Y = F_ts$`Achillea collina`,
  E = F_opt_params$E[1],
  theta = F_opt_params$theta[1],
  lib = libs_struct,
  X =   makeblock(F_ts$`Achillea collina`, F_opt_params$E[1])
  )
length(tmp2)

tmp$Y_hat %>% is.na() %>% which()
tmp_lib <- libs_struct + 1; tmp_lib[1,1] <- 1

my_preds_Smap(tmp$Y_hat,
              F_opt_params$E[1],
              F_opt_params$theta[1],
              .lib = tmp_lib,
              steps = 1
              )

for(i in 1:4){
  if(i == 1) Y_use <- F_ts$`Achillea collina`
  TS <- my_preds_Smap(Y_use,
                      F_opt_params$E[1],
                      F_opt_params$theta[1],
                      .lib = libs_struct,
                      steps = i)
  Y_use <- TS$Y_hat
  print(i)
}
sapply(
  1:2,
  function(x){
    my_preds_Smap(F_ts$`Achillea collina`,
    F_opt_params$E[1],
    F_opt_params$theta[1],
    .lib = libs_struct,
    steps = x)
    }
  )

tmp$Y_hat

make_X_use(makeblock(F_ts$`Achillea collina`, 
                     E = F_opt_params$E[1]
))

X_use <- rbind(X, newX)
Y_use <- c(F_ts$`Achillea collina`, rep(NA, nrow(newX)))
sout  <- S_map_Sugihara1994(Y = Y_use, 
                            E = F_opt_params$E[1], 
                            theta = F_opt_params$theta[1],
                            X = X_use,
                            lib = libs_struct)
