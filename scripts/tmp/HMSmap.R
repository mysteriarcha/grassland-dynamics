
## Make HMS-map as defined by Ezguerra and Munch 2024

## Broadly, we need:
#  1) A focal point x_t along the 1, ..., t indices of the time series. 
#  2) A delayed embedding of previous points in the state space from x_t to
#  x_(t-E*tau)
#  3) Then, the embedding map takes, to predict x_(t+1), its delay embedding
#  except its last observation. Hence, x_(t+1) = f(x_t, ..., x_(t-E+1))
#  From a 1st order Taylor expansion, we can approximate the function f as 
#  a local weighted linear regression with an exponential decay kernel. We can
#  do this by exp(-theta**2( ||x* - x_t|| / D )), where D is sqrt(E)*(max(x_t) - min(x_t))
#  NOTE that we are talking about a vectorized x_t, so we are taking its
#  delayed embedding from t. x* is the focal point for the state space reconstruction



#  Make a function to get the the embeddings, or function f(x) in the paper
get_x_point <- function(ts,      # The time series to embed
                        t,       # time index of the focal point
                        E = 1,   # Default, can be optimized later
                        tau = 1  # We will keep this time lag constant
                        ){
  if((t - E*tau) < 1) stop("There is not enough data to make the embedding.\nDiminish dimension E or go further in t")
  emb <- (t-(E*tau))
  x <- ts[t:emb]
  return(x)
}

get_embeddings <- function(ts, t, E = 1){
  sapply((E+1):t,
         function(x) get_x_point(ts, t = x, E = E)
  )
}
{
  x_ind <- 20
  embeddings <- get_embeddings(T_Devin_wide[[spp[[x_ind]]]], t = 36,
                               E = T_opt_params$E[x_ind])
  Smap_coefs <- S_map_Sugihara1994(T_Devin_wide[[spp[[x_ind]]]], 
                                   E = T_opt_params$E[x_ind], 
                                   theta = T_opt_params$theta[x_ind])$C
  Smap_preds <- S_map_Sugihara1994(T_Devin_wide[[spp[[x_ind]]]], 
                                   E = T_opt_params$E[x_ind], 
                                   theta = T_opt_params$theta[x_ind])$Y_hat
  Smap_preds <- na.omit(Smap_preds)
  # Smap_preds[1] <- T_Devin_wide[[spp[[x_ind]]]][1]
  X <- Smap_coefs %*% embeddings
  X[1, ] <- embeddings[2,]
  plot(Smap_preds ~ X[1,])
  abline(a=0,b=1)
}
sapply(1:nrow(X), function(x) cor(X[x, ] , embeddings[2, ]))
X[1, ] <- embeddings[2,]
CSmap <- vector("list", NCOL(X))
for(i in 1:NCOL(X)){
  CSmap[[i]] <- S_map_Sugihara1994(X[,i], E = 1, theta = 0)$C
}

dim(X)

# This object has the 

S_map_weights <- function(x_, x)
  
  