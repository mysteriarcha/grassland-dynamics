# Function to obtain the optimized theta values from the S-map by using
# RMSE (Root Mean Squared Error) as a criterion.

my_Smap_optimizer <- function(ts, # Time series to fit
                              E)  # Maximum number of embedding dimensions
  { 
  E_range <- 1:E # Create the range of embedding dimensions
  theta_optim <- vector("list", E) # Preallocate vector of optimal thetas
  
  # Iterate theta optimization along the range of embedding dimensions:
  for(i in E_range){
    .X <- makeblock(ts, i) # Create the embedding-delay matrix for E
    theta_optim[[i]] <- 
      optimize(f = function(x){
        S_map_Sugihara1994(
          Y = ts, 
          E = i, 
          theta = x,
          X = .X,
          lib = libs_struct  
        )$RMSE},
        interval = c(0,10) # The search for optimal values will be between 0 and 10.
        # This can be change to include higher (extremely high already) degrees of
        # nonlinearity
      )
  }
  
  # Subset optimal theta values along the embedding dimension range: 
  th_opt <- sapply(theta_optim, "[[", "minimum") 
  
  # Subset the RMSE for each parameter combination:
  RMSE   <- sapply(theta_optim, "[[", "objective")
  
  # Store the combination as a data.frame:
  df <- data.frame("theta_opt" = th_opt, 
                   "RMSE" = RMSE)
  
  return(df)
}
