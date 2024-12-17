rm(list=ls())

require(pttstability)
require(ecostatscale)

?symdynN

set.seed(123122)
# make fake data:
{
  r     <- 1          # rate of recovery
  d     <- (0)        # mean size of disturbance (mu in text)
  d_sd  <- sqrt(0.1)  # SD of disturbances (sigma in text)
  f     <- 1          # average time between disturbances (1/lambda in text)
  sf    <- 0.1        # sampling interval
  tmax  <- 10         # maximum time for simulation
  d_cov <- 0          # covariance in disturbances among species
  amu   <- (-0.5)     # average interaction coefficient
  asd   <- 0.1        # standard deviation of interaction coefficient
  N     =  5          # number of species
}

symdynN

dat <- symdynN(r = r, 
               amu = amu, 
               asd = asd, 
               f = f, d = d,
               d_sd = d_sd, 
               d_cov = d_cov, 
               N = N,
               sf = sf, 
               tmax = tmax,
               stocht = T,
               xstart = rep(-1, N), 
               fullout = TRUE
               )
SpM <- dat$datout[,-c(1:2)]+1 # just species data

matplot(dat$datout[,"time"], SpM, type = "l", xlab = "time", ylab = "abundance")
abline(h=c(0,1), lty=2)


parout = data.frame(species = 1:N,
                    r = NA,
                    alphaii = NA,
                    alphaij = NA)
# get interactionns through s-mapping
for(i in 1:N) { # for each species
  Y = SpM[-1,i] #abundande of species i at time t+1
  X = cbind(1, SpM[-nrow(SpM),i], as.matrix(SpM[-nrow(SpM),-i])) #abundance of all species at time t
  
  # theta>0 assumes model parameters varay with time
  # theta=0 assumes a single value across all time steps (differences caused only by cross-validation)
  smap_out = S_map_Sugihara1994(Y = Y, X = X, E = ncol(X)-1, theta = 0)
  parout[i,"r"] = mean(smap_out$C[,1]) # discrete-time growth rate estimate for each time-step
  parout[i,"alphaii"] = mean(smap_out$C[,2]) # self-limitation term for each time-step
  parout[i,"alphaij"] = mean(smap_out$C[,-c(1:2)]) # competition term for each time-step
}

parout # estimates from s-mapping
dat$A  # true alpha values - note, not directly comparable, since symdynN is continuous time (and linear)


## example with theta > 0
{
  i = 1
  Y = SpM[-1,i] #abundande of species i at time t+1
  X = cbind(1, SpM[-nrow(SpM),i], as.matrix(SpM[-nrow(SpM),-i])) #abundance of all species at time t
}

# find best theta
optout = optimize(f = function(x) {S_map_Sugihara1994(Y = Y, X = X, E = ncol(X)-1, theta = x)$RMSE}, interval = c(0,10))
smap_out = S_map_Sugihara1994(Y = Y, X = X, E = ncol(X)-1, theta = optout$minimum)

# predict dynamics
plot(smap_out$Y, Y, xlab = "prediction", ylab = "observation")
abline(a=0, b=1, lty=2)

# show r vs. time for species i
plot(dat$datout[-1,"time"], smap_out$C[,1], type = "l", xlab = "time", ylab = "r")

# show alpha_1,2 vs. abundance of species 2
plot(SpM[-1,2], smap_out$C[,3], type = "l", xlab = "N_2", ylab = "alpha_1,2")




