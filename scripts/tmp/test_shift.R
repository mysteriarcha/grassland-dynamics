shift <- function(x, n = 1, blocks = 7){
  
  l <- length(x)
  inits <- c(1, which(is.na(x))+1)
  ends  <- c(which(is.na(x))-1, l)
  
  parts <- vector("list", n)
  NAs   <- sapply(1:n, function(x) rep(NA, x))
  
  for(i in seq_along(n)){
    parts[[i]] <-
      sapply(1:blocks, function(y) c(NAs[[i]], x[inits[y]:ends[y]]))[1:l]
      parts[[i]][ends-1] <- NA
  }
  return(parts)
}
shift(F_ts$`Teucrium chamaedrys`) %>% 
  unlist() %>% 
  is.na() %>% 
  which()
