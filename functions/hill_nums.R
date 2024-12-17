

### Make Hill-numbers function:

## These functions don't work (why???)

# get_Hill_nums <- function(p, q) sapply(q, function(q) sum(p^q)^(1/(1-q)))
# 
# get_Hill_nums <- function(p, q){
#   p <- p[p>0]
#   sum(p**q)**(1/1-q)
# }

## Have to use this one, less efficient:
get_hill_nums <- function(sp_ws, q){
  sp_ws <- sp_ws[sp_ws != 0]
  if(abs(q - 1) < 0.005) return(exp(vegan::diversity(sp_ws)))
  sum(sp_ws^q)^(1/(1-q))
}
get_hill_nums <- Vectorize(get_hill_nums, "q")