# Make a function to detect which individual species is in each interaction
spp_detect <- function(spp_names, position = 1){
  spp_nms  <- str_split(spp_names, "\\&", simplify = TRUE)
  nm <- spp_nms[position]
  nm <- str_remove(nm, "\ $|(^\ )")
  return(nm)
}
