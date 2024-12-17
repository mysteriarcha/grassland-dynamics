get_F_ts_libs <- function(ts = F_ts){
  F_ts <- F_ts[[1]]
  NA_inds <- which(is.na(F_ts))
  ind_start <- c(1, NA_inds + 1)
  ind_finish <- c(NA_inds-1, NROW(F_ts))
  cbind(ind_start, ind_finish)
}
# Store the default results in an object
libs_struct <- get_F_ts_libs()
rownames(libs_struct) <- paste0("block_", 1:7)
