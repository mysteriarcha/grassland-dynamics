my_ccm_boot <- function(pw_ts, .scale = "T", its = 249L){
  
  spp_nms <- colnames(pw_ts)
  if(.scale == "T"){
    E1 <- T_E_use[spp_nms[1]]; E2 <- T_E_use[spp_nms[2]]
  }
  if(.scale == "F") E1 <- F_E_use[spp_nms[2]]; E2 <- F_E_use[spp_nms[2]]
  if(is.null(E1)) stop(".scale must be either 'T' or 'F' (characters)")
  
  res_E1 <- multispatialCCM::CCM_boot(pw_ts[[1]], pw_ts[[2]], E = E1, iterations = its)
  res_E2 <- multispatialCCM::CCM_boot(pw_ts[[2]], pw_ts[[1]], E = E2, iterations = its)
  
  return(list(res_E1, res_E2))
}