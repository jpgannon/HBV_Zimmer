#Run HBV, return just negative NSE for optimization routine
hbvKGE <- function(params, dat, obs, routing){
  
  library(hydroGOF)
  
  results <- HBV(params, dat$Precip, dat$Temp, dat$PET, routing = 0)
  
  results <- bind_cols(results, Qobs = obs)
  
  KGEval <- KGE(results$q, results$Qobs)
  
  return(-KGEval)
}
