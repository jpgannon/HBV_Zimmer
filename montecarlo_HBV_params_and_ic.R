#run HBV with random parameter and intial condition set
#assuming no snow at start
#return NSE, params, initial conditions
hbv_MC <- function(P, Temp, PET, Qobs){
  
#function to generate random parameter set
  genpars <- function(){
    FC    <- runif(1, min = 40   , max = 400)  #Max soil moisture storage, field capacity
    beta  <- runif(1, min = 1    , max = 6)    #Shape coefficient governing fate of water input to soil moisture storage
    LP    <- runif(1, min = 0.3  , max = 1)    #Threshold for reduction of evap
    SFCF  <- runif(1, min = 0.4  , max = 1.2)  #Snowfall correction factor
    TT    <- runif(1, min = -1.5 , max = 1.2)  #Threshold temperature
    CFMAX <- runif(1, min = 1    , max = 8)    #Degree-day factor
    k0    <- runif(1, min = 0.05 , max = 0.5)  #Recession constant (upper storage, near surface)
    k1    <- runif(1, min = 0.01 , max = 0.3)  #Recession constant (upper storage)
    k2    <- runif(1, min = 0.001, max = 0.15) #Recession constant (lower storage)
    UZL   <- runif(1, min = 0    , max = 70)   #Threshold for shallow storage
    PERC  <- runif(1, min = 0    , max = 4)    #Percolation, max flow from upper to lower storage
    MAXBAS<- rep(1)
    
    c(FC, beta, LP, SFCF, TT, CFMAX, k0, k1, k2, UZL, PERC, MAXBAS)
  }
 
#function to generate random intial conditions 
  gen_init_cond <- function(){
    SWE    <- 0      #initial SWE value
    SUZ    <- runif(1, min = 0    , max = 1000)      #Initial upper zone storage
    SLZ    <- runif(1, min = 0    , max = 1000)        #Initial lower zone storage
    SP     <- 0      #initial value for simulated snowpack 
    WC     <- 0      #Initial liquid water in snowpack 
    SM     <- runif(1, min = 0    , max = 1000)        #Initial soil storage content
    
    c(SWE, SUZ, SLZ, SP, WC, SM)
   }
 
  params <- genpars()
  initcond <- gen_init_cond()

  results <- HBV(params, initcond, P, Temp, PET, routing = 0)
  
  results <- bind_cols(results, Qobs = Qobs)

  #don't trim (commented out) bc its a short timeseries  
#  EvalStart <- floor(length(results$Qobs) * 0.4)
#  EvalEnd <- length(results$Qobs)
#  
#  #trim the first 40% of the record so it isn't included in the NSE calculation
#  results <- results[EvalStart:EvalEnd,]
  
  NSE <- 1 - ((sum((results$Qobs - results$q)^2, na.rm = TRUE))/
                sum((results$Qobs - mean(results$Qobs)) ^ 2, na.rm = TRUE))
  
  return(c(NSE, params, initcond))
}
