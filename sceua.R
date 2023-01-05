
#read data
Weather <- read_csv("ArborRelay_Weather_Daily.csv")

#Run this to optimize NDC
#wsinfo <- read_csv("NDC_Q_2021_2022_daily.csv")

#Run this to optimize SDC
wsinfo <- read.csv("SDC_Q_2021_2022_daily.csv") |> 
  mutate(datetime = ymd(datetime)) 

#join together for model input
dat <- inner_join(Weather, wsinfo, by = "datetime")|>
  select(datetime, Precip = Rainfall_mm_daily, 
         Temp = AirTemp_C_Mean, PET = PET_Tmean_mm_day,
         Qobs = Streamflow_mm_day)
fulldat <- dat

obs <- dat$Qobs

dat <- select(dat, -Qobs, -datetime)

source("HBV_var_init_cond.R")

library(rtop)

#source("hbvnse.R")
source("hbvKG.R")

lowpar <-  c(40,  1,   0.3, 0.4, -1.5 , 1,  0.05, 0.01, 0.001,  0,0,0,0,0,0,0,0)   
highpar <- c(400, 6,   1,   1.2,  1.2 , 8,  0.5,  0.03, 0.15,  70,4,.001,1000,1000,0.001,.001,1000)

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

guessp <- c(FC, beta, LP, SFCF, TT, CFMAX, k0, k1, k2, UZL, PERC)

SWE    <- 0      #initial SWE value
SUZ    <- runif(1, min = 0    , max = 1000)      #Initial upper zone storage
SLZ    <- runif(1, min = 0    , max = 1000)        #Initial lower zone storage
SP     <- 0      #initial value for simulated snowpack 
WC     <- 0      #Initial liquid water in snowpack 
SM     <- runif(1, min = 0    , max = 1000)        #Initial soil storage content

guessp <- c(guessp,SWE, SUZ, SLZ, SP, WC, SM)

sceuaout <- sceua(hbvnse, 
                  pars = sceuaout$par, #guessp, 
                  lower = lowpar, 
                  upper = highpar, 
                  dat = dat, 
                  obs = obs, 
                  routing = 0,
                  maxn = 1000000)

HBVout <- HBV(sceuaout$par, dat$Precip, dat$Temp, dat$PET, routing = 0)

results <- bind_cols(HBVout, Qobs = obs, datetime = fulldat$datetime)

NSE <- 1 - ((sum((results$Qobs - results$q)^2, na.rm = TRUE))/
              sum((results$Qobs - mean(results$Qobs)) ^ 2, na.rm = TRUE))

ggplot(results, aes(x = datetime, y = q, col = "Modeled"))+
  geom_line()+
  geom_line(aes(x = datetime, y = Qobs, col = "Observed"))+
  theme_classic()+
  ggtitle(paste("NSE =",NSE))

#SCUEA pars for SDC
#[1] 3.894441e+02 2.805701e+00 3.060249e-01 8.294203e-01 3.894554e-01 1.409295e+00 2.586264e-01
#[8] 2.998523e-02 1.015219e-03 2.737420e+01 3.828068e+00 5.560927e-05 2.865512e+00 1.459548e-01
#[15] 9.025751e-04 2.680626e-04 1.799262e+02

#SCUEA pars for NDC
#[1]  3.938357e+02  3.856892e+00  4.185099e-01  1.181043e+00 -4.735899e-02  6.809862e+00
#[7]  4.228831e-01  1.004853e-02  1.005214e-03  3.397284e+01  1.692910e+00  7.139913e-04
#[13]  3.078851e+01  1.418214e+01  1.242340e-04  6.305947e-04  2.962834e+02