source("HBV_var_init_cond.R")
source("montecarlo_HBV_params_and_ic.R")

library(tidyverse)
library(lubridate)

#read data
Weather <- read_csv("ArborRelay_Weather_Daily.csv")
NDCQ <- read_csv("NDC_Q_2021_2022_daily.csv")

#this file crashes read_csv for some reason
#SDCQ <- read.csv("SDC_Q_2021_2022_daily.csv") |> 
#  mutate(datetime = ymd(datetime)) 

#join together for model input
inputdatNDC <- inner_join(Weather, NDCQ, by = "datetime")|>
  select(datetime, P = Rainfall_mm_daily, 
         Temp = AirTemp_C_Mean, PET = PET_Tmean_mm_day,
         Qobs = Streamflow_mm_day)

#inputdatSDC <- inner_join(Weather, SDCQ, by = "datetime") |> 
#  select(datetime, P = Rainfall_mm_daily, 
#         Temp = AirTemp_C_Mean, PET = PET_Tmean_mm_day,
#         Qobs = Streamflow_mm_day)

#run montecarlo and save parameters and initial conditions of best run
runs <- 500000

for(x in 1:runs){
    #run HBV with ramdom params and initial conditions, get NSE val
    NSE_pic <- hbv_MC(P = inputdatNDC$P, Temp = inputdatNDC$Temp, 
           PET = inputdatNDC$PET, Qobs = inputdatNDC$Qobs)
  
    if(x == 1) bestrun <- NSE_pic
    
    if(x > 1){
      if(NSE_pic[1] > bestrun[1])
        bestrun <- NSE_pic
    }

    }

#retrieve model output for best run
#best from 500000 run MC
#[1] 7.020108e-01 3.019609e+02 4.028145e+00 3.223260e-01 7.908681e-01 4.296827e-01 4.072993e+00
#[8] 3.271594e-01 1.825658e-01 1.031739e-03 2.579748e+01 3.818635e+00 1.000000e+00 0.000000e+00
#[15] 6.883549e+00 5.937273e+01 0.000000e+00 0.000000e+00 2.778988e+02
HBVout <- HBV(pars = bestrun[2:13], initcond =  bestrun[14:19], 
              P = inputdatNDC$P,
              Temp = inputdatNDC$Temp,
              PET = inputdatNDC$PET)
bestNSE <- bestrun[1]

#add obs discharge
HBVout <- bind_cols(HBVout, Qobs = inputdatNDC$Qobs, datetime = inputdatNDC$datetime)

#plot discharge for best run
HBVout |>
  ggplot(aes(x = datetime, y = Qobs, color = "Q observed"))+
  geom_line()+
  geom_line(aes(y = q, color = "Q modeled"))+
  theme_classic()+
  ggtitle(paste("NSE =", bestNSE))
