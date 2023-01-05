source("HBV_var_init_cond.R")
source("montecarlo_HBV_params_and_ic.R")

library(tidyverse)
library(lubridate)
library(patchwork)

#read data
Weather <- read_csv("ArborRelay_Weather_Daily.csv")

#this file crashes read_csv for some reason
SDCQ <- read.csv("SDC_Q_2021_2022_daily.csv") |> 
  mutate(datetime = ymd(datetime)) 

#join together for model input
inputdatSDC <- inner_join(Weather, SDCQ, by = "datetime") |> 
  select(datetime, P = Rainfall_mm_daily, 
         Temp = AirTemp_C_Mean, PET = PET_Tmean_mm_day,
         Qobs = Streamflow_mm_day)

#run montecarlo and save parameters and initial conditions of best run
runs <- 500000

for(x in 1:runs){
  #run HBV with ramdom params and initial conditions, get NSE val
  NSE_pic <- hbv_MC(P = inputdatSDC$P, Temp = inputdatSDC$Temp, 
                    PET = inputdatSDC$PET, Qobs = inputdatSDC$Qobs)
  
  if(x == 1) bestrun <- NSE_pic
  
  if(x > 1){
    if(NSE_pic[1] > bestrun[1]) bestrun <- NSE_pic
    print(bestrun[1])
  }
}

#retrieve model output for best run
#best from 500000 run MC

HBVout <- HBV(pars = bestrun[2:13], initcond =  bestrun[14:19], 
              P = inputdatSDC$P,
              Temp = inputdatSDC$Temp,
              PET = inputdatSDC$PET)
bestNSE <- bestrun[1]

#add obs discharge
HBVout <- bind_cols(HBVout, Qobs = inputdatSDC$Qobs, datetime = inputdatSDC$datetime)

#plot discharge for best run
qplot <- HBVout |>
  ggplot(aes(x = datetime, y = Qobs, color = "Q observed"))+
  geom_line()+
  geom_line(aes(y = q, color = "Q modeled"))+
  theme_classic()+
  ggtitle(paste("NSE =", bestNSE))

pplot <- 
  ggplot(HBVout, aes(x = datetime, y = P))+
  geom_bar(stat = "identity")+
  theme_classic()

pplot / qplot
