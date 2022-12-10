source("HBV.R")

library(tidyverse)
library(lubridate)

#read data
Weather <- read_csv("ArborRelay_Weather_Daily.csv")
NDCQ <- read_csv("NDC_Q_2021_2022_daily.csv")

#this file crashes read_csv for some reason
SDCQ <- read.csv("SDC_Q_2021_2022_daily.csv") |> 
  mutate(datetime = ymd(datetime)) 

#join together for model input
inputdatNDC <- inner_join(Weather, NDCQ, by = "datetime")
inputdatSDC <- inner_join(Weather, SDCQ, by = "datetime")
