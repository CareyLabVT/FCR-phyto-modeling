#Format data for each model and fit models from 2018-2021
#Author: Mary Lofton
#Date: 13MAR23

#Purpose: Format Falling Creek Data downloaded from EDI to inputs needed for each model
#and fit models from 2018-2021

library(tidyverse)
library(lubridate)

#Load data formatting functions
data.format.functions <- list.files("./multi-model-ensemble/code/function_library/format_data")
sapply(paste0("./multi-model-ensemble/code/function_library/format_data/", data.format.functions),source,.GlobalEnv)

#Format data
obs <- format_chla_obs()
dat_persistence <- format_data_persistence()
dat_historicalMean <- format_data_historicalMean()
dat_DOY_chla <- format_data_DOY_chla()
dat_ARIMA <- format_data_ARIMA()
dat_ETS <- format_data_ETS()

################################################################################
#Temporary kludge for input data until have either 2022 observed chemistry or
#GLM-AED output in hand: linear interpolation until 2021, then GLM-AED output for
#2021 used as driver data for 2022 for DIN and SRP
input_li <- read_csv("./multi-model-ensemble/data/data_processed/ARIMA_LinearInterp.csv")
input_glmi <- read_csv("./multi-model-ensemble/data/data_processed/ARIMA_GLM-AEDInterp.csv") 
dates_2022 <- as.Date(unlist(c(input_li[which(year(input_li$Date) == 2022),"Date"])))
dates_2021 <- as.Date(unlist(c(input_li[which(year(input_li$Date) == 2021),"Date"])))

#sub in GLM-AED 2021 output for DIN and SRP for 2022
input_li[which(input_li$Date %in% dates_2022),"DIN_ugL"] <- input_glmi[which(input_li$Date %in% dates_2021),"DIN_ugL"]
input_li[which(input_li$Date %in% dates_2022),"SRP_ugL"] <- input_glmi[which(input_li$Date %in% dates_2021),"SRP_ugL"]
dat_ARIMA <- input_li
#end kludge
################################################################################

#Write processed data to file
write.csv(obs, "./multi-model-ensemble/data/data_processed/chla_obs.csv",row.names = FALSE)
write.csv(dat_persistence, "./multi-model-ensemble/data/data_processed/persistence.csv",row.names = FALSE)
write.csv(dat_historicalMean, "./multi-model-ensemble/data/data_processed/historicalMean.csv",row.names = FALSE)
write.csv(dat_DOY_chla, "./multi-model-ensemble/data/data_processed/DOY.csv",row.names = FALSE)
write.csv(dat_ARIMA, "./multi-model-ensemble/data/data_processed/ARIMA.csv",row.names = FALSE)
write.csv(dat_ETS, "./multi-model-ensemble/data/data_processed/ETS.csv",row.names = FALSE)

