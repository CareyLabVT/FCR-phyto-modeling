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
dat_DOY_chla <- format_data_DOY_chla()
dat_ARIMA <- format_data_ARIMA()

#Write processed data to file
write.csv(obs, "./multi-model-ensemble/data/data_processed/chla_obs.csv",row.names = FALSE)
write.csv(dat_persistence, "./multi-model-ensemble/data/data_processed/persistence.csv",row.names = FALSE)
write.csv(dat_DOY_chla, "./multi-model-ensemble/data/data_processed/DOY.csv",row.names = FALSE)
write.csv(dat_ARIMA, "./multi-model-ensemble/data/data_processed/ARIMA.csv",row.names = FALSE)

