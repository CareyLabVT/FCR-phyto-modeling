#Format data for each model and fit models from 2018-2021
#Author: Mary Lofton
#Date: 13MAR23

#Purpose: Format Falling Creek Data downloaded from EDI to inputs needed for each model
#and fit models from 2018-2021

library(tidyverse)
library(lubridate)

#Load model fitting functions
fit.model.functions <- list.files("./multi-model-ensemble/code/function_library/fit_models")
sapply(paste0("./multi-model-ensemble/code/function_library/fit_models/",fit.model.functions),source,.GlobalEnv)

#Read in data
dat_historicalMean <- read_csv("./multi-model-ensemble/data/data_processed/historicalMean.csv")
dat_DOY <- read_csv("./multi-model-ensemble/data/data_processed/DOY.csv")
dat_ETS <- read_csv("./multi-model-ensemble/data/data_processed/ETS.csv")
dat_ARIMA <- read_csv("./multi-model-ensemble/data/data_processed/ARIMA.csv")
dat_TSLM <- read_csv("./multi-model-ensemble/data/data_processed/TSLM.csv")


#Fit models (not applicable for persistence model)
fit_historicalMean <- fit_historicalMean(data = dat_historicalMean, cal_dates = c("2018-08-06","2021-12-31"))
fit_historicalMean$plot

fit_DOY <- fit_DOY_chla(data = dat_DOY, cal_dates = c("2018-08-06","2021-12-31"))
fit_DOY$plot

fit_ETS <- fit_ETS(data = dat_ETS, cal_dates = c("2018-08-06","2021-12-31"))
fit_ETS$plot

fit_ARIMA <- fit_ARIMA(data = dat_ARIMA, cal_dates = c("2018-08-06","2021-12-31"))
fit_ARIMA$plot

fit_TSLM <- fit_TSLM(data = dat_TSLM, cal_dates = c("2018-08-06","2021-12-31"))
fit_TSLM$plot

#Stack model output and write to file (not applicable for persistence model)
mod_output <- bind_rows(fit_DOY$out, fit_ARIMA$out, fit_ETS$out)

#OR if you only want to run one model
mod_output <- read_csv("./multi-model-ensemble/model_output/calibration_output.csv") %>%
  bind_rows(.,fit_TSLM$out)

write.csv(mod_output, "./multi-model-ensemble/model_output/calibration_output.csv", row.names = FALSE)

