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
dat_DOY_chla <- read_csv("./multi-model-ensemble/data/data_processed/DOY.csv")

#Fit models (not applicable for persistence model)
DOY.chla <- fit_DOY_chla(data = dat_DOY_chla, cal_dates = c("2018-08-06","2021-12-31"))
DOY.chla$GAM_plot
DOY.chla$GAM_rmse

