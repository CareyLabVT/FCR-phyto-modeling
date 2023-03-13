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

#Load model fitting functions
fit.model.functions <- list.files("./multi-model-ensemble/code/function_library/fit_models")
sapply(paste0("./multi-model-ensemble/code/function_library/fit_models/",fit.model.functions),source,.GlobalEnv)

#Format data
dat_persistence <- format_data_persistence()
dat_DOY_chla <- format_data_DOY_chla()

#Fit models (not applicable for persistence model)
DOY.chla <- fit_DOY_chla(chla_ts = dat_DOY_chla, max_order = 100)
DOY.chla$RMSE_table

