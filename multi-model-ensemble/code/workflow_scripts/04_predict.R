#Predict chl-a
#Author: Mary Lofton
#Date: 13MAR23

#Purpose: Predict chl-a in Falling Creek Reservoir in 2022 using a suite of models
#1-7 days into the future

library(tidyverse)
library(lubridate)

#Load prediction functions
fit.model.functions <- list.files("./multi-model-ensemble/code/function_library/predict")
sapply(paste0("./multi-model-ensemble/code/function_library/predict/",fit.model.functions),source,.GlobalEnv)

#Read in data
dat_persistence <- read_csv("./multi-model-ensemble/data/data_processed/persistence.csv")
dat_DOY_chla <- read_csv("./multi-model-ensemble/data/data_processed/DOY.csv")

#Set prediction window and forecast horizon
pred_dates <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2022-12-31"), by = "day")
forecast_horizon = 7

#Predict chl-a
#Each function should take processed data, pred_dates, and forecast_horizon
#Each function should subset to pred_dates, run a forecast with max horizon of
#forecast_horizon for each date, and return a dataframe with the following structure:
#Model: name of model
#referenceDate: forecast issue date (will be list of pred_dates)
#Date: date of forecast (will go from pred_date[i] + 1 to pred_date[i] + 7 for each
#referenceDate)
#Chla_ugL: predicted value of chl-a

pred_persistence <- persistence(data = dat_persistence,
                                pred_dates = pred_dates,
                                forecast_horizon = forecast_horizon)

pred_DOY <- DOY(data = dat_DOY_chla,
                pred_dates = pred_dates,
                forecast_horizon = forecast_horizon)

#Stack model output and write to file
mod_output <- bind_rows(pred_persistence, pred_DOY)

write.csv(mod_output, "./multi-model-ensemble/model_output/validation_output.csv", row.names = FALSE)

