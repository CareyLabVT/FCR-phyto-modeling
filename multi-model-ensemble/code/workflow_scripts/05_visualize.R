#Visualize model output
#Author: Mary Lofton
#Date: 13MAR23

#Purpose: Visualize model output to assess model performance

#load packages
library(tidyverse)
library(lubridate)

#Load plotting functions
plot.functions <- list.files("./multi-model-ensemble/code/function_library/visualization")
sapply(paste0("./multi-model-ensemble/code/function_library/visualization/",plot.functions),source,.GlobalEnv)


#Read in data

#different interp methods
input_li <- read_csv("./multi-model-ensemble/data/data_processed/ARIMA_LinearInterp.csv")
input_doyi <- read_csv("./multi-model-ensemble/data/data_processed/ARIMA_DOYInterp.csv")
input_glmi <- read_csv("./multi-model-ensemble/data/data_processed/ARIMA_GLM-AEDInterp.csv")

#input data for models with predictors
input <- read_csv("./multi-model-ensemble/data/data_processed/ARIMA.csv")

#final dataset
out <- read_csv("./multi-model-ensemble/model_output/validation_output.csv")
obs <- read_csv("./multi-model-ensemble/data/data_processed/chla_obs.csv")

#Set arguments for plotting functions
reference_datetime = "2022-11-30"
forecast_horizon = 7


#Plot 

PlotInputData(input_data = input)

PlotInterpMethods(interp_methods = c("Linear","DOY","GLM-AED"),
                  data_lst = list(input_li,
                                  input_doyi,
                                  input_glmi),
                  interp_vars = c("DIN_ugL","SRP_ugL","LightAttenuation_Kd"))

SevenDayPrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime, 
                         forecast_horizon = forecast_horizon)

RMSEVsHorizon(observations = obs, 
                          model_output = out, 
                          reference_datetime = reference_datetime, 
                          forecast_horizon = forecast_horizon)

OneHorizonTimeseries(observations = obs, 
                                 model_output = out, 
                                 forecast_horizon = 7)


