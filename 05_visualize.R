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
out <- read_csv("./multi-model-ensemble/model_output/validation_output.csv")
obs <- read_csv("./multi-model-ensemble/data/data_processed/chla_obs.csv")

#Set arguments for plotting functions
reference_datetime = "2022-03-14"
forecast_horizon = 7


#Plot data
SevenDayPrediction(observations = obs, 
                         model_output = out, 
                         reference_datetime = reference_datetime, 
                         forecast_horizon = forecast_horizon)

RMSEVsHorizon(observations = obs, 
                          model_output = out, 
                          reference_datetime = reference_datetime, 
                          forecast_horizon = forecast_horizon)



