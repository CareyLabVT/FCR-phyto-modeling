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
cal <- read_csv("./multi-model-ensemble/model_output/calibration_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM"),"statistical",
                                    ifelse(model_id %in% c("LSTM","XGBoost"),"machine learning","process"))))
out <- read_csv("./multi-model-ensemble/model_output/validation_output.csv") %>%
  mutate(model_type = ifelse(model_id %in% c("DOY","persistence","historical mean"),"null",
                             ifelse(model_id %in% c("ARIMA","ETS","TSLM"),"statistical",
                                    ifelse(model_id %in% c("LSTM","XGBoost"),"machine learning","process"))))
obs <- read_csv("./multi-model-ensemble/data/data_processed/chla_obs.csv")

#Set arguments for plotting functions
reference_datetime = "2022-10-06"
forecast_horizon = 35



#Plot 

p1 <- PlotObservations(observations = obs, pred_only = TRUE)
p1
ggsave(p1, filename = "./multi-model-ensemble/figures/observations2022.png",
       device = "png", height = 3, width = 5, units = "in")

PlotInputData(input_data = input)

PlotInterpMethods(interp_methods = c("Linear","DOY","GLM-AED"),
                  data_lst = list(input_li,
                                  input_doyi,
                                  input_glmi),
                  interp_vars = c("DIN_ugL","SRP_ugL","LightAttenuation_Kd"))

p3 <- PlotModelFits(observations = obs, 
                        predictions = cal, 
                        model_ids = c("DOY","ARIMA","OptimumSteeleNP"))
p3
ggsave(p3, filename = "./multi-model-ensemble/figures/exampleModelFits.png",
       device = "png", height = 3, width = 6, units = "in")

p2 <- ExamplePrediction(observations = obs, 
                        model_output = out, 
                        reference_datetime = reference_datetime, 
                        forecast_horizon = forecast_horizon,
                        model_ids = c("DOY","ARIMA","OptimumSteeleNP"))
p2
ggsave(p2, filename = "./multi-model-ensemble/figures/examplePrediction.png",
       device = "png", height = 3, width = 7, units = "in")

p5 <- RMSEVsHorizon(observations = obs, 
                          model_output = out, 
                          forecast_horizon = forecast_horizon)
p5
ggsave(p5, filename = "./multi-model-ensemble/figures/RMSEvsHorizon.png",
       device = "png", height = 4, width = 7, units = "in")

#need to figure out how to detach legend from this and make it a separate
#plot, then add
# 2022-03-26, 2022, 06-05, 2022-09-21
p4 <- PerformanceRelativeToBloom(observations = obs,
                           model_output = out,
                           variable_name = "chlorophyll-a",
                           max_horizon_past = -35,
                           score = "rmse",
                           focal_dates = c("2022-03-26","2022-06-05","2022-09-21","2022-11-06"),
                           data_plot = FALSE)
p4
ggsave(p4, filename = "./multi-model-ensemble/figures/performanceRelativeToBloom.png",
       device = "png", height = 4, width = 7, units = "in")

OneHorizonTimeseries(observations = obs, 
                                 model_output = out, 
                                 forecast_horizon = 7)


