#Fit process models for chl-a
#Author: Mary Lofton
#Date: 19APR23

#Purpose: fit process models model for chla from 2018-2021

library(tidyverse)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_processModels <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    filter(Date >= start_cal & Date <= stop_cal) 
  
  #source process model functions
  source("./multi-model-ensemble/code/function_library/fit_models/processModelFunctions.R")
  
  chla = df$Chla_ugL
  wtemp = df$WaterTemp_C
  swr = df$Shortwave_Wm2
  
  par <- c(2, 15, 22, 0.4, 100, 0.8, 3)
  
  fit <- optim(par = par, fn = LL_fn, method = "Nelder-Mead", chla = chla, 
               wtemp = wtemp, swr = swr, hessian = FALSE)
  
  pred_chla = proc_model(par = fit$par, wtemp, chla, swr)
  plot(df$Date, chla)
  lines(df$Date, pred_chla, col = "red")
  
  ARIMA_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = Date, y = Chla_ugL, fill = "obs"))+
    geom_line(data = fitted_values, aes(x = Date, y = .fitted, color = "ARIMA"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(Date >= start_cal & Date <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "ARIMA",
                       datetime = dates$Date,
                       variable = "chlorophyll-a",
                       prediction = fitted_values$.fitted)

  
  #return output + model with best fit + plot
  return(list(out = df.out, ARIMA = my.arima, plot = ARIMA_plot))
}
