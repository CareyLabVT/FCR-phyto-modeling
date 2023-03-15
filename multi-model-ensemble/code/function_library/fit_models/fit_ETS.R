#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: fit ARIMA model for chla from 2018-2021

library(fable)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_ETS <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- as_tsibble(data) %>%
    filter(Date >= start_cal & Date <= stop_cal) 
  
  #fit ARIMA from fable package
  my.ets <- df %>%
    model(ets = fable::ETS(Chla_ugL)) 
  fitted_values <- fitted(my.ets)
  
  ETS_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = Date, y = Chla_ugL, fill = "obs"))+
    geom_line(data = fitted_values, aes(x = Date, y = .fitted, color = "ETS"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(Date >= start_cal & Date <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "ETS",
                       datetime = dates$Date,
                       variable = "chlorophyll-a",
                       prediction = fitted_values$.fitted)

  
  #return output + model with best fit + plot
  return(list(out = df.out, ETS = my.ets, plot = ETS_plot))
}
