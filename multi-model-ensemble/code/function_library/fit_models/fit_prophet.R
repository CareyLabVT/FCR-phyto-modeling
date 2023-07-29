#Fit prophet model for chl-a
#Author: Mary Lofton
#Date: 28JUL23

#Purpose: fit prophet model for chla from 2018-2021

library(prophet)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_prophet <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    filter(Date >= start_cal & Date <= stop_cal) %>%
    select(-Flag_Chla_ugL) %>%
    rename(ds = Date,
           y = Chla_ugL)
  
  #fit prophet model 
  my.prophet <- prophet(df) 
  
  future <- make_future_dataframe(my.prophet, periods = 365)
  
  forecast <- predict(my.prophet, future) %>%
    filter(ds <= "2021-12-31")
  
  prophet_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = ds, y = y, fill = "obs"))+
    geom_line(data = forecast, aes(x = as.Date(ds), y = yhat, color = "prophet"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #get list of calibration dates
  dates <- data %>%
    filter(Date >= start_cal & Date <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "prophet",
                       datetime = dates$Date,
                       variable = "chlorophyll-a",
                       prediction = forecast$yhat)

  
  #return output + model with best fit + plot
  return(list(out = df.out, prophet = my.prophet, plot = prophet_plot))
}
