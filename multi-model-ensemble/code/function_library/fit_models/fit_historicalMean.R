#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: fit DOY model for chla from 2018-2021

library(mgcv)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_DOY_chla <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    filter(Date >= start_cal & Date <= stop_cal) 
  
  #calculate historical mean
  historicalMean <- mean(df$Chla_ugL, na.rm = TRUE)
  
  HM_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = Date, y = Chla_ugL, fill = "obs"))+
    geom_hline(yintercept = historicalMean, aes(color = "historical mean"))+
    theme_classic()+
    labs(color = NULL, fill = NULL)

  
  #get list of calibration dates
  dates <- data %>%
    filter(Date >= start_cal & Date <= stop_cal)
  
  #build output df
  df.out <- data.frame(model_id = "DOY",
                       datetime = dates$Date,
                       variable = "chlorophyll-a",
                       prediction = historicalMean)

  
  #return output + model with best fit + plot
  return(list(out = df.out, historicalMean = historicalMean, plot = HM_plot))
}
