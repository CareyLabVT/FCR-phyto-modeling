#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: fit DOY model for chla from 2018-2021

#'Function to fit day of year model for chla
#'@param chla_ts data frame with columns DateTime (yyyy-mm-dd hh:mm:ss) and
#'EXO_chla_ugL_1 with chl-a measurements in ug/L

fit_DOY_chla <- function(chla_ts){
  
  #aggregate to daily median
  
  #fit polynomial curves with various orders
  fit1 <- lm(y~x, data=df)
  fit2 <- lm(y~poly(x,2,raw=TRUE), data=df)
  fit3 <- lm(y~poly(x,3,raw=TRUE), data=df)
  fit4 <- lm(y~poly(x,4,raw=TRUE), data=df)
  fit5 <- lm(y~poly(x,5,raw=TRUE), data=df)
  
  #plot
  plot_DOY_chla <- function(processed_chla_ts = chla_ts2, 
                            fit01 = fit1, 
                            fit02 = fit2, 
                            fit03 = fit3, 
                            fit04 = fit4, 
                            fit05 = fit5){
  plot(df$x, df$y, pch=19, xlab='x', ylab='y')
  lines(x_axis, predict(fit1, data.frame(x=x_axis)), col='green')
  lines(x_axis, predict(fit2, data.frame(x=x_axis)), col='red')
  lines(x_axis, predict(fit3, data.frame(x=x_axis)), col='purple')
  lines(x_axis, predict(fit4, data.frame(x=x_axis)), col='blue')
  lines(x_axis, predict(fit5, data.frame(x=x_axis)), col='orange')
  }
  DOY_plot <- plot_DOY_chla(processed_chla_ts = chla_ts2, 
                            fit01 = fit1, 
                            fit02 = fit2, 
                            fit03 = fit3, 
                            fit04 = fit4, 
                            fit05 = fit5)
  
  #return model with best fit
  return(list(DOY_plot = DOY_plot, fit1 = fit1, fit2 = fit2, fit3 = fit3, fit4 = fit4, fit5 = fit5))
}