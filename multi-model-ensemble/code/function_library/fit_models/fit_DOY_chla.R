#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: fit DOY model for chla from 2018-2021

library(mgcv)

#'Function to fit day of year model for chla
#'@param chla_ts data frame with columns DateTime (yyyy-mm-dd hh:mm:ss) and
#'EXO_chla_ugL_1 with chl-a measurements in ug/L

fit_DOY_chla <- function(chla_ts){
  
  #assign target and predictors
  df <- chla_ts %>%
    mutate(doy = yday(Date)) %>%
    select(doy, Chla_ugL)
  colnames(df) <- c("x","y")
  
  #fit GAM following methods in ggplot()
  my.gam <- mgcv::gam(formula = y ~ s(x, bs = "cs"), family = gaussian(),
                      data = df, method = "REML")
  GAM_plot <- ggplot(data = df, aes(x = x, y = y))+
    geom_point()+
    geom_smooth()+
    theme_classic()
  GAM_predicted <- predict(my.gam, x=df$x)
  GAM_rmse <- sqrt(mean((df$y - GAM_predicted)^2))

  
  #return model with best fit
  return(list(gam = my.gam, GAM_plot = GAM_plot, GAM_rmse = GAM_rmse))
}
