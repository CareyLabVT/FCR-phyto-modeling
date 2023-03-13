#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: fit DOY model for chla from 2018-2021

library(mgcv)

#'Function to fit day of year model for chla
#'@param chla_ts data frame with columns DateTime (yyyy-mm-dd hh:mm:ss) and
#'EXO_chla_ugL_1 with chl-a measurements in ug/L

fit_DOY_chla <- function(chla_ts, max_order){
  
  #assign target and predictors
  df <- chla_ts %>%
    mutate(doy = yday(Date)) %>%
    select(doy, Chla_ugL)
  colnames(df) <- c("x","y")
  
  #assign max order
  poly_max = max_order
  
  #fit polynomial curves with various orders
  fit_curves <- function(poly_max = poly_max,
                         df = df){
  
    poly <- c(1:poly_max)
    
    curve_list <- list()
  
  for(p in 1:length(poly)){
    if(p == 1){
      fit <- lm(y~x, data=df)
    } else {
      fit <- lm(y~poly(x,poly[p],raw=TRUE), data=df)
    }
    curve_list[[p]] <- fit
  }
    
    return(curve_list)

  }
  fits <- fit_curves(poly_max = poly_max, df = df)
  
  #plot
  plot_DOY_chla <- function(df = df, 
                            fits  = fits,
                            x_axis = df$x){
    #make plot with each fit as a line
  plot(df$x, df$y, pch=19, xlab='x', ylab='y')
    
    #set colors
    color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    my.cols = sample(color, poly_max)
    
    #add lines
    for(l in 1:length(fits)){
      
      curr_fit = fits[[l]]
  lines(x_axis, predict(curr_fit, x=x_axis), col=my.cols[l])
  
    }
  }
  DOY_plot <- plot_DOY_chla(df = df, 
                            fits  = fits,
                            x_axis = df$x)
  
  #calculate RMSE
  calculate_RMSE <- function(df = df, 
                            fits = fits,
                            poly_max = poly_max,
                            x_axis = df$x){
    
    #create table to populate with RMSE for each fit
    table <- data.frame(fit = c(1:poly_max),
                        rmse = NA)
      
    #calculate RMSE for each fit
    for(f in 1:poly_max){
      predicted <- predict(fits[[f]], x=x_axis)
      rmse <- sqrt(mean((df$y - predicted)^2))
      table[f,2] <- rmse
    }
    
    return(table)
  }
  
  RMSE_table <- calculate_RMSE(df = df, 
                               fits = fits,
                               poly_max = poly_max,
                               x_axis = df$x)
  
  #try with GAM
  my.gam <- mgcv::gam(formula = y ~ s(x, bs = "cs"), family = gaussian(),
                      data = df, method = "REML")
  GAM_plot <- ggplot(data = df, aes(x = x, y = y))+
    geom_point()+
    geom_smooth()+
    theme_classic()
  GAM_predicted <- predict(my.gam, x=df$x)
  GAM_rmse <- sqrt(mean((df$y - GAM_predicted)^2))

  
  #return model with best fit
  return(list(DOY_plot = DOY_plot, fits = fits, RMSE_table = RMSE_table,
              gam = my.gam, GAM_plot = GAM_plot, GAM_rmse = GAM_rmse))
}
