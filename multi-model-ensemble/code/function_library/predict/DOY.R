#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: make predictions using DOY model for chla

library(mgcv)

#'Function to predict chl-a using DOY model
#'@param data data frame with columns DateTime (yyyy-mm-dd hh:mm:ss) and
#'EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

DOY <- function(data, pred_dates, forecast_horizon){
  
  #Fit model
  #assign target and predictors
  df <- data %>%
    filter(!Date %in% pred_dates) %>%
    mutate(doy = yday(Date)) %>%
    select(doy, Chla_ugL)
  colnames(df) <- c("x","y")
  
  #fit GAM following methods in ggplot()
  my.gam <- mgcv::gam(formula = y ~ s(x, bs = "cs"), family = gaussian(),
                      data = df, method = "REML")
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #subset to reference_datetime and identify doy
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    doy = yday(forecast_dates)

    #set up dataframe for today's prediction
    temp.df <- data.frame(model_id = "DOY",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = forecast_dates,
                          variable = "chlorophyll-a",
                          prediction = rep(NA,forecast_horizon+1))
    
    for(h in 1:(forecast_horizon+1)){
      
      #make prediction
      temp.df$prediction = predict.gam(my.gam, data.frame(x=doy))
      
    } #end of today's prediction loop
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
