#Predict chl-a with Optimum Monod model
#Author: Mary Lofton
#Date: 31MAY23

#Purpose: make predictions using OptimumMonod model for chla

#'Function to predict chl-a using DOY model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions
#'@param fit trimmed JAGS object w/ parameter values

OptimumSteele <- function(data, pred_dates, forecast_horizon, fit){
  
  #assign target and predictors
  df <- as_tsibble(data) %>%
    filter(Date < pred_dates[1]) 
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  #name parameters
  Topt <- fit$summary$statistics[2,1]
  I_S <- fit$summary$statistics[3,1]
  R_growth <- fit$summary$statistics[4,1]
  R_resp <- fit$summary$statistics[5,1]
  
  for(t in 1:length(pred_dates)){
    
    #subset to reference_datetime 
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]+1), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    #name drivers
    drivers = as_tsibble(data) %>%
      filter(Date %in% forecast_dates)
    wtemp <- drivers$WaterTemp_C
    swr <- drivers$Shortwave_Wm2

    #set initial conditions
    curr_chla <- data %>%
      filter(Date == pred_dates[t]) %>%
      pull(Chla_ugL)
    
    #generate predictions
    pred <- c(curr_chla)
    for(i in 2:(length(forecast_dates)+1)){
    pred[i] = pred[i-1] + (pred[i-1] * R_growth * (((wtemp[i-1] - 0) / (Topt - 0)) *((100 - wtemp[i-1]) / (100 - Topt)) ^((100 - Topt) / (Topt - 0))) * ((swr[i-1]/I_S) * exp(1 - (swr[i-1]/I_S))) * (DIN[i-1]/(DIN[i-1]+ksdin)) * (SRP[i-1]/(SRP[i-1]+kssrp))) - (pred[i-1] * R_resp * (1.08^(wtemp[i-1] - 20))) 
    }
    
    #set up dataframe for today's prediction
    temp.df <- data.frame(model_id = "OptimumSteele",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = c(pred_dates[t],forecast_dates),
                          variable = "chlorophyll-a",
                          prediction = pred)
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
