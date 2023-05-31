#Predict chl-a with Optimum Monod model
#Author: Mary Lofton
#Date: 31MAY23

#Purpose: make predictions using OptimumMonod model for chla

#'Function to predict chl-a using DOY model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

OptimumMonod <- function(data, pred_dates, forecast_horizon){
  
  #load model output
  load("./multi-model-ensemble/model_output/OptimumMonod_output.rds")
  
  #assign target and predictors
  df <- as_tsibble(data) %>%
    filter(Date < pred_dates[1]) 
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
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
    
    #name parameters
    muopt <- trim_OptimumMonod$summary$statistics[2,1]
    Topt <- trim_OptimumMonod$summary$statistics[3,1]
    I_K <- trim_OptimumMonod$summary$statistics[4,1]
    R_growth <- trim_OptimumMonod$summary$statistics[5,1]
    R_resp <- trim_OptimumMonod$summary$statistics[6,1]
    
    #generate predictions
    pred <- c(curr_chla)
    for(i in 2:(length(forecast_dates)+1)){
    pred[i] = pred[i-1] + (pred[i-1] * R_growth * (((wtemp[i-1] - 1) / (Topt - 1)) *((40 - wtemp[i-1]) / (40 - Topt)) ^((40 - Topt) / (Topt - 1))) * ((swr[i-1]/I_K) / (1 + (swr[i-1]/I_K)))) - (pred[i-1] * R_resp * (1.08^(wtemp[i-1] - 20))) 
    }
    
    #set up dataframe for today's prediction
    temp.df <- data.frame(model_id = "OptimumMonod",
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
