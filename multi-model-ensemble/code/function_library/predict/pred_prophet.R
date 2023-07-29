#Fit prophet model for chl-a
#Author: Mary Lofton
#Date: 28JUL23

#Purpose: make predictions using prophet model for chla

library(prophet)

#'Function to predict chl-a using prophet model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

pred_prophet <- function(data, pred_dates, forecast_horizon){
  
  #Fit model
  #assign target and predictors
  df <- data %>%
    filter(Date < pred_dates[1]) %>%
    select(-Flag_Chla_ugL) %>%
    rename(ds = Date,
           y = Chla_ugL)
  
  #fit prophet model 
  my.prophet <- prophet(df) 
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #subset to reference_datetime 
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]+1), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    #build refit dataset
    refit <- data %>%
      filter(Date <= pred_dates[t]) %>%
      select(-Flag_Chla_ugL) %>%
      rename(ds = Date,
             y = Chla_ugL)
    
    #refit model
    ref <- prophet(refit)
    
    #build future dataframe
    future <- make_future_dataframe(ref, periods = forecast_horizon)
    
    #generate predictions
    pred <- predict(ref, future) %>%
      mutate(ds = as.Date(ds)) %>%
      filter(ds %in% forecast_dates)

    #set up dataframe for today's prediction
    curr_chla_df <- data %>%
      filter(Date == pred_dates[t]) %>%
      select(Chla_ugL)
    
    curr_chla <- curr_chla_df$Chla_ugL[1]
    
    temp.df <- data.frame(model_id = "prophet",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = c(pred_dates[t],forecast_dates),
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,pred$yhat))
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
    #send update to user
    message(pred_dates[t])
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
