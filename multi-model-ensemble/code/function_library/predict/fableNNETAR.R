#Fit NNETAR model for chl-a
#Author: Mary Lofton
#Date: 10OCT23

#Purpose: make predictions using DOY model for chla

library(fable)

#'Function to predict chl-a using DOY model
#'@param data formatted data including both chl-a and predictors; output of
#'format_data_ARIMA() function in 02_format_data.R workflow script
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

fableNNETAR <- function(data, pred_dates, forecast_horizon){
  
  #Fit model
  
  # #define scaling function
  # scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  # 
  # #define vars
  # vars <- c("AirTemp_C","Shortwave_Wm2","Windspeed_ms","Inflow_cms", "WaterTemp_C" ,"LightAttenuation_Kd", "DIN_ugL", "SRP_ugL")
  # 
  #assign target and predictors
  df <- as_tsibble(data) %>%
    filter(Date < pred_dates[1]) #%>%
    #mutate_at(vars, scale2)
  
  #fit NNETAR from fable package
  my.nnar <- df %>%
    model(nnar = fable::NNETAR(formula = Chla_ugL ~ AirTemp_C + Shortwave_Wm2 + Windspeed_ms + Inflow_cms + WaterTemp_C + LightAttenuation_Kd + DIN_ugL + SRP_ugL)) 
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
    
    #subset to reference_datetime 
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]+1), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    
    #build driver dataset
    drivers = as_tsibble(data) %>%
      filter(Date %in% forecast_dates) #%>%
    #mutate_at(vars, scale2)
    drivers[,"Chla_ugL"] <- NA
    
    #refit model
    new.data <- as_tsibble(data) %>%
      filter(Date <= pred_dates[t]) #%>%
      #mutate_at(vars, scale2)
    ref <- refit(object = my.nnar, new_data = new.data)
    
    #generate predictions
    #notes times = 0 specification is critical to avoid super slow runtimes
    #we are ok with times = 0 b/c we aren't using a probabilistic forecast
    #for this project
    pred <- forecast(ref, new_data = drivers, times = 0)
    
    #set up dataframe for today's prediction
    curr_chla_df <- data %>%
      filter(Date == pred_dates[t]) %>%
      select(Chla_ugL)
    curr_chla <- curr_chla_df$Chla_ugL[1]
    temp.df <- data.frame(model_id = "NNETAR",
                          reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                          datetime = c(pred_dates[t],forecast_dates),
                          variable = "chlorophyll-a",
                          prediction = c(curr_chla,pred$.mean))
    
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  #return predictions
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}
