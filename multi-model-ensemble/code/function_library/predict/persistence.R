#Persistence model
#Author: Mary Lofton
#Date: 28FEB23

#'Function to predict chl-a using persistence model
#'@param data data frame with columns DateTime (yyyy-mm-dd hh:mm:ss) and
#'EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param pred_dates list of dates on which you are making predictions
#'@param forecast_horizon maximum forecast horizon of predictions

persistence <- function(data, pred_dates, forecast_horizon){
  
  #set up empty dataframe
  df.cols = c("model_id","reference_datetime","datetime","variable","prediction") 
  pred.df <- data.frame(matrix(nrow = 0, ncol = length(df.cols))) 
  colnames(pred.df) = df.cols
  
  for(t in 1:length(pred_dates)){
  
    #subset to reference_datetime and identify observation
    forecast_dates <- seq.Date(from = as.Date(pred_dates[t]), to = as.Date(pred_dates[t]+forecast_horizon), by = "day")
    chla <- data[which(data$Date == pred_dates[t]),"Chla_ugL"]
    
    #set up dataframe for today's prediction
    temp.df <- data.frame(model_id = "persistence",
                     reference_datetime = rep(pred_dates[t],forecast_horizon+1),
                     datetime = forecast_dates,
                     variable = "chlorophyll-a",
                     prediction = rep(NA,forecast_horizon+1))
  
    for(h in 1:(forecast_horizon+1)){
      
      #make prediction
      temp.df$prediction[h] = chla
      
    } #end of today's prediction loop
  
    #bind today's prediction to larger dataframe
    pred.df <- rbind(pred.df, temp.df)
    
  } #end of all prediction loop
  
  pred.df$prediction <- as.double(pred.df$prediction)
  return(pred.df)
}