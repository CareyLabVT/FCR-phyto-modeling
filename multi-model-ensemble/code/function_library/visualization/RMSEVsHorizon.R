#7-day prediction plot
#Author: Mary Lofton
#Date: 14MAR23

#Purpose: plot a prediction from 1-7 days into future with all models plotted

library(tidyverse)
library(lubridate)

#'Function to fit day of year model for chla
#'@param observations data frame with columns:
#'Date: yyyy-mm-dd
#'Chla_ugL: observed daily median of chlorophyll-a from EXO in ug/L
#'@param model_output data frame with columns:
#'model_id: name of model (e.g., persistence)
#'reference_datetime: date prediction was issued (yyyy-mm-dd)
#'datetime: date of prediction (yyyy-mm-dd)
#'variable: predicted variable (chlorophyll-a)
#'prediction: value of prediction (ug/L)
#'@param reference_datetime date (yyyy-mm-dd) on which prediction you want to 
#'plot starts
#'@param forecast_horizon maximum horizon that you want to plot

RMSEVsHorizon <- function(observations, 
                               model_output, 
                               forecast_horizon){
  
  #reformat observations
  pred_dates <- data.frame(Date = unique(model_output$reference_datetime)) %>%
    left_join(., observations, by = "Date") %>%
    rename(datetime = Date)
  
  
  #reformat model output
  output <- model_output %>% 
    group_by(model_id, reference_datetime) %>%
    mutate(horizon = datetime - reference_datetime) %>%
    ungroup() %>%
    separate(horizon, c("horizon"), sep = " ") %>%
    left_join(., pred_dates, by = "datetime") %>%
    group_by(model_id, horizon) %>%
    summarize(rmse = sqrt(mean((Chla_ugL - prediction)^2, na.rm = TRUE))) %>%
    filter(!horizon == 0) %>%
    mutate(horizon = as.numeric(horizon)) %>%
    arrange(model_id, horizon)
  
  p <- ggplot()+
    geom_point(data = output, aes(x = horizon, y = rmse, 
                                  group = model_id, shape = model_id,
                                  color = model_id))+
    geom_line(data = output, aes(x = horizon, y = rmse,
                                   group = model_id, color = model_id))+
    xlab("Forecast horizon (days)")+
    ylab(expression(paste("RMSE (",mu,g,~L^-1,")")))+
    scale_color_discrete(name = "Model ID")+
    scale_shape_discrete(name = "Model ID")+
    theme_classic()+
    theme(axis.text.x = element_text(size = 10))
  
  return(p)
    
}