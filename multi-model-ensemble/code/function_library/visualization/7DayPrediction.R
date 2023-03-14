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

SevenDayPrediction <- function(observations, 
                               model_output, 
                               reference_datetime, 
                               forecast_horizon){
  
  #get plotting dates
  ref_datetime <- as.Date(reference_datetime)
  plot_dates <- seq.Date(from = as.Date(ref_datetime-3), to = as.Date(ref_datetime+forecast_horizon), by = "day")  
  
  #limit to relevant observations
  plot_obs <- observations %>%
    filter(Date %in% plot_dates) %>%
    mutate(variable = "observed")
  
  #limit model output to relevant dates
  plot_mod <- model_output %>%
    filter(reference_datetime == ref_datetime & datetime %in% plot_dates)
  
  p <- ggplot()+
    geom_point(data = plot_obs, aes(x = Date, y = Chla_ugL, 
                                    group = variable, fill = variable))+
    geom_line(data = plot_mod, aes(x = datetime, y = prediction,
                                   group = model_id, color = model_id))+
    geom_vline(xintercept = ref_datetime, linetype = "dashed")+
    xlab("")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    scale_color_discrete(name = "Model ID")+
    scale_fill_discrete(name = "")+
    theme_classic()+
    theme(axis.text.x = element_text(size = 10))
  
  return(p)
    
}