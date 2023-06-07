#Example prediction plot
#Author: Mary Lofton
#Date: 14MAR23

#Purpose: plot a prediction into future with all models plotted

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

ExamplePrediction <- function(observations, 
                              model_output, 
                              reference_datetime, 
                              forecast_horizon,
                              model_ids){
  
  #get plotting dates
  ref_datetime <- as.Date(reference_datetime)
  plot_dates <- seq.Date(from = as.Date(ref_datetime-3), to = as.Date(ref_datetime+forecast_horizon), by = "day")  
  
  #limit to relevant observations
  plot_obs <- observations %>%
    filter(Date %in% plot_dates) %>%
    mutate(variable = ifelse(Date <= ref_datetime,"observed, seen by model","observed, not seen by model")) %>%
    mutate(variable = factor(variable, levels = c("observed, seen by model","observed, not seen by model")))
  
  #limit model output to relevant dates
  plot_mod <- model_output %>%
    filter(reference_datetime == ref_datetime & datetime %in% plot_dates & model_id %in% model_ids) %>%
    mutate(model_id = factor(model_id, levels = model_ids))
  
  p <- ggplot()+
    geom_point(data = plot_obs, aes(x = Date, y = Chla_ugL, 
                                    group = variable, fill = variable),
               shape = 21)+
    geom_line(data = plot_mod, aes(x = datetime, y = prediction,
                                   group = model_id, color = model_id))+
    geom_vline(xintercept = ref_datetime, linetype = "dashed")+
    annotate("text", x = ref_datetime + 3.5, y = max(plot_obs$Chla_ugL), 
             label = "date prediction generated", hjust = 0.25)+
    xlab("")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    scale_color_manual(name = "Model ID", values = c("#71BFB9","#B85233","#F2EC67","#56B4E9"))+
    scale_fill_manual(name = "", values = c("observed, seen by model" = "black",
                                            "observed, not seen by model" = "white"))+
    theme_classic()+
    ggtitle("2022")+
    theme(axis.text = element_text(size = 12),
          axis.title.y = element_text(size = 16),
          plot.title = element_text(size = 16, face = "bold", hjust = 1),
          legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1))+
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2))
  
  return(p)
    
}