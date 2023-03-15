#Plot input data
#Author: Mary Lofton
#Date: 14MAR23

#Purpose: plot model input data, showing interpolated and non-interpolated values

#load packages
library(tidyverse)
library(lubridate)

#'Function to fit day of year model for chla
#'@param interp_methods character vector of names for different interpolation methods
#'@param data_list list of data frames where each data frame corresponds to data
#'interpolated using each method; make sure names of methods are in same order as data
#'frames in list!
#'@param interp_vars list of environmental variables you have interpolated and want to plot


PlotInterpMethods <- function(interp_methods, data_lst, interp_vars){
  
  dates <- data_lst[[1]][,"Date"]
  flag_cols <- paste0("Flag_",interp_vars)
  
  #reformat data for plotting
  for(i in 1:length(interp_methods)){
    
    dat <- data_lst[[i]][,c(interp_vars, flag_cols)]
    dat2 <- bind_cols(dates, dat)
    dat2$Interp_method <- interp_methods[i]
    
    if(i == 1){
      final <- dat2
    } else {
      final <- bind_rows(final, dat2)
    }
  }
  
  plot_vars <- final[,c("Date","Interp_method",interp_vars)] %>%
    pivot_longer(-c(Date, Interp_method), names_to = "variable", values_to = "value")
  
  plot_flags <- final[,c("Date","Interp_method",flag_cols)] 
  colnames(plot_flags) <- c("Date","Interp_method",interp_vars)
  plot_flags <- plot_flags %>%
    pivot_longer(-c(Date, Interp_method), names_to = "variable", values_to = "flag")
  
  plot_data <- left_join(plot_flags, plot_vars, by = c("Date","Interp_method","variable")) %>%
    pivot_wider(id_cols = c("Date","Interp_method", "variable"), names_from = flag) %>%
    rename(Interpolated = `1`,
           Observed = `0`)
  
  p <- ggplot(data = plot_data)+
    facet_grid(rows = vars(variable), cols = vars(Interp_method), scales = "free_y")+
    geom_point(aes(x = Date, y = Interpolated, color = "Interpolated"))+
    geom_point(aes(x = Date, y = Observed, color = "Observed"))+
    xlab("")+
    ylab("")+
    labs(color = NULL)+
    theme_bw()
  
  return(p)
}