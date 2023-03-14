#Format data for ARIMA model for chl-a
#Author: Mary Lofton
#Date: 14MAR23

#Purpose: create daily dataframe from Aug 6, 2018 to Dec. 31, 2022 of 
#daily median values
#'Interpolation Methods
#'1. Linear interpolation for high-frequency variables
#'2. For low-frequency variables (chem, Secchi), create a DOY model (GAM from ggplot)
#'using 2013-2018 data to fill in for 2018-2022
#'3. If data are missing from beginning or end of dataset, curently just filling with 
#'first or most recent value
#'
#'#load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)

#'Function to format data for ARIMA model for chla from 2018-2022
#'@param daily_dates data frame of dates over which to interpolate (yyyy-mm-dd)
#'with column name "Date"
#'@param data data frame with the following columns:
#'Date: yyyy-mm-dd
#'variables: variables to be interpolated
#'@param variables vector of variable names (character) to be interpolated
#'@param method interpolation method;
#'choose from "linear" or "DOY"
#'@param DOY_data if interpolation method is "DOY", data frame to use for DOY model
#'calibration with same format as argument "data"

interpolate <- function(daily_dates, data, variables, method, DOY_data){
  
  #linear interpolation to fill in missing values
  df <- left_join(daily_dates, data, by = "Date")
  
  #set flag column names
  flag_names <- paste0("Flag_",variables)
  
  for(i in 1:length(variables)){
    #make flag columns
    df[,flag_names[i]] <- ifelse(is.na(df[,variables[i]]),1,0)
  }
  
  if(method == "linear"){
  
  for(i in 1:length(variables)){
  
    #replace missing values at beginning of timeseries
    df[cumall(is.na(df[,variables[i]])),variables[i]] <- as.double(subset(df, !is.na(df[,variables[i]]))[1,variables[i]])
    
    #create interpolated timeseries
    interp <- na.approx(df[,variables[i]])
    
    #fill in missing values at end of timeseries
    if(length(interp) < length(daily_dates$Date)){
      num_NA = length(daily_dates$Date) - length(interp)
      nas <- rep(NA, times = num_NA)
      interp2 = c(interp, nas)
      interp3 <- na.locf(interp2)
      df[,variables[i]] <- interp3
    } else {
      df[,variables[i]] <- interp
    }
    
  }
  } else if(method == "DOY"){
    
    #create doy column
    cal <- DOY_data %>%
      mutate(doy = yday(Date))
    df <- df %>%
      mutate(doy = yday(Date))
    
    for(i in 1:length(variables)){
    
    #fit GAM following methods in ggplot()
    temp <- data.frame(doy = cal$doy,
                       y = cal[,variables[i]])
    colnames(temp) <- c("doy","y")
    my.gam <- mgcv::gam(formula = y ~ s(doy, bs = "cs"), family = gaussian(),
                            data = temp, method = "REML")
    
    for(j in 1:length(daily_dates$Date)){
    #fill in df with GAM predictions 
      df[j,variables[i]] <- ifelse(is.na(df[j,variables[i]]),mgcv::predict.gam(my.gam, data.frame(doy=df[j,"doy"])),df[j,variables[i]])
    }
    
    df <- df %>%
      select(-doy)
    
    }
    
  } #end of DOY method
  
  return(df)
    
}
