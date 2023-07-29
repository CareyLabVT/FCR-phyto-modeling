#Format data for prophet model for chl-a
#Author: Mary Lofton
#Date: 28JUL23

#Purpose: format data for prophet model for chla from 2018-2021

#'Function to fit day of year model for chla
#'@param filepath filepath to exo data product from EDI for FCR

#load packages
library(tidyverse)
library(lubridate)

format_data_prophet <- function(filepath = "./multi-model-ensemble/data/data_raw/FCR_Catwalk_EDI_2018_2022.csv",
                            start_date = "2018-08-06",
                            end_date = "2022-12-31"){

  #get list of dates
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  daily_dates <- tibble(seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day"))
  colnames(daily_dates)[1] <- "Date"
  
  ##EXO----
  
  #message
  message("interpolating exo")
  exo <- read_csv(filepath,
                  col_types = cols(
                    .default = col_double(),
                    Reservoir = col_character(),
                    DateTime = col_datetime(format = ""),
                    EXOTemp_C_1 = col_double(),
                    Flag_EXOTemp_C_1 = col_double()
                  )) %>%
    mutate(EXOTemp_C_1 = ifelse(Flag_EXOTemp_C_1 == 0,EXOTemp_C_1,NA),
           EXOChla_ugL_1 = ifelse(Flag_EXOChla_ugL_1 == 0,EXOChla_ugL_1,NA)) %>%
    select(DateTime, EXOChla_ugL_1, EXOTemp_C_1) %>%
    mutate(Date = date(DateTime)) %>%
    group_by(Date) %>%
    summarize(WaterTemp_C = median(EXOTemp_C_1, na.rm = TRUE),
              Chla_ugL = median(EXOChla_ugL_1, na.rm = TRUE)) %>%
    filter(Date >= start_date)
  
  #interpolation
  exo2 <- interpolate(daily_dates = daily_dates,
                      data = exo,
                      variables = c("WaterTemp_C","Chla_ugL"),
                      method = "linear")
  
  #format final data frame
  df.out <- exo2 %>%
    select(Date, Chla_ugL, Flag_Chla_ugL)
    
return(df.out)
}
