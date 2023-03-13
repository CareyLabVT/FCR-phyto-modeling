#Format data for DOY model for chl-a
#Author: Mary Lofton
#Date: 13MAR23

#Purpose: format data for persistence model for chla from 2018-2021

#'Function to fit persistence model for chla
#'@param filepath filepath to exo data product from EDI for FCR

#load packages
library(tidyverse)
library(lubridate)

format_data_persistence <- function(filepath = "./multi-model-ensemble/data/data_raw/FCR_Catwalk_EDI_2018_2022.csv"){

exo <- read_csv(filepath) %>%
  filter(Flag_EXOChla_ugL_1 == 0 & year(DateTime) %in% c(2018:2021)) %>%
  select(DateTime, EXOChla_ugL_1) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date) %>%
  summarize(Chla_ugL = median(EXOChla_ugL_1, na.rm = TRUE))
    
return(exo)
}
