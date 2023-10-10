#Format data for NNETAR model for chl-a
#Author: Mary Lofton
#Date: 10OCT23

#Purpose: create daily dataframe from Aug 6, 2018 to Dec. 31, 2022 of 
#daily median of the following variables:

#'1. Met: SWR, air temp, wind speed 
#'2. Inflow: discharge 
#'3. Catwalk: EXO chl-a, 1.6 water temp 
#'4. YSI_Secchi: Kd from Secchi
#'5. Chem: SRP @ 1.6, DIN @ 1.6
#'
#'Interpolation Methods
#'1. Linear interpolation for high-frequency variables
#'2. For low-frequency variables (chem, Secchi), create a DOY model (GAM from ggplot)
#'using 2013-2018 data to fill in for 2018-2022
#'3. If data are missing from beginning or end of dataset, curently just filling with 
#'first or most recent value

#load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)
library(glmtools)


#source internal functions
source("./multi-model-ensemble/code/function_library/format_data/interpolation.R")

#'Function to format data for ARIMA model for chla from 2018-2022
#'@param filepath_exo filepath to exo data product from EDI for FCR
#'@param filepath_chemistry filepath to chemistry data product from EDI
#'@param filepath_Secchi filepath to YSI/Secchi data product from EDI
#'@param filepath_met filepath to FCR met data product from EDI
#'@param filepath_inflow filepath to FCR inflow data product from EDI
#'@param start_date start date of desired formatted data file
#'@param end_date end date of desired formatted data file

format_data_NNETAR <- function(filepath_exo = "./multi-model-ensemble/data/data_raw/FCR_Catwalk_EDI_2018_2022.csv",
                                 filepath_chemistry = "./multi-model-ensemble/data/data_raw/chemistry_2013_2021.csv",
                                 filepath_Secchi = "./multi-model-ensemble/data/data_raw/Secchi_depth_2013-2022.csv",
                                 filepath_met = "./multi-model-ensemble/data/data_raw/FCR_Met_final_2015_2022.csv",
                                 filepath_inflow = "./multi-model-ensemble/data/data_raw/Inflow_2013_2022.csv",
                                 start_date = "2018-08-06",
                                 end_date = "2022-12-31"){

  #get list of dates
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  daily_dates <- tibble(seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day"))
  colnames(daily_dates)[1] <- "Date"
  
  #set GLM-AED output filepath for vars that use this method of interp
  glm_output_filepath = "./Eco-KGML-transfer-learning/data/data_raw/ModelOutputFCR/output.nc"
  

##MET----
  #message
  message("interpolating met")
  #read in data
  met_raw <- fread(filepath_met)
  #initial data wrangling
  met <- tibble(met_raw) %>%
    select(DateTime, ShortwaveRadiationDown_Average_W_m2, Flag_ShortwaveRadiationDown_Average_W_m2, AirTemp_C_Average, Flag_AirTemp_C_Average, WindSpeed_Average_m_s, Flag_WindSpeed_Average_m_s) %>%
    filter(!Flag_ShortwaveRadiationDown_Average_W_m2 == 5, !Flag_AirTemp_C_Average == 5, !Flag_WindSpeed_Average_m_s == 5) %>%
    select(-Flag_ShortwaveRadiationDown_Average_W_m2, -Flag_AirTemp_C_Average, -Flag_WindSpeed_Average_m_s) %>%
    mutate(Date = date(DateTime)) %>%
    group_by(Date) %>%
    summarize(Shortwave_Wm2 = median(ShortwaveRadiationDown_Average_W_m2, na.rm = TRUE),
              AirTemp_C = median(AirTemp_C_Average, na.rm = TRUE),
              Windspeed_ms = median(WindSpeed_Average_m_s, na.rm = TRUE)) %>%
    filter(Date >= start_date)

  #interpolation
  met2 <- interpolate(daily_dates = daily_dates,
                      data = met,
                      variables = c("AirTemp_C","Shortwave_Wm2","Windspeed_ms"),
                      method = "linear")


##INFLOW----
  
  #message
  message("interpolating inflow")
  
  inf <- read_csv(filepath_inflow) %>%
    mutate(WVWA_Flow_cms = ifelse(Flag_WVWA_Flow_cms %in% c(NA, 0, 1, 3),WVWA_Flow_cms,NA)) %>%
    select(DateTime, WVWA_Flow_cms) %>%
    mutate(Date = date(DateTime)) %>%
    group_by(Date) %>%
    summarize(Inflow_cms = median(WVWA_Flow_cms, na.rm = TRUE)) %>%
    filter(Date >= start_date)
  
  #interpolation
  inf2 <- interpolate(daily_dates = daily_dates,
                      data = inf,
                      variables = c("Inflow_cms"),
                      method = "linear")

##EXO----
  
  #message
  message("interpolating exo")
  exo <- read_csv(filepath_exo,
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


##CHEM----
#not removing flags here b/c I don't think any of these will outweigh
#the uncertainty introduced via interpolation
  
  #message
  message("interpolating chem")

##start with Site 50
  chem <- read_csv(filepath_chemistry,
                   col_types = cols(
                     .default = col_double(),
                     Reservoir = col_character(),
                     DateTime = col_datetime(format = ""),
                     DIC_mgL = col_double(),
                     DC_mgL = col_double(),
                     DN_mgL = col_double()
                   )) %>%
    filter(Reservoir == "FCR" & Site == 50 & Depth_m == 1.6) %>%
    select(DateTime, SRP_ugL, NH4_ugL, NO3NO2_ugL) %>%
    mutate(Date = date(DateTime),
           DIN_ugL = NH4_ugL + NO3NO2_ugL) %>%
    group_by(Date) %>%
    summarize(SRP_ugL = median(SRP_ugL, na.rm = TRUE),
              DIN_ugL = median(DIN_ugL, na.rm = TRUE)) 
    
  chem_cal <- chem %>%
    filter_at(vars(DIN_ugL, SRP_ugL),all_vars(!is.na(.))) %>%
    filter(Date <= start_date)
  
  chem_interp <- chem %>%
    filter(Date >= start_date)
  
  #interpolation
  chem2 <- interpolate(daily_dates = daily_dates,
                      data = chem_interp,
                      variables = c("DIN_ugL","SRP_ugL"),
                      method = "DOY",
                      DOY_data = chem_cal,
                      glm_output_filepath = glm_output_filepath)
  
##SECCHI----
  
  #message
  message("interpolating Kd")
  secchi <- read_csv(filepath_Secchi) %>%
    filter(Reservoir == "FCR" & Site == 50) %>%
    mutate(Date = date(DateTime)) %>%
    group_by(Date) %>%
    summarize(median_Secchi_m = median(Secchi_m, na.rm = TRUE)) %>%
    mutate(LightAttenuation_Kd = (1.7/median_Secchi_m)) %>%
    select(-median_Secchi_m)
  
  secchi_cal <- secchi %>%
    filter(Date <= start_date)
  
  secchi_interp <- secchi %>%
    filter(Date >= start_date)
  
  #interpolation
  secchi2 <- interpolate(daily_dates = daily_dates,
                       data = secchi_interp,
                       variables = c("LightAttenuation_Kd"),
                       method = "linear",
                       DOY_data = secchi_cal,
                       glm_output_filepath = glm_output_filepath)

  #PREPARE FINAL DF
  message("preparing final df")
  
  df.out <- left_join(met2, inf2) %>%
    left_join(., exo2) %>%
    left_join(., chem2) %>%
    left_join(., secchi2) %>%
    select(Date, AirTemp_C, Shortwave_Wm2, Windspeed_ms, Inflow_cms, WaterTemp_C,
           SRP_ugL, DIN_ugL, LightAttenuation_Kd, Chla_ugL,
           Flag_AirTemp_C, Flag_Shortwave_Wm2, Flag_Windspeed_ms, Flag_Inflow_cms, Flag_WaterTemp_C, Flag_SRP_ugL,
           Flag_DIN_ugL, Flag_LightAttenuation_Kd, Flag_Chla_ugL)
  
  return(df.out)
}
