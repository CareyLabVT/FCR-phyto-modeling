#Title: NEON lakes data munging
#Author: Mary Lofton
#Date: last updated 02MAR23

#Purpose: Wrangle observed data from NEON lakes for transfer learning project
#according to template at "transfer learning project data template"
# in Eco-KGML shared drive

#Note: much of this code is repurposed from code developed for the aquatic targets
#for the NEON forecast challenge, available at:
#https://github.com/OlssonF/neon4cast-targets/blob/main/aquatics_targets.R

# Load packages
# install.packages('pacman')
pacman::p_load(tidyverse, lubridate, data.table, zoo)

#define functions
#' retreive the model time steps based on start and stop dates and time step
#'
#' @param model_start model start date in date class
#' @param model_stop model stop date in date class
#' @param time_step model time step, defaults to daily timestep
get_model_dates = function(model_start, model_stop, time_step = 'days'){
  
  model_dates = seq.Date(from = as.Date(model_start), to = as.Date(model_stop), by = time_step)
  
  return(model_dates)
}

#read in site info
sites <- readr::read_csv("./Eco-KGML-transfer-learning/data/data_raw/DataNEONLakes/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(aquatics == 1 & field_site_subtype == "Lake")

#read in wq data

neon <- arrow::s3_bucket("neon4cast-targets/neon",
                         endpoint_override = "data.ecoforecast.org",
                         anonymous = TRUE)

wq_portal <- purrr::map_dfr(sites$field_site_id, function(site){
  message(site)
  arrow::open_dataset(neon$path("waq_instantaneous-basic-DP1.20288.001"), partitioning = "siteID") %>%   # waq_instantaneous
    #wq_portal <- neonstore::neon_table("waq_instantaneous", site = sites$field_site_id, lazy = TRUE) %>%   # waq_instantaneous
    dplyr::filter(siteID %in% site) %>%
    dplyr::select(siteID, startDateTime, sensorDepth,
                  chlorophyll, chlorophyllExpUncert,chlorophyllFinalQF) %>%
    dplyr::mutate(sensorDepth = as.numeric(sensorDepth),
                  chla = as.numeric(chlorophyll),
                  chlorophyllExpUncert = as.numeric(chlorophyllExpUncert)) %>%
    dplyr::rename(site_id = siteID) %>% 
    dplyr::filter(((sensorDepth > 0 & sensorDepth < 1)| is.na(sensorDepth))) |> 
    collect() %>% # sensor depth of NA == surface?
    dplyr::mutate(startDateTime = as_datetime(startDateTime)) %>%
    dplyr::mutate(time = as_date(startDateTime)) %>%
    # QF (quality flag) == 0, is a pass (1 == fail), 
    # make NA so these values are not used in the mean summary
    dplyr::mutate(chla = ifelse(chlorophyllFinalQF == 0, chla, NA)) %>% 
    dplyr::group_by(site_id, time) %>%
    dplyr::summarize(chla = median(chla, na.rm = TRUE),.groups = "drop") %>%
    dplyr::select(time, site_id, chla) 
}
)

#QC water quality data

wq_full <- wq_portal

# chlorophyll ranges
chla_max <- 200
chla_min <- 0

# GR flag will be true if either the DO concentration or the chlorophyll are 
# outside the ranges specified about

wq_cleaned <- wq_full %>%
  dplyr::mutate(chla = ifelse(is.na(chla),chla, ifelse(chla >= chla_min & chla <= chla_max, chla, NA)))
  # manual cleaning based on visual inspection

ggplot(data = wq_cleaned, aes(x = time, y = chla))+
  geom_point()+
  facet_wrap(vars(site_id), scales = "free", nrow = 2)+
  theme_bw()
#based on this plot I would think TOOK and PRLA are probably not super-well suited
#for the project in terms of data availability

#also best years look to be 2019-2021; lots of oddities w/ 2022 data?
#going to work up 2019-2021

#linear interpolation to fill in missing values
wq_tl <- wq_cleaned %>%
  filter(time >= "2019-01-01" & time <= "2021-12-31" & month(time) %in% c(6:10))

check <- wq_tl %>%
  filter(time == "2019-06-01" | time == "2020-06-01" | time == "2021-06-01" | time == "2019-10-31" | time == "2020-10-31" | time == "2021-10-31")

lakes <- unique(wq_cleaned$site_id)[c(1:3,5,6)]

#create daily date vector
dates <- c(get_model_dates(model_start = "2019-06-01", model_stop = "2019-10-31", time_step = 'days'),
           get_model_dates(model_start = "2020-06-01", model_stop = "2020-10-31", time_step = 'days'),
           get_model_dates(model_start = "2021-06-01", model_stop = "2021-10-31", time_step = 'days'))
daily_dates <- tibble(dates)
colnames(daily_dates)[1] <- "time"

years <- c(2019:2021)

final <- NULL

for(i in 1:length(lakes)){
  
  for(j in 1:length(years)){
  
  mylake <- wq_tl %>%
    filter(site_id == lakes[i] & year(time) == years[j]) %>%
    mutate(chla = replace(chla, cumall(is.na(chla)), chla[!is.na(chla)][1]))

  temp <- left_join(subset(daily_dates, year(daily_dates$time) == years[j]), mylake, by = "time") %>%
    mutate(site_id = ifelse(is.na(site_id),lakes[i],site_id))

  med_chla <- na.approx(temp$chla)
  
  num_NA = length(temp$chla) - length(med_chla)
  
  if(num_NA > 0){
    nas <- rep(NA, times = num_NA)
    med_chla = c(med_chla, nas)
  }
  
  med_chla_final <- na.locf(med_chla)
  
  temp$chla_interp <- med_chla_final

  temp <- temp %>%
  mutate(Interp_Flag_chla = ifelse(is.na(chla),TRUE,FALSE))
  
  if(i == 1 & j == 1){
    final <- temp
  } else {
    final <- bind_rows(final, temp)
  }

}
}

ggplot(data = final, aes(x = time, y = chla_interp, group = Interp_Flag_chla, color = Interp_Flag_chla))+
  geom_point()+
  facet_wrap(vars(site_id), scales = "free", nrow = 2)+
  theme_bw()

chla_final <- final %>%
  rename(Lake = site_id,
         DateTime = time,
         Chla_ugL = chla_interp) %>%
  mutate(Flag_Chla_ugL = ifelse(Interp_Flag_chla == TRUE,1,0)) %>%
  add_column(Depth_m = 0.5,
             Site = "buoy",
             DataType = "observed",
             ModelRunType = NA,
             AirTemp_C = NA,
             Shortwave_Wm2 = NA,
             Inflow_cms = 0,
             WaterTemp_C = NA,
             SRP_ugL = NA,
             DIN_ugL = NA,
             LightAttenuation_Kd = NA,
             Flag_AirTemp_C = NA,
             Flag_Shortwave_Wm2 = NA,
             Flag_Inflow_cms = 2,
             Flag_WaterTemp_C = NA,
             Flag_SRP_ugL = NA,
             Flag_DIN_ugL = NA,
             Flag_LightAttenuation_Kd = NA) %>%
  select(Lake, DateTime, Site, Depth_m, DataType, ModelRunType, AirTemp_C, Shortwave_Wm2,
         Inflow_cms, WaterTemp_C, SRP_ugL, DIN_ugL, LightAttenuation_Kd, Chla_ugL,
         Flag_AirTemp_C, Flag_Shortwave_Wm2, Flag_Inflow_cms, Flag_WaterTemp_C, Flag_SRP_ugL,
         Flag_DIN_ugL, Flag_LightAttenuation_Kd, Flag_Chla_ugL)

write.csv(chla_final, "./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv",row.names = FALSE)

#Next steps:
#1. Use Freya's code to pull in water temp data and wrangle that
#2. Use more traditional API method to pull in: Secchi, DIN, SRP
#3. Figure out what data product we want to use for NEON lakes for weather (NLDAS?)
  