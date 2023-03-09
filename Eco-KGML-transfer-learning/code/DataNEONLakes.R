#Title: NEON lakes data munging
#Author: Mary Lofton
#Date: last updated 02MAR23

#Purpose: Wrangle observed data from NEON lakes for transfer learning project
#according to template at "transfer learning project data template"
# in Eco-KGML shared drive

#Note: much of this code is repurposed from code developed for the aquatic targets
#for the NEON forecast challenge, available at:
#https://github.com/OlssonF/neon4cast-targets/blob/main/aquatics_targets.R

#Next steps:
#1. Use Freya's code to pull in water temp data and wrangle that - check 8MAR23
#2. Use more traditional API method to pull in: Secchi, DIN, SRP
#3. Figure out what data product we want to use for NEON lakes for weather (NLDAS?)

# Load packages
# install.packages('pacman')
pacman::p_load(tidyverse, lubridate, data.table, zoo, sparklyr, neonUtilities)

#define NEON token
source("./Eco-KGML-transfer-learning/code/neon_token_source.R")

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
lake_sites <- sites$field_site_id[(which(sites$field_site_subtype == "Lake"))]

#create directory to download data
EDI_file_directory <- "./Eco-KGML-transfer-learning/data/data_raw/DataNEONLakes"


#read in wq data

neon <- arrow::s3_bucket("neon4cast-targets/neon",
                         endpoint_override = "data.ecoforecast.org",
                         anonymous = TRUE)

####Chl-a ===============================================#######

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


##########WATER TEMP===============================================#####

#define function for temperature QC
#### Temperature data QC ####
# Function to QC temperature profiles 
# option to group by site and depth or just by site
# need to specify the 1) absolute range (vector - length 2) and the 2) the acceptable rate of change (absolute value)
QC.temp <- function(df, range, spike, by.depth = T) {
  if (by.depth == F) {
    df_QC <- df %>%
      arrange(site_id, time) %>%
      group_by(site_id) %>%
      mutate(temp_change = observation - dplyr::lag(observation), 
             
             flagged_abs_val = ifelse(between(observation, min(range), max(range)), F, T),
             flagged_spike = ifelse(abs(temp_change) > spike & time != first(time),
                                    T, F),
             flagged_flat = ifelse(temp_change == 0  & time != first(time),
                                   T, F),
             final_flag = ifelse(flagged_spike != F | 
                                   flagged_abs_val != F |
                                   flagged_flat != F,
                                 T, F),
             observation = ifelse(final_flag == T, 
                                  NA, observation)) %>%
      select(-c(contains('flag'), temp_change))
  } else {
    df_QC <- df %>%
      arrange(site_id, time) %>%
      group_by(site_id, depth) %>%
      mutate(temp_change = observation - dplyr::lag(observation), 
             
             flagged_abs_val = ifelse(between(observation, min(range), max(range)), F, T),
             flagged_spike = ifelse(abs(temp_change) > spike & time != first(time),
                                    T, F),
             flagged_flat = ifelse(temp_change == 0 & time != first(time),
                                   T, F),
             final_flag = ifelse((flagged_spike == T | 
                                    flagged_abs_val == T |
                                    flagged_flat == T),
                                 T, F),
             observation = ifelse((final_flag == F | is.na(final_flag)), 
                                  observation, NA)) %>%
      select(-c(contains('flag'), temp_change))
  }
  return(df_QC)
}

message("#### Generate hourly temperature profiles for lake #############")
message("##### NEON portal data #####")
hourly_temp_profile_portal <- arrow::open_dataset(neon$path("TSD_30_min-basic-DP1.20264.001"), partitioning = "siteID") %>%
  #hourly_temp_profile_portal <- neonstore::neon_table("TSD_30_min", site = sites$field_site_id) %>%
  dplyr::filter(siteID %in% lake_sites) %>%
  rename(site_id = siteID,
         depth = thermistorDepth) %>%
  dplyr::select(startDateTime, site_id, tsdWaterTempMean, depth, tsdWaterTempExpUncert, tsdWaterTempFinalQF, verticalPosition) %>%
  dplyr::collect() %>%
  # errors in the sensor depths reported - see "https://www.neonscience.org/impact/observatory-blog/incorrect-depths-associated-lake-and-river-temperature-profiles"
  # sensor depths are manually assigned based on "vertical position" variable as per table on webpage
  dplyr::mutate(depth = ifelse(site_id == "CRAM" & 
                                 lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                 verticalPosition == 502, 1.75, depth),
                depth = ifelse(site_id == "CRAM" & 
                                 lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                 verticalPosition == 503, 3.45, depth),
                depth = ifelse(site_id == "CRAM" & 
                                 lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                 verticalPosition == 504, 5.15, depth),
                depth = ifelse(site_id == "CRAM" & 
                                 lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                 verticalPosition == 505, 6.85, depth),
                depth = ifelse(site_id == "CRAM" & 
                                 lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                 verticalPosition == 506, 8.55, depth),
                depth = ifelse(site_id == "CRAM" & 
                                 lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                 verticalPosition == 505, 10.25, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                    lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 502, 0.55, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                    lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 503, 1.05, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                    lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 504, 1.55, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                    lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 505, 2.05, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                    lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 506, 2.55, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                    lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 507, 3.05, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                    lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 502, 0.3, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                    lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 503, 0.55, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                    lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 504, 0.8, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                    lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 505, 1.05, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                    lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 506, 1.3, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                    lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 507, 1.55, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                    lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 508, 2.05, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                    lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 509, 2.55, depth),
                depth = ifelse(site_id == "BARC" & 
                                 (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") | 
                                    lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                 verticalPosition == 510, 3.05, depth)) %>%
  dplyr::filter((tsdWaterTempFinalQF == 0 | (tsdWaterTempFinalQF == 1 & as_date(startDateTime) > as_date("2020-07-01")))) %>% 
  dplyr::mutate(time = ymd_h(format(startDateTime, "%y-%m-%d %H")),
                depth = round(depth, 1)) %>% # round to the nearest 0.1 m
  group_by(site_id, depth, time) %>%
  dplyr::summarize(temperature = median(tsdWaterTempMean, na.rm = TRUE),.groups = "drop") %>%
  dplyr::select(time, site_id, temperature, depth) |> 
  rename(observation = temperature) |> 
  mutate(variable = "temperature") |> 
  QC.temp(range = c(-5, 40), spike = 5, by.depth = T) %>%
  mutate(data_source = 'NEON_portal')

message("##### Sonde EDI data #####")
# Only 6 lake sites available on EDI
edi_url_lake <- c("https://pasta.lternet.edu/package/data/eml/edi/1071/1/7f8aef451231d5388c98eef889332a4b",
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/2c8893684d94b9a52394060a76cab798", 
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/770e2ab9d957991a787a2f990d5a2fad",
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/2e52d63ba4dc2040d1e5e2d11114aa93",
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/60df35a34bb948c0ca5e5556d129aa98", 
                  "https://pasta.lternet.edu/package/data/eml/edi/1071/1/004857d60d6fe7587b112d714e0380d0")
lake_edi_profile <- c("NEON.D03.BARC.DP0.20005.001.01378.csv",
                      "NEON.D05.CRAM.DP0.20005.001.01378.csv",
                      "NEON.D05.LIRO.DP0.20005.001.01378.csv",
                      "NEON.D09.PRLA.DP0.20005.001.01378.csv",
                      "NEON.D09.PRPO.DP0.20005.001.01378.csv",
                      "NEON.D03.SUGG.DP0.20005.001.01378.csv")

fs::dir_create(EDI_file_directory) # ignores existing directories unlike dir.create()
# Download the data

for(i in 1:length(edi_url_lake)){
  if (!file.exists(file.path(EDI_file_directory,  lake_edi_profile[i]))) {
    if (!dir.exists(dirname(file.path(EDI_file_directory, 
                                      lake_edi_profile[i])))) {
      dir.create(dirname(file.path(EDI_file_directory, 
                                   lake_edi_profile[i])))
    }
    download.file(edi_url_lake[i], destfile = file.path(EDI_file_directory, lake_edi_profile[i]))
  }
}


# List all the files in the EDI directory 
edi_data <- list.files(file.path(EDI_file_directory), full.names = T)
# Get the lake sites subset
edi_lake_files <- c(edi_data[grepl(x = edi_data, pattern= lake_sites[1])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[2])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[3])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[4])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[5])],
                    edi_data[grepl(x = edi_data, pattern= lake_sites[6])])

# Calculate the hourly average profile 
hourly_temp_profile_EDI <- purrr::map_dfr(.x = edi_lake_files, ~ read.csv(file = .x)) %>%
  rename('site_id' = siteID,
         'depth' = sensorDepth,
         'observation' = waterTemp) %>%
  mutate(startDate  = lubridate::ymd_hm(startDate),
         time = lubridate::ymd_h(format(startDate, '%Y-%m-%d %H')),
         depth = round(depth, digits = 1)) %>%
  group_by(site_id, time, depth) %>%
  summarise(observation = mean(observation),.groups = "drop") %>%
  mutate(variable = "temperature") %>%
  # include first QC of data
  QC.temp(range = c(-5, 40), spike = 5, by.depth = T) %>%
  mutate(data_source = 'MS_raw')

# Combine the three data sources
hourly_temp_profile_lakes <- bind_rows(hourly_temp_profile_portal, hourly_temp_profile_EDI) %>%
  arrange(time, site_id, depth) %>%
  group_by(time, site_id, depth) %>%
  summarise(observation = median(observation, na.rm = T), .groups = "drop") |> 
  mutate(variable = "temperature") |> 
  select(time, site_id, depth, variable, observation)

#======================================================#

message("#### Generate surface (< 1 m) temperature #############")
message("###### Lake temperatures #####")
# Daily surface lake temperatures generated from the hourly profiles created above
daily_temp_surface_lakes <- hourly_temp_profile_lakes %>%
  dplyr::filter(depth <= 1) %>%
  mutate(time = lubridate::as_date(time)) %>%
  group_by(site_id, time) %>%
  summarise(observation = mean(observation, na.rm = T),.groups = "drop") %>%
  mutate(variable = 'temperature') 

#### Temp QC protocol=================
temp_full <- daily_temp_surface_lakes
# additional QC steps implemented (FO, 2022-07-13)
##### check 1 Gross range tests on temperature
# temperature ranges 
T_max <- 32 # gross max
T_min <- -2 # gross min

# GR flag will be true if the temperature is outside the range specified 
temp_cleaned <-  temp_full %>%
  dplyr::mutate(observation =ifelse(observation >= T_min & observation <= T_max , 
                                    observation, NA))  %>%
  # manual cleaning based on observation
  dplyr:: mutate(observation = ifelse(site_id == "PRLA" & time <ymd("2019-01-01"),
                                      NA, observation)) %>%
  rename(wtemp = observation) %>%
  select(-variable)

#plot cleaned data
ggplot(data = temp_cleaned, aes(x = time, y = wtemp))+
  geom_point()+
  facet_wrap(vars(site_id), scales = "free", nrow = 2)+
  theme_bw()

#based on this plot I would think TOOK and PRLA are probably not super-well suited
#for the project in terms of data availability; and YIKES CRAM is missing an entire
#year of surface temp data??

#also best years look to be 2019-2021; lots of oddities w/ 2022 data?
#going to work up 2019-2021 and eliminate CRAM

#linear interpolation to fill in missing values
temp_tl <- temp_cleaned %>%
  filter(time >= "2019-01-01" & time <= "2021-12-31" & month(time) %in% c(6:10))

check <- temp_tl %>%
  filter(time == "2019-06-01" | time == "2020-06-01" | time == "2021-06-01" | time == "2019-10-31" | time == "2020-10-31" | time == "2021-10-31")

lakes <- unique(temp_cleaned$site_id)[c(1,3,5,6)]

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
    
    mylake <- temp_tl %>%
      filter(site_id == lakes[i] & year(time) == years[j]) %>%
      mutate(wtemp = replace(wtemp, cumall(is.na(wtemp)), wtemp[!is.na(wtemp)][1]))
    
    temp <- left_join(subset(daily_dates, year(daily_dates$time) == years[j]), mylake, by = "time") %>%
      mutate(site_id = ifelse(is.na(site_id),lakes[i],site_id))
    
    med_wtemp <- na.approx(temp$wtemp)
    
    num_NA = length(temp$wtemp) - length(med_wtemp)
    
    if(num_NA > 0){
      nas <- rep(NA, times = num_NA)
      med_wtemp = c(med_wtemp, nas)
    }
    
    med_wtemp_final <- na.locf(med_wtemp)
    
    temp$wtemp_interp <- med_wtemp_final
    
    temp <- temp %>%
      mutate(Interp_Flag_wtemp = ifelse(is.na(wtemp),TRUE,FALSE))
    
    if(i == 1 & j == 1){
      final <- temp
    } else {
      final <- bind_rows(final, temp)
    }
    
  }
}

ggplot(data = final, aes(x = time, y = wtemp_interp, group = Interp_Flag_wtemp, color = Interp_Flag_wtemp))+
  geom_point()+
  facet_wrap(vars(site_id), scales = "free", nrow = 2)+
  theme_bw()

DataNEONLakes <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv") %>%
  filter(Lake %in% lakes) %>%
  mutate(WaterTemp_C = final$wtemp_interp,
         Flag_WaterTemp_C = ifelse(final$Interp_Flag_wtemp == TRUE, 1, 0))

write.csv(DataNEONLakes, "./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv",row.names = FALSE)


########### Light attenuation

#Using this data product for lake Secchi: https://data.neonscience.org/data-products/DP1.20252.001

sec <- loadByProduct(dpID="DP1.20252.001", site=c("BARC","SUGG","CRAM","LIRO","PRLA","PRPO","TOOK"),
                     startdate="2019-01", enddate="2021-12", 
                     package="expanded", 
                     token = Sys.getenv("NEON_TOKEN"),
                     check.size = F)

# unlist the variables and add to the global environment
list2env(sec, .GlobalEnv)

#format data
sec2 <- dep_secchi %>%
  select(siteID, date, secchiMeanDepth) %>%
  mutate(DateTime = date(date)) %>%
  rename(Lake = siteID) %>%
  select(Lake, DateTime, secchiMeanDepth)

#plot formatted data
ggplot(data = sec2, aes(x = DateTime, y = secchiMeanDepth))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

#linear interpolation to fill in missing values
sec_tl <- sec2 %>%
  filter(DateTime >= "2019-01-01" & DateTime <= "2021-12-31" & month(DateTime) %in% c(6:10))

lakes <- c("BARC","SUGG","LIRO","PRPO")

#create daily date vector
dates <- c(get_model_dates(model_start = "2019-06-01", model_stop = "2019-10-31", time_step = 'days'),
           get_model_dates(model_start = "2020-06-01", model_stop = "2020-10-31", time_step = 'days'),
           get_model_dates(model_start = "2021-06-01", model_stop = "2021-10-31", time_step = 'days'))
daily_dates <- tibble(dates)
colnames(daily_dates)[1] <- "DateTime"

years <- c(2019:2021)

final <- NULL

for(i in 1:length(lakes)){
  
  for(j in 1:length(years)){
    
    mylake <- sec_tl %>%
      filter(Lake == lakes[i] & year(DateTime) == years[j]) %>%
      mutate(secchiMeanDepth = replace(secchiMeanDepth, cumall(is.na(secchiMeanDepth)), secchiMeanDepth[!is.na(secchiMeanDepth)][1]))
    
    temp <- left_join(subset(daily_dates, year(daily_dates$DateTime) == years[j]), mylake, by = "DateTime") %>%
      mutate(Lake = ifelse(is.na(Lake),lakes[i],Lake))
    
    med_secchiMeanDepth <- na.approx(temp$secchiMeanDepth)
    
    num_NA = length(temp$secchiMeanDepth) - length(med_secchiMeanDepth)
    
    if(num_NA > 0){
      nas <- rep(NA, times = num_NA)
      med_secchiMeanDepth = c(med_secchiMeanDepth, nas)
    }
    
    med_secchiMeanDepth_final <- na.locf(med_secchiMeanDepth)
    
    temp$secchiMeanDepth_interp <- med_secchiMeanDepth_final
    
    temp <- temp %>%
      mutate(Interp_Flag_secchiMeanDepth = ifelse(is.na(secchiMeanDepth),TRUE,FALSE))
    
    if(i == 1 & j == 1){
      final <- temp
    } else {
      final <- bind_rows(final, temp)
    }
    
  }
}

ggplot(data = final, aes(x = DateTime, y = secchiMeanDepth_interp, group = Interp_Flag_secchiMeanDepth, color = Interp_Flag_secchiMeanDepth))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

sec3 <- final %>%
  mutate(LightAttenuation_Kd = (1.7/secchiMeanDepth_interp)) %>%
  select(Lake, DateTime, LightAttenuation_Kd, Interp_Flag_secchiMeanDepth)

DataNEONLakes <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv") %>%
  select(-LightAttenuation_Kd) %>%
  left_join(., sec3, by = c("Lake","DateTime")) %>%
  mutate(Flag_LightAttenuation_Kd = ifelse(Interp_Flag_secchiMeanDepth == TRUE, 1, 0)) %>%
  select(-Interp_Flag_secchiMeanDepth) %>%
  select(Lake, DateTime, Site, Depth_m, DataType, ModelRunType, AirTemp_C, Shortwave_Wm2,
         Inflow_cms, WaterTemp_C, SRP_ugL, DIN_ugL, LightAttenuation_Kd, Chla_ugL,
         Flag_AirTemp_C, Flag_Shortwave_Wm2, Flag_Inflow_cms, Flag_WaterTemp_C, Flag_SRP_ugL,
         Flag_DIN_ugL, Flag_LightAttenuation_Kd, Flag_Chla_ugL)

write.csv(DataNEONLakes, "./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv",row.names = FALSE)


########### DIN and SRP

#Using this data product for lake nutrients: https://data.neonscience.org/data-products/DP1.20093.001

nuts <- loadByProduct(dpID="DP1.20093.001", site=c("BARC","SUGG","CRAM","LIRO","PRLA","PRPO","TOOK"),
                     startdate="2019-01", enddate="2021-12", 
                     package="expanded", 
                     token = Sys.getenv("NEON_TOKEN"),
                     check.size = F)

# unlist the variables and add to the global environment
list2env(nuts, .GlobalEnv)

#format data
#NOTE: c0 means sample collected at 0.5 m and lake not stratified; c1 means
#sample collected at 0.5 m and lake stratified
nuts2 <- swc_externalLabDataByAnalyte %>%
  select(siteID, namedLocation, collectDate, analyte, analyteConcentration, analyteUnits) %>%
  mutate(DateTime = date(collectDate)) %>%
  rename(Lake = siteID,
         Site = namedLocation) %>%
  select(Lake, DateTime, Site, analyte, analyteConcentration, analyteUnits) %>%
  filter(analyte == "TDN" | analyte == "TDP") %>%
  filter(!is.na(analyteConcentration)) %>%
  filter(grepl("buoy.c0",Site) | grepl("buoy.c1",Site)) %>%
  group_by(Lake, Site, DateTime, analyte) %>%
  summarize(analyteConcentration = mean(analyteConcentration, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = analyte, values_from = analyteConcentration) %>%
  mutate(DIN_ugL = 1000*TDN,
         SRP_ugL = 1000*TDP) %>%
  select(-Site, -TDN, -TDP)

#plot formatted data
ggplot(data = nuts2, aes(x = DateTime, y = DIN_ugL))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

ggplot(data = nuts2, aes(x = DateTime, y = SRP_ugL))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

#linear interpolation to fill in missing values
nuts_tl <- nuts2 %>%
  filter(DateTime >= "2019-01-01" & DateTime <= "2021-12-31" & month(DateTime) %in% c(6:10))

lakes <- c("BARC","SUGG","LIRO","PRPO")

#create daily date vector
dates <- c(get_model_dates(model_start = "2019-06-01", model_stop = "2019-10-31", time_step = 'days'),
           get_model_dates(model_start = "2020-06-01", model_stop = "2020-10-31", time_step = 'days'),
           get_model_dates(model_start = "2021-06-01", model_stop = "2021-10-31", time_step = 'days'))
daily_dates <- tibble(dates)
colnames(daily_dates)[1] <- "DateTime"

years <- c(2019:2021)

final <- NULL

for(i in 1:length(lakes)){
  
  for(j in 1:length(years)){
    
    mylake <- nuts_tl %>%
      filter(Lake == lakes[i] & year(DateTime) == years[j]) %>%
      mutate(DIN_ugL = replace(DIN_ugL, cumall(is.na(DIN_ugL)), DIN_ugL[!is.na(DIN_ugL)][1]),
             SRP_ugL = replace(SRP_ugL, cumall(is.na(SRP_ugL)), SRP_ugL[!is.na(SRP_ugL)][1]))
    
    temp <- left_join(subset(daily_dates, year(daily_dates$DateTime) == years[j]), mylake, by = "DateTime") %>%
      mutate(Lake = ifelse(is.na(Lake),lakes[i],Lake))
    
    med_DIN_ugL <- na.approx(temp$DIN_ugL)
    med_SRP_ugL <- na.approx(temp$SRP_ugL)
    
    num_NA_DIN = length(temp$DIN_ugL) - length(med_DIN_ugL)
    num_NA_SRP = length(temp$SRP_ugL) - length(med_SRP_ugL)
    
    
    if(num_NA_DIN > 0){
      nas <- rep(NA, times = num_NA_DIN)
      med_DIN_ugL = c(med_DIN_ugL, nas)
    }
    if(num_NA_SRP > 0){
      nas <- rep(NA, times = num_NA_SRP)
      med_SRP_ugL = c(med_SRP_ugL, nas)
    }
    
    med_DIN_ugL_final <- na.locf(med_DIN_ugL)
    med_SRP_ugL_final <- na.locf(med_SRP_ugL)
    
    
    temp$DIN_ugL_interp <- med_DIN_ugL_final
    temp$SRP_ugL_interp <- med_SRP_ugL_final
    
    temp <- temp %>%
      mutate(Interp_Flag_DIN_ugL = ifelse(is.na(DIN_ugL),TRUE,FALSE),
             Interp_Flag_SRP_ugL = ifelse(is.na(SRP_ugL),TRUE,FALSE))
    
    if(i == 1 & j == 1){
      final <- temp
    } else {
      final <- bind_rows(final, temp)
    }
    
  }
}

ggplot(data = final, aes(x = DateTime, y = DIN_ugL_interp, group = Interp_Flag_DIN_ugL, color = Interp_Flag_DIN_ugL))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

ggplot(data = final, aes(x = DateTime, y = SRP_ugL_interp, group = Interp_Flag_SRP_ugL, color = Interp_Flag_SRP_ugL))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

nuts3 <- final %>%
  select(-DIN_ugL, -SRP_ugL) %>%
  rename(DIN_ugL = DIN_ugL_interp,
         SRP_ugL = SRP_ugL_interp)

DataNEONLakes <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv") %>%
  select(-DIN_ugL, -SRP_ugL) %>%
  left_join(., nuts3, by = c("Lake","DateTime")) %>%
  mutate(Flag_DIN_ugL = ifelse(Interp_Flag_DIN_ugL == TRUE, 1, 0),
         Flag_SRP_ugL = ifelse(Interp_Flag_SRP_ugL == TRUE, 1, 0)) %>%
  select(-Interp_Flag_DIN_ugL, -Interp_Flag_SRP_ugL) %>%
  select(Lake, DateTime, Site, Depth_m, DataType, ModelRunType, AirTemp_C, Shortwave_Wm2,
         Inflow_cms, WaterTemp_C, SRP_ugL, DIN_ugL, LightAttenuation_Kd, Chla_ugL,
         Flag_AirTemp_C, Flag_Shortwave_Wm2, Flag_Inflow_cms, Flag_WaterTemp_C, Flag_SRP_ugL,
         Flag_DIN_ugL, Flag_LightAttenuation_Kd, Flag_Chla_ugL)

write.csv(DataNEONLakes, "./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv",row.names = FALSE)


########### Air temp 

#Using this data product for air temp: https://data.neonscience.org/data-products/DP1.00002.001

at <- loadByProduct(dpID="DP1.00002.001", site=c("BARC","SUGG","CRAM","LIRO","PRLA","PRPO","TOOK"),
                     startdate="2019-01", enddate="2021-12", 
                     package="basic",
                     tabl="SAAT_30min",
                     token = Sys.getenv("NEON_TOKEN"),
                     check.size = F)

# unlist the variables and add to the global environment
list2env(at, .GlobalEnv)

#format data
at2 <- SAAT_30min %>%
  select(siteID, startDateTime, tempSingleMean) %>%
  mutate(DateTime = date(startDateTime)) %>%
  rename(Lake = siteID) %>%
  group_by(Lake, DateTime) %>%
  summarize(at = median(tempSingleMean, na.rm = TRUE)) %>%
  arrange(Lake, DateTime)

#plot formatted data
ggplot(data = at2, aes(x = DateTime, y = at))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

#use BARC and SUGG to fill in for each other here
barc_at <- at2 %>%
  filter(Lake == "BARC") %>%
  rename(BARC = at) %>%
  ungroup() %>%
  select(-Lake)

sugg_at <- at2 %>%
  filter(Lake == "SUGG") %>%
  rename(SUGG = at) %>%
  ungroup() %>%
  select(-Lake)

fl <- left_join(barc_at,sugg_at, by = "DateTime") %>%
  mutate(BARC = ifelse(is.na(BARC),SUGG,BARC),
         SUGG = ifelse(is.na(SUGG),BARC,SUGG)) %>%
  gather(BARC:SUGG, key = "Lake", value = "at")

at3 <- at2 %>%
  filter(!Lake %in% c("BARC","SUGG"))

at4 <- bind_rows(at3, fl)

#plot formatted data
ggplot(data = at4, aes(x = DateTime, y = at))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()
  

#linear interpolation to fill in missing values
at_tl <- at4 %>%
  filter(DateTime >= "2019-01-01" & DateTime <= "2021-12-31" & month(DateTime) %in% c(6:10))

lakes <- c("BARC","SUGG","LIRO","PRPO")

#create daily date vector
dates <- c(get_model_dates(model_start = "2019-06-01", model_stop = "2019-10-31", time_step = 'days'),
           get_model_dates(model_start = "2020-06-01", model_stop = "2020-10-31", time_step = 'days'),
           get_model_dates(model_start = "2021-06-01", model_stop = "2021-10-31", time_step = 'days'))
daily_dates <- tibble(dates)
colnames(daily_dates)[1] <- "DateTime"

years <- c(2019:2021)

final <- NULL

for(i in 1:length(lakes)){
  
  for(j in 1:length(years)){
    
    mylake <- at_tl %>%
      filter(Lake == lakes[i] & year(DateTime) == years[j]) %>%
      mutate(at = replace(at, cumall(is.na(at)), at[!is.na(at)][1]))
    
    temp <- left_join(subset(daily_dates, year(daily_dates$DateTime) == years[j]), mylake, by = "DateTime") %>%
      mutate(Lake = ifelse(is.na(Lake),lakes[i],Lake))
    
    med_at <- na.approx(temp$at)
    
    num_NA = length(temp$at) - length(med_at)
    
    if(num_NA > 0){
      nas <- rep(NA, times = num_NA)
      med_at = c(med_at, nas)
    }
    
    med_at_final <- na.locf(med_at)
    
    temp$at_interp <- med_at_final
    
    temp <- temp %>%
      mutate(Interp_Flag_at = ifelse(is.na(at),TRUE,FALSE))
    
    if(i == 1 & j == 1){
      final <- temp
    } else {
      final <- bind_rows(final, temp)
    }
    
  }
}

ggplot(data = final, aes(x = DateTime, y = at_interp, group = Interp_Flag_at, color = Interp_Flag_at))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

at5 <- final %>%
  rename(AirTemp_C = at_interp) %>%
  select(Lake, DateTime, AirTemp_C, Interp_Flag_at)

DataNEONLakes <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv") %>%
  select(-AirTemp_C) %>%
  left_join(., at5, by = c("Lake","DateTime")) %>%
  mutate(Flag_AirTemp_C = ifelse(Interp_Flag_at == TRUE, 1, 0)) %>%
  select(-Interp_Flag_at) %>%
  select(Lake, DateTime, Site, Depth_m, DataType, ModelRunType, AirTemp_C, Shortwave_Wm2,
         Inflow_cms, WaterTemp_C, SRP_ugL, DIN_ugL, LightAttenuation_Kd, Chla_ugL,
         Flag_AirTemp_C, Flag_Shortwave_Wm2, Flag_Inflow_cms, Flag_WaterTemp_C, Flag_SRP_ugL,
         Flag_DIN_ugL, Flag_LightAttenuation_Kd, Flag_Chla_ugL)

write.csv(DataNEONLakes, "./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv",row.names = FALSE)


########### Shortwave 

#Using this data product for shortwave: https://data.neonscience.org/data-products/DP1.00023.001

sw <- loadByProduct(dpID="DP1.00023.001", site=c("BARC","SUGG","CRAM","LIRO","PRLA","PRPO","TOOK"),
                    startdate="2019-01", enddate="2021-12", 
                    package="basic",
                    tabl="SLRNR_30min",
                    token = NEON_TOKEN,
                    check.size = F)

# unlist the variables and add to the global environment
list2env(sw, .GlobalEnv)

#format data
sw2 <- SLRNR_30min %>%
  select(siteID, startDateTime, inSWMean) %>%
  mutate(DateTime = date(startDateTime)) %>%
  rename(Lake = siteID) %>%
  group_by(Lake, DateTime) %>%
  summarize(sw = median(inSWMean, na.rm = TRUE)) %>%
  arrange(Lake, DateTime) %>%
  #inserting some QC here for super-high values
  mutate(sw = ifelse(sw > 400 | (sw > 3*mean(sw, na.rm = TRUE)),NA,sw))

#plot formatted data
ggplot(data = sw2, aes(x = DateTime, y = sw))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

#what to do about outliers here? probably set a hard cap similar to chla?
#set hard cap of 400 and also eliminated anything that was 3x greater than the mean

#linear interpolation to fill in missing values
sw_tl <- sw2 %>%
  filter(DateTime >= "2019-01-01" & DateTime <= "2021-12-31" & month(DateTime) %in% c(6:10))

lakes <- c("BARC","SUGG","LIRO","PRPO")

#create daily date vector
dates <- c(get_model_dates(model_start = "2019-06-01", model_stop = "2019-10-31", time_step = 'days'),
           get_model_dates(model_start = "2020-06-01", model_stop = "2020-10-31", time_step = 'days'),
           get_model_dates(model_start = "2021-06-01", model_stop = "2021-10-31", time_step = 'days'))
daily_dates <- tibble(dates)
colnames(daily_dates)[1] <- "DateTime"

years <- c(2019:2021)

final <- NULL

for(i in 1:length(lakes)){
  
  for(j in 1:length(years)){
    
    mylake <- sw_tl %>%
      filter(Lake == lakes[i] & year(DateTime) == years[j]) %>%
      mutate(sw = replace(sw, cumall(is.na(sw)), sw[!is.na(sw)][1]))
    
    temp <- left_join(subset(daily_dates, year(daily_dates$DateTime) == years[j]), mylake, by = "DateTime") %>%
      mutate(Lake = ifelse(is.na(Lake),lakes[i],Lake))
    
    med_sw <- na.approx(temp$sw)
    
    num_NA = length(temp$sw) - length(med_sw)
    
    if(num_NA > 0){
      nas <- rep(NA, times = num_NA)
      med_sw = c(med_sw, nas)
    }
    
    med_sw_final <- na.locf(med_sw)
    
    temp$sw_interp <- med_sw_final
    
    temp <- temp %>%
      mutate(Interp_Flag_sw = ifelse(is.na(sw),TRUE,FALSE))
    
    if(i == 1 & j == 1){
      final <- temp
    } else {
      final <- bind_rows(final, temp)
    }
    
  }
}

ggplot(data = final, aes(x = DateTime, y = sw_interp, group = Interp_Flag_sw, color = Interp_Flag_sw))+
  geom_point()+
  facet_wrap(vars(Lake), scales = "free", nrow = 2)+
  theme_bw()

sw3 <- final %>%
  rename(Shortwave_Wm2 = sw_interp) %>%
  select(Lake, DateTime, Shortwave_Wm2, Interp_Flag_sw)

DataNEONLakes <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv") %>%
  select(-Shortwave_Wm2) %>%
  left_join(., sw3, by = c("Lake","DateTime")) %>%
  mutate(Flag_Shortwave_Wm2 = ifelse(Interp_Flag_sw == TRUE, 1, 0)) %>%
  select(-Interp_Flag_sw) %>%
  select(Lake, DateTime, Site, Depth_m, DataType, ModelRunType, AirTemp_C, Shortwave_Wm2,
         Inflow_cms, WaterTemp_C, SRP_ugL, DIN_ugL, LightAttenuation_Kd, Chla_ugL,
         Flag_AirTemp_C, Flag_Shortwave_Wm2, Flag_Inflow_cms, Flag_WaterTemp_C, Flag_SRP_ugL,
         Flag_DIN_ugL, Flag_LightAttenuation_Kd, Flag_Chla_ugL)

write.csv(DataNEONLakes, "./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv",row.names = FALSE)
