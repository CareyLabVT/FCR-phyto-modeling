#Title: Format GLM-AED output for LSTM
#Author: Mary Lofton
#Date: 09NOV22

#Purpose: create daily dataframe from July 7 2015 to Dec. 31 2021 of 
#daily median of the following variables using GLM-AED drivers and output:

#'1. Met: SWR, air temp - from GLM input files
#'2. Inflow: discharge - from GLM input files
#'3. Catwalk: EXO chl-a, 1.6 water temp - pull from output
#'4. Flora: total - crypto, green, cyano, brown - this will be following AED functional groups instead
#'5. CTD: water temp @ 1.6 - won't have this b/c we are already pulling 1.6 m water temp above
#'6. YSI_Secchi: Kd from Secchi (not sure what would make sense to pull from output to replace this?)
#'7. Chem: SRP @ 1.6, DIN @ 1.6, SRP @ inflow, DIN @ inflow - inflow from inputs, 1.6 m from output
#'
#'Input files:
#'
#'met_avg_filtered.csv
#'FCR_weir_inflow_2013_2021_20220927_allfractions_2poolsDOC_1dot5xDOCr.csv


#load packages
#if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, ncdf4, GLMr, glmtools, data.table, zoo)

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

##MET----
met <- read_csv("./data/predictors/met_avg_filtered.csv") %>%
  select(time, AirTemp, ShortWave) %>%
  mutate(Date = date(time)) %>%
  group_by(Date) %>%
  summarize(median_ShortWave = median(ShortWave, na.rm = TRUE),
            median_AirTemp = median(AirTemp, na.rm = TRUE)) 

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2015-07-07", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
met <- left_join(daily_dates, met, by = "Date")
#not needed in this case - no missing values :-)

at_plot <- ggplot(data = met, aes(x = Date, y = median_AirTemp))+
  geom_point(size = 1)+
  ggtitle("FCR Air temperature")+
  ylab("daily air temperature (degrees C)")+
  theme_bw()
at_plot

ggsave(at_plot, filename = "./figures/air_temperature_GLM.png",height = 4, width = 10,
       units = "in", dpi = 300, dev = "png")

swr_plot <- ggplot(data = met, aes(x = Date, y = median_ShortWave))+
  geom_point(size = 1)+
  ggtitle("FCR Shortwave radiation")+
  ylab("daily shortwave radiation (W/m2)")+
  theme_bw()
swr_plot

ggsave(swr_plot, filename = "./figures/shortwave_radiation_GLM.png",height = 4, width = 10,
       units = "in", dpi = 300, dev = "png")

##INFLOW + INFLOW CHEM----
inf <- read_csv("./data/predictors/FCR_weir_inflow_2013_2021_20220927_allfractions_2poolsDOC_1dot5xDOCr.csv") %>%
  select(time, FLOW, NIT_amm, NIT_nit, PHS_frp) %>%
  mutate(Date = date(time)) %>%
  mutate(NIT_amm = NIT_amm*18.04, NIT_nit = NIT_nit*62.0049, PHS_frp = PHS_frp*94.9714) %>%
  mutate(DIN = NIT_amm + NIT_nit) %>%
  select(Date, FLOW, DIN, PHS_frp) %>%
  filter(Date >= "2015-07-07")

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2015-07-07", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
inf <- left_join(daily_dates, inf, by = "Date")
#not needed b/c there are no missing values :-)

inf_plot <- ggplot(data = inf, aes(x = Date, y = FLOW))+
  geom_point(size = 1)+
  ggtitle("FCR Inflow")+
  ylab("daily inflow (cms)")+
  theme_bw()
inf_plot

ggsave(inf_plot, filename = "./figures/inflow_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

inf_srp_plot <- ggplot(data = inf, aes(x = Date, y = PHS_frp))+
  geom_point(size = 1)+
  ggtitle("FCR phosphorus (SRP) @ inflow")+
  ylab("daily phosphorus (SRP; ug/L)")+
  theme_bw()
inf_srp_plot

ggsave(inf_srp_plot, filename = "./figures/inflow_srp_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

inf_din_plot <- ggplot(data = inf, aes(x = Date, y = DIN))+
  geom_point(size = 1)+
  ggtitle("FCR nitrogen (DIN) @ inflow")+
  ylab("daily nitrogen (DIN; ug/L)")+
  theme_bw()
inf_din_plot

ggsave(inf_din_plot, filename = "./figures/inflow_din_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

##VARIABLES FROM GLM-AED OUTPUT----

#'NOTES:
#'Output variables include: water temp @ 1.6 m, chl-a @ 1.6 m, each of the three phyto groups
#'@ 1.6 m, DIN @ 1.6 m, and SRP @ 1.6 m. GLMr and glmtools are not available for this version
#'of R (4.2.2) so I am accessing the output.nc file using ncdf4. 

#set file path for output.nc file
nc_file <- file.path("./data/output.nc")

#open nc file
nc <- nc_open(nc_file)

#view relevant info
print(nc)
# time  Size:56856   *** is unlimited *** 
#   units: hours since 2015-07-07 12:00:00
# ok supposedly this is the time origin, but no indication of time zone and I get funkiness
# when I convert the DateTimes - argh - not sure if this is even noon or midnight? I think
# midnight from what Quinn has said in the past

#look at variable names
attributes(nc$var)$names
# relevant var names
# "temp" index = 14
# maybe "extc_coef" index = 17
# "NIT_amm"            "NIT_nit" indices = 38, 39
# "PHS_frp" index = 44
# "PHY_cyano" index = 75
# "PHY_green" index = 98
# "PHY_diatom" index = 121
# "PHY_tchla" index = 145

# look at dimension attribute names
attributes(nc$dim)$names
# [1] "lon"     "lat"     "z"       "restart" "nzones"  "time"  

#get depths
nc_depths <- ncvar_get( nc, attributes(nc$dim)$names[3])
dim(nc_depths)

#view list of depths
#depth in GLM is from the sediments up, so we will reference a depth of 7.67 which is ~1.6 m
#from the surface, index = 49
nc_depths[,1]

#get time
time <- ncvar_get( nc, attributes(nc$dim)$names[6])

#convert time to DateTimes
DateTime <- as.POSIXct(time*3600,origin='2015-07-07 12:00')
head(DateTime)
tail(DateTime)
head(time)
#not sure what to do w/ the hour funkiness here; may not matter too much for our application;
#probably has something to do w/ daylight saving time and time zone conversion :-(

#get temperature at 1.6 m
temp <- ncvar_get(nc, attributes(nc$var)$names[14])
dim(temp)
temp_1.6 <- temp[49,]

#get chla at 1.6 m
chla <- ncvar_get(nc, attributes(nc$var)$names[145])
chla_1.6 <- chla[49,]

#get nitrate at 1.6 m and convert to ug/L
mutate(NIT_amm = NIT_amm*18.04, NIT_nit = NIT_nit*62.0049, PHS_frp = PHS_frp*94.9714) 
  
no3 <- ncvar_get(nc, attributes(nc$var)$names[39])
no3_1.6 <- no3[49,]*62.0049

#get ammonium at 1.6 m and convert to ug/L
nh4 <- ncvar_get(nc, attributes(nc$var)$names[38])
nh4_1.6 <- nh4[49,]*18.04

#sum nitrate and ammonium for DIN
din_1.6 <- no3_1.6 + nh4_1.6

#get SRP at 1.6 m and convert to ug/L
srp <- ncvar_get(nc, attributes(nc$var)$names[44])
srp_1.6 <- srp[49,]*94.9714

#get green algae at 1.6 m --> these units are not going to readily match FP
green <- ncvar_get(nc, attributes(nc$var)$names[98])
green_1.6 <- green[49,]

#get brown algae at 1.6 m --> these units are not going to readily match FP
brown <- ncvar_get(nc, attributes(nc$var)$names[121])
brown_1.6 <- brown[49,]

#get cyanobacteria at 1.6 m --> these units are not going to readily match FP
cyano <- ncvar_get(nc, attributes(nc$var)$names[75])
cyano_1.6 <- cyano[49,]

#get light extinction coefficient at 1.6 m
kd <- ncvar_get(nc, attributes(nc$var)$names[17])
kd_1.6 <- kd[49,]

#join all vars w/ DateTime
mod_out <- data.frame("DateTime" = DateTime, 
                      "temp_1.6" = temp_1.6,
                      "chla_1.6" = chla_1.6,
                      "din_1.6" = din_1.6,
                      "srp_1.6" = srp_1.6,
                      "green_1.6" = green_1.6,
                      "brown_1.6" = brown_1.6,
                      "cyano_1.6" = cyano_1.6,
                      "kd_1.6" = kd_1.6) 

#summarize to daily median
mod_out <- mod_out %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date) %>%
  summarize(temp = median(temp_1.6, na.rm = TRUE),
            chla = median(chla_1.6, na.rm = TRUE),
            din = median(din_1.6, na.rm = TRUE),
            srp = median(srp_1.6, na.rm = TRUE),
            green = median(green_1.6, na.rm = TRUE),
            brown = median(brown_1.6, na.rm = TRUE),
            cyano = median(cyano_1.6, na.rm = TRUE),
            kd = median(kd_1.6, na.rm = TRUE))

#plotting
mod_temp_plot <- ggplot(data = mod_out, aes(x = Date, y = temp))+
  geom_point(size = 1)+
  ggtitle("Modeled FCR water temperature @ 1.6 m")+
  ylab("daily median water temp (degrees C)")+
  theme_bw()
mod_temp_plot

ggsave(mod_temp_plot, filename = "./figures/GLM_water_temperature.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

mod_chla_plot <- ggplot(data = mod_out, aes(x = Date, y = chla))+
  geom_point(size = 1)+
  ggtitle("Modeled FCR chlorophyll-a @ 1.6 m")+
  ylab("daily median chlorophyll-a (ug/L)")+
  theme_bw()
mod_chla_plot

ggsave(mod_chla_plot, filename = "./figures/chla_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

mod_din_plot <- ggplot(data = mod_out, aes(x = Date, y = din))+
  geom_point(size = 1)+
  ggtitle("Modeled FCR DIN @ 1.6 m")+
  ylab("daily median DIN (ug/L)")+
  theme_bw()
mod_din_plot

ggsave(mod_din_plot, filename = "./figures/Site50_din_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

mod_srp_plot <- ggplot(data = mod_out, aes(x = Date, y = srp))+
  geom_point(size = 1)+
  ggtitle("Modeled FCR SRP @ 1.6 m")+
  ylab("daily median SRP (ug/L)")+
  theme_bw()
mod_srp_plot

ggsave(mod_srp_plot, filename = "./figures/Site50_srp_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

mod_green_plot <- ggplot(data = mod_out, aes(x = Date, y = green))+
  geom_point(size = 1)+
  ggtitle("Modeled FCR Green Algae @ 1.6 m")+
  ylab("daily median green algae biomass (ug/L)")+
  theme_bw()
mod_green_plot

ggsave(mod_green_plot, filename = "./figures/greenAlgae_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

mod_brown_plot <- ggplot(data = mod_out, aes(x = Date, y = brown))+
  geom_point(size = 1)+
  ggtitle("Modeled FCR Brown Algae @ 1.6 m")+
  ylab("daily median brown algae biomass (ug/L)")+
  theme_bw()
mod_brown_plot

ggsave(mod_brown_plot, filename = "./figures/brownAlgae_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

mod_cyano_plot <- ggplot(data = mod_out, aes(x = Date, y = cyano))+
  geom_point(size = 1)+
  ggtitle("Modeled FCR Cyanobacteria @ 1.6 m")+
  ylab("daily median cyanobacteria biomass (ug/L)")+
  theme_bw()
mod_cyano_plot

ggsave(mod_cyano_plot, filename = "./figures/cyanobacteria_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

mod_kd_plot <- ggplot(data = mod_out, aes(x = Date, y = kd))+
  geom_point(size = 1)+
  ggtitle("Modeled FCR light extinction coefficient")+
  ylab("daily median light extinction coefficient (per meter)")+
  theme_bw()
mod_kd_plot

ggsave(mod_kd_plot, filename = "./figures/kd_GLM.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

##MEGAMATRIX ------------------------------------------------

#rename inf colnames so don't confuse DIN and SRP between inf and Site 50
colnames(inf)[3:4] <- c("DIN_inflow","PHS_frp_inflow") 

mega <- left_join(met, inf, by = "Date") %>%
  left_join(., mod_out, by='Date')

colnames(mega)
mega <- mega[,c(1,3,2,4,7,10,9,6,5,14,11,13,12,8)]

write.csv(mega, file = "./data/LSTM_modeled_dataset_10NOV22.csv", row.names = FALSE)
col_key <- data.frame(column_names = colnames(mega))
write.csv(col_key, file = "./data/LSTM_modeled_dataset_column_key_10NOV22.csv")
