#Title: Data munging
#Author: Mary Lofton
#Date: last updated 01SEP22

#Purpose: Download all published data from Environmental Data Initiative (EDI) 
#and tidy it so it can be pulled in for analysis

library(tidyverse)
library(lubridate)
library(data.table)

##Data download----

#download FP data from EDI 
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.272.6&entityid=6b3151c0fdd913e02641363c2b00ae57"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/FluoroProbe_2014_2021.csv", method='libcurl')

#download Secchi data from EDI 
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.198.10&entityid=375f87747001e1681b0e805d00cc1341"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/Secchi_depth_2013-2021.csv", method='libcurl')

#download YSI/PAR data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.198.10&entityid=b3bd353312f9e37ca392e2a5315cc9da"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/YSI_PAR_profiles_2013-2021.csv", method='libcurl')

#download filtered chl-a data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.555.2&entityid=c90ced759d8a7c994e20e7d3227a84da"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/manual_chlorophyll_2014_2021.csv", method='libcurl')

#download CTD data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.200.12&entityid=0a62d1946e8d9a511bc1404e69e59b8c"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/CTD dataset 2013-2021.csv", method='libcurl')

#download chemistry data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.199.10&entityid=aa2ccc23688fc908f9d61cb217210a3d"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/chemistry_2013_2021.csv", method='libcurl')

#download inflow data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.202.8&entityid=cc045f9fe32501138d5f4e1e7f40d492"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/Inflow_2013_2021.csv", method='libcurl')

#download met data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.389.6&entityid=a5524c686e2154ec0fd0459d46a7d1eb"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/FCR_Met_final_2015_2021.csv", method='libcurl')

#download EXO data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.271.6&entityid=23a191c1870a5b18cbc17f2779f719cf"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/FCR_Catwalk_2018_2021.csv", method='libcurl')

##Data munging----

fp <- read_csv("./data/raw/FluoroProbe_2014_2021.csv") %>%
  filter(Reservoir == "FCR" & Site == 50)
write.csv(fp, "./data/predictors/FluoroProbe_2014_2021_FCR_50.csv", row.names = FALSE)

ctd <- fread("./data/raw/CTD dataset 2013-2021.csv")
ctd <- tibble(ctd) %>%
  select(Reservoir, Site, Date, Depth_m, Temp_C, Chla_ugL, PAR_umolm2s, Desc_rate, Flag_Temp, Flag_Chla, Flag_PAR, Flag_DescRate) %>%
  filter(Reservoir == "FCR" & Site == 50) 
write.csv(ctd, "./data/predictors/CTD_2013_2021_subset_FCR_50.csv", row.names = FALSE)

met <- fread("./data/raw/FCR_Met_final_2015_2021.csv")
ctd <- tibble(ctd) %>%
  select(Reservoir, Site, Date, Depth_m, Temp_C, Chla_ugL, PAR_umolm2s, Desc_rate, Flag_Temp, Flag_Chla, Flag_PAR, Flag_DescRate) %>%
  filter(Reservoir == "FCR" & Site == 50) 
write.csv(ctd, "./data/predictors/CTD_2013_2021_subset_FCR_50.csv", row.names = FALSE)

secchi <- read_csv("./data/raw/Secchi_depth_2013-2021.csv") %>%
  filter(Reservoir == "FCR" & Site == 50)
write.csv(secchi, "./data/predictors/Secchi_depth_2013-2021_FCR_50.csv", row.names = FALSE)

chemistry <- read_csv("./data/raw/chemistry_2013_2021.csv") %>%
  filter(Reservoir == "FCR" & (Site == 50 | Site == 100))
write.csv(chemistry, "./data/predictors/chemistry_2013-2021_FCR_50_100.csv", row.names = FALSE)

ysipar <- read_csv("./data/raw/YSI_PAR_profiles_2013-2021.csv") %>%
  filter(Reservoir == "FCR" & Site == 50)
write.csv(ysipar, "./data/predictors/YSI_PAR_profiles_2013-2021_FCR_50.csv", row.names = FALSE)

chla <- read_csv("./data/raw/manual_chlorophyll_2014_2021.csv") %>%
  filter(Reservoir == "FCR" & Site == 50)
write.csv(ysipar, "./data/predictors/manual_chlorophyll_2014_2021_FCR_50.csv", row.names = FALSE)

catwalk <- fread("./data/raw/FCR_Catwalk_2018_2021.csv")
colnames(catwalk)
catwalk <- catwalk[,c(1:13,24,30,31,34,35,37,47:56,63,69:70,73:74)]
colnames(catwalk)
catwalk <- tibble(catwalk) %>%
  filter(Reservoir == "FCR" & Site == 50) 
exo <- catwalk[,c(1:3,15:16,31:32)]
catwalk_pred <- catwalk[,-c(15:16,31:32)]
write.csv(exo, "./data/targets/EXO_chla_2018_2021.csv", row.names = FALSE)
write.csv(catwalk_pred,"./data/predictors/FCR_Catwalk_subset_2018_2021.csv", row.names = FALSE)


#Create dataframe for initial LSTM testing
#Author: Mary Lofton
#Date: 16SEP22
#Purpose: create daily dataframe from Aug 1, 2018 to Dec. 31, 2021 of 
#daily median of the following variables:

#'1. Met: SWR, air temp - check
#'2. Inflow: discharge - check
#'3. Catwalk: EXO chl-a, 1.6 water temp - check
#'4. Flora: total - crypto, green, cyano, brown
#'5. CTD: water temp @ 1.6
#'6. YSI_Secchi: Kd from Secchi
#'7. Chem: SRP @ 1.6, DIN @ 1.6, SRP @ inflow, DIN @ inflow - check
#'
#'If data are missing from beginning or end of dataset, curently just filling with first
#'or most recent value

#load packages
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)

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
met_raw <- fread("./data/raw/FCR_Met_final_2015_2021.csv")
met <- tibble(met_raw) %>%
  select(Reservoir, DateTime, ShortwaveRadiationDown_Average_W_m2, Flag_ShortwaveRadiationDown_Average_W_m2, AirTemp_Average_C, Flag_AirTemp_Average_C) %>%
  filter(!Flag_ShortwaveRadiationDown_Average_W_m2 == 5, !Flag_AirTemp_Average_C == 5) %>%
  select(-Flag_ShortwaveRadiationDown_Average_W_m2, -Flag_AirTemp_Average_C) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Reservoir, Date) %>%
  summarize(median_ShortwaveRadiationDown_Average_W_m2 = median(ShortwaveRadiationDown_Average_W_m2, na.rm = TRUE),
            median_AirTemp_Average_C = median(AirTemp_Average_C, na.rm = TRUE)) %>%
  filter(Date >= "2018-08-01")

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-08-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
met <- left_join(daily_dates, met, by = "Date")
tail(met)
met_at <- na.approx(met$median_AirTemp_Average_C)
met_swr <- na.approx(met$median_ShortwaveRadiationDown_Average_W_m2)

met$median_at_interp <- met_at
met$median_swr_interp <- met_swr

met <- met %>%
  mutate(Interp_Flag_ShortwaveRadiationDown_W_m2 = ifelse(is.na(median_ShortwaveRadiationDown_Average_W_m2),TRUE,FALSE),
         Interp_Flag_AirTemp_Average_C = ifelse(is.na(median_AirTemp_Average_C),TRUE,FALSE))

at_plot <- ggplot(data = met, aes(x = Date, y = median_at_interp, col = Interp_Flag_AirTemp_Average_C))+
  geom_point(size = 1)+
  ggtitle("FCR Air temperature")+
  ylab("daily air temperature (degrees C)")+
  theme_bw()
at_plot

ggsave(at_plot, filename = "./figures/air_temperature.png",height = 4, width = 10,
       units = "in", dpi = 300, dev = "png")

swr_plot <- ggplot(data = met, aes(x = Date, y = median_swr_interp, col = Interp_Flag_ShortwaveRadiationDown_W_m2))+
  geom_point(size = 1)+
  ggtitle("FCR Shortwave radiation")+
  ylab("daily shortwave radiation (W/m2)")+
  theme_bw()
swr_plot

ggsave(swr_plot, filename = "./figures/shortwave_radiation.png",height = 4, width = 10,
       units = "in", dpi = 300, dev = "png")

#set column names to be consistent for megamatrix
met <- met[,c(1,5:8)]
met <- met[,c(1,2,5,3,4)]
colnames(met) <- c("Date","daily_median_airtemp_interp_degC","flag_daily_median_airtemp_interp_degC","daily_median_swr_interp_Wm2","flag_daily_median_swr_interp_Wm2")

##INFLOW----
inf <- read_csv("./data/predictors/Inflow_2013_2021.csv") %>%
  mutate(WVWA_Flow_cms = ifelse(WVWA_Flag_Flow %in% c(NA, 0, 1, 3),WVWA_Flow_cms,NA)) %>%
  select(Reservoir, DateTime, WVWA_Flow_cms) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Reservoir, Date) %>%
  summarize(median_WVWA_Flow_cms = median(WVWA_Flow_cms, na.rm = TRUE)) %>%
  filter(Date >= "2018-08-01")

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-08-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
inf <- left_join(daily_dates, inf, by = "Date")
tail(inf)
med_WVWA_Flow_cms <- na.approx(inf$median_WVWA_Flow_cms)

inf$median_WVWA_Flow_cms_interp <- med_WVWA_Flow_cms

inf <- inf %>%
  mutate(Interp_Flag_inf = ifelse(is.na(median_WVWA_Flow_cms),TRUE,FALSE))

inf_plot <- ggplot(data = inf, aes(x = Date, y = median_WVWA_Flow_cms_interp, col = Interp_Flag_inf))+
  geom_point(size = 1)+
  ggtitle("FCR Inflow")+
  ylab("daily inflow (cms)")+
  theme_bw()
inf_plot

ggsave(inf_plot, filename = "./figures/inflow.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

inf <- inf[,c(1,4,5)]
colnames(inf) <- c("Date","daily_median_inflow_interp_cms","flag_daily_median_inflow_interp_cms")


##EXO----
exo <- read_csv("./data/predictors/FCR_Catwalk_subset_2018_2021.csv",
                col_types = cols(
                  .default = col_double(),
                  Reservoir = col_character(),
                  DateTime = col_datetime(format = ""),
                  EXOTemp_C_1 = col_double(),
                  Flag_EXOTemp = col_double()
                )) %>%
  mutate(EXOTemp_C_1 = ifelse(Flag_EXOTemp == 0,EXOTemp_C_1,NA)) %>%
  select(Reservoir, DateTime, EXOTemp_C_1) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Reservoir, Date) %>%
  summarize(median_EXOTemp_C_1 = median(EXOTemp_C_1, na.rm = TRUE)) %>%
  filter(Date >= "2018-08-01")

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-08-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
exo <- left_join(daily_dates, exo, by = "Date")
med_EXOTemp_C_1 <- na.approx(exo$median_EXOTemp_C_1)
exo$median_EXOTemp_C_1_interp <- 26.884000
exo$median_EXOTemp_C_1_interp[6:1249] <- med_EXOTemp_C_1

exo <- exo %>%
  mutate(Interp_Flag_wt = ifelse(is.na(median_EXOTemp_C_1),TRUE,FALSE))

exo_temp_plot <- ggplot(data = exo, aes(x = Date, y = median_EXOTemp_C_1_interp, col = Interp_Flag_wt))+
  geom_point(size = 1)+
  ggtitle("FCR Water temperature @ 1.6 m depth")+
  ylab("daily water temperature (degrees C)")+
  theme_bw()
exo_temp_plot

ggsave(exo_temp_plot, filename = "./figures/exo_water_temperature.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

exo <- exo[,c(1,4,5)]
colnames(exo) <- c("Date","daily_median_watertemp_interp_degC","flag_daily_median_watertemp_interp_degC")


##CHLA----
chla <- read_csv("./data/targets/EXO_chla_2018_2021.csv",
                 col_types = cols(
                   Reservoir = col_character(),
                   Site = col_double(),
                   DateTime = col_datetime(format = ""),
                   EXOChla_RFU_1 = col_double(),
                   EXOChla_ugL_1 = col_double(),
                   Flag_Chla_RFU = col_double(),
                   Flag_Chla_ugL = col_double()
                 )) %>%
  mutate(EXOChla_ugL_1 = ifelse(Flag_Chla_ugL == 0,EXOChla_ugL_1,NA)) %>%
  select(Reservoir, DateTime, EXOChla_ugL_1) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Reservoir, Date) %>%
  summarize(median_EXOChla_ugL_1 = median(EXOChla_ugL_1, na.rm = TRUE)) %>%
  filter(Date >= "2018-08-01")

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-08-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
chla <- left_join(daily_dates, chla, by = "Date")
tail(chla)
med_EXOChla_ugL_1 <- na.approx(chla$median_EXOChla_ugL_1)
chla$median_EXOChla_ugL_1_interp <- 4.010000
chla$median_EXOChla_ugL_1_interp[6:1249] <- med_EXOChla_ugL_1

chla <- chla %>%
  mutate(Interp_Flag_chla = ifelse(is.na(median_EXOChla_ugL_1),TRUE,FALSE))

chla_plot <- ggplot(data = chla, aes(x = Date, y = median_EXOChla_ugL_1_interp, col = Interp_Flag_chla))+
  geom_point(size = 1)+
  ggtitle("FCR Chlorophyll-a @ 1.6 m depth")+
  ylab("daily chlorophyll-a (ug/L)")+
  theme_bw()
chla_plot

ggsave(chla_plot, filename = "./figures/chla.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

chla <- chla[,c(1,4,5)]
colnames(chla) <- c("Date","daily_median_chla_interp_ugL","flag_daily_median_chla_interp_ugL")


##CHEM----
#not removing flags here b/c I don't think any of these will outweigh
#the uncertainty introduced via interpolation

##start with Site 50
chem <- read_csv("./data/predictors/chemistry_2013-2021_FCR_50_100.csv",
                 col_types = cols(
                   .default = col_double(),
                   Reservoir = col_character(),
                   DateTime = col_datetime(format = ""),
                   DIC_mgL = col_double(),
                   DC_mgL = col_double(),
                   DN_mgL = col_double()
                 )) %>%
  filter(Site == 50 & Depth_m == 1.6) %>%
  select(Reservoir, DateTime, SRP_ugL, NH4_ugL, NO3NO2_ugL) %>%
  mutate(Date = date(DateTime),
         DIN_ugL = NH4_ugL + NO3NO2_ugL) %>%
  filter(Date >= "2018-08-01" & Date <= "2021-12-31") %>%
  group_by(Reservoir, Date) %>%
  summarize(median_SRP_ugL = median(SRP_ugL, na.rm = TRUE),
            median_DIN_ugL = median(DIN_ugL, na.rm = TRUE)) %>%
  filter(Date >= "2018-08-01")

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-08-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
chem <- left_join(daily_dates, chem, by = "Date")
chem$Reservoir <- "FCR"
med_SRP_ugL <- na.approx(chem$median_SRP_ugL)
med_DIN_ugL <- na.approx(chem$median_DIN_ugL)
tail(chem)

chem$median_SRP_ugL_interp <- 10
chem$median_SRP_ugL_interp[6:1224] <- med_SRP_ugL
chem$median_SRP_ugL_interp[1225:1249] <- 12

chem$median_DIN_ugL_interp <- 24
chem$median_DIN_ugL_interp[6:1224] <- med_DIN_ugL
chem$median_DIN_ugL_interp[1225:1249] <- 9

chem <- chem %>%
  mutate(Interp_Flag_srp = ifelse(is.na(median_SRP_ugL),TRUE,FALSE),
         Interp_Flag_din = ifelse(is.na(median_DIN_ugL),TRUE,FALSE))

srp_plot <- ggplot(data = chem, aes(x = Date, y = median_SRP_ugL_interp, col = Interp_Flag_srp))+
  geom_point(size = 1)+
  ggtitle("FCR phosphorus (SRP) @ 1.6 m depth")+
  ylab("daily phosphorus (SRP; ug/L)")+
  theme_bw()
srp_plot

ggsave(srp_plot, filename = "./figures/Site50_srp.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

din_plot <- ggplot(data = chem, aes(x = Date, y = median_DIN_ugL_interp, col = Interp_Flag_din))+
  geom_point(size = 1)+
  ggtitle("FCR nitrogen (DIN) @ 1.6 m depth")+
  ylab("daily nitrogen (DIN; ug/L)")+
  theme_bw()
din_plot

ggsave(din_plot, filename = "./figures/Site50_din.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

chem <- chem[,c(1,5,7,6,8)]
colnames(chem) <- c("Date","daily_median_srp_interp_ugL","flag_daily_median_srp_interp_ugL","daily_median_din_interp_ugL","flag_daily_median_din_interp_ugL")

##Now inflow chem
##start with Site 50
inf_chem <- read_csv("./data/predictors/chemistry_2013-2021_FCR_50_100.csv",
                 col_types = cols(
                   .default = col_double(),
                   Reservoir = col_character(),
                   DateTime = col_datetime(format = ""),
                   DIC_mgL = col_double(),
                   DC_mgL = col_double(),
                   DN_mgL = col_double()
                 )) %>%
  filter(Site == 100 & Depth_m == 0.1) %>%
  select(Reservoir, DateTime, SRP_ugL, NH4_ugL, NO3NO2_ugL) %>%
  mutate(Date = date(DateTime),
         DIN_ugL = NH4_ugL + NO3NO2_ugL) %>%
  filter(Date >= "2018-08-01" & Date <= "2021-12-31") %>%
  group_by(Reservoir, Date) %>%
  summarize(median_SRP_ugL = median(SRP_ugL, na.rm = TRUE),
            median_DIN_ugL = median(DIN_ugL, na.rm = TRUE)) %>%
  filter(Date >= "2018-08-01")

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-08-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
inf_chem <- left_join(daily_dates, inf_chem, by = "Date")
med_inf_SRP_ugL <- na.approx(inf_chem$median_SRP_ugL)
med_inf_DIN_ugL <- na.approx(inf_chem$median_DIN_ugL)
tail(inf_chem)

inf_chem$median_SRP_ugL_interp <- 23
inf_chem$median_SRP_ugL_interp[6:1210] <- med_inf_SRP_ugL
inf_chem$median_SRP_ugL_interp[1211:1249] <- 20

inf_chem$median_DIN_ugL_interp <- 195
inf_chem$median_DIN_ugL_interp[6:1210] <- med_inf_DIN_ugL
inf_chem$median_DIN_ugL_interp[1211:1249] <- 4

inf_chem <- inf_chem %>%
  mutate(Interp_Flag_srp = ifelse(is.na(median_SRP_ugL),TRUE,FALSE),
         Interp_Flag_din = ifelse(is.na(median_DIN_ugL),TRUE,FALSE))

inf_srp_plot <- ggplot(data = inf_chem, aes(x = Date, y = median_SRP_ugL_interp, col = Interp_Flag_srp))+
  geom_point(size = 1)+
  ggtitle("FCR phosphorus (SRP) @ inflow")+
  ylab("daily phosphorus (SRP; ug/L)")+
  theme_bw()
inf_srp_plot

ggsave(inf_srp_plot, filename = "./figures/inflow_srp.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

inf_din_plot <- ggplot(data = inf_chem, aes(x = Date, y = median_DIN_ugL_interp, col = Interp_Flag_din))+
  geom_point(size = 1)+
  ggtitle("FCR nitrogen (DIN) @ inflow")+
  ylab("daily nitrogen (DIN; ug/L)")+
  theme_bw()
inf_din_plot

ggsave(inf_din_plot, filename = "./figures/inflow_din.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

inf_chem <- inf_chem[,c(1,5,7,6,8)]
colnames(inf_chem) <- c("Date","daily_median_inflow_srp_interp_ugL","flag_daily_median_inflow_srp_interp_ugL","daily_median_inflow_din_interp_ugL","flag_daily_median_inflow_din_interp_ugL")

##SECCHI----

#NOTE: pick up here; don't calculate anything from Secchi b/c the algorithm
#doesn't care about the magnitude of the values anyways; just check flags and 
#interpolate to daily and be done!
secchi <- read_csv("./data/predictors/Secchi_depth_2013-2021_FCR_50.csv") %>%
  mutate(Date = date(DateTime)) %>%
  filter(Date >= "2018-08-01" & Date <= "2021-12-31") %>%
  group_by(Reservoir, Date) %>%
  summarize(median_Secchi_m = median(Secchi_m, na.rm = TRUE)) %>%
  filter(Date >= "2018-08-01")

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-08-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
secchi <- left_join(daily_dates, secchi, by = "Date")
med_Secchi_m <- na.approx(secchi$median_Secchi_m)
tail(secchi)

secchi$median_Secchi_m_interp <- 1.86
secchi$median_Secchi_m_interp[6:1224] <- med_Secchi_m
secchi$median_Secchi_m_interp[1225:1249] <- 1.75

secchi <- secchi %>%
  mutate(Interp_Flag_secchi = ifelse(is.na(median_Secchi_m),TRUE,FALSE))

secchi_plot <- ggplot(data = secchi, aes(x = Date, y = median_Secchi_m_interp, col = Interp_Flag_secchi))+
  geom_point(size = 1)+
  ggtitle("FCR Secchi depth")+
  ylab("daily Secchi depth (m)")+
  theme_bw()
secchi_plot

ggsave(secchi_plot, filename = "./figures/secchi.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

secchi <- secchi[,c(1,4,5)]
colnames(secchi) <- c("Date","daily_median_Secchi_interp_m","flag_daily_median_Secchi_interp_m")


##CTD----

#'use closest function to get 1.6 m, check flags, interpolate
ctd <- fread("./data/predictors/CTD_2013_2021_subset_FCR_50.csv")

ctd <- tibble(ctd) %>%
  filter(Flag_Temp == 0) %>%
  mutate(Date = date(Date)) %>%
  filter(Date >= "2018-08-01" & Date <= "2021-12-31") %>%
  select(Date, Depth_m, Temp_C) %>%
  group_by(Date) %>%
  slice(which.min(abs(as.numeric(Depth_m) - 1.6))) %>%
  select(-Depth_m) %>%
  summarize(median_Temp_C = median(Temp_C, na.rm = TRUE)) 
  
  

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-08-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
ctd <- left_join(daily_dates, ctd, by = "Date")
med_Temp_C <- na.approx(ctd$median_Temp_C)
tail(ctd)

ctd$median_Temp_C_interp <- 24.5979
ctd$median_Temp_C_interp[2:1232] <- med_Temp_C
ctd$median_Temp_C_interp[1233:1249] <- 6.0624

ctd <- ctd %>%
  mutate(Interp_Flag_wt = ifelse(is.na(median_Temp_C),TRUE,FALSE))

ctd_plot <- ggplot(data = ctd, aes(x = Date, y = median_Temp_C_interp, col = Interp_Flag_wt))+
  geom_point(size = 1)+
  ggtitle("FCR water temperature @ 1.6 m")+
  ylab("daily water temperature (degrees C)")+
  theme_bw()
ctd_plot

ggsave(ctd_plot, filename = "./figures/CTD_water_temperature.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

ctd <- ctd[,c(1,3,4)]
colnames(ctd) <- c("Date","daily_median_CTD_Temp_C","flag_daily_median_CTD_Temp_C")


##FLORA----

#'double-check with team about this - do we really want this as a predictor?
#'I think we want to assimilate these data, not have them as drivers
#'
#'07OCT22 the team wants FP in the dataframe :-)

#'get 1.6 m, check flags, interpolate
fp <- read_csv("./data/predictors/FluoroProbe_2014_2021_FCR_50.csv") %>%
  filter(Flag_GreenAlgae == 0,
         Flag_BluegreenAlgae == 0,
         Flag_BrownAlgae == 0,
         Flag_MixedAlgae == 0,
         Flag_TotalConc == 0) %>%
  mutate(Date = date(DateTime)) %>%
  filter(Date >= "2018-08-01" & Date <= "2021-12-31") %>%
  mutate(TotalConcNoMixed_ugL = TotalConc_ugL - MixedAlgae_ugL) %>%
  select(Date, Depth_m, GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, TotalConcNoMixed_ugL) %>%
  group_by(Date) %>%
  slice(which.min(abs(as.numeric(Depth_m) - 1.6))) %>%
  select(-Depth_m) %>%
  summarize(median_GreenAlgae_ugL = median(GreenAlgae_ugL, na.rm = TRUE),
            median_Bluegreens_ugL = median(Bluegreens_ugL, na.rm = TRUE),
            median_BrownAlgae_ugL = median(BrownAlgae_ugL, na.rm = TRUE),
            median_TotalConcNoMixed_ugL = median(TotalConcNoMixed_ugL, na.rm = TRUE)) 



#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-08-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
fp <- left_join(daily_dates, fp, by = "Date")
med_GreenAlgae_ugL <- na.approx(fp$median_GreenAlgae_ugL)
med_Bluegreens_ugL <- na.approx(fp$median_Bluegreens_ugL)
med_BrownAlgae_ugL <- na.approx(fp$median_BrownAlgae_ugL)
med_TotalConcNoMixed_ugL <- na.approx(fp$median_TotalConcNoMixed_ugL)

tail(fp)

fp$median_GreenAlgae_ugL_interp <- 3.25
fp$median_GreenAlgae_ugL_interp[2:1224] <- med_GreenAlgae_ugL
fp$median_GreenAlgae_ugL_interp[1225:1249] <- 0.00

fp$median_Bluegreens_ugL_interp <- 6.93
fp$median_Bluegreens_ugL_interp[2:1224] <- med_Bluegreens_ugL
fp$median_Bluegreens_ugL_interp[1225:1249] <- 2.37

fp$median_BrownAlgae_ugL_interp <- 2.32
fp$median_BrownAlgae_ugL_interp[2:1224] <- med_BrownAlgae_ugL
fp$median_BrownAlgae_ugL_interp[1225:1249] <- 30.08

fp$median_TotalConcNoMixed_ugL_interp <- 12.50
fp$median_TotalConcNoMixed_ugL_interp[2:1224] <- med_TotalConcNoMixed_ugL
fp$median_TotalConcNoMixed_ugL_interp[1225:1249] <- 32.45

fp <- fp %>%
  mutate(Interp_Flag_ga = ifelse(is.na(median_GreenAlgae_ugL),TRUE,FALSE),
         Interp_Flag_bg = ifelse(is.na(median_Bluegreens_ugL),TRUE,FALSE),
         Interp_Flag_ba = ifelse(is.na(median_BrownAlgae_ugL),TRUE,FALSE),
         Interp_Flag_tc = ifelse(is.na(median_TotalConcNoMixed_ugL),TRUE,FALSE))

ga_plot <- ggplot(data = fp, aes(x = Date, y = median_GreenAlgae_ugL_interp, col = Interp_Flag_ga))+
  geom_point(size = 1)+
  ggtitle("FCR green algae biomass @ 1.6 m")+
  ylab("daily green algae biomass (ug/L)")+
  theme_bw()
ga_plot

ggsave(ga_plot, filename = "./figures/greenAlgae.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

bg_plot <- ggplot(data = fp, aes(x = Date, y = median_Bluegreens_ugL_interp, col = Interp_Flag_bg))+
  geom_point(size = 1)+
  ggtitle("FCR cyanobacteria biomass @ 1.6 m")+
  ylab("daily cyanobacteria biomass (ug/L)")+
  theme_bw()
bg_plot

ggsave(bg_plot, filename = "./figures/blueGreens.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

ba_plot <- ggplot(data = fp, aes(x = Date, y = median_BrownAlgae_ugL_interp, col = Interp_Flag_ba))+
  geom_point(size = 1)+
  ggtitle("FCR brown algae biomass @ 1.6 m")+
  ylab("daily brown algae biomass (ug/L)")+
  theme_bw()
ba_plot

ggsave(ba_plot, filename = "./figures/brownAlgae.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

tc_plot <- ggplot(data = fp, aes(x = Date, y = median_TotalConcNoMixed_ugL_interp, col = Interp_Flag_tc))+
  geom_point(size = 1)+
  ggtitle("FCR total phytoplankton biomass @ 1.6 m")+
  ylab("daily total phytoplankton biomass (ug/L)")+
  theme_bw()
tc_plot

ggsave(tc_plot, filename = "./figures/totalConcNoMixed.png",height = 4, width = 8,
       units = "in", dpi = 300, dev = "png")

fp <- fp[,c(1,6,10,7,11,8,12,9,13)]
colnames(fp) <- c("Date","daily_median_GreenAlgae_ugL","flag_daily_median_GreenAlgae_ugL","daily_median_Bluegreens_ugL","flag_daily_median_Bluegreens_ugL","daily_median_BrownAlgae_ugL","flag_daily_median_BrownAlgae_ugL","daily_median_TotalConcNoMixed_ugL","flag_daily_median_TotalConcNoMixed_ugL")

##MEGAMATRIX----
mega <- left_join(met, inf, by='Date') %>%
  left_join(., exo, by='Date') %>%
  left_join(., chla, by='Date') %>%
  left_join(., chem, by='Date') %>%
  left_join(., inf_chem, by='Date') %>%
  left_join(., secchi, by='Date') %>%
  left_join(., ctd, by='Date') %>%
  left_join(., fp, by='Date')
colnames(mega)
mega <- mega[,c(1,2:9,12:31,10,11)]
write.csv(mega, file = "./data/LSTM_dataset_07OCT22.csv", row.names = FALSE)
col_key <- data.frame(column_names = colnames(mega))
write.csv(col_key, file = "./data/LSTM_dataset_column_key_07OCT22.csv")

##Re-munging for template at "transfer learning project data template"
# in Eco-KGML shared drive

dat <- read_csv("./Eco-KGML/data/data_processed/LSTM_dataset_07OCT22.csv")
colnames(dat)

dat2 <- dat %>%
  add_column(Lake = "FCR",
             Site = 50,
             Depth_m = 1.6,
             DataType = "observed",
             ModelRunType = NA) %>%
  rename(DateTime = Date,
         AirTemp_C = daily_median_airtemp_interp_degC,
         Shortwave_Wm2 = daily_median_swr_interp_Wm2,
         Inflow_cms = daily_median_inflow_interp_cms,
         WaterTemp_C = daily_median_watertemp_interp_degC,
         SRP_ugL = daily_median_srp_interp_ugL,
         DIN_ugL = daily_median_din_interp_ugL,
         Chla_ugL = daily_median_chla_interp_ugL) %>%
  mutate(LightAttenuation_Kd = (1.7/daily_median_Secchi_interp_m),
         Flag_AirTemp_C = ifelse(flag_daily_median_airtemp_interp_degC == TRUE,1,0),
         Flag_Shortwave_Wm2 = ifelse(flag_daily_median_swr_interp_Wm2 == TRUE,1,0),
         Flag_Inflow_cms = ifelse(flag_daily_median_inflow_interp_cms == TRUE,1,0),
         Flag_WaterTemp_C = ifelse(flag_daily_median_watertemp_interp_degC == TRUE,1,0),
         Flag_SRP_ugL = ifelse(flag_daily_median_srp_interp_ugL == TRUE,1,0),
         Flag_DIN_ugL = ifelse(flag_daily_median_din_interp_ugL == TRUE,1,0),
         Flag_LightAttenuation_Kd = ifelse(flag_daily_median_Secchi_interp_m == TRUE,1,0),
         Flag_Chla_ugL = ifelse(flag_daily_median_chla_interp_ugL == TRUE,1,0)) %>%
  select(Lake, DateTime, Site, Depth_m, DataType, ModelRunType, AirTemp_C, Shortwave_Wm2,
         Inflow_cms, WaterTemp_C, SRP_ugL, DIN_ugL, LightAttenuation_Kd, Chla_ugL,
         Flag_AirTemp_C, Flag_Shortwave_Wm2, Flag_Inflow_cms, Flag_WaterTemp_C, Flag_SRP_ugL,
         Flag_DIN_ugL, Flag_LightAttenuation_Kd, Flag_Chla_ugL)

#write to file
write.csv(dat2, "./Eco-KGML/data/data_processed/DataFCR.csv",row.names = FALSE)
