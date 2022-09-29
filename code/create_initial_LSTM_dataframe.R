#Create dataframe for initial LSTM testing
#Author: Mary Lofton
#Date: 16SEP22
#Purpose: create daily dataframe from June 1, 2018 to Dec. 31, 2021 of 
#daily median of the following variables:

#'SWR
#'airtemp
#'discharge
#'inflow SRP
#'inflow DIN
#'EXO chla
#'EXO watertemp
#'FP total - crypto, green, brown, cyano at 50
#'CTD watertemp at 50
#'Kd from Secchi at 50
#'1.6 SRP at 50
#'1.6 DIN at 50

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

met <- fread("./data/predictors/FCR_Met_final_2015_2021.csv")
tail(met)


##INFLOW----
inf <- read_csv("./data/predictors/Inflow_2013_2021.csv") %>%
  mutate(WVWA_Flow_cms = ifelse(WVWA_Flag_Flow %in% c(NA, 0, 1, 3),WVWA_Flow_cms,NA)) %>%
  select(Reservoir, DateTime, WVWA_Flow_cms) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Reservoir, Date) %>%
  summarize(median_WVWA_Flow_cms = median(WVWA_Flow_cms, na.rm = TRUE)) %>%
  filter(Date >= "2018-06-01")

#18 missing values; could linear interpolate or fill with VT values
check <- inf %>%
  filter(is.na(median_WVWA_Flow_cms))

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-06-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
med_WVWA_Flow_cms <- na.approx(inf$median_WVWA_Flow_cms)

inf$median_WVWA_Flow_cms_interp <- med_WVWA_Flow_cms

inf_plot <- ggplot(data = inf)+
  geom_point(aes(x = Date, y = median_WVWA_Flow_cms_interp),size = 1, col = "red")+
  geom_line(aes(x = Date, y = median_WVWA_Flow_cms_interp), col = "red")+
  geom_point(aes(x = Date, y = median_WVWA_Flow_cms), size = 1, col = "black")+
  geom_line(aes(x = Date, y = median_WVWA_Flow_cms), col = "black")+
  ggtitle("FCR inflow (WVWA sensor; red = interpolated)")+
  ylab("median daily inflow (cms)")+
  theme_bw()
inf_plot

ggsave(inf_plot, filename = "./figures/inflow.png",height = 4, width = 6,
       units = "in", dpi = 300, dev = "png")

##EXO----
exo <- read_csv("./data/predictors/FCR_Catwalk_subset_2018_2021.csv") %>%
  mutate(EXOTemp_C_1 = ifelse(Flag_EXOTemp == 0,EXOTemp_C_1,NA)) %>%
  select(Reservoir, DateTime, EXOTemp_C_1) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Reservoir, Date) %>%
  summarize(median_EXOTemp_C_1 = median(EXOTemp_C_1, na.rm = TRUE)) %>%
  filter(Date >= "2018-06-01")

#69 missing values
check <- exo %>%
  filter(is.na(median_EXOTemp_C_1))

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-06-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
exo <- left_join(daily_dates, exo, by = "Date")
med_EXOTemp_C_1 <- na.approx(exo$median_EXOTemp_C_1)
exo$median_EXOTemp_C_1_interp <- NA
exo$median_EXOTemp_C_1_interp[67:1310] <- med_EXOTemp_C_1

exo_temp_plot <- ggplot(data = exo)+
  geom_point(aes(x = Date, y = median_EXOTemp_C_1_interp),size = 1, col = "red")+
  geom_line(aes(x = Date, y = median_EXOTemp_C_1_interp), col = "red")+
  geom_point(aes(x = Date, y = median_EXOTemp_C_1), size = 1, col = "black")+
  geom_line(aes(x = Date, y = median_EXOTemp_C_1), col = "black")+
  ggtitle("FCR water temp 1.6 m (EXO; red = interpolated)")+
  ylab("median daily water temp (degrees C)")+
  theme_bw()
exo_temp_plot

ggsave(exo_temp_plot, filename = "./figures/exo_temp.png",height = 4, width = 6,
       units = "in", dpi = 300, dev = "png")

##CHLA----
chla <- read_csv("./data/targets/EXO_chla_2018_2021.csv") %>%
  mutate(EXOChla_ugL_1 = ifelse(Flag_Chla_ugL == 0,EXOChla_ugL_1,NA)) %>%
  select(Reservoir, DateTime, EXOChla_ugL_1) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Reservoir, Date) %>%
  summarize(median_EXOChla_ugL_1 = median(EXOChla_ugL_1, na.rm = TRUE)) %>%
  filter(Date >= "2018-06-01")

#69 missing values
check <- chla %>%
  filter(is.na(median_EXOChla_ugL_1))

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-06-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
chla <- left_join(daily_dates, chla, by = "Date")
med_EXOChla_ugL_1 <- na.approx(chla$median_EXOChla_ugL_1)
chla$median_EXOChla_ugL_1_interp <- NA
chla$median_EXOChla_ugL_1_interp[67:1310] <- med_EXOChla_ugL_1

chla_plot <- ggplot(data = chla)+
  geom_point(aes(x = Date, y = median_EXOChla_ugL_1_interp),size = 1, col = "red")+
  geom_line(aes(x = Date, y = median_EXOChla_ugL_1_interp), col = "red")+
  geom_point(aes(x = Date, y = median_EXOChla_ugL_1), size = 1, col = "black")+
  geom_line(aes(x = Date, y = median_EXOChla_ugL_1), col = "black")+
  ggtitle("FCR chl-a 1.6 m (EXO; red = interpolated)")+
  ylab("median daily chlorophyll-a (ug/L)")+
  theme_bw()
chla_plot

ggsave(chla_plot, filename = "./figures/chla.png",height = 4, width = 6,
       units = "in", dpi = 300, dev = "png")

##CHEM----
#not removing flags here b/c I don't think any of these will outweigh
#the uncertainty introduced via interpolation

#COME BACK AND GRAB LAST POINT IN MAY 2018 TO DO INTERPOLATION FOR
#FIRST THREE DAYS IN JUNE; ALSO ASK DEXTER/HEATHER FOR JAN 2022 DATA
chem <- read_csv("./data/predictors/chemistry_2013-2021_FCR_50.csv") %>%
  filter(Depth_m == 1.6) %>%
  select(Reservoir, DateTime, SRP_ugL, NH4_ugL, NO3NO2_ugL) %>%
  mutate(Date = date(DateTime),
         DIN_ugL = NH4_ugL + NO3NO2_ugL) %>%
  filter(Date >= "2018-06-01" & Date <= "2021-12-31") %>%
  group_by(Reservoir, Date) %>%
  summarize(median_SRP_ugL = median(SRP_ugL, na.rm = TRUE),
            median_DIN_ugL = median(DIN_ugL, na.rm = TRUE)) %>%
  filter(Date >= "2018-06-01")

#create daily date vector
daily_dates <- tibble(get_model_dates(model_start = "2018-06-01", model_stop = "2021-12-31", time_step = 'days'))
colnames(daily_dates)[1] <- "Date"

#linear interpolation to fill in missing values
chem <- left_join(daily_dates, chem, by = "Date")
chem$Reservoir <- "FCR"
med_SRP_ugL <- na.approx(chem$median_SRP_ugL)
med_DIN_ugL <- na.approx(chem$median_DIN_ugL)

chem$median_SRP_ugL_interp <- NA
chem$median_SRP_ugL_interp[4:1285] <- med_SRP_ugL
chem$median_DIN_ugL_interp <- NA
chem$median_DIN_ugL_interp[4:1285] <- med_DIN_ugL

srp_plot <- ggplot(data = chem)+
  geom_point(aes(x = Date, y = median_SRP_ugL_interp),size = 1, col = "red")+
  geom_line(aes(x = Date, y = median_SRP_ugL_interp), col = "red")+
  geom_point(aes(x = Date, y = median_SRP_ugL), size = 1, col = "black")+
  geom_line(aes(x = Date, y = median_SRP_ugL), col = "black")+
  ggtitle("FCR SRP 1.6 m (red = interpolated)")+
  ylab("daily SRP (ug/L)")+
  theme_bw()
srp_plot

ggsave(srp_plot, filename = "./figures/srp.png",height = 4, width = 6,
       units = "in", dpi = 300, dev = "png")

din_plot <- ggplot(data = chem)+
  geom_point(aes(x = Date, y = median_DIN_ugL_interp),size = 1, col = "red")+
  geom_line(aes(x = Date, y = median_DIN_ugL_interp), col = "red")+
  geom_point(aes(x = Date, y = median_DIN_ugL), size = 1, col = "black")+
  geom_line(aes(x = Date, y = median_DIN_ugL), col = "black")+
  ggtitle("FCR DIN 1.6 m (red = interpolated)")+
  ylab("daily DIN (ug/L)")+
  theme_bw()
din_plot

ggsave(din_plot, filename = "./figures/din.png",height = 4, width = 6,
       units = "in", dpi = 300, dev = "png")
