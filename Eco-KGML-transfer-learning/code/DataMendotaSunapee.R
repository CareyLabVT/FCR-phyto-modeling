#Title: Mendota/Sunapee Data munging
#Author: Mary Lofton
#Date: last updated 27FEB23

#Purpose: Wrangle observed data from Sunapee and Mendota for transfer learning project
#according to template at "transfer learning project data template"
# in Eco-KGML shared drive

# Load packages
# install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

# Read in data

# met & inflow
met_inf <- read_csv("./Eco-KGML/data/data_processed/ModelOutputMendotaSunapee.csv") 
colnames(met_inf)
met_inf2 <- met_inf %>%
  mutate(DataType = "observed",
         ModelRunType = NA,
         WaterTemp_C = NA,
         SRP_ugL = NA,
         DIN_ugL = NA,
         LightAttenuation_Kd = NA,
         Chla_ugL = NA)
  

# water quality
men <- read_csv("./Eco-KGML/data/data_raw/DataMendotaSunapee/ntl38_v3.csv")
sun <- read_csv("./Eco-KGML/data/data_raw/DataMendotaSunapee/limnol_allsources_to2013.csv")

colnames(men)
colnames(sun)

#Let's do Sunapee first as it's more complicated (ha!)
unique(sun$variable)
unique(sun$depth.m)
unique(sun$depth.measurement)
unique(sun$sitetype)

sun_chla <- sun %>%
  filter(sitetype == "pelagic" & variable == "chla_ugL" & year(date) >= 1990)

ggplot(data = sun_chla, aes(x = date, y = value)) +
  geom_point()+
  theme_bw()

#Now Mendota
unique(men$depth_range_m)
unique(sun$depth.m)
unique(sun$depth.measurement)
unique(sun$sitetype)

men_chla <- men %>%
  filter(lakeid == "ME" & (depth_range_m == "0-1" | depth_range_m == "0-2") & year4 >= 2005)

ggplot(data = men_chla, aes(x = sampledate, y = correct_chl_fluor)) +
  geom_point()+
  theme_bw()


#DON'T FORGET TO ADJUST FLAGS!

## Notes 27FEB23: 
#' Right now the resolution of these data may be too low for our purposes, as chl-a
#' is measured at most twice a month and not all year round.
#' In addition, there are some key variables missing: water temp for Sunapee and
#' SRP, DIN, Kd, water temp for Mendota.
#' Are there datasets that have the variables we need at sufficiently high resolution
#' to make it worthwhile to track down observed data from these lakes?
