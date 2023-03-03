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
pacman::p_load(tidyverse, lubridate, data.table)

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

#Next steps:
#1. Use Freya's code to pull in water temp data and wrangle that
#2. Use more traditional API method to pull in: Secchi, DIN, SRP
#3. Figure out what data product we want to use for NEON lakes for weather (NLDAS?)
  