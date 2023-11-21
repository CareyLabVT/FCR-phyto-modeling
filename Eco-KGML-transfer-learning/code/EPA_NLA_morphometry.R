# Building morphometry dataset for EPA NLA lakes
# Author: Mary Lofton
# DAte: 20NOV23

# Purpose: build interpolated bathymetry and initial conditions dataset for EPA NLA lakes

# Load packages
library(tidyverse)
library(lubridate)

morph_metadata <- read_tsv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_site_information-metadata.txt")
morph <- read_csv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_site_information-data.csv") %>%
  filter(SITESAMP == "Y" & FRAME17 == "Include") %>%
  select(AREA_HA, LAT_DD83, LON_DD83, SITE_ID, ELEVATION) %>%
  distinct()

prof_metadata <- read_tsv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_profile-metadata.txt")
prof <- read_csv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_profile-data.csv") %>%
  select(SITE_ID, INDEX_SITE_DEPTH) %>%
  distinct() %>%
  filter(complete.cases(.)) 

nla <- left_join(morph, prof, by = "SITE_ID") %>%
  filter(complete.cases(.)) %>%
  distinct()

lakes <- unique(nla$SITE_ID)

final <- tibble(ELEVATION_M = numeric(),
                AREA_M2 = numeric(),
                SITE_ID = character(),
                DEPTH_M = numeric())

for(i in 1:length(unique(nla$SITE_ID))){
  lake <- nla %>%
    filter(SITE_ID == lakes[i]) %>%
    filter(INDEX_SITE_DEPTH == max(INDEX_SITE_DEPTH)) %>%
    distinct()
  
  elevs <- seq((lake$ELEVATION - max(lake$INDEX_SITE_DEPTH)),lake$ELEVATION, by = 0.1)
  depths <- rev(seq(0, max(lake$INDEX_SITE_DEPTH),by = 0.1))
  
  if(!lake$ELEVATION %in% elevs){
    elevs <- c(elevs, lake$ELEVATION)
  }
  if(!max(lake$INDEX_SITE_DEPTH) %in% depths){
    depths <- c(max(lake$INDEX_SITE_DEPTH), depths)
  }
  
  areas <- seq(0,lake$AREA_HA,length.out = length(depths))*10000
  
  df <- tibble(ELEVATION_M = elevs,
               AREA_M2 = areas,
               SITE_ID = lakes[i],
               INDEX_SITE_DEPTH = lake$INDEX_SITE_DEPTH[1],
               DEPTH_M = depths)
  
  final <- bind_rows(final, df)
}

bathy <- left_join(final, nla, by = c("SITE_ID","INDEX_SITE_DEPTH")) %>%
  rename(SURFACE_AREA_HA = AREA_HA,
         SURFACE_ELEVATION_M = ELEVATION) %>%
  select(SITE_ID, LAT_DD83, LON_DD83, SURFACE_AREA_HA, INDEX_SITE_DEPTH, SURFACE_ELEVATION_M, ELEVATION_M, DEPTH_M, AREA_M2)

write.csv(bathy, file = "./Eco-KGML-transfer-learning/data/data_processed/NLA_interpolated_bathymetry.csv",row.names = FALSE)

# 