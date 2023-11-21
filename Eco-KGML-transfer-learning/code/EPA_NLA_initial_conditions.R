# Building initial conditions dataset for EPA NLA lakes
# Author: Mary Lofton
# DAte: 20NOV23

# Purpose: build interpolated initial conditions dataset for EPA NLA lakes

# for current FCR-GLM configuration, need IC for:
# temperature
# oxygen
# ammonium
# nitrate
# phosphate
# doc

# EPA NLA has profiles of oxygen, and values for all other vars except phosphate,
# but they do have total phosphorus

# need to read in the interpolated bathymetry data files, join to temperature
# and oxygen profiles, and then populate profiles with surface measurements of 
# nutrients and carbon

# Load packages
library(tidyverse)
library(lubridate)

prof_metadata <- read_tsv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_profile-metadata.txt")
prof <- read_csv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_profile-data.csv") %>%
  filter(is.na(FLAG)) %>%
  select(SITE_ID, DATE_COL, DEPTH, OXYGEN, TEMPERATURE) %>%
  distinct() %>%
  filter(complete.cases(.)) %>%
  arrange(SITE_ID, DATE_COL, DEPTH) %>%
  rename(DEPTH_M = DEPTH)

bathy <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/NLA_interpolated_bathymetry.csv") %>%
  arrange(SITE_ID, DEPTH_M)

temp_oxy <- left_join(prof, bathy, by = c("SITE_ID","DEPTH_M")) %>%
  arrange(SITE_ID, DATE_COL, DEPTH_M)

chem_metadata <- read_tsv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_water_chemistry_chla-metadata.txt")
chem <- read_csv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_water_chemistry_chla-data.csv") %>%
  filter(SITE_ID %in% temp_oxy$SITE_ID & ANALYTE %in% c("DOC","NITRATE_N","AMMONIA_N","PTL")) %>%
  select(SITE_ID, ANALYTE, RESULT) %>%
  group_by(SITE_ID, ANALYTE) %>%
  summarize(MEAN_RESULT = mean(RESULT, na.rm = TRUE))


samp_dates <- temp_oxy %>%
  select(SITE_ID, DATE_COL) %>%
  filter(!is.na(DATE_COL)) %>%
  distinct()

lakes <- unique(samp_dates$SITE_ID)

for(i in 1:length(lakes)){
  
  lake <- temp_oxy %>%
    filter(SITE_ID == lakes[i])
  
  lake_bathy <- bathy %>%
    filter(SITE_ID == lakes[i]) %>%
    select(SITE_ID, DEPTH_M, ELEVATION_M, AREA_M2)
  
  lake_chem <- chem %>%
    filter(SITE_ID == lakes[i]) %>%
    spread(ANALYTE, MEAN_RESULT)
  
  if(all(is.na(lake$LAT_DD83))) next
  
  sparse_bathy_depths <- seq(0, max(lake$INDEX_SITE_DEPTH, na.rm = TRUE),by = 1)
  
  if(!max(lake$INDEX_SITE_DEPTH) %in% sparse_bathy_depths){
    sparse_bathy_depths <- c(sparse_bathy_depths, max(lake$INDEX_SITE_DEPTH))
  }
  
  sparse_bathy <- lake_bathy %>%
    filter(DEPTH_M %in% sparse_bathy_depths)
  
  date_cols <- lake %>%
    filter(!is.na(DATE_COL)) %>%
    pull(DATE_COL) %>%
    unique()
  
  for(j in 1:length(date_cols)){
    
    temp <- lake %>%
      filter(DATE_COL == date_cols[j]) %>%
      full_join(sparse_bathy, by = c("SITE_ID","DEPTH_M","ELEVATION_M","AREA_M2")) %>%
      arrange(DEPTH_M) %>%
      fill(SITE_ID:AREA_M2, .direction = "updown") 
    
    temp1 <- temp %>%
      add_column(DOC = lake_chem$DOC,
             AMMONIA_N = lake_chem$AMMONIA_N,
             NITRATE_N = lake_chem$NITRATE_N,
             PTL = lake_chem$PTL)
    
    if(i == 1 & j == 1){
      final <- temp1
    } else {
      final <- bind_rows(final, temp1)
    }
    
  }

}

write.csv(final, file = "./Eco-KGML-transfer-learning/data/data_processed/NLA_interpolated_initial_conditions.csv",row.names = FALSE)
