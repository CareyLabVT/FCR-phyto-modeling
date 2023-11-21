# Exploring EPA NLA data
# Author: Mary Lofton
# Date: 16OCT23

# Purpose: Explore model run corpus for Abhilash transfer learning project

# Read packages
library(tidyverse)
library(lubridate)
library(sf)
pacman::p_load("ggExtra")
library(ggExtra)


morph_metadata <- read_tsv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_site_information-metadata.txt")
morph <- read_csv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_site_information-data.csv") %>%
  filter(SITESAMP == "Y" & FRAME17 == "Include") %>%
  select(AREA_HA, LAT_DD83, LON_DD83, SITE_ID) %>%
  distinct()


prof_metadata <- read_tsv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_profile-metadata.txt")
prof <- read_csv("./Eco-KGML-transfer-learning/data/NLA 2017 Report Data Files/nla_2017_profile-data.csv") %>%
  select(SITE_ID, INDEX_LAT_DD, INDEX_LON_DD, INDEX_SITE_DEPTH) %>%
  distinct() %>%
  filter(complete.cases(.)) %>%
  mutate(INDEX_LAT_DD = as.double(INDEX_LAT_DD),
         INDEX_LON_DD = as.double(INDEX_LON_DD))
head(prof)

nla <- left_join(morph, prof, by = "SITE_ID") %>%
  filter(complete.cases(.)) %>%
  distinct()
  



# create data for world coordinates using 
# map_data() function
usa_coordinates <- map_data("state") 

# map of lakes by area
us <- ggplot() +
  geom_map(
    data = usa_coordinates, map = usa_coordinates,
    aes(x = long, y = lat, map_id = region), fill = "lightblue",
    color = "black", linewidth = 0.3
  )+
  geom_point(
    data = nla,
    aes(LON_DD83, LAT_DD83, size = AREA_HA),
    shape = 21, alpha = 1, color = "black", fill = "white"
  ) +
  xlab("")+
  ylab("")+
  ggtitle("LAKE AREA")+
  theme_classic()+
  scale_size_continuous(name = "Area (ha)")+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())

us
ggsave(us, filename = "./Eco-KGML-transfer-learning/figures/NLA2017LakeArea.png",device = "png")

# same thing but for depth of index site
us1 <- ggplot() +
  geom_map(
    data = usa_coordinates, map = usa_coordinates,
    aes(x = long, y = lat, map_id = region), fill = "khaki1",
    color = "black", linewidth = 0.3
  )+
  geom_point(
    data = nla,
    aes(INDEX_LON_DD, INDEX_LAT_DD, size = INDEX_SITE_DEPTH),
    shape = 21, alpha = 1, color = "black", fill = "white"
  ) +
  xlab("")+
  ylab("")+
  ggtitle("MID-LAKE SAMPLING SITE DEPTH")+
  theme_classic()+
  scale_size_continuous(name = "Depth (m)")+
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank())

us1
ggsave(us1, filename = "./Eco-KGML-transfer-learning/figures/NLA2017MidLakeSamplingSiteDepth.png",device = "png")

joint <- ggplot(data = nla, aes(x = log(AREA_HA), y = INDEX_SITE_DEPTH))+
  geom_point()+
  xlab("log of lake area (ha)") +
  ylab("depth at mid-lake sample site (m)") +
  theme_classic()
joint

joint1 <- ggMarginal(joint, type="histogram") 
joint1
ggsave(joint1, filename = "./Eco-KGML-transfer-learning/figures/JointDistributionLakeAreaSamplingDepth.png",device = "png")

