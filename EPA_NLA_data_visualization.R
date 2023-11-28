# EPA NLA data visualization
# Author: Mary Lofton
# Date: 28NOV23

# Purpose: visualize interpolated bathymetry and initial conditions profiles

# Read in packages
library(tidyverse)
library(lubridate)

# Read in data
ic <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/NLA_interpolated_initial_conditions.csv")
bth <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/NLA_interpolated_bathymetry.csv")

all <- left_join(bth, ic)
sites <- unique(all$SITE_ID)

# pick a site to visualize
i = 428
bth_plot <- all %>%
    filter(SITE_ID == sites[i]) %>%
    ggplot()+
    geom_line(aes(x = AREA_M2, y = DEPTH_M))+
    ggtitle(sites[i])+
    scale_y_reverse()+
    theme(legend.position = "none")+
    theme_bw()
bth_plot

ic_plot <- all %>%
  filter(SITE_ID == sites[i]) %>%
  select(SITE_ID, DEPTH_M, OXYGEN:PTL) %>%
  pivot_longer(OXYGEN:PTL, names_to = "variable", values_to = "value") %>%
  arrange(DEPTH_M) %>%
  ggplot(aes(x = DEPTH_M, y = value, group = variable, color = variable))+
  geom_point(size = 2)+
  geom_smooth(stat = "smooth", se = FALSE)+
  coord_flip()+
  facet_grid(cols = vars(variable), scales = "free_x")+
  scale_x_reverse()+
  ggtitle(sites[i])+
  theme_bw()
ic_plot

# sometimes profiles were collected twice! this will be important to guard for
