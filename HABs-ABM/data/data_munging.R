#Title: Data munging
#Author: Mary Lofton
#Date: last updated 20DEC22

#Purpose: Download all published data from Environmental Data Initiative (EDI) 
#and tidy it so it can be pulled in for analysis

library(tidyverse)
library(lubridate)
library(data.table)

##Data download----

#download FP data from EDI 
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.272.6&entityid=6b3151c0fdd913e02641363c2b00ae57"
destination <- "./data/raw"

download.file(data,destfile = "./HABs-ABM/data/FluoroProbe_2014_2021.csv", method='libcurl')

##Data munging----

fp <- read_csv("./HABs-ABM/data/FluoroProbe_2014_2021.csv") %>%
  filter(Reservoir == "FCR" & Site == 50 & year(DateTime) == 2018 & month(DateTime) %in% c(8:12))

fp2 <- fp %>%
  filter(date(DateTime) == "2018-08-02" & Depth_m <= 9.3) %>%
  mutate(TotalConcNoMixed_ugL = TotalConc_ugL - MixedAlgae_ugL) %>%
  select(DateTime, Depth_m, GreenAlgae_ugL, Bluegreens_ugL, BrownAlgae_ugL, TotalConcNoMixed_ugL)

# Assign all depths in between 0.1 m increments to the relevant increment (e.g., anything 
# between 0.06 - 0.15 is 0.1; anything between 0.16 and 0.25 is 0.2; etc.)
fp3 <- fp2 %>%
  mutate(Depth_inc = round(Depth_m, 1)) %>%
  group_by(Depth_inc) %>%
  summarize(GreenAlgae_ugL = as.double(mean(GreenAlgae_ugL, na.rm = TRUE)),
            Bluegreens_ugL = as.double(mean(Bluegreens_ugL, na.rm = TRUE)),
            BrownAlgae_ugL = as.double(mean(BrownAlgae_ugL, na.rm = TRUE)),
            TotalConcNoMixed_ugL = as.double(mean(TotalConcNoMixed_ugL, na.rm = TRUE)))

#actual missing depths: 0.1, 0.2, 0.3, 0.4, 6.9, 8.3
depths <- data.frame(Depth_inc = c(0.1, 0.2, 0.3, 0.4, 6.9, 8.3),
                     GreenAlgae_ugL = rep(NA,6),
                     Bluegreens_ugL = rep(NA,6),
                     BrownAlgae_ugL = rep(NA,6),
                     TotalConcNoMixed_ugL = rep(NA,6))
depths <- tibble(depths)

fp4 <- bind_rows(depths,fp3) %>%
  arrange(Depth_inc)
fp4[c(1:4),c(2:5)] <- fp4[5,c(2:5)]
fp4[69,c(2:5)] <- fp4[70,c(2:5)]
fp4[83,c(2:5)] <- fp4[84,c(2:5)]

ggplot(data = fp4, aes(x = TotalConcNoMixed_ugL, y = Depth_inc))+
  geom_path()+
  scale_y_reverse()+
  theme_classic()

write.csv(fp4,"./HABs-ABM/data/FluoroProbe_2018-08-02_FCR_50.csv",row.names = FALSE)
