#Aggregation Script
#Author: Mary Lofton
#Date: 27FEB 23

#Purpose: aggregate observed data and model output from all lakes for transfer learning
#project

#load packages
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

#Read in and combine lakes

DataFCR <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/DataFCR.csv")
ModelOutputFCR <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/ModelOutputFCR.csv")
ModelOutputMendotaSunapee <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/ModelOutputMendotaSunapee.csv")
DataNEONLakes <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/DataNEONLakes.csv")

final <- bind_rows(DataFCR, ModelOutputFCR) %>%
  mutate(Site = as.character(Site)) %>%
  bind_rows(., ModelOutputMendotaSunapee) %>%
  bind_rows(., DataNEONLakes) %>%
  filter(month(DateTime) %in% c(6:10))

unique(final$Lake)

write.csv(final, "./Eco-KGML-transfer-learning/data/TransferLearningData.csv", row.names = FALSE)
