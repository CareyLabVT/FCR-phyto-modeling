#Interpolation Experiment Data Prep
#Author: Mary Lofton
#Date: 03APR23

#set options
options(warn=2)

#install/load packages
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

#read in data
dat <- read_csv("./Eco-KGML-transfer-learning/data/TransferLearningData.csv")

#calculate summary statistics by lake, month, variable
sumstat <- read_csv("./Eco-KGML-transfer-learning/data/TransferLearningData.csv") %>%
  mutate(AirTemp_C = ifelse(Flag_AirTemp_C == 1, NA, AirTemp_C),
         Shortwave_Wm2 = ifelse(Flag_Shortwave_Wm2 == 1, NA, Shortwave_Wm2),
         Inflow_cms = ifelse(Flag_Inflow_cms == 1, NA, Inflow_cms),
         WaterTemp_C = ifelse(Flag_WaterTemp_C == 1, NA, WaterTemp_C),
         SRP_ugL = ifelse(Flag_SRP_ugL == 1, NA, SRP_ugL),
         DIN_ugL = ifelse(Flag_DIN_ugL == 1, NA, DIN_ugL),
         LightAttenuation_Kd = ifelse(Flag_LightAttenuation_Kd == 1, NA, LightAttenuation_Kd),
         Chla_ugL = ifelse(Flag_Chla_ugL == 1, NA, Chla_ugL)) %>%
  select(-starts_with("Flag")) %>%
  pivot_longer(AirTemp_C:Chla_ugL, names_to = "variable", values_to = "value") %>%
  mutate(Month = month(DateTime)) %>%
  group_by(Lake, Month, variable) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

#for-loop to render three datasets with different simple interpolation methods

#1: LinearProcessNoise: linear interpolation with process noise added by drawing
# from a distribution with mean of 0 and sd of the observed sd of that variable
# each month

#2: HistoricalMean: missing values are replaced with the historical mean of that 
# variable during that month of the year

#3: HistoricalMeanProcessNoise: missing values are replaced with the historical mean of that 
# variable during that month of the year with process noise added by drawing
# from a distribution with mean of 0 and sd of the observed sd of that variable
# each month

dat_lpn <- dat
dat_hm <- dat
dat_hmpn <- dat

lakes <- unique(sumstat$Lake)
months <- unique(sumstat$Month)
vars <- unique(sumstat$variable)
update_cols <- colnames(dat)[-c(7:14)]

#kludge because apparently we don't have enough data in July at PRPO to estimate sd
sumstat[rowSums(is.na(sumstat)) > 0, ] 

prpo <- sumstat %>%
  filter(Lake == "PRPO" & (variable == "DIN_ugL" | variable == "SRP_ugL"))

julysddin <- mean(c(prpo$sd[1],prpo$sd[5]))
julysdsrp <- mean(c(prpo$sd[2],prpo$sd[6]))

sumstat[171,5] <- julysddin
sumstat[175,5] <- julysdsrp


for(l in 1:length(lakes)){
  for(m in 1:length(months)){
      
      var_means <- sumstat %>%
        filter(Lake == lakes[l] & Month == months[m]) %>%
        pull(mean)
      
      var_sds <- sumstat %>%
        filter(Lake == lakes[l] & Month == months[m]) %>%
        pull(sd)
      
      temp_lpn <- dat_lpn %>%
        filter(Lake == lakes[l] & month(DateTime) == months[m]) %>%
        mutate(AirTemp_C = ifelse(Flag_AirTemp_C == 1, (rnorm(length(AirTemp_C), AirTemp_C,var_sds[1])), AirTemp_C),
               Chla_ugL = ifelse(Flag_Chla_ugL == 1, (rnorm(length(Chla_ugL), Chla_ugL,var_sds[2])), Chla_ugL),
               DIN_ugL = ifelse(Flag_DIN_ugL == 1, (rnorm(length(DIN_ugL), DIN_ugL,var_sds[3])), DIN_ugL),
               Inflow_cms = ifelse(Flag_Inflow_cms == 1, (rnorm(length(Inflow_cms), Inflow_cms,var_sds[4])), Inflow_cms),
               LightAttenuation_Kd = ifelse(Flag_LightAttenuation_Kd == 1, (rnorm(length(LightAttenuation_Kd), LightAttenuation_Kd,var_sds[5])), LightAttenuation_Kd),
               Shortwave_Wm2 = ifelse(Flag_Shortwave_Wm2 == 1, (rnorm(length(Shortwave_Wm2), Shortwave_Wm2,var_sds[6])), Shortwave_Wm2),
               SRP_ugL = ifelse(Flag_SRP_ugL == 1, (rnorm(length(SRP_ugL), SRP_ugL,var_sds[7])), SRP_ugL),
               WaterTemp_C = ifelse(Flag_WaterTemp_C == 1, (rnorm(length(WaterTemp_C), WaterTemp_C,var_sds[8])), WaterTemp_C))
      
      #LinearProcessNoise
      dat_lpn <- dat_lpn %>%
        rows_update(temp_lpn, by = c(update_cols))
      
      temp_lpn <- dat_lpn %>%
        filter(Lake == lakes[l] & month(DateTime) == months[m]) %>%
        mutate(AirTemp_C = ifelse(Flag_AirTemp_C == 1, (rnorm(length(AirTemp_C), AirTemp_C,var_sds[1])), AirTemp_C),
               Chla_ugL = ifelse(Flag_Chla_ugL == 1, (rnorm(length(Chla_ugL), Chla_ugL,var_sds[2])), Chla_ugL),
               DIN_ugL = ifelse(Flag_DIN_ugL == 1, (rnorm(length(DIN_ugL), DIN_ugL,var_sds[3])), DIN_ugL),
               Inflow_cms = ifelse(Flag_Inflow_cms == 1, (rnorm(length(Inflow_cms), Inflow_cms,var_sds[4])), Inflow_cms),
               LightAttenuation_Kd = ifelse(Flag_LightAttenuation_Kd == 1, (rnorm(length(LightAttenuation_Kd), LightAttenuation_Kd,var_sds[5])), LightAttenuation_Kd),
               Shortwave_Wm2 = ifelse(Flag_Shortwave_Wm2 == 1, (rnorm(length(Shortwave_Wm2), Shortwave_Wm2,var_sds[6])), Shortwave_Wm2),
               SRP_ugL = ifelse(Flag_SRP_ugL == 1, (rnorm(length(SRP_ugL), SRP_ugL,var_sds[7])), SRP_ugL),
               WaterTemp_C = ifelse(Flag_WaterTemp_C == 1, (rnorm(length(WaterTemp_C), WaterTemp_C,var_sds[8])), WaterTemp_C))
      
      dat_lpn <- dat_lpn %>%
        rows_update(temp_lpn, by = c(update_cols))
      
      #HistoricalMean
      temp_hm <- dat_hm %>%
        filter(Lake == lakes[l] & month(DateTime) == months[m]) %>%
        mutate(AirTemp_C = ifelse(Flag_AirTemp_C == 1, var_means[1], AirTemp_C),
               Chla_ugL = ifelse(Flag_Chla_ugL == 1, var_means[2], Chla_ugL),
               DIN_ugL = ifelse(Flag_DIN_ugL == 1, var_means[3], DIN_ugL),
               Inflow_cms = ifelse(Flag_Inflow_cms == 1, var_means[4], Inflow_cms),
               LightAttenuation_Kd = ifelse(Flag_LightAttenuation_Kd == 1, var_means[5], LightAttenuation_Kd),
               Shortwave_Wm2 = ifelse(Flag_Shortwave_Wm2 == 1, var_means[6], Shortwave_Wm2),
               SRP_ugL = ifelse(Flag_SRP_ugL == 1, var_means[7], SRP_ugL),
               WaterTemp_C = ifelse(Flag_WaterTemp_C == 1,var_means[8], WaterTemp_C))

      dat_hm <- dat_hm %>%
        rows_update(temp_hm, by = c(update_cols))

      #HistoricalMeanProcessNoise
      temp_hmpn <- dat_hmpn %>%
        filter(Lake == lakes[l] & month(DateTime) == months[m]) %>%
        mutate(AirTemp_C = ifelse(Flag_AirTemp_C == 1, (rnorm(length(AirTemp_C), var_means[1],var_sds[1])), AirTemp_C),
               Chla_ugL = ifelse(Flag_Chla_ugL == 1, (rnorm(length(Chla_ugL), var_means[2],var_sds[2])), Chla_ugL),
               DIN_ugL = ifelse(Flag_DIN_ugL == 1, (rnorm(length(DIN_ugL), var_means[3],var_sds[3])), DIN_ugL),
               Inflow_cms = ifelse(Flag_Inflow_cms == 1, (rnorm(length(Inflow_cms), var_means[4],var_sds[4])), Inflow_cms),
               LightAttenuation_Kd = ifelse(Flag_LightAttenuation_Kd == 1, (rnorm(length(LightAttenuation_Kd), var_means[5],var_sds[5])), LightAttenuation_Kd),
               Shortwave_Wm2 = ifelse(Flag_Shortwave_Wm2 == 1, (rnorm(length(Shortwave_Wm2), var_means[6],var_sds[6])), Shortwave_Wm2),
               SRP_ugL = ifelse(Flag_SRP_ugL == 1, (rnorm(length(SRP_ugL), var_means[7],var_sds[7])), SRP_ugL),
               WaterTemp_C = ifelse(Flag_WaterTemp_C == 1, (rnorm(length(WaterTemp_C), var_means[8],var_sds[8])), WaterTemp_C))

      dat_hmpn <- dat_hmpn %>%
        rows_update(temp_hmpn, by = c(update_cols))
  }
}

#example plot to illustrate different methods
check1 <- dat %>%
  filter(Lake == "PRPO" & year(DateTime) == 2019) 
check2 <- dat_lpn %>%
  filter(Lake == "PRPO" & year(DateTime) == 2019)   
check3 <- dat_hm %>%
  filter(Lake == "PRPO" & year(DateTime) == 2019) 
check4 <- dat_hmpn %>%
  filter(Lake == "PRPO" & year(DateTime) == 2019) 

ggplot()+
  geom_line(data = check1, aes(x = DateTime, y = DIN_ugL, color = "Linear"))+
  geom_line(data = check2, aes(x = DateTime, y = DIN_ugL, color = "Linear with process noise"))+
  geom_line(data = check3, aes(x = DateTime, y = DIN_ugL, color = "Historical mean"))+
  geom_line(data = check4, aes(x = DateTime, y = DIN_ugL, color = "Historical mean with process noise"))+
  scale_color_manual(values = c("Linear" = "black",
                            "Linear with process noise" = "gray",
                            "Historical mean" = "darkblue",
                            "Historical mean with process noise" = "lightblue"),
                     name = "")+
  ggtitle("PRPO 2019")+
  theme_bw()

#write to file
write.csv(dat_lpn, "./Eco-KGML-transfer-learning/data/data_interpolation_experiments/TransferLearningData_LinearInterpolationProcessNoise.csv",row.names = FALSE)
write.csv(dat_hm, "./Eco-KGML-transfer-learning/data/data_interpolation_experiments/TransferLearningData_HistoricalMeanInterpolation.csv",row.names = FALSE)
write.csv(dat_hmpn, "./Eco-KGML-transfer-learning/data/data_interpolation_experiments/TransferLearningData_HistoricalMeanInterpolationProcessNoise.csv",row.names = FALSE)

