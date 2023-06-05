#Format data for each model and fit models from 2018-2021
#Author: Mary Lofton
#Date: 13MAR23

#Purpose: Format Falling Creek Data downloaded from EDI to inputs needed for each model
#and fit models from 2018-2021

library(tidyverse)
library(lubridate)

#Load model fitting functions
fit.model.functions <- list.files("./multi-model-ensemble/code/function_library/fit_models")
sapply(paste0("./multi-model-ensemble/code/function_library/fit_models/",fit.model.functions),source,.GlobalEnv)

#Read in data
dat_historicalMean <- read_csv("./multi-model-ensemble/data/data_processed/historicalMean.csv")
dat_DOY <- read_csv("./multi-model-ensemble/data/data_processed/DOY.csv")
dat_ETS <- read_csv("./multi-model-ensemble/data/data_processed/ETS.csv")
dat_ARIMA <- read_csv("./multi-model-ensemble/data/data_processed/ARIMA.csv")
dat_TSLM <- read_csv("./multi-model-ensemble/data/data_processed/TSLM.csv")
dat_processModels <- read_csv("./multi-model-ensemble/data/data_processed/processModels.csv")

#Fit models (not applicable for persistence model)
fit_historicalMean <- fit_historicalMean(data = dat_historicalMean, cal_dates = c("2018-08-06","2021-12-31"))
fit_historicalMean$plot

fit_DOY <- fit_DOY_chla(data = dat_DOY, cal_dates = c("2018-08-06","2021-12-31"))
fit_DOY$plot

fit_ETS <- fit_ETS(data = dat_ETS, cal_dates = c("2018-08-06","2021-12-31"))
fit_ETS$plot

fit_ARIMA <- fit_ARIMA(data = dat_ARIMA, cal_dates = c("2018-08-06","2021-12-31"))
fit_ARIMA$plot

fit_TSLM <- fit_TSLM(data = dat_TSLM, cal_dates = c("2018-08-06","2021-12-31"))
fit_TSLM$plot

#process models fit in JAGS may need trimming

#OptimumMonod
#fit model
fit_OM <- fit_OptimumMonod(data = dat_processModels, cal_dates = c("2018-08-06","2021-12-31"))

#plot parameters
for (i in 1:length(fit_OM$params)){
  plot(fit_OM$jags.out, vars = fit_OM$params[i])
}

#trim model
trim_OM <- trim_OptimumMonod(jags.out = fit_OM$jags.out, 
                   trim_window = c(20001, 50000),
                   params = fit_OM$params,
                   data = dat_processModels,
                   cal_dates = c("2018-08-06","2021-12-31"),
                   thin = 3)
plot(trim_OM$param.object)
trim_OM$pred_plot

#write fitted model info to file 
save(fit_OM, trim_OM, file = "./multi-model-ensemble/model_output/OptimumMonod_output.rds")

#OptimumSteele
#fit model
fit_OS <- fit_OptimumSteele(data = dat_processModels, cal_dates = c("2018-08-06","2021-12-31"))

#drop chain that misbehaved
jags.divide <- divide.jags(fit_OS$jags.out, which.chains=c(2,3))                     

#plot parameters
for (i in 1:length(fit_OS$params)){
  plot(jags.divide, vars = fit_OS$params[i])
}

#trim model
trim_OS <- trim_OptimumSteele(jags.out = jags.divide, 
                             trim_window = c(20001, 50000),
                             params = fit_OS$params,
                             data = dat_processModels,
                             cal_dates = c("2018-08-06","2021-12-31"),
                             thin = 3)
plot(trim_OS$param.object)
trim_OS$pred_plot

#write fitted model info to file - eventually this should save fit_OptimumMonod
#instead of jags.out and params, so can keep straight among models
save(fit_OS, trim_OS, file = "./multi-model-ensemble/model_output/OptimumSteele_output.rds")

#OptimumSteeleNP
#fit model
fit_SNP <- fit_OptimumSteeleNP(data = dat_processModels, cal_dates = c("2018-08-06","2021-12-31"))

#plot parameters
for (i in 1:length(fit_SNP$params)){
  plot(fit_SNP$jags.out, vars = fit_SNP$params[i])
}

#trim model
trim_SNP <- trim_OptimumSteeleNP(jags.out = fit_SNP$jags.out, 
                              trim_window = c(30001, 60000),
                              params = fit_SNP$params,
                              data = dat_processModels,
                              cal_dates = c("2018-08-06","2021-12-31"),
                              thin = 3)
plot(trim_SNP$param.object)
trim_SNP$pred_plot

#write fitted model info to file - eventually this should save fit_OptimumMonod
#instead of jags.out and params, so can keep straight among models
save(fit_SNP, trim_SNP, file = "./multi-model-ensemble/model_output/OptimumSteeleNP_output.rds")



#Stack model predictions and write to file (not applicable for persistence model
#and currently not supported for models fit in JAGS)
mod_output <- bind_rows(fit_DOY$out, fit_ARIMA$out, fit_ETS$out)

#OR if you only want to run (or re-run) one or a few models
mod_output <- read_csv("./multi-model-ensemble/model_output/calibration_output.csv") %>%
  #filter(!model_id %in% c("OptimumMonod")) %>% #names of re-run models if applicable
  bind_rows(.,trim_SNP$out) %>% #bind rows with models to add/replace if applicable
  bind_rows(.,trim_OS$out)
  
write.csv(mod_output, "./multi-model-ensemble/model_output/calibration_output.csv", row.names = FALSE)

