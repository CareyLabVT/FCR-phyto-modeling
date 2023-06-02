#Fit OptimumSteele model for chl-a
#Author: Mary Lofton
#Date: 31May23

#Purpose: fit process models model for chla from 2018-2021

pacman::p_load(tidyverse, lubridate, rjags, runjags, moments, coda, zoo)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_OptimumSteeleNP <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  df <- data %>%
    filter(Date >= start_cal & Date <= stop_cal)
  
  #retrieve model
  model = "./multi-model-ensemble/code/JAGS_models/OptimumSteeleNP.R"
  
  #assemble data
  model.data <- list(chla = df$Chla_ugL, 
               wtemp = df$WaterTemp_C, 
               swr = df$Shortwave_Wm2,
               DIN = df$DIN_ugL,
               SRP = df$SRP_ugL,
               mu1 = df$Chla_ugL[1])
  
  variable.namesout <- c("tau_obs",
                         "Topt",
                         "I_S",
                         "ksdin",
                         "kssrp",
                         "R_growth",
                         "R_resp",
                         "mu")

  init <- list(list(tau_obs = 0.01,
                    Topt = 5,
                    I_S = 1,
                    ksdin = 0.1,
                    kssrp = 0.1,
                    R_growth = 0.01,
                    R_resp = 0.01),
               list(tau_obs = 0.1,
                    Topt = 15,
                    I_S = 100,
                    ksdin = 1,
                    kssrp = 1,
                    R_growth = 0.1,
                    R_resp = 0.1),
               list(tau_obs = 1,
                    Topt = 25,
                    I_S = 500,
                    R_growth = 0.5,
                    R_resp = 0.5)
               )

  params <- c("tau_obs",
              "Topt",
              "I_S",
              "R_growth",
              "R_resp")
  
  #run model
  jags.out <- run.jags(model = model,
                       data = model.data,
                       adapt =  5000,
                       burnin =  10000,
                       sample = 50000,
                       n.chains = 3,
                       inits = init,
                       monitor = variable.namesout)
  
  #return model fitting output + summary + list of params
  return(list(jags.out = jags.out, summary = summary(jags.out),
              params = params))
}
