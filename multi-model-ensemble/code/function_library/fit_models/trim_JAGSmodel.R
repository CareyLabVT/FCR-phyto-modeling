#Trim JAGS model for chl-a
#Author: Mary Lofton
#Date: 31MAY23

#Purpose: trim OptimumSteele process model for chla from 2018-2021

pacman::p_load(tidyverse, lubridate, rjags, runjags, moments, coda, zoo)

#'Function to fit day of year model for chla
#'@param trim_window vector of start and end values for iteration window
#'@param jags.out runjags object from fit_OptimumMonod functionn

trim_JAGSmodel <- function(trim_window, jags.out){

#trim to converged window for parameter estimates
mcmc.object <- as.mcmc.list(jags.out)
windowed.object <- window(mcmc.object, start=trim_window[1], end = trim_window[2])

#return necessary info
return(list(windowed.object = windowed.object,
            summary = summary(windowed.object),
            crosscorr = crosscorr(windowed.object),
            prsf = gelman.diag(windowed.object),
            plot = plot(windowed.object)))
}