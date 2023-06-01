#Trim JAGS model for chl-a
#Author: Mary Lofton
#Date: 31MAY23

#Purpose: trim OptimumSteele process model for chla from 2018-2021

pacman::p_load(tidyverse, lubridate, rjags, runjags, moments, coda, zoo)

#'Function to fit day of year model for chla
#'@param trim_window vector of start and end values for iteration window
#'@param jags.out runjags object from fit_OptimumMonod functionn

trim_OptimumMonod <- function(trim_window, jags.out, params, data, cal_dates, thin){

#trim to converged window for parameter estimates
mcmc.object <- as.mcmc.list(jags.out)
windowed.object <- window(mcmc.object, start=trim_window[1], end = trim_window[2], thin = thin)
param.object <- windowed.object[,c(params)]

#save predicted states
#assign model fit start and stop dates
start_cal <- date(cal_dates[1])
stop_cal <- date(cal_dates[2])

#assign target and predictors
df <- data %>%
  filter(Date >= start_cal & Date <= stop_cal) 

out <- as.matrix(windowed.object)
pred <- out[,grep("mu",colnames(out))]
pred <- pred[,-1]
pred <- data.frame(pred) %>%
  colMeans(.)

pred.v2 <- data.frame(model_id = "OptimumMonod",
                      datetime = df$Date,
                      variable = "chlorophyll-a",
                      prediction = unname(pred)
                      )

OptimumMonod_plot <- ggplot()+
  xlab("")+
  ylab("Chla (ug/L)")+
  geom_point(data = df, aes(x = Date, y = Chla_ugL, fill = "obs"))+
  geom_line(data = pred.v2, aes(x = datetime, y = prediction, color = "OptimumMonod"))+
  labs(color = NULL, fill = NULL)+
  theme_classic()
  

#return necessary info
return(list(param.object = param.object,
            out = pred.v2,
            summary = summary(param.object),
            crosscorr = crosscorr(param.object),
            prsf = gelman.diag(param.object),
            param_plot = plot(param.object),
            pred_plot = OptimumMonod_plot))
}