## Script to establish flow vs. nitrogen relationships for Yahara, Pheasant data ####
# last updated 13 Oct. 2017 by KJF

#install.packages('pacman')
pacman::p_load(plyr, tidyverse) # Load packages

# Pull in USGS gauge data
pheas <- read.csv('./NP_comparison/Mendota/USGS_data/NWIS_Pheasant.csv')
windsor <- read.csv('./NP_comparison/Mendota/USGS_data/NWIS_Windsor.csv')

## Subset dataframes to retain gauge ID, datetime, and N variables and validity codes ####   
# Ammonia (00608); Ammonia + organic N (00625); Nitrate + nitrite (00631)
pheas <- select(pheas, matches('site_no|datetime|00060|00608|00625|00631'))
names(pheas)[1:10] <- c('site','datetime','flow','flow_cd','nit','nit_cd','ammorg','ammorg_cd','amm','amm_cd')
pheas <- select(pheas, -contains('_cd'))

windsor <- select(windsor, matches('site_no|datetime|00060|00608|00625|00631')) 
names(windsor)[1:10] <- c('site','datetime','flow','flow_cd','ammorg','ammorg_cd','amm','amm_cd','nit','nit_cd')
windsor <- select(windsor, -contains('_cd'))

flows <- full_join(pheas, windsor)
flows$site <- factor(flows$site, levels=c('5427948', '5427718'), labels=c('Pheas','Winds'))
flows$datetime <- lubridate::mdy(flows$datetime)
flows$orgPON <- (flows$ammorg - flows$amm)

flows <- gather(flows, nutrient, value, -site, -datetime, -flow, na.rm=T)

# Build Q vs. N relationships per N species ####
# Quick visualization for Amm, Nit, 
ggplot(flows, aes(x=datetime, y = value, fill=site))+
  geom_point(pch=21) +
  geom_smooth(method='lm')+
  facet_grid(. ~ nutrient)

ggplot(flows, aes(x=log(flow), y = log(value)))+
  geom_point(pch=21) +
  geom_smooth(method='lm')+
  facet_grid(. ~ nutrient)

# Calcualte linear fit for each N species by site as a function of flow
# Split data by site and nutrient  ####
splt.by <- c('nutrient')
nuts <- split(flows, flows[,splt.by] )

nutrient_slopes <- ldply(nuts, function(nuts) {
  lm <-lm(log(value) ~ log(flow), data=nuts)
  r.sq <- summary(lm)$r.squared
  slope <- (summary(lm)$coefficients[2])
  Fv <- as.numeric((summary(lm))$fstatistic) 
  ss <- sum(is.nan(Fv)) >0 
  P <- ifelse(!ss, 1-pf(Fv[1],Fv[2],Fv[3]),NA) #Is slope sig. diff. from zero
  
  data.frame(nuts$nutrient, slope, r.sq, P)
})
nutrient_slopes<- unique(nutrient_slopes)
nutrient_slopes$.id <- NULL
colnames(nutrient_slopes) <- c('Nutrient', 'Slope', 'R-Squared', 'P Value')

# Back apply to estimate N for unknown dates

# Save N data to inflow files

