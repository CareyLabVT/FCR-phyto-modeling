##### PULL & TRANSFORM USGS DATA FOR GLM INFLOWS ####
  ## Includes flow, nutrient concentrations, water temperature
  ## Written by KJF AND AIK

#### LOAD PACKAGES AND SET MODEL DATES ####
#install.packages('pacman')
pacman::p_load(dataRetrieval, lubridate, segmented, tidyverse) # Load packages

startDate = ymd('2003-11-08') # Model start date
endDate = ymd('2014-12-31')   # Model end date
#### PULL RAW USGS FLOW DATA FOR ALL MENDOTA GAUGES #### 
# Read in Mendota surface inflow and outflow gauge ID's:  
# Yahara at Highway 113: 05427850, Yahara at Windsor: 05427718, 
# 6 Mile: 05427910, Pheasant Branch: 05427948, Spring Harbor: 05427965
# OUTFLOW: Yahara outlet: 05428500

mendota_flows <- c('05427850', '05427718', '05427910', '05427948', '05427965', '05428500')

Q_in <- list()    # Empty list to populate with daily discharge values

pcode <- '00060'  # Discharge, mean daily (cfs)

# readNWISdv to pull daily discharge values for each gauge between specified dates
for (i in 1:length(mendota_flows)){
  Q_in[[mendota_flows[i]]] <- readNWISdv(mendota_flows[i], parameterCd=pcode, 
                                         startDate=startDate, endDate=endDate)
  }
Q_in <- rapply(Q_in,function(x) ifelse(x<0,NA,x), how = "replace") # Replace FLOW < 0 with NA

# USGS flow data (no interpolation) in wide format ####
USGS_wide <- do.call(rbind.data.frame, Q_in) %>%
  select(-agency_cd, -X_00060_00003_cd) %>%
  spread(site_no, "X_00060_00003") %>% 
  dplyr::rename(Windsor = "05427718", Highway = "05427850", SixMile = "05427910",
         Pheasant = "05427948", Spring = "05427965", Outlet = "05428500") %>%
  mutate(Date = as.Date(Date, origin = "1970-01-01"))
#write_csv(USGS_wide, './Mendota/USGS_data/USGS_NWIS_wide.csv)

# Interpolate missing dates #### 
flow_data <- USGS_wide %>% select(Date)

# Linear interpolation between missing daily Q values for each gauge; paste to flow_data
# Note: output for all gauges is mean daily Q in m3/s! 
for(i in 1:length(mendota_flows)){ 
  val <- approx(x=Q_in[[i]][,3], y=(Q_in[[i]][,4]* 0.028316846592), # Convert cfs to m3/s
                xout= seq(startDate, endDate, by='days'), method='linear')$y  
  flow_data = cbind(flow_data, data.frame(val))
  }

colnames(flow_data) <- c('Date', 'YaharaHighway', 'YaharaWindsor' ,'SixMile', 'Pheasant', 'SpringCreek', 'YaharaOutlet')
flow_data$Date <- as.Date(flow_data$Date,"%Y-%m-%d")

# Estimate balance based on 2 gauged inflows ####
# Retain only Yahara Highway, Pheasant inflows vs. Yahara Outlet
balance <- flow_data %>%
  select(-YaharaWindsor, -SixMile, -SpringCreek) %>%
  mutate(Bal = YaharaOutlet - (YaharaHighway + Pheasant)) %>%
  mutate(Bal = replace(Bal, Bal<0, 0))

#### READ IN FULL NWIS DATA FOR TEMP AND NUTRIENTS ####
windsorData = read_csv('./Mendota/Data Aggregation/USGS_data/NWIS_Windsor.csv') # NWIS Windsor
pheasantData = read_csv('./Mendota/Data Aggregation/USGS_data/NWIS_Pheasant.csv') # NWIS Pheasant
highwayData = read_csv('./Mendota/Data Aggregation/USGS_data/NWIS_Highway.csv') # NWIS Highway 113
carbonData = read_csv('./Mendota/Data Aggregation/USGS_data/stream_weekly_carbon_ysi_v1.csv') # J.Hart C data
metData <- read_csv('./Mendota/GLM/met_hourly.csv') # Meteorological data

#### TEMPERATURE ####
# Estimate water temp as function of mean daily air temp
dailyTemp <- metData %>% filter(time >= startDate & time <= endDate) %>%
  mutate(datetime = as.Date(time, format='%m/%d/%Y'), datetime = round_date(datetime, unit="day")) %>%
  group_by(datetime) %>% summarize(AirTemp = mean(AirTemp))

# Highway inflow water temp (includes NA values for 2011-09-30 to 2014-10-05)
HwyTemp <- highwayData %>%
  filter(mdy(as.character(datetime)) >= startDate & mdy(as.character(datetime)) <= endDate) %>%
  select(datetime, TEMP = ends_with('00010_00003')) %>%
  mutate(datetime = as.Date(datetime, format = '%m/%d/%Y'))

allTemps <- left_join(dailyTemp, HwyTemp) # join air, Hwy temps by date

# Segment analysis for InflowWaterTemp ~ AirTemp
my.seg <- segmented(lm(TEMP ~ AirTemp, data= (allTemps %>% na.omit)), 
                    seg.Z = ~ AirTemp, psi = list(AirTemp = c(0, 20)))

# Extract slope, intercept for segments
b0 <- coef(my.seg)[[1]] # 1st seg. intercept
b1 <- coef(my.seg)[[2]] # 1st seg. slope

c1 <- coef(my.seg)[[2]] + coef(my.seg)[[3]] # 2nd seg. slope
break1 <- my.seg$psi[[3]]
c0 <- b0 + b1 * break1 - c1 * break1 # 2nd seg. intercept

d1 <- coef(my.seg)[[4]] + c1 # 3rd seg. slope
break2 <- my.seg$psi[[4]]
d0 <- c0 + c1 * break2 - d1 * break2 # 3rd seg. intercept

# Fill NA values from HwyTemp based on segment best fit lines and breakpoints
allTemps <- allTemps %>% 
  mutate(waterTemp = if_else(is.na(allTemps$TEMP), 
                             if_else(allTemps$AirTemp >= break2, (d0 + d1 * allTemps$AirTemp),
                                     if_else((allTemps$AirTemp < break2 & allTemps$AirTemp >= break1), (c0 + c1 * allTemps$AirTemp),
                                             if_else(allTemps$AirTemp < break1, (b0 + b1 * allTemps$AirTemp), 0.1))),
                             allTemps$TEMP)) 

# Estimate NA values for HwyTemp
HwyTemp <- HwyTemp %>%
  mutate(TEMP = if_else(is.na(TEMP), allTemps$waterTemp, TEMP), TEMP = if_else(TEMP <0, 0, TEMP))

# Fill NA Pheasant temps based on segment best fit lines and breakpoints
PheasTemp <- pheasantData %>%
  filter(mdy(as.character(datetime)) >= startDate & mdy(as.character(datetime)) <= endDate) %>%
  select(datetime, TEMP = ends_with('00010_00003')) %>%
  mutate(datetime = as.Date(datetime, format = '%m/%d/%Y'), TEMP = as.double(TEMP), 
         TEMP = if_else(is.na(TEMP), allTemps$waterTemp, TEMP), TEMP = if_else(TEMP <0, 0, TEMP))

#### CARBON ####
convertC = 1000 / 12.01 # Convert C: mg/L to mmol/m3; C molar mass 12.01 g/mol

# Static median DOC and POC for each gauge
WindsC <- carbonData %>% filter(sample_site == "Yahara") %>%
  summarise(doc = median(doc * convertC, na.rm = T), poc = median(poc * convertC, na.rm=T))

PheasC <- carbonData %>% filter(sample_site == "Pheasant Branch") %>%
  summarise(doc = median(doc * convertC, na.rm = T), poc = median(poc * convertC, na.rm=T))

#### NITROGEN ####
convertN = 1000 / 14.01 # Convert N: mg/L to mmol/m3; N molar mass 14.01 g/mol 
convertFlow <- 0.028316846592 # Convert ft3/s to m3/s

## Pull Nitrogen data for each site from USGS .csv's ##
WindsN <- windsorData %>%
  filter(mdy(as.character(datetime)) >= startDate & mdy(as.character(datetime)) <= endDate) %>%
  select(datetime, Q = ends_with('00060_00003'), amm = ends_with('00608_00003'), 
         ammOrg = ends_with('00625_00003'), nit = ends_with('00631_00003')) %>%
  mutate(amm = as.numeric(amm) * convertN, ammOrg = as.numeric(ammOrg) * convertN, 
         nit=as.numeric(nit) * convertN, Q = Q * convertFlow, PON = (ammOrg-amm)) 

gather(WindsN, nutrient, conc, amm:PON) %>%
  group_by(nutrient) %>%
  do(mod = lm(log(conc) ~ log(Q), data=.)) %>%
  mutate (slope = summary(mod)$coeff[2], int = summary(mod)$coeff[1])

amm_modW = exp(1.1749817 * log(WindsN$Q) + 1.237242)
nit_modW = exp(-0.2884606 * log(WindsN$Q) + 6.203113)
PON_modW = exp(0.5921538 * log(WindsN$Q) + 3.779407)

WindsN <- WindsN %>% mutate (amm = if_else(is.na(amm), amm_modW, amm)) %>%
  mutate(nit = if_else(is.na(nit), nit_modW, nit)) %>% 
  mutate(PON = if_else(is.na(PON), PON_modW, PON))

PheasN <- pheasantData %>%
  filter(mdy(as.character(datetime)) >= startDate & mdy(as.character(datetime)) <= endDate) %>%
  select(datetime, Q = ends_with('00060_00003'), amm = ends_with('00608_00003'), 
         ammOrg = ends_with('00625_00003'), nit = ends_with('00631_00003')) %>%
  mutate(amm = as.numeric(amm) * convertN, ammOrg = as.numeric(ammOrg) * convertN, 
         nit=as.numeric(nit) * convertN, Q = Q * convertFlow, PON = (ammOrg-amm))

gather(PheasN, nutrient, conc, amm:PON) %>%
  group_by(nutrient) %>%
  do(mod = lm(log(conc) ~ log(Q), data=.)) %>%
  mutate (slope = summary(mod)$coeff[2], int = summary(mod)$coeff[1])

amm_modP = exp(0.11085963 * log(PheasN$Q) + 2.854888)
nit_modP = exp(0.06247405 * log(PheasN$Q) + 4.439594)
PON_modP = exp(0.21080683 * log(PheasN$Q) + 4.433546)

PheasN <- PheasN %>% mutate (amm = if_else(is.na(amm), amm_modP, amm)) %>%
  mutate(nit = if_else(is.na(nit), nit_modP, nit)) %>% 
  mutate(PON = if_else(is.na(PON), PON_modP, PON))

#### PHOSPHORUS ####
## Pull TP data for each site from USGS .csv's ##
WindsTP <- windsorData %>%
  filter(mdy(as.character(datetime)) >= startDate & mdy(as.character(datetime)) <= endDate) %>%
  select(datetime, TP=ends_with('00665_00003'))

PheasTP <- pheasantData %>%
  filter(mdy(as.character(datetime)) >= startDate & mdy(as.character(datetime)) <= endDate) %>%
  select(datetime, TP=ends_with('00665_00003'))

## PHOSPHORUS CONVERSION FACTOR AND FRACTIONATION ## 
convertP = 1000 / 30.97 # Convert P: mg/L to mmol/m3 (P molar mass 30.97 g/mol)

# Phosphorus: fractions as proportion of TP. 
pMultiplier = 2 # Multiplied by 2 because adsorbed est. equal to USGS gauge TP

FRP_winds = convertP * pMultiplier * WindsTP$TP * 0.23485 # FRP
DOP_winds = convertP * pMultiplier * WindsTP$TP * 0.195   # OGM_dop
POP_winds = convertP * pMultiplier * WindsTP$TP * 0.07015 # OGM_pop
ads_winds = convertP * pMultiplier * WindsTP$TP * 0.5     # PHS_frp_ads

FRP_pheas = convertP * pMultiplier * PheasTP$TP * 0.1507  # FRP
DOP_pheas = convertP * pMultiplier * PheasTP$TP * 0.195   # OGM_dop
POP_pheas = convertP * pMultiplier * PheasTP$TP * 0.15425 # OGM_pop
ads_pheas = convertP * pMultiplier * PheasTP$TP * 0.5     # PHS_frp_ads

#### WRITE ALL INFLOW NUTRIENTS TO .CSV FILES ####
pheasant_inflow <- balance %>% select(Date, Pheasant) %>% 
  dplyr::rename(time = Date, FLOW = Pheasant) %>%
  add_column(SALT = 0, TEMP = PheasTemp$TEMP) %>%  
  add_column(OGM_doc = PheasC$doc, OGM_poc = PheasC$poc) %>% # Carbon
  add_column(OGM_don = (PheasN$nit * .1), NIT_nit = PheasN$nit, NIT_amm = PheasN$amm, OGM_pon = PheasN$PON) %>% # Nitrogen
  add_column(PHS_frp = FRP_pheas, OGM_dop = DOP_pheas, # Phosphorus
             OGM_pop = POP_pheas, PHS_frp_ads = ads_pheas) %>% mutate_at(vars(-time), funs(round(.,4))) %>%
  write_csv('./Mendota/GLM/inflow_Pheasant.csv')

highway_inflow <- balance %>% select(Date, YaharaHighway) %>% 
  dplyr::rename(time = Date, FLOW = YaharaHighway) %>% 
  add_column(SALT = 0, TEMP = HwyTemp$TEMP) %>%  
  add_column(OGM_doc = WindsC$doc, OGM_poc = WindsC$poc) %>% # Carbon
  add_column(OGM_don = (WindsN$nit * .1), NIT_nit = WindsN$nit, NIT_amm = WindsN$amm, OGM_pon = WindsN$PON) %>% # Nitrogen
  add_column(PHS_frp = FRP_winds, OGM_dop = DOP_winds, # Phosphorus
             OGM_pop = POP_winds, PHS_frp_ads = ads_winds) %>% mutate_at(vars(-time), funs(round(.,4))) %>%
  write_csv('./Mendota/GLM/inflow_YaharaHighway.csv')

balance_inflow <- balance %>% select(Date, Bal) %>% 
  dplyr::rename(time = Date, FLOW = Bal) %>% 
  add_column(SALT = 0, TEMP = HwyTemp$TEMP) %>%  
  add_column(OGM_doc = WindsC$doc, OGM_poc = WindsC$poc) %>% # Carbon 
  add_column(OGM_don = (WindsN$nit * .1), NIT_nit = WindsN$nit, NIT_amm = WindsN$amm, OGM_pon = WindsN$PON) %>% # Nitrogen
  add_column(PHS_frp = FRP_winds, OGM_dop = DOP_winds, # Phosphorus
             OGM_pop = POP_winds, PHS_frp_ads = ads_winds) %>% mutate_at(vars(-time), funs(round(.,4))) %>%
  write_csv('./Mendota/GLM/inflow_Balance.csv')

outflow <- balance %>% select(Date, YaharaOutlet) %>% 
  dplyr::rename(time = Date, FLOW = YaharaOutlet) %>%
  write_csv('./Mendota/GLM/outflow.csv')