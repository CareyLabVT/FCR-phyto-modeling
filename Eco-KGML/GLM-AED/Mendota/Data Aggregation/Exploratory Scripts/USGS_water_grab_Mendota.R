# Pull USGS stream discharge data ####
# Last modified 2-Oct-17 by KJF

pacman::p_load(cowplot, dataRetrieval, ggplot2, plyr, RColorBrewer, reshape2) # load all relevant packages

##################### PULL USGS FLOW DATA FOR ALL MENDOTA GAUGES #########################
# Read in Mendota surface inflow and outflow gauge ID's:  
  # Yahara at Highway 113: 05427850, Yahara at Windsor: 05427718, 
  # 6 Mile: 05427910, Pheasant Branch: 05427948, Spring Harbor: 05427965
  # OUTFLOW: Yahara outlet: 05428500

mendota_flows <- c('05427850', '05427718', '05427910', '05427948', '05427965', '05428500')

Q_in <- list() # Create empty list to populate with daily discharge values

startDate <- '2003-11-08' # Date that outlet gauge record begins

pcode <- '00060' # Focal parameter: Discharge, mean daily, cfs

# Create empty start values for model start and end dates
mod_start <- as.Date('2003-11-08') # Date that outlet gauge record begins
mod_end <- as.Date('2014-12-31') # Model run end date

# readNWISdv to pull daily discharge values for each gauge between specified dates
for (i in 1:length(mendota_flows)){
  Q_in[[mendota_flows[i]]] <- readNWISdv(mendota_flows[i], parameterCd=pcode, startDate=startDate)
}

Q_in <- rapply(Q_in,function(x) ifelse(x<0,NA,x), how = "replace") # Replace FLOW < 0 with NA

# Save Raw USGS flow data (no interpolation) in wide format ####
`*tmp*` <- do.call(rbind.data.frame, Q_in)
rawUSGS_wide <- dcast(`*tmp*`, Date ~ site_no, value.var="X_00060_00003")
rawUSGS_wide <- rename(rawUSGS_wide, c("05427718"="Windsor", "05427850"="Highway", "05427910"="SixMile",
                       "05427948"="Pheasant", "05427965"="Spring", "05428500"="Outlet"))
rawUSGS_wide$Date <- as.Date(rawUSGS_wide$Date, origin = "1970-01-01")

# Interpolate missing dates #### 
# create continuous set of dates between mod_start mod_end ###
dates <- seq(as.Date(mod_start), as.Date(mod_end), by='days')

# Create empty dataframe with continuous dates onto which you'll paste your Q data
flow_data <- data.frame(Date=dates) # dataframe w/ 1st column 'date'

# Linear interpolation between missing daily Q values for each gauge; paste to flow_data
# Note: output for all gauges is mean daily Q in m3/s! 
for(i in 1:length(mendota_flows)){ 
  val <- approx(x=Q_in[[i]][,3], y=(Q_in[[i]][,4]* 0.028316846592), xout= dates, method='linear')$y  # Calculate directly as m3/s, not cfs from gauge
  flow_data = cbind(flow_data, data.frame(val))
}

colnames(flow_data) <- c('Date', 'YaharaHighway', 'YaharaWindsor' ,'SixMile', 'Pheasant', 'SpringCreek', 'YaharaOutlet')
flow_data$Date <- as.Date(flow_data$Date,"%Y-%m-%d")

# Save csv of all Mendota gauges ####
# write.csv(flow_data, './Mendota NP/USGS_gauge_data_Mendota.csv',row.names=FALSE)

##################### ESTIMATE BALANCE BASED ON 2 GAUGED INFLOWS ##########################
# Retain only Yahara Highway, Pheasant inflows vs. Yahara Outlet
balance <- flow_data[-c(3,4,6)]
balance$Bal <- balance$YaharaOutlet - (balance$YaharaHighway + balance$Pheasant)

# Visualize "Balance" flow over time
ggplot(balance, aes(x= Date, y = Bal)) + 
  geom_line() + 
  geom_hline(yintercept=0, col='red',lty=2) +
  scale_y_continuous("Balance Flow (m3/s)", limits=c(-40,10)) +
  scale_x_date(date_breaks="1 year", date_labels = "%Y")

# Change negative balance values to 0 inflow
balance[balance <= 0] <- 0
attach(balance)

# Dataframes for inflow balance, Pheasant, YaharaHighway, and outflow
Yahara_Highway <- data.frame(time=Date, FLOW=YaharaHighway, SALT=0)
Pheasant <- data.frame(time=Date, FLOW=Pheasant, SALT=0)
Balance <- data.frame(time=Date, FLOW=Bal, SALT=0)
Outflow <- data.frame(time=Date, FLOW=YaharaOutlet)

# Eventual additional columns for full inflow files
# TEMP=NA, OGM_doc=NA, OGM_poc=NA, OGM_don=NA, NIT_nit=NA, NIT_amm=NA, OGM_pon=NA,	PHS_frp=NA,	OGM_dop=NA,	OGM_pop=NA,	PHS_frp_ads=NA

# Write dataframes as csv's
write.csv(Yahara_Highway, row.names=FALSE, file='./NP_comparison/Mendota/GLM/inflow_YaharaHighway.csv')
write.csv(Pheasant, row.names=FALSE, file='./NP_comparison/Mendota/GLM/inflow_Pheasant.csv')
write.csv(Balance, row.names=FALSE, file='./NP_comparison/Mendota/GLM/inflow_Balance.csv')
write.csv(Outflow, row.names=FALSE, file='./NP_comparison/Mendota/GLM/outflow.csv')

##################### EXPLORE PROPORTIONAL FLOW CONTRIBUTIONS OF DIFFERENT STREAMS ########
# Calculate proportional contribution to outflow for period where all inflows gauged ####
prop_flow <- subset(flow_data, Date > as.Date("2012-07-25"))
attach(prop_flow)

prop_flow$propWind <- YaharaWindsor / YaharaOutlet
prop_flow$propHwy <- YaharaHighway / YaharaOutlet
prop_flow$prop6Mi <- SixMile / YaharaOutlet
prop_flow$propPhes <- Pheasant / YaharaOutlet
prop_flow$propSpr <- SpringCreek / YaharaOutlet

prop_flow[,2:7] <- NULL # remove gauge Q columns; retain proportions
colnames(prop_flow) <- c('Date', 'Windsor','Highway','6Mile' ,'Pheasant', 'SpringCr')

prop_long <- melt(prop_flow, id.vars=c('Date'))

# Average proportion of flow, Aug 2012 to present
mean(prop_flow$Windsor) # Windsor mean == 25.1%
mean(prop_flow$Highway) # Highway mean == 67.0%
mean(prop_flow$'6Mile') # 6 Mile == 23.6%
mean(prop_flow$Pheasant) # Pheasant == 6.7%
mean(prop_flow$SpringCr) # Spring == 1.9%

# With Highway, sum inflow mean is 99.2% of outflow
# with Windsor, sum inflow mean is 57.3% of outflow

##################### EXPLORATORY PLOTTING FOR INFLOW/OUTFLOW BALANCE #####################
# Plot Yahara Highway ~ Yahara Windsor ####
# How much higher Q is Highway? (1.80 - 2.85x Windsor)
attach(flow_data)

plot(YaharaHighway ~ YaharaWindsor, ylim=c(0,10), xlim=c(0,10), 
     ylab=expression(Yahara~Highway~(m^3~s^-1)), xlab=expression(Yahara~Windsor~(m^3~s^-1)))
abline(0,1, col='black', lty=2, lwd=3) # 1:1 line
abline(lm(YaharaHighway ~ 0+YaharaWindsor, data=flow_data), col='blue', lwd=3) # fit for full dataset
abline(lm(YaharaHighway ~ 0+YaharaWindsor, data=subset(flow_data, YaharaWindsor < 3)), col='cyan', lwd=3) # fit for low-flows
legend("bottomright", c("1:1 line", "All points", "Windsor Q < 3"), lty=c(2,1,1), col=c('black','blue','cyan'))

# ggplots of flow contributions by stream ####
mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=16, colour='black'), axis.text.y=element_text(size=16, colour='black'), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))

flow_long <- melt(flow_data, id.vars=c('Date')) # reshape flow_data from wide to long

# Compare two Yahara inflows (Highway, Windsor) to outflow ####
all <- ggplot(subset(flow_long, variable %in% c('YaharaHighway', 'YaharaWindsor','YaharaOutlet')), 
              aes(x=Date, y =value, lty=variable, colour=variable)) +
  geom_line() + 
  scale_y_continuous(limits=c(0,40)) + labs(y=expression(Flow~~(m^3~s^-1))) +
  scale_x_date('',date_breaks=c('1 year'), date_labels="%Y")+
  scale_colour_manual(values=c('#e41a1c','#377eb8', '#000000')) +
  scale_linetype_manual(values=c(1,1,2)) + 
  theme(legend.position=c(1,1), legend.justification=c(1,1),axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(geom="text", x = as.Date('2004-03-01'), y = 38, label="All flows")

trunc <- ggplot(subset(flow_long, variable %in% c('YaharaHighway', 'YaharaWindsor','YaharaOutlet')), 
                aes(x=Date, y =value, lty=variable, colour=variable)) +
  geom_line() + 
  scale_y_continuous(limits=c(0,15)) + labs(y=expression(Flow~~(m^3~s^-1))) +
  scale_x_date(date_breaks=c('1 year'), date_labels="%Y")+
  scale_colour_manual(values=c('#e41a1c','#377eb8', '#000000')) +
  scale_linetype_manual(values=c(1,1,2)) + 
  theme(legend.position='none',axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(geom="text", x = as.Date('2004-06-01'), y = 15, label="Low flow only")

plot_grid(all, trunc, ncol=1, align='v')

# Plot all contributing inflows versus outflow; Windsor, Highway gauges on separate plots ####
windsor <- ggplot(subset(flow_long, variable!='YaharaHighway'), 
                  aes(x = Date, y = value, group= variable, colour = variable, lty=variable)) + mytheme+
  geom_line(lwd=.65) +
  scale_x_date("",date_breaks=c('1 year'), date_labels="%Y")+
  scale_y_continuous(limits=c(0,15)) + labs(y=expression(Flow~~(m^3~s^-1))) +
  scale_colour_manual(values=c('#377eb8', '#4daf4a','#984ea3','#ff7f00','#000000')) + 
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position=c(1,1), legend.justification=c(1,1))+
  scale_linetype_manual(values=c(1,1,1,1,2))

highway <- ggplot(subset(flow_long, variable!='YaharaWindsor'), 
                  aes(x = Date, y = value, group= variable, colour = variable, lty=variable)) + mytheme+
  geom_line(lwd=.65) +
  scale_x_date(date_breaks=c('1 year'), date_labels="%Y")+
  scale_y_continuous(limits=c(0,15)) + labs(y=expression(Flow~~(m^3~s^-1))) +
  scale_colour_manual(values=c('#e41a1c', '#4daf4a','#984ea3','#ff7f00','#000000')) + 
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position=c(1,1), legend.justification=c(1,1))+
  scale_linetype_manual(values=c(1,1,1,1,2))

plot_grid(windsor, highway, ncol=1, align='v')

# Proportion of outflow provisioned by each inflow [Windsor or Highway, plus 6Mile, Pheasant, SpringCr]
windsor2 <- ggplot(subset(prop_long, variable!='Highway'), 
                   aes(x = Date, y = value, group= variable, colour = variable)) + mytheme+
  geom_line(lwd=.65) +
  scale_x_date("",date_breaks=c('1 year'), date_labels="%Y")+
  scale_y_continuous(limits=c(0,5)) + labs(y='Proportion of outflow') +
  geom_hline(yintercept=1, lty=2)+
  scale_colour_manual(values=c('#377eb8', '#4daf4a','#984ea3','#ff7f00','#000000')) + 
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1),legend.position=c(1,1), legend.justification=c(1,1))

highway2 <- ggplot(subset(prop_long, variable!='Windsor'), 
                   aes(x = Date, y = value, group= variable, colour = variable)) + mytheme+
  geom_line(lwd=.65) +
  scale_x_date(date_breaks=c('1 year'), date_labels="%Y")+
  scale_y_continuous(limits=c(0,5)) + labs(y='Proportion of outflow') +
  geom_hline(yintercept=1, lty=2)+
  scale_colour_manual(values=c('#e41a1c', '#4daf4a','#984ea3','#ff7f00','#000000')) + 
  theme(legend.title = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1),legend.position=c(1,1), legend.justification=c(1,1))

plot_grid(windsor2, highway2, ncol=1, align='v')