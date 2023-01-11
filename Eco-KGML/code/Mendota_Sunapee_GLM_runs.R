# Cross-Scale Interactions Module ####
 # This module was initially developed by Carey, C.C. and K.J. Farrell. 13 Aug. 2017.
 # Macrosystems EDDIE: Cross-Scale Interactions. Macrosystems EDDIE Module 2, Version 1. 
 # module2.macrosystemseddie.org
 # Module development was supported by NSF EF 1702506.

# modified for use in Eco-KGML project by Mary Lofton, 20DEC22

Sys.setenv(R_INSTALL_STAGED = FALSE)
# This sets the environment variables for R version 3.6.0 

# Install packages
# install.packages('sp') 
# install.packages('devtools') 
# devtools::install_github("CareyLabVT/GLMr") 
# devtools::install_github("CareyLabVT/glmtools") 

# Load the packages you just downloaded
library(sp) 
library(devtools)
library(glmtools) 
library(GLMr) 
library(tidyverse)
library(lubridate)
# If this worked, GLMr should load without error messages. Hooray!

# See what version of GLM you are running- should be v.2.x.x
glm_version() 

#set lake names
lakes <- c("Mendota","Sunapee")

#set scenario names
scen <- c("baseline","plus1","plus2","plus3","plus4","plus5","plus6")

#set degrees warming
deg <- c(0:6)

#run model scenarios
for(i in 1:length(lakes)){
  
  for(j in 1:length(scen)){
    
    ##!! Edit this line to define the sim_folder location for your model lake. 
    sim_folder <- paste0("./Eco-KGML/GLM-AED/",lakes[i],"/GLM/",scen[j]) 

    
    # # Alter met file according to scenario
    # met <- read_csv(paste0(sim_folder,"/met_hourly.csv")) %>%
    #   mutate(AirTemp = AirTemp + deg[j])
    # write.csv(met, file = paste0(sim_folder,"/met_hourly.csv"), row.names = FALSE)
    # 
    # Run model
    run_glm(sim_folder, verbose=TRUE) 
    
    
  }
}

#plot met data to be sure we have adjusted airtemp
plotdata <- tibble(
  Lake = character(),
  Scenario = character(),
  AirTemp = numeric()
)

for(i in 1:length(lakes)){
  
  for(j in 1:length(scen)){
    
    ##!! Edit this line to define the sim_folder location for your model lake. 
    sim_folder <- paste0("./Eco-KGML/GLM-AED/",lakes[i],"/GLM/",scen[j]) 
    
    # Alter met file according to scenario
    met <- read_csv(paste0(sim_folder,"/met_hourly.csv")) %>%
      mutate(Lake = lakes[i],
             Scenario = scen[j]) %>%
      select(Lake, Scenario,AirTemp)
    
    # Bind to final
    plotdata <- bind_rows(plotdata,met)
  }
}

ggplot(data = plotdata, aes(x = AirTemp, group = Scenario, color = Scenario))+
  geom_density()+
  facet_grid(rows = vars(Lake))+
  theme_classic()
#hooray the AirTemp was altered properly

################################################
# Format output for LSTM
################################################

#create empty dataframe for GLM-AED data
out <- data.frame(lake = character(),
                  scenario = character(),
                  time = as.POSIXct(character()),
                  AirTemp = double(),
                  ShortWave = double(),
                  FLOW = double(),
                  DIN.inf = double(),
                  PHS_frp.inf = double(),
                  PHY_TCHLA_1.5 = double(),
                  PHS_frp_1.5 = double(),
                  NIT_amm_1.5 = double(),
                  NIT_nit_1.5 = double(),
                  temp_1.5 = double(),
                  extc_coef_1.5 = double(),
                  brown = double(),
                  cyano = double(),
                  green = double())

vars <- c("PHY_TCHLA","PHS_frp","NIT_amm","NIT_nit","temp","extc_coef")
phyto.M <- c("PHY_CYANOPCH1","PHY_CYANONPCH2","PHY_CHLOROPCH3",
             "PHY_DIATOMPCH4")
phyto.S <- c("PHY_CRYSOPCH1","PHY_CYANONPCH2","PHY_CHLOROPCH3",
             "PHY_DIATOMPCH4")

#' Notes: phyto groups are not the same
#' Mendota has two cyano groups, one of which is N-fixing
#' Sunapee has one cyano and one chryso group
#' Should sum the two Mendota cyano groups
#' Should sum the diatom and chryso groups at Sunapee
#' This will be most comparable to FP
#' However, this means that you will need two separate loops for Sunapee and Mendota for 
#' phyto groups


#pull and append met data
for(i in 1:length(lakes)){
  
  for(j in 1:length(scen)){
    
    ##!! Edit this line to define the sim_folder location for your model lake. 
    sim_folder <- paste0("./Eco-KGML/GLM-AED/",lakes[i],"/GLM/",scen[j]) 
    
    # Pull AirTemp and ShortWave from met files and assign lake and scenario columns
    temp.met <- read_csv(paste0(sim_folder,"/met_hourly.csv")) %>%
      select(time,AirTemp,ShortWave) %>%
      mutate(lake = lakes[i],
             scenario = scen[j],
             time = date(time)) %>%
      group_by(lake, scenario, time) %>%
      summarize(AirTemp = median(AirTemp, na.rm = TRUE),
                ShortWave = median(ShortWave, na.rm = TRUE)) %>%
      filter(time >= "2003-11-08" & time <= "2014-12-31")
    
    # Pull discharge data from inflow files and append to met
    if(lakes[i] == "Mendota"){
      
    temp.inf.1 <- read_csv(paste0(sim_folder,"/inflow_Balance.csv")) %>%
      select(time, FLOW, NIT_amm, NIT_nit, PHS_frp) %>%
      mutate(NIT_amm = NIT_amm*18.04, NIT_nit = NIT_nit*62.0049, PHS_frp = PHS_frp*94.9714) %>%
      mutate(DIN = NIT_amm + NIT_nit) %>%
      select(time, FLOW, DIN, PHS_frp) 
    
    temp.inf.2 <- read_csv(paste0(sim_folder,"/inflow_Pheasant.csv")) %>%
      select(time, FLOW, NIT_amm, NIT_nit, PHS_frp) %>%
      mutate(NIT_amm = NIT_amm*18.04, NIT_nit = NIT_nit*62.0049, PHS_frp = PHS_frp*94.9714) %>%
      mutate(DIN = NIT_amm + NIT_nit) %>%
      select(time, FLOW, DIN, PHS_frp) 
    
    temp.inf.3 <- read_csv(paste0(sim_folder,"/inflow_YaharaHighway.csv")) %>%
      select(time, FLOW, NIT_amm, NIT_nit, PHS_frp) %>%
      mutate(NIT_amm = NIT_amm*18.04, NIT_nit = NIT_nit*62.0049, PHS_frp = PHS_frp*94.9714) %>%
      mutate(DIN = NIT_amm + NIT_nit) %>%
      select(time, FLOW, DIN, PHS_frp) 
    
    temp.inf <- left_join(temp.inf.1, temp.inf.2, by = "time") %>%
      left_join(., temp.inf.3, by='time') %>%
      mutate(FLOW = FLOW + FLOW.x + FLOW.y,
             DIN = DIN + DIN.x + DIN.y,
             PHS_frp = PHS_frp + PHS_frp.x + PHS_frp.y) %>%
      select(time, FLOW, DIN, PHS_frp) %>%
      rename(DIN.inf = DIN,
             PHS_frp.inf = PHS_frp)
    
    } else {
      
      temp.inf <- read_csv(paste0(sim_folder,"/oneInflow.csv")) %>%
        select(time, FLOW, NIT_amm, NIT_nit, PHS_frp) %>%
        mutate(NIT_amm = NIT_amm*18.04, NIT_nit = NIT_nit*62.0049, PHS_frp = PHS_frp*94.9714) %>%
        mutate(DIN = NIT_amm + NIT_nit) %>%
        select(time, FLOW, DIN, PHS_frp) %>%
        rename(DIN.inf = DIN,
               PHS_frp.inf = PHS_frp)
      
    }
    
    # Join all drivers
    temp.drivers <- left_join(temp.met, temp.inf, by = "time") 
    temp <- temp.drivers[-1,]
    
    # Pull model output data from nc files and append to drivers
    nc_file <- file.path(sim_folder, 'output.nc') 
    
    for(k in 1:length(vars)){
      
      depths = 1.5
      
      var <- get_var(nc_file, var_name = vars[k], reference="surface", z_out=depths) %>%
        mutate(time = date(DateTime)) %>%
        select(-DateTime)
      
      temp <- left_join(temp,var,by = "time")
      
    }
    
    phyto.temp <- temp[,"time"]
    
    if(lakes[i] == "Mendota"){
      for(p in 1:length(phyto.M)){
        depths = 1.5
        
        var <- get_var(nc_file, var_name = phyto.M[p], reference="surface", z_out=depths) %>%
          rename(time = DateTime)
        
        phyto.temp <- left_join(phyto.temp,var,by = "time")
      }
      
      phytos1 <- phyto.temp %>%
        mutate(cyano = PHY_CYANOPCH1_1.5 + PHY_CYANONPCH2_1.5) %>%
        rename(brown = PHY_DIATOMPCH4_1.5,
               green = PHY_CHLOROPCH3_1.5) %>%
        select(time, brown, cyano, green)
    } else {
      for(p in 1:length(phyto.S)){
        depths = 1.5
        
        var <- get_var(nc_file, var_name = phyto.S[p], reference="surface", z_out=depths) %>%
          mutate(time = date(DateTime)) %>%
          select(-DateTime)
        
        phyto.temp <- left_join(phyto.temp,var,by = "time")
      }
      
      phytos1 <- phyto.temp %>%
        mutate(brown = PHY_CRYSOPCH1_1.5 + PHY_DIATOMPCH4_1.5) %>%
        rename(cyano = PHY_CYANONPCH2_1.5,
               green = PHY_CHLOROPCH3_1.5) %>%
        select(time, brown, cyano, green)
    }
    
    temp <- left_join(temp, phytos1, by = "time")
      
    # Append data to other lake-scenario combinations
    out <- rbind(out, temp)
    
  }
}

sim_vars(file = nc_file)
#remember to sum NIT_amm and NIT_nit after

out1 <- out %>%
  mutate(DIN = NIT_amm_1.5 + NIT_nit_1.5) %>%
  rename(Date = time,
         median_AirTemp = AirTemp,
         median_ShortWave = ShortWave,
         srp = PHS_frp_1.5,
         din = DIN,
         PHS_frp_inflow = PHS_frp.inf,
         DIN_inflow = DIN.inf,
         kd = extc_coef_1.5,
         chla = PHY_TCHLA_1.5,
         temp = temp_1.5) %>%
  select(-NIT_amm_1.5,-NIT_nit_1.5)

check <- read_csv("./Eco-KGML/data/LSTM_modeled_dataset_10NOV22.csv")
colnames(check)

out2 <- out1[,c("lake","scenario",colnames(check))]
write.csv(out2,"./Eco-KGML/data/LSTM_modeled_dataset_Mendota_Sunapee_21DEC22.csv",row.names = FALSE)
col_key <- data.frame(column_names = colnames(out2))
write.csv(col_key, file = "./Eco-KGML/data/LSTM_modeled_dataset_Mendota_Sunapee_column_key_21DEC22.csv")
