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

