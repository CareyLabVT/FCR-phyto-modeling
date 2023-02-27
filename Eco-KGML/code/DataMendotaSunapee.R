#Title: Mendota/Sunapee Data munging
#Author: Mary Lofton
#Date: last updated 27FEB23

#Purpose: Wrangle observed data from Sunapee and Mendota for transfer learning project
#according to template at "transfer learning project data template"
# in Eco-KGML shared drive

# Load packages
# install.packages('pacman')
pacman::p_load(tidyverse, lubridate)

# Read in data
men <- read_csv("./Eco-KGML/data/data_raw/DataMendotaSunapee/ntl38_v3.csv")
sun <- read_csv("./Eco-KGML/data/data_raw/DataMendotaSunapee/limnol_allsources_to2013.csv")

colnames(men)
colnames(sun)
unique(sun$variable)

#Mendota met/inflow
#met_hourly.csv
#'inflow_YaharaHighway.csv','inflow_Pheasant.csv', 'inflow_Balance.csv'

#Sunapee met/inflow
#met_hourly.csv
#oneInflow_oldVersion.csv