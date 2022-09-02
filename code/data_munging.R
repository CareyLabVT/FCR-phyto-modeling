#Title: Data munging
#Author: Mary Lofton
#Date: last updated 01SEP22

#Purpose: Download all published data from Environmental Data Initiative (EDI) 
#and tidy it so it can be pulled in for analysis

library(tidyverse)
library(lubridate)

##Data download----

#download FP data from EDI 
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.272.6&entityid=6b3151c0fdd913e02641363c2b00ae57"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/FluoroProbe_2014_2021.csv", method='libcurl')

#download Secchi data from EDI 
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.198.10&entityid=375f87747001e1681b0e805d00cc1341"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/Secchi_depth_2013-2021.csv", method='libcurl')

#download YSI/PAR data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.198.10&entityid=b3bd353312f9e37ca392e2a5315cc9da"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/YSI_PAR_profiles_2013-2021.csv", method='libcurl')

#download filtered chl-a data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.555.2&entityid=c90ced759d8a7c994e20e7d3227a84da"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/manual_chlorophyll_2014_2021.csv", method='libcurl')

#download CTD data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.200.12&entityid=0a62d1946e8d9a511bc1404e69e59b8c"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/CTD dataset 2013-2021.csv", method='libcurl')

#download chemistry data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.199.10&entityid=aa2ccc23688fc908f9d61cb217210a3d"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/chemistry_2013_2021.csv", method='libcurl')

#download inflow data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.202.8&entityid=cc045f9fe32501138d5f4e1e7f40d492"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/Inflow_2013_2021.csv", method='libcurl')

#download met data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.389.6&entityid=a5524c686e2154ec0fd0459d46a7d1eb"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/FCR_Met_final_2015_2021.csv", method='libcurl')

#download met data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.389.6&entityid=a5524c686e2154ec0fd0459d46a7d1eb"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/FCR_Met_final_2015_2021.csv", method='libcurl')

#download EXO data from EDI
data  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.271.6&entityid=23a191c1870a5b18cbc17f2779f719cf"
destination <- "./data/raw"

download.file(data,destfile = "./data/raw/FCR_Catwalk_2018_2021.csv", method='libcurl')

##Data munging----

fp <- read_csv("./data/raw/FluoroProbe_2014_2021.csv") %>%
  filter(Reservoir == "FCR" & Site == 50)
write.csv(fp, "./data/predictors/FluoroProbe_2014_2021_FCR_50.csv")

ctd <- read_csv("./data/raw/CTD dataset 2013-2021.csv") %>%
  filter(Reservoir == "FCR" & Site == 50)
write.csv(fp, "./data/predictors/FluoroProbe_2014_2021_FCR_50.csv")
