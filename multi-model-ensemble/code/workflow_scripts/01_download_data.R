#Read in FCR data from EDI
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: Download and read in data from Falling Creek Reservoir stored in the 
#Environmental Data Initiative Repository

#Data needed:
#'1. met station (air temp, shortwave, windspeed)
#'2. inflow at weir (Q)
#'3. catwalk sensors (EXO chl-a, water temp)
#'4. water chemistry (SRP, DIN)
#'5. CTD (Kd)

options(timeout=10000)

#1. met
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/389/7/02d36541de9088f2dd99d79dc3a7a853" 
infile1 <- paste0("./multi-model-ensemble/data/data_raw/FCR_Met_final_2015_2022.csv")
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

#2: inflow
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/202/9/c065ff822e73c747f378efe47f5af12b" 
infile1 <- paste0("./multi-model-ensemble/data/data_raw/Inflow_2013_2022.csv")
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

#3. catwalk
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f" 
infile1 <- paste0("./multi-model-ensemble/data/data_raw/FCR_Catwalk_EDI_2018_2022.csv")
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

#4. chemistry
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/10/aa2ccc23688fc908f9d61cb217210a3d" 
infile1 <- paste0("./multi-model-ensemble/data/data_raw/chemistry_2013_2021.csv")
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

#5. CTD
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf" 
infile1 <- paste0("./multi-model-ensemble/data/data_raw/CTD_2013_2022.csv")
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")