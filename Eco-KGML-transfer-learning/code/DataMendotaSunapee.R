#Title: Mendota/Sunapee Data munging
#Author: Mary Lofton
#Date: last updated 27FEB23

##SEE NOTES AT BOTTOM OF DOC BEFORE YOU GO ANY FURTHER WITH THIS

#Purpose: Wrangle observed data from Sunapee and Mendota for transfer learning project
#according to template at "transfer learning project data template"
# in Eco-KGML shared drive

# Load packages
# install.packages('pacman')
pacman::p_load(tidyverse, lubridate, data.table)

# Read in data

# met & inflow
met_inf <- read_csv("./Eco-KGML-transfer-learning/data/data_processed/ModelOutputMendotaSunapee.csv") 
colnames(met_inf)
range(year(met_inf$DateTime))
met_inf2 <- met_inf %>%
  mutate(DataType = "observed",
         ModelRunType = NA,
         WaterTemp_C = NA,
         SRP_ugL = NA,
         DIN_ugL = NA,
         LightAttenuation_Kd = NA,
         Chla_ugL = NA) %>%
  filter(Lake == "Mendota" & year(DateTime) %in% c(2006:2014))
head(met_inf2)
tail(met_inf2)

# water quality
men <- read_csv("./Eco-KGML/data/data_raw/DataMendotaSunapee/ntl38_v3.csv")
sun <- read_csv("./Eco-KGML-transfer-learning/data/data_raw/DataMendotaSunapee/limnol_allsources_to2013.csv")

colnames(men)
colnames(sun)

#Let's do Sunapee first as it's more complicated (ha!)
unique(sun$variable)
unique(sun$depth.m)
unique(sun$depth.measurement)
unique(sun$sitetype)

sun_chla <- sun %>%
  filter(sitetype == "pelagic" & variable == "chla_ugL" & year(date) >= 1990)

ggplot(data = sun_chla, aes(x = date, y = value)) +
  geom_point()+
  theme_bw()

sun_tin <- sun %>%
  filter(sitetype == "pelagic" & variable == "TIN_mgL" & year(date) >= 1990)

ggplot(data = sun_tin, aes(x = date, y = value)) +
  geom_point()+
  theme_bw()

sun_po4 <- sun %>%
  filter(sitetype == "pelagic" & variable == "PO4_mgL" & year(date) >= 1990)

ggplot(data = sun_po4, aes(x = date, y = value)) +
  geom_point()+
  theme_bw()

#OK, I think this is the end of the line for Sunapee; just not enough data!

#Now Mendota

#read in and wrangle raw data

#water temp

urls <- c("https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/71831f38a4e590e6ec34d49b64b38313",
          "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/91e2fc216c289b007503b31a0b46fb64",
          "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/9739be632846ac2eaa7910278fa831b8",
          "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/28d5e476384c8808e1999460093d96dc",
          "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/3dbc4013a2e0d147bf8f098e90159c0c",
          "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/09dc2d104f729077978cb2d042019e84",
          "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/fd68691b4dd9d51f42390e4e5855ca65",
          "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/90948288600352ffed0129a9733906bb",
          "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/30/5b9493e070b212bd30f7d85495ee7c22")

filenames <- c("ntl130_2006_v1.csv",
               "ntl130_2007_v1.csv",
               "ntl130_2008_v1.csv",
               "ntl130_2009_v1.csv",
               "ntl130_2010_v1.csv",
               "ntl130_2011_v1.csv",
               "ntl130_2012_v1.csv",
               "ntl130_2013_v1.csv",
               "ntl130_2014_v1.csv")

for (i in 1:length(urls)){
inUrl1  <- urls[i]
infile1 <- paste0("./Eco-KGML-transfer-learning/data/data_raw/DataMendotaSunapee/",filenames[i])
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
}

for (i in 1:length(filenames)){
temp <- fread(paste0("./Eco-KGML-transfer-learning/data/data_raw/DataMendotaSunapee/",filenames[i]))

temp2 <- temp %>%
  filter(depth == 1.5) %>%
  group_by(sampledate) %>%
  summarize(med_temp = median(wtemp, na.rm = TRUE))

if(i == 1){daily_med <- temp2}
else{daily_med <- bind_rows(daily_med,temp2)}
  
rm(temp, temp2)

}

colnames(daily_med) <- c("DateTime","WaterTemp_C")
daily_med <- daily_med %>%
mutate(DateTime = date(DateTime))

men <- left_join(met_inf2, daily_med, by = c("DateTime"))

#other water quality variables
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/129/34/b4becc2c76260ef599b0cc96b2bb9779" 
infile1 <- paste0("./Eco-KGML-transfer-learning/data/data_raw/DataMendotaSunapee/ntl129_3_v10.csv")
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

temp <- fread(paste0("./Eco-KGML-transfer-learning/data/data_raw/DataMendotaSunapee/ntl129_3_v10.csv"))
colnames(temp)
temp2 <- temp %>%
  filter(year4 %in% c(2006:2014)) %>%
  select(sampledate,chlor_rfu,par,par_below) %>%
  group_by(sampledate) %>%
  summarize(med_chla = median(chlor_rfu, na.rm = TRUE))

ggplot(data = temp2, aes(x = sampledate, y = med_chla))+
  geom_line()+
  theme_bw()

#DON'T FORGET TO ADJUST FLAGS!

## Notes 27FEB23: 
#' Right now the resolution of these data may be too low for our purposes, as chl-a
#' is measured at most twice a month and not all year round.
#' In addition, there are some key variables missing: water temp for Sunapee and
#' SRP, DIN, Kd, water temp for Mendota.
#' Are there datasets that have the variables we need at sufficiently high resolution
#' to make it worthwhile to track down observed data from these lakes?
#' 
#' Notes 02MAR23
#' CCC pointed towards high-frequency data on EDI. High-frequency data available from
#' 2006-2019 for Mendota for water temp, chl-a and Kd, with DIN/SRP being lower 
#' frequency (max twice per month). Data packages:
#' https://doi.org/10.6073/pasta/ab6872e23055211b6b61ae4dc0a14bba
#' https://doi.org/10.6073/pasta/adbf73a2462f84635acee6fdbaaa7511
#' https://doi.org/10.6073/pasta/c923b8e044310f3f5612dab09c2cc6c2
#' However, PAR data only start in 2019, which means we have at most 3 yr
#' of high-frequency data across variables (2019-2021).
#' 
#' For Sunapee, there is high-frequency water temp data
#' https://doi.org/10.6073/pasta/8fc8e53b0ad784437b87e2dd5d9b961a
#' But not much else, so I still think this is not worth it
#' 
#' More broadly, for all of the lakes in Abhilash's project except FCR, most of the observational data will have a large gap in the winter months, and the DIN, SRP, and possibly light attenuation data will probably be ~2x/month on average, even in summer. Given this, before we go too much further into pulling the dataset together, I think it would be good to revisit the questions of:
# 
# 1. whether we are developing a "seasonal" model that predicts the summer only, or how are we handling the winter gap?
#   2. how are we handling interpolation for variables, like DIN and SRP, where our highest-frequency data is only twice per month?
#   
#   Cayelan, I've listed my specific data questions for Mendota below; I'm happy to reach out to Paul/Bennett or maybe you will get a chance to speak with Paul on Friday; just let me know what you prefer!
#   
#   Questions for Paul/Bennett re: Mendota data:
#   
#   1. For the high-frequency buoy dataset (linked here), is there an equation available that is commonly used to convert the chlorophyll RFU to ug/L? All the rest of the data Abhilash has for chl-a is in ug/L, so we would need to convert the Mendota data for him.
# 
# 2. Is there a data package of Mendota inflows covering the time period from 2015-onward? I poked around a bit and couldn't find it. I have inflow data from Kait Farrell's GLM runs for 2004-2014, but not after that.
# 
# 3. I found this data package that has grab samples of DIN and SRP ~2x/month; is this the best dataset to use for these soluble nutrients?
#   
#   4. I found this data package that has Secchi from 1981-present; is this the best data package if I'd like to develop a timeseries of Kd based on Secchi? Or, even better, is there by chance a high-frequency timeseries of surface/subsurface PAR for any years before 2019 that could be used to calculate Kd?
# 
# 5. As a quick note, the most recent version of the buoy water temperature package (here) only runs through 2019, so that's currently the end of the timeseries that I could pull together.
