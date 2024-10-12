
library(ncdf4)    #to read the netcdf data
library(raster)   #to extract times series data from raster
library(tidyverse)#to mutate data, apply statistics
library(dplyr)
library(ggplot2)  #to plot
library(trend)

setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/RAIN4PE//RAW")
rm(list=ls())

################################################################################
#                              LS
################################################################################

North <- read.csv("LS_raw1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFLS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCanESM2_WRFLS$start_date<- NULL
NUtahCanESM2_WRFLS$end_date <- NULL


North <- read.csv("LS_raw2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCSIRO_Mk3LS$start_date<- NULL
NUtahCSIRO_Mk3LS$end_date <- NULL


North <- read.csv("LS_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahEC_EARTH_RCA4_v3LS$start_date<- NULL
NUtahEC_EARTH_RCA4_v3LS$end_date <- NULL

North <- read.csv("LS_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahIPSL_RCA4_v3LS$start_date<- NULL
NUtahIPSL_RCA4_v3LS$end_date <- NULL


North <- read.csv("LS_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahMIROC5_RCA4_v3LS$start_date<- NULL
NUtahMIROC5_RCA4_v3LS$end_date <- NULL


North <- read.csv("LS_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahHadGEM2_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahHadGEM2_RCA4_v3LS$start_date<- NULL
NUtahHadGEM2_RCA4_v3LS$end_date <- NULL

North <- read.csv("LS_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3LS$start_date<- NULL
NUtahESM2M_RCA4_v3LS$end_date <- NULL


North <- read.csv("LS_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorESM1_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorESM1_RCA4_v3LS$start_date<- NULL
NUtahNorESM1_RCA4_v3LS$end_date <- NULL

North <- read.csv("LS_raw9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorCanESM2_SAM20LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorCanESM2_SAM20LS$start_date<- NULL
NUtahNorCanESM2_SAM20LS$end_date <- NULL

North <- read.csv("LS_raw10.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorMIROC5_SAM20LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorMIROC5_SAM20LS$start_date<- NULL
NUtahNorMIROC5_SAM20LS$end_date <- NULL


################################################################################
#                              PT
################################################################################

North <- read.csv("PT_raw1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFPT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCanESM2_WRFPT$start_date<- NULL
NUtahCanESM2_WRFPT$end_date <- NULL


North <- read.csv("PT_raw2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCSIRO_Mk3PT$start_date<- NULL
NUtahCSIRO_Mk3PT$end_date <- NULL


North <- read.csv("PT_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahEC_EARTH_RCA4_v3PT$start_date<- NULL
NUtahEC_EARTH_RCA4_v3PT$end_date <- NULL

North <- read.csv("PT_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahIPSL_RCA4_v3PT$start_date<- NULL
NUtahIPSL_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahMIROC5_RCA4_v3PT$start_date<- NULL
NUtahMIROC5_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

HadGEM2_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

HadGEM2_RCA4_v3PT$start_date<- NULL
HadGEM2_RCA4_v3PT$end_date <- NULL

North <- read.csv("PT_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3PT$start_date<- NULL
NUtahESM2M_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorESM1_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorESM1_RCA4_v3PT$start_date<- NULL
NUtahNorESM1_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorCanESM2_SAM20PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorCanESM2_SAM20PT$start_date<- NULL
NUtahNorCanESM2_SAM20PT$end_date <- NULL

North <- read.csv("PT_raw10.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorMIROC5_SAM20PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorMIROC5_SAM20PT$start_date<- NULL
NUtahNorMIROC5_SAM20PT$end_date <- NULL

################################################################################
#                              DM
################################################################################

North <- read.csv("DM_raw1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCanESM2_WRFDM$start_date<- NULL
NUtahCanESM2_WRFDM$end_date <- NULL


North <- read.csv("DM_raw2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCSIRO_Mk3DM$start_date<- NULL
NUtahCSIRO_Mk3DM$end_date <- NULL


North <- read.csv("DM_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahEC_EARTH_RCA4_v3DM$start_date<- NULL
NUtahEC_EARTH_RCA4_v3DM$end_date <- NULL

North <- read.csv("DM_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahIPSL_RCA4_v3DM$start_date<- NULL
NUtahIPSL_RCA4_v3DM$end_date <- NULL


North <- read.csv("DM_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahMIROC5_RCA4_v3DM$start_date<- NULL
NUtahMIROC5_RCA4_v3DM$end_date <- NULL


North <- read.csv("DM_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

HadGEM2_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

HadGEM2_RCA4_v3DM$start_date<- NULL
HadGEM2_RCA4_v3DM$end_date <- NULL

North <- read.csv("DM_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3DM$start_date<- NULL
NUtahESM2M_RCA4_v3DM$end_date <- NULL

North <- read.csv("DM_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4DM$start_date<- NULL
NUtahESM2M_RCA4DM$end_date <- NULL

################################################################################
#                                BC
################################################################################
setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/RAIN4PE//BC/")

################################################################################
#                              LS
################################################################################

North <- read.csv("LS_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS1$start_date<- NULL
LS1$end_date <- NULL


North <- read.csv("LS_2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS2$start_date<- NULL
LS2$end_date <- NULL


North <- read.csv("LS_3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS3$start_date<- NULL
LS3$end_date <- NULL

North <- read.csv("LS_4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS4$start_date<- NULL
LS4$end_date <- NULL


North <- read.csv("LS_5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS5$start_date<- NULL
LS5$end_date <- NULL


North <- read.csv("LS_6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS6$start_date<- NULL
LS6$end_date <- NULL

North <- read.csv("LS_7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS7$start_date<- NULL
LS7$end_date <- NULL


North <- read.csv("LS_8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS8$start_date<- NULL
LS8$end_date <- NULL

North <- read.csv("LS_9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS9<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS9$start_date<- NULL
LS9$end_date <- NULL

North <- read.csv("LS_10.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS10<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS10$start_date<- NULL
LS10$end_date <- NULL


################################################################################
#                              PT
################################################################################

North <- read.csv("PT_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT1$start_date<- NULL
PT1$end_date <- NULL


North <- read.csv("PT_2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT2$start_date<- NULL
PT2$end_date <- NULL


North <- read.csv("PT_3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT3$start_date<- NULL
PT3$end_date <- NULL

North <- read.csv("PT_4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT4$start_date<- NULL
PT4$end_date <- NULL


North <- read.csv("PT_5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT5$start_date<- NULL
PT5$end_date <- NULL


North <- read.csv("PT_6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT6$start_date<- NULL
PT6$end_date <- NULL

North <- read.csv("PT_7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT7$start_date<- NULL
PT7$end_date <- NULL


North <- read.csv("PT_8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT8$start_date<- NULL
PT8$end_date <- NULL


North <- read.csv("PT_9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT9<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT9$start_date<- NULL
PT9$end_date <- NULL

North <- read.csv("PT_10.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT10<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT10$start_date<- NULL
PT10$end_date <- NULL

################################################################################
#                              DM
################################################################################

North <- read.csv("DM_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM1$start_date<- NULL
DM1$end_date <- NULL


North <- read.csv("DM_2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM2$start_date<- NULL
DM2$end_date <- NULL


North <- read.csv("DM_3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM3$start_date<- NULL
DM3$end_date <- NULL

North <- read.csv("DM_4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM4$start_date<- NULL
DM4$end_date <- NULL


North <- read.csv("DM_5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM5$start_date<- NULL
DM5$end_date <- NULL


North <- read.csv("DM_6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM6$start_date<- NULL
DM6$end_date <- NULL

North <- read.csv("DM_7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM7$start_date<- NULL
DM7$end_date <- NULL

North <- read.csv("DM_8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM8$start_date<- NULL
DM8$end_date <- NULL


################################################################################
#                              RAIN4PE-PISCO
################################################################################

North <- read.csv("RAIN4PE.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahRAIN4PE_PIS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-04-01') | same_year >=as.Date('1981-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahRAIN4PE_PIS$start_date<- NULL
NUtahRAIN4PE_PIS$end_date <- NULL

################################################################################
NUtahRAIN4PE_PIS$RCM <- c("OBS") #NUtahRAIN4PE_PIS$RCM <- c("RAIN4PE")
mediana_rain4pe <- median(NUtahRAIN4PE_PIS$Precip, na.rm = TRUE)


LS1$RCM <- c("CCCma-CanESM2")
LS2$RCM <- c("CSIRO-QCCCE-Mk3")
LS3$RCM <- c("ICHEC-EC-EARTH")
LS4$RCM <- c("IPSL-CM5A-MR")
LS5$RCM <- c("MIROC-MIROC5")
LS6$RCM <- c("MOHC-HadGEM2-ES")
LS7$RCM <- c("NCC-NorESM1-M")
LS8$RCM <- c("NOAA-GFDL-ESM2M")
LS9$RCM <- c("CCCma-CanESM2-SAM20")
LS10$RCM <- c("MIROC-MIROC5-SAM20")

PT1$RCM <- c("CCCma-CanESM2")
PT2$RCM <- c("CSIRO-QCCCE-Mk3")
PT3$RCM <- c("ICHEC-EC-EARTH")
PT4$RCM <- c("IPSL-CM5A-MR")
PT5$RCM <- c("MIROC-MIROC5")
PT6$RCM <- c("MOHC-HadGEM2-ES")
PT7$RCM <- c("NCC-NorESM1-M")
PT8$RCM <- c("NOAA-GFDL-ESM2M")
PT9$RCM <- c("CCCma-CanESM2-SAM20")
PT10$RCM <- c("MIROC-MIROC5-SAM20")

DM1$RCM <- c("CCCma-CanESM2")
DM2$RCM <- c("CSIRO-QCCCE-Mk3")
DM3$RCM <- c("ICHEC-EC-EARTH")
DM4$RCM <- c("IPSL-CM5A-MR")
DM5$RCM <- c("MIROC-MIROC5")
DM6$RCM <- c("MOHC-HadGEM2-ES")
DM7$RCM <- c("NCC-NorESM1-M")
DM8$RCM <- c("NOAA-GFDL-ESM2M")

LS <- rbind(LS1, LS2,LS3,
            LS4,LS5, 
            LS6,LS7,LS8,
            LS9,LS10,NUtahRAIN4PE_PIS)
LS$BC <- "LS"
PT <- rbind(PT1, PT2,PT3,
            PT4,PT5,
            PT6,PT7,PT8,
            PT9,PT10,NUtahRAIN4PE_PIS)
PT$BC <- "PT"
DM <- rbind(DM1, DM2,DM3,
            DM4,DM5, 
            DM6,DM7,DM8, NUtahRAIN4PE_PIS)

DM$BC <- "DM"


NUtahCanESM2_WRFLS$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3LS$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3LS$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3LS$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3LS$RCM <- c("MIROC-MIROC5")
NUtahHadGEM2_RCA4_v3LS$RCM <- c("MOHC-HadGEM2-ES")
NUtahESM2M_RCA4_v3LS$RCM <- c("NCC-NorESM1-M")
NUtahNorESM1_RCA4_v3LS$RCM <- c("NOAA-GFDL-ESM2M")
NUtahNorCanESM2_SAM20LS$RCM <- c("CCCma-CanESM2-SAM20")
NUtahNorMIROC5_SAM20LS$RCM <- c("MIROC-MIROC5-SAM20")

NUtahCanESM2_WRFPT$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3PT$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3PT$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3PT$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3PT$RCM <- c("MIROC-MIROC5")
HadGEM2_RCA4_v3PT$RCM <- c("MOHC-HadGEM2-ES")
NUtahESM2M_RCA4_v3PT$RCM <- c("NCC-NorESM1-M")
NUtahNorESM1_RCA4_v3PT$RCM <- c("NOAA-GFDL-ESM2M")
NUtahNorCanESM2_SAM20PT$RCM <- c("CCCma-CanESM2-SAM20")
NUtahNorMIROC5_SAM20PT$RCM <- c("MIROC-MIROC5-SAM20")

NUtahCanESM2_WRFDM$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3DM$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3DM$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3DM$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3DM$RCM <- c("MIROC-MIROC5")
HadGEM2_RCA4_v3DM$RCM <- c("MOHC-HadGEM2-ES")
NUtahESM2M_RCA4_v3DM$RCM <- c("NCC-NorESM1-M")
NUtahESM2M_RCA4DM$RCM <- c("NOAA-GFDL-ESM2M")


LS_raw <- rbind(NUtahCanESM2_WRFLS, NUtahCSIRO_Mk3LS,NUtahEC_EARTH_RCA4_v3LS,
                NUtahIPSL_RCA4_v3LS,NUtahMIROC5_RCA4_v3LS, 
                NUtahHadGEM2_RCA4_v3LS,NUtahESM2M_RCA4_v3LS,NUtahNorESM1_RCA4_v3LS,
                NUtahNorCanESM2_SAM20LS,NUtahNorMIROC5_SAM20LS,NUtahRAIN4PE_PIS)
LS_raw$BC <- "LS_raw"
PT_raw <- rbind(NUtahCanESM2_WRFPT, NUtahCSIRO_Mk3PT,NUtahEC_EARTH_RCA4_v3PT,
                NUtahIPSL_RCA4_v3PT,NUtahMIROC5_RCA4_v3PT,
                HadGEM2_RCA4_v3PT,NUtahESM2M_RCA4_v3PT,NUtahNorESM1_RCA4_v3PT,
                NUtahNorCanESM2_SAM20PT,NUtahNorMIROC5_SAM20PT,NUtahRAIN4PE_PIS)
PT_raw$BC <- "PT_raw"
DM_raw <- rbind(NUtahCanESM2_WRFDM, NUtahCSIRO_Mk3DM,NUtahEC_EARTH_RCA4_v3DM,
                NUtahIPSL_RCA4_v3DM,NUtahMIROC5_RCA4_v3DM, 
                HadGEM2_RCA4_v3DM,NUtahESM2M_RCA4_v3DM,NUtahESM2M_RCA4DM, NUtahRAIN4PE_PIS)

DM_raw$BC <- "DM_raw"
#all_precip <- rbind(LS_raw, PT_raw, DM_raw, LS, PT, DM)
all_precip_invierno <- rbind(LS, PT, DM, LS_raw, PT_raw, DM_raw)

all_precip_invierno $BC <- factor(all_precip_invierno $BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT"))

# Filter out year 2100 since this would represent the 2100-2101 ski season, and we don't have 2101 data (so it's not accurate)
Combined_precip_invierno <- all_precip_invierno  %>% dplyr::filter(run != 2005) %>% dplyr::filter(run > 1980)

library(ggplot2)

boxplot_fig <- ggplot(Combined_precip_invierno, aes(x = run, y = Precip, fill = RCM)) +
  geom_boxplot() + 
  scale_fill_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                               "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), Dec - March", x = " ") +
  xlim(1981,2004)+ theme_bw()+
  #geom_boxplot(width = 0.8)+
  facet_wrap(~ BC, ncol=2, nrow = 3)+ 
  geom_hline(yintercept = 3.448689, linetype = "dashed", color = "blue")# verano  2.396 mediana

boxplot_fig <- boxplot_fig +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "bottom", title.hjust = 0.5,title = " ")) +
  theme(
    legend.position = "bottom",  # Coloca la leyenda en la parte inferior
    legend.direction = "horizontal",
    text = element_text(size = 7)
  )

boxplot_fig

################################################################################
#                              verano
################################################################################


library(ncdf4)    #to read the netcdf data
library(raster)   #to extract times series data from raster
library(tidyverse)#to mutate data, apply statistics
library(dplyr)
library(ggplot2)  #to plot
library(trend)

setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/RAIN4PE//RAW")
#rm(list=ls())

################################################################################
#                              LS
################################################################################

North <- read.csv("LS_raw1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFLS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCanESM2_WRFLS$start_date<- NULL
NUtahCanESM2_WRFLS$end_date <- NULL


North <- read.csv("LS_raw2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCSIRO_Mk3LS$start_date<- NULL
NUtahCSIRO_Mk3LS$end_date <- NULL


North <- read.csv("LS_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahEC_EARTH_RCA4_v3LS$start_date<- NULL
NUtahEC_EARTH_RCA4_v3LS$end_date <- NULL

North <- read.csv("LS_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahIPSL_RCA4_v3LS$start_date<- NULL
NUtahIPSL_RCA4_v3LS$end_date <- NULL


North <- read.csv("LS_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahMIROC5_RCA4_v3LS$start_date<- NULL
NUtahMIROC5_RCA4_v3LS$end_date <- NULL


North <- read.csv("LS_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahHadGEM2_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahHadGEM2_RCA4_v3LS$start_date<- NULL
NUtahHadGEM2_RCA4_v3LS$end_date <- NULL

North <- read.csv("LS_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3LS$start_date<- NULL
NUtahESM2M_RCA4_v3LS$end_date <- NULL


North <- read.csv("LS_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorESM1_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorESM1_RCA4_v3LS$start_date<- NULL
NUtahNorESM1_RCA4_v3LS$end_date <- NULL

North <- read.csv("LS_raw9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorCanESM2_SAM20LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorCanESM2_SAM20LS$start_date<- NULL
NUtahNorCanESM2_SAM20LS$end_date <- NULL

North <- read.csv("LS_raw10.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorMIROC5_SAM20LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorMIROC5_SAM20LS$start_date<- NULL
NUtahNorMIROC5_SAM20LS$end_date <- NULL


################################################################################
#                              PT
################################################################################

North <- read.csv("PT_raw1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFPT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCanESM2_WRFPT$start_date<- NULL
NUtahCanESM2_WRFPT$end_date <- NULL


North <- read.csv("PT_raw2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCSIRO_Mk3PT$start_date<- NULL
NUtahCSIRO_Mk3PT$end_date <- NULL


North <- read.csv("PT_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahEC_EARTH_RCA4_v3PT$start_date<- NULL
NUtahEC_EARTH_RCA4_v3PT$end_date <- NULL

North <- read.csv("PT_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahIPSL_RCA4_v3PT$start_date<- NULL
NUtahIPSL_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahMIROC5_RCA4_v3PT$start_date<- NULL
NUtahMIROC5_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

HadGEM2_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

HadGEM2_RCA4_v3PT$start_date<- NULL
HadGEM2_RCA4_v3PT$end_date <- NULL

North <- read.csv("PT_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3PT$start_date<- NULL
NUtahESM2M_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorESM1_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorESM1_RCA4_v3PT$start_date<- NULL
NUtahNorESM1_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorCanESM2_SAM20PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorCanESM2_SAM20PT$start_date<- NULL
NUtahNorCanESM2_SAM20PT$end_date <- NULL

North <- read.csv("PT_raw10.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorMIROC5_SAM20PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorMIROC5_SAM20PT$start_date<- NULL
NUtahNorMIROC5_SAM20PT$end_date <- NULL

################################################################################
#                              DM
################################################################################

North <- read.csv("DM_raw1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCanESM2_WRFDM$start_date<- NULL
NUtahCanESM2_WRFDM$end_date <- NULL


North <- read.csv("DM_raw2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCSIRO_Mk3DM$start_date<- NULL
NUtahCSIRO_Mk3DM$end_date <- NULL


North <- read.csv("DM_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahEC_EARTH_RCA4_v3DM$start_date<- NULL
NUtahEC_EARTH_RCA4_v3DM$end_date <- NULL

North <- read.csv("DM_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahIPSL_RCA4_v3DM$start_date<- NULL
NUtahIPSL_RCA4_v3DM$end_date <- NULL


North <- read.csv("DM_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahMIROC5_RCA4_v3DM$start_date<- NULL
NUtahMIROC5_RCA4_v3DM$end_date <- NULL


North <- read.csv("DM_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

HadGEM2_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

HadGEM2_RCA4_v3DM$start_date<- NULL
HadGEM2_RCA4_v3DM$end_date <- NULL

North <- read.csv("DM_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3DM$start_date<- NULL
NUtahESM2M_RCA4_v3DM$end_date <- NULL

North <- read.csv("DM_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4DM$start_date<- NULL
NUtahESM2M_RCA4DM$end_date <- NULL

################################################################################
#                                BC
################################################################################
setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/RAIN4PE//BC/")

################################################################################
#                              LS
################################################################################

North <- read.csv("LS_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS1$start_date<- NULL
LS1$end_date <- NULL


North <- read.csv("LS_2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS2$start_date<- NULL
LS2$end_date <- NULL


North <- read.csv("LS_3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS3$start_date<- NULL
LS3$end_date <- NULL

North <- read.csv("LS_4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS4$start_date<- NULL
LS4$end_date <- NULL


North <- read.csv("LS_5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS5$start_date<- NULL
LS5$end_date <- NULL


North <- read.csv("LS_6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS6$start_date<- NULL
LS6$end_date <- NULL

North <- read.csv("LS_7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS7$start_date<- NULL
LS7$end_date <- NULL


North <- read.csv("LS_8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS8$start_date<- NULL
LS8$end_date <- NULL

North <- read.csv("LS_9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS9<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS9$start_date<- NULL
LS9$end_date <- NULL

North <- read.csv("LS_10.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

LS10<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS10$start_date<- NULL
LS10$end_date <- NULL


################################################################################
#                              PT
################################################################################

North <- read.csv("PT_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT1$start_date<- NULL
PT1$end_date <- NULL


North <- read.csv("PT_2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT2$start_date<- NULL
PT2$end_date <- NULL


North <- read.csv("PT_3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT3$start_date<- NULL
PT3$end_date <- NULL

North <- read.csv("PT_4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT4$start_date<- NULL
PT4$end_date <- NULL


North <- read.csv("PT_5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT5$start_date<- NULL
PT5$end_date <- NULL


North <- read.csv("PT_6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT6$start_date<- NULL
PT6$end_date <- NULL

North <- read.csv("PT_7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT7$start_date<- NULL
PT7$end_date <- NULL


North <- read.csv("PT_8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT8$start_date<- NULL
PT8$end_date <- NULL


North <- read.csv("PT_9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT9<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT9$start_date<- NULL
PT9$end_date <- NULL

North <- read.csv("PT_10.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

PT10<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT10$start_date<- NULL
PT10$end_date <- NULL

################################################################################
#                              DM
################################################################################

North <- read.csv("DM_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM1$start_date<- NULL
DM1$end_date <- NULL


North <- read.csv("DM_2.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM2$start_date<- NULL
DM2$end_date <- NULL


North <- read.csv("DM_3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM3$start_date<- NULL
DM3$end_date <- NULL

North <- read.csv("DM_4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM4$start_date<- NULL
DM4$end_date <- NULL


North <- read.csv("DM_5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM5$start_date<- NULL
DM5$end_date <- NULL


North <- read.csv("DM_6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM6$start_date<- NULL
DM6$end_date <- NULL

North <- read.csv("DM_7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM7$start_date<- NULL
DM7$end_date <- NULL

North <- read.csv("DM_8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

DM8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM8$start_date<- NULL
DM8$end_date <- NULL


################################################################################
#                              RAIN4PE-PISCO
################################################################################

North <- read.csv("RAIN4PE.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('1981-01-01'), as.Date('2004-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahRAIN4PE_PIS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '1981-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('1981-06-01') | same_year >=as.Date('1981-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahRAIN4PE_PIS$start_date<- NULL
NUtahRAIN4PE_PIS$end_date <- NULL

################################################################################
NUtahRAIN4PE_PIS$RCM <- c("OBS")
mediana_rain4pe <- median(NUtahRAIN4PE_PIS$Precip, na.rm = TRUE)


LS1$RCM <- c("CCCma-CanESM2")
LS2$RCM <- c("CSIRO-QCCCE-Mk3")
LS3$RCM <- c("ICHEC-EC-EARTH")
LS4$RCM <- c("IPSL-CM5A-MR")
LS5$RCM <- c("MIROC-MIROC5")
LS6$RCM <- c("MOHC-HadGEM2-ES")
LS7$RCM <- c("NCC-NorESM1-M")
LS8$RCM <- c("NOAA-GFDL-ESM2M")
LS9$RCM <- c("CCCma-CanESM2-SAM20")
LS10$RCM <- c("MIROC-MIROC5-SAM20")

PT1$RCM <- c("CCCma-CanESM2")
PT2$RCM <- c("CSIRO-QCCCE-Mk3")
PT3$RCM <- c("ICHEC-EC-EARTH")
PT4$RCM <- c("IPSL-CM5A-MR")
PT5$RCM <- c("MIROC-MIROC5")
PT6$RCM <- c("MOHC-HadGEM2-ES")
PT7$RCM <- c("NCC-NorESM1-M")
PT8$RCM <- c("NOAA-GFDL-ESM2M")
PT9$RCM <- c("CCCma-CanESM2-SAM20")
PT10$RCM <- c("MIROC-MIROC5-SAM20")

DM1$RCM <- c("CCCma-CanESM2")
DM2$RCM <- c("CSIRO-QCCCE-Mk3")
DM3$RCM <- c("ICHEC-EC-EARTH")
DM4$RCM <- c("IPSL-CM5A-MR")
DM5$RCM <- c("MIROC-MIROC5")
DM6$RCM <- c("MOHC-HadGEM2-ES")
DM7$RCM <- c("NCC-NorESM1-M")
DM8$RCM <- c("NOAA-GFDL-ESM2M")

LS <- rbind(LS1, LS2,LS3,
            LS4,LS5, 
            LS6,LS7,LS8,
            LS9,LS10,NUtahRAIN4PE_PIS)
LS$BC <- "LS"
PT <- rbind(PT1, PT2,PT3,
            PT4,PT5,
            PT6,PT7,PT8,
            PT9,PT10,NUtahRAIN4PE_PIS)
PT$BC <- "PT"
DM <- rbind(DM1, DM2,DM3,
            DM4,DM5, 
            DM6,DM7,DM8, NUtahRAIN4PE_PIS)

DM$BC <- "DM"


NUtahCanESM2_WRFLS$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3LS$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3LS$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3LS$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3LS$RCM <- c("MIROC-MIROC5")
NUtahHadGEM2_RCA4_v3LS$RCM <- c("MOHC-HadGEM2-ES")
NUtahESM2M_RCA4_v3LS$RCM <- c("NCC-NorESM1-M")
NUtahNorESM1_RCA4_v3LS$RCM <- c("NOAA-GFDL-ESM2M")
NUtahNorCanESM2_SAM20LS$RCM <- c("CCCma-CanESM2-SAM20")
NUtahNorMIROC5_SAM20LS$RCM <- c("MIROC-MIROC5-SAM20")

NUtahCanESM2_WRFPT$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3PT$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3PT$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3PT$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3PT$RCM <- c("MIROC-MIROC5")
HadGEM2_RCA4_v3PT$RCM <- c("MOHC-HadGEM2-ES")
NUtahESM2M_RCA4_v3PT$RCM <- c("NCC-NorESM1-M")
NUtahNorESM1_RCA4_v3PT$RCM <- c("NOAA-GFDL-ESM2M")
NUtahNorCanESM2_SAM20PT$RCM <- c("CCCma-CanESM2-SAM20")
NUtahNorMIROC5_SAM20PT$RCM <- c("MIROC-MIROC5-SAM20")

NUtahCanESM2_WRFDM$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3DM$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3DM$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3DM$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3DM$RCM <- c("MIROC-MIROC5")
HadGEM2_RCA4_v3DM$RCM <- c("MOHC-HadGEM2-ES")
NUtahESM2M_RCA4_v3DM$RCM <- c("NCC-NorESM1-M")
NUtahESM2M_RCA4DM$RCM <- c("NOAA-GFDL-ESM2M")


LS_raw <- rbind(NUtahCanESM2_WRFLS, NUtahCSIRO_Mk3LS,NUtahEC_EARTH_RCA4_v3LS,
                NUtahIPSL_RCA4_v3LS,NUtahMIROC5_RCA4_v3LS, 
                NUtahHadGEM2_RCA4_v3LS,NUtahESM2M_RCA4_v3LS,NUtahNorESM1_RCA4_v3LS,
                NUtahNorCanESM2_SAM20LS,NUtahNorMIROC5_SAM20LS,NUtahRAIN4PE_PIS)
LS_raw$BC <- "LS_raw"
PT_raw <- rbind(NUtahCanESM2_WRFPT, NUtahCSIRO_Mk3PT,NUtahEC_EARTH_RCA4_v3PT,
                NUtahIPSL_RCA4_v3PT,NUtahMIROC5_RCA4_v3PT,
                HadGEM2_RCA4_v3PT,NUtahESM2M_RCA4_v3PT,NUtahNorESM1_RCA4_v3PT,
                NUtahNorCanESM2_SAM20PT,NUtahNorMIROC5_SAM20PT,NUtahRAIN4PE_PIS)
PT_raw$BC <- "PT_raw"
DM_raw <- rbind(NUtahCanESM2_WRFDM, NUtahCSIRO_Mk3DM,NUtahEC_EARTH_RCA4_v3DM,
                NUtahIPSL_RCA4_v3DM,NUtahMIROC5_RCA4_v3DM, 
                HadGEM2_RCA4_v3DM,NUtahESM2M_RCA4_v3DM,NUtahESM2M_RCA4DM, NUtahRAIN4PE_PIS)

DM_raw$BC <- "DM_raw"
#all_precip <- rbind(LS_raw, PT_raw, DM_raw, LS, PT, DM)
all_precip_verano <- rbind(LS, PT, DM, LS_raw, PT_raw, DM_raw)

all_precip_verano$BC <- factor(all_precip_verano$BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT"))

# Filter out year 2100 since this would represent the 2100-2101 ski season, and we don't have 2101 data (so it's not accurate)
Combined_precip_verano <- all_precip_verano %>% dplyr::filter(run != 2005) %>% dplyr::filter(run > 1980)

# Figure
precip_fig <- ggplot(all_precip,
                     aes(x=run,y=Precip, color=RCM)) + 
  scale_color_manual(values = c('red',"#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                                "#0072B2", "#D55E00", "#CC79A7", "#83db5a", "#CC79A7", "black"))+
  geom_line(alpha=1.5, lwd = 0.6)+
  #geom_smooth(method = "lm", lwd = 0.6)+
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = "Year") +
  xlim(1981,2004)+ theme_bw()+
  facet_wrap(~ BC, ncol=1) # if removed, x t

precip_fig


library(ggplot2)

boxplot_fig <- ggplot(all_precip, aes(x = run, y = Precip, fill = RCM)) +
  geom_boxplot() + 
  scale_fill_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                               "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Summer Precipitation (mm), June - Sept", x = " ") +
  xlim(1981,2004)+ theme_bw()+
  #geom_boxplot(width = 0.8)+
  facet_wrap(~ BC, ncol=2, nrow = 3)+ 
  geom_hline(yintercept = 2.396, linetype = "dashed", color = "blue")

boxplot_fig <- boxplot_fig +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "bottom", title.hjust = 0.5,title = " ")) +
  theme(
    legend.position = "bottom",  # Coloca la leyenda en la parte inferior
    legend.direction = "horizontal",
    text = element_text(size = 7)
  )

boxplot_fig



line_fig <- ggplot(all_precip, aes(x = run, y = Precip, color = RCM)) +
  geom_line(stat = "summary", fun.y = "median", size = .7) +  # Cambiado a geom_line() con la mediana como línea
  scale_color_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                                "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Summer Precipitation (mm), June - Sept", x = " ") +
  xlim(1981, 2004) + theme_bw() +
  facet_wrap(~ BC, ncol = 2, nrow = 3) + 
  geom_hline(yintercept = 2.396, linetype = "dashed", color = "blue")


point_fig <- ggplot(all_precip, aes(x = run, y = Precip, color = RCM)) +
  geom_point(size = 1, alpha = 0.7, shape = 2) +  # Cambiado a geom_point()
  scale_color_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                                "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = " ") +
  xlim(1981, 2004) + theme_bw() +
  facet_wrap(~ BC, ncol = 2, nrow = 3) +  # Puntos
  geom_smooth(method = "lm", se = FALSE, size = .7, linetype = "solid") + 
  #geom_hline(yintercept = 2.396, linetype = "dashed", color = "blue")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "bottom", title.hjust = 0.5,title = " ")) +
  theme(
    legend.position = "bottom",  # Coloca la leyenda en la parte inferior
    legend.direction = "horizontal",
    text = element_text(size = 7)
  )


point_fig


###################################################################################
###############  ok plot
##################################################################################
combined_data <- rbind(
  transform(all_precip_invierno, season = "DJF"),
  transform(Combined_precip_verano, season = "JJA")
) 


boxplot_fig <- ggplot(combined_data, aes(x = run, y = Precip, fill = RCM, group = interaction(RCM, season))) +
  geom_boxplot(position = position_dodge2(width = 0.75, preserve = "single"), width = 0.7) +
  scale_fill_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                               "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Precipitation (mm)", x = "") +
  #xlim(1981, 2004) + 
  theme_bw() +
  facet_wrap(~ BC, ncol = 2, nrow = 3) + 
  geom_hline(yintercept = 3.448689, linetype = "dotdash", color = "blue") +
  geom_hline(yintercept = 2.396355, linetype = "dotdash", color = "red") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "bottom", title.hjust = 0.5, title = " ")) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = element_text(size = 7.5)
  ) +
  scale_x_continuous(limits = c(1981, 2004), breaks = c(1981, 2004), labels = c("1981","2004"))+
  
  
  annotate("rect", xmin = 1981, xmax = 1992.5, ymin = 0, ymax = 11.7,
           alpha = .2, fill = "cyan")+
  annotate("text", x = 1981.9, y = 11, label = "DJF", size = 3.7, color = "orange", family = "serif")+
  
  annotate("rect", xmin = 1992.5, xmax = 2004, ymin = 0, ymax = 11.7,
           alpha = .2, fill = "brown")+
  annotate("text", x = 1993.3, y = 11, label = "JJA", size = 3.7, color = "blue", family = "serif")

boxplot_fig




boxplot_fig <- ggplot(combined_data, aes(x = run, y = Precip, fill = RCM, group = interaction(RCM, season))) +
  geom_boxplot(position = position_dodge2(width = 0.75, preserve = "single"), width = 0.7) +
  scale_fill_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                               "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Precipitation (mm)", x = "") +
  theme_bw() +
  facet_wrap(~ BC, ncol = 2, nrow = 3) + 
  geom_hline(yintercept = 3.448689, linetype = "dotdash", color = "blue") +
  geom_hline(yintercept = 2.396355, linetype = "dotdash", color = "red") +
  
  # Ajuste de leyenda
  guides(fill = guide_legend(
    nrow = 3, 
    byrow = TRUE, 
    title.position = "bottom", 
    title.hjust = 0.5, 
    title = " ", 
    keywidth = 0.7, keyheight = 0.7  # Ajuste del tamaño de las cajas de color en la leyenda
  )) +
  
  # Control del tamaño de la leyenda y del gráfico
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 7),  # Control del tamaño del texto de la leyenda
    legend.title = element_text(size = 8, face = "bold"),  # Control del tamaño y estilo del título de la leyenda
    legend.spacing.x = unit(0.5, 'cm'),  # Espaciado entre los elementos de la leyenda
    legend.key.size = unit(0.5, 'cm'),  # Ajuste del tamaño de los símbolos de la leyenda
    text = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  
  scale_x_continuous(limits = c(1981, 2004), breaks = c(1981, 2004), labels = c("1981", "2004")) +
  
  annotate("rect", xmin = 1981, xmax = 1992.5, ymin = 0, ymax = 11.7,
           alpha = .03, fill = "cyan") +
  annotate("text", x = 1987, y = 10, label = "DJF", size = 4, color = "orange", family = "serif", fontface = "bold") +
  
  annotate("rect", xmin = 1992.5, xmax = 2004, ymin = 0, ymax = 11.7,
           alpha = .03, fill = "brown") +
  annotate("text", x = 1998, y = 10, label = "JJA", size = 4, color = "blue", family = "serif", fontface = "bold")
# Centrar y aumentar el texto



# Guardar la figura co

























# Muestra el gráfico
print(boxplot_fig)


library(ggplot2)
library(RColorBrewer)

# Define paletas de colores
palette1 <- brewer.pal(5, "Blues")
palette2 <- brewer.pal(6, "Reds")
palette3 <- brewer.pal(8, "Greens")
palette4 <- brewer.pal(4, "Purples")

# Combina las paletas
combined_palette <- c(palette1, palette2, palette3, palette4)

# Asegúrate de tener la cantidad deseada de colores únicos
total_colors <- 22
combined_palette <- unique(rep(combined_palette, length.out = total_colors))

# Crea el gráfico de cajas
boxplot_fig <- ggplot(combined_data, aes(x = factor(BC), y = Precip, fill = interaction(RCM, season))) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7, color = "darkgray", outlier.colour = "darkgray") +
  geom_hline(yintercept = 3.448689, linetype = "twodash", color = "blue", size = 1) +
  geom_hline(yintercept = 2.396355, linetype = "dotdash", color = "red", size = 1) +
  scale_fill_manual(values = combined_palette, name = "RCM & Season") +
  labs(
    y = "Projection for Avg. Daily Precipitation (mm)",
    x = "",
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.title = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 6),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title.y = element_text(size = 8.5, face = "bold"),  # Tamaño del título del eje y
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  guides(fill = guide_legend(ncol = 6.3, title = "RCM & Season"))+
  annotate("text", x = 0.95, y = 11.45, label = "H - Historical (1981 - 2004)", size = 3.7, color = "#0072B2", family = "serif")

# Muestra el gráfico
print(boxplot_fig)

# Define colores únicos para cada combinación de RCM y season
# Define colores únicos para cada combinación de RCM y season
colores <- c(
  'red', '#E69F00', 'blue', 'green', '#F0E442', '#0072B2', '#D55E00', '#5acadb', '#83db5a', '#CC79A7', 'black'
)

# Asigna colores y nombres a las combinaciones únicas
colores_combinaciones <- rep(colores, length.out = length(unique(interaction(combined_data$RCM, combined_data$season))))

# Crea el gráfico de cajas
boxplot_fig <- ggplot(combined_data, aes(x = factor(BC), y = Precip, fill = interaction(RCM, season))) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = colores_combinaciones, name = "RCM & Season") +
  labs(y = "Proyección de precipitación diaria promedio (mm)", x = "BC") +
  theme_bw() +
  geom_hline(yintercept = 5.94623, linetype = "dashed", color = "blue") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = element_text(size = 7)
  )

# Muestra el gráfico
print(boxplot_fig)


# Define colores únicos para cada combinación de RCM y season
colores <- c(
  'red', '#E69F00', 'blue', 'green', '#F0E442', '#0072B2', '#D55E00', '#5acadb', '#83db5a', '#CC79A7', 'black'
)

# Asigna colores y nombres a las combinaciones únicas
colores_combinaciones <- rep(colores, length.out = length(unique(interaction(combined_data$RCM, combined_data$season))))

# Crea el gráfico de cajas
boxplot_fig <- ggplot(combined_data, aes(x = factor(BC), y = Precip, fill = interaction(RCM, season))) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7, color = "darkgray", outlier.colour = "darkgray") +
  geom_boxplot(data = subset(combined_data, season == "verano"), 
               aes(x = factor(BC), y = Precip, fill = interaction(RCM, season)),
               position = position_dodge(width = 0.75), width = 0.7, color = "dodgerblue", outlier.colour = "dodgerblue") +
  geom_boxplot(data = subset(combined_data, season == "invierno"), 
               aes(x = factor(BC), y = Precip, fill = interaction(RCM, season)),
               position = position_dodge(width = 0.75), width = 0.7, color = "darkorange", outlier.colour = "darkorange") +
  scale_fill_manual(values = colores_combinaciones, name = "RCM & Season") +
  labs(y = "Proyección de precipitación diaria promedio (mm)", x = "BC") +
  theme_bw() +
  geom_hline(yintercept = 5.94623, linetype = "dashed", color = "blue") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    text = element_text(size = 7)
  )

# Muestra el gráfico
print(boxplot_fig)




library(ggplot2)
library(RColorBrewer)

# Paletas de colores
palette1 <- brewer.pal(5, "Blues")
palette2 <- brewer.pal(6, "Reds")
palette3 <- brewer.pal(8, "Greens")
palette4 <- brewer.pal(4, "Purples")

# Combina las paletas
combined_palette <- c(palette1, palette2, palette3, palette4)

# Asegúrate de tener la cantidad deseada de colores únicos
total_colors <- 22
combined_palette <- unique(rep(combined_palette, length.out = total_colors))

# Crea el gráfico de cajas
boxplot_fig <- ggplot(combined_data, aes(x = factor(BC), y = Precip, fill = interaction(RCM, season))) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7, color = "darkgray", 
               outlier.colour = "darkgray",outlier.size = 0.7, outlier.shape = 8) +
  geom_hline(yintercept = 3.448689, linetype = "twodash", color = "blue", size = 0.7) +
  geom_hline(yintercept = 2.396355, linetype = "dotdash", color = "red", size = 0.7) +
  scale_fill_manual(values = combined_palette, name = "RCM & Season") +
  labs(
    y = "Projection for Avg. Daily Precipitation (mm)",
    x = "",
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "center",
    legend.title = element_text(size = 5, face = "bold"),
    legend.text = element_text(size = 4.5),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(size = 7.3, face = "bold"),  # Tamaño del título del eje y
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),    #poner 730 por 430 tamaño grafica
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
  ) +
  guides(fill = guide_legend(ncol = 6.5, title = "RCM & Season")) +
  annotate("text", x = 0.95, y = 12.5, label = "H - Historical (1981 - 2004)", size = 3.7, color = "#0072B2", family = "serif") 


boxplot_fig <- boxplot_fig +
  theme(panel.background = element_rect(fill = "white"))
