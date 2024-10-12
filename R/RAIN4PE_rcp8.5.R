

library(ncdf4)    #to read the netcdf data
library(raster)   #to extract times series data from raster
library(tidyverse)#to mutate data, apply statistics
library(dplyr)
library(ggplot2)  #to plot
library(trend)

setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/RAIN4PE/RAW _rcp8.5/")
rm(list=ls())

################################################################################
#                              LS
################################################################################

North <- read.csv("LS_raw1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFLS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3LS$start_date<- NULL
NUtahESM2M_RCA4_v3LS$end_date <- NULL

North <- read.csv("LS_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorESM1_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorESM1_RCA4_v3LS$start_date<- NULL
NUtahNorESM1_RCA4_v3LS$end_date <- NULL


North <- read.csv("LS_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorCanESM2_SAM20LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorCanESM2_SAM20LS$start_date<- NULL
NUtahNorCanESM2_SAM20LS$end_date <- NULL

North <- read.csv("LS_raw9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorMIROC5_SAM20LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFPT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3PT$start_date<- NULL
NUtahESM2M_RCA4_v3PT$end_date <- NULL

North <- read.csv("PT_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorESM1_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorESM1_RCA4_v3PT$start_date<- NULL
NUtahNorESM1_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorCanESM2_SAM20PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorCanESM2_SAM20PT$start_date<- NULL
NUtahNorCanESM2_SAM20PT$end_date <- NULL


North <- read.csv("PT_raw9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorMIROC5_SAM20PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_WRFDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCSIRO_WRFDM$start_date<- NULL
NUtahCSIRO_WRFDM$end_date <- NULL


North <- read.csv("DM_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahICHECECDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahICHECECDM$start_date<- NULL
NUtahICHECECDM$end_date <- NULL

North <- read.csv("DM_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEIPSLCM5ADM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahEIPSLCM5ADM$start_date<- NULL
NUtahEIPSLCM5ADM$end_date <- NULL


North <- read.csv("DM_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROCMIROC5DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahMIROCMIROC5DM$start_date<- NULL
NUtahMIROCMIROC5DM$end_date <- NULL


North <- read.csv("DM_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NCCNorESM1DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NCCNorESM1DM$start_date<- NULL
NCCNorESM1DM$end_date <- NULL


North <- read.csv("DM_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNOAAGFDLESM2MDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNOAAGFDLESM2MDM$start_date<- NULL
NUtahNOAAGFDLESM2MDM$end_date <- NULL


################################################################################
#                                BC
################################################################################
setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/RAIN4PE/BC_rcp8.5/")

################################################################################
#                              LS
################################################################################

North <- read.csv("LS_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS9<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS9$start_date<- NULL
LS9$end_date <- NULL



################################################################################
#                              PT
################################################################################

North <- read.csv("PT_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT9<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT9$start_date<- NULL
PT9$end_date <- NULL


################################################################################
#                              DM
################################################################################

North <- read.csv("DM_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-04-01') | same_year >=as.Date('2070-11-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM7$start_date<- NULL
DM7$end_date <- NULL

################################################################################
#mediana_pisco <- median(NUtahRAIN4PE_PIS$Precip, na.rm = TRUE)


LS1$RCM <- c("CCCma-CanESM2")
LS2$RCM <- c("CSIRO-QCCCE-Mk3")
LS3$RCM <- c("ICHEC-EC-EARTH")
LS4$RCM <- c("IPSL-CM5A-MR")
LS5$RCM <- c("MIROC-MIROC5")
LS6$RCM <- c("NCC-NorESM1-M")
LS7$RCM <- c("NOAA-GFDL-ESM2M")
LS8$RCM <- c("CCCma-CanESM2-SAM20")
LS9$RCM <- c("MIROC-MIROC5-SAM20")

PT1$RCM <- c("CCCma-CanESM2")
PT2$RCM <- c("CSIRO-QCCCE-Mk3")
PT3$RCM <- c("ICHEC-EC-EARTH")
PT4$RCM <- c("IPSL-CM5A-MR")
PT5$RCM <- c("MIROC-MIROC5")
PT6$RCM <- c("NCC-NorESM1-M")
PT7$RCM <- c("NOAA-GFDL-ESM2M")
PT8$RCM <- c("CCCma-CanESM2-SAM20")
PT9$RCM <- c("MIROC-MIROC5-SAM20")

DM1$RCM <- c("CCCma-CanESM2")
DM2$RCM <- c("CSIRO-QCCCE-Mk3")
DM3$RCM <- c("ICHEC-EC-EARTH")
DM4$RCM <- c("IPSL-CM5A-MR")
DM5$RCM <- c("MIROC-MIROC5")
DM6$RCM <- c("NCC-NorESM1-M")
DM7$RCM <- c("NOAA-GFDL-ESM2M")

LS <- rbind(LS1, LS2,LS3,
            LS4,LS5, 
            LS6,LS7,LS8,
            LS9)
LS$BC <- "LS"
PT <- rbind(PT1, PT2,PT3,
            PT4,PT5,
            PT6,PT7,PT8,
            PT9)
PT$BC <- "PT"
DM <- rbind(DM1, DM2,DM3,
            DM4,DM5, 
            DM6,DM7)

DM$BC <- "DM"


NUtahCanESM2_WRFLS$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3LS$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3LS$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3LS$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3LS$RCM <- c("MIROC-MIROC5")
NUtahESM2M_RCA4_v3LS$RCM <- c("NCC-NorESM1-M")
NUtahNorESM1_RCA4_v3LS$RCM <- c("NOAA-GFDL-ESM2M")
NUtahNorCanESM2_SAM20LS$RCM <- c("CCCma-CanESM2-SAM20")
NUtahNorMIROC5_SAM20LS$RCM <- c("MIROC-MIROC5-SAM20")

NUtahCanESM2_WRFPT$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3PT$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3PT$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3PT$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3PT$RCM <- c("MIROC-MIROC5")
NUtahESM2M_RCA4_v3PT$RCM <- c("NCC-NorESM1-M")
NUtahNorESM1_RCA4_v3PT$RCM <- c("NOAA-GFDL-ESM2M")
NUtahNorCanESM2_SAM20PT$RCM <- c("CCCma-CanESM2-SAM20")
NUtahNorMIROC5_SAM20PT$RCM <- c("MIROC-MIROC5-SAM20")

NUtahCanESM2_WRFDM$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_WRFDM$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahICHECECDM$RCM <- c("ICHEC-EC-EARTH")
NUtahEIPSLCM5ADM$RCM <- c("IPSL-CM5A-MR")
NUtahMIROCMIROC5DM$RCM <- c("MIROC-MIROC5")
NCCNorESM1DM$RCM <- c("NCC-NorESM1-M")
NUtahNOAAGFDLESM2MDM$RCM <- c("NOAA-GFDL-ESM2M")



LS_raw <- rbind(NUtahCanESM2_WRFLS, NUtahCSIRO_Mk3LS,NUtahEC_EARTH_RCA4_v3LS,
                NUtahIPSL_RCA4_v3LS,NUtahMIROC5_RCA4_v3LS, 
                NUtahESM2M_RCA4_v3LS,NUtahNorESM1_RCA4_v3LS,
                NUtahNorCanESM2_SAM20LS,NUtahNorMIROC5_SAM20LS)
LS_raw$BC <- "LS_raw"
PT_raw <- rbind(NUtahCanESM2_WRFPT, NUtahCSIRO_Mk3PT,NUtahEC_EARTH_RCA4_v3PT,
                NUtahIPSL_RCA4_v3PT,NUtahMIROC5_RCA4_v3PT,
                NUtahESM2M_RCA4_v3PT,NUtahNorESM1_RCA4_v3PT,
                NUtahNorCanESM2_SAM20PT,NUtahNorMIROC5_SAM20PT)
PT_raw$BC <- "PT_raw"
DM_raw <- rbind(NUtahCanESM2_WRFDM, NUtahCSIRO_WRFDM,NUtahICHECECDM,
                NUtahEIPSLCM5ADM, NUtahMIROCMIROC5DM,
                NCCNorESM1DM,NUtahNOAAGFDLESM2MDM)

DM_raw$BC <- "DM_raw"

all_precip_invierno <- rbind(LS, PT, DM, LS_raw, PT_raw, DM_raw)  #Para Plotear
#all_precip <- rbind(LS, PT, DM)                         #  Kendall

all_precip_invierno$BC <- factor(all_precip_invierno$BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT")) #Para Plotear

# Filter out year 2100 since this would represent the 2100-2101 ski season, and we don't have 2101 data (so it's not accurate)
Combined_precip_invierno <- all_precip_invierno %>% dplyr::filter(run != 2100) %>% dplyr::filter(run > 2069)

# Figure
precip_fig <- ggplot(all_precip,
                     aes(x=run,y=Precip, color=RCM)) + 
  scale_color_manual(values = c('red',"#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                                "#0072B2", "#D55E00", "#CC79A7", "#83db5a", "#CC79A7", "black"))+
  geom_line(alpha=1.5, lwd = 0.6)+
  #geom_smooth(method = "lm", lwd = 0.6)+
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = "") +
  xlim(2070,2099)+ theme_bw()+
  facet_wrap(~ BC, ncol=2, nrow = 3) # if removed, x t

precip_fig <- ggplot(all_precip,
                     aes(x = run, y = Precip, color = RCM)) + 
  scale_color_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                                "#0072B2", "#D55E00", "#CC79A7", "#83db5a", "#CC79A7", "black")) +
  geom_point(alpha = 1.5, size = 2, shape = 2) +  # Cambiado a geom_point() para puntos en lugar de líneas
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = "") +
  xlim(2070, 2099) + theme_bw() +
  facet_wrap(~ BC, ncol = 2, nrow = 3)


precip_fig <- precip_fig +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "bottom", title.hjust = 0.5,title = " ")) +
  theme(
    legend.position = "bottom",  # Coloca la leyenda en la parte inferior
    legend.direction = "horizontal",
    text = element_text(size = 8)
  )

precip_fig


library(ggplot2)

boxplot_fig <- ggplot(Combined_precip_invierno , aes(x = run, y = Precip, fill = RCM)) +
  geom_boxplot() + 
  scale_fill_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                               "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = " ") +
  xlim(2070,2099)+ theme_bw()+
  #geom_boxplot(width = 0.8)+
  facet_wrap(~ BC, ncol=2, nrow = 3)
#geom_hline(yintercept = 3.998, linetype = "dashed", color = "blue")
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
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = " ") +
  xlim(2070,2099) + theme_bw() +
  facet_wrap(~ BC, ncol = 2, nrow = 3) + 
  geom_hline(yintercept = 2.396, linetype = "dashed", color = "blue")


point_fig <- ggplot(all_precip, aes(x = run, y = Precip, color = RCM)) +
  geom_point(size = 1, alpha = 0.7, shape = 2) +  # Cambiado a geom_point()
  scale_color_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                                "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), Dec - March", x = " ") +
  xlim(2070,2099) + theme_bw() +
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


################################################################################
# KENDALL 
################################################################################
all_precip <- rbind(LS, PT, DM)                         #  Kendall

all_precip$BC <- factor(all_precip$BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT")) #Para Plotear

# Filter out year 2100 since this would represent the 2100-2101 ski season, and we don't have 2101 data (so it's not accurate)
Combined_precip <- all_precip %>% dplyr::filter(run != 2100) %>% dplyr::filter(run > 2069)

# Setting up an empty table to record results
table1 <- matrix(NA, nrow = 25, ncol = 12)
x <- c("RCM", "geo", "z stat", "S", "S var", "tau", "p-value", "sen's slope", "95% CI, low", "95% CI, high", "Change by 2100", "metric")
colnames(table1) <- x
table1 <- as.data.frame(table1)
table1[,1:2] <- as.character(table1[,1:2])

GEO <- c("LS","LS","LS", "LS", "LS", "LS", "LS","LS", "LS","PT","PT","PT", "PT", "PT", "PT", "PT", "PT", "PT","DM", "DM", "DM", "DM", "DM", "DM", "DM")# 9, 9,7
RCM <- c("CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20", # 9
         "CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20", # 9
         "CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M") # 7

##### Max Temp M-K
for (i in 1:length(GEO)) {
  rcm <- as.character(RCM[i])  #choose scenario
  geo <- as.character(GEO[i]) # Chose geography
  
  data1 <- Combined_precip %>% dplyr::filter(RCM == rcm) %>% dplyr::filter(BC == geo) #Filters the database to only have data from this geo and RCP
  maxtemp <- trend::mk.test(data1$Precip, continuity = TRUE) # Runs Mann-Kendall trend test
  sen <- sens.slope(data1$Precip, conf.level = 0.95)
  # Fill in the table
  table1[i,1] <- rcm
  table1[i,2] <- geo
  table1[i,3] <- maxtemp$statistic # records z statistic
  table1[i,4] <- maxtemp$estimates[1] # records S
  table1[i,5] <- maxtemp$estimates[2] # records variance on S
  table1[i,6] <- maxtemp$estimates[3] # records tau
  table1[i,7] <- maxtemp$p.value # records p-value
  table1[i,8] <- sen$estimates # records Sen's slope
  table1[i,9] <- sen$conf.int[1] # records CI
  table1[i,10] <- sen$conf.int[2] # records CI
  table1[i,11] <- (sen$estimates) * 78
  table1[i,12] <- "Precipitation Projection (daily mm by season)"
}

# Rounds everything to 3 decimals
table1[,3:11] <- round(table1[,3:11], 3)
table1
# Output table if desired
write.csv(table1, "M_K_trends_precip_PROJECTION_winter_rain4pe.csv", row.names = FALSE)


################################################################################
#                              verano
################################################################################


library(ncdf4)    #to read the netcdf data
library(raster)   #to extract times series data from raster
library(tidyverse)#to mutate data, apply statistics
library(dplyr)
library(ggplot2)  #to plot
library(trend)

setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/RAIN4PE/RAW _rcp8.5/")
#rm(list=ls())

################################################################################
#                              LS
################################################################################

North <- read.csv("LS_raw1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFLS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3LS$start_date<- NULL
NUtahESM2M_RCA4_v3LS$end_date <- NULL

North <- read.csv("LS_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorESM1_RCA4_v3LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorESM1_RCA4_v3LS$start_date<- NULL
NUtahNorESM1_RCA4_v3LS$end_date <- NULL


North <- read.csv("LS_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorCanESM2_SAM20LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorCanESM2_SAM20LS$start_date<- NULL
NUtahNorCanESM2_SAM20LS$end_date <- NULL

North <- read.csv("LS_raw9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorMIROC5_SAM20LS<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFPT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_Mk3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROC5_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahESM2M_RCA4_v3PT$start_date<- NULL
NUtahESM2M_RCA4_v3PT$end_date <- NULL

North <- read.csv("PT_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorESM1_RCA4_v3PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorESM1_RCA4_v3PT$start_date<- NULL
NUtahNorESM1_RCA4_v3PT$end_date <- NULL


North <- read.csv("PT_raw8.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorCanESM2_SAM20PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNorCanESM2_SAM20PT$start_date<- NULL
NUtahNorCanESM2_SAM20PT$end_date <- NULL


North <- read.csv("PT_raw9.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNorMIROC5_SAM20PT<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCanESM2_WRFDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahCSIRO_WRFDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahCSIRO_WRFDM$start_date<- NULL
NUtahCSIRO_WRFDM$end_date <- NULL


North <- read.csv("DM_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahICHECECDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahICHECECDM$start_date<- NULL
NUtahICHECECDM$end_date <- NULL

North <- read.csv("DM_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEIPSLCM5ADM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahEIPSLCM5ADM$start_date<- NULL
NUtahEIPSLCM5ADM$end_date <- NULL


North <- read.csv("DM_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahMIROCMIROC5DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahMIROCMIROC5DM$start_date<- NULL
NUtahMIROCMIROC5DM$end_date <- NULL


North <- read.csv("DM_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NCCNorESM1DM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NCCNorESM1DM$start_date<- NULL
NCCNorESM1DM$end_date <- NULL


North <- read.csv("DM_raw7.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahNOAAGFDLESM2MDM<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

NUtahNOAAGFDLESM2MDM$start_date<- NULL
NUtahNOAAGFDLESM2MDM$end_date <- NULL


################################################################################
#                                BC
################################################################################
setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/RAIN4PE/BC_rcp8.5/")

################################################################################
#                              LS
################################################################################

North <- read.csv("LS_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

LS9<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

LS9$start_date<- NULL
LS9$end_date <- NULL



################################################################################
#                              PT
################################################################################

North <- read.csv("PT_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT8<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

PT9<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

PT9$start_date<- NULL
PT9$end_date <- NULL


################################################################################
#                              DM
################################################################################

North <- read.csv("DM_1.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM1<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM2<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM3<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM4<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM5<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM6<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
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
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

DM7<- NUtah %>% 
  # reformat data to keep months and days, but use identical year, so...
  mutate(same_year = as.Date(format(date, '2070-%m-%d'))) %>% 
  # Use the dates for the ski season here
  filter(same_year < as.Date('2070-06-01') | same_year >=as.Date('2070-10-30')) %>% 
  # shift so all in one year and use for grouping
  group_by(run=as.integer(format(date-250, '%Y'))) %>% 
  summarise(    # aggregate each gruop for the specified time period
    start_date = min(date), end_date = max(date), 
    Precip = mean(Ppt))

#write.csv(NUtahCanESM2_WRF , "D:/R/NUtahCanESM2_WRF.csv")

DM7$start_date<- NULL
DM7$end_date <- NULL

################################################################################
#NUtahRAIN4PE_PIS$RCM <- c("PISCO")
#mediana_pisco <- median(NUtahRAIN4PE_PIS$Precip, na.rm = TRUE)


LS1$RCM <- c("CCCma-CanESM2")
LS2$RCM <- c("CSIRO-QCCCE-Mk3")
LS3$RCM <- c("ICHEC-EC-EARTH")
LS4$RCM <- c("IPSL-CM5A-MR")
LS5$RCM <- c("MIROC-MIROC5")
LS6$RCM <- c("NCC-NorESM1-M")
LS7$RCM <- c("NOAA-GFDL-ESM2M")
LS8$RCM <- c("CCCma-CanESM2-SAM20")
LS9$RCM <- c("MIROC-MIROC5-SAM20")

PT1$RCM <- c("CCCma-CanESM2")
PT2$RCM <- c("CSIRO-QCCCE-Mk3")
PT3$RCM <- c("ICHEC-EC-EARTH")
PT4$RCM <- c("IPSL-CM5A-MR")
PT5$RCM <- c("MIROC-MIROC5")
PT6$RCM <- c("NCC-NorESM1-M")
PT7$RCM <- c("NOAA-GFDL-ESM2M")
PT8$RCM <- c("CCCma-CanESM2-SAM20")
PT9$RCM <- c("MIROC-MIROC5-SAM20")

DM1$RCM <- c("CCCma-CanESM2")
DM2$RCM <- c("CSIRO-QCCCE-Mk3")
DM3$RCM <- c("ICHEC-EC-EARTH")
DM4$RCM <- c("IPSL-CM5A-MR")
DM5$RCM <- c("MIROC-MIROC5")
DM6$RCM <- c("NCC-NorESM1-M")
DM7$RCM <- c("NOAA-GFDL-ESM2M")

LS <- rbind(LS1, LS2,LS3,
            LS4,LS5, 
            LS6,LS7,LS8,
            LS9)
LS$BC <- "LS"
PT <- rbind(PT1, PT2,PT3,
            PT4,PT5,
            PT6,PT7,PT8,
            PT9)
PT$BC <- "PT"
DM <- rbind(DM1, DM2,DM3,
            DM4,DM5, 
            DM6,DM7)

DM$BC <- "DM"


NUtahCanESM2_WRFLS$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3LS$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3LS$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3LS$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3LS$RCM <- c("MIROC-MIROC5")
NUtahESM2M_RCA4_v3LS$RCM <- c("NCC-NorESM1-M")
NUtahNorESM1_RCA4_v3LS$RCM <- c("NOAA-GFDL-ESM2M")
NUtahNorCanESM2_SAM20LS$RCM <- c("CCCma-CanESM2-SAM20")
NUtahNorMIROC5_SAM20LS$RCM <- c("MIROC-MIROC5-SAM20")

NUtahCanESM2_WRFPT$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_Mk3PT$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahEC_EARTH_RCA4_v3PT$RCM <- c("ICHEC-EC-EARTH")
NUtahIPSL_RCA4_v3PT$RCM <- c("IPSL-CM5A-MR")
NUtahMIROC5_RCA4_v3PT$RCM <- c("MIROC-MIROC5")
NUtahESM2M_RCA4_v3PT$RCM <- c("NCC-NorESM1-M")
NUtahNorESM1_RCA4_v3PT$RCM <- c("NOAA-GFDL-ESM2M")
NUtahNorCanESM2_SAM20PT$RCM <- c("CCCma-CanESM2-SAM20")
NUtahNorMIROC5_SAM20PT$RCM <- c("MIROC-MIROC5-SAM20")

NUtahCanESM2_WRFDM$RCM <- c("CCCma-CanESM2")
NUtahCSIRO_WRFDM$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahICHECECDM$RCM <- c("ICHEC-EC-EARTH")
NUtahEIPSLCM5ADM$RCM <- c("IPSL-CM5A-MR")
NUtahMIROCMIROC5DM$RCM <- c("MIROC-MIROC5")
NCCNorESM1DM$RCM <- c("NCC-NorESM1-M")
NUtahNOAAGFDLESM2MDM$RCM <- c("NOAA-GFDL-ESM2M")



LS_raw <- rbind(NUtahCanESM2_WRFLS, NUtahCSIRO_Mk3LS,NUtahEC_EARTH_RCA4_v3LS,
                NUtahIPSL_RCA4_v3LS,NUtahMIROC5_RCA4_v3LS, 
                NUtahESM2M_RCA4_v3LS,NUtahNorESM1_RCA4_v3LS,
                NUtahNorCanESM2_SAM20LS,NUtahNorMIROC5_SAM20LS)
LS_raw$BC <- "LS_raw"
PT_raw <- rbind(NUtahCanESM2_WRFPT, NUtahCSIRO_Mk3PT,NUtahEC_EARTH_RCA4_v3PT,
                NUtahIPSL_RCA4_v3PT,NUtahMIROC5_RCA4_v3PT,
                NUtahESM2M_RCA4_v3PT,NUtahNorESM1_RCA4_v3PT,
                NUtahNorCanESM2_SAM20PT,NUtahNorMIROC5_SAM20PT)
PT_raw$BC <- "PT_raw"
DM_raw <- rbind(NUtahCanESM2_WRFDM, NUtahCSIRO_WRFDM,NUtahICHECECDM,
                NUtahEIPSLCM5ADM, NUtahMIROCMIROC5DM,
                NCCNorESM1DM,NUtahNOAAGFDLESM2MDM)

DM_raw$BC <- "DM_raw"

all_precip_verano <- rbind(LS, PT, DM, LS_raw, PT_raw, DM_raw)  #Para Plotear
#all_precip <- rbind(LS, PT, DM)                         #  Kendall

all_precip_verano$BC <- factor(all_precip_verano$BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT")) #Para Plotear

# Filter out year 2100 since this would represent the 2100-2101 ski season, and we don't have 2101 data (so it's not accurate)
Combined_precip_verano <- all_precip_verano %>% dplyr::filter(run != 2100) %>% dplyr::filter(run > 2069)

# Figure
precip_fig <- ggplot(all_precip,
                     aes(x=run,y=Precip, color=RCM)) + 
  scale_color_manual(values = c('red',"#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                                "#0072B2", "#D55E00", "#CC79A7", "#83db5a", "#CC79A7", "black"))+
  geom_line(alpha=1.5, lwd = 0.6)+
  #geom_smooth(method = "lm", lwd = 0.6)+
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = "") +
  xlim(2070,2099)+ theme_bw()+
  facet_wrap(~ BC, ncol=2, nrow = 3) # if removed, x t

precip_fig <- ggplot(all_precip,
                     aes(x = run, y = Precip, color = RCM)) + 
  scale_color_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                                "#0072B2", "#D55E00", "#CC79A7", "#83db5a", "#CC79A7", "black")) +
  geom_point(alpha = 1.5, size = 2, shape = 2) +  # Cambiado a geom_point() para puntos en lugar de líneas
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = "") +
  xlim(2070, 2099) + theme_bw() +
  facet_wrap(~ BC, ncol = 2, nrow = 3)


precip_fig <- precip_fig +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "bottom", title.hjust = 0.5,title = " ")) +
  theme(
    legend.position = "bottom",  # Coloca la leyenda en la parte inferior
    legend.direction = "horizontal",
    text = element_text(size = 8)
  )

precip_fig


library(ggplot2)

boxplot_fig <- ggplot(all_precip, aes(x = run, y = Precip, fill = RCM)) +
  geom_boxplot() + 
  scale_fill_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                               "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = " ") +
  xlim(2070,2099)+ theme_bw()+
  #geom_boxplot(width = 0.8)+
  facet_wrap(~ BC, ncol=2, nrow = 3)
#geom_hline(yintercept = 3.998, linetype = "dashed", color = "blue")
boxplot_fig <- boxplot_fig +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "bottom", title.hjust = 0.5,title = " ")) +
  theme(
    legend.position = "bottom",  # Coloca la leyenda en la parte inferior
    legend.direction = "horizontal",
    text = element_text(size = 7)
  )

boxplot_fig

boxplot_fig <- ggplot(all_precip, aes(x = run, y = Precip, fill = RCM)) +
  geom_boxplot() + 
  scale_fill_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                               "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = " ") +
  xlim(2070,2099)+ theme_bw()+
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
  labs(y = "Projection for Avg. Daily Winter Precipitation (mm), June - Sept", x = " ") +
  xlim(2070,2099) + theme_bw() +
  facet_wrap(~ BC, ncol = 2, nrow = 3) + 
  geom_hline(yintercept = 2.396, linetype = "dashed", color = "blue")


point_fig <- ggplot(all_precip, aes(x = run, y = Precip, color = RCM)) +
  geom_point(size = 1, alpha = 0.7, shape = 2) +  # Cambiado a geom_point()
  scale_color_manual(values = c('red', "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                                "#D55E00", "#5acadb", "#83db5a", "#CC79A7", "black")) +
  labs(y = "Projection for Avg. Daily Summer Precipitation (mm), June - Sept", x = " ") +
  xlim(2070,2099) + theme_bw() +
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


################################################################################
                             # KENDALL 
################################################################################
all_precip <- rbind(LS, PT, DM)                         #  Kendall

all_precip$BC <- factor(all_precip$BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT")) #Para Plotear

# Filter out year 2100 since this would represent the 2100-2101 ski season, and we don't have 2101 data (so it's not accurate)
Combined_precip <- all_precip %>% dplyr::filter(run != 2100) %>% dplyr::filter(run > 2069)

# Setting up an empty table to record results
table1 <- matrix(NA, nrow = 25, ncol = 12)
x <- c("RCM", "geo", "z stat", "S", "S var", "tau", "p-value", "sen's slope", "95% CI, low", "95% CI, high", "Change by 2100", "metric")
colnames(table1) <- x
table1 <- as.data.frame(table1)
table1[,1:2] <- as.character(table1[,1:2])

GEO <- c("LS","LS","LS", "LS", "LS", "LS", "LS","LS", "LS","PT","PT","PT", "PT", "PT", "PT", "PT", "PT", "PT","DM", "DM", "DM", "DM", "DM", "DM", "DM")# 9, 9,7
RCM <- c("CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20", # 9
         "CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20", # 9
         "CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M") # 7

##### Max Temp M-K
for (i in 1:length(GEO)) {
  rcm <- as.character(RCM[i])  #choose scenario
  geo <- as.character(GEO[i]) # Chose geography
  
  data1 <- Combined_precip %>% dplyr::filter(RCM == rcm) %>% dplyr::filter(BC == geo) #Filters the database to only have data from this geo and RCP
  maxtemp <- trend::mk.test(data1$Precip, continuity = TRUE) # Runs Mann-Kendall trend test
  sen <- sens.slope(data1$Precip, conf.level = 0.95)
  # Fill in the table
  table1[i,1] <- rcm
  table1[i,2] <- geo
  table1[i,3] <- maxtemp$statistic # records z statistic
  table1[i,4] <- maxtemp$estimates[1] # records S
  table1[i,5] <- maxtemp$estimates[2] # records variance on S
  table1[i,6] <- maxtemp$estimates[3] # records tau
  table1[i,7] <- maxtemp$p.value # records p-value
  table1[i,8] <- sen$estimates # records Sen's slope
  table1[i,9] <- sen$conf.int[1] # records CI
  table1[i,10] <- sen$conf.int[2] # records CI
  table1[i,11] <- (sen$estimates) * 78
  table1[i,12] <- "Precipitation Projection (daily mm by season)"
}

# Rounds everything to 3 decimals
table1[,3:11] <- round(table1[,3:11], 3)
table1
# Output table if desired
write.csv(table1, "M_K_trends_precip_PROJECTION.csv", row.names = FALSE)


# Crear un gráfico de barras para mostrar las estimaciones de la pendiente de Sen
library(ggplot2)

# Crear un gráfico de barras para mostrar las estimaciones de la pendiente de Sen
ggplot(data = table1, aes(x = interaction(RCM, GEO), y = `sen's slope`, fill = factor(GEO))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ factor("GEO"), ncol = 1) +
  labs(x = "Combinación RCM y GEO", y = "Estimación de la pendiente de Sen",
       fill = "Geografía (GEO)", title = "Estimación de la pendiente de Sen por RCM y GEO") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###################################################################################
###############  ok plot
##################################################################################
combined_data <- rbind(
  transform(all_precip_invierno, season = "DJF"),
  transform(Combined_precip_verano, season = "JJA")
)


library(ggplot2)
library(RColorBrewer)

# Define paletas de colores
palette1 <- brewer.pal(4, "PuRd")
palette2 <- brewer.pal(5, "PuOr")
palette3 <- brewer.pal(7, "YlGn")
palette4 <- brewer.pal(4, "BuGn")

# Combina las paletas
combined_palette <- c(palette1, palette2, palette3, palette4)

# Asegúrate de tener la cantidad deseada de colores únicos
total_colors <- 18
combined_palette <- unique(rep(combined_palette, length.out = total_colors))

# Crea el gráfico de cajas
boxplot_fig <- ggplot(combined_data, aes(x = factor(BC), y = Precip, fill = interaction(RCM, season))) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7, color = "darkgray", 
               outlier.colour = "blue",outlier.size = 0.7, outlier.shape = 8) + #outlier.colour = "cornflowerblue"
  #geom_hline(yintercept = 3.448689, linetype = "twodash", color = "blue", size = 1) +
  #geom_hline(yintercept = 2.396355, linetype = "dotdash", color = "red", size = 1) +
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
  annotate("text", x = 0.9, y = 10.3, label = "F - Future (2070 - 2099)", size = 3.7, color = "cornflowerblue", family = "serif")

# Muestra el gráfico
print(boxplot_fig)



# Color gris transparente
library(scales)
# Asegúrate de que BC y season son factores con los niveles adecuado

# Define las categorías sombreadas
shaded_categories <- c("LS", "DM", "LS", "PTT")

# Color azul claro transparente
transparent_blue <- alpha("lightblue", .01)

# Crea el gráfico
boxplot_fig <- ggplot(combined_data, aes(x = factor(BC), y = Precip, fill = interaction(RCM, season))) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7, color = "darkgray", outlier.colour = "darkgray") +
  scale_fill_manual(values = combined_palette, name = "RCM & Season") +
  labs(
    y = "Proyección de precipitación diaria promedio (mm)",
    x = "BC",
    title = "Boxplot de Proyección de Precipitación",
    subtitle = "Datos de Invierno y Verano",
    caption = "Fuente: Tu fuente de datos"
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
    axis.title.y = element_text(size = 8.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  geom_hline(yintercept = 5.94623, linetype = "dashed", color = "blue") +
  geom_rect(
    xmin = as.numeric(factor(shaded_categories[1])) - 0.1,
    xmax = as.numeric(factor(shaded_categories[2])) + 3,
    ymin = -Inf, ymax = Inf,
    fill = transparent_blue
  )

# Muestra el gráfico
print(boxplot_fig)


transparent_blue <- alpha("lightblue", .3)
transparent_green <- alpha("lightgreen", .3)
transparent_yellow <- alpha("lightyellow", .3)

# Crea el gráfico
boxplot_fig <- ggplot(combined_data, aes(x = factor(BC), y = Precip, fill = interaction(RCM, season))) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.7, color = "darkgray", outlier.colour = "darkgray") +
  scale_fill_manual(values = combined_palette, name = "RCM & Season") +
  labs(
    y = "Proyección de precipitación diaria promedio (mm)",
    x = "BC",
    title = "Boxplot de Proyección de Precipitación",
    subtitle = "Datos de Invierno y Verano",
    caption = "Fuente: Tu fuente de datos"
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
    axis.title.y = element_text(size = 8.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  geom_hline(yintercept = 5.94623, linetype = "dashed", color = "blue") +
  geom_rect(
    xmin = as.numeric(factor(shaded_categories[1])) - 0.5,
    xmax = as.numeric(factor(shaded_categories[2])) + 1,
    ymin = -Inf, ymax = Inf,
    fill = transparent_blue
  ) +
  geom_rect(
    xmin = as.numeric(factor(shaded_categories[3])) - 0.0001,
    xmax = as.numeric(factor(shaded_categories[4])) + 4,
    ymin = -Inf, ymax = Inf,
    fill = transparent_green
  ) +
  geom_rect(
    xmin = as.numeric(factor(shaded_categories[5])) - 0.1,
    xmax = as.numeric(factor(shaded_categories[6])) + 3,
    ymin = -Inf, ymax = Inf,
    fill = transparent_yellow
  )

# Muestra el gráfico
print(boxplot_fig)





