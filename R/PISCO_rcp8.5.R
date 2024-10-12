
library(ncdf4)    #to read the netcdf data
library(raster)   #to extract times series data from raster
library(tidyverse)#to mutate data, apply statistics
library(dplyr)
library(ggplot2)  #to plot
library(trend)

setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/PISCO/RAW _rcp8.5/")
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

NUtahCSIRO_Mk3DM<- NUtah %>% 
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

NUtahCSIRO_Mk3DM$start_date<- NULL
NUtahCSIRO_Mk3DM$end_date <- NULL


North <- read.csv("DM_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3DM<- NUtah %>% 
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

NUtahEC_EARTH_RCA4_v3DM$start_date<- NULL
NUtahEC_EARTH_RCA4_v3DM$end_date <- NULL

North <- read.csv("DM_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3DM<- NUtah %>% 
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

NUtahIPSL_RCA4_v3DM$start_date<- NULL
NUtahIPSL_RCA4_v3DM$end_date <- NULL


North <- read.csv("DM_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

HadGEM2_RCA4_v3DM<- NUtah %>% 
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

HadGEM2_RCA4_v3DM$start_date<- NULL
HadGEM2_RCA4_v3DM$end_date <- NULL


North <- read.csv("DM_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3DM<- NUtah %>% 
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

NUtahESM2M_RCA4_v3DM$start_date<- NULL
NUtahESM2M_RCA4_v3DM$end_date <- NULL


################################################################################
#                                BC
################################################################################
setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/PISCO/BC_rcp8.5/")

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

DM1$RCM <- c("CSIRO-QCCCE-Mk3")
DM2$RCM <- c("ICHEC-EC-EARTH")
DM3$RCM <- c("IPSL-CM5A-MR")
DM4$RCM <- c("MIROC-MIROC5")
DM5$RCM <- c("NCC-NorESM1-M")
DM6$RCM <- c("NOAA-GFDL-ESM2M")


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
            DM6)

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

NUtahCanESM2_WRFDM$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahCSIRO_Mk3DM$RCM <- c("ICHEC-EC-EARTH")
NUtahEC_EARTH_RCA4_v3DM$RCM <- c("IPSL-CM5A-MR")
NUtahIPSL_RCA4_v3DM$RCM <- c("MIROC-MIROC5")
HadGEM2_RCA4_v3DM$RCM <- c("NCC-NorESM1-M")
NUtahESM2M_RCA4_v3DM$RCM <- c("NOAA-GFDL-ESM2M")


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
DM_raw <- rbind(NUtahCanESM2_WRFDM, NUtahCSIRO_Mk3DM,NUtahEC_EARTH_RCA4_v3DM,
                NUtahIPSL_RCA4_v3DM, 
                HadGEM2_RCA4_v3DM,NUtahESM2M_RCA4_v3DM)

DM_raw$BC <- "DM_raw"
#all_precip <- rbind(LS_raw, PT_raw, DM_raw, LS, PT, DM)
all_precip_invierno <- rbind(LS, PT, DM, LS_raw, PT_raw, DM_raw)

all_precip_invierno $BC <- factor(all_precip_invierno $BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT"))

# Filter out year 2100 since this would represent the 2100-2101 ski season, and we don't have 2101 data (so it's not accurate)
Combined_precip_invierno <- all_precip_invierno  %>% dplyr::filter(run != 2100) %>% dplyr::filter(run > 2069)

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

precip_fig <- precip_fig +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "bottom", title.hjust = 0.5,title = " ")) +
  theme(
    legend.position = "bottom",  # Coloca la leyenda en la parte inferior
    legend.direction = "horizontal",
    text = element_text(size = 8)
  )

precip_fig


###########################################################################################
################## ok plot
##########################################################################################
library(gridExtra)

library(ggplot2)

# Crear el gráfico de cajas
boxplot_fig <- ggplot(Combined_precip_invierno, aes(x = run, y = Precip, fill = RCM)) +
  geom_boxplot() +
  scale_fill_manual(values = c('red', '#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', 
                               '#D55E00', '#5acadb', '#83db5a', '#CC79A7', 'black')) +
  labs(y = 'Projection for Avg. Daily Winter Precipitation (mm), Dec - March',
       x = '',
       title = '',
       subtitle = '',
       caption = '') +
  xlim(2070, 2099) +
  theme_bw() +
  facet_wrap(~ BC, ncol = 2, nrow = 3) +
  #geom_hline(yintercept = 2.396, linetype = 'dashed', color = 'blue') +
  theme(
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    legend.title = element_text(size = 7, face = 'bold'),
    legend.text = element_text(size = 6),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    plot.title = element_text(size = 16, face = 'bold'),
    plot.subtitle = element_text(size = 12),
    axis.title.y = element_text(size = 8.5, face = 'bold'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = 'black'),
    axis.text = element_text(color = 'black'),
    axis.ticks = element_line(color = 'black')
  )

# Añadir línea de tendencia al boxplot_fig
boxplot_fig <- boxplot_fig +
  geom_line(data = Combined_precip_invierno, aes(x = run, y = Precip, color = RCM),
            stat = 'summary', fun = 'mean', size = 0.05, linetype = 'solid') +
  geom_point(data = Combined_precip_invierno, aes(x = run, y = Precip, color = RCM), size = 0.05)

# Suprimir la leyenda para las líneas
boxplot_fig <- boxplot_fig + theme(legend.box = 'none')

# Muestra el gráfico
print(boxplot_fig)




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

#file.choose()
#setwd('D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/RAIN4PE/BC_rcp8.5')
#pdf (file ='plotsss.pdf')
#boxplot_fig
#dev.off()
################################################################################
# KENDALL 
################################################################################
all_precip <- rbind(LS, PT, DM)                         #  Kendall

#all_precip$BC <- factor(all_precip$BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT")) #Para Plotear

# Filter out year 2100 since this would represent the 2100-2101 ski season, and we don't have 2101 data (so it's not accurate)
Combined_precip <- all_precip %>% dplyr::filter(run != 2100) %>% dplyr::filter(run > 2069)

# Setting up an empty table to record results
table1 <- matrix(NA, nrow = 24, ncol = 12)
x <- c("RCM", "geo", "z stat", "S", "S var", "tau", "p-value", "sen's slope", "95% CI, low", "95% CI, high", "Change by 2100", "metric")
colnames(table1) <- x
table1 <- as.data.frame(table1)
table1[,1:2] <- as.character(table1[,1:2])

GEO <- c("LS","LS","LS", "LS", "LS", "LS", "LS","LS", "LS","PT","PT","PT", "PT", "PT", "PT", "PT", "PT", "PT","DM", "DM", "DM", "DM", "DM", "DM") # 9 ,9 ,6
RCM <- c("CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20",# 9
         "CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20",# 9
         "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M") # 6

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
write.csv(table1, "M_K_trends_v5 djf pisco.csv", row.names = FALSE)
################################################################################
# KENDALL    raw
################################################################################

all_precip <- rbind(LS_raw, PT_raw, DM_raw)                         #  Kendall

all_precip$BC <- factor(all_precip$BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT")) #Para Plotear

# Filter out year 2100 since this would represent the 2100-2101 ski season, and we don't have 2101 data (so it's not accurate)
Combined_precip <- all_precip %>% dplyr::filter(run != 2100) %>% dplyr::filter(run > 2069)

# Setting up an empty table to record results
table1 <- matrix(NA, nrow = 24, ncol = 12)
x <- c("RCM", "geo", "z stat", "S", "S var", "tau", "p-value", "sen's slope", "95% CI, low", "95% CI, high", "Change by 2100", "metric")
colnames(table1) <- x
table1 <- as.data.frame(table1)
table1[,1:2] <- as.character(table1[,1:2])

GEO <- c("LS_raw","LS_raw","LS_raw", "LS_raw", "LS_raw", "LS_raw", "LS_raw","LS_raw", "LS_raw","PT_raw","PT_raw","PT_raw", "PT_raw", "PT_raw", "PT_raw", "PT_raw", "PT_raw", "PT_raw","DM_raw", "DM_raw", "DM_raw", "DM_raw", "DM_raw", "DM_raw") # 9 ,9 ,6
RCM <- c("CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20",# 9
         "CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20",# 9
         "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M") # 6

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
write.csv(table1, "M_K_trends_precip_PROJECTION_pisco_WINTER.csv", row.names = FALSE)


################################################################################
#                              verano
################################################################################


library(ncdf4)    #to read the netcdf data
library(raster)   #to extract times series data from raster
library(tidyverse)#to mutate data, apply statistics
library(dplyr)
library(ggplot2)  #to plot
library(trend)

setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/PISCO/RAW _rcp8.5/")
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

NUtahCSIRO_Mk3DM<- NUtah %>% 
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

NUtahCSIRO_Mk3DM$start_date<- NULL
NUtahCSIRO_Mk3DM$end_date <- NULL


North <- read.csv("DM_raw3.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahEC_EARTH_RCA4_v3DM<- NUtah %>% 
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

NUtahEC_EARTH_RCA4_v3DM$start_date<- NULL
NUtahEC_EARTH_RCA4_v3DM$end_date <- NULL

North <- read.csv("DM_raw4.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahIPSL_RCA4_v3DM<- NUtah %>% 
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

NUtahIPSL_RCA4_v3DM$start_date<- NULL
NUtahIPSL_RCA4_v3DM$end_date <- NULL


North <- read.csv("DM_raw5.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

HadGEM2_RCA4_v3DM<- NUtah %>% 
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

HadGEM2_RCA4_v3DM$start_date<- NULL
HadGEM2_RCA4_v3DM$end_date <- NULL


North <- read.csv("DM_raw6.csv" ,head = T,check.names = F,stringsAsFactors = F)

North$North <- North$Ppt*1

#Create data in tibble, this is required for calculations to be done
#by resort season
NUtah<- tibble(
  date = seq(as.Date('2070-01-01'), as.Date('2099-12-31'), by = 'day'), 
  Ppt= North$North)

NUtahESM2M_RCA4_v3DM<- NUtah %>% 
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

NUtahESM2M_RCA4_v3DM$start_date<- NULL
NUtahESM2M_RCA4_v3DM$end_date <- NULL


################################################################################
#                                BC
################################################################################
setwd("D:/Paper_Climate/Paper/Data/climate_/v2_PISCO_RAIN4PE/PISCO/BC_rcp8.5/")

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

DM1$RCM <- c("CSIRO-QCCCE-Mk3")
DM2$RCM <- c("ICHEC-EC-EARTH")
DM3$RCM <- c("IPSL-CM5A-MR")
DM4$RCM <- c("MIROC-MIROC5")
DM5$RCM <- c("NCC-NorESM1-M")
DM6$RCM <- c("NOAA-GFDL-ESM2M")


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
            DM6)

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

NUtahCanESM2_WRFDM$RCM <- c("CSIRO-QCCCE-Mk3")
NUtahCSIRO_Mk3DM$RCM <- c("ICHEC-EC-EARTH")
NUtahEC_EARTH_RCA4_v3DM$RCM <- c("IPSL-CM5A-MR")
NUtahIPSL_RCA4_v3DM$RCM <- c("MIROC-MIROC5")
HadGEM2_RCA4_v3DM$RCM <- c("NCC-NorESM1-M")
NUtahESM2M_RCA4_v3DM$RCM <- c("NOAA-GFDL-ESM2M")


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
DM_raw <- rbind(NUtahCanESM2_WRFDM, NUtahCSIRO_Mk3DM,NUtahEC_EARTH_RCA4_v3DM,
                NUtahIPSL_RCA4_v3DM, 
                HadGEM2_RCA4_v3DM,NUtahESM2M_RCA4_v3DM)

DM_raw$BC <- "DM_raw"   # hasta aqui si desea para kendall
#all_precip <- rbind(LS_raw, PT_raw, DM_raw, LS, PT, DM)   
all_precip_verano <- rbind(LS, PT, DM, LS_raw, PT_raw, DM_raw)  #plot

all_precip_verano$BC <- factor(all_precip_verano$BC, levels = c('LS_raw', 'LS', 'DM_raw', "DM","PT_raw","PT"))

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
table1 <- matrix(NA, nrow = 24, ncol = 12)
x <- c("RCM", "geo", "z stat", "S", "S var", "tau", "p-value", "sen's slope", "95% CI, low", "95% CI, high", "Change by 2100", "metric")
colnames(table1) <- x
table1 <- as.data.frame(table1)
table1[,1:2] <- as.character(table1[,1:2])

GEO <- c("LS","LS","LS", "LS", "LS", "LS", "LS","LS", "LS","PT","PT","PT", "PT", "PT", "PT", "PT", "PT", "PT","DM", "DM", "DM", "DM", "DM", "DM") # 9 ,9 ,6
RCM <- c("CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20",# 9
         "CCCma-CanESM2", "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M","CCCma-CanESM2-SAM20", "MIROC-MIROC5-SAM20",# 9
         "CSIRO-QCCCE-Mk3", "ICHEC-EC-EARTH", "IPSL-CM5A-MR", "MIROC-MIROC5", "NCC-NorESM1-M", "NOAA-GFDL-ESM2M") # 6

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
write.csv(table1, "M_K_trends_v5 jjaok pisco.csv", row.names = FALSE)


###################################################################################
###############  ok plot   rcp8.5
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
  annotate("text", x = 0.9, y = 12.3, label = "F - Future (2070 - 2099)", size = 3.7, color = "cornflowerblue", family = "serif")+

  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 11.7,
           alpha = .2, fill = "orange")+
  annotate("text", x = 0.7, y = 11, label = "DJF", size = 3.7, color = "orange", family = "serif")+
  
  annotate("rect", xmin = 1, xmax = 1.5, ymin = 0, ymax = 11.7,
           alpha = .2, fill = "blue")+
  annotate("text", x = 1.3, y = 11, label = "JJA", size = 3.7, color = "blue", family = "serif")
# Muestra el gráfico
print(boxplot_fig)
# Muestra las fuentes instaladas en el sistem


# Instala e carga el paquete ggplot2 si aún no lo has hecho
# install.packages("ggplot2")
# Instala e carga el paquete ggplot2 si aún no lo has hecho
# install.packages("ggplot2")
# Instala e carga el paquete ggplot2 si aún no lo has hecho
# install.packages("ggplot2")
library(ggplot2)
# Datos proporcionados
# Datos proporci
library(ggh4x)

# Datos proporcionados
datos <- data.frame(
  Mes = 1:12,
 
  LS = c(435.899795,291.41191,493.506366,98.415606,35.161396,39.776591,12.435318,21.833265,77.715811,82.155236,86.727721,212.413142),
  PT = c(437.300205	,292.087064	,493.507187,	98.598768	,35.466119,	40.346612	,12.726899,	21.936756	,78.154415	,82.059959,	86.616016,	211.687064),
  DM = c(431.255852,	288.929774	,497.605749	,100.279261	,35.074333 ,	41.78809	, 13.553183 ,	21.681314	, 79.261602	, 83.610678 ,	87.917864 ,	210.785216),
  OBS= c(449.807803,	296.013142,	483.419302,	99.513758,	35.295277	,40.876386,	12.631622,	23.405339	,78.797536,	80.908419,	84.684189	,209.020945),
  RAW = c(236.058316,	198.025462,	173.79384	,107.556468	,47.980287,	36.965914	,36.473922,	59.324846	,90.433676,	86.309651	,120.216838	,211.209035)
)

# Transforma los datos a formato largo para ggplot2
datos_largos <- tidyr::gather(datos, key = "BC", value = "Valor", -Mes)

# Crea el gráfico de líneas con ggplot2
ggplot(datos_largos, aes(x = as.factor(Mes), y = Valor, color = BC, linetype = BC, group = BC)) +
  geom_line(size = 1.5, position = position_dodge(width = 0.7)) +
  labs(title = "",
       x = "",
       y = "Total accumulated precipitation (mm)",
       color = "BC", linetype = "Type of line :") +
  scale_color_manual(values = c("LS" = "#e41a1c","PT" = "#a65628","DM" = "#984ea3","RAW" = "#57e41a", "OBS" = "black"),
                     breaks = c("LS", "PT", "DM", "RAW", "OBS")) +  # Define el orden deseado
  scale_linetype_manual(values = c("LS" = "dotdash", "PT" = "dashed", "DM" = "dotted", "RAW" = "longdash","OBS" = "solid"),
                        breaks = c("LS", "PT", "DM", "RAW", "OBS")) +  # Define el orden deseado
  scale_x_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")) +
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.title.x = element_text(size = 9.5),
    axis.title.y = element_text(size = 9.5),  # Ajusta el tamaño del título del eje y
    axis.text = element_text(size = 7),  # Ajusta el tamaño del texto del eje
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
    panel.grid.minor = element_line(color = "gray", linetype = "dashed", size = 0.5),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
    axis.text.y = element_text(margin = margin(0, 0, 0, 10),size = 10)

  ) +
  geom_vline(xintercept = 1:12, linetype = "dashed", color = "gray", size = 0.5) +
  guides(color = guide_legend(nrow = 1, ncol = 7, byrow = TRUE, title.position = "bottom", title.hjust = 0.5, title = ""))+
  
  annotate("text", x = 2.7, y = 235, label = "Bias correction", size = 4, color = "black", family = "serif", show.legend = FALSE) +
  geom_segment(aes(x = 2.3, y = 250, xend = 3, yend = 250),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 1.3, show.legend = FALSE) +
  annotate("text", x = 1.50, y = 195, label = "Before Bias", size = 4, color = "#57e41a", family = "serif", show.legend = FALSE)



