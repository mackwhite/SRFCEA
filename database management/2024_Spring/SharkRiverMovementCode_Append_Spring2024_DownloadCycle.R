# Startup and information -------------------------------------------------
# SharkRiverMovementCode_AppendDownloadCycle_"Month(s)Year".R
# Date Last Updated: 10/18/2023
# Author: J. Massie - Modified by Mack White Summer 2023
# Purpose: Script takes raw VUE movement data from database offload and merges with a table to add distance from river mouth (station distances in river km), screens for Rehage Lab tags, filters detections by species, and plots timerseries of movement tracks

# turn on softwrap longlines
# turn on rainbow parantheses

# Read in packages from library and setwd ()-------------------------------
library(tidyverse)
library(scales)
library(ggplot2)

setwd("/Users/mack/Dropbox/Research/field work/Acoustic Telemetry/Sampling Events/Nov2023_DL")

# define custom save_rds function -----------------------------------------

save_rds <- function(data, filename) {
      saveRDS(data, file = filename)
      return(data)
}

# read in vue offload from previous download ------------------------------

NewestVUE <- read.csv("NovDec_2023_VUEoffload.csv")
glimpse(NewestVUE)

# manipulate data to match format of previous all vue rds file ------------
NewestVUE1 <- NewestVUE |> 
      select(-Transmitter.Type, -Sensor.Precision) |> 
      mutate(across(c(Receiver, Transmitter, Transmitter.Name, 
                      Sensor.Unit, Station.Name), factor),
             Datetime_UTC = as.POSIXct(Date.and.Time..UTC.,
                                            format = "%Y-%m-%d %H:%M:%S")) |> 
      select(-Date.and.Time..UTC.)
glimpse(NewestVUE1)

NewestVUE2 <- NewestVUE1[c(ncol(NewestVUE1), 1:(ncol(NewestVUE1)-1))]
NewestVUE2$Transmitter.Serial <- as.integer(NewestVUE2$Transmitter.Serial)
glimpse(NewestVUE2)

# read in "old" All VUE file generated during previous append -------------

OldVUE <- readRDS("FinalVUE_wDatetime_10182023_ALL.rds")
glimpse(OldVUE)

# make sure everything matches up for rbind -------------------------------

glimpse(OldVUE)
glimpse(NewestVUE2)

AllVUE <- rbind(OldVUE, NewestVUE2)

AllVUE$Datetime_UTC <- as.POSIXct(AllVUE$Datetime_UTC, 
                                  format="%Y-%m-%d %H:%M:%S", tz = "UTC")
glimpse(AllVUE)
# saveRDS(AllVUE, "RDS_files/FinalVUE_wDatetime_01312024_ALL.rds") #becomes OldVUE on next append
rm(list = ls()) #clear env to save memory
FinalVUE <- readRDS("RDS_files/FinalVUE_wDatetime_01312024_ALL.rds")

vue_check <- FinalVUE |> 
      mutate(
            Year = year(Datetime_UTC),
            Month = month(Datetime_UTC),
            YearMonth = paste(Year,Month, sep = "-")
      )

vue_check <- as.data.frame(unique(vue_check$YearMonth))

# read in tag list and station data ---------------------------------------
TagList <- read.csv("Acoustic Tags Master List_06132023update.csv") |> 
      mutate(Transmitter = as.factor(Transmitter))
StationData <- read.csv("Station_Distance_Updated07142020.csv") |> 
      mutate(Station.Name = as.factor(VUE_Name))

# join station, tag, and detection data -----------------------------------

RawVUE <- FinalVUE |> 
      select(-Latitude, -Longitude)
RehageVUE <- left_join(RawVUE, TagList, by = "Transmitter")
RehageVUE_Distance <- left_join(RehageVUE, StationData, by = "Station.Name")

# saveRDS(RehageVUE_Distance, "RDS_files/RehageVUE_Distance_01312024.rds")

# generate period of record files for species -----------------------------

RehageVUE_Distance |> 
      filter(Species == "Snook") |> 
      save_rds("RDS_files/AllSnook_POR_01312024.rds")

RehageVUE_Distance |> 
      filter(Species == "Bass") |> 
      save_rds("RDS_files/AllBass_POR_01312024.rds")

RehageVUE_Distance |> 
      filter(Species == "Redfish") |> 
      save_rds("RDS_files/AllRedfish_POR_01312024.rds")

RehageVUE_Distance |> 
      filter(Species == "Tarpon") |> 
      save_rds("RDS_files/AllTarpon_POR_01312024.rds")