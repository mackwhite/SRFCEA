###project: SRFCEA
###author(s): Mack White
###goal(s): append new detections to period of record file
###date(s): December 2024
###note(s):
### had some major issues with VR2TXs this download cycle - many dead on arrival to download
### some notes on receivers that could be important later
### Heithaus receivers with issues
# 484353tx, 484367tx, 484388tx, 137849w
### Rehage receivers with issues
# 484391tx, 484357tx

### notes on 03-05-2025
## added a few extra lines of code to manually add will loaner station observations
## some of the code below will be copied out to maintain integrety of previous/updated version of code

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, scales, ggplot2)

# define custom save_rds function -----------------------------------------

save_rds <- function(data, filename) {
      saveRDS(data, file = filename)
      return(data)
}

# read in vue offload from previous download ------------------------------

NewestVUE <- read_csv("database management/2024_Fall/Fall2024_VUEoffload.csv")
glimpse(NewestVUE)
### checking for will loaner receivers in offload ---
# test <- NewestVUE |> select(Receiver) |> distinct() |> arrange(Receiver)
# test <- NewestVUE |> select(`Station Name`) |> distinct() |> arrange(`Station Name`)
# NewestVUE_stations <- NewestVUE |> select(`Station Name`) |> distinct()
# NewestVUE_receivers <- NewestVUE |> select(Receiver) |> distinct()

### reading in will loaner receivers manually (03/05/2025) ---
srl1 <- read_csv("database management/2024_Fall/will_loaners/VR2W_138649_20241020_1.csv")
dcb4 <- read_csv("database management/2024_Fall/will_loaners/VR2W_138352_20241020_1.csv")
srcn1 <- read_csv("database management/2024_Fall/will_loaners/VR2W_138397_20241019_1.csv")
srsm1 <- read_csv("database management/2024_Fall/will_loaners/VR2W_138368_20241019_1.csv")
srsu1 <- read_csv("database management/2024_Fall/will_loaners/VR2W_138364_20241019_1.csv")
will <- rbind(srl1, dcb4, srcn1, srsm1, srsu1)
glimpse(will) #17031 obs
test <- will |> select(`Station Name`, Receiver) |> distinct()
NewestVUEtest <- rbind(NewestVUE, will)
# 1723869-1706838 = 17031
rm(NewestVUE)
NewestVUE <- NewestVUEtest
rm(NewestVUEtest)
# manipulate data to match format of previous all vue rds file ------------
NewestVUE1 <- NewestVUE |> 
      select(-`Transmitter Type`, -`Sensor Precision`) |> 
      mutate(across(c(Receiver, Transmitter, `Transmitter Name`, 
                      `Sensor Unit`, `Station Name`), factor),
             Datetime_UTC = as.POSIXct(`Date and Time (UTC)`,
                                            format = "%Y-%m-%d %H:%M:%S")) |> 
      select(-`Date and Time (UTC)`) |> 
      ### renaming to join with old vue dataset
      ### this could be a function of using the VR2TXs (?) - but appears all data
      ### made its way over from vue, so not worried about issues between
      ### VR2W and VRT2X info
      rename(
            Transmitter.Name = `Transmitter Name`,
            Transmitter.Serial = `Transmitter Serial`,
            Sensor.Value = `Sensor Value`,
            Sensor.Unit = `Sensor Unit`,
            Station.Name = `Station Name`
      ) |> 
      select(Datetime_UTC, everything()) |> 
      mutate(Transmitter.Serial = as.integer(Transmitter.Serial))
glimpse(NewestVUE1)

### Mack added this to above code chunk for ease
# NewestVUE2 <- NewestVUE1[c(ncol(NewestVUE1), 1:(ncol(NewestVUE1)-1))]
# NewestVUE2$Transmitter.Serial <- as.integer(NewestVUE2$Transmitter.Serial)
# glimpse(NewestVUE2)

# read in "old" All VUE file generated during previous append -------------
OldVUE <- readRDS("database management/2024_Fall/FinalVUE_wDatetime_04222024_ALL.rds")
glimpse(OldVUE)

# make sure everything matches up for rbind -------------------------------
glimpse(OldVUE)
glimpse(NewestVUE2)

AllVUE <- rbind(OldVUE, NewestVUE1)
### manually sum old and new number of observations to ensure everything 'adds' up haha
# 42693219+1723869

AllVUE$Datetime_UTC <- as.POSIXct(AllVUE$Datetime_UTC, 
                                  format="%Y-%m-%d %H:%M:%S", tz = "UTC")
glimpse(AllVUE)

### save this vue dataframe - it will becomes OldVUE on next append to period of record
# saveRDS(AllVUE, "database management/2024_Fall/RDS_files/FinalVUE_wDatetime_12192024_ALL.rds")
saveRDS(AllVUE, "database management/2024_Fall/RDS_files/FinalVUE_wDatetime_03052025_ALL.rds")

rm(list = ls()) #clear env to save memory

# FinalVUE <- readRDS("database management/2024_Fall/RDS_files/FinalVUE_wDatetime_12192024_ALL.rds")
FinalVUE <- readRDS("database management/2024_Fall/RDS_files/FinalVUE_wDatetime_03052025_ALL.rds")
unique(FinalVUE$Station.Name)

### checking integrety in october 2024 - don't need everytime
# FinalVUE2 <- readRDS("database management/2024_Spring/RDS_files/FinalVUE_wDatetime_04222024_ALL.rds")
# unique(FinalVUE2$Station.Name)
# vue_check <- FinalVUE |> 
#       mutate(
#             Year = year(Datetime_UTC),
#             Month = month(Datetime_UTC),
#             YearMonth = paste(Year,Month, sep = "-")
#       )
# 
# vue_check <- as.data.frame(unique(vue_check$YearMonth))
# rm(vue_check)

# read in tag list and station data ---------------------------------------
TagList <- read_csv("database management/core_data/Acoustic Tags Master List_06132023update.csv") |> 
      mutate(Transmitter = as.factor(Transmitter))
# StationData <- read_csv("database management/core_data/Station_Distance_Updated07142020.csv") |> 
#       mutate(Station.Name = as.factor(VUE_Name))
### updated station list with will receivers ---
StationData <- read_csv("database management/core_data/Station_Distance_Updated03052025.csv") |> 
      mutate(Station.Name = as.factor(VUE_Name))

# join station, tag, and detection data -----------------------------------
RawVUE <- FinalVUE |> 
      select(-Latitude, -Longitude)
RehageVUE <- left_join(RawVUE, TagList, by = "Transmitter")

### clean environment ---
keep <- c("RehageVUE", "StationData", "TagList")
rm(list = setdiff(ls(), keep))

RehageVUE_Distance <- left_join(RehageVUE, StationData, by = "Station.Name") |> 
      ### just to be safe - check for duplicate observations
      distinct()

### clean environment ---
keep <- c("RehageVUE_Distance")
rm(list = setdiff(ls(), keep))

### keeps all detections on record, but could sort for our tags/make assumptions about identity of others (cough cough bull sharks) if wanted for some other analyses 
# saveRDS(RehageVUE_Distance, "database management/2024_Fall/RDS_files/RehageVUE_All_12192024.rds")
saveRDS(RehageVUE_Distance, "database management/2024_Fall/RDS_files/RehageVUE_All_03052025.rds")

# generate period of record files for species -----------------------------

### define function for saving out rds
save_rds <- function(data, filename) {
      saveRDS(data, file = filename)
      return(data)
}

RehageVUE_Distance |> 
      filter(Species == "Snook") |> 
      # save_rds("database management/2024_Fall/RDS_files/AllSnook_POR_12192024.rds") 
      save_rds("database management/2024_Fall/RDS_files/AllSnook_POR_03052025.rds")

RehageVUE_Distance |> 
      filter(Species == "Bass") |> 
      # save_rds("database management/2024_Fall/RDS_files/AllBass_POR_12192024.rds") 
      save_rds("database management/2024_Fall/RDS_files/AllBass_POR_03052025.rds")

RehageVUE_Distance |> 
      filter(Species == "Redfish") |> 
      # save_rds("database management/2024_Fall/RDS_files/AllRedfish_POR_12192024.rds")
      save_rds("database management/2024_Fall/RDS_files/AllRedfish_POR_03052025.rds")

RehageVUE_Distance |> 
      filter(Species == "Tarpon") |> 
      # save_rds("database management/2024_Fall/RDS_files/AllTarpon_POR_12192024.rds")
      save_rds("database management/2024_Fall/RDS_files/AllTarpon_POR_03052025.rds")
