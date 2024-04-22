###project: SRFCEA
###author(s): Mack White
###goal(s): append new detections to period of record file
###date(s): April 2024
###note(s): stations misproperly assigned in November corrected in script titled
# "station reassignment_sprin2024.R"

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

NewestVUE <- read_csv("database management/2024_Spring/Spring2024_VUEoffload_STATIONSFIXED.csv")
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

OldVUE <- readRDS("database management/2024_Spring/FinalVUE_wDatetime_01312024_ALL.rds")
glimpse(OldVUE)

# make sure everything matches up for rbind -------------------------------

glimpse(OldVUE)
glimpse(NewestVUE2)

AllVUE <- rbind(OldVUE, NewestVUE2)

AllVUE$Datetime_UTC <- as.POSIXct(AllVUE$Datetime_UTC, 
                                  format="%Y-%m-%d %H:%M:%S", tz = "UTC")
glimpse(AllVUE)
# saveRDS(AllVUE, "database management/2024_Spring/RDS_files/FinalVUE_wDatetime_04222024_ALL.rds") #becomes OldVUE on next append
rm(list = ls()) #clear env to save memory
FinalVUE <- readRDS("database management/2024_Spring/RDS_files/FinalVUE_wDatetime_04222024_ALL.rds")

vue_check <- FinalVUE |> 
      mutate(
            Year = year(Datetime_UTC),
            Month = month(Datetime_UTC),
            YearMonth = paste(Year,Month, sep = "-")
      )

vue_check <- as.data.frame(unique(vue_check$YearMonth))

# read in tag list and station data ---------------------------------------
TagList <- read_csv("database management/2024_Spring/Acoustic Tags Master List_04222024update.csv") |> 
      mutate(Transmitter = as.factor(Transmitter))
StationData <- read_csv("database management/2024_Spring/Station_Distance_Updated07142020.csv") |> 
      mutate(Station.Name = as.factor(VUE_Name))

# join station, tag, and detection data -----------------------------------

RawVUE <- FinalVUE |> 
      select(-Latitude, -Longitude)
RehageVUE <- left_join(RawVUE, TagList, by = "Transmitter")
RehageVUE_Distance <- left_join(RehageVUE, StationData, by = "Station.Name")

### keeps all detections on record, but could sort for our tags/make assumptions about identity of others (cough cough bull sharks) if wanted for some other analyses 
saveRDS(RehageVUE_Distance, "database management/2024_Spring/RDS_files/RehageVUE_All_04222024.rds")

# generate period of record files for species -----------------------------

RehageVUE_Distance |> 
      filter(Species == "Snook") |> 
      save_rds("database management/2024_Spring/RDS_files/AllSnook_POR_04222024.rds")

RehageVUE_Distance |> 
      filter(Species == "Bass") |> 
      save_rds("database management/2024_Spring/RDS_files/AllBass_POR_04222024.rds")

RehageVUE_Distance |> 
      filter(Species == "Redfish") |> 
      save_rds("database management/2024_Spring/RDS_files/AllRedfish_POR_04222024.rds")

RehageVUE_Distance |> 
      filter(Species == "Tarpon") |> 
      save_rds("database management/2024_Spring/RDS_files/AllTarpon_POR_04222024.rds")
