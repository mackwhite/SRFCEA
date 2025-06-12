###project: SRFCEA
###author(s): Mack White
###goal(s): append new detections to period of record file
###date(s): June 2025
###note(s):

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, scales, ggplot2)

### define custom functions ---
save_rds <- function(data, filename) {
      saveRDS(data, file = filename)
      return(data)
}

nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

### read in core data ----
### don't forget to update tag list every so often :) 
TagList <- read_csv("database management/core_data/Acoustic Tags Master List_04222025_update.csv") |> 
      mutate(Transmitter = as.factor(Transmitter))
### should never be updated... unless you add stations
StationData <- read_csv("database management/core_data/Station_Distance_Updated03052025_annotated.csv") |> 
      mutate(Station.Name = as.factor(VUE_Name))

### read in VUE offload of most recent download cycle ---
NewestVUE <- read_csv("database management/2025_Spring/Spring2025_VUEoffload.csv")
glimpse(NewestVUE)

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
      mutate(Transmitter.Serial = as.integer(Transmitter.Serial)) |> 
      select(-Latitude, -Longitude)
glimpse(NewestVUE1)

# read in "old" All VUE file generated during previous append -------------
OldVUE <- readRDS("database management/2025_March_MissingCoordinates_Exercise/RDS_files/FinalVUE_wDatetime_03052025_ALL.rds")
glimpse(OldVUE)
OldVUE1 <- OldVUE |> select(-Station, -Distance, -Latitude, -Longitude)
# make sure everything matches up for rbind -------------------------------
glimpse(OldVUE)
glimpse(NewestVUE1)
rm(NewestVUE, OldVUE)

AllVUE <- rbind(OldVUE1, NewestVUE1)
### manually sum old and new number of observations to ensure everything 'adds' up
44417088+952252
rm(OldVUE1,NewestVUE1)
glimpse(AllVUE)

AllVUE$Datetime_UTC <- as.POSIXct(AllVUE$Datetime_UTC, 
                                  format="%Y-%m-%d %H:%M:%S", tz = "UTC")
glimpse(AllVUE)

AllVUEwCoords <- AllVUE |> left_join(StationData)
nacheck(AllVUEwCoords) #204746 NAs in most columns - NAs due to missing spatial information for several 'sites' (e.g., 'fff', 'garbage', 'NA') and normal - Should be 0 NAs for datetime, receiver, transmitter, and station.name
rm(AllVUE)

### save this - it will becomes OldVUE on next append to period of record
# saveRDS(AllVUEwCoords, "database management/2025_Spring/RDS_files/FinalVUE_wDatetime_05122025_ALL.rds")
# AllVUEwCoords <- readRDS("database management/2025_Spring/RDS_files/FinalVUE_wDatetime_05122025_ALL.rds")

# join station, tag, and detection data -----------------------------------
RehageVUE_Distance <- AllVUEwCoords |> 
      left_join(TagList, by = "Transmitter") 

rm(AllVUEwCoords, TagList)

RehageVUE_Distance <- RehageVUE_Distance |> 
      ### freakin snook-bass
      mutate(Species = case_when(
            Transmitter == "A69-1303-51312" & Datetime_UTC >= "2014-03-01 00:00:00" ~ "Bass",
            Transmitter == "A69-1303-51312" & Datetime_UTC < "2014-03-01 00:00:00" ~ "Snook",
            TRUE ~ Species
      )) |> 
      distinct()

### clean environment ---
keep <- c("RehageVUE_Distance")
rm(list = setdiff(ls(), keep))


# generate period of record files for species -----------------------------
### define function for saving out rds - had to ditch above to keep R from crashing
save_rds <- function(data, filename) {
      saveRDS(data, file = filename)
      return(data)
}

RehageVUE_Distance |> 
      filter(Species == "Snook") |> 
      save_rds("database management/2025_Spring/RDS_files/AllSnook_POR_0612025.rds")

RehageVUE_Distance |> 
      filter(Species == "Bass") |> 
      save_rds("database management/2025_Spring/RDS_files/AllBass_POR_0612025.rds")

RehageVUE_Distance |> 
      filter(Species == "Redfish") |> 
      save_rds("database management/2025_Spring/RDS_files/AllRedfish_POR_0612025.rds")

RehageVUE_Distance |> 
      filter(Species == "Tarpon") |> 
      save_rds("database management/2025_Spring/RDS_files/AllTarpon_POR_0612025.rds")

RehageVUE_Distance |> 
      save_rds("database management/2025_Spring/RDS_files/RehageVUE_All_06122025.rds")