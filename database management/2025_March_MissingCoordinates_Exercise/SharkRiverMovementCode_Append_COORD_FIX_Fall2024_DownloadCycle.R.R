###project: SRFCEA
###author(s): Mack White
###goal(s): correct missing latitude and longitude information ----
###date(s): March 2025
###note(s):
### conclusions from missing coordinate exploration: 
## FFF & Garbage - there is no information whatsoever that I can track down...
## Stations with missing names - also no information to go off of
## FFF, Garbage, and NAs - don't think we can salvage information - at least not as of march 5 2025
## found some coordinate information from a document that Jordan Massie sent to OTN in December 2022
## generated a new Station Distance document titled, "Station_Distance_Updated03052025_annotated.csv"
## in here, you will see a column for information that has potentially been missing - going to see if it helps :)
## lastly, one of the stations with missing coordinate information is likely from a typo
## Shark River LittleSharkRiverLSR2 should be "Shark River South SRSLittleShark2 for LSR2

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, scales, ggplot2)

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

# read in "old" All VUE file generated during previous append -------------
OldVUE <- readRDS("database management/2024_Fall/FinalVUE_wDatetime_04222024_ALL.rds")
glimpse(OldVUE)

# make sure everything matches up for rbind -------------------------------
glimpse(OldVUE)
glimpse(NewestVUE1)

AllVUE <- rbind(OldVUE, NewestVUE1)
### manually sum old and new number of observations to ensure everything 'adds' up haha
# 42693219+1723869

AllVUE$Datetime_UTC <- as.POSIXct(AllVUE$Datetime_UTC, 
                                  format="%Y-%m-%d %H:%M:%S", tz = "UTC")
glimpse(AllVUE)

### correct typo resulting in missing coordinate data ---
### only need to do this once, then will be fixed
test <- AllVUE |> select(Station.Name) |> distinct() # 97 stations

AllVUE <- AllVUE |> 
      mutate(Station.Name = case_when(
            Station.Name == "Shark River LittleSharkRiverLSR2" ~ "Shark River South SRSLittleShark2",
            TRUE ~ Station.Name
      ))
test <- AllVUE |> select(Station.Name) |> distinct() # 96 observations - fixed

### save this vue dataframe - it will becomes OldVUE on next append to period of record
# saveRDS(AllVUE, "database management/2024_Fall/RDS_files/FinalVUE_wDatetime_12192024_ALL.rds")
# saveRDS(AllVUE, "database management/2024_Fall/RDS_files/FinalVUE_wDatetime_03052025_ALL.rds") # adding will stations

### read in station information and attach coordinates here ---
### checking to ensure clean join below --
AllVUE <- AllVUE |> select(-Latitude, -Longitude)
test <- AllVUE |> select(Station.Name) |> distinct() |> mutate(version = "og")
StationData <- read_csv("database management/core_data/Station_Distance_Updated03052025_annotated.csv") |> 
      select(-march2025_edits) |> 
      rename(Station.Name = VUE_Name)

new_test <- StationData |> select(Station.Name) |> distinct() |> mutate(version1 = "updated")
join <- left_join(test, new_test) # looks like everything, minus FFF, Garbage, and NA is updated

final_with_coords <- left_join(AllVUE, StationData, by = "Station.Name")
nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}
nacheck(final_with_coords)
test <- final_with_coords |> filter(is.na(Latitude))
unique(test$Station.Name) #[1] ""        "Garbage" "FFF"    

### clean environment ---
keep <- c("final_with_coords")
rm(list = setdiff(ls(), keep))

saveRDS(final_with_coords, "database management/2025_March_MissingCoordinates_Exercise/RDS_files/FinalVUE_wDatetime_03052025_ALL.rds") # fixing missing coordinates where possible
final_with_coords <- read_rds("database management/2025_March_MissingCoordinates_Exercise/FinalVUE_wDatetime_03052025_ALL.rds")
# read in tag list and station data ---------------------------------------
TagList <- read_csv("database management/core_data/Acoustic Tags Master List_06132023update.csv") |> 
      mutate(Transmitter = as.factor(Transmitter))
# StationData <- read_csv("database management/core_data/Station_Distance_Updated07142020.csv") |> 
#       mutate(Station.Name = as.factor(VUE_Name))
### updated station list with will receivers ---
StationData <- read_csv("database management/core_data/Station_Distance_Updated03052025_annotated.csv") |> 
      mutate(Station.Name = as.factor(VUE_Name)) |> 
      select(-march2025_edits)

RawVUE <- final_with_coords |> 
      select(-Latitude, -Longitude, -Station, -Distance)
RehageVUE <- left_join(RawVUE, TagList, by = "Transmitter")
test <- RehageVUE |> filter(Transmitter == "A69-1303-51312") #Infamous SnookBass
RehageVUE <- RehageVUE |> 
      mutate(Species = case_when(
            Transmitter == "A69-1303-51312" & Datetime_UTC >= "2014-03-01 00:00:00" ~ "Bass",
            Transmitter == "A69-1303-51312" & Datetime_UTC < "2014-03-01 00:00:00" ~ "Snook",
            TRUE ~ Species
      )) |> 
      distinct()

test <- RehageVUE |> filter(Transmitter == "A69-1303-51312") #SnookBass fixed
glimpse(RehageVUE)

### clean environment ---
keep <- c("RehageVUE", "StationData", "TagList")
rm(list = setdiff(ls(), keep))

RehageVUE_Distance <- left_join(RehageVUE, StationData, by = "Station.Name") |> 
      ### just to be safe - check for duplicate observations
      distinct()

### clean environment ---
keep <- c("RehageVUE_Distance")
rm(list = setdiff(ls(), keep))
glimpse(RehageVUE_Distance)

### keeps all detections on record, but could sort for our tags/make assumptions about identity of others (cough cough bull sharks) if wanted for some other analyses 
# saveRDS(RehageVUE_Distance, "database management/2024_Fall/RDS_files/RehageVUE_All_12192024.rds")
# saveRDS(RehageVUE_Distance, "database management/2024_Fall/RDS_files/RehageVUE_All_03052025.rds")
saveRDS(RehageVUE_Distance, "database management/2025_March_MissingCoordinates_Exercise/RDS_files/RehageVUE_All_03052025.rds")

# generate period of record files for species -----------------------------

### define function for saving out rds
save_rds <- function(data, filename) {
      saveRDS(data, file = filename)
      return(data)
}

RehageVUE_Distance |> 
      filter(Species == "Snook") |> 
      # save_rds("database management/2024_Fall/RDS_files/AllSnook_POR_12192024.rds")
      # save_rds("database management/2024_Fall/RDS_files/AllSnook_POR_03052025.rds")
      saveRDS("database management/2025_March_MissingCoordinates_Exercise/RDS_files/AllSnook_POR_03052025.rds")

RehageVUE_Distance |> 
      filter(Species == "Bass") |> 
      # save_rds("database management/2024_Fall/RDS_files/AllBass_POR_12192024.rds") 
      # save_rds("database management/2024_Fall/RDS_files/AllBass_POR_03052025.rds")
      saveRDS("database management/2025_March_MissingCoordinates_Exercise/RDS_files/AllBass_POR_03052025.rds")

RehageVUE_Distance |> 
      filter(Species == "Redfish") |> 
      # save_rds("database management/2024_Fall/RDS_files/AllRedfish_POR_12192024.rds")
      # save_rds("database management/2024_Fall/RDS_files/AllRedfish_POR_03052025.rds")
      saveRDS("database management/2025_March_MissingCoordinates_Exercise/RDS_files/AllRedfish_POR_03052025.rds")

RehageVUE_Distance |> 
      filter(Species == "Tarpon") |> 
      # save_rds("database management/2024_Fall/RDS_files/AllTarpon_POR_12192024.rds")
      # save_rds("database management/2024_Fall/RDS_files/AllTarpon_POR_03052025.rds")
      saveRDS("database management/2025_March_MissingCoordinates_Exercise/RDS_files/AllTarpon_POR_03052025.rds")

nacheck <- function(df) {
      na_count_per_column <- sapply(df, function(x) sum(is.na(x)))
      print(na_count_per_column)
}

nacheck(RehageVUE_Distance)
old <- read_rds("database management/2024_Fall/RDS_files/RehageVUE_All_03052025.rds") |> 
      filter(is.na(Latitude))

old_summary <- old |> 
      group_by(Station.Name) |> 
      summarize(min(Datetime_UTC),
               max(Datetime_UTC),
               obs = n())

new <- read_rds("database management/2025_March_MissingCoordinates_Exercise/RDS_files/RehageVUE_All_03052025.rds") |> 
      filter(is.na(Latitude))

new_summary <- new |> 
      group_by(Station.Name) |> 
      summarize(min(Datetime_UTC),
                max(Datetime_UTC),
                obs = n())

final <- read_rds("database management/2025_March_MissingCoordinates_Exercise/RDS_files/FinalVUE_wDatetime_03052025_ALL.rds")
final1 <- read_rds("database management/2025_March_MissingCoordinates_Exercise/RDS_files/RehageVUE_All_03052025.rds")
final1 <- final1 |> 
      # select(-VUE_Name) |> 
      janitor::clean_names()
saveRDS(final1, "database management/2025_March_MissingCoordinates_Exercise/RDS_files/FinalVUE_wDatetime_RehageSpecies_03052025_All.rds")
glimpse(final1)
unique(final1$species)

test1 <- test |> 
      filter(is.na(Latitude))
