###project: SRFCEA
###author(s): Mack White
###goal(s): correct missing latitude and longitude information ----
###date(s): June 2025
###note(s):

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
NewestVUE <- read_csv("database management/2025_Spring/Spring2025_VUEoffload.csv")
glimpse(NewestVUE)

acc_test <- NewestVUE |> filter(!is.na(`Sensor Value`)) |> filter(`Date and Time (UTC)` >= "2025-02-03 12:51:11.00")
### three fish with acceleration detection histories in spring of 2025
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
            Station.Name = `Station Name`,
      ) |> 
      select(Datetime_UTC, everything()) |> 
      mutate(Transmitter.Serial = as.integer(Transmitter.Serial),
             Station = NA_character_,
             Distance = NA_real_) |> 
      select(Datetime_UTC, Receiver, Transmitter, Transmitter.Name, Transmitter.Serial,
             Sensor.Value, Sensor.Unit, Station.Name, Station, Distance, Latitude, Longitude)
glimpse(NewestVUE1)

# read in "old" All VUE file generated during previous append -------------
OldVUE <- readRDS("database management/2025_March_MissingCoordinates_Exercise/RDS_files/FinalVUE_wDatetime_03052025_ALL.rds")
glimpse(OldVUE)

# make sure everything matches up for rbind -------------------------------
glimpse(OldVUE)
glimpse(NewestVUE1)

AllVUE <- rbind(OldVUE, NewestVUE1)
### manually sum old and new number of observations to ensure everything 'adds' up haha
# 44417088 + 952252

AllVUE$Datetime_UTC <- as.POSIXct(AllVUE$Datetime_UTC, 
                                  format="%Y-%m-%d %H:%M:%S", tz = "UTC")
glimpse(AllVUE)

### save this vue dataframe - it will becomes OldVUE on next append to period of record
saveRDS(AllVUE, "database management/2024_Fall/RDS_files/FinalVUE_wDatetime_06112025_ALL.rds")

### read in station information and attach coordinates here ---
### checking to ensure clean join below --

AllVUE <- AllVUE |> select(-Latitude, -Longitude, -Station, -Distance)
StationData <- read_csv("database management/core_data/Station_Distance_Updated03052025_annotated.csv") |> 
      select(-march2025_edits) |> 
      rename(Station.Name = VUE_Name)

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
TagList <- read_csv("database management/core_data/Acoustic Tags Master List_04222025_update.csv") |> 
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

test <- readRDS("database management/2025_March_MissingCoordinates_Exercise/RDS_files/FinalVUE_wDatetime_RehageSpecies_03052025_All.rds")
filter <- test |> filter(transmitter == "A69-1303-58413")
glimpse(test)

write_csv(test, 'data/oos/snook_1303_58413_sharkriver_05072025.csv')
