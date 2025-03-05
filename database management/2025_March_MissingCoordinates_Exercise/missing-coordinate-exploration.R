###project: SRFCEA
###author(s): Mack White
###goal(s): check out missing latitude and longitude information
###date(s): March 2025
###note(s):
### conclusions: 
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

core_station <- read_csv("database management/core_data/Station_Distance_Updated03052025.csv")
all <- read_rds("database management/2024_Fall/RDS_files/FinalVUE_wDatetime_03052025_ALL.rds") 
missing_all <- all |> select(Station.Name, Latitude, Longitude) |> distinct()
rehage <- read_rds("database management/2024_Fall/RDS_files/RehageVUE_All_03052025.rds")
missing_rehage <- rehage |> select(Station.Name, Station, Latitude, Longitude) |> distinct() 

rehage_missing_coords <- rehage |> filter(is.na(Latitude))
all_missing_coords <- all |> filter(is.na(Latitude))

rehage_stations_missing_coords <- rehage_missing_coords |> 
      select(Datetime_UTC, Station.Name, Station, Latitude, Longitude) |> 
      group_by(Station.Name, Station, Latitude, Longitude) |> 
      summarize(mindate = min(Datetime_UTC),
             maxdate = max(Datetime_UTC)) |> 
      distinct() |> 
      ungroup()

all_stations_missing_coords <- all_missing_coords |> 
      select(Datetime_UTC, Station.Name, Latitude, Longitude) |> 
      group_by(Station.Name, Latitude, Longitude) |> 
      summarize(mindate = min(Datetime_UTC),
                maxdate = max(Datetime_UTC)) |> 
      ungroup() |> 
      distinct()

anti <- anti_join(rehage_stations_missing_coords, all_stations_missing_coords)

### clean environment ---
keep <- c("missing_rehage", "missing_all", "core_station", "rehage_missing_coords")
rm(list = setdiff(ls(), keep))

rehage_stations <- missing_rehage |> 
      rename(lat_new = Latitude,
             long_new = Longitude,
             station = Station) |> 
      select(Station.Name, everything()) |> 
      mutate(version = "rehage")

all_stations <- missing_all |> 
      rename(lat_old = Latitude,
             long_old = Longitude) |> 
      select(Station.Name, everything()) |> 
      mutate(version = "all")

join <- full_join(all_stations, rehage_stations)
