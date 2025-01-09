###project: Mesopredator Behavior
###author(s): MW
###goal(s): Relating all data tables
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, scales, ggplot2, suncalc, dataRetrieval)
### read in data ---

dat <- readRDS("database management/2024_Fall/RDS_files/RehageVUE_All_12192024.rds")
glimpse(dat)
unique(dat$Transmitter)

tags <- read_csv("data/accelerometer-tags.csv") |> 
      rename(Transmitter = transmitter) |> 
      mutate(tag = 'accelerometer')

all <- left_join(dat, tags, by = "Transmitter")
acc_fish <- all |> filter(tag == "accelerometer")
unique(acc_fish$Transmitter)

### clean environment ---
keep <- c("acc_fish")
rm(list = setdiff(ls(), keep))

acc <- acc_fish |> 
      select(Datetime_UTC, Receiver, Sensor.Value, Station.Name, species, Station, 
             Distance, Latitude, Longitude, date, id, tl, sl, weight) |> 
      rename(datetime = Datetime_UTC,
             receiver = Receiver,
             acceleration = Sensor.Value, 
             station_name = Station.Name,
             date_tagged = date) |> 
      janitor::clean_names() |> 
      select(datetime, station, species, id, tl, sl, weight, everything())

rm(acc_fish)

acc |> 
      ggplot(aes(x = as.factor(id), y = acceleration, colour = species)) +
      geom_boxplot()

acc <- acc |> filter(id != 5044)

acc |> 
      ggplot(aes(x = as.factor(id), y = acceleration, colour = species)) +
      geom_boxplot()

# binning data ------------------------------------------------------------

acc_binned <- acc |> 
      mutate(hour_block = floor_date(datetime, unit = "hour")) |> 
      group_by(id, hour_block) |>                                     
      summarize(mean_acceleration = mean(acceleration, na.rm = TRUE),
                mean_distance = mean(distance, na.rm = TRUE),
                .groups = "drop") |> 
      rename(datetime = hour_block)

### split up datetime information

acc_time <- acc_binned |> 
      mutate(datetime_eastern = with_tz(datetime, tzone = "America/New_York"),
             date = as.Date(datetime_eastern),
             year = year(datetime_eastern),
             month = month(datetime_eastern),
             day = day(datetime_eastern),
             time = format(datetime_eastern, format = "%H:%M:%S")) |> 
      select(-datetime) |> 
      rename(datetime_est = datetime_eastern) |> 
      select(datetime_est, date, year, month, day, time, everything())

glimpse(acc_time)
summary(acc_time)

### pull, or read in environmental conditions ---
### starting off with sun and moon position/phases/times
lat <- 25.458015
long <- -80.875852

dates <- seq(as.Date("2024-01-13"), as.Date("2024-12-14"), by = "day")

sun_time <- getSunlightTimes(
      date = dates,
      lat = lat,
      lon = long, 
      keep = c('sunrise', 'sunset', 
               'dawn', 'dusk', 
               'nauticalDawn','nauticalDusk'),
      tz = 'EST'
)

acc_time_sun <- left_join(acc_time, sun_time, by = 'date') |> 
      rename(nautical_dawn = nauticalDawn,
             nautical_dusk = nauticalDusk)

glimpse(acc_time_sun)

acc_time_sun_clean <- acc_time_sun |> 
      mutate(across(
            c(sunrise:nautical_dusk),
            ~format(.x, format = "%H:%M:%S"),
            .names = '{.col}_time'
      )) |> 
      select(-c(sunrise:nautical_dusk)) |> 
      rename_with(
            ~gsub("_time$", "", .x),
            starts_with("sunrise_time") | starts_with("nautical_dusk_time")
      )

### read in temperature from bottle creek ---
### Bottle Creek Temp near Rookery Branch, Everglades NPS 
siteNumber <- "022908295" # found here https://waterdata.usgs.gov/monitoring-location/022908295/#parameterCode=00065&period=P7D
parameterCd <- "00010" # parameter codes listed here -> http://water.nv.gov/hearings/past/Spring%20Valley%202006/exhibits/SNWA/5__Hydrochemistry_and_Geochemistry/Data/USGS/USGS_NWIS/ParameterCodes.htm

bc_temperature <- readNWISdv(
      siteNumber, parameterCd,
      "2024-01-13", "2024-12-14"
)

temp <- bc_temperature |> 
      mutate(temp = X_00010_00003) |> 
      rename(date = Date) |> 
      select(date, temp)

glimpse(temp)
summary(temp)

acc_time_sun_clean_temp <- left_join(acc_time_sun_clean, temp, by = 'date')
na_count_per_column <- sapply(acc_time_sun_clean_temp, function(x) sum(is.na(x)))
print(na_count_per_column)

### clean environment ---
acc <- acc_time_sun_clean_temp
keep <- c("acc")
rm(list = setdiff(ls(), keep))

### add marsh stage data

stage <- readxl::read_xlsx('../MAP/data/hydrology/mo215_current.xlsx') |> 
      mutate(date = as.Date(Date)) |> 
      filter(date >= as.Date("2024-01-13") & date <= as.Date("2024-12-14")) |> 
      rename(marsh_stage = `Stage (cm)`) |> 
      select(marsh_stage, date)

acc_stage <- left_join(acc, stage, by = 'date')

### read in salinity data from bottle creek ---
### Bottle Creek Temp near Rookery Branch, Everglades NPS 
siteNumber <- "022908295" # found here https://waterdata.usgs.gov/monitoring-location/022908295/#parameterCode=00065&period=P7D
parameterCd <- "00480" # parameter codes listed here -> http://water.nv.gov/hearings/past/Spring%20Valley%202006/exhibits/SNWA/5__Hydrochemistry_and_Geochemistry/Data/USGS/USGS_NWIS/ParameterCodes.htm

bc_salinity <- readNWISdv(
      siteNumber, parameterCd,
      "2024-01-13", "2024-12-14"
)

salinity <- bc_salinity|> 
      mutate(salinity = X_00480_00003) |> 
      rename(date = Date) |> 
      select(date, salinity)

glimpse(salinity)
summary(salinity)

acc_stage_salinity <- left_join(acc_stage, salinity, by = 'date')
na_count_per_column <- sapply(acc_stage_salinity, function(x) sum(is.na(x)))
print(na_count_per_column)

### clean environment ---
acc <- acc_stage_salinity
keep <- c("acc")
rm(list = setdiff(ls(), keep))
glimpse(acc)

tags <- read_csv("data/accelerometer-tags.csv") |> 
      rename(Transmitter = transmitter,
             date_tagged = date) |> 
      select(id, species, tl, sl, weight, date_tagged)

### join with individual information ---
all <- acc |> left_join(tags, by = "id")

### clean environment ---
keep <- c("all")
rm(list = setdiff(ls(), keep))
glimpse(all)
na_count_per_column <- sapply(all, function(x) sum(is.na(x)))

all1 <- all |> 
      group_by(id, date) |> 
      mutate(sd_acceleration = sd(mean_acceleration))

na_count_per_column <- sapply(all1, function(x) sum(is.na(x)))
print(na_count_per_column)

### save data for future use
write_rds(all1, 'data/binned-accelerometer-model-data-012025.RDS')
