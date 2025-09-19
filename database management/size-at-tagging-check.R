###project: SRFCEA
###author(s): Mack White
###goal(s): check on length estimates of tagged fish
###date(s): June 2025
###note(s):

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, scales, ggplot2)

master <- read_csv('database management/core_data/srfcea-acoustic-tag-species-list_MWupdated06182025.csv')
size <- read_csv('database management/core_data/srfcea-size-at-tagging_MWupdated06182025.csv') |> 
      filter(!is.na(Transmitter))

coords <- read_csv('../snook-forage-quality/mapping/efishing_monitoringstation_coords.csv') |> 
      filter(Type == 'Electrofishing') |> 
      select(-Type) |> 
      rename(LocationTagged = `Site Name`)

join <- left_join(master, size) |> group_by(Transmitter) |> mutate(count = n())
join1 <- left_join(join, coords)

formatted <- join1 |>
      rename(Year = year,
             Month = month,
             Day = day) |> 
      select(Transmitter, CodeSpace, TagNumber, Species, TL_cm, SL_cm, Weight_kg, DateTagged, Year, Month, Day, 
             LocationTagged, Latitude, Longitude, everything()) |> 
      select(-length_test, -count)
glimpse(formatted)

# write_csv(formatted, 'database management/core_data/srfcea-size-at-tagging-mw-updated06182025.csv')
