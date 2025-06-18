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

master <- read_csv('database management/core_data/Acoustic Tags Master List_062025_update.csv')
size <- read_csv('database management/core_data/acoustic-tags-snook-and-bass-size-thru062025-MW.csv') |> 
      filter(!is.na(Transmitter))

coords <- read_csv('../snook-forage-quality/mapping/efishing_monitoringstation_coords.csv') |> 
      filter(Type == 'Electrofishing') |> 
      select(-Type) |> 
      rename(LocationTagged = `Site Name`)

join <- left_join(master, size) |> group_by(Transmitter) |> mutate(count = n())
join1 <- left_join(join, coords)

formatted <- join1 |>
      mutate(
            DateTagged= mdy(DateTagged),  # Convert to Date format
            year = year(DateTagged),
            month = month(DateTagged),
            day = day(DateTagged)
      ) |> 
      rename(ID_Code = ID.Code) |> 
      ungroup()
glimpse(formatted)

write_csv(formatted, 'database management/core_data/srfcea-size-at-tagging-mw-updated06182025.csv')
