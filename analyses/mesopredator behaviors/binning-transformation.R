###project: Mesopredationb Behavior
###author(s): MW
###goal(s): Binning data in 24 hr bins
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, scales, ggplot2, suncalc, dataRetrieval)
### read in data ---

acc <- read_rds('data/accelerometer-model-data-012025.RDS')

acc_binned <- acc |> 
      mutate(hour_block = floor_date(datetime_est, unit = "hour")) |> 
      group_by(id, hour_block) |>                                     
      summarize(mean_acceleration = mean(acceleration, na.rm = TRUE),
                mean_distance = mean(distance, na.rm = TRUE),
                .groups = "drop")
