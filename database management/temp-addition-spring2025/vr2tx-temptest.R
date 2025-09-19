###project: SRFCEA
###author(s): Mack White
###goal(s): test out vr2tx temp
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

fall2024 <- read_csv('database management/temp-addition-spring2025/fall2024-srfcea-temp.csv') |> 
      filter(Description == 'Temperature')
spring2025 <- read_csv('database management/temp-addition-spring2025/spring2025-srfcea-temp.csv') |> 
      filter(Description == 'Temperature')

temp <- rbind(fall2024, spring2025)
temp1 <- temp |> 
      mutate(across(c(Receiver, Description), factor),
             Datetime_UTC = as.POSIXct(`Date and Time (UTC)`,
                                       format = "%Y-%m-%d %H:%M:%S")) |> 
      mutate(TempC = as.numeric(Data)) |> 
      select(-`Date and Time (UTC)`, Data) |> 
      select(Datetime_UTC, Receiver, TempC)
glimpse(temp1)

snook <- read_rds('database management/2025_Spring/RDS_files/AllSnook_POR_0612025.rds')
glimpse(snook)
snooktest <- snook |> filter(Receiver == "VR2Tx-484375")
# rm(snook, fall2024, spring2025, temp)

snooktemp <- temp1 |> left_join(snook)
test <- snooktemp |> filter(Receiver == "VR2Tx-484384")
