librarian::shelf(tidyverse, scales, ggplot2)

dat <- readRDS("database management/2024_Fall/RDS_files/RehageVUE_All_12192024.rds")
glimpse(dat)
unique(dat$Transmitter)

acc <- read_csv("data/accelerometer_list.csv") |> 
      rename(Transmitter = transmitter) |> 
      mutate(tag = 'accelerometer')

all <- left_join(dat, acc, by = "Transmitter")
accelerometer <- all |> filter(tag == "accelerometer")
unique(accelerometer$Transmitter)

accelerometer |> 
      ggplot(aes(x = as.factor(id), y = Sensor.Value)) +
      geom_boxplot() +
      facet_wrap(~species)
