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
      ggplot(aes(x = as.factor(id), y = Sensor.Value, colour = species)) +
      geom_boxplot()

### next steps are to separate the dates and time (keep datetime as well) + 
### add temperature and hydrology data + relative prey abundance + 
### relative snook and bass abundance + combined mesopredator abundance + 
### moon phase/meteorological data (e.g., cloud cover?)
### check these goodies for acceleration studies for treating data, questions, etc:
### would maybe be interesting to see if behaviors similar to some terrestrial predators?

# Kadar, Julianna, Monique Ladds, Johann Mourier, Joanna Day, and Culum Brown. 2019. “Acoustic Accelerometry Reveals Diel Activity Patterns in Premigratory Port Jackson Sharks.” Ecology and Evolution 9 (16): 8933–44.
# Lennox, Robert J., Sindre H. Eldøy, Lotte S. Dahlmo, Jordan K. Matley, and Knut Wiik Vollset. 2023. “Acoustic Accelerometer Transmitters and Their Growing Relevance to Aquatic Science.” Movement Ecology 11 (1): 45.
# Meese, Emily N., and Christopher G. Lowe. 2020. “Active Acoustic Telemetry Tracking and Tri-Axial Accelerometers Reveal Fine-Scale Movement Strategies of a Non-Obligate Ram Ventilator.” Movement Ecology 8 (1): 8.
