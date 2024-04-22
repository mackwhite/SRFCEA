###project: SRFCEA
###author(s): Mack White
###goal(s): re-assign stations not properly assigned in field
###date(s): April 2024
###note(s): a total of four sites not assigned properly during hot swaps on November 10, 2023
# 1) Rookery Branch Headwater Base RBHW3 (VR2W-102073) assigned as Rookery Branch South Base RBS2
# 2) Rookery Branch Headwater Base RBHW2 (VR2W-103523) assigned as Rookery Branch Headwater Base RBHW1
# 3) Rookery Branch East Base RBE2 (VR2W-128957) assigned as Rookery Branch Headwater Base RBHW4
# 4) Rookery Branch East Base RBE1 (VR2W-100856) assigned as Otter Creek OT1
###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, scales, ggplot2)

NewestVUE <- read.csv("database management/2024_Spring/Spring2024_VUEoffload.csv")
glimpse(NewestVUE)
unique(NewestVUE$Station.Name) #35 stations - should be 39

exampleVUE <- read.csv("database management/2023_Fall/NovDec_2023_VUEoffload.csv")

CorrectVUE <- NewestVUE |>
      ### fix station assignment issue number one
      mutate(
            Station.Name = if_else(
                  Station.Name == "Rookery Branch South Base RBS2" & Receiver == "VR2W-102073",
                  "Rookery Branch Headwater Base RBHW3",
                  Station.Name
            )
      ) |> 
      ### fix station assignment issue number two
      mutate(
            Station.Name = if_else(
                  Station.Name == "Rookery Branch Headwater Base RBHW1" & Receiver == "VR2W-103523",
                  "Rookery Branch Headwater Base RBHW2",
                  Station.Name
            )
      ) |> 
      ### fix station assignment issue number three
      mutate(
            Station.Name = if_else(
                  Station.Name == "Rookery Branch Headwater Base RBHW4" & Receiver == "VR2W-128957",
                  "Rookery Branch East Base RBE2",
                  Station.Name
            )
      ) |> 
      ### fix station assignment issue number four
      mutate(
            Station.Name = if_else(
                  Station.Name == "Otter Creek OT1" & Receiver == "VR2W-100856",
                  "Rookery Branch East Base RBE1",
                  Station.Name
            )
      )
unique(CorrectVUE$Station.Name) # we now have 39 stations - this is correct
write_csv(CorrectVUE, "database management/2024_Spring/Spring2024_VUEoffload_STATIONSFIXED.csv")
