###project: SRFCEA
###author(s): Mack White
###goal(s): append new detections to period of record file
###date(s): April 2024
###note(s): stations misproperly assigned in November corrected in script titled
# "station reassignment_sprin2024.R"

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
librarian::shelf(tidyverse, scales, ggplot2)

snook <- readRDS("database management/2024_Spring/RDS_files/AllSnook_POR_04222024.rds")
bass <- readRDS("database management/2024_Spring/RDS_files/AllBass_POR_04222024.rds")
all <- rbind(snook, bass)
glimpse(all)

all_time <- all |> 
      mutate(
      year = year(Datetime_UTC),
      month = month(Datetime_UTC),
      day = day(Datetime_UTC),
      yearmonth = paste(year,month, sep = "-")
) |> 
      janitor::clean_names()

plot_data <- all_time |> 
      group_by(species, transmitter, year, month, day) |> 
      mutate(avg_dist = mean(distance),
             date = paste(year, month, day, sep = "-"),
             date = as.Date(date))
glimpse(plot_data)

plot_data_recent <- plot_data |> 
      filter(year > 2022)
snook_recent <- plot_data_recent |> 
      filter(species == "Snook")
bass_recent <- plot_data_recent |> 
      filter(species == "Bass")

snook_recent |> 
      ggplot(aes(x = date, y = avg_dist, group = transmitter)) +
      geom_line(alpha = 0.4, color = 'black') +
      theme_classic() +
      labs(x = 'Date', y = 'Distance') +
      facet_wrap(~transmitter) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))

ggsave(
      filename = "snook_distance_plots_2023thruPresent.png",
      path = "plots",
      width = 15, height = 9
)

snook_recent |> 
      filter(year == 2024) |> 
      ggplot(aes(x = date, y = avg_dist, group = transmitter)) +
      geom_line(alpha = 0.4, color = 'black') +
      theme_classic() +
      labs(x = 'Date', y = 'Distance') +
      facet_wrap(~transmitter) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))

ggsave(
      filename = "snook_distance_plots_2024thruPresent.png",
      path = "plots",
      width = 15, height = 9
)

bass_recent |> 
      ggplot(aes(x = date, y = avg_dist, group = transmitter)) +
      geom_line(alpha = 0.4, color = 'black') +
      theme_classic() +
      labs(x = 'Date', y = 'Distance') +
      facet_wrap(~transmitter) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))

ggsave(
      filename = "bass_distance_plots_2023thruPresent.png",
      path = "plots",
      width = 15, height = 9
)

bass_recent |> 
      filter(year == 2024) |> 
      ggplot(aes(x = date, y = avg_dist, group = transmitter)) +
      geom_line(alpha = 0.4, color = 'black') +
      theme_classic() +
      labs(x = 'Date', y = 'Distance') +
      facet_wrap(~transmitter) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))

ggsave(
      filename = "bass_distance_plots_2024thruPresent.png",
      path = "plots",
      width = 15, height = 9
)
