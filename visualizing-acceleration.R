###project: Mesopredator Behavior
###author(s): MW
###goal(s): Visualizing data
###date(s): January 2025
###note(s): 

# Housekeeping ------------------------------------------------------------

### load necessary libraries ---
# install.packages("librarian")
librarian::shelf(tidyverse, scales, ggplot2, suncalc, dataRetrieval)
### read in data ---

acc <- read_rds('data/accelerometer-model-data-012025.RDS') |> 
      select(datetime_est, date, year, month, day, time, id, acceleration, everything()) |> 
      mutate(scaled_acceleration = scale(acceleration, center = TRUE)) |> 
      group_by(id) |> 
      mutate(ind_scaled_acceleration = scale(acceleration, center = TRUE)) |> 
      ungroup() |> 
      group_by(species) |> 
      mutate(species_scaled_acceleration = scale(acceleration, center = TRUE)) |> 
      ungroup()
glimpse(acc)

acc_bin <- read_rds('data/binned-accelerometer-model-data-012025.RDS') |> 
      select(datetime_est, date, year, month, day, time, id, mean_acceleration, sd_acceleration, everything()) |> 
      mutate(scaled_acceleration = scale(mean_acceleration, center = TRUE)) |> 
      group_by(id) |> 
      mutate(ind_scaled_acceleration = scale(mean_acceleration, center = TRUE)) |> 
      ungroup() |> 
      mutate(scaled_sd_acceleration = scale(sd_acceleration, center = TRUE)) |> 
      group_by(id) |> 
      mutate(ind_scaled_sd_acceleration = scale(sd_acceleration, center = TRUE)) |> 
      ungroup() |> 
      group_by(species) |> 
      mutate(species_scaled_acceleration = scale(mean_acceleration, center = TRUE)) |> 
      ungroup() |> 
      group_by(species) |> 
      mutate(species_scaled_sd_acceleration = scale(sd_acceleration, center = TRUE)) |> 
      ungroup()
      
glimpse(acc_bin)
      
# visualize below ---------------------------------------------------------

### temperature effects ---
acc |> 
      ggplot(aes(temp, y =acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc |> 
      ggplot(aes(temp, y =scaled_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc |> 
      ggplot(aes(temp, y =ind_scaled_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc |> 
      ggplot(aes(temp, y =species_scaled_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

### binned data ---

acc_bin |> 
      ggplot(aes(temp, y = mean_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "Mean Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc_bin |> 
      ggplot(aes(temp, y =sd_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "SD Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc_bin |> 
      ggplot(aes(temp, y = scaled_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "Mean Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc_bin |> 
      ggplot(aes(x = temp, y = scaled_sd_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "SD Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc_bin |> 
      ggplot(aes(temp, y = ind_scaled_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "Mean Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc_bin |> 
      ggplot(aes(temp, y = ind_scaled_sd_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "SD Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc_bin |> 
      ggplot(aes(temp, y = species_scaled_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "Mean Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc_bin |> 
      ggplot(aes(temp, y = species_scaled_sd_acceleration, color = species)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "SD Acceleration", color = 'Species') +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

### snook only
acc_bin |> 
      filter(species == 'common snook') |> 
      ggplot(aes(x = temp, y = ind_scaled_acceleration)) +
      # geom_point(size = 1) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = "Acceleration") +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))

acc_bin |> 
      filter(species == 'common snook') |> 
      ggplot(aes(x = temp, y = species_scaled_sd_acceleration)) +
      geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
      labs(x = "Water Temperature (°C)", y = " Scaled SD Acceleration") +
      theme_bw() +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # legend.position = "none",
            legend.position = "right",
            legend.text = element_text(size = 12, color = "black", face = 'bold'),
            legend.title = element_text(size = 12, color = "black", face = 'bold'),
            panel.background = element_rect(fill = "white"),
            strip.background = element_rect(fill = 'white'),
            strip.text = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5))


