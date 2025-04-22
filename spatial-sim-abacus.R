librarian::shelf(readr, ggplot2, car, reshape, reshape2, plyr, dplyr,
                 tidyr, visreg, modEvA, gridExtra, AICcmodavg, nlme, mgcv,
                 lme4, splitstackshape, chron, RInSp, boot, cowplot, ggpubr,
                 lsmeans, MixSIAR, hypervolume, truncnorm, tidyverse, alphahull,
                 dplyr, svMisc, writexl, stringr, zoo, rphylopic)

# pre-processing and manipulation -----------------------------------------

tracks <- read_rds("database management/2025_March_MissingCoordinates_Exercise/RDS_files/AllSnook_POR_03052025.rds")
glimpse(tracks)
df <- tracks |>
      mutate(year = year(Datetime_UTC),
             month = month(Datetime_UTC),
             day = day(Datetime_UTC),
             date = make_date(year, month, day),
             id = Transmitter,
             distance = Distance) |> 
      select(-c(Transmitter, Distance)) |> 
      filter(date >= "2010-07-08" & date <= "2023-07-08")
glimpse(df)

p <- ggplot(df, aes(x = date, y = id, color = distance)) +
      geom_point(size = 0.5, alpha = 0.7) +
      scale_color_viridis_c(name = "Distance\nto Gulf of\nMX", option = "C") +
      scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.y = element_blank(),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            legend.position = "right") +
      labs(x = "Date", y = "Transmitter", 
           title = "Snook Detections Over Time",
           subtitle = "Since 2012 | n = 227")

ggsave('figs/spatsim-abacus.pdf', plot = p, 
       units= 'in', height = 8, width = 8)

ggsave('figs/spatsim-abacus.png', plot = p, dpi = 600, 
       units= 'in', height = 8, width = 4)
