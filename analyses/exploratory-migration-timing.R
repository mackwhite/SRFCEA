librarian::shelf(tidyverse, readxl, gridExtra, glmmTMB, MuMIn, sjPlot, lme4, corrplot, performance, ggeffects, ggpubr, parameters)

dt <- read_rds("data/snook_cnd_movement_clean.rds")
glimpse(dt)

hydro <- read_csv("data/ssr_waterlevels.csv") |> 
      mutate(date = mdy(date)) |> 
      select(-stage_feet)
glimpse(hydro)

dt_hydro <- left_join(dt, hydro, by = "date") |> 
      mutate(date = as.Date(date),
             year = year(date),
             month = month(date),
             dayofyear = yday(date))
glimpse(dt_hydro)

dt_hydro_filter <- dt_hydro |> 
      group_by(id) |> 
      filter(n_distinct(year) >= 4) %>%
      ungroup()
unique(dt_hydro_filter$id)

# generate breaks + labels for plotting -----------------------------------
breaks <- yday(as.Date(paste0("2023-", 1:12, "-01")))
labels <- month.name[1:12]


dt_hydro |> 
      # group_by(id, date, distance) |> 
      # mutate(mean_dist = mean(distance)) |> 
      filter(id == "A69-1303-32892") |> 
      ggplot() +
      geom_line(aes(x = dayofyear, y = distance, color = as.factor(year)), size = 1) +
      scale_x_continuous(breaks = breaks, labels = labels) +
      theme_classic() +
      labs(title = "A69-1303-32892",
           x = 'date',
           y = 'distance') +
      scale_fill_brewer(palette = "Set1") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))

migration_plot <- function(f) {
      dt_hydro_filter |> 
            filter(id == f) |> 
            ggplot() +
            geom_line(aes(x = dayofyear, y = distance, color = as.factor(year)), size = 1) +
            scale_x_continuous(breaks = breaks, labels = labels) +
            theme_classic() +
            labs(title = "A69-1303-32892",
                 x = 'date',
                 y = 'distance') +
            scale_fill_brewer(palette = "Set1") + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.background = element_rect(fill = "white"),
                  axis.line = element_line("black"),
                  axis.text = element_text(face = "bold"),
                  axis.title = element_text(face = "bold"))
}

# Generate boxplot figures for each projecthabitat
migration_test <- map(unique(dt_hydro_filter$id), migration_plot)

ggsave(
      filename = "migration-timing-test.pdf",
      # path = "migration_test",
      plot = marrangeGrob(migration_test, nrow = 1, ncol = 1),
      width = 15, height = 9
)
