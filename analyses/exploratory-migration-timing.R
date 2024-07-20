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
             yearmonth = paste(year, month, sep = "-"),
             dayofyear = yday(date))
glimpse(dt_hydro)

# looking at inter-annual timing of multi-yr fishes -----------------------

dt_hydro_filter <- dt_hydro |> 
      group_by(id) |> 
      filter(n_distinct(year) >= 4) %>%
      ungroup()
unique(dt_hydro_filter$id)

# generate breaks + labels for plotting -----------------------------------
breaks <- yday(as.Date(paste0("2023-", 1:12, "-01")))
labels <- month.name[1:12]

migration_plot <- function(f) {
      dt_hydro_filter |> 
            filter(id == f) |> 
            ggplot() +
            geom_line(aes(x = dayofyear, y = distance, color = as.factor(year)), size = 1) +
            scale_x_continuous(breaks = breaks, labels = labels) +
            theme_classic() +
            labs(title = f,
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
      filename = "inter-annual-migration-timing.pdf",
      # path = "migration_test",
      plot = marrangeGrob(migration_test, nrow = 1, ncol = 1),
      width = 15, height = 9
)

# looking at all movement plots -------------------------------------------

migration_plot2 <- function(f) {
      dt_hydro |> 
            filter(id == f) |> 
            ggplot() +
            geom_line(aes(x = date, y = distance, color = as.factor(year)), size = 1) +
            # scale_x_continuous(breaks = breaks, labels = labels) +
            theme_classic() +
            labs(title = f,
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
migration_test2 <- map(unique(dt_hydro$id), migration_plot2)

ggsave(
      filename = "general-movement-plots.pdf",
      # path = "migration_test",
      plot = marrangeGrob(migration_test2, nrow = 1, ncol = 1),
      width = 15, height = 9
)
