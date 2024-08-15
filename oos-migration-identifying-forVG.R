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
             dayofyear = yday(date),
             id_year = paste(id,year,sep = " "))
glimpse(dt_hydro)

# generate breaks + labels for plotting -----------------------------------
breaks <- yday(as.Date(paste0("2023-", 1:12, "-01")))
labels <- month.name[1:12]

dt_hydro |> 
      filter(id_year =='A69-1303-32892 2014') |> 
      ggplot(aes(x = dayofyear, y = distance)) +
      geom_line(size = 0.5) +
      geom_point(size = 1.5) +
      geom_hline(yintercept = c(15,23), linetype = 'dashed', color = 'red') +
      geom_vline(xintercept = c(91,320), linetype = 'dashed', color = 'black') +
      scale_x_continuous(breaks = breaks, labels = labels) +
      scale_y_continuous(limits = c(0,35), breaks = seq(0,35, by = 5)) +
      theme_classic() +
      labs(title = 'f',
           x = 'date',
           y = 'distance') +
      scale_fill_brewer(palette = "Set1") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))


#######

breaks <- yday(as.Date(paste0("2023-", 1:12, "-01")))
labels <- month.name[1:12]

migration_plot <- function(f) {
      dt_hydro |> 
            filter(id_year == f) |> 
            ggplot(aes(x = dayofyear, y = distance)) +
            geom_line(size = 0.5) +
            geom_point(size = 1.5) +
            geom_hline(yintercept = c(15,23), linetype = 'dashed', color = 'red') +
            geom_vline(xintercept = c(91,320), linetype = 'dashed', color = 'black') +
            scale_x_continuous(breaks = breaks, labels = labels) +
            scale_y_continuous(limits = c(0,35), breaks = seq(0,35, by = 5)) +
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
migration_test <- map(unique(dt_hydro$id_year), migration_plot)

ggsave(
      filename = "oos-paper-sharkriver-movement-plots.pdf",
      # path = "migration_test",
      plot = marrangeGrob(migration_test, nrow = 1, ncol = 1),
      width = 15, height = 9
)
