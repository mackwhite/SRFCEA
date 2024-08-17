### Project: Shark River Out of System Movements
### Goal(s): build a map for victoria
### Author(s): Mack White
### Date(s): Summer 2024
### Notes:

# Housekeeping ------------------------------------------------------------
### install necessary packages ---
install.packages("tmap", repos = c("https://r-tmap.r-universe.dev",
                                   "https://cloud.r-project.org"))
librarian::shelf(tidyverse, readr, janitor, zoo, 
                 lubridate, openintro, maps, ggmap,
                 ggthemes)
### set theme ---
theme_set(theme_minimal())

### set up stadia maps key ---
register_stadiamaps('4d350dd1-f4d4-471f-abb2-8e5f806c05eb', write = FALSE)
stadiamaps_key()
has_stadiamaps_key()

### read in necessary data ---
dat <- read_csv("analyses/oos_vg/snook_compiled_filtered_Aug14.csv") |> 
      select(-...1) |> 
      select(station_name, latitude, longitude, array) |> 
      distinct() |> 
      filter(latitude <= 30)

array_coords <- dat |> 
      group_by(array) |> 
      summarize(latitude = mean(latitude),
                longitude = mean(longitude))

florida_map <- get_stadiamap(
      bbox = c(left = -81.85, bottom = 25.0, right = -80.0, top = 26.0),
      maptype = 'stamen_terrain',
      zoom = 10
)

ggmap(florida_map) +
      # geom_point(data = array_coords,
      #            aes(x = longitude, y = latitude),
      #            size = 3) +
      geom_point(data = dat,
                 aes(x = longitude, y = latitude, color = array),
                 size = 1.5) +
      # geom_text(data = array_coords, 
      #           aes(x = longitude, y = latitude, 
      #               label = array), 
      #           vjust = -1, size = 2.75, 
      #           color = "black", fontface = "bold") +
      theme_map()

ggmap(florida_map) +
      geom_point(data = dat,
                 aes(x = longitude, y = latitude),
                 size = 1.5,
                 color = "black") +
      geom_point(data = dat,
                 aes(x = longitude, y = latitude, color = array),
                 size = 1.0) +
      theme_map() + 
      theme(legend.background = element_blank(),
            legend.title = element_text(face = "bold", size = 14, hjust = 0.5),
            legend.text = element_text(face = "bold"), size = 12)

### save for publication
# ggsave("analyses/oos_vg/drafted_map.tiff", units = "in", width = 12,
#        height = 6, dpi =  600, compression = "lzw")

      
