# Housekeeping ------------------------------------------------------------
### install necessary packages ---
librarian::shelf(tidyverse, readr, janitor, zoo, 
                 lubridate, openintro, maps, ggmap,
                 ggthemes, shapefiles, broom, sf, ggrepel)

### set theme ---
theme_set(theme_minimal())

### set up google map key ---
api_secret <- 'AIzaSyB_Q-Ow1klpX9jblm7M2614k5KCVYUXTZM'
register_google(key = api_secret)
has_google_key()

### read in necessary data ---
dat <- read_csv("data/current_array_config_2024.csv") |> 
      janitor::clean_names()

florida_map2 <- get_map(
      ### determine bounding box: https://www.openstreetmap.org/#map=5/25.304/-69.412
      c(left = -81.27, bottom = 25.30, right = -80.75, top = 25.52),     
      maptype = 'satellite',
      source = 'google',
      api_key = api_secret
)

ggmap(florida_map2) +
      geom_jitter(data = dat,
                  aes(x = longitude, y = latitude),
                  size = 2.5,
                  color = "white",
                  position = 'jitter') +
      theme_map() + 
      scale_y_continuous(limits =c(25.30,25.47)) +
      geom_text_repel(data = dat,
                      aes(x = longitude, y = latitude, label = name),
                      color = "white", 
                      size = 2, 
                      fontface = "bold",
                      # nudge_y = 0.01, # Optional to manually adjust y position
                      # nudge_x = 0.01, # Optional to manually adjust x position
                      box.padding = 0.35,  # Add space between labels
                      point.padding = 0.30, # Add space between labels and points
                      segment.color = 'grey', # Line connecting label to point
                      segment.size = 0.4) +  # Size of the line segment
      annotation_north_arrow(location = 'tl',
                             style = north_arrow_fancy_orienteering(text_col = 'white',
                                                                    fill = 'white',
                                                                    line_col = 'white',
                                                                    text_face = "bold",
                                                                    text_size = 18)) +
      annotation_scale(location = 'bl', width_hint = 0.5, text_cex = 1.25,
                       text_face = "bold", text_col = 'white') +
      coord_sf(crs = 4326) +
      theme_map() +
      theme(legend.background = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(face = 'bold',
                                       color = 'white',
                                       size = 16),
            legend.position = c(0.001, 0.275))

### save for publication
ggsave("mapping/current-array-setup.tiff", units = "in", width = 12,
       height = 12, dpi =  600, compression = "lzw")
