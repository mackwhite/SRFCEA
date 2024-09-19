### Project: Snook Movement Syndromes
### Goal(s): build a map with google satellite imagery
### Author(s): Mack White
### Date(s): Summer 2024
### Notes:

# Housekeeping ------------------------------------------------------------
### install necessary packages ---
librarian::shelf(tidyverse, readr, janitor, zoo, 
                 lubridate, openintro, maps, ggmap,
                 ggthemes, shapefiles, broom, sf, ggspatial, 
                 GISTools)

### set theme ---
theme_set(theme_minimal())

### set up google map key ---
api_secret <- 'AIzaSyB_Q-Ow1klpX9jblm7M2614k5KCVYUXTZM'
register_google(key = api_secret)
has_google_key()

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

joins <- data.frame(
      array = c("CELA", "Kroetz", "NOSAWR", "OTN.BTTFLK", "Rookery Bay", "SRFCEA"),
      new = c("CELA", "NOSAWRA", "NOSAWRA", "BTT", "RBNERRA", "SRFCEA")
)

btt_wreck <- data.frame(
      station_name = "btt_wreck",
      latitude = 25.6689,
      longitude = -81.4278,
      array = "BTT_wreck",
      new = "BTTA"
)

plot_dt <- dat |> 
      left_join(joins, by = "array") |> 
      rbind(btt_wreck) |> 
      mutate(new = case_when(new == 'NOSAWRA' &  latitude >= 25.6 ~ 'NOSAWRA-N',
                             new == 'NOSAWRA' &  latitude < 25.6 ~ 'NOSAWRA-S',
                             new == 'BTTA' & latitude >= 25.6 ~ 'BTT-N',
                             new == 'BTTA' & latitude < 25.6 ~ 'BTT-S',
                             TRUE ~ new))
      

florida_map2 <- get_map(
      ### determine bounding box: https://www.openstreetmap.org/#map=5/25.304/-69.412
      c(left = -81.85, bottom = 25.0, right = -80.0, top = 26.0),
      maptype = 'satellite',
      source = 'google',
      api_key = api_secret
)

ggmap(florida_map2) +
      geom_jitter(data = plot_dt,
                 aes(x = longitude, y = latitude),
                 size = 2.5,
                 color = "black",
                 position = 'jitter') +
      geom_jitter(data = plot_dt,
                 aes(x = longitude, y = latitude, color = new),
                 size = 2.0,
                 alpha = 0.9,
                 position = 'jitter') +
      theme_map() + 
      scale_y_continuous(limits =c(25.0,26.0)) +
      # scale_x_continuous(limits = c(-81.85, -80.6)) +
      # scale_x_continuous(limits = c(-81.17,-80.81)) +
      # geom_text(data = zone_points,
      #           aes(x = mean_lon, y = mean_lat, label = zone),
      #           color = "white", size = 6, fontface = "bold",
      #           hjust = 0.5, vjust = 0.5) +
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
# ggsave("mapping/test_map.tiff", units = "in", width = 12,
#        height = 6, dpi =  600, compression = "lzw")
