librarian::shelf(tidyverse, readr, janitor, lubridate, stringr, readxl)

SR_POR <- read_rds("SR_POR_zonesanddepartures_Aug29.rds") #fully filtered
all <- read_rds("snook_compiled_filtered_Aug29.rds") #fully filtered
OOS <- all %>% filter(in_or_out == "out") 


#steps: combine oos with sr por, indicate station distance for oos as -8 or something, and then make movement plots
glimpse(SR_POR)
glimpse(OOS)

SR_tocombine <- SR_POR %>%
  select(datetime_utc, transmitter, receiver, station_name, latitude, longitude, in_or_out, array, year, distance, detection_zone, depart_criteria)

OOS_tocombine <- OOS %>%
  select(datetime_utc, transmitter, receiver, station_name, latitude, longitude, in_or_out, array) %>%
  mutate(year = year(datetime_utc),
         distance = -10,
         detection_zone = "out",
         depart_criteria = "out") %>% 
  mutate(array = as.factor(gsub("Kroetz", "NOSAWR", as.character(array))), #this line renames Kroetz array to NOSAWR
         array = case_when(array == "NOSAWR" & latitude <= 25.277 ~ "NOSAWR_S",
                                     array == "NOSAWR" & latitude > 25.277 ~ "NOSAWR_N",
                                     TRUE ~ as.character(array))) %>% 
  mutate(array = as.factor(array)) #recategorizes NOSAWR to be N or S

glimpse(OOS_tocombine)

#seems to be successful - now need to write these two separate files to .csv / .rds and then bind rows---------

# write.csv(OOS_tocombine, "OOS_tocombine_Aug29.csv")
# write_rds(OOS_tocombine, "OOS_tocombine_Aug29.rds")
# write.csv(SR_tocombine, "SR_tocombine_Aug29.csv")
# write_rds(SR_tocombine, "SR_tocombine_Aug29.rds")

#now to bind rows and write to .csv, .rds----------------------------------------------------------------------

snookPOR_final <- bind_rows(SR_tocombine, OOS_tocombine) #6114547, correct

# write.csv(snookPOR_final, "snookPOR_OOS_final_Aug29.csv")
# write_rds(snookPOR_final, "snookPOR__OOS_final_Aug29.rds")

remove(all, OOS, OOS_tocombine, SR_POR, SR_tocombine)

#after successfully binding rows, need to make the movement plots!----------------------------------------------

#mack suggestion to fix color palette: mutate case when using depart criteria / oos design vs array and then manually
#assign colors to be consistent.

snookPOR_final <- snookPOR_final %>%
  mutate(plot_designation = case_when(depart_criteria == "out" & array == "CELA" ~ "CELA",
                                      depart_criteria == "out" & array == "NOSAWR_N" ~ "NOSAWR_N",
                                      depart_criteria == "out" & array == "NOSAWR_S" ~ "NOSAWR_S",
                                      depart_criteria == "out" & array == "Rookery Bay" ~ "RookeryBay",
                                      depart_criteria == "out" & array == "OTN.BTTFLK" ~ "BTT",
                                      depart_criteria == "out" & array == "OTN.PML" ~ "OTN.PML",#WTF?
                                      depart_criteria == "out" & array == "OTN.QBI" ~ "OTN.QBI",#WTF?
                                      depart_criteria == "out" & array == "OTN.PBSM" ~ "OTN.PBSM",#WTF?
                                      depart_criteria == "out" & array == "OTN.TOUR" ~ "OTN.TOUR",
                                      TRUE ~ depart_criteria))#WTF?

movements <- function(f) {
  snookPOR_final |> #dataset
    filter(transmitter == f) |> #transmitter
    ggplot() +
    geom_line(aes(x = datetime_utc, y = distance), linewidth = 1) +
    # scale_x_continuous(breaks = breaks, labels = labels) +
    geom_point(data = . %>% 
                 filter(plot_designation != "detection") %>% 
                 droplevels(), aes(x = datetime_utc, y = distance, color = plot_designation, shape = plot_designation),
               size = 2, stroke = 1) +
    geom_point(data = . %>% filter(detection_zone == "departure") %>%
                 filter(plot_designation == "detection"), aes(x = datetime_utc, y = distance), color = "black", size = 1) +
    geom_point(data = . %>% filter(detection_zone == "out"), aes(x = datetime_utc, y = distance), color = plot_designation, size = 1) +
    geom_hline(yintercept =15, linetype ="dashed", color ="darkmagenta", linewidth =1) +
    geom_hline(yintercept =23, linetype ="dashed", color ="darkmagenta", linewidth =1) +
    scale_y_continuous(breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30), limits = c(-10, 35)) +
    theme_classic() +
    labs(title = f, #f = ID
         x = 'datetime_utc',
         y = 'distance') +
    scale_color_manual(values = c("orange", "green", "red", "turquoise","blue", "hotpink", "darkorchid", "magenta", "slateblue3", "chocolate4", "chocolate4", "chocolate4", "chocolate4"),
                       breaks = c("dept", "return", "finaldept", "NOSAWR_N", "Rookery Bay", "NOSAWR_S", "CELA", "BTT", "OTN.PML", "OTN.QBI", "OTN.PBSM", "OTN.TOUR")) +
    scale_shape_manual(values = c(1,2,13,15,15,15,15,15,15,15,15,15),
                       breaks = c("dept", "return", "finaldept", "NOSAWR_N", "Rookery Bay", "NOSAWR_S", "CELA", "BTT", "OTN.PML", "OTN.QBI", "OTN.PBSM", "OTN.TOUR")) +
    # scale_color_manual(values = c("blue", "turquoise", "darkorchid", "magenta", "slateblue3"),
    #                    breaks = c("NOSAWR_N", "Rookery Bay", "NOSAWR_S", "CELA", "OTN.BTTFLK")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line("black"),
          axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
}

final_mapped <- map(unique(snookPOR_final$transmitter), movements)

library(gridExtra)

ggsave(
  filename = "SnookOOS_movementplots1_aug29.pdf",
  # path = "migration_test",
  plot = marrangeGrob(final_mapped, nrow = 1, ncol = 1),
  width = 15, height = 9
)

unique(snookPOR_final$detection_zone)

