snookPOR_final |> #dataset
      filter(transmitter == "A69-1303-32892") |> #transmitter
      ggplot() +
      geom_line(aes(x = datetime_utc, y = distance), linewidth = 1) +
      # scale_x_continuous(breaks = breaks, labels = labels) +
      geom_point(data = . %>% 
                       filter(plot_designation != "detection") %>% 
                       droplevels(), aes(x = datetime_utc, y = distance, color = plot_designation, shape = plot_designation),
                 size = 2, stroke = 1) +
      geom_point(data = . %>% filter(detection_zone == "departure") %>%
                       filter(plot_designation == "detection"), aes(x = datetime_utc, y = distance), color = "black", size = 1) +
      geom_point(data = . %>% filter(detection_zone == "out"), aes(x = datetime_utc, y = distance, color = plot_designation), size = 1) +
      geom_hline(yintercept =15, linetype ="dashed", color ="darkmagenta", linewidth =1) +
      geom_hline(yintercept =23, linetype ="dashed", color ="darkmagenta", linewidth =1) +
      scale_y_continuous(breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30), limits = c(-10, 35)) +
      theme_classic() +
      labs(title = "f", #f = ID
           x = 'datetime_utc',
           y = 'distance') +
      scale_color_manual(values = c("orange", "green", "red", "turquoise","blue", "hotpink", 
                                    "darkorchid", "magenta", "slateblue3", "chocolate4", "chocolate4", 
                                    "chocolate4"),
                         breaks = c("dept", "return", "finaldept", "NOSAWR_N", "Rookery Bay", 
                                    "NOSAWR_S", "CELA", "BTT", "OTN.PML", "OTN.QBI", "OTN.PBSM", 
                                    "OTN.TOUR")) +
      scale_shape_manual(values = c(1,2,13,15,15,15,15,15,15,15,15,15),
                         breaks = c("dept", "return", "finaldept", "NOSAWR_N", 
                                    "Rookery Bay", "NOSAWR_S", "CELA", "BTT", "OTN.PML", 
                                    "OTN.QBI", "OTN.PBSM", "OTN.TOUR")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))

snookPOR_final |> #dataset
      filter(transmitter == "A69-1303-32892") |> 
      ggplot() +
      geom_line(aes(x = datetime_utc, y = distance), linewidth = 1) +
      geom_point(data = snookPOR_final |> 
                       filter(transmitter == "A69-1303-32892") |> 
                       filter(plot_designation != "detection") |> 
                       droplevels(), aes(x = datetime_utc, y = distance, color = plot_designation, 
                                         shape = plot_designation),
                 size = 2, stroke = 1) +
      geom_point(data = snookPOR_final |>  
                       filter(transmitter == "A69-1303-32892") |> 
                       filter(detection_zone == "departure") |> 
                       filter(plot_designation == "detection"), aes(x = datetime_utc, y = distance), 
                 color = "black", size = 1) +
      geom_point(data = snookPOR_final |>
                       filter(transmitter == "A69-1303-32892") |> 
                       filter(detection_zone == "out"), aes(x = datetime_utc, y = distance, 
                                                            color = plot_designation), size = 1) +
      geom_hline(yintercept =15, linetype ="dashed", color ="darkmagenta", linewidth =1) +
      geom_hline(yintercept =23, linetype ="dashed", color ="darkmagenta", linewidth =1) +
      scale_y_continuous(breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30), limits = c(-10, 35)) +
      theme_classic() +
      labs(title = "f", #f = ID
           x = 'datetime_utc',
           y = 'distance') +
      scale_color_manual(values = c("orange", "green", "red", "turquoise","blue", "hotpink", 
                                    "darkorchid", "magenta", "slateblue3", "chocolate4", "chocolate4", 
                                    "chocolate4"),
                         breaks = c("dept", "return", "finaldept", "NOSAWR_N", "Rookery Bay", 
                                    "NOSAWR_S", "CELA", "BTT", "OTN.PML", "OTN.QBI", "OTN.PBSM", 
                                    "OTN.TOUR")) +
      scale_shape_manual(values = c(1,2,13,15,15,15,15,15,15,15,15,15),
                         breaks = c("dept", "return", "finaldept", "NOSAWR_N", 
                                    "Rookery Bay", "NOSAWR_S", "CELA", "BTT", "OTN.PML", 
                                    "OTN.QBI", "OTN.PBSM", "OTN.TOUR")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))

snookPOR_final |> #dataset
      filter(transmitter == "A69-1303-32892") |> 
      ggplot() +
      geom_line(aes(x = datetime_utc, y = distance), linewidth = 1) +
      geom_point(data = snookPOR_final |> 
                       filter(transmitter == "A69-1303-32892") |> 
                       filter(plot_designation != "detection") |> 
                       droplevels(), aes(x = datetime_utc, y = distance, color = plot_designation, 
                                         shape = plot_designation),
                 size = 2, stroke = 1) +
      geom_point(data = snookPOR_final |>  
                       filter(transmitter == "A69-1303-32892") |> 
                       filter(detection_zone == "departure") |> 
                       filter(plot_designation == "detection"), aes(x = datetime_utc, y = distance), 
                 color = "black", size = 1) +
      geom_point(data = snookPOR_final |>
                       filter(transmitter == "A69-1303-32892") |> 
                       filter(detection_zone == "out"), aes(x = datetime_utc, y = distance, 
                                                            color = plot_designation), size = 1) +
      geom_hline(yintercept =15, linetype ="dashed", color ="darkmagenta", linewidth =1) +
      geom_hline(yintercept =23, linetype ="dashed", color ="darkmagenta", linewidth =1) +
      geom_hline(yintercept =0, linetype ="dashed", color ="darkmagenta", linewidth =1) +
      scale_y_continuous(breaks = c(-10, -5, 0, 5, 10, 15, 20, 25, 30), limits = c(-10, 35)) +
      theme_classic() +
      labs(title = "f", #f = ID
           x = 'datetime_utc',
           y = 'distance') +
      scale_color_manual(values = c("orange", "green", "red", "turquoise","blue", "hotpink", 
                                    "darkorchid", "magenta", "slateblue3", "chocolate4", "chocolate4", 
                                    "chocolate4"),
                         breaks = c("dept", "return", "finaldept", "NOSAWR_N", "Rookery Bay", 
                                    "NOSAWR_S", "CELA", "BTT", "OTN.PML", "OTN.QBI", "OTN.PBSM", 
                                    "OTN.TOUR")) +
      scale_shape_manual(values = c(1,2,13,15,15,15,15,15,15,15,15,15),
                         breaks = c("dept", "return", "finaldept", "NOSAWR_N", 
                                    "Rookery Bay", "NOSAWR_S", "CELA", "BTT", "OTN.PML", 
                                    "OTN.QBI", "OTN.PBSM", "OTN.TOUR")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_rect(fill = "white"),
            axis.line = element_line("black"),
            axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold"))
