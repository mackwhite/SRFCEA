### Project: Shark River Movement Syndromes
### Goal(s): add tagger size information and make data pseudo-continuous
### Author(s): Mack White
### Date(s): Summer 2024
### Notes:

# Housekeeping ------------------------------------------------------------
###install necessary packages

# install.packages("librarian")
librarian::shelf(tidyverse, readr, janitor, zoo, summarytools)

df <- read_csv("data/archive/oos_final_datatable_VG_Sept4.csv")
view(dfSummary(df, method = "viewer"))
glimpse(df)

na <- df |> filter(is.na(DateTagged))
na_save <- na |> 
      select(transmitter, DateTagged, min_det) |> distinct()

# write_csv(na_save, "data/archive/na_size_save.csv")
fixed_nas <- read_csv("data/archive/na_size_save_fixed.csv") |> 
      select(transmitter, DateTagged, SL_cm) |> 
      mutate(DateTagged = case_when(
            DateTagged == "5/14/2021" ~ "12/10/2023", 
            TRUE ~ DateTagged
      ))

df1 <- df |> left_join(fixed_nas, by = "transmitter") |> 
      mutate(DateTagged = case_when(
            is.na(DateTagged.x) ~ DateTagged.y,
            TRUE ~ DateTagged.x
      )) |> 
      mutate(SL_cm = case_when(
            is.na(SL_cm.x) ~ SL_cm.y,
            TRUE ~ SL_cm.x
      )) |> 
      select(-DateTagged.x, -DateTagged.y, -SL_cm.x, -SL_cm.y)

na <- df1 |> filter(is.na(SL_cm))
na <- df1 |> filter(is.na(DateTagged))


# L_asymp_high <- 947.3 + 32.15
L_asymp <- 947.3
# L_asymp_low <- 947.3 - 32.15
# K_high <- 0.175 + 0.0155
K <- 0.175
# K_low <- 0.175 - 0.0155
# t0_high <- -1.352 + 0.1714
t0 <- -1.352
# t0_low <- -1.352 - 0.1714

df_dttm <- df1 |> 
      ### get rid of ID column
      select(-...1) |> 
      ### format dttm columns correctly
      mutate(dept = as.POSIXct(dept, format = "%m/%d/%Y %H:%M"),
             return = as.POSIXct(return, format = "%m/%d/%Y %H:%M"),
             min_det = as.POSIXct(min_det, format = "%m/%d/%Y %H:%M"),
             max_det = as.POSIXct(max_det, format = "%m/%d/%Y %H:%M"),
             DateTagged = as.POSIXct(DateTagged, format = "%m/%d/%Y %H:%M")) |> 
      ### SL_cm to fl_mm conversion (Y = a + b*X)
      mutate(sl_mm = SL_cm*10,
             fl_mm = 20.0876 + 1.0630 * sl_mm)
glimpse(df_dttm)

gas <- df_dttm |> 
      mutate(age = t0 - (1 / K) * log(1 - fl_mm / L_asymp))|> 
      mutate(age_days = age*365.25,
             id = row_number()) |> 
      mutate(time_diff_years = as.numeric(difftime(dept, min_det, units = "days")) / 365.25,
             length_at_dept_fl_mm = L_asymp * (1 - exp(-K * (time_diff_years + age - t0)))) |> 
      mutate(length_at_dept_sl_mm = -14.7684 + 0.9338 * length_at_dept_fl_mm,
             length_at_dept_sl_cm = length_at_dept_sl_mm/10) |> 
      select(-sl_mm, -fl_mm, -age_days, -id, -length_at_dept_sl_mm, -length_at_dept_fl_mm) |> 
      separate(transmitter, into = c("code1", "code2", "id"), sep = "-", remove = FALSE) |> 
      select(-code1, -code2)
      
write_csv(gas, "data/thatgassy_gas_jan22_2025.csv")
