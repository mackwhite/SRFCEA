###project: Mesopredator Behavior
###author(s): MW
###goal(s): Running some og models
###date(s): January 2025
###note(s): 

###########################################################################
# Housekeeping ------------------------------------------------------------
###########################################################################

### load necessary libraries
### install.packages("librarian")
# remotes::install_github('m-clark/mixedup')
librarian::shelf(tidyverse, readr, readxl, MuMIn, visreg, mgcv, corrplot, glmmTMB, DHARMa,
                 performance, ggeffects, lsmeans, emmeans, ggpubr)

### read in necessary data---
acc_bin <- read_rds('data/binned-accelerometer-model-data-012025.RDS') |> 
      select(datetime_est, date, year, month, day, time, id, 
             mean_acceleration, sd_acceleration, everything()) |> 
      filter(!is.na(sd_acceleration))

### selected data for models---
mod_df <- acc_bin |> 
      select(year, month, day, time, species, id, tl, mean_acceleration, sd_acceleration, 
             mean_distance, temp, marsh_stage, salinity) |> 
      rename(act = mean_acceleration,
             act_sd = sd_acceleration) |> 
      filter(act_sd >= 1) |> 
      mutate(id = as.factor(id),
             species = as.factor(species))

### clean environment ---
keep <- c("mod_df")
rm(list = setdiff(ls(), keep))
hist(mod_df$act_sd)
mod_df$logsdact <- log(mod_df$act_sd)
hist(mod_df$logsdact)
glimpse(mod_df)
keep <- c("mod_df")
rm(list = setdiff(ls(), keep))

# temperature and hydrology effects on snook activity ---------------------------

snook_df <- mod_df |> 
      rename(stage = marsh_stage) |> 
      filter(species == "common snook")

### null model---
m1 <- mgcv::gam(act_sd ~ 1 + s(id, bs="re"),
                data = snook_df,
                family = gaussian(link = 'log'),
                method = 'REML')

### temperature models---
m2 <- mgcv::gam(act_sd ~ s(temp) + s(id, bs="re"),
                data = snook_df,
                family = gaussian(link = 'log'),
                method = 'REML')

### hydrology models---
m3 <- mgcv::gam(act_sd ~ s(stage) + s(id, bs="re"),
                data = snook_df,
                family = gaussian(link = 'log'),
                method = 'REML')

### hydrology and temperature models---
m4 <- mgcv::gam(act_sd ~ s(temp) + s(stage) + s(id, bs="re"),
                data = snook_df,
                family = gaussian(link = 'log'),
                method = 'REML')

m5 <- mgcv::gam(act_sd ~ s(temp, by = stage) + s(id, bs="re"),
                data = snook_df,
                family = gaussian(link = 'log'),
                method = 'REML')

model_performance <- compare_performance(m1,m2,m3,m4,m5) |> 
      mutate(dAICc = AICc-min(AICc))
summary(m4)

### visualize temperature effects---

temp_vis <- visreg(m4, "temp", type = "conditional", scale = "response")

temp_visfit <- temp_vis$fit |> 
      rename(fit = visregFit,
             fit_lower = visregLwr,
             fit_upper = visregUpr) |> 
      select(fit, fit_lower, fit_upper, temp)

temp_visfit |> 
      ggplot(aes(temp, fit))+
      geom_ribbon(aes(ymin = fit_lower, ymax = fit_upper), fill = "grey60", alpha = 0.3)+
      geom_line(linewidth = 2, color= "black") + theme_bw()+
      labs(x = "Water Temperature (°C)", y = expression(bold("SD Acceleration (m/s"^2*")"))) +
      # # scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
      # ### x axis goes to 80, but constricted to 30 cm in above line of code
      # ### this is reason for wonky figure with shrunken x axis
      # scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
      # ylim(0.3,0.8)+
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
            axis.title = element_text(size = 16, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())

### visualize hydrology effects---

stage_vis <- visreg(m4, "stage", type = "conditional", scale = "response")

stage_visfit <- stage_vis$fit |> 
      rename(fit = visregFit,
             fit_lower = visregLwr,
             fit_upper = visregUpr) |> 
      select(fit, fit_lower, fit_upper, stage)

stage_visfit |> 
      ggplot(aes(stage, fit))+
      geom_ribbon(aes(ymin = fit_lower, ymax = fit_upper), fill = "grey60", alpha = 0.3)+
      geom_line(linewidth = 2, color= "black") + theme_bw()+
      labs(x = "Marsh Stage (cm)", y = expression(bold("Mean Acceleration (m/s"^2*")"))) +
      # # scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
      # ### x axis goes to 80, but constricted to 30 cm in above line of code
      # ### this is reason for wonky figure with shrunken x axis
      # scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
      # ylim(0.3,0.8)+
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
            axis.title = element_text(size = 16, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())

# temperature and hydrology effects on bass act_sdivity ---------------------------

bass_df <- mod_df |> 
      rename(stage = marsh_stage) |> 
      filter(species == "largemouth bass")

### null model---
m1 <- mgcv::gam(act_sd ~ 1 + s(id, bs="re"),
                data = bass_df,
                family = gaussian(link = 'log'),
                method = 'REML')

### temperature models---
m2 <- mgcv::gam(act_sd ~ s(temp) + s(id, bs="re"),
                data = bass_df,
                family = gaussian(link = 'log'),
                method = 'REML')

### hydrology models---
m3 <- mgcv::gam(act_sd ~ s(stage) + s(id, bs="re"),
                data = bass_df,
                family = gaussian(link = 'log'),
                method = 'REML')

### hydrology and temperature models---
m4 <- mgcv::gam(act_sd ~ s(temp) + s(stage) + s(id, bs="re"),
                data = bass_df,
                family = gaussian(link = 'log'),
                method = 'REML')

m5 <- mgcv::gam(act_sd ~ s(temp, by = stage) + s(id, bs="re"),
                data = bass_df,
                family = gaussian(link = 'log'),
                method = 'REML')

model_performance <- compare_performance(m1,m2,m3,m4,m5) |> 
      mutate(dAICc = AICc-min(AICc))
summary(m4)

### visualize temperature effects---

temp_vis <- visreg(m4, "temp", type = "conditional", scale = "response")

temp_visfit <- temp_vis$fit |> 
      rename(fit = visregFit,
             fit_lower = visregLwr,
             fit_upper = visregUpr) |> 
      select(fit, fit_lower, fit_upper, temp)

temp_visfit |> 
      ggplot(aes(temp, fit))+
      geom_ribbon(aes(ymin = fit_lower, ymax = fit_upper), fill = "grey60", alpha = 0.3)+
      geom_line(linewidth = 2, color= "black") + theme_bw()+
      labs(x = "Water Temperature (°C)", y = expression(bold("Mean Acceleration (m/s"^2*")"))) +
      # # scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
      # ### x axis goes to 80, but constricted to 30 cm in above line of code
      # ### this is reason for wonky figure with shrunken x axis
      # scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
      # ylim(0.3,0.8)+
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
            axis.title = element_text(size = 16, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())

### visualize hydrology effects---

stage_vis <- visreg(m4, "stage", type = "conditional", scale = "response")

stage_visfit <- stage_vis$fit |> 
      rename(fit = visregFit,
             fit_lower = visregLwr,
             fit_upper = visregUpr) |> 
      select(fit, fit_lower, fit_upper, stage)

stage_visfit |> 
      ggplot(aes(stage, fit))+
      geom_ribbon(aes(ymin = fit_lower, ymax = fit_upper), fill = "grey60", alpha = 0.3)+
      geom_line(linewidth = 2, color= "black") + theme_bw()+
      labs(x = "Marsh Stage (cm)", y = expression(bold("Mean Acceleration (m/s"^2*")"))) +
      # # scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
      # ### x axis goes to 80, but constricted to 30 cm in above line of code
      # ### this is reason for wonky figure with shrunken x axis
      # scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
      # ylim(0.3,0.8)+
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
            axis.title = element_text(size = 16, face = "bold", colour = "black"),
            plot.title = element_text(size = 16, face = "bold", colour = "black"),
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
