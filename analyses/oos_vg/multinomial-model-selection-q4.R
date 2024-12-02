### Project: Shark River Out of System Movement MS
### Goal(s): exploring multinomial model potential for Q4 
### Author(s): MW & VG
### Date(s): December 2024
### Note(s): have three different response metrics interested in - lets model with 
## multinomial model potentially

# Housekeeping ------------------------------------------------------------
###install necessary packages ---
# install.packages("librarian")
librarian::shelf(tidyverse, readr, janitor, zoo, summarytools, dataRetrieval, 
                 corrplot, nnet, broom, gtsummary, ggeffects, marginaleffects,
                 GGally, car, knitr, kableExtra, performance, MuMIn)
# parallel::detectCores()

# read in necessary data --------------------------------------------------
### load in model data and format for multinomial models ----
df <- read_csv("../../../../../../Downloads/q4data.csv")
glimpse(df)

df1 <- df |> 
      ### updating categories 
      mutate(
            category1 = as.factor(category),
            ### explicitly map old levels to new ones
            category2 = factor(category1,
                               levels = c("Relocator", "Coastal Emigrator", "Estuarine Returner"))
            ) |> 
      ### pulling out single negative value and making positive
      mutate(
            time_in_SR = abs(time_in_SR)
      ) |> 
      ### pulling out thirteen rows of data where NAs exist
      na.omit()

glimpse(df1)
unique(df1$category2)

### explore multicollinearity ----
numeric_data <- df1 |> 
      ### filter for only numeric data
      select(size, time_in_SR, time_outside)

cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "number")

# fit full model ----------------------------------------------------------
mod <- nnet::multinom(category2 ~ year + depart_period + return_period + time_outside + 
                            time_in_SR + size + m_ret,
                      data = df1)
### summarize model results: using tidy instead of summary here ----
broom::tidy(mod, conf.int = TRUE)
full_modela_tidy <- broom::tidy(mod, conf.int = TRUE) |> 
      mutate(sig = case_when(
            p.value < 0.05 ~ 'signficant',
            p.value >= 0.05 ~ 'nonsignificant',
            TRUE ~ 'WRONG'
      ))

write_csv(full_modela_tidy, "analyses/oos_vg/multinom-q4-summary.csv")

### look at full model with relative risk ratios ----
tidy(mod, conf.int = TRUE, exponentiate = TRUE) |> 
      kable() |> 
      kable_styling("basic", full_width = FALSE)

### following significant variables ----
# year (both estuarine returners and coastal emigrators [relative to relocator])
# depart_periodspawn (both above)
# return_periodspawn (both above)
# time_in_SR (coastal emigrator only)

### interpreting model results ----
me_year <- slopes(mod, variables = "year", type = "probs")
summary(me_year)

me_depart <- slopes(mod, variables = "depart_period", type = "probs")
summary(me_depart)

me_return <- slopes(mod, variables = "return_period", type = "probs")
summary(me_return)

me_srtime <- slopes(mod, variables = "time_in_SR", type = "probs")
summary(me_srtime)

### pull out probability effects ----
ggeffect(mod, terms = "year") |> plot()
pprob_year <- ggeffect(mod, terms = "year[2012:2023 by =1]")
ggeffect(mod, terms = "depart_period") |> plot()
pprob_depart <- ggeffect(mod, terms = "depart_period")
ggeffect(mod, terms = "return_period") |> plot()
pprob_return <- ggeffect(mod, terms = "return_period")
ggeffect(mod, terms = "time_in_SR") |> plot()
pprob_srtime <- ggeffect(mod, terms = "time_in_SR")

pprob_year |> 
      ggplot(aes(x = x, y = predicted, fill = response.level)) +
      geom_bar(stat = "identity", position = 'dodge') +
      # facet_wrap(~response.level, ncol = 1) +
      # facet_wrap(~response.level) +
      # geom_line(linewidth = 2) +
      # geom_smooth(method = "loess", se = TRUE, linewidth = 2, span = 0.4)+
      scale_y_continuous(limits = c(0.0, 1.0), 
                         breaks = seq(0.0, 1.0, by = 0.2)) +
      # scale_y_continuous(breaks = seq(0.28, 0.38, by = 0.05)) +
      scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
      scale_color_brewer(palette = "Dark2",
                         name = "",
                         labels = c("Estuarine Returner",
                                    "Coastal Emigrator",
                                    "Relocator")) +
      labs(x = "Year",
           y = "Probability") +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title = element_text(size = 15, face = "bold", colour = "black"), 
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_blank(),
            legend.text = element_text(size = 15, face = "bold", colour = "black"))

pprob_year |> 
      ggplot(aes(x = x, y = predicted, color = response.level)) +
      geom_smooth(method = "loess", se = TRUE, linewidth = 2, span = 0.4)+
      scale_y_continuous(limits = c(0.0, 1.0), 
                         breaks = seq(0.0, 1.0, by = 0.2)) +
      # scale_y_continuous(breaks = seq(0.28, 0.38, by = 0.05)) +
      scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
      scale_color_brewer(palette = "Dark2",
                         name = "",
                         labels = c("Estuarine Returner",
                                    "Coastal Emigrator",
                                    "Relocator")) +
      labs(x = "Year",
           y = "Probability") +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title = element_text(size = 15, face = "bold", colour = "black"), 
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_blank(),
            legend.text = element_text(size = 15, face = "bold", colour = "black"))

pprob_depart |> 
      filter(x == "spawn") |> 
      ggplot(aes(x = response.level, y = predicted, color = response.level)) +
      geom_point(size = 5) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size = 2, width = 0) +
      scale_y_continuous(limits = c(0.0, 1.0), 
                         breaks = seq(0.0, 1.0, by = 0.2)) +
      # scale_y_continuous(breaks = seq(0.28, 0.38, by = 0.05)) +
      # scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
      scale_color_brewer(palette = "Dark2",
                         name = "",
                         labels = c("Estuarine Returner",
                                    "Coastal Emigrator",
                                    "Relocator")) +
      labs(x = "Category",
           y = "Probability") +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title = element_text(size = 15, face = "bold", colour = "black"), 
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_blank(),
            legend.text = element_text(size = 15, face = "bold", colour = "black"))

pprob_return |> 
      filter(x == "spawn") |> 
      ggplot(aes(x = response.level, y = predicted, color = response.level)) +
      geom_point(size = 5) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size = 2, width = 0) +
      scale_y_continuous(limits = c(0.0, 1.0), 
                         breaks = seq(0.0, 1.0, by = 0.2)) +
      # scale_y_continuous(breaks = seq(0.28, 0.38, by = 0.05)) +
      # scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
      scale_color_brewer(palette = "Dark2",
                         name = "",
                         labels = c("Estuarine Returner",
                                    "Coastal Emigrator",
                                    "Relocator")) +
      labs(x = "Category",
           y = "Probability") +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title = element_text(size = 15, face = "bold", colour = "black"), 
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_blank(),
            legend.text = element_text(size = 15, face = "bold", colour = "black"))

pprob_srtime |> 
      ggplot(aes(x = x, y = predicted, color = response.level)) +
      geom_smooth(method = "loess", se = TRUE, linewidth = 2, span = 0.4)+
      scale_y_continuous(limits = c(0.0, 1.0), 
                         breaks = seq(0.0, 1.0, by = 0.2)) +
      # scale_y_continuous(breaks = seq(0.28, 0.38, by = 0.05)) +
      scale_color_brewer(palette = "Dark2",
                         name = "",
                         labels = c("Estuarine Returner",
                                    "Coastal Emigrator",
                                    "Relocator")) +
      labs(x = "Time in Shark River (days)",
           y = "Probability") +
      theme(axis.text = element_text(size = 14, face = "bold", colour = "black"), 
            axis.title = element_text(size = 15, face = "bold", colour = "black"), 
            plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5), 
            panel.grid.major = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_blank(),
            legend.text = element_text(size = 15, face = "bold", colour = "black"))
