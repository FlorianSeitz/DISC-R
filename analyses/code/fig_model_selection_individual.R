# Plots the number of people best fit by each model for each time pressure condition 
library(data.table)
library(tidyverse)
library(ggpattern)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("5_compares_models.R")
source("fig_setup.R")

model_distr[, best_fitting_model := factor(best_fitting_model, 
                                           levels = c("disc", "mink", "disc_unidim", "mink_unidim", "random"), 
                                           labels = c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM"))]
model_distr <- rbind(model_distr, merge(dt[, length(unique(subj_id)), by = time_pressure_cond],
                                        model_distr[, sum(N), by = time_pressure_cond], 
                                        all = TRUE)[, .(best_fitting_model = "NONE",
                                                        N = diff(V1)), by = time_pressure_cond])
model_distr[, time_pressure_cond := factor(time_pressure_cond, levels = c(TRUE, FALSE))]
labs <- c("Time pressure", "No time pressure")
names(labs) <- c("TRUE", "FALSE")

ggplot(model_distr, aes(x = best_fitting_model, y = N)) +
  geom_bar_pattern(aes(pattern = best_fitting_model, fill = best_fitting_model), colour = 'black', stat = "identity", pattern_fill = "white") +
  scale_pattern_manual(values = bw_patterns) +
  # geom_col_pattern(aes(pattern_fill = best_fitting_model)) +
  # geom_bar(aes(fill = best_fitting_model), stat = "identity", color = "black") +
  scale_fill_manual(values = bw_patterns_cols) +
  facet_grid(~time_pressure_cond, labeller = labeller(time_pressure_cond = labs)) +
  # ggtitle("Number of people best fit by each model for each time pressure condition") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  xlab("Models") +
  ylab(expression(paste(italic("N"), " described participants")))
ggsave("../../output/images/model_selection_individual.png", width = 11)
