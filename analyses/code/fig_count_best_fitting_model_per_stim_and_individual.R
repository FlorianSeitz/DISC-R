# Plots count of best fitting models for each participant and stimulus
library(ggplot2)
library(data.table)

# 0. Prepares data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("5_compares_models.R")
source("fig_setup.R")
theme_set(theme_bw())

ll_test_stim[, best_fitting_model := toupper(best_fitting_model)]

only_weights_above_90 <- TRUE # plot only best fitting models, where weights are > .90
if(only_weights_above_90) {
  dt_plot <- ll_test_stim[weights_above_90_stim] 
} else {
  dt_plot <- ll_test_stim
}

# 1. Plots all best_fitting_models
ggplot(dt_plot, aes(x = best_fitting_model)) +
  geom_bar(aes(fill = best_fitting_model)) +
  facet_grid(time_pressure_cond~stim) +
  scale_fill_manual(values = cols) +
  ggtitle("Count of best fitting models per stimulus and time pressure condition") +
  labs(fill = "Models") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))
