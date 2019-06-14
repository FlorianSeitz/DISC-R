# Plots the number of people best fit by each model for each time pressure condition 
library(data.table)
library(patchwork)
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
source("5_compares_models.R")

model_distr[, best_fitting_model := factor(best_fitting_model, 
                                           levels = c("disc", "mink", "disc_unidim", "mink_unidim", "random"), 
                                           labels = c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM"))]

labs <- c("Without time pressure", "With time pressure")
names(labs) <- c("FALSE", "TRUE")

ggplot(model_distr, aes(x = best_fitting_model, y = N)) +
  geom_bar(aes(fill = best_fitting_model), stat = "identity") +
  facet_grid(~time_pressure_cond, labeller = labeller(time_pressure_cond = labs)) +
  ggtitle("Number of people best fit by each model for each time pressure condition") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none") +
  xlab("best fitting model")
ggsave("../../output/images/model_selection_individual.jpg")
