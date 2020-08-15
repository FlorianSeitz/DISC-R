# Plots the number of people best fit by each model for each time pressure condition 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # changes wd to folder of this script; requires RStudio
source("fig_setup.R")
source("5_compares_models.R")
library(patchwork)
library(tidyverse)

model_distr[, best_fitting_model := factor(best_fitting_model, 
                                           levels = c("disc", "mink", "disc_unidim", "mink_unidim", "random"), 
                                           labels = c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM"))]
model_distr <- rbind(model_distr, model_distr[, .(best_fitting_model = "NONE", N = 64 - sum(N)), by = time_pressure])
model_distr[, time_pressure := factor(time_pressure, levels = c(TRUE, FALSE))]

labs <- c("Time pressure", "No time pressure")
names(labs) <- c("TRUE", "FALSE")

ggplot(model_distr, aes(x = best_fitting_model, y = N)) +
  geom_bar(aes(fill = best_fitting_model), stat = "identity", color = "black") +
  scale_fill_manual(values = c(bw_cols, "NONE" = "black")) +
  facet_grid(~time_pressure, labeller = labeller(time_pressure = tp_labs)) +
  # ggtitle("Number of people best fit by each model for each time pressure condition") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none",
        axis.ticks.x = element_blank()) +
  xlab("Models") +
  ylab(expression(paste(italic("N"), " described participants")))
ggsave("../../../output/images/similarity/model_selection_individual.png", width = 9.5)
