# Plots log likelihood of test phase data given models 
library(data.table)
library(cogsciutils)
library(patchwork)
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
source("5_compares_models.R")

ll_test_agg <- dt[block == "test" & too_slow == FALSE, 
                  lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', na.rm = TRUE)}), 
                  .SDcols = cols, 
                  by = list(time_pressure_cond, subj_id)]
ll_test_agg <- melt(ll_test_agg, id.vars = c("time_pressure_cond", "subj_id"), variable.name = "model", value.name = "log_likelihood")
levels(ll_test_agg$model) <- c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM")

labs <- c("Without time pressure", "With time pressure")
names(labs) <- c("FALSE", "TRUE")

min_ll <- ll_test_agg_mean[, min(-1*log_likelihood), by = time_pressure_cond]

ggplot(ll_test_agg, aes(x = model, y = -1 * log_likelihood)) +
  geom_boxplot(aes(fill = model)) +
  geom_point(alpha = 0.8, position = position_jitter(width = 0.05)) +
  facet_grid(~time_pressure_cond, labeller = labeller(time_pressure_cond = labs)) +
  ggtitle("Median log likelihood of test phase data given models for each time pressure condition") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none") +
  ylab("negative log likelihood")
ggsave("../../output/images/log_lik_aggregated.jpg")

