# Plots log likelihood of test phase data given models 
library(data.table)
library(cogsciutils)
library(patchwork)
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("5_compares_models.R")
source("fig_setup.R")

ll_test_agg <- dt[block == "test" & too_slow == FALSE, 
                  lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", options = list(response = 'disc'), na.rm = TRUE)}), 
                  .SDcols = cols, 
                  by = list(time_pressure_cond, subj_id)]
ll_test_agg <- melt(ll_test_agg, id.vars = c("time_pressure_cond", "subj_id"), variable.name = "model", value.name = "log_likelihood")
levels(ll_test_agg$model) <- c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM")
ll_test_agg[, time_pressure_cond := factor(time_pressure_cond, levels = c(TRUE, FALSE))]

labs <- c("Time pressure", "No time pressure")
names(labs) <- c("TRUE", "FALSE")

ggplot(ll_test_agg, aes(x = model, y = -1 * log_likelihood)) +
  geom_boxplot(aes(fill = model)) +
  scale_fill_manual(values = bw_cols) +
  geom_point(alpha = 0.9, position = position_jitter(width = 0.05)) +
  facet_grid(~time_pressure_cond, labeller = labeller(time_pressure_cond = labs)) +
  # ggtitle("Median log likelihood of test phase data given models for each time pressure condition") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  ylab("Negative log likelihood") +
  xlab("Models")
ggsave("../../output/images/log_lik_aggregated.png", width = 9.5)

