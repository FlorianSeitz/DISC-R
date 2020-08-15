# Plots log likelihood of test phase data given models 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # changes wd to folder of this file (RStudio required)
source("fig_setup.R")
devtools::load_all("~/cogsciutils/")
library(patchwork)
library(tidyverse)

# 0. Prepares data with log likelihoods
ll_test_agg <- fread("../../../data/results/similarity_log_likelihood.csv")
cols <- colnames(ll_test_agg)[grepl(pattern = "^pred", x = colnames(ll_test_agg))]
ll_test_agg <- ll_test_agg[, lapply(.SD, median), .SDcols = cols, by = list(subj_id, time_pressure)] # averages lls across hold-out samples

ll_test_agg <- melt(ll_test_agg, id.vars = c("time_pressure", "subj_id"), variable.name = "model", value.name = "log_likelihood") # transforms it to long format
levels(ll_test_agg$model) <- c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM")
ll_test_agg[, time_pressure := factor(time_pressure, levels = c(TRUE, FALSE))]

# 1. Boxplot
p0 <- ggplot(ll_test_agg, aes(x = model, y = log_likelihood)) +
  geom_boxplot(aes(fill = model)) +
  scale_fill_manual(values = bw_cols) +
  geom_point(alpha = 0.9, position = position_jitter(width = 0.05)) +
  facet_grid(~time_pressure, labeller = labeller(time_pressure = tp_labs)) +
  # ggtitle("Median log likelihood of test phase data given models for each time pressure condition") + 
  # theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none",
        axis.ticks.x = element_blank()) +
  ylab("Log likelihood") +
  xlab("Models")

box_stats <- boxplot.stats(ll_test_agg$log_likelihood) 
ylim1 = box_stats$stats[c(1, 5)] # computes lower and upper whiskers
p0 + coord_cartesian(ylim = ylim1*1.05) # scales y limits based on ylim1

ggsave("../../../output/images/similarity/log_lik_aggregated.png", width = 9.5)

# 2. Lineplot
ll_test_agg_agg <- ll_test_agg[, .(log_likelihood = median(log_likelihood)), by = list(model, time_pressure)] # aggregates across subjects
ll_test_agg <- ll_test_agg[!subj_id == "bmcl"] # only now, remove this or make this more error proof
# ll_test_agg <- ll_test_agg[!log_likelihood %in% box_stats$out] # only now, remove this or make this more error proof

ggplot(ll_test_agg, aes(x = model, y = log_likelihood, group = subj_id)) +
  geom_line(color = grey(level = 0.6, alpha = 0.5)) +
  geom_point(color = grey(level = 0.6, alpha = 0.5)) +
  geom_line(data = ll_test_agg_agg, aes(group = 1), size = 1.4) +
  geom_point(data = ll_test_agg_agg, aes(group = 1), size = 3) +
  facet_grid(~time_pressure, labeller = labeller(time_pressure = tp_labs)) +
  # ggtitle("Median log likelihood of test phase data given models for each time pressure condition") + 
  # theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "none",
        axis.ticks.x = element_blank()) +
  ylab("Log likelihood") +
  xlab("Models")

ggsave("../../../output/images/similarity/log_lik_aggregated_lineplot.png", width = 9.5)
