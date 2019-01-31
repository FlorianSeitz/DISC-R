library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")

dat <- fread("../../data/processed/categorization_data_with_predictions.csv")

dat <- dat[block == "training"]

dat[, subj_id := as.factor(subj_id)]

dat[, trial_bin := cut(trial, breaks = seq(1, max(trial) + 9, 10), include.lowest = TRUE), by = subj_id]
dat[, trial_perc_bin := cut(trial/max(trial), breaks = seq(0, 1, .1), include.lowest = TRUE), by = subj_id]

dat <- melt(dat, measure.vars = c("pred_disc", "pred_mink", "pred_disc_unidim", "pred_mink_unidim"))

dat <- dat[subj_id == 9]

ggplot(dat, aes(x = trial_bin, y = as.numeric(true_cat == response), group = 3)) +
  geom_point(aes(group = subj_id, color = subj_id), stat = "summary", fun.y = mean) + 
  stat_summary(aes(group = subj_id, color = subj_id), geom = "line", fun.y = mean) +
  geom_point(aes(y = 1-abs(value - true_cat), group = variable, color = variable), stat = "summary", fun.y = mean) + 
  stat_summary(aes(y = 1-abs(value - true_cat), group = variable, color = variable), geom = "line", fun.y = mean) +
  ylim(0, 1) +
  ylab("accuracy") +
  xlab("trial") +
  ggtitle("Accuracy in the learning phase") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
