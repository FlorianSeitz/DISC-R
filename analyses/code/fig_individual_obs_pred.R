# Plots predictions from optimal experimental design against new observed data at the aggregate level
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
theme_set(theme_bw())

dt <- fread("../../data/results/categorization_data_with_predictions.csv", 
            key = c("subj_id", "trial"))[fread("../../data/processed/categorization_exp_main.csv", 
                                               select = c("subj_id", "trial", "block", "too_slow", "time_pressure_cond", "response", "stim_type", "stim"), 
                                               colClasses = list("character" = "stim"),
                                               key = c("subj_id", "trial"))]

dt <- dt[block == "test" & stim_type == "new", ]
dt[, time_pressure_cond := as.factor(time_pressure_cond)]
dt[, stim := factor(stim, levels = c("100", "003", "221", "231", "321", "331"))]

dt_agg <- dt[!is.na(response), .(response = mean(response),
                                 se = sd(response)/sqrt(.N),
                                 pred_disc = mean(pred_disc),
                                 pred_mink = mean(pred_mink)), by = list(subj_id, time_pressure_cond, stim)]

ggplot(dt_agg, aes(x = stim, y = response)) +
  geom_point(size = 3, shape = 21, aes(fill = time_pressure_cond)) +
  geom_errorbar(aes(ymin = response - se, ymax = response + se, fill = time_pressure_cond), size = 0.5, width = .5) +
  geom_point(data = melt(dt_agg, measure.vars = c("pred_mink", "pred_disc")), aes(y = value, shape = variable), size = 3) + 
  ylim(0, 1) +
  facet_wrap(~subj_id, scales = "free_x")

dt_agg_agg <- dt_agg[, .(response = mean(response),
                     pred_disc = mean(pred_disc),
                     pred_mink = mean(pred_mink)), by = list(time_pressure_cond, stim)]

ggplot(dt_agg_agg, aes(x = stim, y = response)) +
  geom_point(size = 3, shape = 21, aes(fill = time_pressure_cond)) +
  # geom_errorbar(aes(ymin = response - se, ymax = response + se, fill = time_pressure_cond), size = 0.5, width = .5) +
  geom_point(aes(y = pred_disc), shape = 21, size = 3) + 
  geom_point(aes(y = pred_mink), shape = 22, size = 3) + 
  ylim(0, 1) +
  facet_wrap(~time_pressure_cond)
