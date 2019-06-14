# Plots predictions from optimal experimental design against new observed data at the aggregate level
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
theme_set(theme_bw())

dt <- fread("../../data/processed/categorization_exp_main.csv", key = "subj_id", colClasses = list("character" = "stim"))
dt <- dt[block == "test" & stim_type == "new", ]
dt[, time_pressure_cond := as.factor(time_pressure_cond)]
dt[, stim := factor(stim, levels = c("100", "003", "221", "231", "321", "331"))]

dt[block == "test" & stim_type == "new", .(mean = mean(response, na.rm = T), sd = sd(response, na.rm = T)), by = list(time_pressure_cond, stim)]

sim_mod_pred <- fread("../../experiment/design/simulated_predictions_test.csv", colClasses = list(character = "stimulus.id"))
sim_mod_pred[, stim := factor(stimulus.id, levels = c("100", "003", "221", "231", "321", "331"))]
sim_mod_pred[, metric := factor(grepl("Attr|disc", variable), labels = c("MINK", "DISC"), ordered = TRUE)]

meds <- sim_mod_pred[, .(med = round(median(value), 2)), by = list(variable, stim, metric, dimensionality)]
meds[dimensionality == "UNI", dim := strsplit(variable, "_")[[1]][2], by = list(stim, variable)]
meds[dimensionality == "UNI", dodge := ifelse(duplicated(med), 0, 0.5), by = list(stim, metric)]
meds[, nudge := 0]
meds[dimensionality == "UNI" & metric == "DISC", nudge := ifelse(max(med) > 0.5, -0.05, 0.05), by = list(stim, metric)]

ggplot(sim_mod_pred, aes(x = as.numeric(metric), y = value)) +
  geom_label(data = meds, aes(x = as.numeric(metric) + dodge, y = med + nudge, label = dim, color = metric), label.size = 0) +
  geom_violin(aes(linetype = dimensionality, color = metric), size = 1) +
  geom_point(data = meds, aes(y = med, shape = dimensionality, color = metric), position = position_dodge(width = 0.9), size = 2, stroke = 1.3, fill = "white") +
  geom_hline(yintercept = 0.5, linetype = 2) +
  stat_summary(geom = "point", data = dt, aes(x = 0, y = response, fill = time_pressure_cond), size = 3, shape = 21) +
  stat_summary(geom = "errorbar", data = dt, aes(x = 0, y = response, fill = time_pressure_cond), size = 0.5, width = .5) +
  ylim(0, 1) +
  xlab("Model x stimulus ID") +
  ylab("Categorization probability for category A") +
  ggtitle("Model predictions for test stimuli") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_shape_manual(values = c(22, 24)) +
  scale_color_grey(end = 0.6) +
  labs(fill = "time pressure") +
  # scale_color_discrete(name = "Model", breaks = c("GCM.Attr", "GCM.Eucl", "Unidim_1", "Unidim_2", "Unidim_3"),
  #                      labels = c("GCM_DISC", "GCM_MINK", "UNIDIM_1", "UNIDIM_2", "UNIDIM_3")) +
  facet_wrap(~stim, scales = "free_x") +
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2),
         shape = guide_legend(order = 2),
         fill = guide_legend(order = 3))
ggsave("../../output/images/aggregate_predictions_observations.jpg")
