# Plots predictions from optimal experimental design against new observed data at the aggregate level
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
source("5_compares_models.R")

disc_id <- weights[disc > .90, subj_id]
mink_id <- weights[mink > .90, subj_id]

dt <- dt[block == "test" & stim_type == "new", ]
dt <- dt[subj_id %in% c(disc_id, mink_id), ]
dt[, time_pressure_cond := as.factor(time_pressure_cond)]
dt[, stim := factor(stim, levels = c("100", "003", "221", "231", "321", "331"))]
dt[, best_model := ifelse(subj_id %in% disc_id, "MULTI-DISC", "MULTI-MINK")]

dcast(dt[, .(mean = mean(response, na.rm = T)), by = list(stim, best_model)], stim ~ best_model)

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
  geom_violin(aes(linetype = dimensionality, color = metric), size = 0.75) +
  geom_point(data = meds, aes(y = med, shape = dimensionality, color = metric), position = position_dodge(width = 0.9), size = 2, stroke = 1.3, fill = "white") +
  geom_hline(yintercept = 0.5, linetype = 2) +
  stat_summary(geom = "errorbar", data = dt, aes(x = 0, y = response, fill = best_model), size = 0.5, width = .5) +
  stat_summary(geom = "point", data = dt, aes(x = 0, y = response, fill = best_model), size = 3, shape = 21) +
  ylim(0, 1) +
  xlab("Model x stimulus ID") +
  ylab("Categorization probability for category A") +
  # ggtitle("Model predictions for test stimuli") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_shape_manual(values = c(22, 24), labels = c("MULTI-MINK", "UNI-MINK")) +
  scale_linetype_manual(values = c(1, 42), labels = c("MULTI-MINK", "UNI-MINK")) +
  scale_color_manual(values = grey(c(.3, .6)), labels = c("MULTI-DISC", "UNI-DISC")) +
  # scale_color_grey(end = 0.6) +
  scale_fill_manual(values = c("black", "white")) +
  labs(fill = "time pressure") +
  # scale_color_discrete(name = "Model", breaks = c("GCM.Attr", "GCM.Eucl", "Unidim_1", "Unidim_2", "Unidim_3"),
  #                      labels = c("GCM_DISC", "GCM_MINK", "UNIDIM_1", "UNIDIM_2", "UNIDIM_3")) +
  facet_wrap(~stim, scales = "free_x") +
  guides(color = guide_legend(order = 3, title = element_blank(), override.aes = list(shape = c(22, 24), linetype = c(1, 42), color = grey(c(.6 , .6)))),
         linetype = guide_legend(order = 2, title = "Models", override.aes = list(color = grey(c(.3 , .3)))),
         shape = guide_legend(order = 2, title = "Models"),
         fill = guide_legend(order = 1, title = "Best model"))
ggsave("../../output/images/fig_disc_mink.png")
