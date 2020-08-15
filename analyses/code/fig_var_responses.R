# Plots variability in responses within participants and across participants
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
source("5_compares_models.R")

dt <- dt[block == "test" & stim_type == "new", ]
dt[, time_pressure_cond := as.factor(time_pressure_cond)]
dt[, stim := factor(stim, levels = c("100", "003", "221", "231", "321", "331"))]
var_within <- dt[, sd(response, na.rm = T), by = list(subj_id, time_pressure_cond, stim)][, .(within = mean(V1)), by = list(time_pressure_cond, stim)]
var_across <- dt[, .(across = sd(response, na.rm = T)), by = list(time_pressure_cond, stim)]
var <- merge(var_within, var_across)

ggplot(var, aes(x = within, y = across, fill = time_pressure_cond)) +
  geom_line(aes(group = stim)) +
  geom_point(size = 3, shape = 21) +
  scale_fill_manual(values = c("black", "white")) +
  labs(fill = "Time pressure") +
  ylim(0, .6) +
  xlim(0, .6)
ggsave("../../output/images/fig_var_responses.png")
