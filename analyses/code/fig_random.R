# Plots mean and sd responses of participants best fit by random model
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
source("5_compares_models.R")

tp_weights <- weights[time_pressure_cond == TRUE, ]
random_id <- tp_weights[random > .90, subj_id]

dt <- dt[block == "test" & stim_type == "new", ]
dt <- dt[subj_id %in% random_id, ]
dt[, time_pressure_cond := as.factor(time_pressure_cond)]
dt[, stim := factor(stim, levels = c("100", "003", "221", "231", "321", "331"))]
dt[, best_model := "RANDOM"]

dt[, .(mean = mean(response, na.rm = T),
       sd = sd(response, na.rm = T)), by = list(stim, subj_id)][, mean(mean), by = subj_id]

ggplot(dt, aes(x = stim, y = response)) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  stat_summary(geom = "errorbar") +
  stat_summary(geom = "point") +
  ylim(0, 1) +
  xlab("Stimulus ID") +
  ylab("Proportion category A responses") +
  # ggtitle("Model predictions for test stimuli") +
  theme(
        # axis.ticks.x = element_blank(),
        # axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(~subj_id, scales = "free_x")
ggsave("../../output/images/fig_random.jpg", width = 9)
