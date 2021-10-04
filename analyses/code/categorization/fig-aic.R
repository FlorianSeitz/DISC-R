# ==========================================================================
# Plots the AIC weights of models (y-axis) for each participant (x-axis)
# ==========================================================================
pacman::p_load(ggplot2, ggpattern)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change wd to where this script lies (RStudio needed!)
source("3-modeling-analysis.R")
source("fig_setup.R")

# ==========================================================================
# Prepares AIC weights and order on x-axis
# ==========================================================================
models <- c("gcm", "gcm_disc", "gcm_unidim", "gcm_tau")
models <- c(models, "random")
names(models) <- c("MULTI-MINK", "MULTI-DISC", "UNI-MINK", "UNI-DISC", "RANDOM")
models <- models[c("MULTI-MINK", "UNI-MINK", "MULTI-DISC", "UNI-DISC", "RANDOM")]

weights[, (models) := round(.SD, 4), .SDcols = models]
weights[subj_id %in% weights[, rowSums(.SD), .SDcols = models, by = subj_id][V1 != 1, subj_id], random := random + 1 - rowSums(.SD), .SDcols = models]

weights[, best_model_exists := rowSums(.SD > crit) > 0, .SDcols = models]
weights[, best_model := factor(best_model, levels = models)]
setorderv(weights, cols = c("best_model_exists", "best_model", models), order = -1)
weights[, ord := 1:.N, by = time_pressure_cond]
# weights[best_model_exists == FALSE, ord := ord + as.numeric(time_pressure_cond) + 1]
weights[best_model_exists == FALSE, ord := ord + 2]

weights <- melt(weights, id.vars = c("subj_id", "time_pressure_cond", "best_model", "ord", "best_model_exists"), variable.name = "model", value.name = "weight")
weights[, model := factor(model, levels = models)]

v_lines <- weights[, which(!(1:max(ord)) %in% ord), by = time_pressure_cond][, mean(V1), by = time_pressure_cond]
dt_name <- data.table(text = c("described by a model:"), f1 = .5, f2 = -0.07)
dt_text <- weights[, .(f1 = mean(ord), f2 = -0.07), by = .(time_pressure_cond, best_model_exists)]
dt_text[, text := ifelse(best_model_exists == TRUE, "yes", "no")]

# ==========================================================================
# Makes plot
# ==========================================================================
bw_patterns <- c(gcm = "none", gcm_unidim = "stripe", gcm_disc = "none", gcm_disc_unidim = "stripe", random = "none") 
bw_patterns_cols <- c(gcm = grey(.4), gcm_unidim = grey(.4), gcm_disc = grey(.8), gcm_disc_unidim = grey(.8), random = grey(1))

ggplot(weights, aes(x = ord, y = weight)) +
  geom_bar(aes(fill = model), stat = "identity", color = "black", size = 0.25) +
  scale_fill_manual(values = c(grey(0), grey(.25), grey(.5), grey(.75), grey(1)), name = "Model") +
  # geom_bar_pattern(aes(pattern = model, fill = model), 
  #                  stat = "identity",
  #                  color = "black", 
  #                  pattern_fill = "black",
  #                  pattern_angle = 45,
  #                  pattern_density = 0.1,
  #                  pattern_spacing = 0.025,
  #                  pattern_key_scale_factor = 0.6) +
  # scale_pattern_manual(values = bw_patterns, name = "Model", labels = names(models)) +
  # scale_fill_manual(values = bw_patterns_cols, name = "Model", labels = names(models)) +
  geom_hline(yintercept = crit, linetype = 2) +
  geom_vline(aes(xintercept = V1), data = v_lines, size = 1.2) +
  xlab("Participants") +
  ylab("AIC weights") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank()) +
  scale_y_continuous(breaks = c(0, crit, 1), limits = c(-0.1, 1)) +
  geom_text(data = dt_text, mapping = aes(x = f1, y = f2, label = text), size = 5, hjust = 0.5) +
  geom_text(data = dt_name, mapping = aes(x = f1, y = f2, label = text), size = 5, hjust = 0) +
  ylab("Akaike weights") +
  facet_wrap(~time_pressure_cond, nrow = 2, labeller = labeller(time_pressure_cond = c("FALSE" = "No time pressure", "TRUE" = "Time pressure")))
ggsave("../../../output/images/fig_aic_sim.png", width = 10, height = 6)

ggplot(weights, aes(x = ord, y = weight)) +
  geom_bar(aes(fill = model), stat = "identity", color = "black", size = 0.25) +
  scale_fill_manual(values = c(grey(0), grey(1/3), grey(2/3), grey(1)), name = "Model", labels = names(models)) +
  # geom_bar_pattern(aes(pattern = model, fill = model), 
  #                  stat = "identity",
  #                  color = "black", 
  #                  pattern_fill = "black",
  #                  pattern_angle = 45,
  #                  pattern_density = 0.1,
  #                  pattern_spacing = 0.025,
  #                  pattern_key_scale_factor = 0.6) +
  # scale_pattern_manual(values = bw_patterns, name = "Model", labels = names(models)) +
  # scale_fill_manual(values = bw_patterns_cols, name = "Model", labels = names(models)) +
  geom_hline(yintercept = crit, linetype = 2) +
  geom_vline(aes(xintercept = V1), data = v_lines, size = 1.2) +
  xlab("Participants") +
  ylab("AIC weights") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank()) +
  scale_y_continuous(breaks = c(0, crit, 1), limits = c(-0.1, 1)) +
  geom_text(data = dt_text, mapping = aes(x = f1, y = f2, label = text), size = 5, hjust = 0.5) +
  geom_text(data = dt_name, mapping = aes(x = f1, y = f2, label = text), size = 5, hjust = 0) +
  ylab("Akaike weights") +
  facet_wrap(~time_pressure_cond, nrow = 2, labeller = labeller(time_pressure_cond = c("FALSE" = "No time pressure", "TRUE" = "Time pressure")))
ggsave("../../../output/images/fig_aic_sim_tp.png", width = 10, height = 3)
