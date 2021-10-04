library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("5_compares_models.R")
source("fig_setup.R")

weights[rowSums(round(weights[, 3:7], 2)) != 1]
weights[, 3:7] <- round(weights[, 3:7], 3)
weights[, best_model_exists := as.numeric(weights_above_90)]
weights[, disc_best := as.numeric(disc >= .90)]
weights[, disc_unidim_best := as.numeric(disc_unidim >= .90)]
weights[, mink_best := as.numeric(mink >= .90)]
weights[, mink_unidim_best := as.numeric(mink_unidim >= .90)]
weights[, random_best := as.numeric(random >= .90)]
setorderv(weights, cols = c("best_model_exists", "disc_best", "disc_unidim_best", "mink_best", "mink_unidim_best", "random_best",
                            "disc", "disc_unidim", "mink", "mink_unidim", "random"), order = -1)
weights[, ord := seq_len(.N), by = time_pressure_cond]
weights <- melt(weights, id.vars = c("subj_id", "time_pressure_cond", "ord", "best_model_exists", "disc_best", "disc_unidim_best", "mink_best", "mink_unidim_best", "random_best"), variable.name = "model", value.name = "weight")
weights[, model := toupper(model)]
weights[, model := factor(model, levels = c("DISC", "MINK", "DISC_UNIDIM", "MINK_UNIDIM", "RANDOM"), labels = c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM"))]
weights[, time_pressure_cond := factor(time_pressure_cond, levels = c(TRUE, FALSE))]

ggplot(weights, aes(x = ord, y = weight, fill = model)) +
  geom_bar(stat = "identity", color = "black", size = 0.25) +
  scale_fill_manual(values = bw_cols, name = "Model") +
  # geom_bar_pattern(aes(pattern = model, fill = model), colour = 'black', stat = "identity", pattern_fill = "white") +
  # scale_pattern_manual(values = bw_patterns, name = "Model") +
  # scale_fill_manual(values = bw_patterns_cols, name = "Model") +
  facet_wrap(time_pressure_cond~., ncol = 1, scales = "free", drop = TRUE, labeller = labeller(time_pressure_cond = tp_labs)) +
  xlab("Participants") +
  ylab("Weights") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = 12))
# ggtitle("Distribution of AIC weights per participant")

ggsave("../../output/images/aicweights.png")
