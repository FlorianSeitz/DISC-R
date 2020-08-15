library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
source("5_compares_models.R")

weights[rowSums(round(weights[, 3:7], 2)) != 1]
weights[, 3:7] <- round(weights[, 3:7], 3)
setorderv(weights, cols = c("disc", "disc_unidim", "mink", "mink_unidim", "random"), order = -1)
weights[, ord := seq_len(.N), by = list(time_pressure)]
weights <- melt(weights, id.vars = c("subj_id", "time_pressure", "ord"), variable.name = "model", value.name = "weight")
weights[, model := toupper(model)]
weights[, model := factor(model, levels = c("DISC", "MINK", "DISC_UNIDIM", "MINK_UNIDIM", "RANDOM"), labels = c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM"))]
weights[, time_pressure_cond := factor(time_pressure_cond, levels = c(TRUE, FALSE))]

labs <- c("Time pressure", "No time pressure")
names(labs) <- c("TRUE", "FALSE")
ggplot(weights, aes(x = ord, y = weight, fill = model)) +
  geom_bar(stat = "identity", color = "black", size = 0.25) +
  scale_fill_manual(values = bw_cols, name = "Model") +
  facet_wrap(time_pressure~., ncol = 1, scales = "free", drop = TRUE, labeller = labeller(time_pressure = labs)) +
  xlab("Participants") +
  ylab("Weights") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = 12))
# ggtitle("Distribution of AIC weights per participant")

ggsave("../../../output/images/similarity/aicweights.png")
