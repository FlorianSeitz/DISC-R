# Plots regression betas against model predictions
# 0. Prepares data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
library(lme4)
library(emmeans)

# 1. Computes generalized linear model
full_model <- lmer(response_s ~ time_pressure * (time_pressure_first + type) + (1|subj_id), data = dt) 
fixed_eff <- fixef(full_model)
betas <- as.data.table(fixed_eff[grepl("^type|Intercept", names(fixed_eff))], keep.rownames = TRUE)
betas[, type := levels(dt$type)]
betas[, beta := exp(V2)/(1+exp(V2))]

# 2. Changes data to long format
dt_pred <- fread("../../../data/results/similarity_data_with_predictions.csv")
dt_long <- melt(dt_pred, measure.vars = patterns("pred"), id.vars = c("subj_id", "i_comb", "type", "trial", "time_pressure")) # used for plot below
dt_long <- dt_long[, .(value = unique(value)), by = list(type, trial, variable, subj_id, time_pressure, i_comb)]
dt_long[, variable := toupper(gsub("pred_", "", variable))]

# 3. Makes stim a factor
dt_long[, type := factor(type, levels = levels, labels = labels)]

# 4. Aggregates predictions
dt_long_agg <- dt_long[, .(value = mean(value)), by = list(type, variable)]

# 4. Plot
j <- position_dodge(0.6)
ggplot(data = betas, aes(x = type, y = beta)) +
  geom_bar(fill = "lightgrey", color = "lightgrey", stat = "identity", size = 1) +
  geom_point(data = dt_long_agg, aes(y = value, color = variable), position = j) +
  geom_line(data = dt_long_agg, aes(y = value, group = variable), position = j) +
  ylim(0, 1) +
  # facet_grid(subj_id~stim_type, scales = "free_x", space = "free") +
  guides(fill = "none") +
  scale_color_manual(values = col_cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xl + yl

ggsave("../../output/images/glm_testphase.jpg")