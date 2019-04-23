# Plots regression betas against model predictions
library(data.table)
library(lme4)
library(emmeans) # package emmeans needs to be attached for follow-up tests. 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio

# 0. Prepares data
source("3_predicts_models.R", chdir = TRUE)
source("fig_setup.R")

dt <- dt[block == "test", ]
dt[, time_pressure_cond := as.factor(time_pressure_cond)]
dt[, subj_id := as.factor(subj_id)]
dt[, stim := factor(stim, labels = stim_order)]

# 1. Computes generalized linear model
full_model <- glmer(response ~ time_pressure_cond * stim + (0 + stim|subj_id), data = dt, family = binomial) 
fixed_eff <- fixef(full_model)
betas <- as.data.table(fixed_eff[grepl("^stim|Intercept", names(fixed_eff))], keep.rownames = TRUE)
betas[, stim := levels(dt$stim)] # gsub("stim", "", V1)
betas[, beta := exp(V2)/(1+exp(V2))]

# 2. Changes data to long format
dt_long <- melt(dt, measure.vars = patterns("pred"), id.vars = c("stim", "subj_id")) # used for plot below
dt_long <- dt_long[, .(value = unique(value)), by = list(stim, variable, subj_id)]
dt_long[, variable := toupper(gsub("pred_", "", variable))]

# 3. Splits stimuli in new and old (already seen in learning phase)
learn_stim <- apply(expand.grid(0:1, 0:1, 1:2), 1, paste, collapse = "")
dt_long[, stim_type := factor(!stim %in% learn_stim, labels = c("old", "new"))]
betas[, stim_type := factor(!stim %in% learn_stim, labels = c("old", "new"))]

# 4. Makes stim a factor
dt_long[, stim := factor(stim, levels = stim_order)]
betas[, stim := factor(stim, levels = stim_order)]

# 5. Plot
j <- position_dodge(0.6)
ggplot(data = betas, aes(x = stim, y = beta)) +
  geom_bar(fill = "lightgrey", color = "lightgrey", stat = "identity", size = 1) +
  geom_point(data = dt_long, aes(y = value, color = variable), position = j) +
  geom_line(data = dt_long, aes(y = value, color = variable, group = variable), position = j) +
  ylim(0, 1) +
  facet_grid(~stim_type, scales = "free_x", space = "free") +
  guides(fill = "none") +
  scale_color_manual(values = cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  xl + yl

ggsave("../../output/images/glm_testphase.jpg")
