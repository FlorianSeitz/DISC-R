# Runs a general linear model with logit link on the test dta
library(data.table)
library(lme4)
library(emmeans) # package emmeans needs to be attached for follow-up tests. 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("3_predicts_models.R", chdir = TRUE)
source("fig_setup.R")

dt <- dt[block == "test" & stim_type == "new", ]

dt[, time_pressure_cond := as.factor(time_pressure_cond)]
dt[, subj_id := as.factor(subj_id)]
dt[, stim := factor(stim, labels = c("100", "003", "221", "231", "321", "331"))]

dt <- rbind(dt, dt)
dt[1:700, time_pressure_cond := 0]

full_model <- glmer(response ~ time_pressure_cond * stim + (0 + stim|subj_id), data = dt, family = binomial) # time_pressure_cond * 
summary(full_model)

restricted_model <- glmer(response ~ time_pressure_cond + stim + (0 + stim|subj_id), data = dt, family = binomial) # time_pressure_cond * 
summary(restricted_model)

anova(full_model, restricted_model)

emm1 <- emmeans(full_model, ~ stim |  time_pressure_cond)
pairs(emm1, adjust = "holm") # all pairwise comparisons 
H1 <- list(H1ac = c(5, rep(-1, 5)), # H1a & H1c 
             H1b = c(-1, 5, rep(-1, 4)) # H1b
             ) 
contrast(emm1, H1, adjust = "holm") # check if holm method is specified globally or separately for both time_pressure_cond

betas <- as.data.table(fixef(full_model), keep.rownames = TRUE)
betas[, stim := levels(dt$stim)] # gsub("stim", "", V1)
betas[, beta := exp(V2)/(1+exp(V2))]

dt_long <- melt(dt, measure.vars = patterns("pred"), id.vars = c("stim", "subj_id"))
dt_long <- dt_long[, .(value = mean(value)), by = list(stim, variable, subj_id)]
dt_long[, variable := toupper(gsub("pred_", "", variable))]

learn_stim <- apply(expand.grid(0:1, 0:1, 1:2), 1, paste, collapse = "")
dt_long[, stim_type := factor(!stim %in% learn_stim, labels = c("old", "new"))]
betas[, stim_type := factor(!stim %in% learn_stim, labels = c("old", "new"))]

dt_long[, stim := factor(stim, levels = stim_order)]
betas[, stim := factor(stim, levels = stim_order)]

j <- position_dodge(0.3)
ggplot(data = betas, aes(x = stim, y = beta)) +
  geom_bar(fill = "lightgrey", color = "lightgrey", stat = "identity", size = 1) +
  geom_point(data = dt_long, aes(y = value, color = variable), position = j) +
  geom_line(data = dt_long, aes(y = value, color = variable, group = variable), position = j) +
  ylim(0, 1) +
  facet_grid(subj_id~stim_type, scales = "free_x", space = "free") +
  guides(fill = "none") +
  scale_color_manual(values = cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  xl + yl

ggsave("../../output/images/glm_testphase.jpg")
