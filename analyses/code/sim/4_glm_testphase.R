# Runs a general linear model with logit link on the test dta
library(data.table)
library(lme4)
library(lmerTest)
library(emmeans) # package emmeans needs to be attached for follow-up tests. 
library(pbkrtest)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio

# 0. Prepares data
dt <- fread("../../../data/processed/similarity_exp_main.csv", colClasses = list("character" = c("stim_left", "stim_right")))
dt <- dt[type %in% c("A", "B", "C", "D"), ]
dt[, subj_id := as.factor(subj_id)]

dt[, time_pressure := factor(time_pressure, levels = c(TRUE, FALSE), labels = c(TRUE, FALSE))]
dt[, type := factor(type, levels = c("C", "A", "B", "D"), labels = c("two-large", "one-large", "all-small", "two-small"))]
# dt[, type := factor(type, levels = c("A", "B", "C", "D", "I", "V"), labels = c("one-large", "all-small", "two-large", "two-small", "all-equal", "all-diff"))]

contrasts(dt$type) <- contr.sum
contrasts(dt$time_pressure) <- contr.sum

# 1. Computes generalized linear models
# 1.1. With interaction time_pressure*type
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + time_pressure||subj_id) + (1 + type||subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + time_pressure||subj_id) + (1|subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 |subj_id) + (1 + type||subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + time_pressure + type||subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + time_pressure||subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + type||subj_id), data = dt) 
# full_model <- lmer(response_s ~ time_pressure * (time_pressure_first + type) + (1|subj_id), data = dt)
# summary(full_model)

full_model <- lmer(response_s ~ time_pressure * type + (1|subj_id), data = dt) 
round(summary(full_model)$coefficients, 2)

# 1.2. Without interaction time_pressure*type
restricted_model <- lmer(response_s ~ time_pressure + type + (1|subj_id), data = dt) 
summary(restricted_model)

# 1.3. Tests the necessity of the interaction time_pressure*stim
# 1.3.1. AIC: How much more likely is full_model relative to restricted_model?
aics <- as.data.table(AIC(full_model, restricted_model), keep.rownames = TRUE)
aics[, delta_AIC := AIC - min(AIC)] 
aics[, weights := exp(-0.5*delta_AIC)/sum(exp(-0.5*delta_AIC))]
aics[, weights[1]/weights[2]]

# 1.3.2. F-Test with Kenward-Roger approximation
KRmodcomp(restricted_model, full_model)

# 1.3.3. Anova
anova(restricted_model, full_model, refit = FALSE)

# 2. Pairwise comparisons
emm1 <- emmeans(full_model, ~ type |  time_pressure, pbkrtest.limit = 8206)
pairs(emm1, adjust = "holm") # all pairwise comparisons 

# 2.1. Contrast coefficients
# MMM: beta_type_D > beta_type_B > beta_type_A > beta_type_C
# MDM: beta_type_A > beta_type_D = beta_type_C > beta_type_B

H <- list(MDM1 = c(1, -1, 0, 0), # H1a multidimensional discrete
          MDM2 = c(1, 0, 0, -1),
          MDM3 = c(0, -1, 1, 0),
          MDM4 = c(0, 0, 1, -1),
          MMM1 = c(-1, 0, 0, 1), # H1c (with time pressure) and H2a (without time pressure) multidimensional Minkowski
          MMM2 = c(0, 0, -1, 1),
          MMM3 = c(-1, 1, 0, 0),
          MMM4 = c(0, 1, -1, 0))
 
contrast(emm1, H, adjust = "holm", type = "response") # check if holm method is specified globally or separately for both time_pressure


dt_sub <- dt[, .(block = 1:4, response_s), by = .(subj_id, time_pressure, type, pair = paste0(stim_left, "|", stim_right), trial)]
setkey(dt_sub, subj_id, time_pressure, type, pair, trial)
dt_sub[, trial := rep(1:3, each = 4, times = 4 * 2 * 64)]

# dt_sub <- dt_sub[, .(response_s = mean(response_s, na.rm = TRUE)), by = .(subj_id, time_pressure, type, block)]
# dt_sub <- dcast(dt_sub, formula = subj_id + time_pressure + type ~ block, value.var = "response_s")
# dt_sub[, cor(.SD, use = "pairwise.complete.obs"), by = .(time_pressure), .SDcols = !c("subj_id", "type")]
# dt_sub[, cocron::cronbach.alpha(.SD, standardized = TRUE), by = .(time_pressure), .SDcols = !c("subj_id", "type")]
# cocron::cocron(split(x = dt_sub[, -c("subj_id", "type")], by = c("time_pressure"), keep.by = FALSE), dep = TRUE, standardized = TRUE)
  
dt_sub <- dt_sub[, .(sd = sd(response_s, na.rm = T)), by = .(subj_id, time_pressure, type)]
# dt_sub <- dt_sub[, .(sd = sd(response_s, na.rm = T) / (1 - abs(mean(response_s, na.rm = T) - .5))), by = .(subj_id, time_pressure, type)]

m <- dt_sub[, lmer(sd ~ type + time_pressure + (1|subj_id))]
emm1 <- emmeans(m, ~ type | time_pressure)
pairs(emm1, adjust = "holm") # all pairwise comparisons --> checks for attention focus
emm2 <- emmeans(m, ~ time_pressure | type)
pairs(emm2, adjust = "holm") # all pairwise comparisons --> checks for choice inconsistency

dt_sub <- dcast(dt_sub, formula = subj_id + type ~ time_pressure, value.var = "sd")
colnames(dt_sub)[3:4] <- c("tp", "ntp") 
round(p.adjust(dt_sub[, t.test(x = tp, y = ntp, paired = TRUE), by = type]$p.value, "holm"), 2)
