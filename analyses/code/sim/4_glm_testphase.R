# Runs a general linear model with logit link on the test dta
library(data.table)
library(lme4)
library(lmerTest)
library(emmeans) # package emmeans needs to be attached for follow-up tests. 
library(pbkrtest)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio

# 0. Prepares data
dt <- fread("../../../data/processed/similarity_exp_main.csv",
            colClasses = list("character" = "stim_left", "character" = "stim_right"))
dt <- dt[grep("test", block), ]
dt[, time_pressure := factor(time_pressure, levels = c(TRUE, FALSE), labels = c(TRUE, FALSE))]
dt[, subj_id := as.factor(subj_id)]
dt[, type := factor(type, levels = c("A", "B", "C", "D", "I", "V"), labels = c("A", "B", "C", "D", "I", "V"))]

contrasts(dt$type) <- "contr.sum"
contrasts(dt$time_pressure) <- "contr.sum"

# 1. Computes generalized linear models
# 1.1. With interaction time_pressure*type
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + time_pressure||subj_id) + (1 + type||subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + time_pressure||subj_id) + (1|subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 |subj_id) + (1 + type||subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + time_pressure + type||subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + time_pressure||subj_id), data = dt) 
# full_model <- lmer(response ~ time_pressure * (time_pressure_first + type) + (1 + type||subj_id), data = dt) 
full_model <- lmer(response_s ~ time_pressure * (time_pressure_first + type) + (1|subj_id), data = dt) 
summary(full_model)

# 1.2. Without interaction time_pressure*type
restricted_model <- lmer(response_s ~ time_pressure * time_pressure_first + type + (1|subj_id), data = dt) 
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
H <- list(MDM1 = c(1, -1, 0, 0, 0, 0), # H1a multidimensional discrete
          MDM2 = c(1, 0, 0, -1, 0, 0),
          MDM3 = c(0, -1, 1, 0, 0, 0),
          MDM4 = c(0, 0, 1, -1, 0, 0),
          MMM1 = c(-1, 0, 0, 1, 0, 0), # H1c (with time pressure) and H2a (without time pressure) multidimensional Minkowski
          MMM2 = c(0, 0, -1, 1, 0, 0),
          MMM3 = c(-1, 1, 0, 0, 0, 0),
          MMM4 = c(0, 1, -1, 0, 0, 0))
 
contrast(emm1, H, adjust = "holm", type = "response") # check if holm method is specified globally or separately for both time_pressure
