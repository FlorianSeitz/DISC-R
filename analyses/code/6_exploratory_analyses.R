# Exploratory analyses
rm(list = ls(all = TRUE))
library(gtools)
library(RVAideMemoire)
library(data.table)
library(cogsciutils)
library(splitstackshape)
set.seed(932)
options(contrasts = c("contr.sum", "contr.poly"))

# 0. Prepares data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("source_partial.R")
dt <- fread("../../data/results/categorization_data_with_predictions.csv", 
            key = c("subj_id", "trial"))[fread("../../data/processed/categorization_exp_main.csv", 
                                               select = c("subj_id", "trial", "block", "too_slow", "time_pressure_cond", 
                                                          "response", "feature1", "feature2", "feature3", "stim", "stim_type"), 
                                               key = c("subj_id", "trial"),
                                               colClasses = list("character" = "stim"))]

# 1. Rule-based model (exploratory analyses)
# Fitting on complete test phase data
rule_based_model <- data.table()
rule_based_model <- rbindlist(lapply(unique(dt$subj_id), function(x) {
  # Fitting on last 100 trials of training phase
  reg <- dt[block == "training", tail(.SD, 100), keyby = subj_id][subj_id == x, glm(response ~ feature1 + feature2 + feature3, family = binomial(link = "logit"))]
  nullreg <- dt[block == "training", tail(.SD, 100), keyby = subj_id][subj_id == x, glm(response ~ 1, family = binomial(link = "logit"))]
  # # Fitting on test phase
  # reg <- dt[block == "test" & subj_id == x, glm(response ~ feature1 + feature2 + feature3, family = binomial(link = "logit"))]
  # nullreg <- dt[block == "test" & subj_id == x, glm(response ~ 1, family = binomial(link = "logit"))]
  
  r2 <- 1 - logLik(reg)/logLik(nullreg)
  rule_based_model_temp <- data.table(subj_id = x, predictor = names(reg$coefficients), beta = reg$coefficients, r2 = r2)
  rule_based_model <- rbind(rule_based_model, rule_based_model_temp)
}))

# Fitting on training phase, but predicting test phase
reg <- dt[block == "training", tail(.SD, 100), keyby = subj_id][, glm(response ~ feature1 + feature2 + feature3, family = binomial(link = "logit"))]
coefs <- dt[block == "training", .(coefficient = glm(response ~ feature1 + feature2 + feature3,
                                                     family = binomial(link = "logit"))$coefficients,
                                   name = names(glm(response ~ feature1 + feature2 + feature3,
                                                    family = binomial(link = "logit"))$coefficients)), by = subj_id]
coefs <- dcast(coefs, subj_id ~ name, value.var = "coefficient")
colnames(coefs) <- c("subj_id", "b0", "b1", "b2", "b3")
dt <- merge(dt, coefs)
dt[block == "test", pred_lin_mod := 1 / (1 + 1/exp(b0 + feature1 * b1 + feature2 * b2 + feature3 * b3))]

# # Fitting and predicting for test phase
# dt[block == "test", pred_lin_mod := predict.glm(glm(response ~ feature1 + feature2 + feature3, family = binomial(link = "logit")),
#                                                 newdata = .SD,
#                                                 type = "response"), .SDcols = paste0("feature", 1:3), by = subj_id]
dt[pred_lin_mod > 1, pred_lin_mod := 1]
dt[pred_lin_mod < 0, pred_lin_mod := 0]
cols <- colnames(dt)[grepl(pattern = "^pred", x = colnames(dt))]
out_cols <- substring(cols, regexpr("_", cols) + 1)
ll_test_exp <- dt[block == "test" & too_slow == FALSE,
                  lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', na.rm = TRUE, options = list(response = "discrete"))}),
                  .SDcols = cols,
                  by = list(time_pressure_cond, subj_id)]
setnames(ll_test_exp, cols, out_cols)
ll_test_exp[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond)]
weights_exp <- ll_test_exp[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond), .SDcols = out_cols]
weights_exp[, (out_cols) := round(.SD/ rowSums(.SD), 2), by = list(subj_id, time_pressure_cond)]
weights_above_90_exp <- weights_exp[, base::max(.SD) >= .90, by = list(time_pressure_cond, subj_id)][, V1]
model_distr_exp <- ll_test_exp[weights_above_90_exp, .N, by = list(time_pressure_cond, best_fitting_model)]
setkey(model_distr_exp, time_pressure_cond, N)

model_pairs <- permutations(n = 6, r = 2, v = 3:8)
model_comparisons_individual <- rbindlist(apply(model_pairs, 1, function(x) {
  model_pair <- colnames(weights_exp[, ..x])
  x <- c(2, x)
  model_comparison <- weights_exp[, ..x][, .SD/rowSums(.SD), by = time_pressure_cond]
  model_comparison[, .(time_pressure_cond = time_pressure_cond,
                       m1_better_m2 = ifelse(get(model_pair[1]) >= .90,
                                             1,
                                             ifelse(get(model_pair[1]) <= .10, -1, 0)),
                       m1 = model_pair[1],
                       m2 = model_pair[2])]
}))
model_comparisons_individual[is.na(m1_better_m2), m1_better_m2 := 0]
model_comparisons_individual <- model_comparisons_individual[, .(sum = sum(m1_better_m2)), by = list(time_pressure_cond, m1)]
setkey(model_comparisons_individual, time_pressure_cond, sum)

# Fitting on random half of test phase data and predicting the other half
# rule_based_model_half <- data.table()
# dt_half <- data.table()
# for(x in unique(dt$subj_id)) {
#   subj_dt <- dt[block == "test" & subj_id == x, ]
#   data_to_fit <- stratified(subj_dt, group = "stim", size = 7)
#   data_to_predict <- subj_dt[!data_to_fit]
#   
#   # Fitting
#   reg <- data_to_fit[, lm(response ~ feature1 + feature2 + feature3)]
#   rule_based_model_half_temp <- data.table(subj_id = x, predictor = names(reg$coefficients), beta = reg$coefficients, r2 = summary(reg)$r.squared)
#   rule_based_model_half <- rbind(rule_based_model_half, rule_based_model_half_temp)
#   
#   # Predicting
#   data_to_predict[, pred_lin_mod := predict(object = reg, newdata = .SD)]
#   dt_half <- rbind(dt_half, data_to_predict)
# }
# 
# dt_half[pred_lin_mod > 1, pred_lin_mod := 1]
# dt_half[pred_lin_mod < 0, pred_lin_mod := 0]
# ll_test_exp_half <- dt_half[block == "test" & too_slow == FALSE,
#                   lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', na.rm = TRUE, options = list(response = "discrete"))}),
#                   .SDcols = cols,
#                   by = list(time_pressure_cond, subj_id)]
# setnames(ll_test_exp_half, cols, out_cols)
# ll_test_exp_half[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond)]
# weights_exp_half <- ll_test_exp_half[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond), .SDcols = out_cols]
# weights_exp_half[, (out_cols) := round(.SD/ rowSums(.SD), 2), by = list(subj_id, time_pressure_cond)]
# weights_above_90_exp_half <- weights_exp_half[, base::max(.SD) >= .90, by = list(time_pressure_cond, subj_id)][, V1]
# model_distr_exp_half <- ll_test_exp_half[weights_above_90_exp_half, .N, by = list(time_pressure_cond, best_fitting_model)]

# 2. Discrete threshold model (exploratory analyses)
dt_exploratory <- merge(dt, fread("../../data/results/categorization_data_with_predictions_exploratory.csv", key = c("subj_id", "trial"), 
                                  select = c("subj_id", "trial", "pred_disc_threshold", "pred_disc_threshold_unidim")), by = c("subj_id", "trial"))
fitted_parm <- fread(input = "../../data/processed/categorization_main_fitted_parm_dtm_test.csv", key = "subj_id")
dt_exploratory[subj_id %in% fitted_parm[gamma == 0, subj_id], pred_disc_threshold := pred_disc]
dt_exploratory[, pred_disc := NULL]
fitted_parm <- fread(input = "../../data/processed/categorization_main_fitted_parm_dtm_unidim_test.csv", key = "subj_id")
dt_exploratory[subj_id %in% fitted_parm[gamma == 0, subj_id], pred_disc_threshold_unidim := pred_disc_unidim]
dt_exploratory[, pred_disc_unidim := NULL]

tbl <- merge(dt[, list(subj_id, time_pressure_cond)][, .(time_pressure_cond = unique(time_pressure_cond)), by = subj_id], fitted_parm)[, table(gamma, time_pressure_cond)]
fisher.test(x = matrix(tbl, ncol = 2))

cols <- colnames(dt_exploratory)[grepl(pattern = "^pred", x = colnames(dt_exploratory))]
out_cols <- substring(cols, regexpr("_", cols) + 1)
ll_test <- dt_exploratory[block == "test" & too_slow == FALSE, lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', options = list(response = "discrete"), na.rm = TRUE)}), .SDcols = cols, by = list(time_pressure_cond, subj_id)]

source_partial(file = "5_compares_models.R", start = 27, end = 53)

dt_long <- melt(dt_exploratory, measure.vars = list(grep("pred", colnames(dt_exploratory))), variable.name = "model", value.name = "pred")
levels(dt_long$model) <- out_cols
add_ind <- dt_long[block == "test" & too_slow == FALSE, .(mape = MAPE(obs = response, pred = pred, response = 'disc', discount = 0),
                                                          argmax = mean(choicerule(x = pred, type = "argmax") == response),
                                                          mse = MSE(obs = response, pred = pred, response = 'disc', discount = 0)), by = list(time_pressure_cond, model)]
ll_ind <- dt_long[block == "test" & too_slow == FALSE, .(ll = gof(obs = response, 
                                                                  pred = pred, 
                                                                  type = "loglik", 
                                                                  response = 'disc', 
                                                                  options = list(response = "discrete"), 
                                                                  na.rm = TRUE)), by = list(time_pressure_cond, model, subj_id)][, .(M_LL = mean(ll),
                                                                                                                                     Md_LL = median(ll),
                                                                                                                                     SD_LL = sd(ll)), by = list(time_pressure_cond, model)]

source_partial(file = "5_compares_models.R", start = 73, end = 84)
model_distr <- rbind(model_distr, ll_test[, .(best_fitting_model = "disc_threshold_unidim", # out_cols[out_cols %in% best_fitting_model == FALSE],
                                              N = 0), by = time_pressure_cond])
ll_test[weights_above_90, table(time_pressure_cond, best_fitting_model)]

model_pairs <- permutations(n = 5, r = 2, v = 3:7)
model_comparisons_individual <- rbindlist(apply(model_pairs, 1, function(x) {
  model_pair <- colnames(weights[, ..x])
  x <- c(2, x)
  model_comparison <- weights[, ..x][, .SD/rowSums(.SD), by = time_pressure_cond]
  model_comparison[, .(time_pressure_cond = time_pressure_cond,
                       m1_better_m2 = ifelse(get(model_pair[1]) >= .90,
                                             1,
                                             ifelse(get(model_pair[1]) <= .10, -1, 0)),
                       m1 = model_pair[1],
                       m2 = model_pair[2])]
}))
model_comparisons_individual[is.na(m1_better_m2), m1_better_m2 := 0]
model_comparisons_individual <- model_comparisons_individual[, .(sum = sum(m1_better_m2)), by = list(time_pressure_cond, m1)]
setkey(model_comparisons_individual, time_pressure_cond, sum)
