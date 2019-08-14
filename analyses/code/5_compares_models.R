# Model comparison with log likelihoods and AIC weights
library(gtools)
library(RVAideMemoire)
library(data.table)
library(cogsciutils)

# 0. Prepares data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
dt <- fread("../../data/results/categorization_data_with_predictions.csv", 
            key = c("subj_id", "trial"))[fread("../../data/processed/categorization_exp_main.csv", 
                                               select = c("subj_id", "trial", "block", "too_slow", "time_pressure_cond", "response", "stim", "stim_type"), 
                                               key = c("subj_id", "trial"),
                                               colClasses = list("character" = "stim"))]

# 0.1. Saves names of the different models
cols <- colnames(dt)[grepl(pattern = "^pred", x = colnames(dt))]
out_cols <- substring(cols, regexpr("_", cols) + 1)

# 1. Aggregate model comparison (H1d & H2b)
# 1.1. Calculates log likelihoods
ll_test <- dt[block == "test" & too_slow == FALSE, lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', options = list(response = "discrete"), na.rm = TRUE)}), .SDcols = cols, by = list(time_pressure_cond, subj_id)]

ll_test_agg <- ll_test[, lapply(.SD, median), .SDcols = cols, by = time_pressure_cond]
setnames(ll_test_agg, cols, out_cols)

# 1.2. Calculates best fitting model
ll_test_agg[, best_fitting_model := names(which.max(.SD)), by = time_pressure_cond]

# 1.3. Calculate weights (equal to AIC weights, because no free parameters)
weights_agg <- ll_test_agg[, exp(.SD - max(.SD)), by = time_pressure_cond, .SDcols = out_cols]
weights_agg[, (out_cols) := .SD/ rowSums(.SD), by = time_pressure_cond]

# 1.4. Calculates pairwise comparisons between models (evidence ratio, see Wagenmakers & Farrell, 2004, p.194)
model_pairs <- permutations(n = 5, r = 2, v = 2:6)
model_comparisons <- rbindlist(apply(model_pairs, 1, function(x) {
  model_pair <- colnames(weights_agg[, ..x])
  x <- c(1, x)
  model_comparison <- weights_agg[, ..x][, .SD/rowSums(.SD), by = time_pressure_cond]
  model_comparison[, .(m1_better_m2 = ifelse(get(model_pair[1]) >= .90,
                                             1,
                                             ifelse(get(model_pair[1]) <= .10, -1, 0)),
                       m1 = model_pair[1],
                       m2 = model_pair[2]), by = time_pressure_cond]
}))

model_comparisons[is.na(model_comparisons$m1_better_m2), m1_better_m2 := 0]
model_comparisons <- model_comparisons[, .(sum = sum(m1_better_m2)), by = list(time_pressure_cond, m1)]
setkey(model_comparisons, time_pressure_cond, sum)

# 1.5. Additional fit indices
mape <- dt[block == "test" & too_slow == FALSE, lapply(.SD, function(x) {MAPE(obs = response, pred = x, response = 'disc', discount = 0)}), .SDcols = cols, by = list(time_pressure_cond, subj_id)]
setnames(mape, cols, out_cols)
mape[time_pressure_cond == TRUE, names(.SD)[which.min(.SD)], .SDcols = 3:7, by = subj_id][, table(V1)]

argmax <- dt[block == "test" & too_slow == FALSE, lapply(.SD, function(x) {mean(choicerule(x = x, type = "argmax") == response)}), .SDcols = cols, by = list(time_pressure_cond)]
setnames(argmax, cols, out_cols)

# 2. Individual model comparison
# 2.1. Testset
# 2.1.1. Calculates log likelihoods
setnames(ll_test, cols, out_cols)

# 2.1.2. Calculates best fitting model
ll_test[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond)]

# 2.1.3. Calculates weights (equal to AIC weights, because no free parameters)
weights <- ll_test[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond), .SDcols = out_cols]
weights[, (out_cols) := round(.SD/ rowSums(.SD), 2), by = list(subj_id, time_pressure_cond)]

# 2.1.4. Calculates distribution of best fitting models for each time pressure condition
weights_above_90 <- weights[, base::max(.SD) >= .90, by = list(time_pressure_cond, subj_id)][, V1]
model_distr <- ll_test[weights_above_90, .N, by = list(time_pressure_cond, best_fitting_model)]
model_distr <- rbind(model_distr, ll_test[, .(best_fitting_model = "disc_unidim", # out_cols[out_cols %in% best_fitting_model == FALSE],
                                              N = 0), by = time_pressure_cond])
ll_test[weights_above_90, table(time_pressure_cond, best_fitting_model)]

# 2.1.5. Chi-squared test (H1e & H2c)
model_distr[, chisq.test(N), by = time_pressure_cond]

# 2.1.6. Pairwise comparisons Chi-squared test (H1e & H2c)
model_distr[time_pressure_cond == TRUE, chisq.multcomp(N, p.method = "holm")]
model_distr[time_pressure_cond == FALSE, chisq.multcomp(N, p.method = "holm")]

# 2.1.7. Calculates pairwise comparisons between models (evidence ratio, see Wagenmakers & Farrell, 2004, p.194) (H1f & H2c)
model_pairs <- permutations(n = 5, r = 2, v = 3:7)
model_comparisons_individual <- rbindlist(apply(model_pairs, 1, function(x) {
  model_pair <- colnames(weights[, ..x])
  x <- c(2, x)
  model_comparison <- weights[, ..x][, .SD/rowSums(.SD), by = time_pressure_cond]
  model_comparison[, .(m1_better_m2 = ifelse(get(model_pair[1]) >= .90,
                                             1,
                                             ifelse(get(model_pair[1]) <= .10, -1, 0)),
                       m1 = model_pair[1],
                       m2 = model_pair[2]), by = time_pressure_cond]
}))
model_comparisons_individual[is.na(model_comparisons_individual$m1_better_m2), m1_better_m2 := 0]
model_comparisons_individual <- model_comparisons_individual[, .(sum = sum(m1_better_m2)), by = list(time_pressure_cond, m1)]
setkey(model_comparisons_individual, time_pressure_cond, sum)

# 2.1.8. Fisher's exact test (H3a, H3b)
model_distr_wide <- dcast(model_distr, time_pressure_cond ~ best_fitting_model)
fisher.test(model_distr_wide[, 2:6])

# 2.1.9. Pairwise comparisons Fisher's exact test (H3a, H3b)
fisher.multcomp(as.matrix(model_distr_wide[, 2:6]), p.method = "holm")

binom.test(x = model_distr_wide[time_pressure_cond == TRUE, disc], 
           n = dt[time_pressure_cond == TRUE, length(unique(subj_id))], 
           p = model_distr_wide[time_pressure_cond == FALSE, disc]/dt[time_pressure_cond == FALSE, length(unique(subj_id))], 
           alternative = "greater")
binom.test(x = model_distr_wide[time_pressure_cond == FALSE, mink], 
           n = dt[time_pressure_cond == FALSE, length(unique(subj_id))], 
           p = model_distr_wide[time_pressure_cond == TRUE, mink]/dt[time_pressure_cond == TRUE, length(unique(subj_id))], 
           alternative = "greater")

# 2.10. Rule-based model (exploratory analyses)
# rule_based_model <- data.table()
# rule_based_model <- rbindlist(lapply(unique(dt$subj_id), function(x) {
#   reg <- dt[block == "test" & subj_id == x, lm(response ~ feature1 + feature2 + feature3)]
#   rule_based_model_temp <- data.table(subj_id = x, predictor = names(reg$coefficients), beta = reg$coefficients, r2 = summary(reg)$r.squared)
#   rule_based_model <- rbind(rule_based_model, rule_based_model_temp)
# }))
# 
# dt[block == "test", pred_lin_mod := predict(lm(response ~ feature1 + feature2 + feature3), newdata = .SD), .SDcols = paste0("feature", 1:3), by = subj_id]
# dt[pred_lin_mod > 1, pred_lin_mod := 1]
# dt[pred_lin_mod < 0, pred_lin_mod := 0]
# cols <- colnames(dt)[grepl(pattern = "^pred", x = colnames(dt))]
# out_cols <- substring(cols, regexpr("_", cols) + 1)
# ll_test_exp <- dt[block == "test" & too_slow == FALSE, 
#                   lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', na.rm = TRUE, options = list(response = "discrete"))}), 
#                   .SDcols = cols, 
#                   by = list(time_pressure_cond, subj_id)]
# setnames(ll_test_exp, cols, out_cols)
# ll_test_exp[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond)]
# weights_exp <- ll_test_exp[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond), .SDcols = out_cols]
# weights_exp[, (out_cols) := round(.SD/ rowSums(.SD), 2), by = list(subj_id, time_pressure_cond)]
# weights_above_90_exp <- weights_exp[, base::max(.SD) >= .90, by = list(time_pressure_cond, subj_id)][, V1]
# model_distr_exp <- ll_test_exp[weights_above_90_exp, .N, by = list(time_pressure_cond, best_fitting_model)]

# 2.11. Log likelihoods per participant and stimulus
ll_test_stim <- dt[block == "test" & too_slow == FALSE & stim_type == "new", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', options = list(response = "discrete"), na.rm = TRUE)}), .SDcols = cols, by = list(time_pressure_cond, subj_id, stim)]
setnames(ll_test_stim, cols, out_cols)
ll_test_stim[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond, stim)]
ll_test_stim[, table(time_pressure_cond, best_fitting_model, stim)]

weights_stim <- ll_test_stim[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond, stim), .SDcols = out_cols]
weights_stim[, (out_cols) := round(.SD/ rowSums(.SD), 2), by = list(subj_id, time_pressure_cond, stim)]

weights_above_90_stim <- weights_stim[, base::max(.SD) >= .90, by = list(time_pressure_cond, subj_id, stim)][, V1]
ll_test_stim[weights_above_90_stim, table(time_pressure_cond, best_fitting_model, stim)]

# 2.2. Learningset (only for sanity check)
# 2.2.1. Calculates log likelihoods
ll_learn <- dt[block == "training", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', options = list(response = "discrete"), discount = 8)}), .SDcols = cols, by = subj_id]
setnames(ll_learn, cols, out_cols)

# 2.2.2. Calculate AICs for learning set
AIC <- ll_learn[, -2 * .SD + 2 * c(5, 5, 2, 2), by = subj_id]
AIC[, random := dt[block == "training", .(V1 = -2 * gof(obs = response, pred = rep(0.5, length(response)), type = "loglik", response = 'disc')), by = subj_id][, V1]]
AIC[, round(disc - mink, 1) == 0]

cat("\n Generated ll_test, ll_test_stim, ll_learn, and AIC \n")