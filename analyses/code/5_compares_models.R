# Model comparison with log likelihoods and AIC weights
library(gtools)

# 0. Prepares data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("3_predicts_models.R", chdir = TRUE)

# 0.1. Saves names of the different models
cols <- colnames(dt)[grepl(pattern = "^pred", x = colnames(dt))]
out_cols <- substring(cols, regexpr("_", cols) + 1)

# 1. Aggregate model comparison (H1d & H2b)
# 1.1. Calculates log likelihoods
ll_test_agg <- dt[block == "test" & too_slow == FALSE, lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', na.rm = TRUE)}), .SDcols = cols, by = list(time_pressure_cond)]
setnames(ll_test_agg, cols, out_cols)

# 1.2. Calculates best fitting model
ll_test_agg[, best_fitting_model := names(which.max(.SD)), by = time_pressure_cond]

# 1.3. Calculate weights (equal to AIC weights, because no free parameters)
weights_agg <- ll_test[, exp(.SD - max(.SD)), by = time_pressure_cond, .SDcols = out_cols]
weights_agg[, (out_cols) := .SD/ rowSums(.SD), by = time_pressure_cond]

# 1.4. Calculates pairwise comparisons between models  (evidence ratio, see Wagenmakers & Farrell, 2004, p.194)
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
mape <- dt[block == "test" & too_slow == FALSE, lapply(.SD, function(x) {MAPE(obs = response, pred = x, response = 'disc', discount = 0)}), .SDcols = cols, by = list(time_pressure_cond)]
setnames(mape, cols, out_cols)

argmax <- dt[block == "test" & too_slow == FALSE, lapply(.SD, function(x) {mean(choicerule(x = x, type = "argmax") == response)}), .SDcols = cols, by = list(time_pressure_cond)]
setnames(argmax, cols, out_cols)

# 2. Individual model comparison (H1e & H2c)
# 2.1. Testset (H1e & H2c)
# 2.1.1. Calculates log likelihoods
ll_test <- dt[block == "test" & too_slow == FALSE, lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', na.rm = TRUE)}), .SDcols = cols, by = list(subj_id, time_pressure_cond)]
setnames(ll_test, cols, out_cols)

# 2.1.2. Calculates best fitting model
ll_test[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond)]

# 2.1.3. Calculates weights (equal to AIC weights, because no free parameters)
weights <- ll_test[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond), .SDcols = out_cols]
weights[, (out_cols) := round(.SD/ rowSums(.SD), 2), by = list(subj_id, time_pressure_cond)]

# 2.1.4. Calculates distribution of best fitting models for each time pressure condition
weights_above_90 <- weights[, base::max(.SD) >= .90, by = list(time_pressure_cond, subj_id)][, V1]
model_distr <- ll_test[weights_above_90, .N, by = list(time_pressure_cond, best_fitting_model)]
model_distr <- rbind(model_distr, ll_test[, .(best_fitting_model = out_cols[out_cols %in% best_fitting_model == FALSE],
                                              N = 0), by = time_pressure_cond])

# 2.1.5. Chi-squared test
model_distr[, chisq.test(N), by = time_pressure_cond]

# 2.2. Learningset (only for sanity check)
# 2.2.1. Calculates log likelihoods
ll_learn <- dt[block == "training", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', discount = 8)}), .SDcols = cols, by = subj_id]
setnames(ll_learn, cols, out_cols)

# 2.2.2. Calculate AICs for learning set
AIC <- ll_learn[, -2 * .SD + 2 * c(5, 5, 2, 2), by = subj_id]
AIC[, random := dt[block == "training", .(V1 = -2 * gof(obs = response, pred = rep(0.5, length(response)), type = "loglik", response = 'disc')), by = subj_id][, V1]]
AIC[, round(disc - mink, 1) == 0]

cat("\n Generated ll_test, weights, ll_learn, and AIC \n")
