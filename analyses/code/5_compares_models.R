# Compares models with ll and AIC weights

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("3_predicts_models.R", chdir = TRUE)

# log likelihoods and AICs
# column names
cols <- colnames(dt)[grepl(pattern = "^pred", x = colnames(dt))]
out_cols <- substring(cols, regexpr("_", cols) + 1)

# Calculate log likelihoods for test set
ll_test <- dt[block == "test" & too_slow == FALSE, lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', na.rm = TRUE)}), .SDcols = cols, by = list(subj_id, time_pressure_cond)]
setnames(ll_test, cols, out_cols)

# Calculate best fitting model for test set
ll_test[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond)]

# Calculate weights for test set (equal to AIC weights, because no free parameters)
weights <- ll_test[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond), .SDcols = out_cols]
weights[, (out_cols) := round(.SD/ rowSums(.SD), 2), by = list(subj_id, time_pressure_cond)]

weights_above_90 <- weights[, base::max(.SD) >= .90, by = list(time_pressure_cond, subj_id)][, V1]
ll_test[weights_above_90, table(time_pressure_cond, best_fitting_model)]

# Calculate log likelihoods for learning set
ll_learn <- dt[block == "training", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', discount = 8)}), .SDcols = cols, by = subj_id]
setnames(ll_learn, cols, out_cols)

# Calculate AICs for learning set
AIC <- ll_learn[, -2 * .SD + 2 * c(5, 5, 2, 2), by = subj_id]
AIC[, random := dt[block == "training", .(V1 = -2 * gof(obs = response, pred = rep(0.5, length(response)), type = "loglik", response = 'disc')), by = subj_id][, V1]]

cat("\n Generated ll_test, weights, ll_learn, and AIC \n")
