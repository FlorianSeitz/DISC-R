rm(list = ls(all = TRUE))
library(data.table)
library(cogsciutils)
library(qpcR)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("~/cogscimodels/models/gcm/gcm_unidim.R", chdir = TRUE)

dt <- fread("../../data/processed/categorization_exp_pretest.csv", key = "subj_id")
fitted_parm_gcm <- fread(input = "../../data/processed/categorization_fitted_parm_gcm.csv", key = "subj_id")
fitted_parm_gcm_unidim <- fread(input = "../../data/processed/categorization_fitted_parm_gcm_unidim.csv", key = "subj_id")

predict_gcm <- function(data, metr, id, unidim = FALSE) {
  args <- list(formula = response ~ V1 + V2 + V3, 
               data = data[block == "training"], cat = ~ true_cat, metric = metr, 
               choicerule = "soft")
  if (unidim == FALSE) {
    m <- do.call(gcm, c(args, fixed = list(unlist(fitted_parm_gcm[metric == metr & subj_id == id, -(1:2)])))) # if external variable has same name as a column in datatable, index the external variable with ..
  } else {
    m <- do.call(gcm_unidim, c(args, fixed = list(unlist(fitted_parm_gcm_unidim[metric == metr & subj_id == id, -(1:2)]))))
  }
  c(m$predict(), m$predict(newdata = data[block != "training"]))
}

# Predict for discrete and Minkowski model
dt[, pred_disc := predict_gcm(data = .SD, metr = "disc", id = unique(subj_id)), by = subj_id]
dt[, pred_mink := predict_gcm(data = .SD, metr = "mink", id = unique(subj_id)), by = subj_id]
dt[, pred_disc_unidim := predict_gcm(data = .SD, metr = "disc", id = unique(subj_id), unidim = TRUE), by = subj_id]
dt[, pred_mink_unidim := predict_gcm(data = .SD, metr = "mink", id = unique(subj_id), unidim = TRUE), by = subj_id]
dt[, pred_random := 0.5]
fwrite(dt, "../../data/processed/categorization_data_with_predictions.csv")

# log likelihoods and AICs
# column names
cols <- colnames(dt)[grepl(pattern = "^pred", x = colnames(dt))]
out_cols <- substring(cols, regexpr("_", cols) + 1)

# Calculate log likelihoods for test set
ll <- dt[block == "test", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', na.rm = TRUE)}), .SDcols = cols, by = subj_id]
setnames(ll, cols, out_cols)

# Calculate best fitting model for test set
ll[, best_fitting_model := names(which.max(.SD)), by = subj_id]

# Calculate weights for test set (equal to AIC weights, because no free parameters)
weights <- ll[, exp(.SD - max(.SD)), by = subj_id, .SDcols = out_cols]
weights[, (out_cols) := .SD/ rowSums(.SD), by = subj_id]

# Calculate log likelihoods for learning set
ll <- dt[block == "training", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', discount = 8)}), .SDcols = cols, by = subj_id]
colnames(ll)[colnames(ll) %in% cols] <- out_cols

# Calculate AICs for learning set
AIC <- ll[, -2 * .SD + 2 * c(5, 5, 2, 2), by = subj_id]
AIC[, random := dt[block == "training", .(V1 = -2 * gof(obs = response, pred = rep(0.5, length(response)), type = "loglik", response = 'disc')), by = subj_id][, V1]]


# To do:
# 1. Fit parameters for gcm and unidim --> check
# 2. Log likelihood for testset (gcm and unidim) --> best fitting model, log.likelihood weights / AIC Weights --> check
# 3. AIC for learningset (gcm) verglcih mit log likelihood von random model (da keine freien Parameter ist AIC = LL); expectation: both gcm versions better than random but equally good
# 4. deskriptive Resultate plotten (testset predictions aller modelle pro Stimulus (x-Achse) und observations)