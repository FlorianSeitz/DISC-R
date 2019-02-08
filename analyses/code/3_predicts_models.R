rm(list = ls(all = TRUE))
library(data.table)
library(cogsciutils)
library(qpcR)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("~/cogscimodels/models/gcm/gcm_unidim.R", chdir = TRUE)

dt <- fread("../../data/processed/categorization_exp_pretest.csv", key = "subj_id", colClasses = list("character" = "stim"))
fitted_parm_gcm <- fread(input = "../../data/processed/categorization_pretest_fitted_parm_gcm.csv", key = "subj_id")
fitted_parm_gcm_unidim <- fread(input = "../../data/processed/categorization_pretest_fitted_parm_gcm_unidim.csv", key = "subj_id")

predict_gcm <- function(data, metr, id, unidim = FALSE) {
  args <- list(formula = response ~ feature1 + feature2 + feature3, 
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
# fwrite(dt, "../../data/processed/categorization_data_with_predictions.csv")
cat("\n Loaded dt with predictions \n")