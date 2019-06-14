rm(list = ls(all = TRUE))
library(R6)
library(data.table)
library(cogscimodels)
library(cogsciutils)
library(qpcR)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("~/cogscimodels/R/cogscimodel-class.R", chdir = TRUE)
source("~/cogscimodels/R/gcm.R", chdir = TRUE)
source("~/cogscimodels/R/gcm_unidim.R", chdir = TRUE)

dt <- fread("../../data/processed/categorization_exp_main.csv", key = "subj_id", colClasses = list("character" = "stim"))
fitted_parm_gcm <- fread(input = "../../data/processed/categorization_main_fitted_parm_gcm.csv", key = "subj_id")
fitted_parm_gcm_unidim <- fread(input = "../../data/processed/categorization_main_fitted_parm_gcm_unidim.csv", key = "subj_id")

predict_gcm <- function(data, metr, id, unidim = FALSE) {
  args <- list(formula = response ~ feature1 + feature2 + feature3, 
               data = data[block == "training"], cat = ~ true_cat, metric = metr, 
               choicerule = "soft")
  if (unidim == FALSE) {
    m <- do.call(gcm, c(args, fixed = list(unlist(fitted_parm_gcm[metric == metr & subj_id == id, -(1:2)])))) # if external variable has same name as a column in datatable, index the external variable with ..
  } else {
    m <- do.call(gcm_unidim, c(args, fixed = list(unlist(fitted_parm_gcm_unidim[metric == metr & subj_id == id, -(1:2)]))))
  }
  c(m$predict(), m$predict(newdata = data[block != "training", ]))
}

# Predict for discrete and Minkowski model
dt[, pred_disc := predict_gcm(data = .SD, metr = "disc", id = unique(subj_id)), by = subj_id]
dt[, pred_mink := predict_gcm(data = .SD, metr = "mink", id = unique(subj_id)), by = subj_id]
dt[, pred_disc_unidim := predict_gcm(data = .SD, metr = "disc", id = unique(subj_id), unidim = TRUE), by = subj_id]
dt[, pred_mink_unidim := predict_gcm(data = .SD, metr = "mink", id = unique(subj_id), unidim = TRUE), by = subj_id]
dt[, pred_random := 0.5]
fwrite(dt[, c("subj_id", "trial", grep("^pred", names(dt), value = TRUE)), with = FALSE], "../../data/results/categorization_data_with_predictions.csv")
cat("\n Loaded dt with predictions \n")