# Makes predictions for hold out sample using fitted parameter estimates
rm(list = ls(all = TRUE))
library(R6)
library(data.table)
library(qpcR)
library(Formula)
library(Rsolnp)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("~/cogscimodels/R/cogscimodel-class.R", chdir = TRUE)
source("~/cogscimodels/R/gcm_sim.R", chdir = TRUE)
source("~/cogscimodels/R/gcm_sim_unidim.R", chdir = TRUE)
devtools::load_all("~/cogsciutils/")

# Prepares data sets, one containing the estimated parameter (fitted_parm_gcm) and one containing the raw data (dt)
files <- list.files(path = "../../../data/raw/similarity_fitting_results/", pattern = "results", full.names = TRUE)
fitted_parm_gcm <- rbindlist(lapply(files, fread, fill = TRUE))[!is.na(subj_id), ]
fwrite(fitted_parm_gcm, "../../../data/processed/similarity_main_fitted_parm_gcm.csv")

dt <- fread("../../../data/processed/similarity_exp_main.csv",
            colClasses = list("character" = "stim_left", "character" = "stim_right"))
# for(i in 1:64) {
#   dt[subj_id == unique(dt$subj_id)[i], subj_id := unique(fitted_parm_gcm$subj_id)[i]]
# }
dt <- dt[grep("test", block), ]

# Function that makes predictions for a given model version and data
predict_gcm <- function(data, metr, uni, id, tp_cond, comb) {
  combs <- combs[, i_combs[which(comb == i_combs_id), ]] # selects for each type the stimuli indexes belonging to the fitting sample

  fixed <- fitted_parm_gcm[subj_id == id & i_comb == comb & metric == metr & unidim == uni & tp == tp_cond, 5:11] # selects the parameter estimates
  args <- list(formula = response_s ~ feature1_l + feature2_l + feature3_l | feature1_r + feature2_r + feature3_r,
               data = rbind(data[type == "A", .SD[combs[, 1]], by = trial],
                            data[type == "B", .SD[combs[, 2]], by = trial],
                            data[type == "C", .SD[combs[, 3]], by = trial],
                            data[type == "D", .SD[combs[, 4]], by = trial]),
                 metric = metr, choicerule = NULL, fixed = fixed)
  model <- ifelse(uni, "gcm_sim_unidim", "gcm_sim")
  m <- do.call(model, args)
  m$predict(newdata = data)
}

# Generates combinations for fitting and hold-out sample (for each stimulus type A-D choose 2 out of 4 as fitting sample)
combs <- combn(1:4, 2) # which stimuli of a given type to pick for fitting
i_combs <- as.matrix(expand.grid(1:ncol(combs), 1:ncol(combs), 1:ncol(combs), 1:ncol(combs))) # indices for combs for each type A-D
i_combs_id <- apply(i_combs, 1, paste0, collapse = "")

sapply(unique(fitted_parm_gcm$subj_id), function(id) {
  print(id)
  n_i_combs <- fitted_parm_gcm[subj_id == id, nrow(.SD) == 8, by = i_comb][V1 == TRUE, i_comb] # only now, when not all fits are done
  rep_list <- replicate(length(n_i_combs), dt[subj_id == id], simplify = FALSE)
  rep_dt <- do.call("rbind", mapply(cbind, rep_list, "i_comb" = n_i_combs, SIMPLIFY = FALSE))

  # Predict for discrete and Minkowski model
  rep_dt[, pred_disc := predict_gcm(data = .SD, metr = "disc", uni = FALSE, id = unique(subj_id), tp_cond = unique(time_pressure), comb = unique(i_comb)), by = list(i_comb, time_pressure)]
  rep_dt[, pred_mink := predict_gcm(data = .SD, metr = "mink", uni = FALSE, id = unique(subj_id), tp_cond = unique(time_pressure), comb = unique(i_comb)), by = list(i_comb, time_pressure)]
  rep_dt[, pred_disc_unidim := predict_gcm(data = .SD, metr = "disc", uni = TRUE, id = unique(subj_id), tp_cond = unique(time_pressure), comb = unique(i_comb)), by = list(i_comb, time_pressure)]
  rep_dt[, pred_mink_unidim := predict_gcm(data = .SD, metr = "mink", uni = TRUE, id = unique(subj_id), tp_cond = unique(time_pressure), comb = unique(i_comb)), by = list(i_comb, time_pressure)]
  rep_dt[, pred_random := 0.5]
  fwrite(rep_dt, paste0("../../../data/results/similarity_data_with_predictions_", id, ".csv"))
})
cat("\n Loaded dt with predictions \n")
