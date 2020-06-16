rm(list = ls(all = TRUE))
library(data.table)
# library(cogscimodels)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("~/cogscimodels/R/gcm_sim_unidim.R", chdir = TRUE)
source("~/cogscimodels/R/gcm_sim.R", chdir = TRUE)
source("~/cogscimodels/R/cogscimodel-class.R", chdir = TRUE)
devtools::load_all("~/cogsciutils/")

dt <- fread("../../../data/processed/similarity_exp_main.csv",
            colClasses = list("character" = "stim_left", "character" = "stim_right"))

# Fitting learningset with gcm
fit_ll <- function(data, metr, i_comb, id = NULL, tp = FALSE, unidim = FALSE){
  combs <- combs[, i_comb]
  args <- list(formula = response ~ feature1_l + feature2_l + feature3_l | feature1_r + feature2_r + feature3_r, metric = metr, fixed = c(r = 1, p = 1), choicerule = NULL)
  model <- ifelse(unidim, "gcm_sim_unidim", "gcm_sim")
  m <- do.call(model, c(args, data = list(rbind(data[type == "A", .SD[combs[, 1]], by = trial],
                                                data[type == "B", .SD[combs[, 2]], by = trial],
                                                data[type == "C", .SD[combs[, 3]], by = trial],
                                                data[type == "D", .SD[combs[, 4]], by = trial])), discount = 0))
  return(as.list(c(tp = tp, unidim = unidim, i_comb = paste0(i_comb, collapse = ""), m$parm)))
}

# Combinations for fitting and hold-out sample
combs <- combn(1:4, 2) # which stimuli of a given type to pick for fitting
i_combs <- as.matrix(expand.grid(1:ncol(combs), 1:ncol(combs), 1:ncol(combs), 1:ncol(combs))) # indices for types A-D

# Fit model parameters
tp_dt <- dt[(time_pressure_first == TRUE & block == "test_1") | (time_pressure_first == FALSE & block == "test_2")] # time pressure data
ntp_dt <- dt[(time_pressure_first == FALSE & block == "test_1") | (time_pressure_first == TRUE & block == "test_2")] # no time pressure data

fitted_parm_gcm <- rbindlist(lapply(c("d", "m"), function(x) { # fits both the discrete and the Minkowski metric
  rbindlist(apply(i_combs[1:2, ], 1, function(i_comb) { # fits for all possible combinations 2 out of 4
    rbind(tp_dt[subj_id == "gbwe", {print(.GRP); fit_ll(.SD, metr = x, i_comb = i_comb, tp = TRUE)}, by = subj_id], # fits multidimensional models
          tp_dt[subj_id == "gbwe", {print(.GRP); fit_ll(.SD, metr = x, i_comb = i_comb, tp = TRUE, unidim = TRUE)}, by = subj_id],  # fits unidimensional models
          ntp_dt[subj_id == "gbwe", {print(.GRP); fit_ll(.SD, metr = x, i_comb = i_comb)}, by = subj_id], # fits multidimensional models
          ntp_dt[subj_id == "gbwe", {print(.GRP); fit_ll(.SD, metr = x, i_comb = i_comb, unidim = TRUE)}, by = subj_id] # fits unidimensional models
    )
  }))
}), id = "metric")
fitted_parm_gcm[, metric := ifelse(metric == 1, "disc", "mink")]
fwrite(fitted_parm_gcm, file = "../../../data/processed/similarity_main_fitted_parm_gcm_tp.csv")
