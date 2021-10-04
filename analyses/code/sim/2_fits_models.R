# Fits the parameters of the generalized context model (gcm) using the cognitive models package
rm(list = ls(all = TRUE))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # changes working directory to folder where this script is
library(data.table)
devtools::load_all("~/R/cognitivemodels/")

# source("~/R/cognitivemodels/R/gcm_sim_unidim.R", chdir = TRUE)
# source("~/R/cognitivemodels/R/gcm_sim.R", chdir = TRUE)
# source("~/R/cognitivemodels/R/cogscimodel-class.R", chdir = TRUE)
# devtools::load_all("~/cogsciutils/")

dt <- fread("../../../data/processed/similarity_exp_main.csv",
            colClasses = list("character" = "stim_left", "character" = "stim_right"))

# Makes function to fit test data by time pressure condition with the gcm using 2 (attention: multidim vs unidim) x 2 (metric: Minkowski vs. discrete) = 4 versions
fit <- function(data, metr, i_comb, id = NULL, tp = FALSE, unidim = FALSE){
  combs <- combs[, i_comb]
  args <- list(formula = response_s ~ feature1_l + feature2_l + feature3_l | feature1_r + feature2_r + feature3_r, metric = metr, fixed = c(r = 1, p = 1), choicerule = NULL)
  model <- ifelse(unidim, "gcm_sim_unidim", "gcm_sim")
  m <- do.call(model, c(args, data = list(rbind(data[type == "A", .SD[combs[, 1]], by = trial],
                                                data[type == "B", .SD[combs[, 2]], by = trial],
                                                data[type == "C", .SD[combs[, 3]], by = trial],
                                                data[type == "D", .SD[combs[, 4]], by = trial])), discount = 0))
  return(as.list(c(tp = tp, unidim = unidim, i_comb = paste0(i_comb, collapse = ""), m$parm)))
}

# Generates combinations for fitting and hold-out sample (for each stimulus type A-D choose 2 out of 4 as fitting sample)
combs <- combn(1:4, 2) # which stimuli of a given type to pick for fitting
i_combs <- as.matrix(expand.grid(1:ncol(combs), 1:ncol(combs), 1:ncol(combs), 1:ncol(combs))) # indices for combs for each type A-D

# Splits the data into time pressure (tp) and no time pressure (ntp) subsets
tp_dt <- dt[(time_pressure_first == TRUE & block == "test_1") | (time_pressure_first == FALSE & block == "test_2")]
ntp_dt <- dt[(time_pressure_first == FALSE & block == "test_1") | (time_pressure_first == TRUE & block == "test_2")]

# Fits the parameters of the 4 model versions (2 metrics x 2 attention dimensionalities)
fitted_parm_gcm <- rbindlist(apply(i_combs[1:25, ], 1, function(i_comb) { # fitting sample: 2 out of 4 per type
  rbindlist(lapply(c("d", "m"), function(x) { # metric: discrete vs Minkowski
    rbind(tp_dt[subj_id == "ceok", {print(.GRP); fit(.SD, metr = x, i_comb = i_comb, tp = TRUE)}, by = subj_id], # attention: multidim (tp)
          tp_dt[subj_id == "ceok", {print(.GRP); fit(.SD, metr = x, i_comb = i_comb, tp = TRUE, unidim = TRUE)}, by = subj_id], # attention: unidim (tp)
          ntp_dt[subj_id == "ceok", {print(.GRP); fit(.SD, metr = x, i_comb = i_comb)}, by = subj_id], # attention: multidim (ntp)
          ntp_dt[subj_id == "ceok", {print(.GRP); fit(.SD, metr = x, i_comb = i_comb, unidim = TRUE)}, by = subj_id] # # attention: unidim (ntp)
    )
  }))
}), id = "metric")
fitted_parm_gcm[, metric := ifelse(metric == 1, "disc", "mink")]
# fwrite(fitted_parm_gcm, file = "../../../data/processed/similarity_main_fitted_parm_gcm.csv")
