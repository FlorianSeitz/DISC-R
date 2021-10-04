# ==========================================================================
# Analyses Experiment: 2. Fits cognitive models
# ==========================================================================

# ==========================================================================
study <- 1 # specify the study
# ==========================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, modelr, purrr, devtools, cognitiveutils)
devtools::load_all("~/R/cognitivemodels/")
parallel <- TRUE # fit on a parallel machine (Unix) or single core
if (parallel == TRUE) pacman::p_load(doFuture)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- fread("../../../data/processed/similarity_exp_main.csv", colClasses = list("character" = c("stim_left", "stim_right")))[!type %in% c("fam")]
d[, type_trial := 1:.N, by = .(paste0(type, trial), subj_id, time_pressure)]
# d <- d[subj_id %in% c("deum") & time_pressure == TRUE]
# d <- d[subj_id %in% c("deum", "dezj")]
setkey(d, subj_id, trial_id)

# ==========================================================================
# Specifies to-be-fitted models
# ==========================================================================
source("setup_models.R")
model_list <- list(
  gcm = GCM,                          # gcm (Minkowski)
  gcm_disc = GCM_disc,                # gcm (Discrete)
  gcm_unidim = GCM_unidim,            # gcm (Minkowski, unidimensional)
  gcm_disc_unidim = GCM_disc_unidim   # gcm (Discrete, unidimensional)
)

# ==========================================================================
# Makes cross-validation data sets cv_data for each participant
# ==========================================================================
combs <- combn(1:4, 2) # which stimuli of a given type to pick for fitting
i_combs <- as.matrix(expand.grid(1:ncol(combs), 1:ncol(combs), 1:ncol(combs), 1:ncol(combs))) # indices for combs for each type A-D
its <- expand.grid(subj_id = unique(d$subj_id), time_pressure = unique(d$time_pressure), cv = 1:nrow(i_combs))

# ==========================================================================
# Fits models
# ==========================================================================
if (parallel == TRUE) {
  # registerDoMC(cores = detectCores())
  registerDoFuture()
  plan(multisession, workers = 4L)  ## on MS Windows
  fits <- foreach(x = 1:nrow(its),
                  .combine = "rbind",
                  .inorder = FALSE, 
                  .packages = c("data.table", "cognitivemodels", "modelr", "devtools"),
                  .export = c("model_list", "i_combs", "combs", "GCM", "GCM_disc", "GCM_unidim", "GCM_disc_unidim")) %dopar% {
                    devtools::load_all("~/R/cognitivemodels/")
                    source("setup_models.R", local = TRUE)
                    it <- its[x, ]
                    i_comb <- i_combs[it$cv, ]
                    comb <- combs[, i_comb]
                    d[(subj_id == it$subj_id) & (time_pressure == it$time_pressure), .(
                        cv = paste0(i_comb, collapse = ""),
                        model = names(model_list),
                        map(model_list, exec, dt = .SD, comb = comb)), by = .(subj_id, time_pressure)]
                    }
  #  fits <- fits[cv_data, on = "subj"]
  
} else {
  fits <- d[, .(
    model = names(model_list),
    fit = map(model_list, exec, dt = .SD)),
    by = subj_id]
}

saveRDS(fits, "../../fittedmodels/")
