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
d <- fread("../../../data/processed/categorization_exp_main.csv", colClasses = c("stim" = "character"))[block == "training"]

# ==========================================================================
# Specifies to-be-fitted models
# ==========================================================================
source("setup_models.R")
model_list <- list(
  gcm = GCM,                          # gcm (Minkowski)
  gcm_disc = GCM_disc,                # gcm (Discrete)
  # gcm_small = GCM_small,              # gcm (Minkowski, small discount)
  # gcm_disc_small = GCM_disc_small     # gcm (Discrete, small discount)
  rule_seitz2021 = RULE_seitz2021     # decision tree (first feature 2, then feature 1 or 3)
)

# ==========================================================================
# Makes cross-validation data sets cv_data for each participant
# ==========================================================================
# k <- 20 # number of folds
# set.seed(42)
# cv_data <- d[, .(cvfold = list(crossv_kfold(.SD, k))), by = subj]

# ==========================================================================
# Fits models
# ==========================================================================
if (parallel == TRUE) {
  # registerDoMC(cores = detectCores())
  registerDoFuture()
  plan(multisession, workers = 4L)  ## on MS Windows
  setkey(d, "subj_id")  
  fits <- foreach(x = unique(d$subj_id),
                  .combine = "rbind",
                  .inorder = FALSE, 
                  .packages = c("data.table", "cognitivemodels", "modelr", "devtools"),
                  .export = c("model_list", "GCM", "GCM_disc", "GCM_small", "GCM_disc_small", "RULE_seitz2021")) %dopar% {
                    devtools::load_all("~/R/cognitivemodels/")
                    source("setup_models.R", local = TRUE)
                    d[.(x), .(
                      model = names(model_list),
                      map(model_list, exec, dt = .SD)), by = subj_id]
                  }   
  
  #  fits <- fits[cv_data, on = "subj"]
  
} else {
  fits <- d[, .(
    model = names(model_list),
    fit = map(model_list, exec, dt = .SD)),
    by = subj]
}

# ==========================================================================
# Specifies to-be-fitted models
# ==========================================================================
dt <- fread("../../../data/processed/categorization_exp_main.csv", colClasses = c("stim" = "character"))[!is.na(response)]
dt <- merge(dt[, -"true_cat"], unique(dt[block == "training" | stim_type == "new", .(stim, true_cat)]), by = "stim")[order(subj_id, trial)]
d <- dt[stim_type != "new"]

fits <- readRDS(sub("X", study,"../../fittedmodels/studyX_cognitive_models.rds"))
pars <- fits[model == "gcm", V2[[1]]$parm, by = subj_id]

source("setup_models.R")
model_list <- list(
  gcm_unidim = GCM_unidim,            # gcm unidim (Minkowski similarity)
  gcm_disc_unidim = GCM_disc_unidim   # gcm unidim (Discrete similarity)
)

# ==========================================================================
# Fits models
# ==========================================================================
skip <- 85; end <- 108 # used for sourcing below (keep and ignore)
if (parallel == TRUE) {
  # registerDoMC(cores = detectCores())
  registerDoFuture()
  plan(multisession, workers = 4L)  ## on MS Windows
  setkey(d, "subj_id")  
  fits_unidim <- foreach(x = unique(d$subj_id),
                  .combine = "rbind",
                  .inorder = FALSE, 
                  .packages = c("data.table", "cognitivemodels", "cognitiveutils", "modelr", "devtools"),
                  .export = c("model_list", "pars", "GCM_unidim", "GCM_disc_unidim")) %dopar% {
                    devtools::load_all("~/R/cognitivemodels/")
                    source("setup_models.R", local = TRUE)
                    d[.(x), .(
                      model = names(model_list),
                      map(model_list, exec, dt = .SD, fix = as.list(pars[subj_id == .(x), 5:8]))), by = subj_id]
                  }   
  
} else {
  fits_unidim <- d[, .(
    model = names(model_list),
    fit = map(model_list, exec, dt = .SD)),
    by = subj]
}

invalid_subj_ids <- fits_unidim[, is.na(V2[[1]]$gofvalue), by = subj_id][V1 == TRUE, subj_id]
fits_unidim_valid <- fits_unidim[!subj_id %in% invalid_subj_ids]
d <- dt[subj_id %in% invalid_subj_ids]
pars[subj_id %in% invalid_subj_ids, tau := .1]
lines <- paste(scan("2-cognitive-modeling.R", what=character(), skip = skip, nlines = end-skip, sep = '\n'), collapse = "\n")
source(textConnection(lines)) # Or execute lines 86-108 manually again, if it does not work.
pars <- fits[model == "gcm", V2[[1]]$parm, by = subj_id]

d <- dt[block == "training"]
fits_unidim <- rbind(fits_unidim_valid, fits_unidim)
preds <- fits_unidim[, V2[[1]]$parm, by = .(subj_id, model)]
preds[subj_id %in% invalid_subj_ids, tau := rep(pars[subj_id %in% invalid_subj_ids, tau], each = 2)]
fits_unidim[, V2 := sapply(1:nrow(preds), function(i) {
  gcm_unidim(formula = response ~ feature1 + feature2 + feature3, 
             cat = ~ true_cat, 
             data = d[subj_id == preds[i, subj_id]], 
             metric = ifelse(preds[i, model] == "gcm_unidim", "minkowski", "discrete"), 
             fixed = as.list(preds[i, 3:9]), 
             choicerule = "softmax")})]
fits <- rbind(fits, fits_unidim)[order(subj_id)]

saveRDS(fits, "../../fittedmodels/study1_cognitive_models.rds")
