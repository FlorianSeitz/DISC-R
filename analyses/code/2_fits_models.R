rm(list = ls(all = TRUE))
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("~/cogscimodels/models/gcm/gcm_unidim.R", chdir = TRUE)

dt <- fread("../../data/processed/categorization_exp_pretest.csv")

# Fitting with gcm
fit_ll <- function(data, metric, unidim = FALSE){
  args <- list(formula = response ~ feature1 + feature2 + feature3, cat = ~ true_cat, metric = metric, fixed = c(r = 1, p = 1), choicerule = "soft")
  if (unidim == FALSE) {
    m <- do.call(gcm, c(args, data = list(data[block == "training", ]), discount = 8))
  } else {
    m <- do.call(gcm_unidim, c(args, data = list(data), discount = sum(data$block == "training")))
  }
  return(as.list(c(m$parm)))
}

# Fit Parameters of discrete and Minkowsi model 
fitted_parm_gcm <- rbindlist(lapply(c("d", "m"), function(x) dt[block == "training", {print(.GRP); fit_ll(.SD, metric = x)}, by = subj_id]), id = "metric")
fitted_parm_gcm[, metric := ifelse(metric == 1, "disc", "mink")]
fwrite(fitted_parm_gcm, file = "../../data/processed/categorization_fitted_parm_gcm.csv")

# Fit parameters to unidim model
fitted_parm_gcm_unidim <- rbindlist(lapply(c("d", "m"), function(x) dt[, {print(.GRP); fit_ll(.SD, metric = x, unidim = TRUE)}, by = subj_id]), id = "metric")
fitted_parm_gcm_unidim[, metric := ifelse(metric == 1, "disc", "mink")]
fwrite(fitted_parm_gcm_unidim, file = "../../data/processed/categorization_fitted_parm_gcm_unidim.csv")

