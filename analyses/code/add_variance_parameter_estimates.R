library(data.table)
library(gdata)
last_100 <- fread("../../data/processed/categorization_main_fitted_parm_gcm.csv")
train_wout_last_100 <- fread("../../data/processed/categorization_main_fitted_parm_gcm_-100.csv")
train_wout_last_100_fam <- fread("../../data/processed/categorization_main_fitted_parm_gcm_-100+fam.csv")
fam <- fread("../../data/processed/categorization_main_fitted_parm_gcm_fam.csv")
test <- fread("../../data/processed/categorization_main_fitted_parm_gcm_test.csv")

dt <- as.data.table(combine(last_100, train_wout_last_100, train_wout_last_100_fam, fam, test))
dt[, c("r", "p") := NULL]
dt <- melt(dt, id.vars = c("metric", "subj_id", "source"), variable.name = "par")
var_par <- dt[, var(value), by = list(metric, subj_id, par)][, mean(V1), by = list(metric, par)] # mean of variance across participants

