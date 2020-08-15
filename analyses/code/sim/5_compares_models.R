# Model comparison with log likelihoods and AIC weights
library(gtools)
library(RVAideMemoire)
library(data.table)
library(splitstackshape)
library(esc)
set.seed(932)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # changes wd to folder of script; requires RStudio
devtools::load_all("~/cogsciutils/")

# 0. Prepares data
files <- list.files(path = "../../../data/results/", pattern = c("similarity.+predictions"), full.names = TRUE)
dt <- rbindlist(lapply(files, fread, fill = TRUE, colClasses = list("character" = c("type")),
                       select = c("subj_id", "trial", "block", "stim_left", "stim_right", "type", "response", "response_s", "time_pressure",
                                  "i_comb", "pred_disc", "pred_mink", "pred_disc_unidim", "pred_mink_unidim", "pred_random")))

# 0.1. Generates combinations for fitting and hold-out sample (for each stimulus type A-D choose 2 out of 4 as fitting sample)
combs <- combn(1:4, 2) # which stimuli of a given type to pick for hold-out (reversed in the following line)
combs <- combs[, order(ncol(combs):1)] 
i_combs <- as.matrix(expand.grid(1:ncol(combs), 1:ncol(combs), 1:ncol(combs), 1:ncol(combs))) # indices for combs for each type A-D
i_combs_id <- apply(i_combs, 1, paste0, collapse = "")

# 0.2. Saves names of the different models
cols <- colnames(dt)[grepl(pattern = "^pred", x = colnames(dt))]
out_cols <- substring(cols, regexpr("_", cols) + 1)

# 0.3. Function for calculating the continuous log likelihood (the higher the better)
calculate_gof <- function(data, comb) {
  combs <- combs[, i_combs[which(comb == i_combs_id), ]]
  data = rbind(data[type == "A", .SD[combs[, 1]], by = trial],
               data[type == "B", .SD[combs[, 2]], by = trial],
               data[type == "C", .SD[combs[, 3]], by = trial],
               data[type == "D", .SD[combs[, 4]], by = trial],
               data[type %in% c("I", "V"), .SD])
  data[, lapply(.SD, function(x) {gof(obs = response_s, pred = x, type = "loglik", options = list(response = "continuous", pdf = "truncnormal", a = 0, b = 1), na.rm = TRUE)}), .SDcols = cols]
}

# ll_test <- dt[, calculate_gof(data = .SD, comb = unique(i_comb)), by = list(i_comb, time_pressure, subj_id)]
# fwrite(ll_test, "../../../data/results/similarity_log_likelihood.csv")
ll_test <- fread("../../../data/results/similarity_log_likelihood.csv")
ll_test <- ll_test[, lapply(.SD, median), .SDcols = cols, by = list(subj_id, time_pressure)] # averages lls across hold-out samples

# 1. Aggregate model comparison (H1d & H2b)
# 1.1. Calculates log likelihoods
ll_test_agg <- ll_test[, lapply(.SD, median), .SDcols = cols, by = time_pressure]
setnames(ll_test_agg, cols, out_cols)

# 1.2. Calculates best fitting model
ll_test_agg[, best_fitting_model := names(which.max(.SD)), by = time_pressure]

# 1.3. Calculate weights (equal to AIC weights, because no free parameters)
weights_agg <- ll_test_agg[, exp(.SD - max(.SD)), by = time_pressure, .SDcols = out_cols]
weights_agg[, (out_cols) := .SD/ rowSums(.SD), by = time_pressure]

# 1.4. Calculates pairwise comparisons between models (evidence ratio, see Wagenmakers & Farrell, 2004, p.194)
model_pairs <- permutations(n = 5, r = 2, v = 2:6)
model_comparisons <- rbindlist(apply(model_pairs, 1, function(x) {
  model_pair <- colnames(weights_agg[, ..x])
  x <- c(1, x)
  model_comparison <- weights_agg[, ..x][, .SD/rowSums(.SD), by = time_pressure]
  model_comparison[, .(m1_better_m2 = ifelse(get(model_pair[1]) >= .90,
                                             1,
                                             ifelse(get(model_pair[1]) <= .10, -1, 0)),
                       m1 = model_pair[1],
                       m2 = model_pair[2]), by = time_pressure]
}))

model_comparisons[is.na(model_comparisons$m1_better_m2), m1_better_m2 := 0]
model_comparisons <- model_comparisons[, .(sum = sum(m1_better_m2)), by = list(time_pressure, m1)]
setkey(model_comparisons, time_pressure, sum)
cat("\n Generated ll_test_agg and model_comparisons \n")

# 1.5. Additional fit indices
calculate_additional_indices <- function(data, comb) {
  combs <- combs[, i_combs[which(comb == i_combs_id), ]]
  data = rbind(data[type == "A", .SD[combs[, 1]], by = trial],
               data[type == "B", .SD[combs[, 2]], by = trial],
               data[type == "C", .SD[combs[, 3]], by = trial],
               data[type == "D", .SD[combs[, 4]], by = trial]) # excluded I and V trials to avoid observed values of 0
  # data[!is.na(response_s), lapply(.SD, function(x) {mape = MAPE(obs = response_s, pred = x, response = 'cont', discount = 0)}), .SDcols = cols]
  # data[!is.na(response_s), lapply(.SD, function(x) {mse = MSE(obs = response, pred = x, response = 'cont', discount = 0)}), .SDcols = cols]
  
  dt_long <- melt(data, measure.vars = list(grep("pred", colnames(data))), variable.name = "model", value.name = "pred")
  levels(dt_long$model) <- out_cols
  dt_long[!is.na(response_s), .(mape = MAPE(obs = response_s, pred = pred, response = 'cont', discount = 0),
                                mse = MSE(obs = response, pred = pred, response = 'cont', discount = 0)), by = list(model)]
}
# add_ind <- dt[, calculate_additional_indices(data = .SD, comb = unique(i_comb)), by = list(i_comb, time_pressure, subj_id)]
# add_ind <- add_ind[, .(M_MAPE = mean(mape), Md_MAPE = median(mape), SD_MAPE = sd(mape),
#                        M_MSE = mean(mse), Md_MSE = median(mse), SD_MSE = sd(mse)), by = list(time_pressure, model)]
# 
# ll_ind <- melt(ll_test, measure.vars = list(grep("pred", colnames(ll_test))), variable.name = "model", value.name = "ll")
# ll_ind <- ll_ind[, .(M_LL = mean(ll), Md_LL = median(ll), SD_LL = sd(ll)), by = list(time_pressure, model)]

# 2. Individual model comparison
# 2.1. Testset
# 2.1.1. Uses log likelihoods calculated before and averaged across hold-out samples
setnames(ll_test, cols, out_cols)

# 2.1.2. Calculates best fitting model
ll_test[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure)]

# 2.1.3. Calculates weights (equal to AIC weights, because no free parameters)
weights <- ll_test[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure), .SDcols = out_cols]
weights[, (out_cols) := round(.SD/ rowSums(.SD), 4), by = list(subj_id, time_pressure)]

# 2.1.4. Calculates distribution of best fitting models for each time pressure condition
weights_above_90 <- weights[, base::max(.SD) >= .90, by = list(time_pressure, subj_id)][, V1]
model_distr <- ll_test[weights_above_90, .N, by = list(time_pressure, best_fitting_model)]
# model_distr <- rbind(model_distr, ll_test[, .(best_fitting_model = "disc_unidim", # out_cols[out_cols %in% best_fitting_model == FALSE],
#                                               N = 0), by = time_pressure])
ll_test[weights_above_90, table(time_pressure, best_fitting_model)]

# 2.1.5. Chi-squared test (H1e & H2c)
model_distr <- rbind(model_distr, data.table(TRUE, "disc", 0), use.names = FALSE) # DELETE THIS
model_distr[, chisq.test(N), by = time_pressure]

# 2.1.6. Pairwise comparisons Chi-squared test (H1e & H2c)
model_distr_short <- rbind(model_distr[!grepl("disc", best_fitting_model)], model_distr[grepl("disc", best_fitting_model), .(best_fitting_model = "disc",
                                                                                                                             N = sum(N)), by = time_pressure])
model_distr_short[time_pressure == TRUE, multinomial.multcomp(N, p.method = "holm")]
model_distr_short[time_pressure == FALSE, multinomial.multcomp(N, p.method = "holm")]

# esc_bin_prop(prop1event = 17/30, grp1n = 30, prop2event = 3/30, grp2n = 30, es.type = "or")
# esc_bin_prop(prop1event = 15/31, grp1n = 31, prop2event = 9/31, grp2n = 31, es.type = "or")

# 2.1.7. Calculates pairwise comparisons between models (evidence ratio, see Wagenmakers & Farrell, 2004, p.194) (H1f & H2c)
model_pairs <- permutations(n = 5, r = 2, v = 3:7)
model_comparisons_individual <- rbindlist(apply(model_pairs, 1, function(x) {
  model_pair <- colnames(weights[, ..x])
  x <- c(2, x)
  model_comparison <- weights[, ..x][, .SD/rowSums(.SD), by = time_pressure]
  model_comparison[, .(m1_better_m2 = ifelse(get(model_pair[1]) >= .90,
                                             1,
                                             ifelse(get(model_pair[1]) <= .10, -1, 0)),
                       m1 = model_pair[1],
                       m2 = model_pair[2]), by = time_pressure]
}))
model_comparisons_individual[is.na(m1_better_m2), m1_better_m2 := 0]
model_comparisons_individual <- model_comparisons_individual[, .(sum = sum(m1_better_m2)), by = list(time_pressure, m1)]
setkey(model_comparisons_individual, time_pressure, sum)

# 2.1.8. Fisher's exact test (H3a, H3b)
model_distr_wide <- dcast(model_distr, time_pressure ~ best_fitting_model)
fisher.test(model_distr_wide[, c(2, 3)])

# 2.1.9. Pairwise comparisons Fisher's exact test (H3a, H3b)
disc_distr <- model_distr[grep("disc", best_fitting_model), .(disc = sum(N)), by = time_pressure]
disc_distr[, non_disc := c(64, 64) - disc]
disc_distr <- rbind(setDT(expandRows(disc_distr, "disc"))[, c("model", "model_used") := list("disc", 1), ][, c(1, 3, 4)],
                    setDT(expandRows(disc_distr, "non_disc"))[, c("model", "model_used") := list("disc", 0), ][, c(1, 3, 4)])

mink_distr <- model_distr[best_fitting_model == "mink", .(mink = N), by = time_pressure]
mink_distr[, non_mink := c(64, 64) - mink]
mink_distr <- rbind(setDT(expandRows(mink_distr, "mink"))[, c("model", "model_used") := list("mink", 1), ][, c(1, 3, 4)],
                    setDT(expandRows(mink_distr, "non_mink"))[, c("model", "model_used") := list("mink", 0), ][, c(1, 3, 4)])

# mink_uni_distr <- model_distr[best_fitting_model == "mink_unidim", .(mink_unidim = N), by = time_pressure]
# mink_uni_distr[, non_mink_unidim := c(64, 64) - mink_unidim]
# mink_uni_distr <- rbind(setDT(expandRows(mink_uni_distr, "mink_unidim"))[, c("model", "model_used") := list("mink_unidim", 1), ][, c(1, 3, 4)],
#                         setDT(expandRows(mink_uni_distr, "non_mink_unidim"))[, c("model", "model_used") := list("mink_unidim", 0), ][, c(1, 3, 4)])

# random_distr <- model_distr[best_fitting_model == "random", .(random = N), by = time_pressure]
# random_distr[, non_random := c(64, 64) - random]
# random_distr <- rbind(setDT(expandRows(random_distr, "random"))[, c("model", "model_used") := list("random", 1), ][, c(1, 3, 4)],
#                       setDT(expandRows(random_distr, "non_random"))[, c("model", "model_used") := list("random", 0), ][, c(1, 3, 4)])

fisher.bintest(model_used ~ time_pressure + model, data = rbind(disc_distr, mink_distr), p.method = "holm")

# esc_bin_prop(prop1event = 3/30, grp1n = 30, prop2event = 15/31, grp2n = 31, es.type = "or")
# esc_bin_prop(prop1event = 9/31, grp1n = 31, prop2event = 2/30, grp2n = 30, es.type = "or")
# 
# # 2.12. Log likelihoods per participant and stimulus
# ll_test_stim <- dt[block == "test" & too_slow == FALSE & stim_type == "new", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", options = list(response = "discrete"), na.rm = TRUE)}), .SDcols = cols, by = list(time_pressure_cond, subj_id, stim)]
# setnames(ll_test_stim, cols, out_cols)
# ll_test_stim[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond, stim)]
# ll_test_stim[, table(time_pressure_cond, best_fitting_model, stim)]
# 
# weights_stim <- ll_test_stim[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond, stim), .SDcols = out_cols]
# weights_stim[, (out_cols) := round(.SD/ rowSums(.SD), 2), by = list(subj_id, time_pressure_cond, stim)]
# 
# weights_above_90_stim <- weights_stim[, base::max(.SD) >= .90, by = list(time_pressure_cond, subj_id, stim)][, V1]
# ll_test_stim[weights_above_90_stim, table(time_pressure_cond, best_fitting_model, stim)]

# # 2.2. Learningset (only for sanity check)
# # 2.2.1. Calculates log likelihoods
# ll_learn <- dt[block == "training", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", options = list(response = "discrete"), discount = 8)}), .SDcols = cols, by = subj_id]
# setnames(ll_learn, cols, out_cols)
# 
# # 2.2.2. Calculate AICs for learning set
# AIC <- ll_learn[, -2 * .SD + 2 * c(5, 5, 2, 2), by = subj_id]
# AIC[, random := dt[block == "training", .(V1 = -2 * gof(obs = response, pred = rep(0.5, length(response)), type = "loglik", options = list(response = 'disc'))), by = subj_id][, V1]]
# AIC[, round(disc - mink, 1) == 0]
# 
cat("\n Generated ll_test, ll_test_stim, ll_learn, and AIC \n")