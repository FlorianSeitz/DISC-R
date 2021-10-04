# Model comparison with log likelihoods and AIC weights
rm(list = ls(all = TRUE))
library(gtools)
library(RVAideMemoire)
library(data.table)
# library(cogsciutils)
library(cognitiveutils)
library(splitstackshape)
library(esc)
set.seed(932)

# 0. Prepares data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
dt <- fread("../../data/results/categorization_data_with_predictions.csv", 
            key = c("subj_id", "trial"))[fread("../../data/processed/categorization_exp_main.csv", 
                                               select = c("subj_id", "trial", "block", "too_slow", "time_pressure_cond", 
                                                          "response", "feature1", "feature2", "feature3", "stim", "stim_type"), 
                                               key = c("subj_id", "trial"),
                                               colClasses = list("character" = "stim"))]

# 0.1. Saves names of the different models
cols <- colnames(dt)[grepl(pattern = "^pred", x = colnames(dt))]
out_cols <- substring(cols, regexpr("_", cols) + 1)
ll_test <- dt[block == "test" & too_slow == FALSE & stim_type == "new", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", response = 'disc', options = list(response = "discrete"), na.rm = TRUE)}), .SDcols = cols, by = list(time_pressure_cond, subj_id)]

# 1. Aggregate model comparison (H1d & H2b)
# 1.1. Calculates log likelihoods
ll_test_agg <- ll_test[, lapply(.SD, median), .SDcols = cols, by = time_pressure_cond]
setnames(ll_test_agg, cols, out_cols)

# 1.2. Calculates best fitting model
ll_test_agg[, best_fitting_model := names(which.max(.SD)), by = time_pressure_cond]

# 1.3. Calculate weights (equal to AIC weights, because no free parameters)
weights_agg <- ll_test_agg[, exp(.SD - max(.SD)), by = time_pressure_cond, .SDcols = out_cols]
weights_agg[, (out_cols) := .SD/ rowSums(.SD), by = time_pressure_cond]

# 1.4. Calculates pairwise comparisons between models (evidence ratio, see Wagenmakers & Farrell, 2004, p.194)
model_pairs <- permutations(n = 5, r = 2, v = 2:6)
model_comparisons <- rbindlist(apply(model_pairs, 1, function(x) {
  model_pair <- colnames(weights_agg[, ..x])
  x <- c(1, x)
  model_comparison <- weights_agg[, ..x][, .SD/rowSums(.SD), by = time_pressure_cond]
  model_comparison[, .(m1_better_m2 = ifelse(get(model_pair[1]) >= .70,
                                             1,
                                             ifelse(get(model_pair[1]) <= .30, -1, 0)),
                       m1 = model_pair[1],
                       m2 = model_pair[2]), by = time_pressure_cond]
}))

model_comparisons[is.na(model_comparisons$m1_better_m2), m1_better_m2 := 0]
model_comparisons <- model_comparisons[, .(sum = sum(m1_better_m2)), by = list(time_pressure_cond, m1)]
setkey(model_comparisons, time_pressure_cond, sum)
cat("\n Generated ll_test_agg and model_comparisons \n")

# 1.5. Additional fit indices
dt_long <- melt(dt, measure.vars = list(grep("pred", colnames(dt))), variable.name = "model", value.name = "pred")
levels(dt_long$model) <- out_cols
add_ind <- dt_long[block == "test" & too_slow == FALSE & stim_type == "new", .(mape = MAPE(obs = response, pred = pred, response = 'disc', discount = 0),
                                                                               argmax = mean(choicerule(x = pred, type = "argmax") == response),
                                                                               mse = MSE(obs = response, pred = pred, response = 'disc', discount = 0)), by = list(time_pressure_cond, model)]
ll_ind <- dt_long[block == "test" & too_slow == FALSE & stim_type == "new", .(ll = gof(obs = response, 
                                                                                       pred = pred, 
                                                                                       type = "loglik", 
                                                                                       response = 'disc', 
                                                                                       options = list(response = "discrete"), 
                                                                                       na.rm = TRUE)), by = list(time_pressure_cond, model, subj_id)][, .(M_LL = mean(ll),
                                                                                                                                                          Md_LL = median(ll),
                                                                                                                                                          SD_LL = sd(ll)), by = list(time_pressure_cond, model)]

# 2. Individual model comparison
# 2.1. Testset
# 2.1.1. Calculates log likelihoods
setnames(ll_test, cols, out_cols)

# 2.1.2. Calculates best fitting model
ll_test[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond)]

# 2.1.3. Calculates weights (equal to AIC weights, because no free parameters)
weights <- ll_test[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond), .SDcols = out_cols]
weights[, (out_cols) := round(.SD/ rowSums(.SD), 4), by = list(subj_id, time_pressure_cond)]

# 2.1.4. Calculates distribution of best fitting models for each time pressure condition
weights_above_90 <- weights[, base::max(.SD) >= .70, by = list(time_pressure_cond, subj_id)][, V1]
model_distr <- ll_test[weights_above_90, .N, by = list(time_pressure_cond, best_fitting_model)]
model_distr <- rbind(model_distr, ll_test[, .(best_fitting_model = "disc_unidim", # out_cols[out_cols %in% best_fitting_model == FALSE],
                                              N = 0), by = time_pressure_cond])
ll_test[weights_above_90, table(time_pressure_cond, best_fitting_model)]

# 2.1.5. Chi-squared test (H1e & H2c)
model_distr[, chisq.test(N), by = time_pressure_cond]

# 2.1.6. Pairwise comparisons Chi-squared test (H1e & H2c)
model_distr_short <- rbind(model_distr[!grepl("disc", best_fitting_model)], model_distr[grepl("disc", best_fitting_model), .(best_fitting_model = "disc",
                                                                                                                             N = sum(N)), by = time_pressure_cond])
model_distr_short[time_pressure_cond == TRUE, multinomial.multcomp(N, p.method = "holm")]
model_distr_short[time_pressure_cond == FALSE, multinomial.multcomp(N, p.method = "holm")]

esc_bin_prop(prop1event = 17/30, grp1n = 30, prop2event = 3/30, grp2n = 30, es.type = "or")
esc_bin_prop(prop1event = 13/31, grp1n = 31, prop2event = 8/31, grp2n = 31, es.type = "or")

# 2.1.7. Calculates pairwise comparisons between models (evidence ratio, see Wagenmakers & Farrell, 2004, p.194) (H1f & H2c)
model_pairs <- permutations(n = 5, r = 2, v = 3:7)
model_comparisons_individual <- rbindlist(apply(model_pairs, 1, function(x) {
  model_pair <- colnames(weights[, ..x])
  x <- c(2, x)
  model_comparison <- weights[, ..x][, .SD/rowSums(.SD), by = time_pressure_cond]
  model_comparison[, .(m1_better_m2 = ifelse(get(model_pair[1]) >= .70,
                                             1,
                                             ifelse(get(model_pair[1]) <= .30, -1, 0)),
                       m1 = model_pair[1],
                       m2 = model_pair[2]), by = time_pressure_cond]
}))
model_comparisons_individual[is.na(m1_better_m2), m1_better_m2 := 0]
model_comparisons_individual <- model_comparisons_individual[, .(sum = sum(m1_better_m2)), by = list(time_pressure_cond, m1)]
setkey(model_comparisons_individual, time_pressure_cond, sum)

# 2.1.8. Fisher's exact test (H3a, H3b)
model_distr_wide <- dcast(model_distr, time_pressure_cond ~ best_fitting_model)
fisher.test(model_distr_wide[, c(2, 4:6)])

fisher.test(model_distr_wide[, c(2, 4)])

# 2.1.9. Pairwise comparisons Fisher's exact test (H3a, H3b)
disc_distr <- model_distr[grep("disc", best_fitting_model), .(disc = sum(N)), by = time_pressure_cond]
disc_distr[, non_disc := c(30, 31) - disc]
disc_distr <- rbind(setDT(expandRows(disc_distr, "disc"))[, c("model", "model_used") := list("disc", 1), ][, c(1, 3, 4)],
                    setDT(expandRows(disc_distr, "non_disc"))[, c("model", "model_used") := list("disc", 0), ][, c(1, 3, 4)])

mink_distr <- model_distr[best_fitting_model == "mink", .(mink = N), by = time_pressure_cond]
mink_distr[, non_mink := c(31, 30) - mink]
mink_distr <- rbind(setDT(expandRows(mink_distr, "mink"))[, c("model", "model_used") := list("mink", 1), ][, c(1, 3, 4)],
                    setDT(expandRows(mink_distr, "non_mink"))[, c("model", "model_used") := list("mink", 0), ][, c(1, 3, 4)])

mink_uni_distr <- model_distr[best_fitting_model == "mink_unidim", .(mink_unidim = N), by = time_pressure_cond]
mink_uni_distr[, non_mink_unidim := c(31, 30) - mink_unidim]
mink_uni_distr <- rbind(setDT(expandRows(mink_uni_distr, "mink_unidim"))[, c("model", "model_used") := list("mink_unidim", 1), ][, c(1, 3, 4)],
                        setDT(expandRows(mink_uni_distr, "non_mink_unidim"))[, c("model", "model_used") := list("mink_unidim", 0), ][, c(1, 3, 4)])

random_distr <- model_distr[best_fitting_model == "random", .(random = N), by = time_pressure_cond]
random_distr[, non_random := c(31, 30) - random]
random_distr <- rbind(setDT(expandRows(random_distr, "random"))[, c("model", "model_used") := list("random", 1), ][, c(1, 3, 4)],
                      setDT(expandRows(random_distr, "non_random"))[, c("model", "model_used") := list("random", 0), ][, c(1, 3, 4)])

fisher.bintest(model_used ~ time_pressure_cond + model, data = rbind(disc_distr), p.method = "holm")

esc_bin_prop(prop1event = 6/30, grp1n = 30, prop2event = 1/31, grp2n = 31, es.type = "or")
esc_bin_prop(prop1event = 8/31, grp1n = 31, prop2event = 5/30, grp2n = 30, es.type = "or")

# 2.12. Log likelihoods per participant and stimulus
ll_test_stim <- dt[block == "test" & too_slow == FALSE & stim_type == "new", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", options = list(response = "discrete"), na.rm = TRUE)}), .SDcols = cols, by = list(time_pressure_cond, subj_id, stim)]
setnames(ll_test_stim, cols, out_cols)
ll_test_stim[, best_fitting_model := names(which.max(.SD)), by = list(subj_id, time_pressure_cond, stim)]
ll_test_stim[, table(time_pressure_cond, best_fitting_model, stim)]

weights_stim <- ll_test_stim[, exp(.SD - max(.SD)), by = list(subj_id, time_pressure_cond, stim), .SDcols = out_cols]
weights_stim[, (out_cols) := round(.SD/ rowSums(.SD), 2), by = list(subj_id, time_pressure_cond, stim)]

weights_above_90_stim <- weights_stim[, base::max(.SD) >= .90, by = list(time_pressure_cond, subj_id, stim)][, V1]
ll_test_stim[weights_above_90_stim, table(time_pressure_cond, best_fitting_model, stim)]

# 2.2. Learningset (only for sanity check)
# 2.2.1. Calculates log likelihoods
ll_learn <- dt[block == "training", lapply(.SD, function(x) {gof(obs = response, pred = x, type = "loglik", options = list(response = "discrete"), discount = 8)}), .SDcols = cols, by = subj_id]
setnames(ll_learn, cols, out_cols)

# 2.2.2. Calculate AICs for learning set
AIC <- ll_learn[, -2 * .SD + 2 * c(5, 5, 2, 2), by = subj_id]
AIC[, random := dt[block == "training", .(V1 = -2 * gof(obs = response, pred = rep(0.5, length(response)), type = "loglik", options = list(response = 'disc'))), by = subj_id][, V1]]
AIC[, round(disc - mink, 1) == 0]

cat("\n Generated ll_test, ll_test_stim, ll_learn, and AIC \n")