# ==========================================================================
# Analyses Experiment: 3. Analyses the results of the cognitive model fiting
# ==========================================================================

# ==========================================================================
# Parameters that need to be specified
study <- 1 # specify the study
refit_models <- FALSE # specify if you want to refit the models
models <- c("gcm", "gcm_disc", "gcm_unidim", "gcm_disc_unidim", "rule_seitz2021") # specify the models to analyze
crit <- .90 # pairwise model comparison criterium
assign <- .70 # model assignment threshold
exploratory <- FALSE # if false makes main analyses, if true makes exploratory analyses (with rule_seitz2021)
# ==========================================================================

# ==========================================================================
# Prepares packages and data
# ==========================================================================
pacman::p_load(data.table, cognitiveutils, RVAideMemoire, pwr, esc, splitstackshape, gtools, lme4, ez)
devtools::load_all("~/R/cognitivemodels/")
# pacman::p_load_gh("FlorianSeitz/cognitivemodels")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change wd to where this script lies (RStudio needed!)
source("setup_models.R")
source("utils_ranking.R")
d <- fread("../../../data/processed/categorization_exp_main.csv", 
           key = "subj_id", colClasses = list("character" = c("stim", "feature_presentation_order", "color_presentation_order")))[stim_type == "new"]
ifelse(refit_models, 
       source("2-cognitive-modeling.R"), 
       fits <- readRDS(sub("X", study,"../../fittedmodels/studyX_cognitive_models.rds")))
pars <- fits[model %in% models, V2[[1]]$parm, by = list(subj_id, model)]
pars_long <- melt(pars, id.vars = c("subj_id", "model"), variable.name = "par")
pars_long[!grepl("unidim", model), .(m = mean(value), md = median(value), sd = sd(value)), by = par][, round(.SD, 2), by = par]
pars_long[grepl("unidim", model) & grepl("w", par), .(sum = sum(value)), by = .(model, par)]

# ==========================================================================
# Makes model predictions
# ==========================================================================
preds <- fits[model %in% models, V2[[1]]$predict(newdata = d[subj_id]), by = .(subj_id, model)]
preds[, trial := d[subj_id, "trial"], by = .(subj_id, model)]
preds <- as.data.table(tidyr::pivot_wider(preds, names_from = "model", values_from = "V1"))
preds[, random := 0.5] # prediction of random model
d <- merge(d, preds, by = c("subj_id", "trial"))

# ==========================================================================
# Calculates the models' goodness-of-fit (gof, i.e., log likelihood)
# ==========================================================================
if(exploratory) { # includes random model in analyses
  models <- c(models[models != "random"], "random") 
} else { # exludes rule_seitz2021 model, but includes random model in analyses
  models <- c(models[!models %in% c("rule_seitz2021", "random")], "random") 
}

gofs <- d[, lapply(.SD, function(x) {
  gof(obs = response, pred = x, type = "loglik", response = "disc", na.rm = TRUE)
}), .SDcols = models, by = .(subj_id, time_pressure_cond)]

# gofs <- d[, lapply(.SD, function(x) {
#   sum(log(abs(x - (1 - response))) * ifelse(stim %in% c("100", "003"), 1, .25), na.rm = TRUE)
# }), .SDcols = models, by = .(subj_id, time_pressure_cond)]

# d[stim %in% c("100", "003", "221"), lapply(.SD, function(x) {
#   gof(obs = response, pred = x, type = "loglik", response = "disc", na.rm = TRUE)
# }), .SDcols = models, by = .(subj_id, time_pressure_cond)],

# ==========================================================================
# Aggregate analyses: calculates weights, makes pairwise model comparisons
# ==========================================================================
# gofs_agg <- gofs[, lapply(.SD, median), by = time_pressure_cond, .SDcols = models] # aggregated across participants
# gofs_agg[, best_model := names(which.max(.SD)), by = time_pressure_cond]
# weights_agg <- gofs_agg[, exp(.SD - max(.SD)), by = time_pressure_cond, .SDcols = models]
# weights_agg[, (models) := .SD/ rowSums(.SD), by = time_pressure_cond] # equal to AIC weights, because no free parameters
# 
# model_pairs <- gtools::permutations(n = 5, r = 2) # matrix of model pairs
# model_ranks_agg <- rbindlist(apply(model_pairs, 1, function(x) { # compares models pairwise
#   model_pair <- models[x] # gets the names of the two models
#   weights_comparison <- weights_agg[, ..model_pair] # extracts the weights of the model pair
#   model_comparison <- weights_comparison[, .SD/(rowSums(.SD) + 1e-07)] # normalizes the weights
#   # Makes comparison and weights it with actual weight
#   model_comparison <- model_comparison[, .(m1_better_m2 = ifelse(get(model_pair[1]) >= crit, 1,
#                                                                  ifelse(get(model_pair[2]) >= crit, -1, 0)),
#                                            m1 = model_pair[1],
#                                            m2 = model_pair[2])]
#   model_comparison[, m1_better_m2 := m1_better_m2 * weights_comparison[, abs(get(model_pair[1]) - get(model_pair[2]))]]
#   model_comparison[, time_pressure_cond := weights_agg$time_pressure_cond]
# }))
# model_ranks_agg <- model_ranks_agg[, .(sum = round(sum(m1_better_m2), 2)), by = list(time_pressure_cond, m1)]
# (setkey(model_ranks_agg, time_pressure_cond, sum)) # rank order (higher number = better)

# ==========================================================================
# Individual analyses: calculates weights, makes pairwise model comparisons
# ==========================================================================
gofs[, best_model := names(which.max(.SD)), by = .(subj_id, time_pressure_cond)]
weights <- gofs[, exp(.SD - max(.SD)), .SDcols = models, by = .(subj_id, time_pressure_cond, best_model)]
weights[, (models) := .SD/ rowSums(.SD), .SDcols = models] # equal to AIC weights
ranking(weights, models, crit, exclude = TRUE)
ranking(weights[, lapply(.SD, function(x) round(mean(x), 2)), .SDcols = models, by = .(time_pressure_cond)], models, crit)

# model_ranks <- rbindlist(apply(model_pairs, 1, function(x) {
#   model_pair <- models[x] # gets the names of the two models
#   weights_comparison <- weights[, ..model_pair] # extracts the weights of the model pair
#   model_comparison <- weights_comparison[, .SD/(rowSums(.SD) + 1e-07)] # normalizes the weights
#   # Makes comparison and weights it with actual weight
#   model_comparison <- model_comparison[, .(m1_better_m2 = ifelse(get(model_pair[1]) >= crit, 1,
#                                                                  ifelse(get(model_pair[2]) >= crit, -1, 0)),
#                                            m1 = model_pair[1],
#                                            m2 = model_pair[2])]
#   # model_comparison[, m1_better_m2 := m1_better_m2 * weights_comparison[, abs(get(model_pair[1]) - get(model_pair[2]))]]
#   model_comparison[, time_pressure_cond := weights$time_pressure_cond]
# }))
# model_ranks <- model_ranks[, .(sum = sum(m1_better_m2)), by = list(time_pressure_cond, m1)]
# (setkey(model_ranks, time_pressure_cond, sum)) # rank order (higher number = better)

model_distr <- weights[rowSums((weights > assign)[, models]) > 0, .N, by = .(best_model, time_pressure_cond)]
model_distr[, chisq.test(N), by = time_pressure_cond] # chi-squared test
model_distr[time_pressure_cond == TRUE, multinomial.multcomp(N, p.method = "holm")]
model_distr[time_pressure_cond == FALSE, multinomial.multcomp(N, p.method = "holm")]

model_distr[best_model != "random", ES.h(N[1] / nrow(weights), N[2] / nrow(weights))] # effect size
esc_bin_prop(prop1event = 7/30, grp1n = 30, prop2event = 13/30, grp2n = 30, es.type = "or")
esc_bin_prop(prop1event = 13/30, grp1n = 30, prop2event = 19/31, grp2n = 31, es.type = "or")

model_distr_wide <- dcast(model_distr, time_pressure_cond ~ best_model)
fisher.test(model_distr_wide[, -1])
disc_mink <- model_distr[best_model != "random", .(N = sum(N)), by = .(time_pressure_cond, disc = grepl("disc", best_model))]
fisher.test(dcast(disc_mink, time_pressure_cond ~ disc)[, -1])

# ==========================================================================
# Additional fit indices
# ==========================================================================
d_long <- melt(d, measure.vars = list(models), variable.name = "model", value.name = "pred")
add_gofs <- d_long[!is.na(response), .(mape = MAPE(obs = response, pred = pred, response = 'disc'),
                                       arg = mean(choicerule(x = pred, type = "argmax")[, 1] == response),
                                       mse = mean((response - pred)^2, na.rm = T),
                                       ll = gof(obs = response, pred = pred, type = "loglik", response = 'disc', na.rm = TRUE)), by = .(model, subj_id, time_pressure_cond)]
add_gofs <- melt(add_gofs, measure.vars = c("mape", "arg", "mse", "ll"), variable.name = "gof")
add_gofs[, .(m = mean(value), 
             md = median(value), 
             sd = sd(value)), by = .(model, gof, time_pressure_cond)][, round(.SD, 2), by = .(model, gof, time_pressure_cond)]

# ==========================================================================
# Learningset analyses (only for sanity check)
# ==========================================================================
# gofs_learn <- fits[model %in% models, .(ll = logLik(V2[[1]]), aic = AIC(V2[[1]])), by = .(subj_id, model)]
# gofs_learn[, .(ll = mean(ll), aic = mean(aic)), by = model]
# d_learn <- fread(sub("X", study, "../../data/processed/studyX.csv"), key = "subj_id")[block == "train"]
# d_learn[, .(ll = gof(obs = resp, pred = rep(0.5, .N), type = "loglik", response = 'disc')), by = subj_id][, .(ll = mean(ll), aic = 2 * mean(ll))]

# ==========================================================================
# Choice inconsistency analyses (exploratory)
# ==========================================================================
d2 <- fread("../../../data/processed/categorization_exp_main.csv",
            key = "subj_id", colClasses = c("stim" = "character"))[block != "training"]
models <- models[grepl("gcm", models)]
fits[model %in% models, V2[[1]]$choicerule <- NULL, by = .(subj_id, model)] # removes choice rule
preds_no_cr <- fits[model %in% models, V2[[1]]$predict(newdata = d2[subj_id]), by = .(subj_id, model)] # makes predictions (V1)
preds_no_cr[, c("block", "stim_type", "response") := d2[subj_id, .(block, stim_type, response)], by = .(subj_id, model)]

pacman::p_load(doFuture)
registerDoFuture()
plan(multisession, workers = 4L)  ## on MS Windows
setkey(preds_no_cr, "subj_id")  
tau <- foreach(x = unique(preds_no_cr$subj_id),
               .combine = "rbind",
               .inorder = FALSE, 
               .packages = c("data.table", "cognitivemodels", "devtools")) %dopar% {
                 devtools::load_all("~/R/cognitivemodels/")
                 preds_no_cr[.(x), .(
                   old = softmax(response ~ V1, data = .SD[stim_type == "old"], options = list(solver = "solnp", lb = c(tau = .1)))$par,
                   new = softmax(response ~ V1, data = .SD[stim_type == "new"], options = list(solver = "solnp", lb = c(tau = .1)))$par,
                   fam = softmax(response ~ V1, data = .SD[block == "familiarization"], options = list(solver = "solnp", lb = c(tau = .1)))$par,
                   test = softmax(response ~ V1, data = .SD, options = list(solver = "solnp", lb = c(tau = .1)))$par), by = .(subj_id, model)]
               }   
tau[, c("old", "new", "fam", "test") := list(as.double(old), as.double(new), as.double(fam), as.double(test))]
tau <- merge(weights[weights[, rowSums(.SD > crit) > 0, .SDcols = models], .(subj_id, time_pressure_cond, model = best_model)], tau)
tau <- merge(tau, pars[, .(subj_id, model, training = tau)])
tau[, t.test(log(test)[time_pressure_cond == TRUE], log(test)[time_pressure_cond == FALSE])]
# tau <- as.data.table(pivot_longer(tau, cols = c("training", "test"), names_to = "block"))
# tau[, .(mean(value), median(value), sd(value)), by = .(time_pressure_cond, block)]
# ezANOVA(data = tau, dv = value, between = time_pressure_cond, within = block, wid = subj_id)

tau <- merge(tau, unique(d2[, .(subj_id, time_pressure_cond)]), by = "subj_id")
fits[model %in% models, V2[[1]]$choicerule <- "softmax", by = .(subj_id, model)] # removes choice rule
fits[model %in% models, V2[[1]]$parm$tau <- tau[subj_id, fam], by = .(subj_id, model)] # removes choice rule
preds_tau <- fits[model %in% models, V2[[1]]$predict(newdata = d[subj_id]), by = .(subj_id, model)]
preds_tau[, trial := d[subj_id, "trial"], by = .(subj_id, model)]
preds_tau[, model := "gcm_tau"]
preds_tau <- as.data.table(tidyr::pivot_wider(preds_tau, names_from = "model", values_from = "V1"))
d <- merge(d, preds_tau)
