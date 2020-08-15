# Plots mean and sd responses of each participants for each new test stimulus
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("5_compares_models.R")
source("fig_setup.R")

dt <- dt[block == "test" & stim_type == "new", ]
dt[, time_pressure_cond := as.factor(time_pressure_cond)]
dt[, stim := factor(stim, levels = c("100", "003", "221", "231", "321", "331"))]
best_model <- weights[, .(best_model = colnames(weights)[which(.SD > .90) + 2]), by = subj_id, .SDcols = 3:7]
no_best_model <- data.table(subj_id = unique(dt$subj_id)[unique(dt$subj_id) %in% best_model$subj_id == FALSE], 
                            best_model = "none")
best_model <- rbind(best_model, no_best_model)
dt <- merge(dt, best_model, by = "subj_id")

dt[, .(mean = mean(response, na.rm = T), sd = sd(response, na.rm = T)), by = list(stim, subj_id)]
dt[, best_model := factor(best_model,
                          levels = c("disc", "mink", "disc_unidim", "mink_unidim", "random", "none"),
                          labels = c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM", "NONE"))]

best_model$pred <- paste0("pred_", best_model$best_model)
dt <- merge(dt, best_model[, c(1, 3)])
dt[, pred_value := ifelse(pred == "pred_disc", pred_disc, 
            ifelse(pred == "pred_disc_unidim", pred_disc_unidim,
                   ifelse(pred == "pred_mink", pred_mink,
                          ifelse(pred == "pred_mink_unidim", pred_mink_unidim, 
                                 ifelse(pred == "pred_random", pred_random, NA)))))]
agg_dt <- dt[, .(response_m = mean(response, na.rm = T), response_sd = sd(response, na.rm = T), pred_value = unique(pred_value)), by = list(time_pressure_cond, subj_id, stim, best_model)]

ggplot(dt[time_pressure_cond == TRUE, ], aes(x = stim, y = response)) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  # geom_point(aes(y = response_m)) +
  # geom_errorbar(aes(ymin = response_m - response_sd, ymax = response_m + response_sd)) +
  # geom_line(aes(y = pred_value, color = best_model, group = 1)) +
  # geom_point(aes(y = pred_value, color = best_model)) + 
  stat_summary(geom = "errorbar") +
  stat_summary(geom = "point") +
  stat_summary(geom = "line", aes(y = pred_value, color = best_model, group = 1)) +
  stat_summary(geom = "point", aes(y = pred_value, color = best_model)) +
  ylim(0, 1) +
  xlab("Stimulus ID") +
  ylab("Proportion category A responses") +
  theme(
      panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  facet_wrap(~subj_id, scales = "free_x") +
  labs(title = "Time Pressure Condition: Responses and Predictions of Winning Model", color = "Winning Model")
ggsave("../../output/images/fig_individual_responses_tp.png", height = 10, width = 15)
