# Plots mean and sd responses of each participants for each new test stimulus
library(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
source("5_compares_models.R")

dt[, time_pressure_cond := as.factor(time_pressure_cond)]
dt[, type := factor(type, levels = levels, labels = labels)]
best_model <- weights[, .(best_model = colnames(weights)[which(.SD > .90) + 2]), by = list(subj_id, time_pressure), .SDcols = 3:7]
no_best_model <- expand.grid(subj_id = unique(dt$subj_id), time_pressure = c(TRUE, FALSE))
no_best_model <- no_best_model[do.call(paste0, no_best_model) %in% do.call(paste0, best_model[, 1:2]) == FALSE, ]
no_best_model <- as.data.table(no_best_model)
no_best_model[, best_model := "none"]
best_model <- rbind(best_model, no_best_model)
dt <- merge(dt, best_model, by = c("subj_id", "time_pressure"))

dt[, .(mean = mean(response, na.rm = T), sd = sd(response, na.rm = T)), by = list(type, subj_id, time_pressure)]
dt[, best_model := factor(best_model,
                          levels = c("disc", "mink", "disc_unidim", "mink_unidim", "random", "none"),
                          labels = c("DISC-MULTI", "MINK-MULTI", "DISC-UNI", "MINK-UNI", "RANDOM", "NONE"))]

best_model$pred <- paste0("pred_", best_model$best_model)
dt <- merge(dt, best_model[, c(1, 2, 4)])
dt[, pred_value := ifelse(pred == "pred_disc", pred_disc, 
                          ifelse(pred == "pred_disc_unidim", pred_disc_unidim,
                                 ifelse(pred == "pred_mink", pred_mink,
                                        ifelse(pred == "pred_mink_unidim", pred_mink_unidim, 
                                               ifelse(pred == "pred_random", pred_random, NA)))))]
agg_dt <- dt[, .(response_m = mean(response, na.rm = T), response_sd = sd(response, na.rm = T), pred_value = unique(pred_value)), by = list(time_pressure_cond, subj_id, stim, best_model)]

ggplot(dt[time_pressure == FALSE, ], aes(x = type, y = response_s)) +
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
  labs(title = "No Time Pressure Condition: Responses and Predictions of Winning Model", color = "Winning Model")
ggsave("../../../output/images/similarity/fig_individual_responses_ntp.png", height = 20, width = 15)

# Plots for subjects who were DISC-MULTI without time pressure
dt_sub <- dt[subj_id %in% c("dezj", "tonk"), ]
dt_sub <- melt(dt_sub, id.vars = c("subj_id", "time_pressure", "type", "response_s", "best_model"), measure.vars = c("pred_disc", "pred_mink"), variable.name = "Model", value.name = "pred")

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

ggplot(dt_sub, aes(x = type, y = response_s)) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  # geom_point(aes(y = response_m)) +
  # geom_errorbar(aes(ymin = response_m - response_sd, ymax = response_m + response_sd)) +
  # geom_line(aes(y = pred_value, color = best_model, group = 1)) +
  # geom_point(aes(y = pred_value, color = best_model)) + 
  stat_summary(geom = "errorbar") +
  stat_summary(geom = "point") +
  stat_summary(geom = "line", aes(y = pred, color = Model, group = Model)) +
  stat_summary(geom = "point", aes(y = pred, color = Model)) +
  ylim(0, 1) +
  xl +  yl +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  facet_grid(subj_id~time_pressure, scales = "free_x", labeller = labeller(time_pressure = tp_labs)) +
  # labs(title = "Responses and Predictions for Two Subjects") + 
  scale_color_discrete(name = "Model", labels = c("DISC-MULTI", "MINK-MULTI")) +
  scale_x_discrete(breaks = levels(dt_sub$type), 
                   labels = addline_format(levels(dt_sub$type)))

ggsave("../../../output/images/similarity/fig_disc_mink.png", width = 12)
