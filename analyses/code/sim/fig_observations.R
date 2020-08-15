# Plots similarity ratings
# MMM: beta_type_D > beta_type_B > beta_type_A > beta_type_C
# MDM: beta_type_A > beta_type_D = beta_type_C > beta_type_B
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
library(stringr)

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

# 1. Plot similarity ratings per stimulus type aggregated over subjects
dt <- dt[!is.na(response_s), ]
agg_dt <- dt[, .(mean = mean(response_s),
                 med = median(response_s), 
                 sd = sd(response_s)), by = list(type, time_pressure)]

ggplot(agg_dt, aes(x = type, y = med)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .2,
                position = position_dodge(0.05)) +
  facet_wrap(~ time_pressure, labeller = labeller(time_pressure = tp_labs)) +
  theme_bw() +
  ylim(0, 1.1) +
  xl + yl

# 2. Plot similarity ratings per stimulus type and subjects
agg_dt <- dt[, .(mean = mean(response_s),
                 med = median(response_s), 
                 sd = sd(response_s)), by = list(type, time_pressure, subj_id)]
agg_dt <- agg_dt[type %in% c("all-equal 223-223", "all-different 111-444") == FALSE, ]
agg_agg_dt <- agg_dt[, .(med = median(med)), by = list(type, time_pressure)]

files <- list.files(path = "../../../data/results/", pattern = c("similarity.+predictions"), full.names = TRUE)
dt_pred <- rbindlist(lapply(files, fread, fill = TRUE, colClasses = list("character" = c("type")),
                       select = c("subj_id", "trial", "block", "stim_left", "stim_right", "type", "response", "response_s", "time_pressure",
                                  "i_comb", "pred_disc", "pred_mink", "pred_disc_unidim", "pred_mink_unidim", "pred_random")))
dt_pred_agg <- dt_pred[, .(DISC = median(pred_disc),
                           MINK = median(pred_mink)), by = type]
dt_pred_agg[, type := factor(type, levels = levels, labels = labels)]
dt_pred_agg <- dt_pred_agg[type %in% c("all-equal 223-223", "all-different 111-444") == FALSE, ]
dt_pred_agg <- melt(dt_pred_agg, id.vars = "type", variable.name = "Model", value.name = "med")
ggplot(agg_dt, aes(x = type, y = med, group = subj_id)) +
  geom_line(color = grey(level = 0.6, alpha = 0.5)) +
  geom_point(color = grey(level = 0.6, alpha = 0.5)) +
  geom_line(data = agg_agg_dt, aes(group = 1), size = 1.4) +
  geom_point(data = agg_agg_dt, aes(group = 1), size = 3) +
  geom_line(data = dt_pred_agg, aes(group = Model, color = Model), size = 1.4) +
  geom_point(data = dt_pred_agg, aes(group = Model, color = Model), size = 3) +
  facet_wrap(~ time_pressure, labeller = labeller(time_pressure = tp_labs)) +
  ylim(0, 1) +
  xl + yl + 
  scale_x_discrete(breaks = levels(agg_dt$type), 
                   labels = addline_format(levels(agg_dt$type)))
ggsave("../../../output/images/similarity/observations2.png", width = 10)

# 2. Plot similarity ratings for I and V stimulus types and subjects
dt <- dt[type %in% c("all-equal 223-223", "all-different 111-444"), ]
dt[, abs_diff := as.character(str_pad(abs(as.numeric(stim_left) - as.numeric(stim_right)), 3, pad = "0"))]
agg_dt <- dt[, .(mean = mean(response_s),
                 med = median(response_s), 
                 sd = sd(response_s)), by = list(type, abs_diff, time_pressure, subj_id)]
agg_agg_dt <- agg_dt[, .(med = median(med)), by = list(type, abs_diff, time_pressure)]

dt_pred[, abs_diff := as.character(str_pad(abs(as.numeric(stim_left) - as.numeric(stim_right)), 3, pad = "0"))]
dt_pred_agg <- dt_pred[, .(DISC = median(pred_disc),
                           MINK = median(pred_mink)), by = list(type, abs_diff)]
dt_pred_agg[, type := factor(type, levels = levels, labels = labels)]
dt_pred_agg <- dt_pred_agg[type %in% c("all-equal 223-223", "all-different 111-444"), ]
dt_pred_agg <- melt(dt_pred_agg, id.vars = c("type", "abs_diff"), variable.name = "Model", value.name = "med")

ggplot(agg_dt, aes(x = abs_diff, y = med, group = subj_id)) +
  geom_line(color = grey(level = 0.6, alpha = 0.5)) +
  geom_point(color = grey(level = 0.6, alpha = 0.5)) +
  geom_line(data = agg_agg_dt, aes(group = 1), size = 1.4) +
  geom_point(data = agg_agg_dt, aes(group = 1), size = 3) +
  geom_line(data = dt_pred_agg, aes(group = Model, color = Model), size = 1.4) +
  geom_point(data = dt_pred_agg, aes(group = Model, color = Model), size = 3) +
  facet_wrap(~ time_pressure, labeller = labeller(time_pressure = tp_labs)) +
  ylim(0, 1) +
  xl + yl + 
  scale_x_discrete(breaks = unique(agg_dt$abs_diff), 
                   labels = addline_format(c("all-equal 223-223", "all-different 111-444", "all-different 111-222", "all-different 111-555")))
ggsave("../../../output/images/similarity/observationsIV.png", width = 10)
