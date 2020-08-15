# Plots distribution of estimated parameters
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
library(patchwork)

# 0. Reads in data with parameter estimates
dt <- fread("../../../data/processed/similarity_main_fitted_parm_gcm.csv", key = "subj_id")
dt[, metric := toupper(metric)]
dt <- dt[, !c("p", "r")] # fixed to 1

# 1. Aggregated version (median across i_comb)
agg_dt <- dt[, lapply(.SD, median), .SDcols = c("w1", "w2", "w3", "c", "sigma"), by = list(subj_id, metric, tp, unidim)]
agg_dt <- melt(agg_dt, variable.name = "parameter", id.vars = c("subj_id", "metric", "tp", "unidim"))

# 2. Plots for the multidimensional GCM
agg_dt_multi <- agg_dt[unidim == FALSE, ]
agg_dt_multi_tp <- agg_dt_multi[tp == TRUE, ]

w <- ggplot(agg_dt_multi_tp[grep("w", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

c <- ggplot(agg_dt_multi_tp[grep("c", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  ylim(0, 5) +
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

sigma <- ggplot(agg_dt_multi_tp[grep("sigma", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  ylim(0, 1) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

plot <- w + {c + sigma + plot_layout(nrow = 1)} + plot_layout(ncol = 1)
plot <- wrap_elements(plot)
ggsave("../../../output/images/similarity/parameter_estimates_multidim_tp.png", plot)


agg_dt_multi_ntp <- agg_dt_multi[tp == FALSE, ]

w <- ggplot(agg_dt_multi_ntp[grep("w", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

c <- ggplot(agg_dt_multi_ntp[grep("c", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  ylim(0, 5) +
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

sigma <- ggplot(agg_dt_multi_ntp[grep("sigma", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  ylim(0, 1) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

plot <- w + {c + sigma + plot_layout(nrow = 1)} + plot_layout(ncol = 1)
plot <- wrap_elements(plot)
ggsave("../../../output/images/similarity/parameter_estimates_multidim_ntp.png", plot)

ggplot(agg_dt_multi[grep("sigma", parameter), ], aes(x = tp, y = value)) +
  geom_line(aes(group = subj_id), color = grey(level = 0.6, alpha = 0.5)) +
  geom_point(color = grey(level = 0.6, alpha = 0.5)) +
  # geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  geom_line(data = agg_dt_multi[grep("sigma", parameter), .(value = median(value)), by = tp], aes(group = 1), color = "black", size = 1.4) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  ylim(0.1, 0.6) + 
  scale_x_discrete(name = element_blank(), labels = c("FALSE" = "No time pressure", "TRUE" = "Time pressure"))

ggplot(agg_dt_multi[grep("sigma", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~tp, scales = "free_x", labeller = labeller(tp = tp_labs)) + # add tp and labeller = labeller(tp = tp_labs)
  ylim(0.1, 0.6) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())
ggsave("../../../output/images/similarity/parameter_estimates_multidim_sigma.png")

# 2. Plots for the unidimensional GCM
dt_uni <- dt[unidim == TRUE, ]
dt_uni <- melt(dt_uni, variable.name = "parameter", id.vars = c("subj_id", "metric", "tp", "unidim", "i_comb"))
dt_uni_tp <- dt_uni[tp == TRUE, ]
dt_uni_tp_w <- dt_uni_tp[grep("w", parameter), .(N = sum(value)), by = list(parameter, metric, tp, i_comb)]
agg_dt_uni_tp_w <- dt_uni_tp_w[, .(N = median(N), sd = sd(N)), by = list(parameter)]

w <- ggplot(agg_dt_uni_tp_w, aes(x = parameter, y = N)) +
  geom_bar(stat = "identity", fill = "lightgrey", color = "black") +
  geom_errorbar(stat = "identity", aes(ymin = N-sd, ymax = N+sd), width = .2) +
  facet_grid(~parameter, scales = "free_x") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

agg_dt_uni <- agg_dt[unidim == TRUE, ]
agg_dt_uni_tp <- agg_dt_uni[tp == TRUE, ]
c <- ggplot(agg_dt_uni_tp[grep("c", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  ylim(0, 5) +
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

sigma <- ggplot(agg_dt_uni_tp[grep("sigma", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  ylim(0, 1) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

plot <- w + {c + sigma + plot_layout(nrow = 1)} + plot_layout(ncol = 1)
plot <- wrap_elements(plot)
ggsave("../../../output/images/similarity/parameter_estimates_unidim_tp.png", plot)

dt_uni_ntp <- dt_uni[tp == FALSE, ]
dt_uni_ntp_w <- dt_uni_ntp[grep("w", parameter), .(N = sum(value)), by = list(parameter, metric, tp, i_comb)]
agg_dt_uni_ntp_w <- dt_uni_ntp_w[, .(N = median(N), sd = sd(N)), by = list(parameter)]
w <- ggplot(agg_dt_uni_ntp_w, aes(x = parameter, y = N)) +
  geom_bar(stat = "identity", fill = "lightgrey", color = "black") +
  geom_errorbar(stat = "identity", aes(ymin = N-sd, ymax = N+sd), width = .2) +
  facet_grid(~parameter, scales = "free_x") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

agg_dt_uni_ntp <- agg_dt_uni[tp == FALSE, ]
c <- ggplot(agg_dt_uni_ntp[grep("c", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  ylim(0, 5) +
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

sigma <- ggplot(agg_dt_uni_ntp[grep("sigma", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~parameter, scales = "free_x") + # add tp and labeller = labeller(tp = tp_labs)
  ylim(0, 1) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

plot <- w + {c + sigma + plot_layout(nrow = 1)} + plot_layout(ncol = 1)
plot <- wrap_elements(plot)
ggsave("../../../output/images/similarity/parameter_estimates_unidim_ntp.png", plot)

ggplot(agg_dt_uni[grep("sigma", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~tp, scales = "free_x", labeller = labeller(tp = tp_labs)) + # add tp and labeller = labeller(tp = tp_labs)
  ylim(0, 1) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())
ggsave("../../../output/images/similarity/parameter_estimates_unidim_sigma.png")
