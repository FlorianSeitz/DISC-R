# Plots distribution of estimated parameters
library(data.table)
library(patchwork)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
theme_set(new = theme_bw())

# Multidimensional parameter estimates for w, c and tau
dt_multidim <- fread("../../data/processed/categorization_main_fitted_parm_gcm.csv", key = "subj_id")
dt_multidim <- dt_multidim[!duplicated(subj_id)]
dt_multidim[, metric := toupper(metric)]
dt_multidim <- dt_multidim[, !7:8]
dt_multidim <- melt(dt_multidim, variable.name = "parameter")

w <- ggplot(dt_multidim[grep("w", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~parameter, scales = "free_x") + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

c <- ggplot(dt_multidim[grep("c", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  ylim(0, 5) +
  facet_grid(~parameter, scales = "free_x") + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

tau <- ggplot(dt_multidim[grep("tau", parameter), ], aes(x = parameter, y = value)) +
  geom_violin() +
  geom_point(alpha = 0.15, position = position_jitter(width = 0.05)) +
  stat_summary(geom = "point", fun.y = "median", color = "black", size = 3.5) + 
  facet_grid(~parameter, scales = "free_x") + 
  ylim(0, 1) + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank())

plot <- w + {c + tau + plot_layout(nrow = 1)} + plot_layout(ncol = 1)
plot <- wrap_elements(plot)
ggsave("../../output/images/parameter_estimates_multidim.png", plot)

# Unidimensional parameter estimates (c and tau fixed to multidimensional parameter estimates)
dt_unidim <- fread("../../data/processed/categorization_main_fitted_parm_gcm_unidim.csv", key = "subj_id")
dt_unidim[, metric := toupper(metric)]
dt_unidim <- dt_unidim[, !6:9]
dt_unidim <- melt(dt_unidim, variable.name = "parameter")
dt_unidim <- dt_unidim[, .(N = sum(value)), by = list(parameter, metric)]

ggplot(dt_unidim[parameter != "w2"], aes(x = parameter, y = N)) +
  geom_bar(stat = "identity") +
  facet_grid(~metric)
  # ggtitle("Parameter estimates test phase: unidimensional GCM") + 
  # theme(plot.title = element_text(hjust = 0.5))
ggsave("../../output/images/parameter_estimates_unidim.png")
