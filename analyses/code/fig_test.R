library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
source("3_predicts_models.R")

dt <- dt[block == "test"]

critical_stimuli <- c("114", "211", "332", "342", "432", "442")
dt[block == "test", stimulus_type := ifelse(stim %in% critical_stimuli, "critical", "old")]

dt.long <- melt(dt, id.vars = c("subj_id", "block", "stim", "response", "time_pressure_cond", "stimulus_type"), measure.vars = patterns("^pred"),
     value.name = "pred", variable.name = "model")
dt.long[, model := toupper(gsub("pred_", "", model))]

dt.pred <- dt.long[, .(pred = mean(pred)), by = list(subj_id, stim, block, model, stimulus_type)]
dt.obs <- dt.long[, .(response = mean(response, na.rm = T), response.sd = sd(response, na.rm = T)), by = list(subj_id, stim, block, stimulus_type)]

# Plot for each individual
ggplot(dt.obs, aes(x = stim)) +
  geom_bar(aes(y = response, fill = "obs", color = "obs"), stat = "identity", size = 1) +
  geom_errorbar(aes(ymin = response - response.sd, ymax = response + response.sd, color = "obs")) +
  geom_point(data = dt.pred, aes(y = pred, color = model)) +
  geom_line(data = dt.pred, aes(y = pred, color = model, group = model)) +
  facet_grid(stimulus_type~subj_id) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = col) +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plot across individuals
ggplot(dt[!is.na(time)], aes(x = as.factor(stim), y = response)) +
  geom_bar(stat = "summary", fun.y = mean) +
  geom_errorbar(stat = "summary", fun.data = mean_sdl, fun.args = list(mult = 1), width = .2) +
  facet_wrap(~stimulus_type, scales = "free_x") +
  geom_point(data = dt.long, aes(y = pred, color = model), stat = "summary", fun.y = mean, size = 3) +
  geom_line(data = dt.long, aes(y = pred, color = model, group = model), stat = "summary", fun.y = mean) +
  xlab("Stimulus") +
  ylab("Percentage Cat 1 responses")
