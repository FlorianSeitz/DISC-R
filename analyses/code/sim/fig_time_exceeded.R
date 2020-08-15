# Figure for accuracy in familiarization phase
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # changes wd to the folder where this script is lying; works only in RStudio
source("fig_setup.R")

dt <- fread("../../../data/processed/similarity_exp_main.csv")

dt[, subj_id := as.factor(subj_id)]
dt[, type := as.factor(type)]

# Accuracy x too slow table
fam_too_slow <- dt[type == "fam", .(too_slow = mean(is.na(response))), by = list(subj_id, time_pressure)] 
fam_too_slow[, .(mean = mean(too_slow), median = median(too_slow), sd = sd(too_slow)), by = list(time_pressure)] 

# Accuracy in familiarization phase
test_too_slow <- dt[type != "fam", .(too_slow = mean(is.na(response))), by = list(subj_id, type, time_pressure)]
test_too_slow[, .(mean = mean(too_slow), median = median(too_slow), sd = sd(too_slow)), by = list(time_pressure, type)] 

ggplot(test_too_slow, aes(x = as.factor(type), y = too_slow)) +
  geom_point(stat = "summary", fun.y = mean) +
  geom_errorbar(stat = "summary", fun.data = mean_sdl, fun.args = list(mult = 1), width = .2) +
  xlab("Stimulus type") +
  ylab("Share of too slow responses") + 
  coord_cartesian(ylim=c(0,.2))
                  