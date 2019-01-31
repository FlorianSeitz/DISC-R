# Figure for accuracy in familiarization phase
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")

dat <- fread("../../data/processed/categorization_data_with_predictions.csv")

dat <- dat[block == "familiarization"]

dat[, subj_id := as.factor(subj_id)]
dat[, stim := as.factor(stim)]

# Accuracy x too slow table
dat[, fam_block := rep(c(1, 2), each = 16), by = subj_id]
dat[, .(too_slow_1 = mean(is.na(time[fam_block == 1])), 
        too_slow_2 = mean(is.na(time[fam_block == 2])),
        accurate_1 = mean(true_cat[fam_block == 1] == response[fam_block == 1], na.rm = T),
        accurate_2 = mean(true_cat[fam_block == 2] == response[fam_block == 2], na.rm = T)), by = list(subj_id)] 


# Accuracy in familiarization phase
dat[block == "familiarization", mean(is.na(time)), by = list(subj_id, stim)]
ggplot(dat[block == "familiarization" & !is.na(time)], aes(x = as.factor(stim), y = as.numeric(response == true_cat))) +
  geom_bar(stat = "summary", fun.y = mean) +
  geom_errorbar(stat = "summary", fun.data = mean_sdl, fun.args = list(mult = 1), width = .2) +
  xlab("Stimulus") +
  ylab("Percentage Cat 1 responses")
