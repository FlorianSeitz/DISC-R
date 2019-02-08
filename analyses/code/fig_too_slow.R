# Figure showing how often participants were too slow in familiarization phase
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")

dat <- fread("../../data/processed/categorization_exp_pretest.csv", colClasses = list("character" = "stim"))

dat <- dat[block != "training"]

dat[, subj_id := as.factor(subj_id)]
dat[, stim := as.factor(stim)]

# Accuracy x too slow table
dat[block == "familiarization", fam_block := rep(c(1, 2), each = 16), by = subj_id]
dat[block == "familiarization", .(too_slow_1 = mean(is.na(time[fam_block == 1])), 
                                  too_slow_2 = mean(is.na(time[fam_block == 2])),
                                  accurate_1 = mean(true_cat[fam_block == 1] == response[fam_block == 1], na.rm = T),
                                  accurate_2 = mean(true_cat[fam_block == 2] == response[fam_block == 2], na.rm = T)), by = list(subj_id)] 
dat[block == "test", .(too_slow = mean(is.na(time))), by = list(subj_id)] 

sum_dat <- dat[, .(too_slow = mean(is.na(time))), by = list(subj_id, block, stim)] 
ggplot(sum_dat, aes(x = stim, y = too_slow)) +
  geom_violin() +
  facet_grid(.~block, scales = "free_x") +
  ylab("stimulus") +
  xlab("too slow") +
  ggtitle("Rate of trials where time pressure was exceeded") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("../../output/images/too_slow.jpg")
