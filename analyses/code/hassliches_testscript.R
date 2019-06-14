source("fig_setup.R")
c10 <- fread("../../data/processed/categorization_main_fitted_parm_gcm_c10.csv")
c5 <- fread("../../data/processed/categorization_main_fitted_parm_gcm_c5.csv")
c10_trial100 <- fread("../../data/processed/categorization_main_fitted_parm_gcm.csv")

c10 <- c10[subj_id %in% unique(c5$subj_id), ]
c10_trial100 <- c10_trial100[subj_id %in% unique(c5$subj_id), ]
setkey(c10, subj_id, metric)
setkey(c5, subj_id, metric)
setkey(c10_trial100, subj_id, metric)

c10 <- melt(c10, id.vars = 1:2)
c5 <- melt(c5, id.vars = 1:2)
c10_trial100 <- melt(c10_trial100, id.vars = 1:2)

c10[, type := "c10"]
c5[, type := "c5"]
c10_trial100[, type := "c10_trial100"]

data <- rbind(c10, c5, c10_trial100)
data <- dcast(data, formula = ...~type)
cor(c10$w1, c5$w1)

ggplot(data, mapping = aes(x = c10, y = c10_trial100)) +
  geom_point(size = 2) +
  geom_abline(slope = 1) +
  # xlim(0, 1) +
  # ylim(0, 1) +
  facet_wrap(~variable, scales = "free")


dt <- fread("../../data/processed/categorization_exp_main.csv", colClasses = list(character = "stim"))
dt <- dt[subj_id == "vclt" & block == "training", ]
ggplot(dt, aes(x = stim, y = as.numeric(response == true_cat))) +
  geom_point(stat = "summary", fun = "mean", size = 3) +
  ylim(0, 1)
