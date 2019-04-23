# Preprocess
rm(list = ls(all = TRUE))
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio

files <- list.files(path = "../../data/raw/categorization_exp_main/", pattern = "", full.names = TRUE)
dt <- rbindlist(lapply(files, fread, fill = TRUE, colClasses = list("character" = c("stim", "color_presentation_order", "feature_presentation_order"))))
demographics <- fread("../../data/raw/categorization_demographics_main/Categorization Experiment Demographics_April 8, 2019_05.10.csv")

# Makes stimuli start with 0
stim <- matrix(as.numeric(t(dt[, strsplit(as.character(stim), split = "")])), ncol = 3) - 1
dt <- cbind(stim, dt)
dt[, stim := paste0(V1, V2, V3)]
setnames(dt, old = c("V1", "V2", "V3"), new = c(paste0("feature", 1:3)))

# Checks if time limit is exceeded in > 50% of test trials for a given test stimulus
valid_subj_ids <- dt[time_pressure_cond == TRUE & block == "test", .(perc_too_slow = mean(too_slow)), by = list(subj_id, stim)][, all(perc_too_slow <= .50), by = subj_id]$subj_id

# Checks if log reaction time is below M-3SD of log reaction times of learning phase in > 50% of test trials for a given test stimulus
log_times <- dt[time_pressure_cond == FALSE & block == "training" & time > 0, .(mean_log_learn = mean(log(time)),
                                                                                sd_log_learn = sd(log(time))), by = list(subj_id)]
log_times <- merge(dt[time_pressure_cond == FALSE & block == "test" & time > 0, .(log_test = log(time)), by = list(subj_id, stim)], log_times)
valid_subj_ids <- c(valid_subj_ids, log_times[, .(perc_too_slow = mean(log_test < mean_log_learn - 3*sd_log_learn)), by = list(subj_id, stim)][, all(perc_too_slow <= .50), by = subj_id]$subj_id)

# Drops invaid subj_ids
dt <- dt[subj_id %in% valid_subj_ids, ]

# Participants who stated task was rather unclear or absolutely unclear
invalid_subj_ids <- c(4, 9, 16, 32, 34, 40, 46, 54)
dt <- dt[subj_id %in% invalid_subj_ids == FALSE, ]

# Prepares demographics data
demographics <- demographics[!1:2, !3:17]
demographics[, subj_id := 1:nrow(demographics)]

# Change participant ids to random alpha-numeric code
set.seed(432)
dt[, subj_id := factor(subj_id, labels = replicate(length(unique(subj_id)), paste(sample(letters, 4), collapse = "")))]

# Old or novel stimuli
dt[, stim_type := ifelse(stim %in% unique(stim[block == "training"]), "old", "new")]

setcolorder(dt, c(4, 14:15, 5:7, 17, 1:3, 8, 12, 13, 16, 9:11))
fwrite(dt, "../../data/processed/categorization_exp_main.csv")
