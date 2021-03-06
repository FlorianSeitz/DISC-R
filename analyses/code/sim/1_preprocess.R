# Preprocess
rm(list = ls(all = TRUE))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # changes working directory to folder where this script is
library(data.table)
set.seed(932)

files <- list.files(path = "../../../data/raw/similarity_exp_main/", pattern = "", full.names = TRUE)
dt <- rbindlist(lapply(files, fread, fill = TRUE, colClasses = list("character" = c("type", "color_presentation_order", "shape_presentation_order"))))
demographics <- fread("../../../data/raw/similarity_demographics_main/Similarity Experiment Demographics_January 21, 2020_03.55.csv")

# Extracts feature values
dt <- cbind(matrix(as.numeric(t(dt[, strsplit(as.character(stim_right), split = "")])), ncol = 3), dt) # stim_right
setnames(dt, old = c("V1", "V2", "V3"), new = c(paste0("feature", 1:3, "_r")))
dt <- cbind(matrix(as.numeric(t(dt[, strsplit(as.character(stim_left), split = "")])), ncol = 3), dt) # stim_left
setnames(dt, old = c("V1", "V2", "V3"), new = c(paste0("feature", 1:3, "_l")))

# Time pressure: Checks if the time limit is exceeded in more than 50% of the test trials for any given stimulus pair type.
valid_subj_ids <- rbind(dt[time_pressure_first == TRUE & block == "test_1", ],
                        dt[time_pressure_first == FALSE & block == "test_2", ])[, .(perc_too_slow = mean(is.na(response))), by = list(subj_id, type)][, all(perc_too_slow <= .50), by = subj_id]$subj_id

# No time pressure: Checks if log transformed contemplation time is below M - 3SD of the log transformed reaction times of the familiarization phase in more than 50% of the test trials for any given stimulus pair type.
log_times_fam <- rbind(dt[time_pressure_first == TRUE & block == "familiarization", ],
                   dt[time_pressure_first == FALSE & block == "familiarization", ])[, .(mean_log_reaction = mean(log(reaction_time)),
                                                                               sd_log_reaction = sd(log(reaction_time))), by = list(subj_id)]
log_times <- merge(rbind(dt[time_pressure_first == TRUE & block == "test_2", ],
                         dt[time_pressure_first == FALSE & block == "test_1", ])[, .(log_test = log(contemplation_time)), by = list(subj_id, type)], log_times_fam)
valid_subj_ids <- unique(c(log_times[, .(perc_too_slow = mean(log_test < mean_log_reaction - 3*sd_log_reaction)),
                                     by = list(subj_id, type)][, all(perc_too_slow <= .50), by = subj_id]$subj_id, valid_subj_ids))

# Drops invalid subj_ids
dt <- dt[subj_id %in% valid_subj_ids, ]

# Participants who stated task was rather unclear or absolutely unclear
invalid_subj_ids <- demographics[grepl("un", task_clear), sub(".*=", "", Q_URL)]
dt <- dt[subj_id %in% invalid_subj_ids == FALSE, ]

# Prepares demographics data
demographics <- demographics[!1:2, c(18:26, 29)]
demographics <- demographics[!grep("un", task_clear)]

# Change participant ids to random alpha-numeric code
set.seed(432)
dt[, subj_id := factor(subj_id, labels = replicate(length(unique(subj_id)), paste(sample(letters, 4), collapse = "")))]

# Adds variable time pressure condition
dt[, time_pressure := ifelse((time_pressure_first == TRUE & block == "test_1") | (time_pressure_first == FALSE & block == "test_2") | (block == "familiarization_time_pressure"), TRUE, FALSE)]
dt[, time_pressure := factor(time_pressure, levels = c(TRUE, FALSE), labels = c(TRUE, FALSE))]

# Rescales responses to lie between 0 and 1
dt[!is.na(response), response_s := (response - min(response))/max(response - min(response)), by = subj_id]

# Rescales responses such that -500 = 0 and 500 = 1 (exploratory)
dt[!is.na(response), response_s_exp := (response + 500)/1000]

# setcolorder(dt, c(4, 14:15, 5:7, 17, 1:3, 8, 12, 13, 16, 9:11))
fwrite(dt, "../../../data/processed/similarity_exp_main.csv")
fwrite(demographics, "../../../data/processed/similarity_demographics_main.csv")
