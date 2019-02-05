# Preprocess
rm(list = ls(all = TRUE))
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio

files <- list.files(path = "../../data/raw/categorization_exp_pretest/", pattern = "", full.names = TRUE)
dt <- rbindlist(lapply(files, fread, fill = TRUE, colClasses = list("character" = "stim")))

# Makes stimuli start with 0
stim <- matrix(as.numeric(t(dt[, strsplit(as.character(stim), split = "")])), ncol = 3) - 1
dt <- cbind(stim, dt)
dt[, stim := paste0(V1, V2, V3)]
setnames(dt, old = c("V1", "V2", "V3"), new = c(paste0("feature", 1:3)))

# Change participant ids to random alpha-numeric code
set.seed(432)
# dt[, subj_id := factor(subj_id, labels = replicate(length(unique(subj_id)), paste(sample(letters, 4), collapse = "")))]

setcolorder(dt, c(4, 14:15, 5:7, 1:3, 8, 12, 13, 9:11))
fwrite(dt, "../../data/processed/categorization_exp_pretest.csv")
