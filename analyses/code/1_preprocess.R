# Preprocess
rm(list = ls(all = TRUE))
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio

files <- list.files(path = "../../data/raw/categorization_exp_pretest/", pattern = "", full.names = TRUE)
dt <- rbindlist(lapply(files, fread, fill = TRUE))
stim <- matrix(as.numeric(t(dt[, strsplit(as.character(stim), split = "")])), ncol = 3)
dt <- cbind(stim, dt)

# Change participant ids to random alpha-numeric code
set.seed(432)
dt[, factor(subj_id, levels = repliate(sample(letters, 4), length(unique(subj_id))))]

fwrite(dt, "../../data/processed/categorization_exp_pretest.csv")