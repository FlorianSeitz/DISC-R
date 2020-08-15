# Plot setup
rm(list = ls(all = TRUE))
library(data.table)
library(ggplot2)
library(Hmisc)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 0. Formal plotting features
theme_set(theme_bw())
themes <- c(strip.text = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10))

col_cols <- c("MINK-MULTI" = "red", "MINK-UNI" = "green", "DISC-MULTI" = "lightblue", "DISC-UNI" = "burlywood4", "RANDOM" = "grey", "obs" = "lightgrey")
bw_cols <- c("MINK-MULTI" = grey(0.75), 
             "MINK-UNI" = grey(0.25), 
             "DISC-MULTI" = grey(1), 
             "DISC-UNI" = grey(0.5), 
             "RANDOM" = grey(0))
bw_cols_light <- c("MINK-MULTI" = grey(0.75, 0.5), 
                   "MINK-UNI" = grey(0.25, 0.5), 
                   "DISC-MULTI" = grey(1, 0.5), 
                   "DISC-UNI" = grey(0.5, 0.5), 
                   "RANDOM" = grey(0, 0.5))
levels <- c("A", "B", "C", "D", "I", "V")
labels <- c("one-extreme 153-553", "all-moderate 234-343", "two-extreme 315-351", "two-moderate 523-532", "all-equal 223-223", "all-different 111-444")

xl <- xlab("Stimulus type")
yl <- ylab("Similarity rating")

tp_labs <- c("Time pressure", "No time pressure")
names(tp_labs) <- c(TRUE, FALSE)

# 1. Prepares data
dt <- fread("../../../data/processed/similarity_exp_main.csv",
            colClasses = list("character" = "stim_left", "character" = "stim_right"))
dt <- dt[grep("test", block), ]
dt[, subj_id := as.factor(subj_id)]
dt[, type := factor(type, 
                    levels = levels, 
                    labels = labels)]

cat("\n Loaded col_cols, bw_cols, bw_cols_light, stim_order, xl, yl, tp_labs, and dt \n")
