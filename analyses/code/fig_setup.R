# Plot setup
library(ggplot2)
library(Hmisc)
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
stim_order <- c("002", "012", "101", "111", "221", "231", "321", "331", "003", "100")

xl <- xlab("Stimulus")
yl <- ylab("Percentage Cat 1 responses")

cat("\n Loaded col, stim_order, xl, and yl \n")