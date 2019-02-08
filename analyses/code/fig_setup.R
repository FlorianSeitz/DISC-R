# Plot setup
library(ggplot2)
library(Hmisc)
theme_set(theme_minimal())
cols <- c("MINK" = "red", "MINK_UNIDIM" = "green", "DISC" = "blue", "DISC_UNIDIM" = "burlywood4", "RANDOM" = "black", "obs" = "lightgrey")
stim_order <- c("002", "012", "101", "111", "221", "231", "321", "331", "003", "100")

xl <- xlab("Stimulus")
yl <- ylab("Percentage Cat 1 responses")

cat("\n Loaded col, stim_order, xl, and yl \n")