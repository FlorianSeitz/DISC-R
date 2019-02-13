library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")

reaction_time <- 0:3000
time_pressure_mixed <- 400 + reaction_time * 0.3
time_pressure_ind <- reaction_time * 0.5

dt <- data.table(reaction_time, time_pressure_ind, time_pressure_mixed)
dt <- melt(dt, id.vars = "reaction_time", variable.name = "func", value.name = "time")

ggplot(dt, aes(x = reaction_time, y = time, color = func)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = 2)
