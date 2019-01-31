rm(list = ls(all = TRUE))
library(data.table)
library(plotly)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("~/cogscimodels/models/gcm/gcm_unidim.R", chdir = TRUE)

dt <- fread("../../data/processed/categorization_exp_pretest.csv")

grid <- as.data.table(expand.grid(cs = seq(0.1, 5.1, 0.5), taus = seq(0.1, 5.1, 0.5)))
grid[, log.lik := gof(pred = gcm_unidim(formula = response ~ V1 + V2 + V3,
                                        data = dt[block == "training" & subj_id == 6], 
                                        cat = ~ true_cat, 
                                        metric = "d",
                                        choicerule = "soft", 
                                        fixed = c(w1 = 0, 
                                                  w2 = 1, 
                                                  w3 = 0, 
                                                  r = 1, 
                                                  p = 1, 
                                                  tau = taus, 
                                                  c = cs))$predict(newdata = dt[block != "training" & subj_id == 6]), 
                      obs = dt[block != "training" & subj_id == 6, response], 
                      type = "log", 
                      response = "discrete",
                      na.rm = TRUE), by = 1:nrow(grid)]

plot_ly(data = grid, x = ~taus, y = ~cs, z = ~log.lik, type = "mesh3d")
