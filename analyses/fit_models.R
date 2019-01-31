# Fit Models

library(data.table)
library(ggplot2)
library(Hmisc)
theme_set(theme_bw())

source("Models/GCM_Prot.R")

files <- list.files(path = "../Experiment_Python/Categorization Experiment/data/pretest/csv/", pattern = "", full.names = TRUE) # *.csv$
dat <- rbindlist(lapply(files, fread, fill = TRUE))

dat[, subj_id := as.factor(subj_id)]

dat[, paste0("Dim", 1:3) := lapply(1:3, function(i) as.numeric(substr(stim, i, i)))]

fit_and_get_parameters <- function(dat, metric) {
  n.dim <- 3
  setnames(dat, old = c("stim", "true_cat"), new = c("ID", "Cat"))
  ls <- as.data.frame(dat[, c("ID", paste0("Dim", 1:3), "Cat")])
  os <- as.data.frame(dat[, c("ID", paste0("Dim", 1:3), "response")])
  setnames(os, old = "response", new = "Cat")
  N <- iterative.frequencies(x = ls)
  
  result <- Max.likelihood.optimization.solnp(n.dim = n.dim,
                                              Observedset = os, 
                                              Learningset = ls,
                                              Model = "GCM", 
                                              Metric = metric,
                                              ignore = 8,
                                              N = N,
                                              convergence = T)
  return(unlist(result, use.names = TRUE))
}

dat[block == "training" & subj_id == "6", fit_and_get_parameters(.SD, metric = "Eucl")]
fitted.w <- head(fitted.par, -1)
fitted.c <- tail(fitted.par, 1)
