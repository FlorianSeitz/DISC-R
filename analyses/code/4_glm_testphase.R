# Runs a general linear model with logit link on the test data
library(data.table)
library(lme4)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio

dat <- fread("../../data/processed/categorization_data_with_predictions.csv")

dat <- dat[block == "test", ]

dat[, subj_id := as.factor(subj_id)]

glm_res <- glmer(response ~ time_pressure_cond * stim + (0+x|subj_id), data = dat, family = binomial)
summary(glm_res)
