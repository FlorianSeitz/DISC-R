library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("fig_setup.R")
source("5_compares_models.R")

weights <- melt(weights, id.vars = "subj_id", variable.name = "model", value.name = "weight")
weights[, model := toupper(model)]

ggplot(weights, aes(x = subj_id, y = weight, fill = model)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cols) +
  ggtitle("Distribution of AIC weights per participant")

ggsave("../../output/images/aicweights.jpg")
