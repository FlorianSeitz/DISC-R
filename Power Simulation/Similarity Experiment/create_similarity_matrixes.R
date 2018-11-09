library(data.table)
#source("VariableDefinition.R")
source("../Experiment_Python/Similarity Experiment/make.stimuli.R")

# Create stimulis and omit identical stimuli
stimuli.left <- rbind(lower.stimuli.1[1:(nrow(lower.stimuli.1)-4),], upper.stimuli.1[1:(nrow(lower.stimuli.1)-4),])
stimuli.right <- rbind(lower.stimuli.2[1:(nrow(lower.stimuli.1)-4),], upper.stimuli.2[1:(nrow(lower.stimuli.1)-4),])

stimulis <- expand.grid(stimuli.left = unique(stimuli.left$ID), stimuli.right = unique(stimuli.right$ID))
stimulis$id.stim.pair <- paste(stimulis$stimuli.left, stimulis$stimuli.right, sep = "-")

# Parameter space
parameter.space <- make.grid(n.dim = n.dim, steps = 20, c = seq(0.1, 4.1, 0.5), add.noise = FALSE)

predictions <- data.table(w1 = NA, w2 = NA, w3 = NA, c = NA, stimuli.left = NA, stimuli.right = NA, id.stim.pair = NA, model = NA, similarity = NA)

for(i in 1:nrow(parameter.space)) {
  print(paste(i, " of ", nrow(parameter.space)))
  pars <- parameter.space[i, ]
  
  # Calculate similarity matrix
  similarities_disc <- as.vector(similarity.matrix(x = stimuli.left, y = stimuli.right, w = pars[1:3], c = pars[4], p = 1, r = 1, Metric = "Attr"))
  similarities_mink <- as.vector(similarity.matrix(x = stimuli.left, y = stimuli.right, w = pars[1:3], c = pars[4], p = 1, r = 1, Metric = "Eucl"))
  
  similarities_disc <- data.table(w1 = pars[1], w2 = pars[2], w3 = pars[3], c = pars[4], 
                                  stimuli.left = stimulis$stimuli.left, stimuli.right = stimulis$stimuli.right, 
                                  id.stim.pair = stimulis$id.stim.pair, model = "Attr", similarity = similarities_disc)
  similarities_mink <- data.table(w1 = pars[1], w2 = pars[2], w3 = pars[3], c = pars[4], 
                                  stimuli.left = stimulis$stimuli.left, stimuli.right = stimulis$stimuli.right, 
                                  id.stim.pair = stimulis$id.stim.pair, model = "Eucl", similarity = similarities_mink)
  predictions <- rbind(result, similarities_disc, similarities_mink)
}

predictions <- predictions[!is.na(w1), ]
fwrite(predictions, "Data/Similarity Matrices/predictions.R")
