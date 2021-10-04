# Wills Experiment 1 Reanalysis

# 0. Load packages and R files
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(yarrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # change this to the folder where this script is lying; works only in RStudio
source("Models/GCM_Prot.R")
source("Models/Similarity Modelle.R")
source("~/FS' 18/Forecasting/Error Measures.R")
setwd("C:/Users/Sylvia/Documents/Masterarbeit/Literatur Masterarbeit/Forced choice similarity classification/Wills 2015")

theme_set(theme_bw())
# 1. Read raw data
exp1 <- read.table(file = "Experiment 1/exe1data.txt", header = T, sep = "\t")
exp1 <- as.data.table(exp1)
# cond: Experimental condition (expressed as stimulus presentation time in milliseconds)
# subj: Participant ID (unqiue within each condition of EXE1)
# trial: Trial number
# triad: ID of presented stimulus triad (see exe1code.txt)
# resp: Participant's response ( 1 = top , 2 = left, 3 = right )
# rt: Reaction time in milliseconds

stimuli1 <- read.table(file = "Experiment 1/exe1code.txt", header = T, sep = "\t")
stimuli1 <- as.data.table(stimuli1)
# triad: Triad ID (as used in exe1data.txt)
# triplet: Triplet ID. Any given stimulus triplet can be presented in six different ways (three stimuli can be placed in three physical locations in six different ways). Hence, there are six different stimulus triad IDs for each triplet ID.
# type: Type 1 or Type 2 triad (not used).
# ident.dim: The stimulus dimension on which two members of the triplet match identically (1 = hull, 2 = sail)
# top1: Stimulus ID for stimulus presented at the top of the triad. Stimulus ID is a two-digit co-ordinate code in arbitrary units. For example Stimulus ID 25 indicates a value of 2 on the hull dimension and a value of 5 on the sail dimension. Larger values on the hull dimension are associated with increasing width of the bottom of the hull. Larger values on the sail dimension are associated with increasing width of the bottom of the sail. The mapping between these stimulus IDs and the stimulus numbers in Figure 5 of Milton, Longmore, Wills (2008), and the numbering of the graphic files in this archive, are as follows: 1 = 16, 2 = 66, 3 = 25, 4 = 55, 5 = 22, 6 = 52, 7 = 11, 8 = 16
# left2: Stimulus ID for stimulus presented at the left of the triad.
# right3: Stimulus ID for stimulus presented at the right of the triad.
# hull: Response expected if classifying on the basis of hull width ( 1 = top , 2 = left, 3 = right )
# sail: Response expected if classifying on the basis of sail width
# os: Response expected if classifying on the basis of overall similarity
# ident: Response expected if classifying on the basis of the identical attribute.

# 2. Take dimension values of stimuli
triads <- stimuli1[, 5:7]
# 2.1. Top
top <- t(sapply(triads$top1, function(x) substring(x, first=c(1,2), last=c(1,2))))
colnames(top) <- c("Dim1", "Dim2")
top <- as.data.frame(apply(top, 2, as.numeric))
top$ID <- 1
# 2.2. Left
left <- t(sapply(triads$left2, function(x) substring(x, first=c(1,2), last=c(1,2))))
colnames(left) <- c("Dim1", "Dim2")
left <- as.data.frame(apply(left, 2, as.numeric))
left$ID <- 2
# 2.3. Right
right <- t(sapply(triads$right3, function(x) substring(x, first=c(1,2), last=c(1,2))))
colnames(right) <- c("Dim1", "Dim2")
right <- as.data.frame(apply(right, 2, as.numeric))
right$ID <- 3

# 3. Analysis: always take the i-th row of top, exemplar 1, and exemplar 2
# 3.1. Discrete, City-Block, and Eucliean Metric
# 3.1.1. Similarities: go through each possible triad, choose first stimuli as top and the others as left and right
sim.discrete.metric <- NULL
sim.city.block.metric <- NULL
sim.euclidean.metric <- NULL
sim.unidim1.eucl <- NULL
sim.unidim2.eucl <- NULL
sim.unidim1.disc <- NULL
sim.unidim2.disc <- NULL
sim.identity.attr <- NULL
sim.identity.eucl <- NULL

for(i in 1:nrow(top)) {
  current.items <- rbind(top[i, ], left[i, ], right[i, ])
  
  discrete <- t(Similarity(Stimuli = current.items, 
                         Metric = "Attr", 
                         c = 1, w = c(.5, .5), p = 1, r = 1))
  sim.discrete.metric <- rbind(sim.discrete.metric, discrete)
  
  city.block <- t(Similarity(Stimuli = current.items, 
                           Metric = "Eucl", 
                           c = 1, w = c(.5, .5), p = 1, r = 1))
  sim.city.block.metric <- rbind(sim.city.block.metric, city.block)
  
  euclidean <- t(Similarity(Stimuli = current.items, 
                          Metric = "Eucl", 
                          c = 1, w = c(.5, .5), p = 1, r = 2))
  sim.euclidean.metric <- rbind(sim.euclidean.metric, euclidean)
  
  uni.dim1.eucl <- t(Similarity(Stimuli = current.items, 
                              Metric = "Eucl", 
                              c = 1, w = c(1, 0), p = 1, r = 1))
  sim.unidim1.eucl <- rbind(sim.unidim1.eucl, uni.dim1.eucl)
  
  uni.dim2.eucl <- t(Similarity(Stimuli = current.items, 
                              Metric = "Eucl", 
                              c = 1, w = c(0, 1), p = 1, r = 1))
  sim.unidim2.eucl <- rbind(sim.unidim2.eucl, uni.dim2.eucl)
  
  uni.dim1.disc <- t(Similarity(Stimuli = current.items, 
                              Metric = "Attr", 
                              c = 1, w = c(1, 0), p = 1, r = 1))
  sim.unidim1.disc <- rbind(sim.unidim1.disc, uni.dim1.disc)
  
  uni.dim2.disc <- t(Similarity(Stimuli = current.items, 
                              Metric = "Attr", 
                              c = 1, w = c(0, 1), p = 1, r = 1))
  sim.unidim2.disc <- rbind(sim.unidim2.disc, uni.dim2.disc)
  
  w1 <- as.numeric(length(unique(current.items[, 1])) < nrow(current.items))
  w2 <- as.numeric(length(unique(current.items[, 2])) < nrow(current.items))
  
  identity.attr <- t(Similarity(Stimuli = current.items, 
                              Metric = "Attr", 
                              c = 1, w = c(w1, w2), p = 1, r = 1))
  sim.identity.attr <- rbind(sim.identity.attr, identity.attr)
  
  identity.eucl <- t(Similarity(Stimuli = current.items, 
                              Metric = "Eucl", 
                              c = 1, w = c(w1, w2), p = 1, r = 1))
  sim.identity.eucl <- rbind(sim.identity.eucl, identity.eucl)
}
colnames(sim.discrete.metric) <- c("1.2", "1.3", "2.3") # shows of which items the similarity is computed. 1 = top, 2 = left, 3 = right
colnames(sim.city.block.metric) <- c("1.2", "1.3", "2.3") # shows of which items the similarity is computed. 1 = top, 2 = left, 3 = right
colnames(sim.euclidean.metric) <- c("1.2", "1.3", "2.3") # shows of which items the similarity is computed. 1 = top, 2 = left, 3 = right
colnames(sim.unidim1.eucl) <- c("1.2", "1.3", "2.3") # shows of which items the similarity is computed. 1 = top, 2 = left, 3 = right
colnames(sim.unidim2.eucl) <- c("1.2", "1.3", "2.3") # shows of which items the similarity is computed. 1 = top, 2 = left, 3 = right
colnames(sim.unidim1.disc) <- c("1.2", "1.3", "2.3") # shows of which items the similarity is computed. 1 = top, 2 = left, 3 = right
colnames(sim.unidim2.disc) <- c("1.2", "1.3", "2.3") # shows of which items the similarity is computed. 1 = top, 2 = left, 3 = right
colnames(sim.identity.attr) <- c("1.2", "1.3", "2.3") # shows of which items the similarity is computed. 1 = top, 2 = left, 3 = right
colnames(sim.identity.eucl) <- c("1.2", "1.3", "2.3") # shows of which items the similarity is computed. 1 = top, 2 = left, 3 = right

sim.discrete.metric <- as.data.frame(sim.discrete.metric, row.names = F)
sim.city.block.metric <- as.data.frame(sim.city.block.metric, row.names = F)
sim.euclidean.metric <- as.data.frame(sim.euclidean.metric, row.names = F)
sim.unidim1.eucl <- as.data.frame(sim.unidim1.eucl, row.names = F)
sim.unidim2.eucl <- as.data.frame(sim.unidim2.eucl, row.names = F)
sim.unidim1.disc <- as.data.frame(sim.unidim1.disc, row.names = F)
sim.unidim2.disc <- as.data.frame(sim.unidim2.disc, row.names = F)
sim.identity.attr <- as.data.frame(sim.identity.attr, row.names = F)
sim.identity.eucl <- as.data.frame(sim.identity.eucl, row.names = F)

# 3.1.2.Probabilistic Predictions
# triads$odd.out.discrete <- 0
pred.discrete.metric <- sim.discrete.metric
pred.city.block.metric <- sim.city.block.metric
pred.euclidean.metric <- sim.euclidean.metric
pred.unidim1.eucl <- sim.unidim1.eucl
pred.unidim2.eucl <- sim.unidim2.eucl
pred.unidim1.disc <- sim.unidim1.disc
pred.unidim2.disc <- sim.unidim2.disc
pred.identity.attr <- sim.identity.attr
pred.identity.eucl <- sim.identity.eucl

for(i in 1:nrow(sim.discrete.metric)) {
  for(j in 1:ncol(sim.discrete.metric)){
    pred.discrete.metric[i, j] <- sim.discrete.metric[i, j] / sum(sim.discrete.metric[i, ])
    pred.city.block.metric[i, j] <- sim.city.block.metric[i, j] / sum(sim.city.block.metric[i, ])
    pred.euclidean.metric[i, j] <- sim.euclidean.metric[i, j] / sum(sim.euclidean.metric[i, ])
    pred.unidim1.eucl[i, j] <- sim.unidim1.eucl[i, j] / sum(sim.unidim1.eucl[i, ])
    pred.unidim2.eucl[i, j] <- sim.unidim2.eucl[i, j] / sum(sim.unidim2.eucl[i, ])
    pred.unidim1.disc[i, j] <- sim.unidim1.disc[i, j] / sum(sim.unidim1.disc[i, ])
    pred.unidim2.disc[i, j] <- sim.unidim2.disc[i, j] / sum(sim.unidim2.disc[i, ])
    pred.identity.attr[i, j] <- sim.identity.attr[i, j] / sum(sim.identity.attr[i, ])
    pred.identity.eucl[i, j] <- sim.identity.eucl[i, j] / sum(sim.identity.eucl[i, ])
    # most.similar <- names(which.max(sim.discrete.metric[i, ]))
    # most.similar <- as.numeric( unlist( strsplit(most.similar, ".", fixed = T) ) )
    # triads$odd.out.discrete[i] <- which(1:3 %in% most.similar == FALSE)
  }
}

colnames(pred.discrete.metric) <- c("response3.frequency", "response2.frequency", "response1.frequency") # shows which item is odd-out. 1 = top, 2 = left, 3 = right
colnames(pred.city.block.metric) <- c("response3.frequency", "response2.frequency", "response1.frequency") # shows which item is odd-out. 1 = top, 2 = left, 3 = right
colnames(pred.euclidean.metric) <- c("response3.frequency", "response2.frequency", "response1.frequency") # shows which item is odd-out. 1 = top, 2 = left, 3 = right
colnames(pred.unidim1.eucl) <- c("response3.frequency", "response2.frequency", "response1.frequency") # shows which item is odd-out. 1 = top, 2 = left, 3 = right
colnames(pred.unidim2.eucl) <- c("response3.frequency", "response2.frequency", "response1.frequency") # shows which item is odd-out. 1 = top, 2 = left, 3 = right
colnames(pred.unidim1.disc) <- c("response3.frequency", "response2.frequency", "response1.frequency") # shows which item is odd-out. 1 = top, 2 = left, 3 = right
colnames(pred.unidim2.disc) <- c("response3.frequency", "response2.frequency", "response1.frequency") # shows which item is odd-out. 1 = top, 2 = left, 3 = right
colnames(pred.identity.attr) <- c("response3.frequency", "response2.frequency", "response1.frequency") # shows which item is odd-out. 1 = top, 2 = left, 3 = right
colnames(pred.identity.eucl) <- c("response3.frequency", "response2.frequency", "response1.frequency") # shows which item is odd-out. 1 = top, 2 = left, 3 = right

pred.discrete.metric$triad <- stimuli1$triad
pred.city.block.metric$triad <- stimuli1$triad
pred.euclidean.metric$triad <- stimuli1$triad
pred.unidim1.eucl$triad <- stimuli1$triad
pred.unidim2.eucl$triad <- stimuli1$triad
pred.unidim1.disc$triad <- stimuli1$triad
pred.unidim2.disc$triad <- stimuli1$triad
pred.identity.attr$triad <- stimuli1$triad
pred.identity.eucl$triad <- stimuli1$triad

# 4. Actual answers
actual.answers <- exp1 %>%
  group_by(triad, cond) %>%
  summarise(
    response1.frequency = sum(resp == 1)/length(resp),
    response2.frequency = sum(resp == 2)/length(resp),
    response3.frequency = sum(resp == 3)/length(resp)
  )

actual.answers.absolute <- exp1 %>%
  group_by(triad, cond)

# 5. Plot actual and predicted answers
ggplot(data = actual.answers, aes(x = response1.frequency, y = response2.frequency)) +
  geom_point(mapping = aes(color = as.factor(cond)), size = 2) +
  scale_colour_manual(values = c(gray(0.7), gray(0.55), gray(0.4), gray(0.25), gray(0.1))) +
  geom_point(data = pred.discrete.metric, color = "red", size = 2) + 
  geom_point(data = pred.city.block.metric, color = "lightblue", size = 2) +
  geom_point(data = pred.euclidean.metric, color = "darkblue", size = 2) +
  geom_point(data = pred.unidim1.eucl, color = "lightgreen", size = 2) + 
  geom_point(data = pred.unidim2.eucl, color = "orange", size = 2) +
  geom_point(data = pred.unidim1.disc, color = "darkgreen", size = 2) +
  geom_point(data = pred.unidim2.disc, color = "brown", size = 2) + 
  geom_point(data = pred.identity.attr, color = "pink", size = 2) +
  geom_point(data = pred.identity.eucl, color = "violet", size = 2) +
  geom_abline(intercept = 1, slope = -1, linetype = 2) +
  theme_bw() +
  facet_wrap(~triad) +
  xlim(0, 1) +
  ylim(0, 1) +
  xlab("Response 1 Frequency") +
  ylab("Response 2 Frequency") +
  labs(title = "Experiment 1: Response Frequencies and Predictions of the Discrete Metric (red), the City-Block Metric (lightblue), the Euclidean Metric (darkblue), \n the unidimensional model (w1-disrete metric (darkgreen), w2-city-block metric (lightgreen), w2-discrete metric (brown), w2-city-block metric (orange), \n and the identity model with discrete metric (pink) or city-block metric (violet)") +
  labs(color = "Time") + 
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 12, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 12))

# 6. Log likelihood calculation
# 6.1. Generate Response Frequency List
pred.discrete.metric <- pred.discrete.metric[, c(3, 2, 1, 4)] # reorder columns, so response equals column
pred.city.block.metric <- pred.city.block.metric[, c(3, 2, 1, 4)] # reorder columns, so response equals column
pred.euclidean.metric <- pred.euclidean.metric[, c(3, 2, 1, 4)] # reorder columns, so response equals column
pred.unidim1.eucl <- pred.unidim1.eucl[, c(3, 2, 1, 4)]
pred.unidim2.eucl <- pred.unidim2.eucl[, c(3, 2, 1, 4)]
pred.unidim1.disc <- pred.unidim1.disc[, c(3, 2, 1, 4)]
pred.unidim2.disc <- pred.unidim2.disc[, c(3, 2, 1, 4)]
pred.identity.attr <- pred.identity.attr[, c(3, 2, 1, 4)]
pred.identity.eucl <- pred.identity.eucl[, c(3, 2, 1, 4)]

response.frequency.list <- list(pred.resp.discrete = pred.discrete.metric, 
                                pred.resp.city = pred.city.block.metric, 
                                pred.resp.eucl = pred.euclidean.metric, 
                                pred.unidim1.eucl = pred.unidim1.eucl, 
                                pred.unidim2.eucl = pred.unidim2.eucl, 
                                pred.unidim1.disc = pred.unidim1.disc, 
                                pred.unidim2.disc = pred.unidim2.disc, 
                                pred.identity.disc = pred.identity.attr, 
                                pred.identity.eucl = pred.identity.eucl)

# pred.resp.discrete <- NULL
# 
# pred.resp.city <- NULL
# 
# pred.resp.eucl <- NULL
# 
# pred.unidim1.eucl.vector <- NULL
# 
# pred.unidim2.eucl.vector <- NULL
# 
# pred.unidim1.disc.vector <- NULL
# 
# pred.unidim2.disc.vector <- NULL
# 
# pred.identity.attr.vector <- NULL
# 
# pred.identity.eucl.vector <- NULL
# 
# for(i in 1:length(exp1$resp)) {
#   response <- exp1$resp[i]
#   triad <- exp1$triad[i]
#   pred.resp.discrete[i] <- pred.discrete.metric[triad, response]
#   pred.resp.city[i] <- pred.city.block.metric[triad, response]
#   pred.resp.eucl[i] <- pred.euclidean.metric[triad, response]
#   pred.unidim1.eucl.vector[i] <- pred.unidim1.eucl[triad, response]
#   pred.unidim2.eucl.vector[i] <- pred.unidim2.eucl[triad, response]
#   pred.unidim1.disc.vector[i] <- pred.unidim1.disc[triad, response]
#   pred.unidim2.disc.vector[i] <- pred.unidim2.disc[triad, response]
#   pred.identity.attr.vector[i] <- pred.identity.attr[triad, response]
#   pred.identity.eucl.vector[i] <- pred.identity.eucl[triad, response]
# }
# 
# # General Log likelihood
# prediction.list <- list(pred.resp.discrete = pred.resp.discrete, 
#                         pred.resp.city = pred.resp.city, 
#                         pred.resp.eucl = pred.resp.eucl, 
#                         pred.unidim1.eucl = pred.unidim1.eucl.vector, 
#                         pred.unidim2.eucl = pred.unidim2.eucl.vector, 
#                         pred.unidim1.disc = pred.unidim1.disc.vector, 
#                         pred.unidim2.disc = pred.unidim2.disc.vector, 
#                         pred.identity.disc = pred.identity.attr.vector, 
#                         pred.identity.eucl = pred.identity.eucl.vector)

# log.vector <- NULL; model.name <- NULL
# for(i in 1:length(prediction.list)) {
#   log.vector[i] <- -sum(log(prediction.list[[i]]))
#   model.name[i] <- names(prediction.list)[i]
# }
# 
# log.dataframe <- data.table(model.name = model.name, log.vector = round(log.vector, 2))
# 
# # Log Likelihood separated for time
# time.pressures <- unique(exp1$cond)
# log.time.pressure <- matrix(nrow = length(prediction.list), ncol = length(time.pressures))
# colnames(log.time.pressure) <- time.pressures
# rownames(log.time.pressure) <- names(prediction.list)
# 
# chi.sq.p <- matrix(nrow = length(response.frequency.list), ncol = length(time.pressures))
# colnames(chi.sq.p) <- time.pressures
# rownames(chi.sq.p) <- names(response.frequency.list)
# 
# chi.sq.statistic <- matrix(nrow = length(response.frequency.list), ncol = length(time.pressures))
# colnames(chi.sq.statistic) <- time.pressures
# rownames(chi.sq.statistic) <- names(response.frequency.list)
# 
# for(i in 1:length(time.pressures)) {
#   current.time.pressure <- time.pressures[i]
#   for(j in 1:length(prediction.list)) {
#     current.rows <- prediction.list[[j]][exp1$cond == current.time.pressure]
#     log.time.pressure[j, i] <- -sum(log(current.rows))
#     current.predictions <- response.frequency.list[[j]][, 1:3]
#     chisq <- chisq.test(x = actual.answers[actual.answers$cond == current.time.pressure, 3:5], y = current.predictions)
#     chi.sq.statistic[j, i] <- chisq$statistic
#     chi.sq.p[j, i] <- chisq$p.value
#   }
# }
# 
# log.dataframe; log.time.pressure

# 6.2. Generate data for each participant and condition indicate response and prediction from all models
actual.answers.vp <- exp1 %>%
  group_by(subj, cond, triad) %>%
  summarise(
    response = resp
  )

for(i in 1:length(response.frequency.list)) {
  actual.answers.vp$new.col <- 0
  names(actual.answers.vp)[names(actual.answers.vp) == "new.col"] <- names(response.frequency.list)[i]
  for(j in 1:nrow(actual.answers.vp)) {
    triad.resp <- actual.answers.vp[j, c("triad", "response")]
    actual.answers.vp[j , names(response.frequency.list)[i]] <- response.frequency.list[[i]][triad.resp$triad, triad.resp$response]
  }
}

# 6.3. Choose the best fitting unidimensional model eucl and disc for every participant
name <- c("triad", "response")
actual.answers.vp2 <- actual.answers.vp[, c(1:2, 5:ncol(actual.answers.vp))]
actual.answers.vp2[, 3:ncol(actual.answers.vp2)] <- -log(actual.answers.vp2[, 3:ncol(actual.answers.vp2)])
vp.cond.log <- aggregate(.~ subj + cond, data = actual.answers.vp2, sum)

uni.dims <- vp.cond.log[, grepl("unidim", colnames(vp.cond.log))]
uni.dims.eucl <- uni.dims[, grepl("eucl", colnames(uni.dims))]
uni.dims.disc <- uni.dims[, grepl("disc", colnames(uni.dims))]
best.unidim.eucl <- apply(uni.dims.eucl, 1, which.min)
best.unidim.disc <- apply(uni.dims.disc, 1, which.min)
for(i in 1:length(best.unidim.eucl)) {
  vp.cond.log$pred.best.unidim.eucl[i] <- uni.dims.eucl[i, best.unidim.eucl[i]]
  vp.cond.log$pred.best.unidim.disc[i] <- uni.dims.disc[i, best.unidim.disc[i]]
}

# 6.4. Generate dataframe aggregated over participants with models: discrete, CB, euclidean, best fitting unidimensional, and idendity
vp.con.log.sub <- vp.cond.log[, c(1:5, 11:13)]
log.models.timepressure <- aggregate(.~cond, data = vp.con.log.sub, sum)
log.models.timepressure.standardized <- log.models.timepressure
log.models.timepressure.standardized[, 3:8] <- log.models.timepressure[, 3:8] / table(exp1$cond)
log.models.timepressure.standardized <- data.frame(cond = rep(log.models.timepressure$cond, times = 6), 
                                      model = rep(c("Discrete", "Manhattan", "Euclidean", "Identity", "Unidim (Minkowski)", "Unidim (Discrete)"), each = 5),
                                      log = unlist(log.models.timepressure.standardized[3:8]))

# 6.5. Plot of Log Likelihoods of every Model for every Time Pressure Condition
ggplot(data = log.models.timepressure.standardized, mapping = aes(x = factor(cond), y = log)) +
  geom_bar(mapping = aes(fill = model), stat = "identity", position = "dodge") +
  geom_errorbar(data = log.models.timepressure.standardized[log.models.timepressure.standardized$model == "Discrete", ], 
                mapping = aes(ymin = log,
                              ymax = log),
                size = 1.2) +
  theme_bw() + 
  xlab("Time Pressure") +
  ylab("Standardized Log Likelihood") +
  labs(title = "Log-likelihoods Of Similarity Models",
       subtitle = "A Reanalysis of Wills (2015), Experiment 1") +
  labs(fill = "Model") + 
  theme(axis.text=element_text(size = 10),
        axis.title=element_text(size = 12, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12))
ggsave("Experiment 1/Plot_log_lik_stand.pdf")

# 7. Error Measures
# 7.1. Choose predictions of best fitting unidimensional model
best.unidim.eucl.triad <- rep(best.unidim.eucl, each = 48) # Best fitting unidimensional model for every subject, replicated for every triad
best.unidim.disc.triad <- rep(best.unidim.disc, each = 48) # Best fitting unidimensional model for every subject, replicated for every triad
pred.unidims <- actual.answers.vp[, grepl("unidim", colnames(actual.answers.vp))]
pred.unidims.eucl <- pred.unidims[, grepl("eucl", colnames(pred.unidims))]
pred.unidims.disc <- pred.unidims[, grepl("disc", colnames(pred.unidims))]
pred.best.unidim.eucl <- NULL
pred.best.unidim.disc <- NULL
for(i in 1:nrow(pred.unidims)) {
  pred.best.unidim.eucl[i] <- pred.unidims.eucl[i, best.unidim.eucl.triad[i]]
  pred.best.unidim.disc[i] <- pred.unidims.disc[i, best.unidim.disc.triad[i]]
}
actual.answers.vp$pred.best.unidim.eucl <- unlist(pred.best.unidim.eucl)
actual.answers.vp$pred.best.unidim.disc <- unlist(pred.best.unidim.disc)

# Make subset of predictions with only best fitting models
actual.answers.vp.sub <- actual.answers.vp[, c(1:2, 5:7, 13:15)]
apply(actual.answers.vp.sub[, -c(1:2)], 2, error.measures, actual = rep(1, length = nrow(actual.answers.vp)), measure = "MAPE")
apply(actual.answers.vp.sub[, -c(1:2)], 2, error.measures, actual = rep(1, length = nrow(actual.answers.vp)), measure = "RMSE")
log.lik.per.person.and.condition <- actual.answers.vp.sub %>% 
  group_by(subj, cond) %>%
  summarise(
    Discrete = error.measures(actual = 1, forecast = pred.resp.discrete, measure = "MAPE"),
    Manhattan = error.measures(actual = 1, forecast = pred.resp.city, measure = "MAPE"),
    Euclidean = error.measures(actual = 1, forecast = pred.resp.eucl, measure = "MAPE"),
    Identity = error.measures(actual = 1, forecast = pred.identity.eucl, measure = "MAPE"),
    Unidim.Manhattan = error.measures(actual = 1, forecast = pred.best.unidim.eucl, measure = "MAPE"),
    Unidim.Discrete = error.measures(actual = 1, forecast = pred.best.unidim.disc, measure = "MAPE")
  )

log.lik.per.person.and.condition <- melt(log.lik.per.person.and.condition, id.vars = c("subj", "cond"), 
                                         measure.vars = c("Discrete", "Manhattan", "Euclidean", "Identity", "Unidim.Manhattan", "Unidim.Discrete"),
                                         value.name = "MAPE",
                                         variable.name = "Model")

ggplot(data = log.lik.per.person.and.condition, mapping = aes(x = Model, y = MAPE)) +
  geom_boxplot(mapping = aes(fill = Model)) +
  geom_jitter(color = "black", alpha = .5, width = .1) +
  theme_bw() + 
  facet_wrap(~factor(cond)) +
  xlab("Models") +
  ylab("MAPE") +
  labs(title = "MAPE Of Similarity Models",
       subtitle = "A Reanalysis of Wills (2015), Experiment 1") +
  labs(fill = "Model") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 12, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12)) +
  theme(legend.position = c(0.85, 0.3))
ggsave("Experiment 1/Error Measures Violinplot.pdf")

fit <- aov(MAPE ~ Model * factor(cond), data = log.lik.per.person.and.condition)
summary(fit)
TukeyHSD(fit)

# 8. Check whether predictions of my unidimensional and identity models are the same as in Wills
identical.to.wills <- rep(0, each = length(response.frequency.list))
for(i in 1:length(response.frequency.list)) {
  response.frequency.list[[i]]$max.arg <- apply(response.frequency.list[[i]][, 1:3], 1, which.max)
  for(j in 8:11) {
    if(all(stimuli1[, ..j] == response.frequency.list[[i]]$max.arg)) {
      identical.to.wills[i] <- names(stimuli1)[j]
    }
  } 
}
models.wills.comparison <- cbind(names(response.frequency.list), identical.to.wills)

# 9. Plot actual and predicted answers
actual.answers.vp <- actual.answers.vp[order(actual.answers.vp[, "cond"], actual.answers.vp[, "subj"]),]

actual.answers.vp <- as.data.table(actual.answers.vp)
actual.answers.vp <- merge(actual.answers.vp, stimuli1[, c(1:2, 5:7)], by = "triad")
actual.answers.vp[, actual.response := cbind(top1, left2, right3)[, response], by = 1:nrow(actual.answers.vp)]

actual.answers.vp[, min.chosen := as.numeric(names(which.min(table(actual.response)))), by = list(triplet, cond)]
actual.answers.vp[, max.chosen := as.numeric(names(which.max(table(actual.response)))), by = list(triplet, cond)]
actual.answers.vp[, observations.max := ifelse(actual.response == max.chosen, 1, 0)]
actual.answers.vp[, observations.min := ifelse(actual.response == min.chosen, -1, 0)]
answers.with.max.chosen <- apply(actual.answers.vp$max.chosen == actual.answers.vp[, 17:19], 1, which)
answers.with.min.chosen <- apply(actual.answers.vp$min.chosen == actual.answers.vp[, 17:19], 1, which)

actual.answers.vp[, x.order := paste(cond, triplet)]
levels <- actual.answers.vp[, .(n = sum(observations.max)), by = list(triplet, cond, x.order)][, x.order[order(n)], by = cond]$V1
actual.answers.vp[, x.order := factor(x.order, levels = levels)]

predictions.max.chosen <- matrix(0, nrow = nrow(actual.answers.vp), ncol = length(response.frequency.list))
predictions.min.chosen <- matrix(0, nrow = nrow(actual.answers.vp), ncol = length(response.frequency.list))
colnames(predictions.max.chosen) <- paste0(names(response.frequency.list), ".max.chosen")
colnames(predictions.min.chosen) <- paste0(names(response.frequency.list), ".min.chosen")

for(i in 1:length(response.frequency.list)) {
  current.list.item <- do.call("rbind", replicate(145, response.frequency.list[[i]], simplify = FALSE))
  for(j in 1:length(answers.with.max.chosen)) {
    predictions.max.chosen[j, i] <- current.list.item[j, answers.with.max.chosen[j]]
    predictions.min.chosen[j, i] <- current.list.item[j, answers.with.min.chosen[j]]
  }
}

actual.answers.vp <- cbind(actual.answers.vp, predictions.max.chosen, predictions.min.chosen)
unidim.eucl <- response.frequency.list[c(4,5)]

triad.rep <- rep(1:48, times = length(best.unidim.eucl))
pred.best.unidim.eucl.max.chosen <- sapply(1:length(answers.with.max.chosen), function(x) {
  unidim.eucl[[best.unidim.eucl[ceiling(x/48)]]][triad.rep[x], answers.with.max.chosen[x]]
})

pred.best.unidim.eucl.min.chosen <- sapply(1:length(answers.with.min.chosen), function(x) {
  unidim.eucl[[best.unidim.eucl[ceiling(x/48)]]][triad.rep[x], answers.with.min.chosen[x]]
})

actual.answers.vp[, pred.best.unidim.eucl.max.chosen := c(pred.best.unidim.eucl.max.chosen)]
actual.answers.vp[, pred.best.unidim.eucl.min.chosen := c(pred.best.unidim.eucl.min.chosen)]

predictions.long.max <- melt(actual.answers.vp, id.vars = c("cond", "triad", "triplet", "subj", "observations.max", "x.order"), 
                             measure.vars = c("pred.resp.discrete.max.chosen", "pred.resp.city.max.chosen", "pred.best.unidim.eucl.max.chosen", "pred.identity.eucl.max.chosen"),
                             value.name = "predictions",
                             variable.name = "model")
levels(predictions.long.max$model) <- c("Discrete", "City-Block", "Unidimensional", "Identity")

predictions.long.min <- melt(actual.answers.vp, id.vars = c("cond", "triad", "triplet", "subj", "observations.min", "x.order"), 
                             measure.vars = c("pred.resp.discrete.min.chosen", "pred.resp.city.min.chosen", "pred.best.unidim.eucl.min.chosen", "pred.identity.eucl.min.chosen"),
                             value.name = "predictions",
                             variable.name = "model")
levels(predictions.long.min$model) <- c("Discrete", "City-Block", "Unidimensional", "Identity")

names(actual.answers.vp)[names(actual.answers.vp) == "pred.resp.discrete"] <- "Discrete"
names(actual.answers.vp)[names(actual.answers.vp) == "pred.resp.city"] <- "City-Block"
names(actual.answers.vp)[names(actual.answers.vp) == "pred.best.unidim.eucl"] <- "Unidimensional"
names(actual.answers.vp)[names(actual.answers.vp) == "pred.identity.eucl"] <- "Identity"

sum.fun.upper <- function(x) mean(x) + sd(x)/sqrt(length(x))
sum.fun.lower <- function(x) mean(x) - sd(x)/sqrt(length(x))

ggplot(actual.answers.vp, aes(x = x.order)) +
  geom_bar(aes(y = observations.max), stat = "summary", fun = mean, position = "dodge") +
  geom_bar(aes(y = observations.min), stat = "summary", fun = mean, position = "dodge") +
  geom_hline(yintercept = 0) +
  geom_line(data = predictions.long.max, aes(y = predictions, color = model, group = model), size = 1, stat = "summary", position = "dodge") +
  geom_line(data = predictions.long.min, aes(y = -predictions, color = model, group = model), size = 1, stat = "summary", position = "dodge") +
  geom_line(data = predictions.long.max,
            aes(y = predictions, color = model, group = model),
            linetype = 2,
            stat = "summary",
            fun.y = sum.fun.upper, position = "dodge") +
  geom_line(data = predictions.long.max,
            aes(y = predictions, color = model, group = model),
            linetype = 2,
            stat = "summary",
            fun.y = sum.fun.lower, position = "dodge") +
  geom_line(data = predictions.long.min,
            aes(y = -predictions, color = model, group = model),
            linetype = 2,
            stat = "summary",
            fun.y = sum.fun.upper, position = "dodge") +
  geom_line(data = predictions.long.min,
            aes(y = -predictions, color = model, group = model),
            linetype = 2,
            stat = "summary",
            fun.y = sum.fun.lower, position = "dodge") +
  facet_grid(model~cond, scales = "free_x") +
  xlab("Stimuli") +
  ylab("Observations (bars) vs. Predictions (Lines)") +
  labs(color = "Model") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave("Observed-Predicted Wills.pdf")

# 10. Log likelihoods
log.lik <- actual.answers.vp[, lapply(.SD, function(z) sum(dbinom(x = 1, prob = z, size = 1, log = TRUE))), .SDcols = c(5, 6, 13, 14), by = list(subj, cond)]
model.logs <<- log.lik[, names(.SD)[which.max(.SD)], by = list(cond, subj)][, .N, by = list(cond, V1)]
ggplot(model.logs, aes(x = V1, y = N, fill = V1)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~cond) +
  labs(fill = "Model") +
  xlab("Model") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
ggsave("Model Fit Wills.png")

# Formel für logs dann nlminb; multinomial distribution dmultinom (library(LaplacesDemon))

# City-Block: Tversky Parameter verändern --> wie verändern sich Similarities in pairwise comparisons? (2. Plot)
# Log likelihood für jedes Modell; χ2 Test für absoluten Fit (Response Probabilites over all participants for each triad vs. predictions of model for each triad)

# implementiere ihre Methode mit arg.max --> schneidet unser Modell besser ab?