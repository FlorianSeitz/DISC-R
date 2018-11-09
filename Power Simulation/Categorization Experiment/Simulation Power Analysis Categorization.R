########################################################################################################################
################################## Power Analysis of Models: Categorization Experiment #################################
# (based on relative likelihood of models, see Wagenmakers & Farrell (2004): AIC model selection using Akaike weights) #
########################################################################################################################

source("Models/GCM_Prot.R")
source("VariableDefinition.R")
library(data.table)
library(truncdist)
library(truncnorm)
library(ggplot2)

# Define learningset
stimuli.space <- do.call(make.learningset, list(a.design.ID = 121, n.Cat1.IDs, n.dim, n.val, n.stim, n.transfer, n.learning.stim, n.Cat1, all.stim))
learningset <- stimuli.space$learningset
learningset <- do.call("rbind", replicate(20, learningset, simplify = FALSE))
# it.freq <- iterative.frequencies(x = learningset)

nrep.test <- 8
test.ids <- c("003", "100", "231", "311", "321", "211", "221", "331")
testset <- stimuli.space$testset[stimuli.space$testset$ID %in% test.ids, ]
testset <- do.call("rbind", replicate(nrep.test, testset, simplify = FALSE))
n.dim <- sum(grepl("^Dim", colnames(testset)))

# Percentage using discrete metric 
tp_using_discrete_metric <- 0.7
ntp_using_discrete_metric <- 0.3

# Possible Total Sample Size (N)
Ns <- seq(20, 200, 10)

# Indicate possible metrics
models <- c("GCM.Attr", "GCM.Eucl")

# r and p parameter (fixed at 1)
r = p <- 1

# Load prediction data
pred <- fread("Data/ModelRecovery_best_design_sim_taus.csv", select = c("true.model", "fitting.model", "true.w1", "true.w2", "true.c", "stimulus.id", "pred.prob.cat1", "stimulus.cat", "tau"), colClasses = c("stimulus.id" = "character"))
pred <- pred[is.na(stimulus.cat), ] # Take only testset rows

# Load parameters achieving accuracy
parameter.space <- fread("Data/result.achieving.accuracy.tau.csv", select = c("true.w1", "true.w2", "true.c", "tau", "loglik", "true.model", "fitting.model"))

pred <- pred[parameter.space, on = c("true.model", "fitting.model", "true.w1", "true.w2", "true.c", "tau")] # take only rows where parameters fulfill accuracy criterion
pred <- pred[stimulus.id %in% test.ids] # only rows, where test.ids appear
pred[, par.id := paste(true.w1, true.w2, true.c, tau)] # create par.id
pred <- dcast(pred, par.id + true.model + stimulus.id ~ fitting.model, value.var = "pred.prob.cat1")
pred <- do.call("rbind", replicate(n = nrep.test, expr = pred, simplify = FALSE))

parameter.space[, true.w3 := 1 - true.w1 - true.w2]

# Save tau as separate vector
taus <- parameter.space$tau

# Order columns and keep only ws and c
parameter.space <- parameter.space[, c(1, 2, 8, 3)]
parameter.space <- as.matrix(parameter.space)
colnames(parameter.space) <- c("w1", "w2", "w3", "c")

power_sim <- function() {
  # Build result template
  result.cols <- c("log.lik", "N", "part.id", "fitting.model", "time.pressure")
  result <- setNames(data.table(matrix(nrow = 0, ncol = length(result.cols))), result.cols)
  
  # Simulate over different N's, parameters, taus 
  for(N in Ns) {
    
    # n per condition
    n <- N/2
    
    # # Time Pressure --> Number of participants choosing discrete metric
    # n_discrete_tp <- n*tp_using_discrete_metric
    # 
    # # No Time Pressure --> Number of participants choosing discrete metric
    # n_discrete_ntp <- n*ntp_using_discrete_metric
    
    mixing_p_ntp <- rtrunc(n, spec = "norm", a = 0, b = 1, mean = ntp_using_discrete_metric, sd = 0.3)
    mixing_p_tp <- rtrunc(n, spec = "norm", a = 0, b = 1, mean = tp_using_discrete_metric, sd = 0.3)
    
    # Participant data.table
    data <- data.table(ID = 1:N,
                       time.pressure = rep(c(TRUE, FALSE), each = n),
                       mixing_p = c(mixing_p_tp, mixing_p_ntp))
    
    # for(par_row in 1:nrow(parameter.space)) {
    #   
    #   # Draw random parameter
    #   parameters <- parameter.space[par_row, ]
    #   ws <- head(parameters, -1)
    #   c <- parameters[length(parameters)]
    #   tau <- taus[par_row]
    #   
    #   # Simulate data for all participants (dependent on the metric they used)
    #   sim.data <- apply(data, 1, function(x) {
    #     predictions <- v.Predict(Probe = testset, Learningset = learningset, Model = "GCM", Metric = x["metric.used"], w = ws, c = c, p = p, r = r)
    #     predictions <- soft.max(predictions, tau = tau)
    #     
    #     # simulations <- Simulate(parameters, Learningset = learningset, Testset = learningset, Model = "GCM", Metric = x["metric.used"], p = p, r = r, N = it.freq, tau = tau)$learningset
    #     resp <- rbinom(n = length(predictions), size = 1, prob = predictions) # arg max choice rule (= converges probability distribution to discrete choice)
    #     simulations <- data.table(Dim1 = testset$Dim1, Dim2 = testset$Dim2, Dim3 = testset$Dim3, Stimulus = testset$ID, Cat = resp, predictions = predictions, id = as.integer(x["ID"]))
    #     
    #     return(simulations)
    #   })
    #   
    #   sim.datas <- do.call("rbind", sim.data)
    
    # Calculate Loglikelihood between responses and predictions
    for(current.id in 1:N) {
      
      # # Sample from t-distribution
      # p_ntp <- p_tp * ntp_using_discrete_metric / tp_using_discrete_metric
      
      # bei gpower unter binomialtests nachschauen; 
      # probabilities die rauskommen: werden mit pred.prob.cat.1 von diskreten Metrik multipliziert --> prob*pred.prob.Cat.1(DM) + (1-prob)*pred.prob.cat.1(MM) 
      
      # current.data <- sim.datas[id == current.id, ]
      mixing_p <- data[ID == current.id, mixing_p]
      
      sample <- pred[, .(par.id = sample(par.id, 1)), by = true.model]
      sample <- pred[sample, on = c("true.model", "par.id")]
      sample <- sample[, .(GCM.Attr = GCM.Attr[true.model == "GCM.Attr"], 
                           GCM.Eucl = GCM.Eucl[true.model == "GCM.Eucl"],
                           comb_pred = GCM.Attr[true.model == "GCM.Attr"]*mixing_p + GCM.Eucl[true.model == "GCM.Eucl"]*(1-mixing_p))]
      sample[, simulated.data := rbinom(n = .N, size = 1, prob = comb_pred)]
      
      for(fitting.model in models) {
        
        # temp.res <- Max.likelihood.optimization.solnp(n.dim = n.dim,
        #                                               Observedset = as.data.frame(current.data),
        #                                               Learningset = learningset,
        #                                               Model = "GCM",
        #                                               Metric = metric,
        #                                               ignore = length(unique(testset$ID)))
        
        out <- sample[, .(log.lik = log.likelihood(observations = simulated.data, predictions = get(fitting.model), ignore = 0))]
        out[, N := N]
        out[, part.id := current.id]
        out[, fitting.model := fitting.model]
        out[, time.pressure := data[ID == current.id, time.pressure]]
        
        # temp.result <- list(N, par_row, current.id, current.metric.used, metric, -1*temp.res$Log.likelihood)
        # temp.res <- log.likelihood(observations = current.data$response, predictions = current.data$predictions, ignore = length(unique(testset$ID)))
        result <- rbind(result, out)
        
      }
      
    }
    
    # } 
    
  }
  
  result <- as.data.table(result)
  
  # Relative Likelihood of Models: Relative Likelihood of Attr vs. Eucl (AIC = loglik, as parameter fitting is given)
  result[, delta := exp(log.lik - max(log.lik)), by = list(N, part.id, time.pressure)]
  
  # Calculate Akaike weights by normalizing relative likelihoods of models
  result[, w_akaike := delta/sum(delta), by = list(N, part.id, time.pressure)]
  
  # Calculate evidence
  result[, evidence := cut(w_akaike, c(0, .05, .95, 1), labels = c("rejected", "inconclusive", "accepted"), include.lowest = TRUE)]
  evidence <- result[, list(evidence = unique(evidence)), by = list(N, part.id, fitting.model, time.pressure)]
  evidence[, as.list(prop.table(table(evidence))), by = list(N, time.pressure, fitting.model)]
  
  # Throw out all rows where evidence is inconclusive: alle inconclusive participants rausschmeissen
  result <- result[!evidence == "inconclusive"]
  
  # Binomialtest between proportion of models by timepressure and N
  # z.B. 5 von 10 TP-Probanden mit Attr, 2 von 10 nTP-Probanden mit Attr --> Vergleich ob das signifikant ist (two proportion z-test)
  prop.table <- result[, .(x = sum(evidence == "accepted" & fitting.model == "GCM.Attr"), n = sum(fitting.model == "GCM.Attr")), by = list(time.pressure, N)]
  return(prop.table[, .(sig = prop.test(x = x, n = n, alternative = "greater")$p.value < 0.05), by = N])
}

power_sim_sig <- replicate(1000, power_sim(), simplify = FALSE)
power_sim_sig <- rbindlist(power_sim_sig)
power_sim_sig[, mean(sig), by = N]

# Plot w_akaike by N and fitting.model
ggplot(power_sim_sig, aes(x = N, y = as.numeric(sig))) +
  stat_summary(geom = "point", fun.y = mean) +
  stat_summary(geom = "line", fun.y = mean)  +
  theme_bw() +
  ylab("Power") +
  scale_x_continuous(breaks = seq(20, 200, 20)) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  labs(title = "Power in dependence of sample size N")

ggsave("Images/Power_Categorization.pdf")
fwrite(power_sim_sig, file = "Data/power_sim_sig.csv")

# tp <- c(.62, .33, .47, .28, .40, .60, .59, .68)
# ntp <- c(.71, .83, .77, .70, .16, .78, .36, .37)
# diff <- abs(tp - ntp)
# library(lsr)
# cohensD(diff)
