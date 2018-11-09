########################################################################################################################
#################################### Power Analysis of Models: Similarity Experiment ###################################
# (based on relative likelihood of models, see Wagenmakers & Farrell (2004): AIC model selection using Akaike weights) #
########################################################################################################################

source("Models/GCM_Prot.R")
#source("VariableDefinition.R")
source("../Experiment_Python/Similarity Experiment/make.stimuli.R")
library(data.table)
library(truncdist)
library(truncnorm)
library(ggplot2)

# Read in similarity matrices for GCM.Eucl and GCM.Attr
predictions <- fread("Data/Similarity Matrices/predictions.R")
predictions[, par.id := paste(w1, w2, w3, c)]
colnames(predictions)[8] <- "true.model"

# Create stimulis and omit identical stimuli
stimuli.left <- rbind(lower.stimuli.1[1:(nrow(lower.stimuli.1)-4),], upper.stimuli.1[1:(nrow(lower.stimuli.1)-4),])
stimuli.right <- rbind(lower.stimuli.2[1:(nrow(lower.stimuli.1)-4),], upper.stimuli.2[1:(nrow(lower.stimuli.1)-4),])

# Repeat stimulis 4 times (randomization left-right here not taken into account)
nrep <- 4
stimuli.left <- do.call("rbind", replicate(nrep, stimuli.left, simplify = FALSE))
stimuli.right <- do.call("rbind", replicate(nrep, stimuli.right, simplify = FALSE))

# Number of dimensions
n.dim <- sum(grepl("^Dim", colnames(stimuli.left)))

# Percentage using discrete metric 
tp_using_discrete_metric <- 0.7
ntp_using_discrete_metric <- 0.3

# Possible Total Sample Size (N)
Ns <- seq(10, 100, 10)

# Indicate possible metrics
models <- c("GCM.Attr", "GCM.Eucl")

# r and p parameter (fixed at 1)
r = p <- 1

# Parameter space
parameter.space <- make.grid(n.dim = n.dim, steps = 20, c = seq(0.1, 4.1, 0.5), add.noise = FALSE)

power_sim <- function() {
  # Build result template
  result.cols <- c("log.lik", "N", "part.id", "fitting.model", "time.pressure")
  result <- setNames(data.table(matrix(nrow = 0, ncol = length(result.cols))), result.cols)
  
  # Simulate over different N's 
  for(N in Ns) {
    print(paste("N:", N))
    
    mixing_p_ntp <- rtrunc(N, spec = "norm", a = 0, b = 1, mean = ntp_using_discrete_metric, sd = 0.3)
    mixing_p_tp <- rtrunc(N, spec = "norm", a = 0, b = 1, mean = tp_using_discrete_metric, sd = 0.3)
    
    # Participant data.table
    data <- data.table(ID = rep(1:N, 2),
                       time.pressure = rep(c(TRUE, FALSE), each = N),
                       mixing_p = c(mixing_p_tp, mixing_p_ntp))
    
    # Calculate Loglikelihood between responses and predictions
    for(current.id in 1:N) {
      print(paste("current.id:", current.id))
      mixing_p <- data[ID == current.id, mixing_p]
      
      sample <- predictions[, .(par.id = sample(par.id, 1)), by = true.model]
      sample <- predictions[sample, on = c("true.model", "par.id")]
      sample <- sample[, .(GCM.Attr = sample[true.model == "Attr", similarity], 
                           GCM.Eucl = sample[true.model == "Eucl", similarity],
                           comb_pred = sample[true.model == "Attr", similarity]*mixing_p + sample[true.model == "Eucl", similarity]*(1-mixing_p))]
      sample[, simulated.data := rtruncnorm(n = 1, a = 0, b = 10, mean = comb_pred * 10, sd = 0.5)]
      
      for(fitting.model in models) {
        print(paste("fitting.model:", fitting.model))
        out <- sample[, .(log.lik = sum(dnorm(x = simulated.data, mean = get(fitting.model), sd = 0.5, log = T)))]
        out[, N := N]
        out[, part.id := current.id]
        out[, fitting.model := fitting.model]
        out[, time.pressure := data[ID == current.id, time.pressure]]
        
        result <- rbind(result, out) 
      }
    }
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

power_sig_sim <- replicate(1000, power_sim(), simplify = FALSE)
power_sig_sim <- rbindlist(power_sig_sim)
power_sig_sim[, mean(sig), by = N]

# Plot w_akaike by N and fitting.model
ggplot(power_sig_sim, aes(x = N, y = as.numeric(sig))) +
  stat_summary(geom = "point", fun.y = mean) +
  stat_summary(geom = "line", fun.y = mean)  +
  geom_hline(aes(yintercept = 0.8), color = "red", size = 1) +
  theme_bw() +
  ylab("Power") +
  scale_x_continuous(breaks = seq(20, 200, 20)) +
  scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  labs(title = "Power in dependence of sample size N")

ggsave("Images/Power_Similarity.jpg")

fwrite(power_sig_sim, file = "Data/power_sim_sig_similarity_exp.csv")
