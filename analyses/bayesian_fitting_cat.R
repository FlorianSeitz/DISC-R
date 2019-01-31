# Bayesian Fitting of GCM

### Model with full individual differences 
library(R2jags)
library(data.table)

#clear list
rm(list=ls(all=TRUE)) 

#set working directory to a folder of your harddrive that you have full access to (reading and writing)
#setwd("~/Work/Teaching/17/Basel/Exercises/Memory retention")

#Prepare the data
files <- list.files(path = "../Experiment_Python/Categorization Experiment/data/pretest/csv/", pattern = "6", full.names = TRUE) # *.csv$
dat <- rbindlist(lapply(files, fread, fill = TRUE))
learn_dat <- dat[block == "training", ]

stim <- learn_dat[, stim]
stim <- matrix(as.numeric(unlist(strsplit(as.character(stim), split = ""))), ncol = 3, byrow = T)

true_cat <- learn_dat[, true_cat]
response <- learn_dat[, response]

nt <- length(response) # number of learning trials
ns <- dat[, length(unique(subj_id))] # number of subjects

data <- list("stim", "true_cat", "response", "ns", "nt") # Variables passed on to JAGS

nChains <- 2

# Specify initial values
myinits <- list(list(w1 = rep(0.33,ns), w2 = rep(0.33,ns), w3 = rep(0.34,ns), c = rep(2, ns)))[rep(1,nChains)]

# Indicate which parameters are to be monitored in JAGS
parameters <- c("w1", "w2", "w3", "c")

# Call JAGS with specific options
samples <- jags(data = data, inits=myinits, parameters.to.save = parameters,
                model.file ="../Analyses/bayesian_fitting_cat.txt",
                n.chains=nChains, n.iter=10000, n.burnin=1, n.thin=1, DIC=T)


# Now the values for the monitored parameters are in the "samples" object, 
# Ready for inspection.
samples$BUGSoutput$summary

#Show DIC 
samples$BUGSoutput$DIC


########## PLOTTING SECTION #########################################################################################
### Plotposterior distributions for the three individuals
#Priors
par(mfrow = c(2,3))
x <- seq(0,1,by=.1)
#Posteriors
plot(density(samples$BUGSoutput$sims.list$alpha[,1]), xlab = expression (alpha), cex.lab = 1.5,
     main = "Posterior for alpha, Subject 1", bty = 'n',lwd=2, ylab="Posterior density", xlim = c(0,1))
plot(density(samples$BUGSoutput$sims.list$alpha[,2]), xlab = expression (alpha), cex.lab = 1.5,
     main = "Posterior for alpha, Subject 2", bty = 'n',lwd=2, ylab="Posterior density", xlim = c(0,1))
plot(density(samples$BUGSoutput$sims.list$alpha[,3]), xlab = expression (alpha), cex.lab = 1.5,
     main = "Posterior for alpha, Subject 3", bty = 'n',lwd=2, ylab="Posterior density", xlim = c(0,1))
#[Add code to show posteriors for the beta parameter]




#Plot predicted values
par(mfrow = c(1,3))
plot(k[1,],type="b",ylim=c(0,18),pch=19,ylab="",xlab="")
par(new=T)
plot(samples$BUGSoutput$mean$predk[1,],type="b",ylim=c(0,18), pch=24,ylab="Number of items correctly recalled",xlab="Time interval")
legend(7,16, c("Data","Prediction"),pch=c(19,24))
par(new=F)
plot(k[2,],type="b",ylim=c(0,18),pch=19,ylab="",xlab="")
par(new=T)
plot(samples$BUGSoutput$mean$predk[2,],type="b",ylim=c(0,18), pch=24, ylab="Number of items correctly recalled",xlab="Time interval")
legend(7,16, c("Data","Prediction"),pch=c(19,24))
par(new=F)
plot(k[3,],type="b",ylim=c(0,18),pch=19,ylab="",xlab="")
par(new=T)
plot(samples$BUGSoutput$mean$predk[3,],type="b",ylim=c(0,18),pch=24, ylab="Number of items correctly recalled",xlab="Time interval")
legend(7,16, c("Data","Prediction"),pch=c(19,24))


par(new=F)

