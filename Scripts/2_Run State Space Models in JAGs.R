library(ggplot2)
library(jagsUI)
library(dplyr)
library(tidyr)
library(doParallel)

#### Step 1: Read in and prep capture data ####
#setwd("~/Dropbox/_Research/_PhD/07_UpperWarren_WA/data/")
load("Data_Clean/UW_uniquecapdata_transect_inputforSSM_16072021.RData") # Period data takes like 4 days to run a model (dont converge though)
transect.capture.data$effort[is.na(transect.capture.data$effort)] <- 0
trap.data = transect.capture.data$woylie

dat<- trap.data
#dat <- dat[dat$TRANSECT == "Balban",] # Subset to a transect if we want

C<- as.matrix(dat[,2:ncol(dat)]) # get counts 2000-2019
session <- 1:ncol(C) # Write the sessions out 

## 
M <- nrow(C) # Sites
T <- ncol(C) # Sessions

#### Step 2: SPECIFY MODELS ####
cat(file = "Scripts/modelA.txt","
model {
  # Priors
  lambda.p1 = 0
  lambda.p2 = 100

  mean.gamma.p1 = 0.9
  mean.gamma.p2 = 1.1
  sd.alpha.gam.0 = 0
  sd.alpha.gam.p = 100
  
  mean.p1 = 0
  mean.p2 = 1
  
  sd.rho.0 = 0
  sd.rho.p = 0.5
  
  # Expected initial abundance
  lambda ~ dunif(lambda.p1, lambda.p2)    
  
  # Model for ’immigration-free’ population growth rate
  for(i in 1:M){
      alpha.gam[i] ~ dnorm(mu.alpha.gam, tau.alpha.gam)
    }
  
  mu.alpha.gam <- log(mean.gamma)
  mean.gamma ~ dunif(mean.gamma.p1, mean.gamma.p2) # Mean of gamma
  tau.alpha.gam <- pow(sd.alpha.gam, -2)
  sd.alpha.gam ~ dnorm(sd.alpha.gam.0, sd.alpha.gam.p) I(0.001,)   # Site/ year-level OD
  
  p ~ dunif(mean.p1, mean.p2)            # Detection probability
  
  for(t in 1:(T-1)){         # Model for random immigration
    log(rho[t]) <- logrho[t]
    logrho[t] ~ dnorm(0, tau.rho)
  }
  tau.rho <- pow(sd.rho, -2)
  sd.rho ~ dnorm(sd.rho.0, sd.rho.p)I(0.001,)  # Half-normal prior for sd

  # Likelihood
  # State process
  for(i in 1:M){
    # Initial conditions
    N[i,1] ~ dpois(lambda)
    log(gamma[i]) <- loggam[i]
    loggam[i] <- alpha.gam[i]
    # Transition model
    for(t in 2:T){
      N[i,t] ~ dpois(N[i,t-1] * gamma[i] + rho[t-1])
    }
    # Observation process
    for(t in 1:T){
      C[i,t] ~ dbin(p, N[i,t])
    }
  }
}
")


cat(file = "Scripts/modelB.txt","
model {
  # Priors
  mean.lambda.0 = 0
  mean.lambda.p = 50
  sd.alpha.lam.0 = 0
  sd.alpha.lam.p = 2
  
  mean.gamma.p1 = 0.9
  mean.gamma.p2 = 1.1
  sd.alpha.gam.0 = 0
  sd.alpha.gam.p = 100
  
  mean.p1 = 0
  mean.p2 = 1
  
  sd.rho.0 = 0
  sd.rho.p = 0.5
  
  # Model for expected initial abundance
  for(i in 1:M){
    alpha.lam[i] ~ dnorm(mu.alpha.lam, tau.alpha.lam)
  }
  mu.alpha.lam <- log(mean.lambda)
  mean.lambda ~ dunif(mean.lambda.0, mean.lambda.p) # Mean of lambda
  tau.alpha.lam <- pow(sd.alpha.lam, -2)
  sd.alpha.lam ~ dnorm(sd.alpha.lam.0, sd.alpha.lam.p) I(0.001,)    # Site-level OD
  
  # Model for ’immigration-free’ population growth rate
  for(i in 1:M){
      alpha.gam[i] ~ dnorm(mu.alpha.gam, tau.alpha.gam)
    }
  
  mu.alpha.gam <- log(mean.gamma)
  mean.gamma ~ dunif(mean.gamma.p1, mean.gamma.p2) # Mean of gamma
  tau.alpha.gam <- pow(sd.alpha.gam, -2)
  sd.alpha.gam ~ dnorm(sd.alpha.gam.0, sd.alpha.gam.p) I(0.001,)   # Site/ year-level OD
  
   p ~ dunif(mean.p1, mean.p2)            # Detection probability
  
  for(t in 1:(T-1)){         # Model for random immigration
    log(rho[t]) <- logrho[t]
    logrho[t] ~ dnorm(0, tau.rho)
  }
  tau.rho <- pow(sd.rho, -2)
  sd.rho ~ dnorm(sd.rho.0, sd.rho.p)I(0.001,)  # Half-normal prior for sd

  # Likelihood
  # State process
  for(i in 1:M){
    # Initial conditions
    N[i,1] ~ dpois(lambda[i])
    log(lambda[i]) <- loglam[i]
    loglam[i] <- alpha.lam[i]
    
    log(gamma[i]) <- loggam[i]
    loggam[i] <- alpha.gam[i]
    # Transition model
    for(t in 2:T){
      N[i,t] ~ dpois(N[i,t-1] * gamma[i] + rho[t-1])

    }
    # Observation process
    for(t in 1:T){
      C[i,t] ~ dbin(p, N[i,t])
    }
  }
}
")

cat(file = "Scripts/modelC.txt"," 
model {
  # Priors
  # Model for expected initial abundance
  for(i in 1:M){
    alpha.lam[i] ~ dnorm(mu.alpha.lam, tau.alpha.lam)
  }
  mu.alpha.lam <- log(mean.lambda)
  mean.lambda ~ dunif(0, 50) # Mean of lambda
  tau.alpha.lam <- pow(sd.alpha.lam, -2)
  sd.alpha.lam ~ dnorm(0, 2) I(0.001,)    # Site-level OD
  
  # Model for ’immigration-free’ population growth rate
  for(i in 1:M){
      alpha.gam[i] ~ dnorm(mu.alpha.gam, tau.alpha.gam)
    }
  
  mu.alpha.gam <- log(mean.gamma)
  mean.gamma ~ dunif(0.9, 1.1) # Mean of gamma
  tau.alpha.gam <- pow(sd.alpha.gam, -2)
  sd.alpha.gam ~ dnorm(0, 100) I(0.001,)   # Site/ year-level OD
  
  ## Detectability model
  alpha.p ~ dnorm(mu.alpha.p, tau.alpha.p)
  mu.alpha.p <- logit(mean.p)
  mean.p ~ dunif(0, 1) # Mean of p
  tau.alpha.p <- pow(sd.alpha.p, -2)
  sd.alpha.p ~ dnorm(0, 10) I(0.001,)      # Site/ year-level OD
  for (v in 1:1){
    beta.p[v] ~ dnorm(0, 0.001) # Covariate coefficient prior
  }
  
  
  for(t in 1:(T-1)){         # Model for random immigration
    log(rho[t]) <- logrho[t]
    logrho[t] ~ dnorm(0, tau.rho)
  }
  tau.rho <- pow(sd.rho, -2)
  sd.rho ~ dnorm(0, 0.5)I(0.001,)  # Half-normal prior for sd

  # Likelihood
  # State process
  for(i in 1:M){
    # Initial conditions
    N[i,1] ~ dpois(lambda[i])
    log(lambda[i]) <- loglam[i]
    loglam[i] <- alpha.lam[i]
    
    log(gamma[i]) <- loggam[i]
    loggam[i] <- alpha.gam[i]
    # Transition model
    for(t in 2:T){
      N[i,t] ~ dpois(N[i,t-1] * gamma[i] + rho[t-1])

    }
    # Observation process
    for(t in 1:T){
      C[i,t] ~ dbin(p[i,t], N[i,t])
      logit(p[i,t]) <- lp[i,t]
      lp[i,t] <- alpha.p + beta.p[1]*Effort[i,t]
    }
  }
}
")

cat(file = "Scripts/modelD.txt","
model {
  # Priors
  # Model for expected initial abundance
  for(i in 1:M){
    alpha.lam[i] ~ dnorm(mu.alpha.lam, tau.alpha.lam)
  }
  mu.alpha.lam <- log(mean.lambda)
  mean.lambda ~ dunif(0, 50) # Mean of lambda
  tau.alpha.lam <- pow(sd.alpha.lam, -2)
  sd.alpha.lam ~ dnorm(0, 2) I(0.001,)    # Site-level OD
  
  # Model for ’immigration-free’ population growth rate
  for(i in 1:M){
    for(t in 1:(T-1)){
      alpha.gam[i,t] ~ dnorm(mu.alpha.gam, tau.alpha.gam)
    }
  }
  
  mu.alpha.gam <- log(mean.gamma)
  mean.gamma ~ dunif(0.9, 1.1) # Mean of gamma
  tau.alpha.gam <- pow(sd.alpha.gam, -2)
  sd.alpha.gam ~ dnorm(0, 100) I(0.001,)   # Site/ year-level OD
  
  ## Detectability model
  alpha.p ~ dnorm(mu.alpha.p, tau.alpha.p)
  mu.alpha.p <- logit(mean.p)
  mean.p ~ dunif(0, 1) # Mean of p
  tau.alpha.p <- pow(sd.alpha.p, -2)
  sd.alpha.p ~ dnorm(0, 10) I(0.001,)      # Site/ year-level OD
  for (v in 1:1){
    beta.p[v] ~ dnorm(0, 0.001) # Covariate coefficient prior
  }
  
  
  for(t in 1:(T-1)){         # Model for random immigration
    log(rho[t]) <- logrho[t]
    logrho[t] ~ dnorm(0, tau.rho)
  }
  tau.rho <- pow(sd.rho, -2)
  sd.rho ~ dnorm(0, 0.5)I(0.001,)  # Half-normal prior for sd

  # Likelihood
  # State process
  for(i in 1:M){
    # Initial conditions
    N[i,1] ~ dpois(lambda[i])
    log(lambda[i]) <- loglam[i]
    loglam[i] <- alpha.lam[i]
    
    # Transition model
    for(t in 2:T){
      N[i,t] ~ dpois(N[i,t-1] * gamma[i,t-1] + rho[t-1])
      log(gamma[i,t-1]) <- loggam[i,t-1]
      loggam[i,t-1] <- alpha.gam[i,t-1]
    }
    # Observation process
    for(t in 1:T){
      C[i,t] ~ dbin(p[i,t], N[i,t])
      logit(p[i,t]) <- lp[i,t]
      lp[i,t] <- alpha.p + beta.p[1]*Effort[i,t] 
    }
  }
}
")

cat(file = "Scripts/modelE.txt","
model {
  # Priors
  # Model for expected initial abundance
  for(i in 1:M){
    alpha.lam[i] ~ dnorm(mu.alpha.lam, tau.alpha.lam)
  }
  mu.alpha.lam <- log(mean.lambda)
  mean.lambda ~ dunif(0, 50) # Mean of lambda
  tau.alpha.lam <- pow(sd.alpha.lam, -2)
  sd.alpha.lam ~ dnorm(0, 2) I(0.001,)    # Site-level OD
  
  # Model for ’immigration-free’ population growth rate
  for(i in 1:M){
    for(t in 1:(T-1)){
      alpha.gam[i,t] ~ dnorm(mu.alpha.gam, tau.alpha.gam)
    }
  }
  mu.alpha.gam <- log(mean.gamma)
  mean.gamma ~ dunif(0.9, 1.1) # Mean of gamma
  tau.alpha.gam <- pow(sd.alpha.gam, -2)
  sd.alpha.gam ~ dnorm(0, 100) I(0.001,)   # Site/ year-level OD
  
  ## Detectability model
  alpha.p ~ dnorm(mu.alpha.p, tau.alpha.p)
  mu.alpha.p <- logit(mean.p)
  mean.p ~ dunif(0, 1) # Mean of p
  tau.alpha.p <- pow(sd.alpha.p, -2)
  sd.alpha.p ~ dnorm(0, 10) I(0.001,)      # Site/ year-level OD
  for (v in 1:2){
    beta.p[v] ~ dnorm(0, 0.001) # Covariate coefficient prior
  }
  
  # Model for random immigration
  for(t in 1:(T-1)){
    log(rho[t]) <- logrho[t]
    logrho[t] ~ dnorm(0, tau.rho)
  }
  tau.rho <- pow(sd.rho, -2)
  sd.rho ~ dnorm(0, 0.5)I(0.001,)  # Half-normal prior for sd

  # Likelihood
  # State process
  for(i in 1:M){
    # Initial conditions
    N[i,1] ~ dpois(lambda[i])
    log(lambda[i]) <- loglam[i]
    loglam[i] <- alpha.lam[i]
    
    
    # Transition model
    for(t in 2:T){
      N[i,t] ~ dpois(N[i,t-1] * gamma[i,t-1] + rho[t-1])
      log(gamma[i,t-1]) <- loggam[i,t-1]
      loggam[i,t-1] <- alpha.gam[i,t-1]
    }
    
    # Observation process
    for(t in 1:T){
      C[i,t] ~ dbin(p[i,t], N[i,t])
      logit(p[i,t]) <- lp[i,t]
      lp[i,t] <- alpha.p + beta.p[1]*Effort[i,t] + beta.p[2]*Woylie[i,t]
    }
  }
}
")

cat(file = "Scripts/modelF.txt","
model {
  # Priors
  # Model for expected initial abundance
  for(i in 1:M){
    alpha.lam[i] ~ dnorm(mu.alpha.lam, tau.alpha.lam)
  }
  mu.alpha.lam <- log(mean.lambda)
  mean.lambda ~ dunif(0, 50) # Mean of lambda
  tau.alpha.lam <- pow(sd.alpha.lam, -2)
  sd.alpha.lam ~ dnorm(0, 2) I(0.001,)    # Site-level OD
  
  # Model for ’immigration-free’ population growth rate
  for(i in 1:M){
    for(t in 1:(T-1)){
      alpha.gam[i,t] ~ dnorm(mu.alpha.gam, tau.alpha.gam)
    }
  }
  mu.alpha.gam <- log(mean.gamma)
  mean.gamma ~ dunif(0.9, 1.1) # Mean of gamma
  tau.alpha.gam <- pow(sd.alpha.gam, -2)
  sd.alpha.gam ~ dnorm(0, 100) I(0.001,)   # Site/ year-level OD
  
  ## Detectability model
  alpha.p ~ dnorm(mu.alpha.p, tau.alpha.p)
  mu.alpha.p <- logit(mean.p)
  mean.p ~ dunif(0, 1) # Mean of p
  tau.alpha.p <- pow(sd.alpha.p, -2)
  sd.alpha.p ~ dnorm(0, 10) I(0.001,)      # Site/ year-level OD
  for (v in 1:1){
    beta.p[v] ~ dnorm(0, 0.001) # Covariate coefficient prior
  }
  
  # Model for random immigration
  for(i in 1:M){
    for(t in 1:(T-1)){
    log(rho[i,t]) <- logrho[i,t]
    logrho[i,t] ~ dnorm(0, tau.rho)
    }
  }
  tau.rho <- pow(sd.rho, -2)
  sd.rho ~ dnorm(0, 0.5)I(0.001,)  # Half-normal prior for sd

  # Likelihood
  # State process
  for(i in 1:M){
    # Initial conditions
    N[i,1] ~ dpois(lambda[i])
    log(lambda[i]) <- loglam[i]
    loglam[i] <- alpha.lam[i]
    
    
    # Transition model
    for(t in 2:T){
      N[i,t] ~ dpois(N[i,t-1] * gamma[i,t-1] + rho[i,t-1])
      log(gamma[i,t-1]) <- loggam[i,t-1]
      loggam[i,t-1] <- alpha.gam[i,t-1]
    }
    
    # Observation process
    for(t in 1:T){
      C[i,t] ~ dbin(p[i,t], N[i,t])
      logit(p[i,t]) <- lp[i,t]
      lp[i,t] <- alpha.p + beta.p[1]*Effort[i,t]
    }
  }
}
")

#### Step 3: FIT MODELS ####
# Prepare data
Woylie.cap = ifelse(is.na(as.matrix(transect.capture.data$woylie[,2:39])), 0, as.matrix(transect.capture.data$woylie[,2:39]))

# Parameters monitored
params <- c("lambda", "gamma", "p", "sd.rho", "rho","N", "beta.p")

# MCMC settings
na <- 5000  ;  ni <- 1000000  ;  nt <- 500   ;  nb <- 500000  ;  nc <- 3  

woylie.models = data.frame(Species = "woylie", 
                           Model = c("modelA.txt", "modelB.txt", "modelC.txt", "modelD.txt", "modelF.txt"))
koomal.models = data.frame(Species = "koomal", 
                           Model = c("modelA.txt", "modelB.txt", "modelC.txt", "modelD.txt", "modelE.txt","modelF.txt"))
chuditch.models = data.frame(Species = "chuditch", 
                             Model = c("modelA.txt", "modelB.txt"))
quenda.models = data.frame(Species = "quenda", 
                           Model = c("modelA.txt", "modelB.txt"))

model.files = rbind(woylie.models, koomal.models, chuditch.models, quenda.models)

parallel::detectCores()
cl <- makeCluster(6, setup_timeout = 0.5) # Need three cores per model, so should be multiple of 3
registerDoParallel(cl)
#Implement fix for parallel not working on Mac properly
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")


model.data.out = foreach(l=1:length(model.files$Species)) %dopar% {
  spp = model.files$Species[l]
  mod.file = model.files$Model[l]
  dat = transect.capture.data[[spp]]
  C<- as.matrix(dat[,2:ncol(dat)]) # get counts 2000-2019
  
  bdata <- list(C = C, # Counts 
                M = nrow(C), # Number of sites
                T = ncol(C),
                Year = AHMbook::standardize(as.matrix(transect.capture.data$year[,2:39])),
                Woylie = AHMbook::standardize(as.matrix(Woylie.cap)), 
                Effort = AHMbook::standardize(as.matrix(transect.capture.data$effort[,2:39])))
  
  # Initial values
  Nst <- C
  Nst[is.na(Nst)] <- 0
  inits <- function(){list(N = Nst + 1)}
  
  out = jagsUI::jags(bdata, inits, params, model.file = mod.file, n.adapt = na, n.chains = nc,
                     n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE, seed = 123)
  out$Species = spp
  out
}

#stopCluster(cl)

#### Step 4: Check Models ####

models.check = data.frame(Name = sapply(model.data.out, function(x) {x$modfile}),
                          Species = sapply(model.data.out, function(x) {x$Species}),
                          DIC = sapply(model.data.out, function(x) {x$DIC}),
                          Rhat_no_1.1 = sapply(model.data.out, function(x) {length(subset(x$summary, x$summary[,8] > 1.1))}),
                          Rhat_no_1.2 = sapply(model.data.out, function(x) {length(subset(x$summary, x$summary[,8] > 1.2))}),
                          Rhat_max = sapply(model.data.out, function(x) {max(x$summary[,8], na.rm=TRUE)}),
                          runtime = sapply(model.data.out, function(x) {x$mcmc.info$elapsed.mins}))

models.check

#### Step 5: Plot Trajectories ####
mod_out = model.data.out[[10]]

site.means = data.frame(mod_out$mean$N)
colnames(site.means) <- 1:38
site.means = cbind(dat[,1], site.means); colnames(site.means)[1] = "TRANSECT"
site.means.long = site.means %>% pivot_longer(cols=2:39, names_to="Period", values_to="Abundance")

site.lower = data.frame(mod_out$q2.5$N)
colnames(site.lower) <- 1:38
site.lower = cbind(dat[,1], site.lower); colnames(site.lower)[1] = "TRANSECT"
site.lower.long = site.lower %>% pivot_longer(cols=2:39, names_to="Period", values_to="Lower")

site.upper = data.frame(mod_out$q97.5$N)
colnames(site.upper) <- 1:38
site.upper = cbind(dat[,1], site.upper); colnames(site.upper)[1] = "TRANSECT"
site.upper.long = site.upper %>% pivot_longer(cols=2:39, names_to="Period", values_to="Upper")

counts = data.frame(mod_out$model$cluster1$data()$C)
colnames(counts) <- 1:38
counts = cbind(dat[,1], counts); colnames(counts)[1] = "TRANSECT"
counts = counts %>% pivot_longer(cols=2:39, names_to="Period", values_to="Counts")

transect.effort = transect.capture.data$effort
transect.effort = transect.effort %>% pivot_longer(cols = 2:39, names_to="PeriodID", values_to="Effort")
transect.effort$PeriodID = as.character(transect.effort$PeriodID)

# Create abundance dataset for modelling in SEM
abundances = left_join(site.means.long, site.lower.long)
abundances = left_join(abundances, site.upper.long)
abundances = left_join(abundances,counts)
abundances$TRANSECT = as.character(abundances$TRANSECT)
abundances = full_join(transect.effort, abundances, by=c("TRANSECT","PeriodID"="Period"))

abundances = abundances %>% filter(Effort > 0)


## 
e=ggplot(abundances) + 
  geom_ribbon(aes(x=as.numeric(PeriodID), ymin=Lower, ymax=Upper,fill="red"), alpha=0.4) + 
  geom_line(aes(x=as.numeric(PeriodID), y=Abundance)) + 
  geom_point(aes(x=as.numeric(PeriodID), y=Counts)) + 
  facet_wrap(~TRANSECT) + theme(legend.position = "none")#+ylim(-50,250)
e


cor.test(abundances$Abundance,abundances$Counts)

plot(abundances$Abundance~abundances$Counts)


#### Step 6: Save abundance predictions for each species' best model #### 
model.data.out = list(best.woylie = model.data.out[[3]], 
                      best.koomal = model.data.out[[9]], 
                      best.chuditch = model.data.out[[12]])


abundance.list = list()
for (i in 1:length(model.data.out)){
  mod_out = model.data.out[[i]]
  site.means = data.frame(mod_out$mean$N)
  colnames(site.means) <- 1:38
  site.means = cbind(dat[,1], site.means)
  site.means.long = site.means %>% pivot_longer(cols=2:39, names_to="Period", values_to="Abundance")
  
  site.lower = data.frame(mod_out$q2.5$N)
  colnames(site.lower) <- 1:38
  site.lower = cbind(dat[,1], site.lower)
  site.lower.long = site.lower %>% pivot_longer(cols=2:39, names_to="Period", values_to="Lower")
  
  site.upper = data.frame(mod_out$q97.5$N)
  colnames(site.upper) <- 1:38
  site.upper = cbind(dat[,1], site.upper)
  site.upper.long = site.upper %>% pivot_longer(cols=2:39, names_to="Period", values_to="Upper")
  
  site.SD = data.frame(mod_out$sd$N)
  colnames(site.SD) <- 1:38
  site.SD = cbind(dat[,1], site.SD)
  site.SD.long = site.upper %>% pivot_longer(cols=2:39, names_to="Period", values_to="SD")
  
  transect.effort = transect.capture.data$effort
  transect.effort = transect.effort %>% pivot_longer(cols = 2:39, names_to="PeriodID", values_to="Effort")
  transect.effort$PeriodID = as.character(transect.effort$PeriodID)
  
  # Create abundance dataset for modelling in SEM
  abundances = left_join(site.means.long, site.lower.long)
  abundances = left_join(abundances, site.upper.long)
  abundances = left_join(abundances, site.SD.long)
  colnames(abundances)[1] = "TRANSECT"
  abundances = full_join(transect.effort, abundances, by=c("TRANSECT","PeriodID"="Period"))
  
  abundances$Species = mod_out$Species
  
  abundances.all = abundances %>% filter(Effort > 0)
  
  abundance.list[[mod_out$Species]] <- abundances.all
}


#save(abundance.list, file = "Data_Processing/transect_bestmodels_abundancesforSEM_23072021.RData")





