#GVS F
data = list(y = y, X = X, n = n, p = p)
init = list(tau = 1, alpha = 0, betaT = rep(0,p),betaT_pseudo=rep(0,p), ind=rep(1,p))

run_GVSF_time <- function(reps = 1) {
  modelstring = "
model {
  for (i in 1:n) {
    mean[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mean[i], tau)
  }

  for (j in 1:p) {
    ind[j] ~ dbern(0.2) 

    betaT[j] ~ dnorm(0, 0.01)  
    
    betaT_pseudo[j] ~ dnorm(0, 0.4)  

    beta[j] <- ind[j] * betaT[j] + (1 - ind[j]) * betaT_pseudo[j]    
  }     

  alpha ~ dnorm(0, 0.1)  
  tau ~ dgamma(0.0001, 0.0001)  
}
"

times <- numeric(reps)

for (i in 1:reps) {
  start <- Sys.time()
  
  model <- jags.model(textConnection(modelstring), 
                      data = data, n.chains = 2, inits = init)
  update(model, n.iter = 5000)
  output <- coda.samples(model, 
                         variable.names = c("alpha", "beta", "ind", "tau"), n.iter = 30000, thin = 20)
  
  times[i] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
}

data.frame(Method = "GVS, Fixed", Time = times)
}

gvsF_times <- run_GVSF_time()
