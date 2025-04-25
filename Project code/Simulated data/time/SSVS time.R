#SSVS F
data <- list(y = y, X = X, n = n, p = p, 
             tau_spike = tau_spike, tau_slab = tau_slab)
init <- list(tau = 1, alpha = 0, IndA = rep(1, p))


run_SSVSF_time <- function(reps = 1) {
  modelstring <- "
model {
  for (i in 1:n) {
    mu[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mu[i], tau)
  }

  for (j in 1:p) {
    IndA[j] ~ dbern(0.2)                                
    TauM[j] <- IndA[j] * tau_slab[j] + (1 - IndA[j]) * tau_spike[j]  
    beta[j] ~ dnorm(0, TauM[j])
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
                         variable.names = c("alpha", "beta", "IndA", "tau"), n.iter = 30000, thin = 20)
  
  times[i] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
}

data.frame(Method = "SSVS, Fixed", Time = times)
}

ssvsF_times <- run_SSVSF_time()
mean(ssvsF_times$Time) 