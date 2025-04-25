#SSVS F
data <- list(y = y, X = X, n = n, p = p)
init <- list(tau = 1, alpha = 0, beta = rep(0, p), IndA = rep(1, p))

run_SSVSF_time <- function(reps = 5) {
  modelstring <- "
model {
  for (i in 1:n) {
    mu[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mu[i], tau)
  }

  for (j in 1:p) {
    IndA[j] ~ dbern(0.5)                               
    TauM[j] <- IndA[j] * (1.0E-1) + (1 - IndA[j]) * 4  
    beta[j] ~ dnorm(0, TauM[j])
  }

  alpha ~ dnorm(65, 1.0E-2)
  tau ~ dgamma(0.0001, 0.0001)
}
"
  times <- numeric(reps)
  
  for (i in 1:reps) {
    start <- Sys.time()
    
    model <- jags.model(textConnection(modelstring), 
                        data = data, n.chains = 2, inits = init)
    update(model, n.iter = 500)
    output <- coda.samples(model, 
                           variable.names = c("alpha", "beta", "IndA", "tau"), n.iter = 1000, thin = 1)
    
    times[i] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  }
  
  data.frame(Method = "SSVS, Fixed", Time = times)
}

ssvsF_times <- run_SSVSF_time()