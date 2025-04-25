#SSVS R
data <- list(y = y, X = X, n = n, p = p)
init <- list(tau = 1, alpha = 0, sdBeta = 1, beta = rep(0, p), IndA = rep(1, p))


run_SSVSR_time <- function(reps = 5) {
  modelstring <- "
model {
  for (i in 1:n) {
    mu[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mu[i], tau)
  }

  for (j in 1:p) {
    IndA[j] ~ dbern(0.5)
    TauM[j] <- IndA[j] * tauBeta + (1 - IndA[j]) * 4  # slab uses tauBeta; spike uses fixed precision = 1/(0.5)^2
    beta[j] ~ dnorm(0, TauM[j])
  }

  alpha ~ dnorm(65, 1.0E-2)
  tau ~ dgamma(0.0001, 0.0001)

  tauBeta <-1/(sdBeta)^2
  sdBeta ~ dunif(0, 3)
}
"
  times <- numeric(reps)

  for (i in 1:reps) {
    start <- Sys.time()
  
    model <- jags.model(textConnection(modelstring), 
                      data = data, n.chains = 2, inits = init)
    update(model, n.iter = 500)
    output <- coda.samples(model, 
                         variable.names = c("alpha", "beta", "IndA", "tau", "sdBeta"), n.iter = 1000, thin = 1)
  
    times[i] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  }

  data.frame(Method = "SSVS, Random", Time = times)
}

ssvsR_times <- run_SSVSR_time()