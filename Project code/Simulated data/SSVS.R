#LSE
ols <- lm(y~X)
print(summary(ols))

beta_std <- summary(ols)$coefficients[-1, 2]  

tau_spike <- (10/ beta_std)^2             # spike precision = 1 / tau^2

tau_slab  <- tau_spike/100^2             

#SSVS simulated
data <- list(y = y, X = X, n = n, p = p, 
             tau_spike = tau_spike, tau_slab = tau_slab)
init <- list(tau = 1, alpha = 0, IndA = rep(1, p))

modelstring <- "
model {
  for (i in 1:n) {
    mu[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mu[i], tau)
  }

  for (j in 1:p) {
    IndA[j] ~ dbern(0.2)                                # inclusion probability
    TauM[j] <- IndA[j] * tau_slab[j] + (1 - IndA[j]) * tau_spike[j]  # mixture prior
    beta[j] ~ dnorm(0, TauM[j])
  }

  alpha ~ dnorm(0, 0.1)
  tau ~ dgamma(0.0001, 0.0001)
}
"
model <- jags.model(textConnection(modelstring),
                    data = data, n.chains = 2, inits = init)

update(model, n.iter = 5000)

output_SSVS <- coda.samples(model,
                               variable.names = c("alpha", "beta", "IndA", "tau"), n.iter = 30000, thin=20)

mcmc_mat_SSVS <- as.matrix(output_SSVS)

#log(total runs)
total_runs_SSVS <- sum(colSums(abs(diff(mcmc_mat_SSVS[, grep("^IndA\\[", colnames(mcmc_mat_SSVS))])) != 0))
log_total_runs_SSVS <- log(total_runs_SSVS)
  
#PIP
ind_cols <- grep("^IndA\\[", colnames(mcmc_mat_SSVS))
ind_samples_SSVS <- mcmc_mat_SSVS[, ind_cols]
pip_SSVS <- colMeans(ind_samples_SSVS)
sort(pip_SSVS, decreasing = TRUE)


summary(output_SSVS)
plot(output_SSVS)
