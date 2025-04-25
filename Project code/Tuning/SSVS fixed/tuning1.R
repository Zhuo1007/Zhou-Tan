#SSVS fixed1
data <- list(y = y, X = X, n = n, p = p)
init <- list(tau = 1, alpha = 0, beta = rep(0, p), IndA = rep(1, p))

modelstring <- "
model {
  for (i in 1:n) {
    mu[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mu[i], tau)
  }

  for (j in 1:p) {
    IndA[j] ~ dbern(0.2)                                # inclusion probability
    TauM[j] <- IndA[j] * (1.0E-4) + (1 - IndA[j]) * 1/(3.5)^2 # mixture prior
    beta[j] ~ dnorm(0, TauM[j])
  }

  alpha ~ dnorm(0, 1.0E-8)
  tau ~ dgamma(0.0001, 0.0001)
}
"
model <- jags.model(textConnection(modelstring),
                    data = data, n.chains = 2, inits = init)

update(model, n.iter = 1000)

output_SSVS_f1_c2 <- coda.samples(model,
                               variable.names = c("alpha", "beta", "IndA", "tau"), n.iter = 10000, thin=20)

mcmc_mat_SSVS_f1_c2 <- as.matrix(output_SSVS_f1_c2)


total_runs_SSVS_f1_c2 <- sum(colSums(abs(diff(mcmc_mat_SSVS_f1_c2[, grep("^IndA\\[", colnames(mcmc_mat_SSVS_f1_c2))])) != 0))

#PIP
ind_cols <- grep("^IndA\\[", colnames(mcmc_mat_SSVS_f1_c2))
ind_samples_SSVS_f1_c2 <- mcmc_mat_SSVS_f1_c2[, ind_cols]
pip_SSVS_f1_c2 <- colMeans(ind_samples_SSVS_f1_c2)

sort(pip_SSVS_f1_c1, decreasing = TRUE)
sort(pip_SSVS_f1_c2, decreasing = TRUE)
sort(pip_SSVS_f1_c3, decreasing = TRUE)
sort(pip_SSVS_f1_c4, decreasing = TRUE)





