#SSVS fixed2
data <- list(y = y, X = X, n = n, p = p)
init <- list(tau = 1, alpha = 0, beta = rep(0, p), IndA = rep(1, p))

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
model <- jags.model(textConnection(modelstring),
                    data = data, n.chains = 2, inits = init)

update(model, n.iter = 1000)

output_SSVS_f2 <- coda.samples(model,
                            variable.names = c("alpha", "beta", "IndA", "tau"), n.iter = 10000, thin=20)

mcmc_mat_SSVS_f2 <- as.matrix(output_SSVS_f2)


#ESS
alpha_SSVSf2<-output_SSVS_f2[,"alpha"]
eff_alpha_SSVSf2<-effectiveSize(alpha_SSVSf2)


#log(total runs)
log_total_runs_SSVS_f2 <- log(sum(colSums(abs(diff(mcmc_mat_SSVS_f2[, grep("^IndA\\[", colnames(mcmc_mat_SSVS_f2))])) != 0)))

total_runs_SSVS_f2_c1 <- sum(colSums(abs(diff(mcmc_mat_SSVS_f2[, grep("^IndA\\[", colnames(mcmc_mat_SSVS_f2))])) != 0))

#PIP
ind_cols <- grep("^IndA\\[", colnames(mcmc_mat_SSVS_f2))
ind_samples_SSVS_f2_c1 <- mcmc_mat_SSVS_f2[, ind_cols]
pip_SSVS_f2_c1 <- colMeans(ind_samples_SSVS_f2_c1)


summary(output_SSVS_f2)
plot(output_SSVS_f2)
