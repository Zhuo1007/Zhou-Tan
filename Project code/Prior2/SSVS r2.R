#SSVS random1
data <- list(y = y, X = X, n = n, p = p)
init <- list(tau = 1, alpha = 0, sdBeta = 1, beta = rep(0, p), IndA = rep(1, p))

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

  tauBeta <- 1/(sdBeta)^2
  sdBeta ~ dunif(0, 3)
}
"
model <- jags.model(textConnection(modelstring),
                    data = data, n.chains = 2, inits = init)
update(model, n.iter = 1000)

output_SSVS_r2 <- coda.samples(model,
                               variable.names = c("alpha", "beta", "IndA", "tau", "sdBeta"), n.iter = 10000, thin=20)

#ESS
alpha_SSVSr2<-output_SSVS_r2[,"alpha"]
eff_alpha_SSVSr2<-effectiveSize(alpha_SSVSr2)


##runs for Indicator 
mcmc_mat_SSVS_r2 <- as.matrix(output_SSVS_r2)
#log(total runs)
log_total_runs_SSVS_r2 <- log(sum(colSums(abs(diff(mcmc_mat_SSVS_r2[, grep("^IndA\\[", colnames(mcmc_mat_SSVS_r2))])) != 0)))

#PIP
ind_cols <- grep("^IndA\\[", colnames(mcmc_mat_SSVS_r2))
ind_samples_SSVS_r2 <- mcmc_mat_SSVS_r2[, ind_cols]
pip_SSVS_r2 <- colMeans(ind_samples_SSVS_r2)



summary(output_SSVS_r2)
plot(output_SSVS_r2)
