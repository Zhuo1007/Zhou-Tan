#GVS simulated
data_GVS <- list(y = y, X = X, n = n, p = p)

init <- list(tau = 1, alpha = 0,
             betaT = rep(0, p),
             betaT_pseudo = rep(0, p),
             ind = rep(0, p))

modelstring = "model {
    for (i in 1:n) {
      mean[i] <- alpha + inprod(X[i,], beta[])
      y[i] ~ dnorm(mean[i], tau)
    }
    
    for (j in 1:p) {
      ind[j] ~ dbern(0.2)
      betaT[j] ~ dnorm(0, 0.01)                          
      betaT_pseudo[j] ~ dnorm(0, 4) # pseudo-prior 
      beta[j] <- ind[j] * betaT[j] + (1 - ind[j]) * betaT_pseudo[j]
    }
    alpha ~ dnorm(0, 0.1)
    tau ~ dgamma(0.0001, 0.0001)
  }"

model = jags.model(textConnection(modelstring), 
                   data = data_GVS, n.chains = 2, inits = init)

update(model, n.iter = 5000)

output_GVS = coda.samples(model, 
                      variable.names = c("alpha", "beta", "ind", "tau"), n.iter = 30000, thin = 20)

summary(output_GVS)

mcmc_mat_GVS <- as.matrix(output_GVS)

#total runs
total_runs_GVS <- sum(colSums(abs(diff(mcmc_mat_GVS[, grep("^ind\\[", colnames(mcmc_mat_GVS))])) != 0))
log_total_runs_GVS <- log(total_runs_GVS)

#PIP
ind_cols <- grep("^ind\\[", colnames(mcmc_mat_GVS))
ind_samples_GVS <- mcmc_mat_GVS[, ind_cols]
pip_GVS <- colMeans(ind_samples_GVS)
sort(pip_GVS, decreasing = TRUE)



