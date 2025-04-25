#K&M fixed1
data<- list(y = y, X = X, n = n, p = p)         
init <- list(tau = 1, alpha = 0,                 
             betaT = rep(0, p), 
             ind = rep(1, p))

  modelstring <- "model {
    for (i in 1:n) {
      mean[i] <- alpha + inprod(X[i,], beta)        
      y[i] ~ dnorm(mean[i], tau)                    
    }
    
    for (j in 1:p) {
      ind[j] ~ dbern(0.2)                           # indicator variable, p = 0.2
      betaT[j] ~ dnorm(0, 1.0E-4)                   # vague prior for betaT
      beta[j] <- ind[j] * betaT[j]                  # if ind[j]=0，then beta[j]=0, otherwise beta[j]=betaT
    }
    alpha ~ dnorm(0, 1.0E-8)                       # vague prior for intercept
    tau ~ dgamma(0.0001, 0.0001)                    # prior for precision
  } "


model = jags.model(textConnection(modelstring), 
                       data = data, n.chains =2, inits = init)
update(model, n.iter = 5000) # burn_in
    
output_KM_f1 = coda.samples(model, 
                       variable.names = c("alpha", "beta", "ind", "tau"), n.iter = 30000, thin = 20)

mcmc_mat_KM_f1 <- as.matrix(output_KM_f1)

#log(total runs)
log_total_runs_KM_f1 <- log(sum(colSums(abs(diff(mcmc_mat_KM_f1[, grep("^ind\\[", colnames(mcmc_mat_KM_f1))])) != 0)))

#PIP
ind_cols <- grep("^ind\\[", colnames(mcmc_mat_KM_f1))
ind_samples_KM_f1 <- mcmc_mat_KM_f1[, ind_cols]
pip_KM_f1 <- colMeans(ind_samples_KM_f1)


print(summary(output_KM_f1))
plot(output_KM_f1)



