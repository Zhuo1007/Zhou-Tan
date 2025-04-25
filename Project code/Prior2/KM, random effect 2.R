#K&M random2
data <- list(y = y, X = X, n = n, p = p)         
init <- list(tau = 1, sdBeta=1, alpha = 0,                 
             betaT = rep(0, p), 
             ind = rep(1, p))

modelstring <- "model {
    for (i in 1:n) {
      mean[i] <- alpha + inprod(X[i,], beta)        
      y[i] ~ dnorm(mean[i], tau)                    
    }
    
    for (j in 1:p) {
      ind[j] ~ dbern(0.5)                           
      betaT[j] ~ dnorm(0, tauBeta)                    
      beta[j] <- ind[j] * betaT[j]                 
    }
    alpha ~ dnorm(65, 1.0E-2)                       # vague prior for intercept
    tau ~ dgamma(0.0001, 0.0001)                    # prior for precision
    tauBeta <- 1/(sdBeta)^2
    sdBeta ~ dunif(0,3)                           #unif distribution for sdBeta
  } "


model <- jags.model(textConnection(modelstring), 
                    data = data,  n.chains =2, inits = init)
update(model, n.iter = 5000) # burn_in

output_KM_r2 <- coda.samples(model, 
                       variable.names = c("alpha", "beta", "ind", "tau", "sdBeta"), n.iter = 30000, thin = 20)

#ESS
alpha_KMr2<-output_KM_r2[,"alpha"]
eff_alpha_KMr2<-effectiveSize(alpha_KMr2)


#runs for Indicator 
mcmc_mat_KM_r2 <- as.matrix(output_KM_r2)
#log(total runs)
log_total_runs_KM_r2 <- log(sum(colSums(abs(diff(mcmc_mat_KM_r2[, grep("^ind\\[", colnames(mcmc_mat_KM_r2))])) != 0)))

ind_cols <- grep("^ind\\[", colnames(mcmc_mat_KM_r2))
ind_samples_KM_r2 <- mcmc_mat_KM_r2[, ind_cols]
pip_KM_r2 <- colMeans(ind_samples_KM_r2)



print(summary(output_KM_r2))
plot(output_KM_r2)