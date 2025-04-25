#K&M fixed2
data <- list(y = y, X = X, n = n, p = p)         
init <- list(tau = 1, alpha = 0,                 
             betaT = rep(0, p), 
             ind = rep(1, p))

modelstring <- "model {
    for (i in 1:n) {
      mean[i] <- alpha + inprod(X[i,], beta)        
      y[i] ~ dnorm(mean[i], tau)                    
    }
    
    for (j in 1:p) {
      ind[j] ~ dbern(0.5)                           #p = 0.5
      betaT[j] ~ dnorm(0, 1.0E-1)                   #informative prior for betaT
      beta[j] <- ind[j] * betaT[j]                  
    }
    alpha ~ dnorm(65, 1.0E-2)                       
    tau ~ dgamma(0.0001, 0.0001)              
  } "


model <- jags.model(textConnection(modelstring), 
                    data = data, n.chains = 2, inits = init)
update(model, n.iter = 5000) # burn_in

output_KM_f2 <- coda.samples(model, 
                       variable.names = c("alpha", "beta", "ind", "tau"), n.iter = 30000, thin = 20)

#ESS
alpha_KMf2<-output_KM_f2[,"alpha"]
eff_alpha_KMf2<-effectiveSize(alpha_KMf2)

#runs for Indicator 
mcmc_mat_KM_f2 <- as.matrix(output_KM_f2)
#log(total runs)
log_total_runs_KM_f2 <- log(sum(colSums(abs(diff(mcmc_mat_KM_f2[, grep("^ind\\[", colnames(mcmc_mat_KM_f2))])) != 0)))

#PIP
ind_cols <- grep("^ind\\[", colnames(mcmc_mat_KM_f2))
ind_samples_KM_f2 <- mcmc_mat_KM_f2[, ind_cols]
pip_KM_f2 <- colMeans(ind_samples_KM_f2)

print(summary(output_KM_f2))
plot(output_KM_f2)


