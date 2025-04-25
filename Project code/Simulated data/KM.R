#K&M simulated
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
      betaT[j] ~ dnorm(0, 0.01)                   
      beta[j] <- ind[j] * betaT[j]                  # if ind[j]=0，then beta[j]=0, otherwise beta[j]=betaT
    }
    alpha ~ dnorm(0, 0.1)                       
    tau ~ dgamma(0.0001, 0.0001)                    # prior for precision
  } "


model = jags.model(textConnection(modelstring), 
                   data = data, n.chains =2, inits = init)
update(model, n.iter = 5000) # burn_in

output_KM = coda.samples(model, 
                            variable.names = c("alpha", "beta", "ind", "tau"), n.iter = 30000, thin = 20)

mcmc_mat_KM <- as.matrix(output_KM)

#total runs
total_runs_KM <- sum(colSums(abs(diff(mcmc_mat_KM[, grep("^ind\\[", colnames(mcmc_mat_KM))])) != 0))
log_total_runs_KM <- log(total_runs_KM)

#PIP
ind_cols <- grep("^ind\\[", colnames(mcmc_mat_KM))
ind_samples_KM <- mcmc_mat_KM[, ind_cols]
pip_KM <- colMeans(ind_samples_KM)
sort(pip_KM, decreasing = TRUE)

print(summary(output_KM))
plot(output_KM)



