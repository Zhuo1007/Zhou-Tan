#GVS random1
data <- list(y = y, X = X, n = n, p = p)
init <- list(tau = 1, sdBeta=1, alpha = 0, 
             betaT = rep(0,p),betaT_pseudo=rep(0,p), ind=rep(1,p))


modelstring = "
model {
  for (i in 1:n) {
    mean[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mean[i], tau)
  }

  for (j in 1:p) {
    ind[j] ~ dbern(0.2) 

    betaT[j] ~ dnorm(0, tauBeta)  #when indicator=1  
    
    betaT_pseudo[j] ~ dnorm(0, tauBeta*1.0E4)  #when indicator=0, automatic pseudo-prior with k=10^2:variance = σ² / 10^4 → precision = tauBeta × 10^4

    beta[j] <- ind[j] * betaT[j] + (1 - ind[j]) * betaT_pseudo[j]  #variable selection
  }     

  alpha ~ dnorm(0, 1.0E-8)  
  tau ~ dgamma(0.0001, 0.0001)  
  tauBeta <- 1/(sdBeta)^2
  sdBeta ~ dunif(0,10)  
}
"


model <- jags.model(textConnection(modelstring), 
                   data = data, n.chain=2, inits = init)
update(model, n.iter = 1000)  


output_GVS_r1 <- coda.samples(model,
                             variable.names = c("alpha", "beta", "ind", "tau", "sdBeta"),
                             n.iter = 10000, thin = 20)

mcmc_mat_GVS_r1 <- as.matrix(output_GVS_r1)

#log(total runs)
log_total_runs_GVS_r1 <- log(sum(colSums(abs(diff(mcmc_mat_GVS_r1[, grep("^ind\\[", colnames(mcmc_mat_GVS_r1))])) != 0)))

#Tuning total runs
total_runs_GVS_r1 <- sum(colSums(abs(diff(mcmc_mat_GVS_r1[, grep("^ind\\[", colnames(mcmc_mat_GVS_r1))])) != 0))
colMeans(mcmc_mat_GVS_r1[, grep("ind", colnames(mcmc_mat_GVS_r1))])


#PIP
ind_cols <- grep("^ind\\[", colnames(mcmc_mat_GVS_r1))
ind_samples_GVS_r1 <- mcmc_mat_GVS_r1[, ind_cols]
pip_GVS_r1 <- colMeans(ind_samples_GVS_r1)



print(summary(output_GVS_r1))
plot(output_GVS_r1)



