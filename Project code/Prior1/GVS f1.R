#GVS fixed1
data <- list(y = y, X = X, n = n, p = p)
init <- list(tau = 1, alpha = 0, betaT = rep(0,p),betaT_pseudo=rep(0,p), ind=rep(1,p))


modelstring = "
model {
  for (i in 1:n) {
    mean[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mean[i], tau)
  }

  for (j in 1:p) {
    ind[j] ~ dbern(0.2) 

    betaT[j] ~ dnorm(0, 1.0E-4)  #when indicator=1
    
    betaT_pseudo[j] ~ dnorm(0, 1)  #when indicator=0,use automatic pseudo-prior with k=10^2

    beta[j] <- ind[j] * betaT[j] + (1 - ind[j]) * betaT_pseudo[j]  #variable selection
  }     

  alpha ~ dnorm(0, 1.0E-8)  
  tau ~ dgamma(0.0001, 0.0001)  
}
"


model = jags.model(textConnection(modelstring), 
                   data = data, n.chain=2, inits = init)
update(model, n.iter = 1000)  


output_GVS_f1 = coda.samples(model,
                      variable.names = c("alpha", "beta", "ind", "tau"),
                      n.iter = 10000, thin = 20)

 
mcmc_mat_GVS_f1 <- as.matrix(output_GVS_f1)

#log(total runs)
log_total_runs_GVS_f1<- log(sum(colSums(abs(diff(mcmc_mat_GVS_f1[, grep("^ind\\[", colnames(mcmc_mat_GVS_f1))])) != 0)))

#Tuning total runs
total_runs_GVS_f1<- sum(colSums(abs(diff(mcmc_mat_GVS_f1[, grep("^ind\\[", colnames(mcmc_mat_GVS_f1))])) != 0))


#PIP
ind_cols <- grep("^ind\\[", colnames(mcmc_mat_GVS_f1))
ind_samples_GVS_f1 <- mcmc_mat_GVS_f1[, ind_cols]
pip_GVS_f1 <- colMeans(ind_samples_GVS_f1)



print(summary(output_GVS_f1))
plot(output_GVS_f1)



