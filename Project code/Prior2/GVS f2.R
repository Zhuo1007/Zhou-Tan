#GVS fixd2
data = list(y = y, X = X, n = n, p = p)
init = list(tau = 1, alpha = 0, betaT = rep(0,p),betaT_pseudo=rep(0,p), ind=rep(1,p))


modelstring = "
model {
  for (i in 1:n) {
    mean[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mean[i], tau)
  }

  for (j in 1:p) {
    ind[j] ~ dbern(0.5) 

    betaT[j] ~ dnorm(0, 1.0E-1)  
    
    betaT_pseudo[j] ~ dnorm(0, 1.0)  

    beta[j] <- ind[j] * betaT[j] + (1 - ind[j]) * betaT_pseudo[j]    
  }     

  alpha ~ dnorm(65, 1.0E-2)  
  tau ~ dgamma(0.0001, 0.0001)  
}
"


model <- jags.model(textConnection(modelstring), 
                   data = data, n.chain=2, inits = init)
update(model, n.iter = 1000)  


output_GVS_f2 <- coda.samples(model,
                      variable.names = c("alpha", "beta", "ind", "tau"),
                      n.iter = 10000, thin = 20)

#ESS
alpha_GVSf2<-output_GVS_f2[,"alpha"]
eff_alpha_GVSf2<-effectiveSize(alpha_GVSf2)


#runs for Indicator 
mcmc_mat_GVS_f2 <- as.matrix(output_GVS_f2)
#log(total runs)
log_total_runs_GVS_f2 <- log(sum(colSums(abs(diff(mcmc_mat_GVS_f2[, grep("^ind\\[", colnames(mcmc_mat_GVS_f2))])) != 0)))
total_runs_GVS_f2<- sum(colSums(abs(diff(mcmc_mat_GVS_f2[, grep("^ind\\[", colnames(mcmc_mat_GVS_f2))])) != 0))

ind_cols <- grep("^ind\\[", colnames(mcmc_mat_GVS_f2))
ind_samples_GVS_f2 <- mcmc_mat_GVS_f2[, ind_cols]
pip_GVS_f2 <- colMeans(ind_samples_GVS_f2)


print(summary(output_GVS_f2))
plot(output_GVS_f2)



