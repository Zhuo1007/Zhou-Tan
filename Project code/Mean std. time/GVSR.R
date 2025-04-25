#GVS R
data <- list(y = y, X = X, n = n, p = p)
init <- list(tau = 1, sdBeta=1, alpha = 0, 
             betaT = rep(0,p),betaT_pseudo=rep(0,p), ind=rep(1,p))


run_GVSR_time <- function(reps = 5) {
  modelstring = "
model {
  for (i in 1:n) {
    mean[i] <- alpha + inprod(X[i,], beta)
    y[i] ~ dnorm(mean[i], tau)
  }

  for (j in 1:p) {
    ind[j] ~ dbern(0.5) 

    betaT[j] ~ dnorm(0, tauBeta)  #when indicator=1
    
    betaT_pseudo[j] ~ dnorm(0, tauBeta*10)  #when indicator=0,use automatic pseudo-prior with k^2=10

    beta[j] <- ind[j] * betaT[j] + (1 - ind[j]) * betaT_pseudo[j]  
  }     

  alpha ~ dnorm(65, 1.0E-2)  
  tau ~ dgamma(0.0001, 0.0001)  
  tauBeta <-1/(sdBeta)^2
  sdBeta ~ dunif(0,3)  
}
"
  times <- numeric(reps)

  for (i in 1:reps) {
    start <- Sys.time()
    
    model <- jags.model(textConnection(modelstring), 
                        data = data, n.chains = 2, inits = init)
    update(model, n.iter = 500)
    output <- coda.samples(model, 
                           variable.names = c("alpha", "beta", "ind", "tau", "sdBeta"), n.iter = 1000, thin = 1)
    
    times[i] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  }
  
  data.frame(Method = "GVS, Random", Time = times)
}

gvsR_times <- run_GVSR_time()
