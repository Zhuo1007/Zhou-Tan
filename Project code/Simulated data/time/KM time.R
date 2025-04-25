#KM F
data<- list(y = y, X = X, n = n, p = p)         
init <- list(tau = 1, alpha = 0,                 
             betaT = rep(0, p), 
             ind = rep(1, p))

run_KMF_time <- function(reps = 1) {
  modelstring <- "model {
    for (i in 1:n) {
      mean[i] <- alpha + inprod(X[i,], beta)        
      y[i] ~ dnorm(mean[i], tau)                    
    }
    
    for (j in 1:p) {
      ind[j] ~ dbern(0.2)                           
      betaT[j] ~ dnorm(0, 0.01)                   
      beta[j] <- ind[j] * betaT[j]                   
    }
    alpha ~ dnorm(0, 0.1)                       
    tau ~ dgamma(0.0001, 0.0001)                    
  } "
  times <- numeric(reps)
  
  for (i in 1:reps) {
    start <- Sys.time()
    
    model <- jags.model(textConnection(modelstring), 
                        data = data, n.chains = 2, inits = init)
    update(model, n.iter = 5000)
    output <- coda.samples(model, 
                           variable.names = c("alpha", "beta", "ind", "tau"), n.iter = 30000, thin = 20)
    
    times[i] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  }
  
  data.frame(Method = "KM, Fixed", Time = times)
}

kmF_times <- run_KMF_time()
 

