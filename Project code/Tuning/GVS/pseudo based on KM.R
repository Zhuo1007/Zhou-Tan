sum_KM <- summary(output_KM_f2)


sum_mat <- sum_KM$statistics


beta_rows <- grep("^beta\\[", rownames(sum_mat))


beta_means <- sum_mat[beta_rows, "Mean"]
beta_sds   <- sum_mat[beta_rows, "SD"]

pseudo_prior_df <- data.frame(
  beta = rownames(sum_mat)[beta_rows],
  mean = beta_means,
  sd = beta_sds
)

print(pseudo_prior_df)

mu_pseudo <- pseudo_prior_df$mean
prec_pseudo <- 1 / (pseudo_prior_df$sd^2)

# GVS with pseudo-prior
data_GVS <- list(y = y, X = X, n = n, p = p,
                 mu_pseudo = mu_pseudo,
                 prec_pseudo = prec_pseudo)

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
      betaT[j] ~ dnorm(0, 1.0E-4)                          
      betaT_pseudo[j] ~ dnorm(mu_pseudo[j], prec_pseudo[j]) # pseudo-prior 
      beta[j] <- ind[j] * betaT[j] + (1 - ind[j]) * betaT_pseudo[j]
    }
    alpha ~ dnorm(0, 1.0E-10)
    tau ~ dgamma(0.0001, 0.0001)
  }"

model = jags.model(textConnection(modelstring), 
                   data = data_GVS, n.chains = 2, inits = init)

update(model, n.iter = 500)

output = coda.samples(model, 
                      variable.names = c("alpha", "beta", "ind", "tau"), n.iter = 1000, thin = 1)


summary(output)


mcmc_mat_GVS_p_f1 <- as.matrix(output)

total_runs_GVS_p_f1<- sum(colSums(abs(diff(mcmc_mat_GVS_p_f1[, grep("^ind\\[", colnames(mcmc_mat_GVS_p_f1))])) != 0))


