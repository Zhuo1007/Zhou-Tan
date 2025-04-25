install.packages("HDInterval")
library(HDInterval)

var_names <- names(attitude)[-1]  
methods <- c("KM_f1", "KM_r1", "GVS_f1", "GVS_r1","SSVS_f1", "SSVS_r1" )
method_names<-c("KM, Fixed", "KM, Random", "GVS, Fixed",  "GVS, Random", "SSVS, Fixed", "SSVS, Random")
beta_cols <- grep("^beta\\[", colnames(mcmc_mat_KM_f1), value = TRUE)

#beta_samples

  #KM f1
beta_samples_KM_f1 <- mcmc_mat_KM_f1[, beta_cols]

beta_KM_f1_I1_list <- list()
beta_KM_f1_I0_list <- list()

for (j in 1:6) {
  beta_j <- beta_samples_KM_f1[, j]
  I_j <- ind_samples_KM_f1[, j]
  
  beta_KM_f1_I1_list[[j]] <- beta_j[I_j == 1]
  beta_KM_f1_I0_list[[j]] <- beta_j[I_j == 0]
}

names(beta_KM_f1_I1_list) <- colnames(beta_samples_KM_f1)
names(beta_KM_f1_I0_list) <- colnames(beta_samples_KM_f1)

  #KM r1
beta_samples_KM_r1 <- mcmc_mat_KM_r1[, beta_cols]

beta_KM_r1_I1_list <- list()
beta_KM_r1_I0_list <- list()

for (j in 1:6) {
  beta_j <- beta_samples_KM_r1[, j]
  I_j <- ind_samples_KM_r1[, j]
  
  beta_KM_r1_I1_list[[j]] <- beta_j[I_j == 1]
  beta_KM_r1_I0_list[[j]] <- beta_j[I_j == 0]
}

names(beta_KM_r1_I1_list) <- colnames(beta_samples_KM_r1)
names(beta_KM_r1_I0_list) <- colnames(beta_samples_KM_r1)

  #GVS f1
beta_samples_GVS_f1 <- mcmc_mat_GVS_f1[, beta_cols]
beta_GVS_f1_I1_list <- list()
beta_GVS_f1_I0_list <- list()

for (j in 1:6) {
  beta_j <- beta_samples_GVS_f1[, j]
  I_j <- ind_samples_GVS_f1[, j]
  
  beta_GVS_f1_I1_list[[j]] <- beta_j[I_j == 1]
  beta_GVS_f1_I0_list[[j]] <- beta_j[I_j == 0]
}

names(beta_GVS_f1_I1_list) <- colnames(beta_samples_GVS_f1)
names(beta_GVS_f1_I0_list) <- colnames(beta_samples_GVS_f1)

  #GVSr1
beta_samples_GVS_r1 <- mcmc_mat_GVS_r1[, beta_cols]

beta_GVS_r1_I1_list <- list()
beta_GVS_r1_I0_list <- list()

for (j in 1:6) {
  beta_j <- beta_samples_GVS_r1[, j]
  I_j <- ind_samples_GVS_r1[, j]
  
  beta_GVS_r1_I1_list[[j]] <- beta_j[I_j == 1]
  beta_GVS_r1_I0_list[[j]] <- beta_j[I_j == 0]
}

names(beta_GVS_r1_I1_list) <- colnames(beta_samples_GVS_r1)
names(beta_GVS_r1_I0_list) <- colnames(beta_samples_GVS_r1)

  #SSVS f1
beta_samples_SSVS_f1 <- mcmc_mat_SSVS_f1[, beta_cols]

beta_SSVS_f1_I1_list <- list()
beta_SSVS_f1_I0_list <- list()

for (j in 1:6) {
  beta_j <- beta_samples_SSVS_f1[, j]
  I_j <- ind_samples_SSVS_f1[, j]
  
  beta_SSVS_f1_I1_list[[j]] <- beta_j[I_j == 1]
  beta_SSVS_f1_I0_list[[j]] <- beta_j[I_j == 0]
}

names(beta_SSVS_f1_I1_list) <- colnames(beta_samples_SSVS_f1)
names(beta_SSVS_f1_I0_list) <- colnames(beta_samples_SSVS_f1)

  #SSVS r1
beta_samples_SSVS_r1 <- mcmc_mat_SSVS_r1[, beta_cols]

beta_SSVS_r1_I1_list <- list()
beta_SSVS_r1_I0_list <- list()

for (j in 1:6) {
  beta_j <- beta_samples_SSVS_r1[, j]
  I_j <- ind_samples_SSVS_r1[, j]
  
  beta_SSVS_r1_I1_list[[j]] <- beta_j[I_j == 1]
  beta_SSVS_r1_I0_list[[j]] <- beta_j[I_j == 0]
}

names(beta_SSVS_r1_I1_list) <- colnames(beta_samples_SSVS_r1)
names(beta_SSVS_r1_I0_list) <- colnames(beta_samples_SSVS_r1)

#Poster mode
get_mode <- function(x) {
  if (length(x) < 2) return(0)
  dens <- density(x)
  dens$x[which.max(dens$y)]
}

  #KM
beta_modes_KM_f1_I1 <- sapply(beta_KM_f1_I1_list, get_mode)
beta_modes_KM_f1_I0 <- sapply(beta_KM_f1_I0_list, get_mode)

beta_modes_KM_r1_I1 <- sapply(beta_KM_r1_I1_list, get_mode)
beta_modes_KM_r1_I0 <- sapply(beta_KM_r1_I0_list, get_mode)

  #GVS
beta_modes_GVS_f1_I1 <- sapply(beta_GVS_f1_I1_list, get_mode)
beta_modes_GVS_f1_I0 <- sapply(beta_GVS_f1_I0_list, get_mode)

beta_modes_GVS_r1_I1 <- sapply(beta_GVS_r1_I1_list, get_mode)
beta_modes_GVS_r1_I0 <- sapply(beta_GVS_r1_I0_list, get_mode)

  #SSVS
beta_modes_SSVS_f1_I1 <- sapply(beta_SSVS_f1_I1_list, get_mode)
beta_modes_SSVS_f1_I0 <- sapply(beta_SSVS_f1_I0_list, get_mode)

beta_modes_SSVS_r1_I1 <- sapply(beta_SSVS_r1_I1_list, get_mode)
beta_modes_SSVS_r1_I0 <- sapply(beta_SSVS_r1_I0_list, get_mode)

#HPD
  #KM
hpd_KM_f1_I1 <- t(sapply(beta_KM_f1_I1_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

hpd_KM_f1_I0 <- t(sapply(beta_KM_f1_I0_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

hpd_KM_r1_I1 <- t(sapply(beta_KM_r1_I1_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

hpd_KM_r1_I0 <- t(sapply(beta_KM_r1_I0_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

  #GVS
hpd_GVS_f1_I1 <- t(sapply(beta_GVS_f1_I1_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

hpd_GVS_f1_I0 <- t(sapply(beta_GVS_f1_I0_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

hpd_GVS_r1_I1 <- t(sapply(beta_GVS_r1_I1_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

hpd_GVS_r1_I0 <- t(sapply(beta_GVS_r1_I0_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

  #SSVS
hpd_SSVS_f1_I1 <- t(sapply(beta_SSVS_f1_I1_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

hpd_SSVS_f1_I0 <- t(sapply(beta_SSVS_f1_I0_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

hpd_SSVS_r1_I1 <- t(sapply(beta_SSVS_r1_I1_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

hpd_SSVS_r1_I0 <- t(sapply(beta_SSVS_r1_I0_list, function(x) {
  if (length(x) >= 2) hdi(x, credMass = 0.7) else c(lower = 0, upper = 0)
}))

#Data frame
#I=1
generate_beta_summary <- function(I_label = "I1") {
  
  result_list <- list()
  
  for (j in 1:6) {
    
    mode_vals <- numeric(6)
    hpd_low <- numeric(6)
    hpd_high <- numeric(6)
    
    for (m in 1:6) {
      
      mode_var <- get(paste0("beta_modes_", methods[m], "_", I_label))
      hpd_mat  <- get(paste0("hpd_", methods[m], "_", I_label))
      
      mode_vals[m] <- mode_var[j]
      hpd_low[m] <- hpd_mat[paste0("beta[", j, "]"), "lower"]
      hpd_high[m] <- hpd_mat[paste0("beta[", j, "]"), "upper"]
    }
    
    result_list[[paste0("v", j)]] <- data.frame(
      Variable = method_names,
      Mode = mode_vals,
      HPD_low = hpd_low,
      HPD_high = hpd_high
    )
  }
  
  return(result_list)
}

v_I1_list <- generate_beta_summary("I1")

#I=0
generate_beta_summary <- function(I_label = "I0") {
  
  result_list <- list()
  
  for (j in 1:6) {
    
    mode_vals <- numeric(6)
    hpd_low <- numeric(6)
    hpd_high <- numeric(6)
    
    for (m in 1:6) {
      
      mode_var <- get(paste0("beta_modes_", methods[m], "_", I_label))
      hpd_mat  <- get(paste0("hpd_", methods[m], "_", I_label))
      
      mode_vals[m] <- mode_var[j]
      hpd_low[m] <- hpd_mat[paste0("beta[", j, "]"), "lower"]
      hpd_high[m] <- hpd_mat[paste0("beta[", j, "]"), "upper"]
    }
    
    result_list[[paste0("v", j)]] <- data.frame(
      Variable = method_names,
      Mode = mode_vals,
      HPD_low = hpd_low,
      HPD_high = hpd_high
    )
  }
  
  return(result_list)
}

v_I0_list <- generate_beta_summary("I0") 




