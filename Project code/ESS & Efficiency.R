S <- niter(output_KM_f2) * nchain(output_KM_f2)

compute_ess_eff_df <- function(output, model_name = "model") {
  n_iter <- niter(output[[1]])
  n_chain <- length(output)
  S <- n_iter * n_chain
  
  ess <- effectiveSize(output)
  eff <- ess / S
  
  df <- data.frame(
    Parameter = names(ess),
    ESS = round(ess, 1),
    Efficiency = round(eff, 4)
  )
  
  cat("\n---", model_name, "---\n")
  print(df)
  
  return(df)
}


ess_df_KMf2  <- compute_ess_eff_df(output_KM_f2,  "KM fixed")
ess_df_KMr2  <- compute_ess_eff_df(output_KM_r2,  "KM random")
ess_df_GVSf2 <- compute_ess_eff_df(output_GVS_f2, "GVS fixed")
ess_df_GVSr2 <- compute_ess_eff_df(output_GVS_r2, "GVS random")
ess_df_SSVSf2 <- compute_ess_eff_df(output_SSVS_f2, "SSVS fixed")
ess_df_SSVSr2 <- compute_ess_eff_df(output_SSVS_r2, "SSVS random")




