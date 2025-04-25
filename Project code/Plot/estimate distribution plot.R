build_plot_data_I1 <- function(I_label = "I1") {
  df <- data.frame()
  for (j in 1:length(var_names)) {
    for (m in 1:length(methods)) {
      method <- methods[m]
      method_name <- method_names[m]
      variable_name <- var_names[j]
      
      mode_vals <- get(paste0("beta_modes_", method, "_", I_label))[j]
      hpd_mat <- get(paste0("hpd_", method, "_", I_label))
      
      hpd_low <- hpd_mat[paste0("beta[", j, "]"), "lower"]
      hpd_high <- hpd_mat[paste0("beta[", j, "]"), "upper"]
      
      df <- rbind(df, data.frame(
        Variable = variable_name,
        Method = method_name,
        Mode = mode_vals,
        HPD_low = hpd_low,
        HPD_high = hpd_high,
        I_group = I_label
      ))
    }
  }
  return(df)
}

build_plot_data_I0 <- function(I_label = "I0") {
  df <- data.frame()
  for (j in 1:length(var_names)) {
    for (m in 1:length(methods)) {
      method <- methods[m]
      method_name <- method_names[m]
      variable_name <- var_names[j]
      
      mode_vals <- get(paste0("beta_modes_", method, "_", I_label))[j]
      hpd_mat <- get(paste0("hpd_", method, "_", I_label))
      
      hpd_low <- hpd_mat[paste0("beta[", j, "]"), "lower"]
      hpd_high <- hpd_mat[paste0("beta[", j, "]"), "upper"]
      
      df <- rbind(df, data.frame(
        Variable = variable_name,
        Method = method_name,
        Mode = mode_vals,
        HPD_low = hpd_low,
        HPD_high = hpd_high,
        I_group = I_label
      ))
    }
  }
  return(df)
}


plot_data_I1 <- build_plot_data_I1("I1")
plot_data_I0 <- build_plot_data_I0("I0")


plot_data <- rbind(plot_data_I1, plot_data_I0)
plot_data$Variable <- factor(plot_data$Variable, levels =var_names)
plot_data$Method <- factor(plot_data$Method,levels = method_names )

### color set
plot_data$I_color <- ifelse(
  plot_data$I_group == "I1" & grepl("Fixed", plot_data$Method), "black",
  ifelse(plot_data$I_group == "I1" & grepl("Random", plot_data$Method), "red",
  ifelse(plot_data$I_group == "I0" & grepl("Fixed", plot_data$Method), "grey50", "pink"))
)


### plot
ggplot(plot_data, aes(x = Mode, y = Method)) +
  geom_vline(xintercept = 0, color = "darkgrey", linetype = 4) +
  geom_point(aes(color = I_color), shape=18, size = 2.5) +
  geom_errorbarh(aes(xmin = HPD_low, xmax = HPD_high, color = I_color), height = 0.25, linewidth = 1.2) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 3) +
  scale_color_identity() +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    

    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    
    axis.ticks = element_line(color = "black", linewidth = 0.5)
  ) +
  xlab(expression(beta[j] ~ "|" ~ I[j] * "), data"))+
  ylab(NULL)


