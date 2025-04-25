methods <- c("KM, Fixed", "KM, Random", 
             "GVS, Fixed", "GVS, Random", 
             "SSVS, Fixed", "SSVS, Random")
#Prior 1
log_values_1 <- c(log_total_runs_KM_f1, log_total_runs_KM_r1,
                log_total_runs_GVS_f1, log_total_runs_GVS_r1,
                log_total_runs_SSVS_f1, log_total_runs_SSVS_r1)

all_run_1 <- data.frame(Method = factor(methods, levels = methods),
                      log_total_run = log_values_1)

p1 <- ggplot(all_run_1, aes(x = log_total_run , y = Method,group=1)) +
  geom_path(data = all_run_1, aes(x = log_total_run , y = Method, group = 1),
            color = "magenta", size = 1) +
  geom_point(data = all_run_1, aes(x = log_total_run , y = Method),
             color = "magenta", size = 2) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,face = "bold"),
    text = element_text(size = 13, ,face = "bold")
  )+
  annotate("text", x = -Inf, y = Inf, label = "a", hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold")

#Prior 2
log_values_2 <- c(log_total_runs_KM_f2, log_total_runs_KM_r2,
                log_total_runs_GVS_f2, log_total_runs_GVS_r2,
                log_total_runs_SSVS_f2, log_total_runs_SSVS_r2)

all_run_2 <- data.frame(Method = factor(methods, levels = methods),
                        log_total_run = log_values_2)

p2 <- ggplot(all_run_2, aes(x = log_total_run , y = Method, group = 1)) +
  geom_path(color = "magenta", size = 1) +
  geom_point(color = "magenta", size = 2) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_blank(),       
    axis.title.y = element_blank(),      
    text = element_text(size = 13, face = "bold")
  )+
  annotate("text", x = -Inf, y = Inf, label = "b", hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold")

#Combine
combined_plot <- (p1 + p2) +
  plot_layout(ncol = 2) +
  plot_annotation(caption = "log(Total Number of Runs for I_j)") &
  theme(
    plot.caption = element_text(
      hjust = 0.7,
      vjust = 1.8,
      size = 14,
      face = "bold"
    ),
    plot.margin = margin(5, 20, 5, 5)  # top, right, bottom, left
  )


combined_plot


