#Number of runs
spike_std = c(1.0, 1.5, 2.5, 3.5)
total_runs = c(total_runs_SSVS_f1_c1, total_runs_SSVS_f1_c2,
               total_runs_SSVS_f1_c3, total_runs_SSVS_f1_c4)

data <- data.frame(
  spike_std_values = spike_std,
  runs_values =total_runs
)


ggplot(data, aes(x = spike_std , y = total_runs)) +
  geom_line(size = 1) +                
  geom_point(size = 2) +
  labs(x = "Spike Standard Deviation", y = "Total Number of Runs") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,face = "bold"),
    panel.grid.major = element_blank(),        
    panel.grid.minor = element_blank(), 
    text = element_text(size = 13, face = "bold")
  ) 

#PIP
pip_df <- data.frame(
  Variable = rep(var_names, 4),
  PIP = c(pip_SSVS_f1_c1, pip_SSVS_f1_c2, pip_SSVS_f1_c3, pip_SSVS_f1_c4),
  Model = rep(c("DeltaY_1", "DeltaY_2", "DeltaY_3", "DeltaY_4"), each = length(var_names))
)

pip_df$Variable <- factor(pip_df$Variable, levels = rev(var_names))

ggplot(pip_df, aes(x = PIP, y = Variable, linetype = Model, color = Model)) +

  geom_vline(xintercept = 0.2, linetype = "dashed", color = "#F4A7B9", size = 1) +
  
  geom_path(aes(group = Model), size = 1) +
  
  scale_color_manual(values = c("DeltaY_1" = "black", 
                                "DeltaY_2" = "gray40", 
                                "DeltaY_3" = "gray60", 
                                "DeltaY_4" = "gray80")) +
  scale_linetype_manual(values = c("DeltaY_1" = "solid", 
                                   "DeltaY_2" = "dashed", 
                                   "DeltaY_3" = "dotdash", 
                                   "DeltaY_4" = "twodash")) +
  guides(
    color = guide_legend(title = expression(Delta*Y[i])),
    linetype = guide_legend(title = expression(Delta*Y[i]))
  ) +
  

  scale_x_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0"),
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  xlab("Probability") +
  ylab(NULL) +
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    text = element_text(size = 13, face = "bold"),
    plot.margin = margin(5, 20, 5, 5),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )