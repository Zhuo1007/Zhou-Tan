var_names <- names(attitude)[-1]  

# Prior 1
#KM
pip_KM1 <- data.frame(
  Variable = rep(var_names, 2),
  PIP = c(pip_KM_f1, pip_KM_r1),
  Model = rep(c("Fixed", "Random"), each = length(var_names))
)

pip_KM1$Variable <- factor(pip_KM1$Variable, levels = rev(var_names))
#Plot
p1<- ggplot(pip_KM1, aes(x = PIP, y = Variable, linetype = Model)) +
  # P(I_j=1)
  geom_vline(xintercept = 0.2, linetype = "dashed", color = "#F4A7B9", size = 1) +
  
  geom_path(aes(group = Model, color = Model), size = 1) +
  scale_color_manual(values = c("Fixed" = "gray40", "Random" = "black")) +
  scale_linetype_manual(values = c("Fixed" = "solid", "Random" = "dashed")) +
  
  guides(color = "none", linetype = "none")+
  
  scale_x_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    labels = rep("", 6),
      limits = c(0, 1.05),  
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Priors 1")+
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold"),
    text = element_text(size = 13, face = "bold"),
    plot.margin = margin(5, 20, 5, 5),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    
  ) 

#GVS
pip_GVS1 <- data.frame(
  Variable = rep(var_names, 2),
  PIP = c(pip_GVS_f1, pip_GVS_r1),
  Model = rep(c("Fixed", "Random"), each = length(var_names))
)

pip_GVS1$Variable <- factor(pip_GVS1$Variable, levels = rev(var_names))
#Plot
p2<-ggplot(pip_GVS1, aes(x = PIP, y = Variable, linetype = Model)) +
  # P(I_j=1)
  geom_vline(xintercept = 0.2, linetype = "dashed", color = "#F4A7B9", size = 1) +
  
  geom_path(aes(group = Model, color = Model), size = 1) +
  scale_color_manual(values = c("Fixed" = "gray40", "Random" = "black")) +
  scale_linetype_manual(values = c("Fixed" = "solid", "Random" = "dashed")) +
  
  guides(color = "none", linetype = "none")+
  
  scale_x_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    labels = rep("", 6) ,
      limits = c(0, 1.05),  
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  xlab(NULL) +
  ylab(NULL) + 
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold"),
    text = element_text(size = 13, face = "bold"),
    plot.margin = margin(5, 20, 5, 5)
  ) 

#SSVS
pip_SSVS1 <- data.frame(
  Variable = rep(var_names, 2),
  PIP = c(pip_SSVS_f1, pip_SSVS_r1),
  Model = rep(c("Fixed", "Random"), each = length(var_names))
)

pip_SSVS1$Variable <- factor(pip_SSVS1$Variable, levels = rev(var_names))
#Plot
p3<-ggplot(pip_SSVS1, aes(x = PIP, y = Variable, linetype = Model)) +
  
  geom_vline(xintercept = 0.2, linetype = "dashed", color = "#F4A7B9", size = 1) +
  
  geom_path(aes(group = Model, color = Model), size = 1) +
  scale_color_manual(values = c("Fixed" = "gray40", "Random" = "black")) +
  scale_linetype_manual(values = c("Fixed" = "solid", "Random" = "dashed")) +
  guides(color = "none", linetype = "none") +
  
  scale_x_continuous(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    limits = c(0, 1.05),  
    expand = expansion(mult = c(0, 0.05))  
  ) +
  
  xlab(NULL) +
  ylab(NULL) + 
  
  theme_bw() +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    text = element_text(size = 13, face = "bold"),
    plot.margin = margin(5, 20, 5, 5)
  )

c1 <- (p1 + p2+ p3) +
  plot_layout(ncol = 1, heights = c(1, 1, 1)) +
  plot_annotation(caption = NULL) &
  theme(
    plot.caption = element_text(
      hjust = 0.7,
      vjust = 1.8,
      size = 14,
      face = "bold"
    ),
    plot.margin = margin(5, 20, 5, 5)  # top, right, bottom, left
  )


c1


