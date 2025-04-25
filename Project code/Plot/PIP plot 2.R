var_names <- names(attitude)[-1]  

# Prior 2
#KM
pip_KM2 <- data.frame(
  Variable = rep(var_names, 2),
  PIP = c(pip_KM_f2, pip_KM_r2),
  Model = rep(c("Fixed", "Random"), each = length(var_names))
)

pip_KM2$Variable <- factor(pip_KM2$Variable, levels = rev(var_names))
#Plot
p4<-ggplot(pip_KM2, aes(x = PIP, y = Variable, linetype = Model)) +
  # P(I_j=1)
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "#F4A7B9", size = 1) +
  
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
  ggtitle("Priors 2") +
  theme_bw() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    plot.margin = margin(5, 20, 5, 5),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    
  ) +
  # label
  annotate(
    "text",
    x = Inf,                
    y = length(unique(pip_KM2$Variable)) / 2 + 0.5,  
    label = "KM",
    angle = 90,             
    hjust = 0.5,            
    vjust = 1.2,           
    color = "black",
    size = 5,
    fontface = "bold"
  ) +
  coord_cartesian(clip = "off")  


#GVS
pip_GVS2 <- data.frame(
  Variable = rep(var_names, 2),
  PIP = c(pip_GVS_f2, pip_GVS_r2),
  Model = rep(c("Fixed", "Random"), each = length(var_names))
)

pip_GVS2$Variable <- factor(pip_GVS2$Variable, levels = rev(var_names))
#Plot
p5<-ggplot(pip_GVS2, aes(x = PIP, y = Variable, linetype = Model)) +
  # P(I_j=1)
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "#F4A7B9", size = 1) +
  
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
  theme_bw() +
  theme(
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    plot.margin = margin(5, 20, 5, 5)
  ) +  
  annotate(
    "text",
    x = Inf,                
    y = length(unique(pip_KM2$Variable)) / 2 + 0.5,  
    label = "GVS",
    angle = 90,             
    hjust = 0.5,            
    vjust = 1.2,           
    color = "black",
    size = 5,
    fontface = "bold"
  ) +
  coord_cartesian(clip = "off")  

#SSVS
pip_SSVS2 <- data.frame(
  Variable = rep(var_names, 2),
  PIP = c(pip_SSVS_f2, pip_SSVS_r2),
  Model = rep(c("Fixed", "Random"), each = length(var_names))
)

pip_SSVS2$Variable <- factor(pip_SSVS2$Variable, levels = rev(var_names))
#Plot
p6<-ggplot(pip_SSVS2, aes(x = PIP, y = Variable, linetype = Model)) +
  
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "#F4A7B9", size = 1) +
  
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
    axis.ticks.x = element_line(color = "black"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_line(color = "black"),
    plot.margin = margin(5, 20, 5, 5)
  )  +
annotate(
  "text",
  x = Inf,                
  y = length(unique(pip_KM2$Variable)) / 2 + 0.5,  
  label = "SSVS",
  angle = 90,             
  hjust = 0.5,            
  vjust = 1.2,           
  color = "black",
  size = 5,
  fontface = "bold"
) +
  coord_cartesian(clip = "off")  




c2 <- (p4 + p5+ p6) +
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

c2

combined_plot <- (c1 | c2) +
  plot_layout(ncol = 2) +
  plot_annotation(caption = expression(Pr(I[j] == 1 ~ "|" ~ data))) &
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