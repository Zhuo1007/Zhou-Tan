values <- c(total_runs_GVS_f1, total_runs_GVS_p_f1,
           total_runs_GVS_r1, total_runs_GVS_p_r1,
           total_runs_GVS_f2, total_runs_GVS_p_f2,
           total_runs_GVS_r2, total_runs_GVS_p_r2)

df <- data.frame(
  Variable = rep(c("f1", "r1", "f2", "r2"), each = 2),
  Setting = rep(c("automatic pseudo prior", "tuning pseudo prior"), times = 4),
  TotalRun = c(
    total_runs_GVS_f1, total_runs_GVS_p_f1,
    total_runs_GVS_r1, total_runs_GVS_p_r1,
    total_runs_GVS_f2, total_runs_GVS_p_f2,
    total_runs_GVS_r2, total_runs_GVS_p_r2
  )
)

df$Color <- rep(c("black", "red", "grey40", "pink"), each = 2)

df$LineType <- ifelse(grepl("f", df$Variable), "solid", "dashed")
df$Setting <- factor(df$Setting, levels = c("automatic pseudo prior", "tuning pseudo prior"))
levels(df$Setting) <- c("automatic\npseudo prior", "tuning\npseudo prior")

ggplot(df, aes(x = Setting, y = TotalRun, group = Variable)) +
  geom_point(aes(color = Color), size = 3) +
  geom_line(aes(color = Color, linetype = LineType),linewidth = 1.0) +
  scale_color_identity() +
  scale_linetype_identity() +
  scale_x_discrete(expand = expansion(mult = c(0.25, 0.35))) +  # 缩小左右空白
  ylab("Total Number of Runs") +
  xlab(NULL) +
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10))
  )
