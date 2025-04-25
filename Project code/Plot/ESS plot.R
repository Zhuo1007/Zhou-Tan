all_eff <- bind_rows(eff_alpha_KMf2/2, eff_alpha_KMr2/2, 
                     eff_alpha_GVSf2/2, eff_alpha_GVSr2/2, 
                     eff_alpha_SSVSf2/2, eff_alpha_SSVSr2/2)


methods<-c("KM, Fixed", "KM, Random", "GVS, Fixed",  "GVS, Random", "SSVS, Fixed", "SSVS, Random")

all_eff$Method <- factor(methods, levels = methods)

#ESS plot
ggplot(all_eff, aes(x = var1 , y = Method,group=1)) +
  geom_path(data = all_eff, aes(x = var1 , y = Method, group = 1),
            color = "magenta", size = 1) +
  geom_point(data = all_eff, aes(x = var1 , y = Method),
             color = "magenta", size = 2) +
  xlab("Effective Number of Samples for α") +
  ylab(NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,face = "bold"),
    text = element_text(size = 13, ,face = "bold")
  )


