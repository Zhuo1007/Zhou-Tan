# Standardization(make mean = 1)
all_times <- bind_rows(kmF_times, kmR_times, gvsF_times, gvsR_times, ssvsF_times, ssvsR_times) %>%
  mutate(Time_std = Time / mean(Time))


# Calculate the standardized time of each method
mean_trend <- all_times %>%
  group_by(Method) %>%
  summarise(mean_time = mean(Time_std, na.rm = TRUE))

all_times$Method <- factor(all_times$Method, levels = c("KM, Fixed", "KM, Random", "GVS, Fixed",  "GVS, Random", "SSVS, Fixed", "SSVS, Random"))
mean_trend$Method <- factor(mean_trend$Method, levels = levels(all_times$Method))

connect_order <- c("SSVS, Random", "SSVS, Fixed", 
                   "GVS, Random", "GVS, Fixed", 
                   "KM, Random", "KM, Fixed")

mean_trend <- mean_trend %>%
  slice(match(connect_order, Method))

# Plot
ggplot(all_times, aes(x = Time_std , y = Method)) +
  geom_path(data = mean_trend, aes(x = mean_time , y = Method, group = 1),
            color = "magenta", size = 1) +
  geom_point(data = mean_trend, aes(x = mean_time , y = Method),
             color = "magenta", size = 2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  xlab("Mean Standardised Time") +
  ylab(NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,face = "bold"),
    text = element_text(size = 13, ,face = "bold")
  )