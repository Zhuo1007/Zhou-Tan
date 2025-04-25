result <- result_3 %>%
  group_by(group) %>%
  arrange(Probability, .by_group = TRUE) %>%
  mutate(model_number = 1:n()) 

ggplot(result, aes(x = model_number, y = Probability)) +
  geom_point(size = 1.2) +
  facet_wrap(~ group, scales = "free") +
  labs(x = "model #", y = "freq") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face= "bold", margin = margin(b=5)),
    axis.title = element_text(size = 12, face= "bold"),
    axis.text = element_text(size = 10, face= "bold"),
    strip.background = element_blank()
  )


visited_once <- result_3 %>% 
  filter(Frequency == 1)
