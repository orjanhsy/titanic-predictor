class_cabin_data <- data %>% 
  select(Pclass, Cabin) %>%
  filter(!is.na(Cabin)) %>%
  mutate(cabin_prefix = substr(Cabin, 1, 1)) %>% 
  select(Pclass, cabin_prefix)

cabin_counts_one <- class_cabin_data %>%
  filter(Pclass == "1") %>% 
  count(cabin_prefix)
glimpse(cabin_counts_one)

cabin_counts_two <- class_cabin_data %>%
  filter(Pclass == "2") %>% 
  count(cabin_prefix)
glimpse(cabin_counts_two)

cabin_counts_three <- class_cabin_data %>%
  filter(Pclass == "3") %>% 
  count(cabin_prefix)
glimpse(cabin_counts_three)

#plot <- ggplot(class_cabin_data, aes(x = cabin_prefix, y = ))

View(class_cabin_data)