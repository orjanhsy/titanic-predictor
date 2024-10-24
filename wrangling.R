
class_cabin_correlation <- function() {
  # Create tibble with all known Cabin and PClass
  class_cabin_data <- data %>% 
    select(Pclass, Cabin) %>%
    filter(!is.na(Cabin)) %>%
    mutate(cabin_prefix = substr(Cabin, 1, 1)) %>% 
    select(Pclass, cabin_prefix)
  
  view(class_cabin_data)
  
  # Count cabin prefix for each class
  cabin_counts <- class_cabin_data %>%
    group_by(Pclass, cabin_prefix) %>%
    count() %>%
    ungroup() 
  
  view(cabin_counts)
  
  # Plot to see correlation between Class and Cabin prefix
  plot <- ggplot(cabin_counts, aes(x = cabin_prefix, y = n, fill = factor(Pclass))) +
    geom_bar(stat = "identity", position = "dodge") +  # Use dodged bars for each class
    labs(x = "Cabin Prefix", y = "Count", title = "Count of Passengers by Cabin Prefix and Pclass") +
    theme_minimal()
  
  plot
}

