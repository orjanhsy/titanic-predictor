library(ggplot2)
library(dplyr)

create_cabin_stacked_plot <- function(data) {
  ggplot(data, aes(x = cabin_prefix, y = n, fill = factor(Pclass))) +
    geom_bar(stat = "identity") + 
    labs(
      title = "Kabin per Klasse",
      x = "Kabin bokstav",
      y = "Antall",
      fill = "Pclass"
    ) +
    scale_fill_manual(values = c("skyblue", "orange", "lightgreen")) +
    theme_minimal() +
    theme(
      plot.background = element_rect(color = "black", linewidth = 1),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold"),
    )
}

plot_cabin_dist <- function(data) {
  class_cabin_data <- data %>%
    select(Pclass, Cabin) %>%
    filter(!is.na(Cabin)) %>%
    mutate(cabin_prefix = substr(Cabin, 1, 1)) %>%
    count(Pclass, cabin_prefix) 
  
  create_cabin_stacked_plot(class_cabin_data)
}