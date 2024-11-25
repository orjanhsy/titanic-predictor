create_title_distribution_plot <- function(data, max_length = 10) {
  
  data$TruncatedTitle <- ifelse(
    nchar(data$Title) > max_length, 
    paste0(substr(data$Title, 1, max_length), "..."), 
    data$Title
  )

  ggplot(data, aes(x = TruncatedTitle)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(
      title = "Fordeling av titler",
      x = "Tittel", 
      y = "Antall"
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(color = "black", linewidth = 1),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    geom_text(
      stat = "count", 
      aes(label = after_stat(count)), 
      vjust = -0.5,  
      size = 3  
    )
}

plot_title_distribution <- function(data) {
  create_title_distribution_plot(data)
}