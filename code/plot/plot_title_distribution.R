library(ggplot2)

# Dependencies
source("code/wrangling/wrangling.R")

create_title_distribution_plot <- function(data) {
  plot <- ggplot(data, aes(x = Title)) +
    geom_bar(fill = "steelblue", color = "black") +
    theme_minimal() +
    labs(title = "Distribution of Titles", x = "Title", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
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