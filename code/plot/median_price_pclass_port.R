library(ggplot2)

create_median_price_plot <- function(data){
  
  data$Embarked <- factor(data$Embarked, 
                          levels = c("C", "Q", "S"), 
                          labels = c("Cherbourg", "Queenstown", "Southampton"))
  
  ggplot(data, aes(x = Embarked, y = average_fare, fill = factor(Pclass))) +
    geom_col(position = "dodge", color = "black") +
    labs(
      title = "Median pris for avreisehavn og klasse",
      x = "Avreisehavn",
      y = "Median pris",
      fill = "Klasse"
    ) +
    scale_fill_manual(values = c("skyblue", "orange", "lightgreen")) +
    theme_minimal() +
    theme(
      plot.background = element_rect(color = "black", linewidth = 1),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold")
    )
  
}

plot_median_price <- function(data){
  create_median_price_plot(data)
}