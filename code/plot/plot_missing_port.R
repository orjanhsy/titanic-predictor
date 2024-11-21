library(ggplot2)
# Dependencies
source("code/wrangling/wrangling.R")

#plotting for visualization
create_median_fare_plot <- function(median_fares, avarage_NA){
  ggplot(median_fares, aes(x = Embarked, y = median_fare)) +
    geom_hline(yintercept = avarage_NA, color = "red", linetype = "dashed", size = 1) +
    geom_col(fill = "skyblue", color = "black") +
    labs(title = "Median bilettpris for avreisedestinasjon",
         x = "Avreisedestinasjon",
         y = "Median Bilettpris") +
    theme_minimal() +
    theme(
      plot.background = element_rect(color = "black", linewidth = 1),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold")
    )
}

plot_median_fare <- function(na_data){
  median_fares <- get_median_fare_by_port(na_data)
  avarage_na <- avarage_na_port(na_data)
  create_median_fare_plot(median_fares, avarage_na)
}
