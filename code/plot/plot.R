# Dependencies
source("code/wrangling/wrangling.R")

#plotting for visualization
create_median_fare_plot <- function(median_fares, avarage_NA){
  ggplot(median_fares, aes(x = Embarked, y = median_fare)) +
    geom_hline(yintercept = avarage_NA, color = "red", linetype = "dashed", size = 1) +
    geom_col(fill = "skyblue") +
    labs(title = "Median bilettpris for avreisedestinasjon",
         x = "Avreisedestinasjon",
         y = "Median Bilettpris") +
    theme_minimal()
}

plot_median_fare <- function(){
  median_fares <- get_median_fare_by_port(na_data)
  avarage_na <- avarage_na_port(na_data)
  plot <- create_median_fare_plot(median_fares, avarage_na)
  print(plot)
}

run_plots <- function() {
  plot_median_fare()
}

run_plots()