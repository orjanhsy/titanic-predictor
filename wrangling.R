
# Dependencies
dependencies <- c("tidyverse", "readr")
for (pkg in dependencies) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

get_median_fare_by_port <- function(data){
  data %>%
    filter(Pclass == 1) %>%
    group_by(Embarked)
}

plot_median_fare <- function(data){
  ggplot(data, aes(x = Embarked, y = median_fare)) +
    geom_hline(yintercept = 80, color = "red", linetype = "dashed") +
    geom_boxplot(
      fill = "skyblue", 
      outliers = TRUE,
      notch = TRUE,
      ) +
    labs(title = "Median Fare by Port of Embarkation for First-Class Passengers",
         x = "Port of Embarkation",
         y = "Median Fare") +
    theme_few()
}
median_fares <- get_median_fare_by_port(data)
plot_median_fare(median_fares)

handle_na_age <- function(data) {
  median_age <- median(data$Age, na.rm = TRUE)
  data <- data %>%
    mutate(Age = ifelse(is.na(Age), median_age, Age))
  return(data)
}

#function that extracts title and adds a new "Title" column
extract_title <- function(data){
  data <- data %>%
    mutate(Title = sub(".*,\\s*(\\w+)\\..*", "\\1", Name))
  return(data)
}

prep_data <- function(na = FALSE) {
  # Importing the data
  path <- here::here("data", "Titanic-Dataset.csv")
  data <- read_csv(path)
  if (na) return (data)
  
  data <- handle_na_age(data)
  data <- extract_title(data)
  view(data)
  return (data)
}


data <- prep_data()
print(data)







