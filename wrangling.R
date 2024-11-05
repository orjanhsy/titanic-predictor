
# Dependencies
dependencies <- c("tidyverse", "readr")
for (pkg in dependencies) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


prep_data <- function(na = FALSE) {
  # Importing the data
  path <- here::here("data", "Titanic-Dataset.csv")
  data <- read_csv(path)
  if (na) {
    return (data)
  }
  
  data <- handle_na_age(data)
  view(data)
  return (data)
}

handle_na_age <- function(data) {
  median_age <- median(data$Age, na.rm = TRUE)
  # print(median_age)
  data %>%
    mutate(Age = ifelse(is.na(Age), median_age, Age))
}

print(handle_na_age(prep_data()))