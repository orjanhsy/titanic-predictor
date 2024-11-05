
# Dependencies
dependencies <- c("tidyverse", "readr")
for (pkg in dependencies) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


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







