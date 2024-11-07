
# Dependencies
dependencies <- c("tidyverse", "readr")
for (pkg in dependencies) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

#get avarage price for NA port
avarage_na_port <- function(data){
  filtered_NA <- data %>%
    filter(Pclass == 1, is.na(Embarked), na.rm = TRUE)
  mean_fare <- mean(filtered_NA$Fare)
  return(mean_fare)
}

get_median_fare_by_port <- function(data){
  data %>%
    filter(Pclass == 1, !is.na(Embarked)) %>%
    group_by(Embarked) %>%
    summarise(median_fare = median(Fare))
}
#plotting for visualization
plot_median_fare <- function(median_fares, avarage_NA){
  ggplot(median_fares, aes(x = Embarked, y = median_fare)) +
    geom_hline(yintercept = avarage_NA, color = "red", linetype = "dashed", size = 1) +
    geom_col(fill = "skyblue") +
    labs(title = "Median bilettpris for avreisedestinasjon",
         x = "Avreisedestinasjon",
         y = "Median Bilettpris") +
    theme_minimal()
}

#set NA Embarked to closest median
set_port <- function(data){
  median_fares_for_port <- get_median_fare_by_port(data)
  mean_fare_NA <- avarage_NA_port(data)
  
  closest_port <- median_fares_for_port %>%
    mutate(diff = abs(median_fare - mean_fare_NA)) %>%
    slice(which.min(diff)) %>%
    pull(Embarked)
    
  data <- data %>%
    mutate(Embarked = ifelse(is.na(Embarked), closest_port, Embarked ))
  return(data)
}

handle_na_age <- function(data) {
  median_age <- median(data$Age, na.rm = TRUE)
  data <- data %>%
    mutate(Age = ifelse(is.na(Age), ifelse((SibSp > 0) & (Parch == 0), getSibSpAge(Name, data), median_age), Age))
  return(data)
}

# gets the mean age of sibling(s) and/or spouse(s?), helpful in cases where age is N/A. 
# The function assumes people with the same last name are siblings.
# In cases where children/parents, or unrelated people with the same last name are present
# this will result in a less accurate estimate of the persons age.
getSibSpAge <- function(name, data) {
  last_name <- get_last_name(name)
  # print(paste("Getting siblings for last name:", last_name))
  
  sib_sp <- filter(group_by(data, Name), get_last_name(Name) == last_name)
  print(sib_sp)
  
  estimated_age <- round(mean(sib_sp$Age, na.rm = TRUE))
  return (estimated_age)
}

get_last_name <- function(name) {
  last_name <- strsplit(name, ',')[[1]][1]
  return (last_name)
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
  
  median_fares <- get_median_fare_by_port(data)
  avarage_na <- avarage_na_port(data)
  plot <- plot_median_fare(median_fares, avarage_na)
  print(plot)
  
  
  data <- handle_na_age(data)
  data <- extract_title(data)
  data <- set_port(data)
  
  #test
  stopifnot(length(data)==length(data %>% filter(!is.na(Embarked))))
  
  return (data)
}

data <- prep_data()
glimpse(data)

