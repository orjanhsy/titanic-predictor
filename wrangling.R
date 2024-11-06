
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
  data <- handle_na_age(data)
  data <- extract_title(data)
  return (data)
}

data <- prep_data()
view(data)






