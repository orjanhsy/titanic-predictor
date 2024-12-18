#Average cost for ticket by port and class. Need for UI
median_price_by_port_class <- function(data) {
  avg_fare_data <- data %>%
    filter(!is.na(Embarked), !is.na(Pclass)) %>%
    group_by(Embarked, Pclass) %>%
    summarise(average_fare = median(Fare, na.rm = TRUE), .groups = "drop")
  
  return(avg_fare_data)
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

#set NA Embarked to closest median
set_port <- function(data){
  median_fares_for_port <- get_median_fare_by_port(data)
  mean_fare_NA <- avarage_na_port(data)
  
  closest_port <- median_fares_for_port %>%
    mutate(diff = abs(median_fare - mean_fare_NA)) %>%
    filter(diff == min(diff)) %>%
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
# The function assumes people with the same surname are siblings.
# In cases where children/parents, or unrelated people with the same surname are present
# this will result in a less accurate estimate of the persons age.
getSibSpAge <- function(name, data) {
  surname <- get_surname(name)
  
  sib_sp <- filter(group_by(data, Name), get_surname(Name) == surname)
  
  estimated_age <- round(mean(sib_sp$Age, na.rm = TRUE))
  return (estimated_age)
}

get_surname <- function(name) {
  surname <- strsplit(name, ',')[[1]][1]
  return (surname)
}

check_titles <- function(data){
  data <- data %>%
    mutate(Title = sub(".*,\\s*(\\w+)\\..*", "\\1", Name))
  return(data)
}

get_titles <- function(data){
  data <- data %>%
    mutate(Title = sub(".*,\\s*(\\w+)\\..*", "\\1", Name))
  
  title_counts <- data %>%
    group_by(Title) %>%
    tally() %>%
    filter(n <= 10) %>%
    pull(Title)

  data <- data %>%
    mutate(Title = ifelse(Title %in% title_counts, "Other", Title))
  
  return(data)
}


wrangle_data <- function(na = FALSE, path) {
  # Importing the data
  data <- read_csv(path)
  # in cases we need unwrangled data
  if (na) return (data)
  
  check <- check_titles(data)
  
  data <- handle_na_age(data)
  data <- get_titles(data)
  data <- set_port(data)
  
  #test
  stopifnot(length(data)==length(data %>% filter(!is.na(Embarked))))
  
  # remove irrelevant variables
  data <- data %>% select(-Cabin, -Name, -PassengerId, -Ticket)
  
  return (list(data = data, title_dist = check))
}



