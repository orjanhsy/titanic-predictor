library(ggplot2)

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
# The function assumes people with the same last name are siblings.
# In cases where children/parents, or unrelated people with the same last name are present
# this will result in a less accurate estimate of the persons age.
getSibSpAge <- function(name, data) {
  last_name <- get_last_name(name)
  
  sib_sp <- filter(group_by(data, Name), get_last_name(Name) == last_name)
  
  estimated_age <- round(mean(sib_sp$Age, na.rm = TRUE))
  return (estimated_age)
}

# gets last
get_last_name <- function(name) {
  last_name <- strsplit(name, ',')[[1]][1]
  return (last_name)
}

#Check for visualization
check_titles <- function(data){
  data <- data %>%
    mutate(Title = sub(".*,\\s*(\\w+)\\..*", "\\1", Name))
  return(data)
}
#Extract title and adds a new "Title" column
get_titles <- function(data){
  data <- data %>%
    mutate(Title = sub(".*,\\s*(\\w+)\\..*", "\\1", Name))
  
  # Count occurrences of each Title
  title_counts <- data %>%
    group_by(Title) %>%
    tally() %>%
    filter(n <= 10) %>%
    pull(Title)
  
  # Replace titles with 10 or fewer occurrences with "Other"
  data <- data %>%
    mutate(Title = ifelse(Title %in% title_counts, "Other", Title))
  
  return(data)
}

# Plot title distribution
plot_title_distribution <- function(data) {
  # Plot the distribution of titles
  plot <- ggplot(data, aes(x = Title)) +
    geom_bar(fill = "steelblue", color = "black") +
    theme_minimal() +
    labs(title = "Distribution of Titles", x = "Title", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
    geom_text(
      stat = "count", 
      aes(label = ..count..), 
      vjust = -0.5,  # Adjust vertical position of the labels
      size = 3  # Adjust label size
    )
  
  # Print the plot
  print(plot)
}


wrangle_data <- function(na = FALSE, path) {
  # Importing the data
  data <- read_csv(path)
  view(data)
  
  if (na) return (data)
  
  check <- check_titles(data)
  plot_title_distribution(check)
  
  data <- handle_na_age(data)
  data <- get_titles(data)
  data <- set_port(data)
  
  #test
  stopifnot(length(data)==length(data %>% filter(!is.na(Embarked))))
  
  # remove irrelevant variables
  data <- data %>% select(-Cabin, -Name, -PassengerId, -Ticket)
  
  return (data)
}



