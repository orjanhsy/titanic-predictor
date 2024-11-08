
# Dependencies
dependencies <- c("tidyverse", "readr", "rsample", "tidymodels", "recipes", "glmnet", "ranger")
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
create_median_fare_plot <- function(median_fares, avarage_NA){
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
  mean_fare_NA <- avarage_na_port(data)
  
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

plot_median_fare <- function(){
  median_fares <- get_median_fare_by_port(na_data)
  avarage_na <- avarage_na_port(na_data)
  plot <- plot_median_fare(median_fares, avarage_na)
  print(plot)
  
}

wrangle_data <- function(na = FALSE) {
  # Importing the data
  path <- here::here("data", "Titanic-Dataset.csv")
  data <- read_csv(path)
  
  if (na) return (data)
  
  data <- handle_na_age(data)
  data <- extract_title(data)
  data <- set_port(data)
  
  #test
  stopifnot(length(data)==length(data %>% filter(!is.na(Embarked))))
  
  # remove irrelevant variables
  data <- data %>% select(-Cabin, -Name, -PassengerId, -Ticket)
  
  return (data)
}

create_dummy_data <- function(data) {
  dummy_data <- data %>%
    mutate(across(where(is.character), as.factor))
  
  split <- initial_split(dummy_data, prop = 0.8)
  t_train <- training(split)
  t_test <- testing(split)
  
  print(t_train)
  
  dummies <- recipe(Survived ~ ., data = t_train) %>%
    step_dummy(all_factor_predictors()) %>%
    prep(training = t_train)
  
  dummy_train <- bake(dummies, new_data = t_train)
  dummy_test <- bake(dummies, new_data = t_test)
  
  return(list(t_train = dummy_train, t_test = dummy_test))
}

linear_regression_model <- function(t_train) {
  model <- linear_reg() %>%
    fit(Survived ~., data = t_train)
}

lasso_model <- function(t_train) {
  lso <- linear_reg(
    penalty = 0.1,
    mixture = 1
  ) %>%
    set_engine("glmnet") %>%
    fit(Survived ~., data = t_train)
}

random_forest_model <- function(t_train) {
  rf <- rand_forest(
    mode = "regression",
    engine = "ranger",
  ) %>%
    fit(Survived ~., data = t_train)
}

xgboost_model <- function(t_train){
  xgbm <- boost_tree() %>%
    set_engine("xgboost") %>%
    set_mode("regression") %>%
    fit(Survived ~., data = t_train)
}

main <- function() {
  # prepare data for models
  data <- wrangle_data()
  na_data <- wrangle_data(na = TRUE)
  
  model_data <- create_dummy_data(data)
  t_train <- model_data$t_train
  t_test <- model_data$t_test
  
  # -- Models --
  # OLS
  ols_model <- linear_regression_model(t_train)
  print("Trained OLS model:")
  print(summary(ols_model))
  print(alias(ols_model$fit))

  ols_pred <- predict(ols_model, new_data = t_test, rankdeficient = 'NA')
  
  # LASSO
  lso_model <- lasso_model(t_train)
  print("Trained LASSO model:")
  print(summary(lso_model))

  lso_pred <- predict(lso_model, new_data = t_test)

  # Random Forest 
  rf_model <- random_forest_model(t_train)
  print("Trained random forest model:")
  print(summary(rf_model))

  rf_pred <- predict(rf_model, new_data = t_test)
  
  # Gradient Boosting Tree
  xgb_model <- xgboost_model(t_train)
  print("Trained Gradient Boosting Tree model:")
  print(summary(xgb_model))
  
  xgb_pred <- predict(xgb_model, new_data = t_test)
}

main()
