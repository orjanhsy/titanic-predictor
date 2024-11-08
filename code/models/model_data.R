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
