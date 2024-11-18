create_dummy_data <- function(data) {
  dummy_data <- data %>%
    mutate(across(where(is.character), as.factor))
  
  split <- initial_split(dummy_data, prop = 0.8)
  t_train <- training(split)
  t_test <- testing(split)
  
  # Survived needs to be a factor in for the classification process.
  t_train <- t_train %>%
    mutate(Survived = as.factor(Survived))
  t_test <- t_test %>%
    mutate(Survived = as.factor(Survived))
  
  print(t_train)
  
  dummies <- recipe(Survived ~ ., data = t_train) %>%
    update_role(Survived, new_role = "outcome") %>%
    step_dummy(all_factor_predictors(), -all_outcomes()) %>%
    prep(training = t_train)
  
  dummy_train <- bake(dummies, new_data = t_train)
  dummy_test <- bake(dummies, new_data = t_test)
  
  return(list(t_train = dummy_train, t_test = dummy_test))
}
