# Dependencies
dependencies <- c("tidyverse", "readr", "rsample", "tidymodels", "recipes", "glmnet", "ranger")
for (pkg in dependencies) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# file dependencies 
source("code/wrangling/wrangling.R")
source("code/models/model_data.R")
source("code/models/models.R")

main <- function() {
  # prepare data for models
  data <- wrangle_data()
  na_data <- wrangle_data(na = TRUE)
  
  model_data <- create_dummy_data(data)
  t_train <- model_data$t_train
  t_test <- model_data$t_test
  
  
  # -- Models --
  # LASSO
  lso_model <- lasso_model(t_train)
  print("Trained LASSO model:")
  print(summary(lso_model))


  # Random Forest 
  rf_model <- random_forest_model(t_train)
  print("Trained random forest model:")
  print(summary(rf_model))

  # Gradient Boosting Tree
  xgb_model <- xgboost_model(t_train)
  print("Trained Gradient Boosting Tree model:")
  print(summary(xgb_model))
  
  # predictions and accuracy
  lso_pred <- predict(lso_model, new_data = t_test, type = "class")$.pred_class
  rf_pred <- predict(rf_model, new_data = t_test, type = "class")$.pred_class
  xgb_pred <- predict(xgb_model, new_data = t_test, type = "class")$.pred_class
  
  lso_acc <- mean(lso_pred == t_test$Survived)
  rf_acc <- mean(rf_pred == t_test$Survived)
  xgb_acc <- mean(xgb_pred == t_test$Survived)
  
  accs <- tibble(
    Model = c("LASSO", "RANDOM FOREST", "XGBOOST"),
    Accuracy = c(lso_acc, rf_acc, xgb_acc)
  )
  
  print("Accuracies:") 
  print(accs)
  
  # Tuned models
  tuned_lasso <- create_tuned_model("random_forest", t_train)
}

main()
