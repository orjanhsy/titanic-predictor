# Dependencies
dependencies <- c("tidyverse", "readr", "rsample", "tidymodels", "recipes", "glmnet", "ranger", "tidyr")
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
source("code/plot/plot_missing_port.R")
source("code/plot/plot_title_distribution.R")

main <- function() {
  # prepare data for models
  path <- paste(getwd(), "/data/", "Titanic-Dataset.csv", sep = '')
  list_data <- wrangle_data(path = path)
  data <- list_data$data
  title_dist <- list_data$title_dist
  na_data <- wrangle_data(na = TRUE, path = path)
  
  
  # see average price for UI - Should make plot
  avg_price_per <- median_price_by_port_class(data)
  print(avg_price_per)
  
  #plot median fare for missing data
  median_fare_plot <- plot_median_fare(na_data)
  print(median_fare_plot)

  #plot title distribution
  title_distribution <- plot_title_distribution(title_dist)
  print(title_distribution)
  
  model_data <- create_dummy_data(data)
  
  t_train <- model_data$t_train
  t_test <- model_data$t_test
  
  view(t_train)
  

  # Models 
  lso_model <- lasso_model(t_train)
  rf_model <- random_forest_model(t_train)
  xgb_model <- xgboost_model(t_train)
  
  # Predictions 
  lso_pred <- predict(lso_model, new_data = t_test, type = "class")$.pred_class
  rf_pred <- predict(rf_model, new_data = t_test, type = "class")$.pred_class
  xgb_pred <- predict(xgb_model, new_data = t_test, type = "class")$.pred_class
  
  # Accuracies
  lso_acc <- mean(lso_pred == t_test$Survived)
  rf_acc <- mean(rf_pred == t_test$Survived)
  xgb_acc <- mean(xgb_pred == t_test$Survived)
  
  accs <- tibble(
    Model = c("LASSO", "RANDOM FOREST", "XGBOOST"),
    Accuracy = c(lso_acc, rf_acc, xgb_acc)
  )
  
  print("Accuracies of:") 
  print(accs)
  
  # Tuned models
  tuned_lso <- create_tuned_model("lasso", t_train) %>%
    fit(Survived ~., data = t_train)
  tuned_rf <- create_tuned_model("random_forest", t_train) %>%
    fit(Survived ~., data = t_train)
  tuned_xgb <- create_tuned_model("xgboost", t_train) %>%
    fit(Survived ~., data = t_train)
  print("Completed tuning!")
  
  # New predictions
  tuned_lso_pred <- predict(tuned_lso, new_data = t_test, type = "class")$.pred_class
  tuned_rf_pred <- predict(tuned_rf, new_data = t_test, type = "class")$.pred_class
  tuned_xgb_pred <- predict(tuned_xgb, new_data = t_test, type = "class")$.pred_class
  
  # Accuracies of tuned models
  tuned_lso_acc <- mean(tuned_lso_pred == t_test$Survived)
  tuned_rf_acc <- mean(tuned_rf_pred == t_test$Survived)
  tuned_xgb_acc <- mean(tuned_xgb_pred == t_test$Survived)
  
  new_accs <- tibble (
    Model = c("LASSO", "RANDOM FOREST", "XGBOOST"),
    untuned_acc = c(lso_acc, rf_acc, xgb_acc),
    tuned_acc = c(tuned_lso_acc, tuned_rf_acc, tuned_xgb_acc)
  )
  print("Accuracy of tuned models:")
  print(new_accs)
}

main()

