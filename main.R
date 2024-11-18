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

  lso_pred <- predict(lso_model, new_data = t_test) %>% pull(.pred)

  # Random Forest 
  rf_model <- random_forest_model(t_train)
  print("Trained random forest model:")
  print(summary(rf_model))

  rf_pred <- predict(rf_model, new_data = t_test) %>% pull(.pred)
  
  # Gradient Boosting Tree
  xgb_model <- xgboost_model(t_train)
  print("Trained Gradient Boosting Tree model:")
  print(summary(xgb_model))
  
  xgb_pred <- predict(xgb_model, new_data = t_test) %>% pull(.pred)
  
  errs <- tibble(
    Actual = t_test$Survived,
    LSO_errors = Actual - lso_pred,
    RF_errors = Actual - rf_pred,
    XGB_errors = Actual - xgb_pred,
  ) 
      
  # mse
  mse_lso <- mean(errs$LSO_errors^2)
  print(paste("MSE LSO: ", mse_lso))
  
  mse_rf <- mean(errs$RF_errors^2)
  print(paste("MSE RF: ", mse_rf))
  
  mse_xgb <- mean(errs$XGB_errors^2)
  print(paste("MSE XGB: ", mse_xgb))
  
  accs <- errs %>%
    summarize(
      acc_ols = sum((ols_pred > 0.499) == Actual) / length(Actual),
      acc_lso = sum((lso_pred > 0.499) == Actual) / length(Actual),
      acc_rf = sum((rf_pred > 0.499) == Actual) / length(Actual),
      acc_xgb = sum((xgb_pred > 0.499) == Actual) / length(Actual),
    ) %>%
    select(starts_with("acc")) 

  print(accs)
  
  plot <- errs %>%
    pivot_longer(cols = ends_with("errors"), names_to = "Model", values_to = "Errors") %>%
    ggplot(aes(x = Errors, fill = Model)
    ) +
    geom_density(alpha = 0.5) +
    theme_minimal()
  
  print(plot)
  
  tuned_lasso <- create_tuned_model("lasso", t_train)
  print(paste("Tuned lasso model: ", summary(tuned_lasso)))
}

main()
