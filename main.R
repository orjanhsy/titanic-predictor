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
  # OLS 
  # TODO: RANK DEFICIENCY warning, needs to be understood -> handled.
  ols_model <- linear_regression_model(t_train)
  print("Trained OLS model:")
  print(summary(ols_model))
  print(alias(ols_model$fit))

  ols_pred <- predict(ols_model, new_data = t_test) %>% pull(.pred)
  
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
    OLS = Actual - ols_pred,
    LSO = Actual - lso_pred,
    RF = Actual - rf_pred,
    XGB = Actual - xgb_pred,
  ) 
      
  # mse
  mse_ols <- mean(errs$OLS^2)
  print(paste("MSE OLS: ", mse_ols))
  
  mse_lso <- mean(errs$LSO^2)
  print(paste("MSE LSO: ", mse_lso))
  
  mse_rf <- mean(errs$RF^2)
  print(paste("MSE RF: ", mse_rf))
  
  mse_xgb <- mean(errs$XGB^2)
  print(paste("MSE XGB: ", mse_xgb))
  
  acc_ols =  sum((errs$OLS > 0.499) == errs$Actual) / length(errs$Actual)
  acc_lso = sum((errs$LSO > 0.499) == errs$Actual) / length(errs$Actual)
  acc_rf = sum((errs$RF > 0.499) == errs$Actual) / length(errs$Actual)
  acc_xgb = sum((errs$XGB > 0.499) == errs$Actual) / length(errs$Actual)
  
  accs <- tibble(
    osl = acc_ols,
    lso = acc_lso,
    rf = acc_rf,
    xgb = acc_xgb
  )
  print(accs)
  
}

main()
