# Dependencies
dependencies <- c("tidyverse", "readr", "rsample", "tidymodels", "recipes", "glmnet", "ranger", "tidyr", "vip")
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
  
  model_data <- create_dummy_data(data)
  
  t_train <- model_data$t_train
  t_test <- model_data$t_test
  
  # Tuned models
  tuned_lso <- create_tuned_model("lasso", t_train) %>%
    fit(Survived ~., data = t_train)
  tuned_rf <- create_tuned_model("random_forest", t_train) %>%
    fit(Survived ~., data = t_train)
  tuned_xgb <- create_tuned_model("xgboost", t_train) %>%
    fit(Survived ~., data = t_train)
  print("Completed tuning!")
  
  # Save the tuned models
  saveRDS(tuned_lso, "code/models/tuned_models/tuned_lasso_model.rds")
  saveRDS(tuned_rf, "code/models/tuned_models/tuned_random_forest_model.rds")
  saveRDS(tuned_xgb, "code/models/tuned_models/tuned_xgboost_model.rds")
  
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
    acc = c(tuned_lso_acc, tuned_rf_acc, tuned_xgb_acc)
  )
  print("Accuracy of tuned models:")
  print(new_accs)
  
  new_tuned_lso <- create_tuned_model("lasso", t_train)
  new_tuned_rf <- create_tuned_model("random_forest", t_train)
  
  num_feats <- length(t_train)
  lso_vip <- new_tuned_lso %>%
    set_engine("glmnet", importance = "permutation") %>%
    fit(Survived ~., data = t_train) %>%
    vip(geom = 'point', num_features = num_feats)
  print(lso_vip)
  
  rf_vip <- new_tuned_rf %>%
    set_engine("ranger", importance = "permutation") %>%
    fit(Survived ~., data = t_train) %>%
    vip(geom = 'point', num_features = num_feats)
  print(rf_vip)
  
  # Combined plot
  lso_vidat <- lso_vip$data
  rf_vidat <- lso_vip$data
  
  lso_vidat$model <- "LASSO"
  rf_vidat$model <- "Random Forest"
  
  vip_data <- bind_rows(lso_vidat, rf_vidat)
  
  # Ensure consistent levels for the variables
  vip_data <- vip_data %>%
    mutate(Variable = factor(Variable, levels = unique(Variable)))
  
  ggplot(vip_data, aes(x = reorder(Variable, Importance), y = Importance, color = model)) +
    geom_point(size = 3) +
    coord_flip() +
    labs(title = "Comparison of Variable Importance",
         x = "Features",
         y = "Importance",
         color = "Model") +
    theme_minimal()
}

main()

