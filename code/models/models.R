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
