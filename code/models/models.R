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

tune <- function(model, t_train, type) {
  spec <- switch(
    type,
    "lasso" = lasso_spec(tune())
  )
}

# -- Model specs, all parameters apart from trees are tunable. --

# ols cannot be tuned as it has no hyperparams, default engine of linear_reg is lm.
ols_spec <- function() {
  return (linear_reg())
}

lasso_spec <- function(penalty) {
  return (
    linear_reg(
      engine = "glmnet",
      mode = "classification",
      penalty = penalty,
      mixture = 1 # constant required for pure lasso
    ) 
  )
}

rf_spec <- function(mtry = NULL, trees = 500,  min_n = NULL) {
  return (
    rand_forest(
      mtry = mtry, 
      trees = trees,
      min_n = min_n,
      mode = "classification",
      engine = "ranger",
    )   
  )
}

xgb_spec <- function(trees = 500, depth = NULL, learn_rate = NULL, loss_reduction = NULL, min_n = NULL) {
  return (
    boost_tree(
      engine = "xgboost",
      mode = "classification", 
      tree_depth = depth,
      learn_rate = learn_rate,
      loss_reduction = loss_reduction,
      min_n = min_n
    )
  )
}







