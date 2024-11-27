# base (untuned) models
lasso_model <- function(t_train) {
  lso <- logistic_reg(
    penalty = 0.1,
    mixture = 1
  ) %>%
    set_engine("glmnet") %>%
    set_mode("classification") %>%
    fit(Survived ~., data = t_train)
}

random_forest_model <- function(t_train) {
  rf <- rand_forest(
    engine = "ranger",
    mode = "classification",
  ) %>%
    fit(Survived ~., data = t_train)
}

xgboost_model <- function(t_train){
  xgbm <- boost_tree() %>%
    set_engine("xgboost") %>%
    set_mode("classification") %>%
    fit(Survived ~., data = t_train)
}

# return a specified (non fit) tuned model
create_tuned_model <- function(model_type, t_train) {
  folds <- vfold_cv(t_train, v = 10)
  
  rec <- recipe(Survived ~., data = t_train) 
  
  spec <- switch(model_type, 
    "lasso" = lso_spec(),
    "random_forest" = rf_spec(),
    "xgboost" = xgb_spec()
  )
  
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec) 
  
  grid <- switch(model_type,
    "lasso" = grid_regular(
      penalty(), 
      levels = 50
    ), 
    "random_forest" = grid_regular(
      mtry(range = c(2, ncol(t_train) - 1)),
      min_n(range = c(2, 10)),
      levels = 30
    ),
    "xgboost" = grid_space_filling(
      parameters(
        tree_depth(range = c(3, 10)),
        learn_rate(range = c(0.001, 0.1)),
        loss_reduction(range = c(0, 5)),
        min_n(range = c(2, 20))
      ),
      size = 50 
    )
  )
  
  tuned <- tune_grid(
    wf,
    resamples = folds,
    grid = grid
  )
  
  best <- select_best(tuned, metric = "accuracy")
  
  model <- finalize_model(spec, best)
  print(model)
  
  print(paste("Completed tuning", model_type))
  
  return (model)
}

# -- Model specs --
# Passing tune() as an argument to the functions does not work
# therefore they have no params
lso_spec <- function() {
  return (
    logistic_reg(
      engine = "glmnet",
      mode = "classification",
      penalty = tune(),
      mixture = 1 # constant required for pure lasso
    ) 
  )
}

rf_spec <- function() {
  return (
    rand_forest(
      mtry = tune(), 
      min_n = tune(),
      trees = 500,
    ) %>%
      set_mode("classification") %>%
      set_engine("ranger")
  )
}

xgb_spec <- function() {
  return (
    boost_tree(
      engine = "xgboost",
      mode = "classification", 
      trees = 500,
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      min_n = tune()
    )
  )
}







