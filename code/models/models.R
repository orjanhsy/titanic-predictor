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
  # TODO: folds as it stands is only used here (for tuning hyperparams).
  # If k-folds is helpful outside of tuning we move it out of this function
  # and pass it as an argument to this function instead (the one currently named t_train).
  folds <- vfold_cv(t_train, v = 10, repeats = 2)
  
  rec <- recipe(Survived ~., data = t_train)
  
  spec <- switch(model_type,
    "lasso" = lasso_spec(tune()),
    "random_forest" = rf_spec(mtry = tune(), min_n = tune()),
    "xgboost" = xgb_spec(depth = tune(), learn_rate = tune(), loss_reduction = tune(), min_n = tune())
  )
  
  wf <- workflow() %>%
    add_model(spec) %>%
    add_recipe(rec)
  
  grid <- switch(model_type,
    "lasso" = grid_regular(penalty(), levels = 50), # similar to lasso.R in machine learning II files
    "random_forest" = grid_regular(
      mtry(range = c(2, ncol(t_train) - 1)),
      min_n(range = c(2, 10)),
      levels = 50
    ),
    "xgboost" = grid_latin_hypercube(
      tree_depth(range = c(3, 10)),
      learn_rate(range = c(-2, -0.1)),
      loss_reduction(range = c(0, 5)),
      min_n(range = c(2, 20)),
      size = 50 
    )
  )
  
  # metrics might have to be researched a bit as classification might not be a correct mode for our models
  tuned <- tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metric_set(accuracy, roc_auc, f_meas) # needs to be changed
  )
  
  best <- select_best(tuned, "rmse")
  return (finalize_workflow(wf, best))
}

# -- Model specs, all parameters apart from trees are tunable. --
lasso_spec <- function(penalty) {
  return (
    linear_reg(
      engine = "glmnet",
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







