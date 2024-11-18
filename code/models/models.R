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
  folds <- vfold_cv(t_train, v = 2) # insanely slow for high  repeats and v
  print("COMPLETED FOLDING")
  
  rec <- recipe(Survived ~., data = t_train) 
  print("COMPLETED RECIPE")
  
  spec <- switch(model_type, 
    "lasso" = lasso_spec(penalty = tune()),
    "random_forest" = rand_forest(
      mtry = tune(), 
      min_n = tune(),
      trees = 500,
    ) %>%
      set_mode("classification") %>%
      set_engine("ranger")
    ,
    "xgboost" = xgb_spec(depth = tune(), learn_rate = tune(), loss_reduction = tune(), min_n = tune())
  )
  print("FOUND SPEC")
  
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec) 
  print("CREATED WORKFLOW")
  print(wf)
  
  grid <- switch(model_type,
    "lasso" = grid_regular(
      penalty(), 
      levels = 5
    ), 
    "random_forest" = grid_regular(
      mtry(range = c(2, ncol(t_train) - 1)),
      min_n(range = c(2, 10)),
      levels = 5
    ),
    "xgboost" = grid_latin_hypercube(
      tree_depth(range = c(3, 10)),
      learn_rate(range = c(0.001, 0.1)),
      loss_reduction(range = c(0, 5)),
      min_n(range = c(2, 20)),
      size = 5 
    )
  )
  print("CREATED GRID")
  print(grid)
  
  print("Starting tuning process .. ")
  # metrics might have to be researched a bit as classification might not be a correct mode for our models
  tuned <- tune_grid(
    wf,
    resamples = folds,
    grid = grid
  )
  print("TUNED GRID")
  print(tuned)
  
  best <- select_best(tuned, metric = "accuracy")
  print("FOUND BEST")
  print(best)
  
  model <- finalize_workflow(wf, best)
  print("Finalized model") 
  print(model)
  
  return (model)
}

# -- Model specs, all parameters apart from trees are tunable. --
lasso_spec <- function(penalty) {
  return (
    logistic_reg(
      engine = "glmnet",
      mode = "classification",
      penalty = penalty,
      mixture = 1 # constant required for pure lasso
    ) 
  )
}

rf_spec <- function(mtry = NULL, min_n = NULL) {
  return (
    rand_forest(
      mtry = mtry, 
      min_n = min_n,
      trees = 500,
    ) %>%
      set_mode("classification") %>%
      set_engine("ranger")
  )
}

xgb_spec <- function(depth = NULL, learn_rate = NULL, loss_reduction = NULL, min_n = NULL) {
  return (
    boost_tree(
      engine = "xgboost",
      mode = "classification", 
      trees = 500,
      tree_depth = depth,
      learn_rate = learn_rate,
      loss_reduction = loss_reduction,
      min_n = min_n
    )
  )
}







