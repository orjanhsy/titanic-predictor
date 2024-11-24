
library(ggplot2)

# Dependencies
dependencies <- c("tidyverse", "readr", "rsample", "tidymodels", "recipes", "glmnet", "ranger", "tidyr", "vip")
for (pkg in dependencies) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

source("code/wrangling/wrangling.R")
source("code/models/model_data.R")
source("code/models/models.R")

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
tuned_lso <- create_tuned_model("lasso", t_train)
tuned_rf <- create_tuned_model("random_forest", t_train) 
tuned_xgb <- create_tuned_model("xgboost", t_train) 
print("Completed tuning!")


lso <- tuned_lso %>%
  set_engine("glmnet", importance = "permutation") %>%
  fit(Survived ~., data = t_train) 

rf <- tuned_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Survived ~., data = t_train)

lso_vip <- lso %>% vip(geom = 'point', num_features = length(t_train))
rf_vip <- rf %>% vip(geom = 'point', num_features = length(t_train))

lso_dat <- lso_vip$data
rf_dat <- rf_vip$data

common_cols <- intersect(names(lso_dat), names(rf_dat)) # Add xgb_vip_data if needed
lso_dat <- lso_dat[, common_cols]
rf_dat <- rf_dat[, common_cols]
lso_dat <- lso_dat %>% mutate(model = "Lasso")
rf_dat <- rf_dat %>% mutate(model = "Random Forest")

combined_vip <- rbind(lso_dat, rf_dat)

pl <- ggplot(combined_vip, aes(x = reorder(Variable, Importance), y = Importance, fill = model)) +
  geom_col(position = "stack") +  # Use "dodge" to place bars side-by-side
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Variable Importance Plots (Lasso & Random Forest)",
       x = "Variable",
       y = "Importance") +
  theme_bw() +
  theme(legend.position = "bottom") # Adjust legend position as needed.

print(pl)

total_importance_lso <- sum(lso_dat$Importance)
total_importance_rf <- sum(rf_dat$Importance)
lso_dat$Importance_percentage <- (lso_dat$Importance / total_importance_lso) * 100 
rf_dat$Importance_percentage <- (rf_dat$Importance / total_importance_rf) * 100 

combined_percent <- rbind(lso_dat, rf_dat)

percentage_plot <- ggplot(combined_percent, aes(x = reorder(Variable, Importance_percentage), y = Importance_percentage, fill = model)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Variable Importance as Percentage of Total Importance",
       x = "Variable",
       y = "Importance (%)") +
  theme_bw() +
  theme(legend.position = "bottom")

print(precentage_plot)