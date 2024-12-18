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

vip_lso_rf <- function(lso, rf, t_train) {
  lso <- lso %>%
    set_engine("glmnet", importance = 'permutation') %>%
    fit(Survived ~., data = t_train)
  
  rf <- rf %>%
    set_engine("ranger", importance = 'permutation') %>%
    fit(Survived ~., data = t_train)
  
  lso_vip <- lso %>% vip(geom = 'point', num_features = length(t_train))
  lso_dat <- lso_vip$data
 
  rf_vip <- rf %>% vip(geom = 'point', num_features = length(t_train))
  rf_dat <- rf_vip$data
  
  common_cols <- intersect(names(lso_dat), names(rf_dat))
  lso_dat <- lso_dat[, common_cols]
  rf_dat <- rf_dat[, common_cols]
  lso_dat <- lso_dat %>% mutate(model = "Lasso")
  rf_dat <- rf_dat %>% mutate(model = "Random Forest")
  
  total_importance_lso <- sum(lso_dat$Importance)
  total_importance_rf <- sum(rf_dat$Importance)
  
  lso_dat$Importance_percentage <- (lso_dat$Importance / total_importance_lso) * 100 
  rf_dat$Importance_percentage <- (rf_dat$Importance / total_importance_rf) * 100 
  
  combined_percent <- rbind(lso_dat, rf_dat)
  
  return (combined_percent)
}

vip_xgb <- function(xgb, t_train) {
  # xgb cannot set importance as a parameter in set_engine
  xgb <- xgb %>%
    fit(Survived ~., data = t_train)
  
  vip <- xgb %>% vip(geom = 'point', num_features = length(t_train))
  vip <- vip$data
  
  total_importance <- sum(vip$Importance)
  vip$Importance_percentage <- (vip$Importance / total_importance) * 100 
  
  return (vip)
}

plot_vip_lso_rf <- function(vip_data) {
  percentage_plot <- ggplot(vip_data, aes(x = reorder(Variable, Importance_percentage), y = Importance_percentage, fill = model)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(title = "Faktorinnvirkning Lasso og Random Forest",
         x = "Variabel",
         y = "Innvirkning (%)") +
    scale_fill_manual(values = c("Lasso" = "skyblue", "Random Forest" = "orange")) +
    theme_minimal() +
    theme(
      plot.background = element_rect(color = "black", linewidth = 1),  
      panel.background = element_rect(fill = "white"), 
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
      axis.title = element_text(face = "bold"),  
      axis.text = element_text(face = "bold")  
    )
  
  return(percentage_plot)
}

plot_vip_xgb <- function(vip_data) {
  xgb_percentage_plot <- ggplot(vip_data, aes(x = reorder(Variable, Importance_percentage), y = Importance_percentage)) +
    geom_col(fill = "skyblue") +
    geom_text(aes(label = round(Importance_percentage, 1)), hjust = -0.1, size = 3) +  
    coord_flip() +
    labs(title = "Faktorinnvirkning Gradient Boosting Tree",
         x = "Variabel",
         y = "Innvirkning (%)") +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(
      plot.background = element_rect(color = "black", linewidth = 1),  
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
      axis.title = element_text(face = "bold", size = 12),  
      axis.text = element_text(face = "bold", size = 10),  
      plot.margin = margin(10, 10, 10, 10)  
    )
  
  return(xgb_percentage_plot)
}