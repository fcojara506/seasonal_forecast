# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/GitHub/seasonal_forecast")

# Load required packages
library(lubridate)
library(dplyr)
library(caret)


# # Source required scripts
source("base/Preprocess_data.R")
# source("base/Regression_model.R")

# ----------------------
# Functions
# ----------------------

# Function for removing correlated predictors
remove_correlated_predictors <- function(X_train, predictor_list, cutoff = 0.8) {
  cor_matrix <- cor(X_train)
  high_cor <- caret::findCorrelation(cor_matrix, cutoff = cutoff, verbose = FALSE)
  
  if (rlang::is_empty(high_cor)) {
    return(predictor_list)
  } else {
    return(predictor_list[-high_cor])
  }
}


# Function for calculating model metrics
calculate_model_metrics <- function(regression_model) {
  library(locfit)
  library(caret)
  mlrmod <- regression_model$finalModel
  pr <- resid(mlrmod) / (1 - lm.influence(mlrmod)$hat)
  
  pred <- mlrmod$fitted.values
  obs <- regression_model$pred$obs
  
  # Calculate model metrics using caret
  rmse <- RMSE(pred, obs)
  r2 <- R2(pred, obs)
  mae <- MAE(pred, obs)
  pbias <- abs(mean((obs - pred) / obs))
  
  metrics <- list(
    gcv = unlist(gcv(mlrmod, maxk = 1e6)[[4]]),
    bic = BIC(mlrmod),
    press = sum(pr^2),
    aic = AIC(mlrmod),
    rmse = rmse,
    r2 = r2,
    mae = mae,
    pbias = pbias
  )
  
  return(metrics)
}




# Function for saving model results
save_model_results <- function(df_info,
                               predictor,
                               regression_model,
                               imp_var,
                               metrics) {
  result <- list(
    df_info = df_info,
    predictor = convert_items_to_lists(predictor),
    reg_model = regression_model,
    imp_var = (imp_var),
    metrics = (metrics)
  )
  
  
  return((result))
}

# ------------------------------
# Main code for training models
# ------------------------------

# Set catchment code and months of initialization
catchment_code <- "4503001"#"5410002" #6028001 #"5410002" #"4503001" #"3414001"
months_initialisation <- 5:9

# Define predictors
a <- grid_pred(c("SOI", "PDO", "ONI","NINO1.2"), 1, "mean")
b <- grid_pred(c("STORAGE"), 1, "last")
predictors <- c(a, b)

library(foreach)
library(doSNOW)
cl <- makeCluster(parallel::detectCores()-1L)
registerDoSNOW(cl)

data_input <- preprocess_data(
  datetime_initialisation = lubridate::make_date(2022, 5),
  forecast_mode = "cv",
  catchment_code = catchment_code,
  predictor_list = predictors[[5]],save_raw = T
)

# Train regression models for each combination of predictors and month of initialization
models <-
foreach(month_initialisation=months_initialisation,.combine = "c") %do% { 
  #for (month_initialisation in months_initialisation[1]) {
    
  
  library(foreach)
  library(caret)
  # Source required scripts
  source("base/Preprocess_data.R")

  
  # Preprocess input data
  data_input <- preprocess_data(
    datetime_initialisation = lubridate::make_date(2022, month_initialisation),
    forecast_mode = "cv",
    catchment_code = catchment_code,
    predictor_list = predictors
  )
  predictor_list <- data_input$info$predictor_list[[1]]
  
  # Remove correlated predictors
  predictors_uncorrelated <- remove_correlated_predictors(data_input$X_train, predictor_list)
  
  # Generate all possible combinations of predictors
  predictors_comb <- unlist(lapply(seq_along(predictors_uncorrelated), function(i)
    combn(predictors_uncorrelated, i, simplify = FALSE)), recursive = FALSE)
  
  #Iterate through each predictor combination
  #for (predictor in predictors_comb) {
    foreach(predictor=predictors_comb ) %do% {
      #for (predictor in predictors_comb[31]) {
     # Preprocess input data
      x = select(data_input$X_train,all_of(predictor)) 
      
    #   data_input <- preprocess_data(
    #     datetime_initialisation = lubridate::make_date(2022, month_initialisation),
    #     forecast_mode = "cv",
    #     catchment_code = catchment_code,
    #     predictor_list = predictor
    #   )
    #   
    # x = data_input$X_train
    y = data_input$y_train$volume
    # Train regression model
    regression_model <- 
    train(
      x,
      y,
      metric = "RMSE",
      trControl = trainControl(method = "LOOCV",savePredictions = "all"),
      method = "lm",
      preProcess = c("center", "scale")
    )
    # Calculate model metrics
    metrics <- calculate_model_metrics(regression_model)

    # Calculate variable importance if there are multiple predictors
    imp_var <- if (length(predictor) > 1) relaimpo::calc.relimp(regression_model$finalModel, rela = TRUE)$lmg else setNames(1, predictor)
  
    # Save results
    #models[[length(models) + 1]] 
    model <- save_model_results(data.frame(month_initialisation,catchment_code),
                                                       list(predictor = predictor),
                                                       regression_model,
                                                       data.frame(t(imp_var)),
                                                       metrics)
    
   }
}

stopCluster(cl)
  
# ------------------------------
#   Post-process the results
# ------------------------------
select_best_models <- function(models) {
  
model_list <- purrr::transpose(models)
predictor_list =  rbindlist(model_list$predictor)


summary_df <- cbind( rbindlist(model_list$df_info),
            rbindlist(model_list$metrics),
            predictor_list,
            rbindlist(model_list$imp_var,fill = T),
            model = model_list$reg_model
            )
best = summary_df %>% 
  group_by(month_initialisation,catchment_code) %>% 
  slice(which.min(aic))

return(list(best_results = best,
            unique_predictors = unique(unlist(predictor_list)),
            summary_df = summary_df))
}

best = select_best_models(models)


library(tidyverse)
# Create a new data frame for the plot

data_importance <- best$best_results %>%
  select(month_initialisation, catchment_code,matches(best$unique_predictors)) %>%
  tidyr::pivot_longer(cols = -c(month_initialisation, catchment_code), names_to = "predictor", values_to = "importance") %>%
  filter(!is.na(importance)) %>% 
  group_by(month_initialisation, catchment_code) %>%
  mutate(total_importance = sum(importance),
         percentage = importance / total_importance * 100) %>%
  ungroup() %>% 
  mutate(var = tstrsplit(predictor, "_", fixed = TRUE)[[1]]) %>% 
  arrange(month_initialisation) %>%
  mutate(date_label = paste0("1˚ ", lubridate::month(lubridate::make_date(year = 2001, month = month_initialisation), label = TRUE)),
         date_label = factor(date_label, levels = unique(date_label)))


# Load the RColorBrewer package

# Create a custom color palette
unique_vars <- unique(data_importancia$var)
palette_size <- length(unique_vars)

# Create the custom_colors vector, excluding the color for the "STORAGE" predictor
custom_colors <- c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00")

# Match the number of colors in the custom_colors vector with the number of unique variables (excluding "STORAGE")
palette_size <- palette_size - 1

if (length(custom_colors) > palette_size) {
  custom_colors <- custom_colors[1:palette_size]
} else if (length(custom_colors) < palette_size) {
  custom_colors <- rep(custom_colors, length.out = palette_size)
}

# Add the color for the "STORAGE" predictor and create the color_palette
color_palette <- c(setNames(custom_colors, unique_vars[unique_vars != "STORAGE"]),
                   "STORAGE" = "#ff6d5c")


# Create the ggplot with updated x-axis labels
ggplot(data_importancia, aes(x = date_label, y = importance, fill = var)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Mes de emisión",
       y = "Importancia del predictor",
       fill = "Variable",
       title = "Contribución de cada predictor por mes") +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete( expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.1,"in")
  )

