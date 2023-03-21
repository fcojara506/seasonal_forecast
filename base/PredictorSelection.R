# Load required packages
# library(lubridate)
# library(dplyr)
# library(caret)
# library(foreach)
# library(doSNOW)
# library(data.table)
# library(tidyverse)

grid_pred  <- function(  variable,
                         agg_months,
                         agg_func) {
  # explore variables (averaged)
  grid = expand.grid(variable, agg_months)
  predictors = paste(grid[,1], agg_func, paste0(grid[,2],"months"), sep = "_")
  return(predictors)
}


remove_correlated_predictors <- function(X_train, predictor_list, cutoff = 0.8) {
  cor_matrix <- cor(X_train)
  high_cor <- caret::findCorrelation(cor_matrix, cutoff = cutoff, verbose = FALSE)
  
  if (rlang::is_empty(high_cor)) {
    return(predictor_list)
  } else {
    return(predictor_list[-high_cor])
  }
}

calculate_model_metrics <- function(regression_model) {
  library('locfit')
  mlrmod <- regression_model$finalModel
  pr <- resid(mlrmod) / (1 - lm.influence(mlrmod)$hat)
  
  pred <- mlrmod$fitted.values
  obs <- regression_model$trainingData$.outcome
  
  rmse <- RMSE(pred, obs)
  r2 <- R2(pred, obs)
  mae <- MAE(pred, obs)
  pbias <- abs(mean((obs - pred) / obs))
  
  metrics <- list(
    gcv = unlist(locfit::gcv(mlrmod, maxk = 1e6)[[4]]),
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

save_model_results <- function(df_info,
                               predictor,
                               regression_model,
                               imp_var,
                               metrics) {
  result <- list(
    df_info = df_info,
    predictor = list(predictor),
    reg_model = regression_model,
    imp_var = (imp_var),
    metrics = (metrics)
  )
  
  return(result)
}

select_best_models <- function(models) {
  library("data.table")
  model_list <- purrr::transpose(models)
  predictor_list =  model_list$predictor
  
  all_combinations <- cbind( rbindlist(model_list$df_info),
                       rbindlist(model_list$metrics),
                       predictors = predictor_list,
                       rbindlist(model_list$imp_var, fill = T),
                       model = model_list$reg_model
  )
  
  best_combination = dplyr::slice(dplyr::group_by(all_combinations,
                                      month_initialisation,catchment_code),
                 which.min(aic))
  
  all_combinations$model <- NULL
  
  return(list(best_combination = best_combination,
              unique_predictors = unique(unlist(predictor_list)),
              all_combinations = all_combinations))
}

plot_importance <- function(best_list) {
  library('dplyr')
  library('ggplot2')
  
  # Create a new data frame for the plot
  predictor_importance <- best_list$best_combination %>%
    select(month_initialisation, catchment_code,matches(best_list$unique_predictors)) %>%
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
  unique_vars <- unique(predictor_importance$var)
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
  p = ggplot(predictor_importance, aes(x = date_label, y = importance, fill = var)) +
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
  return(p)
}


select_predictor <- function(
    catchment_code = "4503001",
    months_initialisation = 5:9,
    chart = F) {
  
  library('doSNOW')
  # Set catchment code and months of initialization
  predictors <- grid_pred(c("SOI", "PDO", "ONI","NINO1.2"), 1, "mean")
  predictors <- c(predictors, grid_pred(c("STORAGE"), 1, "last"))
  
  cl <- makeCluster(parallel::detectCores() - 1L)
  registerDoSNOW(cl)
  
  models <- foreach(month_initialisation = months_initialisation, .combine = "c", .export=c('remove_correlated_predictors','calculate_model_metrics','save_model_results')) %dopar% { 
    source("base/Preprocess_data.R")
    library("foreach")
    library("caret")
    library("locfit")
    
    data_input <- preprocess_data(
      datetime_initialisation = lubridate::make_date(2022, month_initialisation),
      forecast_mode = "cv",
      catchment_code = catchment_code,
      predictor_list = predictors
    )
    
    predictor_list <- data_input$info$predictor_list[[1]]
    predictors_uncorrelated <- remove_correlated_predictors(data_input$X_train, predictor_list)
    
    predictors_comb <- unlist(lapply(seq_along(predictors_uncorrelated), function(i)
      combn(predictors_uncorrelated, i, simplify = FALSE)), recursive = FALSE)
    
    model_results <- vector("list", length(predictors_comb))
    
    foreach(predictor = predictors_comb) %dopar% {
      
      x = dplyr::select(data_input$X_train, dplyr::all_of(predictor))
      y = data_input$y_train$volume
      
      
      regression_model <- train(
        x,
        y,
        metric = "RMSE",
        trControl = trainControl(method = "LOOCV", savePredictions = "all"),
        method = "lm",
        preProcess = c("center", "scale")
      )
      
      metrics <- calculate_model_metrics(regression_model)
      
      imp_var <- if (length(predictor) > 1) relaimpo::calc.relimp(regression_model$finalModel, rela = TRUE)$lmg else setNames(1, predictor)
      
      model <- save_model_results(data.frame(month_initialisation, catchment_code),
                                  predictor,
                                  regression_model,
                                  data.frame(t(imp_var)),
                                  metrics)
    }
  }
  
  stopCluster(cl)
  
  best_list = select_best_models(models)
  
  return(best_list)
}

# Run the function
results <- select_predictor()

#plot_importance(results)






