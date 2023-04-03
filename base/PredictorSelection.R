setwd("~/Documents/GitHub/seasonal_forecast")
library(data.table)
grid_pred  <- function(  variable,
                         agg_months,
                         agg_func) {
  # explore variables (averaged)
  grid = expand.grid(variable, agg_months)
  predictors = paste(grid[,1], agg_func, paste0(grid[,2],"months"), sep = "_")
  return(predictors)
}

# Function for preprocessing data before training
preprocess_before_train <- function(x, preProcMethod = c("center", "scale")) {
  # Preprocess data
  preProcData <- preProcess(x, method = preProcMethod)
  # Get preprocessed data
  x_pp <- predict(preProcData, newdata = x)
  return(x_pp)
}

remove_correlated_predictors <- function(cor_matrix, predictor_list, cutoff = 0.6) {

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
  #pr <- resid(mlrmod) / (1 - lm.influence(mlrmod)$hat)
  
  y_pred <- mlrmod$fitted.values
  y_true <- regression_model$trainingData$.outcome
  
  metrics <- list(
    rmse = caret::RMSE(y_pred, y_true),
    r2 = caret::R2(y_pred, y_true),
    mae = caret::MAE(y_pred, y_true),
    pbias = mean((y_true - y_pred) / y_true),
    #gcv = unlist(locfit::gcv(mlrmod, maxk = 1e6)[[4]]),
    #bic = BIC(mlrmod),
    #press = sum(pr^2),
    aic = AIC(mlrmod)
  )
  
  return(metrics)
}

save_model_results <- function(df_info,
                               predictor,
                               #regression_model,
                               imp_var,
                               metrics) {
  result <- list(
    df_info = df_info,
    predictor = list(predictor),
    #reg_model = regression_model,
    imp_var = (imp_var),
    metrics = (metrics)
  )
  
  return(result)
}

select_best_models <- function(models, objective_metric = "aic") {
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
                                  which.min(.data[[objective_metric]]))
  
  all_combinations$model <- NULL
  
  return(list(best_combination = best_combination,
              unique_predictors = unique(unlist(predictor_list)),
              all_combinations = all_combinations))
}



plot_importance <- function(best_list,objective_metric) {
  library('dplyr')
  library("data.table")
  library('ggplot2')
  months_es <- c("ene", "feb", "mar","abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic")
  months_wy <- c("abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic", "ene", "feb", "mar")

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
    mutate(date_label = paste0("1˚",months_es[month_initialisation]))
  
  predictor_importance$date_label = factor(predictor_importance$date_label, levels = paste0("1˚",months_wy) )
  
  
  
  xx = best_list$best_combination %>% 
    mutate(date_label = paste0("1˚",months_es[month_initialisation])) %>% 
    mutate(date_label = factor(date_label, levels = paste0("1˚",months_wy)))
  
  predictor_importance = merge(xx, predictor_importance)
  #print(predictor_importance)
  #catchment name
  catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 
  cod_cuencas = fread(catchments_attributes_filename)
  catchment_name = cod_cuencas[cod_cuencas$cod_cuenca == unique(best_list$best_combination$catchment_code),"gauge_name"]
  
  # Add the color for the "STORAGE" predictor and create the color_palette
  color_palette <- c("NINO1.2" =  "#3B9AB2",
                     "ONI" =  "#78B7C5",
                     "PDO" =  "#EBCC2A",
                     "SOI" =  "#E1AF00",
                     "STORAGE" = "#ff6d5c")
  
  
  # Create the ggplot with updated x-axis labels
  p = ggplot(predictor_importance, aes(x = date_label, y = importance, fill = var)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
    geom_text(aes(label = round(r2,2)),position = position_stack(vjust = 0.96))+
    labs(x = "Mes de emisión",
         y = "Importancia del predictor",
         fill = "Variable",
         title = paste0("Contribución de cada predictor para el mejor modelo (",toupper(objective_metric),")"),
         subtitle = catchment_name) +
    scale_fill_manual(values = color_palette) +
    scale_x_discrete( expand = c(0, 0)) +
    scale_y_continuous( expand = c(0, 0)) +
    theme_minimal()+
    theme(
      legend.position = "bottom",
      legend.key.width = unit(0.1,"in")
    )
  
  ggsave(filename = paste0("data_output/figuras/importancia_predictores/importancia_predictores_",objective_metric,"_",unique(best_list$best_combination$catchment_code),'_v2.png'),
         width = 6, height = 6,dpi = 400)
  return(predictor_importance)
}



select_predictor <- function(
    catchment_code,
    months_initialisation,
    objective_metric = "aic",
    save = F,
    chart = F) {
  
  library('doSNOW')
  # Set catchment code and months of initialization
  predictors <- grid_pred(c("SOI", "PDO","NINO1.2","STORAGE"), 1, "mean")
  
  cl <- makeCluster(parallel::detectCores() - 3L)
  registerDoSNOW(cl)
  
  models <- foreach(month_initialisation = months_initialisation,
                    .combine = "c",
                    .export=c('remove_correlated_predictors','calculate_model_metrics','save_model_results','preprocess_before_train')) %dopar% { 
    source("base/Preprocess_data.R")
    source("base/Charts.R")
    library("foreach")
    library("caret")
    library("locfit")
    
    data_input <- preprocess_data(
      datetime_initialisation = lubridate::make_date(2021, 12) %m+% months(month_initialisation),
      forecast_mode = "cv",
      catchment_code = catchment_code,
      predictor_list = predictors,
      save_raw = T
    )
    month_initialisation = month(data_input$info$datetime_initialisation)
    
    p1 = plot_X_y_train(data_input)
    ggsave(glue("data_output/figuras/scatter_xy/scatter_xy_{data_input$info$catchment_code}_{month_initialisation}.png"),
           plot=p1,width=10,height=4,dpi=400)
    
    
    predictor_list <- data_input$info$predictor_list
    x_train = data_input$X_train
    x_train_normalised = preprocess_before_train(x_train)
    
    ### correlacion entre predictores
    cor_matrix <- cor(x_train_normalised,method = "spearman")
    # #guardar grafico de correlaciones
    # 
    #   png(file = glue("data_output/figuras/correlacion_predictores/cor_spearman_{catchment_code}_{month_initialisation}.png"),
    #       width=5,height=5,units = "in",res = 400)
    #   corrplot::corrplot(corr = cor_matrix,method = "number",
    #                      type = "lower",
    #                      diag = F,
    #                      title = paste(catchment_code,month_initialisation));
    #   dev.off()
    # 
    

           
    
    
    predictors_uncorrelated <- remove_correlated_predictors(cor_matrix, predictor_list)
    
    predictors_comb <- unlist(lapply(seq_along(predictors_uncorrelated), function(i)
      combn(predictors_uncorrelated, i, simplify = FALSE)), recursive = FALSE)
    #var creation
    model_results <- vector("list", length(predictors_comb))
    
    foreach(predictor = predictors_comb) %dopar% {
      
      x = dplyr::select(x_train_normalised, dplyr::all_of(predictor))
      y = data_input$y_train$volume
      
      
      regression_model <- train(
        x,
        y,
        metric = "RMSE",
        trControl = trainControl(method = "LOOCV", savePredictions = "all"),
        method = "lm",
        preProcess = NULL
      )
      
      metrics <- calculate_model_metrics(regression_model)
      
      imp_var <- if (length(predictor) > 1) relaimpo::calc.relimp(regression_model$finalModel, rela = TRUE)$lmg else setNames(1, predictor)
      
      model <- save_model_results(data.frame(month_initialisation, catchment_code),
                                  predictor,
                                  #regression_model,
                                  data.frame(t(imp_var)),
                                  metrics)
    }
  }
  
  stopCluster(cl)
  
  
  best_list = select_best_models(models, objective_metric)
  
  if (chart) {
    p = plot_importance(best_list,objective_metric)
    best_list = append(list(importance = p),best_list)
  }
  if (save) {
    saveRDS(best_list,file = paste0("data_output/mejores_modelos_cuenca_mes/",catchment_code,'_may-mar.RDS') )
  }
  return(best_list)
}

# Run the function




library(dplyr)
# all available catchments, no data 6008005, 7317005, 7355002, 8106001
catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 

cod_cuencas = fread(catchments_attributes_filename) %>%
  subset(!(cod_cuenca %in% c(6008005, 7317005, 7355002, 8106001))) %>% 
  select(cod_cuenca) %>% unlist()

  for (catchment_code in cod_cuencas) {
    print(catchment_code)
    select_predictor(catchment_code = catchment_code,
                     chart = T,
                     objective_metric = "aic",
                     save = T,
                     months_initialisation = 5:15)

}






