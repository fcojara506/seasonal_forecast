rm(list = ls())

source("utils/run_model_function.R")
#test catchments
catchments_attributes_filename = "df/attributes/attributes_49catchments_ChileCentral.feather" 
cod_cuencas = feather::read_feather(catchments_attributes_filename)$cod_cuenca
#test months initial
months_initialisation = c('abr','may','jun','jul','ago','sep')

# test climate indices
# climate indices
climate_indices = c("MEIv2", "SOI", "ONI", "BIENSO")
months_horizon = c(1, 2, 3, 4, 5, 6)
climate_grid = expand.grid(climate_indices, months_horizon)
climate_predictors = paste(climate_grid[,1], "mean", paste0(climate_grid[,2],"months"), sep = "_")


library(foreach)
library(doParallel)
registerDoParallel(cores = parallel::detectCores())

model <-
  foreach(month_initialisation=months_initialisation,.combine = "c") %:%
  foreach(climate_predictor = climate_predictors,.combine = "c") %:%
  foreach(catchment_code=cod_cuencas) %dopar% {

    run_model(
      catchment_code = catchment_code,
      month_initialisation = month_initialisation,
      horizon_month_start = "sep",
      horizon_month_end = "mar",
      horizon_strategy = "static",
      predictor_list = climate_predictor,
      wy_holdout = 2022,
      remove_wys = seq(1950,1980),
      units_q = "m3/s",
      units_y = "GL",
      mode = "cv",
      export = "scores"
    )
  } %>% purrr::transpose()

stopImplicitCluster()



df <- cbind(rbindlist(model$info),rbindlist(model$scores))
# modify names
df <- dplyr::rename(df,'predictor_name' = 'predictor_list_corrected')

# order columns
df$month_initialisation <- factor(df$month_initialisation,levels = months_initialisation)
df$predictor_name <- factor(df$predictor_name)
df$catchment_code <- as.numeric(df$catchment_code)

saveRDS(df,paste0("data_output/scores/RDS/model_results_",today(),".RDS"))

