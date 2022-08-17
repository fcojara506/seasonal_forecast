rm(list = ls())

source("run_model_function.R")

catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
attributes_catchments = feather::read_feather(catchments_attributes_filename)

cod_cuencas = attributes_catchments$cod_cuenca
months_initialisation = c("jun","oct")#c('may','jun','jul','ago','sep','oct','nov','dic','ene','feb')

regions = c(
  #"ChileCentral_CR2MET",
  #"ChileCentral_era5raw",
  "ChileCentral_era5QDM",
  "ChileCentral_ens30avg"
  )
directions = c("vol_to_flow","flow_to_vol")

iterations = length(cod_cuencas)*length(months_initialisation)*length(regions)*length(directions)
print(iterations)

library(foreach)
library(doParallel)
registerDoParallel(cores=8)

model <-
  foreach(month_initialisation=months_initialisation,.combine = "c") %:%
  foreach(region=regions,.combine = "c") %:%
  foreach(direction=directions,.combine = "c") %:%
  foreach(catchment_code=cod_cuencas) %dopar% {
    
    run_model(
      catchment_code = catchment_code,
      month_initialisation = month_initialisation,
      region = region,
      direction = direction
    )
    
  } %>% purrr::transpose()


scores = rbindlist(model$scores)
info = rbindlist(model$info)

data_input = cbind(info,scores) %>%
  merge(attributes_catchments,
        by.x = "catchment_code",
        by.y = "cod_cuenca"
        )

saveRDS(data_input,"model_results_v2.RDS")
