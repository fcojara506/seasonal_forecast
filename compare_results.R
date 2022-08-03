rm(list = ls())
directory = "/Users/fco/CAPTA/Pronostico_estacional/"
setwd(directory)

source("run_model_function.R")

catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
attributes_catchments = feather::read_feather(catchments_attributes_filename)#[,c("cod_cuenca","gauge_name","gauge_lat","gauge_lon","mean_elev")]

cod_cuencas = attributes_catchments$cod_cuenca
months_initialisation = c('may','jun','jul','ago','sep','oct','nov','dic','ene','feb')

regions = c(
  "ChileCentral_CR2MET",
  "ChileCentral_era5raw",
  "ChileCentral_era5QDM",
  "ChileCentral_ens30avg",
  "ChileCentral_ens30"
  )
iterations = length(cod_cuencas)*length(months_initialisation)*length(regions)

library(foreach)
library(doParallel)


#pb <- txtProgressBar(max = iterations, style = 3)
#progress <- function(n) setTxtProgressBar(pb, n)
#opts <- list(progress = progress)
#registerDoParallel(8)

model <-
  foreach(catchment_code=cod_cuencas[1:4],.combine = "c") %:%
  foreach(region=regions[1:2]) %:%
  foreach(month_initialisation=months_initialisation[1:2]) %dopar% {
    run_model(
      catchment_code = catchment_code,
      month_initialisation = month_initialisation,
      region = region
    )
  } %>% purrr::transpose()



scores = rbindlist(model$scores)
info = rbindlist(model$info)

data_input = cbind(info,scores) %>%
  merge(attributes_catchments,
        by.x = "catchment_code",
        by.y = "cod_cuenca"
        )
# 
# #order x axis
# data_input$month_initialisation = factor(
#   data_input$month_initialisation,
#   levels = months_initialisation
#   ) 
# 
# 
# library(ggplot2)
# 
# ggplot(data=data_input)+
#   geom_line(
#     aes(
#       x=month_initialisation,
#       y=crpss_climatology,
#       col=mean_elev,
#       group=mean_elev
#       )
#     )+
#   scale_color_viridis_b()
# 
# ggplot(data = data_input)+
#   geom_point(
#     aes(
#       x=crpss_climatology,
#       y =gauge_lat,
#       col = month_initialisation,
#     )
#   )+
#   scale_color_viridis_d()
# 
