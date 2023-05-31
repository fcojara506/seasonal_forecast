rm(list = ls())
source('utils/main_regression_model.R')

library(data.table)
library(dplyr)

attributes_catchments_file <- "data_input/attributes/attributes_45catchments_ChileCentral.csv"
attributes_catchments <- fread(attributes_catchments_file)

codigos_cuencas = attributes_catchments$cod_cuenca

meses_emision = c(5,6,7,8,9,10,11,12,1,2,3)

for (codigo_cuenca in codigos_cuencas) {
  for (mes_emision in meses_emision) {
    
    a = run_model(catchment_code = codigo_cuenca,
                  month_initialisation = mes_emision,
                  forecast_mode = "cv",
                  export = "all"
                  )
    #guardar todo  
    saveRDS(a,
            file = glue("data_output/modelos_periodo_historico/todos_procesos/todo_1980_2019_{codigo_cuenca}_mes{mes_emision}.RDS"))
    
    #guardar modelo de regresion
    saveRDS(a$data_fore$regression_model,
            file = glue("data_output/modelos_periodo_historico/modelo_regresion/modelo_regresion_{codigo_cuenca}_mes{mes_emision}.RDS"))
    }
}
