### propiedades mas comunes para correr el modelo
source("utils/run_model_operativo.R")
# codigos de las cuencas
attributes_catchments_file <-
  "data_input/attributes/attributes_45catchments_ChileCentral.csv"
attributes_catchments <- fread(attributes_catchments_file)

codigos_cuencas = attributes_catchments$cod_cuenca
codigos_cuencas_subconjunto = c(3820001, 5410002, 7321002)

#### fecha de emision
fecha_actual = Sys.Date()
fecha_emision_mas_reciente = encontrar_fecha_emision(
  current_date = fecha_actual,
  dias_retraso_entradas = 6)

fecha_emision_test = "1989-09-01"
