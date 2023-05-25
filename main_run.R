# instalar paquetes escenciales
source(file = "base/Load_libraries.R")

descargar_nuevos_datos = FALSE
if (descargar_nuevos_datos) {
  # preproceso (descarga,limpieza,correccion sesgo) meteorológico
  source(file = "base/MeteoPresente1_Request-CDO.R")
  source(file = "base/MeteoPresente2_EscalaCuenca.R")
  source(file = "base/MeteoPresente3_DiasSimilares.R")
  source(file = "base/MeteoPresente4_BiasAdjEnsemble.R")
  
  # correr modelo hidrológico
  source(file = "base/SimulacionTUW1.R")
}

# preprocesar simulaciones del modelo hidrológico (diario a mensual)
source(file = "utils/convert_daily_to_monthly_storage.R")

# preproceso (descarga y limpieza) indices climáticos
source(file = "base/Climate_index.R")

#carga los codigos y propiedades recomendadas (codigos_cuencas)
source(file = "default_input_data.R")
codigos_cuencas = codigos_cuencas
fecha_emision = "2022-09-01"

# correr modelo operativo (modo restrospectivo y predicción)
source("utils/run_model_operativo.R")
resultados = 
pronostico_operativo(
  codigos_cuencas = codigos_cuencas,
  fecha_emision_Y_M_D = fecha_emision
)

library(DependenciesGraphs)
deps = funDependencies(envir = environment(),name.function = "pronostico_operativo")
plot(deps)
