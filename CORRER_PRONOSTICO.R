# ----------------------------------------------------------------------------
# Nombre del Proyecto: Pronóstico híbridos del volumen estacional en Chile Central 
# Autor(es): Francisco Jara, Diego Hernández
# Fecha de Finalización: 2023/04/31
# Contacto: pronostico.caudales@gmail.com
# GitHub: https://github.com/fcojara506
# ----------------------------------------------------------------------------

# Descripción:
# Este script ejecuta los principales comandos del modelo de pronostico
# ----------------------------------------------------------------------------


# instalar paquetes escenciales
source(file = "base/Load_libraries.R")

descargar_nuevos_datos = FALSE
if (descargar_nuevos_datos) {
  # preproceso (descarga,limpieza,correccion sesgo) meteorológico
  #descarga
  source(file = "base/MeteoPresente1_Request-CDO.R")
  descargar_era5(CDS_user = NULL,CDS_key = NULL)
  #conversion a nivel de cuenca
  source(file = "base/MeteoPresente2_EscalaCuenca.R")
  #encontrar dias similares meteorologicamente
  source(file = "base/MeteoPresente3_DiasSimilares.R")
  dias_similares_operativo(N = 30)
  
  source(file = "base/MeteoPresente4_BiasAdjEnsemble.R")
  correccion_sesgo_meteorologico(N = 30)
  
  # correr modelo hidrológico
  source(file = "base/SimulacionTUW1.R")
  # preprocesar simulaciones del modelo hidrológico (diario a mensual)
  source(file = "utils/convert_daily_to_monthly_storage.R")
  
  # preproceso (descarga y limpieza) indices climáticos
  source(file = "base/Climate_index.R")
  
}

# cargar modelo operativo (modo restrospectivo y predicción)
source("utils/run_model_operativo.R")

#carga los codigos y propiedades mas recientes (codigos_cuencas)
source(file = "default_input_data.R")
codigos_cuencas = codigos_cuencas_subconjunto
fecha_emision = fecha_emision_test



resultados = 
pronostico_operativo(
  codigos_cuencas = codigos_cuencas,
  fecha_emision_Y_M_D = fecha_emision,
  exportar_figuras = TRUE
)
# guardar en archivos
write.csv(resultados$plataforma_volumen,
          file = glue("data_output/plataforma/volumen/volumen_estacional_{fecha_emision}_version{Sys.Date()}.csv"),
          row.names = FALSE)

write.csv(resultados$plataforma_caudal,
          file = glue("data_output/plataforma/caudales/caudalesmediosmensuales_{fecha_emision}_version{Sys.Date()}.csv"),
          row.names = FALSE)

saveRDS(resultados$resultados_tecnicos, 
        file = glue("data_output/plataforma/resultados_en_detalles/resultadostecnicos_{fecha_emision}_version{Sys.Date()}.RDS"))

# library(DependenciesGraphs)
# deps = funDependencies(envir = environment(),
#                        name.function = "pronostico_operativo")
# plot(deps)

