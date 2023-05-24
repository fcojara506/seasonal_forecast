
# preproceso (descarga,limpieza,correccion sesgo) meteorológico
source(file = "base/MeteoPresente1_Request-CDO.R")
source(file = "base/MeteoPresente2_EscalaCuenca.R")
source(file = "base/MeteoPresente3_DiasSimilares.R")
source(file = "base/MeteoPresente4_BiasAdjEnsemble.R")

# correr modelo hidrológico
source(file = "base/SimulacionTUW1.R")

# preprocesar simulaciones del modelo hidrológico (diario a mensual)
source(file = "utils/convert_daily_to_monthly_storage.R")

# preproceso (descarga y limpieza) indices climáticos
source(file = "base/Climate_index.R")



#cargar atributos de las 45 cuencas

catchment_code = 7321002
datetime_initialisation = "2022-09-01"


