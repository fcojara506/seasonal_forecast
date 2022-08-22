rm(list = ls())
directory = "/Users/fco/CAPTA/Pronostico_estacional/"
setwd(directory)

source("run_model_function.R")

catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
attributes_catchments = feather::read_feather(catchments_attributes_filename)#[,c("cod_cuenca","gauge_name","gauge_lat","gauge_lon","mean_elev")]

cod_cuencas = attributes_catchments$cod_cuenca
months_initialisation = c('jul')
len_w = length(cod_cuencas)*length(months_initialisation)

#initial lists
model_init = run_model()
df_platform_vol = lapply(seq_len(len_w), function(x) model_init$df_platform_vol )
df_platform_q = lapply(seq_len(len_w), function(x) model_init$df_platform_q )

w=1

for (cod_cuenca in cod_cuencas) {
  for (month_initialisation in months_initialisation) {
      
      model<- run_model(
        catchment_code = cod_cuenca,
        month_initialisation = month_initialisation
      )
      
      df_platform_vol[[w]] = model$df_platform_vol
      df_platform_q[[w]] = model$df_platform_q
      print(w)
      w=w+1
    
  }
}


df_platform_vol = rbindlist(df_platform_vol)
df_platform_q = rbindlist(df_platform_q)

write.csv(df_platform_vol,row.names=FALSE,file = "data_output/pronostico_volumen/plataforma/volumen_pronosticado_test_1ago2022.csv")
write.csv(df_platform_q,row.names=FALSE,file = "data_output/pronostico_caudal/plataforma/caudales_pronosticados_test_1ago2022.csv")
