rm(list = ls())

source("utils/run_model_function.R")

catchments_attributes_filename = "base/data_input/attributes/attributes_49catchments_ChileCentral.feather"
attributes_catchments = feather::read_feather(catchments_attributes_filename)#[,c("cod_cuenca","gauge_name","gauge_lat","gauge_lon","mean_elev")]

cod_cuencas = attributes_catchments$cod_cuenca
wys_holdout = seq(2019,2022)
len_w = length(cod_cuencas)*length(wys_holdout)

#initial lists
model_init = run_model(month_initialisation = "oct",wy_holdout = 2022)
df_platform_vol = lapply(seq_len(len_w), function(x) model_init$df_platform_vol )
df_platform_q = lapply(seq_len(len_w), function(x) model_init$df_platform_q )

w=1

for (cod_cuenca in cod_cuencas) {
  for (wy_holdout in wys_holdout) {
      
      model<- run_model(
        catchment_code = cod_cuenca,
        month_initialisation = "oct",
        wy_holdout = wy_holdout,
        predictor_list = c("pr_sum_-1months","tem_mean_3months"),
        units_q = "m^3/s",
        units_y = "GL"
      )
      stop()
      df_platform_vol[[w]] = model$df_platform_vol
      df_platform_q[[w]] = model$df_platform_q
      print(w)
      w=w+1
    
  }
}


df_platform_vol = rbindlist(df_platform_vol)
df_platform_q = rbindlist(df_platform_q)

write.csv(df_platform_vol,
          row.names=FALSE,
          file = "utils/data_output/pronostico_volumen/plataforma/volumen_pronosticado_test_13dic2022.csv")
write.csv(df_platform_q,
          row.names=FALSE,
          file = "utils/data_output/pronostico_caudal/plataforma/caudales_pronosticados_test_13dic2022.csv")
