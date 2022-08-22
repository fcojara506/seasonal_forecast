rm(list = ls())
source("base/Transform_variables_to_monthly.R")

for (model in c("GR4J","TUW","SAC")) {
for (fo in c("EVDSep","KGE","KGE+logNSE","NSE","SKGE")) {
  
  #filenames list
  storage_filenames = storage_variables_filename(
    hydrological_model = model,
    objective_function = fo,
    folder_storage_variables = "base/data_input/storage_variables"
  )
  # transform and export to use in model
  export_hydro(files_list = storage_filenames)
  
}  
}

# function

# # export meteo different versions (raw, pre-processed, ensemble-pre-processed)
# 
#CR2MET
# export_meteo(
#   filename_pr = "data_input/precip_CR2MET_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "data_input/temp_CR2MET_1979-2020_fondef-dga_v1.csv",
#   filename_export = "data_input/meteo_monthly_catchments_ChileCentral_CR2MET.feather"
# )

# #ERA5-raw
# export_meteo(
#   filename_pr = "data_input/precip_ERA5-raw_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "data_input/temp_ERA5-raw_1979-2020_fondef-dga_v1.csv",
#   filename_export = "data_input/meteo_monthly_catchments_ChileCentral_era5raw.feather"
# )
# 
# #ERA5- QDM
# export_meteo(
#   filename_pr = "data_input/precip_ERA5-BC-benchmark_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "data_input/temp_ERA5-BC-benchmark_1979-2020_fondef-dga_v1.csv",
#   filename_export = "data_input/meteo_monthly_catchments_ChileCentral_era5QDM.feather"
# )
# 
# #ERA5- ensemble promedio N=20
# export_meteo(
#   filename_pr = "data_input/precip_ERA5-BC-ensemble-prom_N20_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "data_input/temp_ERA5-BC-ensemble-prom_N20_1979-2020_fondef-dga_v1.csv",
#   filename_export = "data_input/meteo_monthly_catchments_ChileCentral_ens20avg.feather"
# )
# 
# #ERA5- ensemble-pre-processed 20 ensemble
# export_meteo(
#   filename_pr = "data_input/precip_ERA5-BC-ensemble-miembros_N20_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "data_input/temp_ERA5-BC-ensemble-miembros_N20_1979_2020_fondef-dga_v1.csv",
#   filename_export = "data_input/meteo_monthly_catchments_ChileCentral_ens20.feather"
# )
# 
# #ERA5- ensemble-pre-processed 30 ensembles
# export_meteo(
#   filename_pr = "data_input/precip_ERA5-BC-ensemble-miembros_N30_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "data_input/temp_ERA5-BC-ensemble-miembros_N30_1979_2020_fondef-dga_v1.csv",
#   filename_export = "data_input/meteo_monthly_catchments_ChileCentral_ens30.feather"
# )
# #ERA5- ensemble promedio N=30
# export_meteo(
#   filename_pr = "data_input/precip_ERA5-BC-ensemble-prom_N30_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "data_input/temp_ERA5-BC-ensemble-prom_N30_1979-2020_fondef-dga_v1.csv",
#   filename_export = "data_input/meteo_monthly_catchments_ChileCentral_ens30avg.feather"
# )

#ERA5- ensemble-pre-processed 50 ensembles
# export_meteo(
#   filename_pr = "data_input/precip_ERA5-BC-ensemble-miembros_N50_1979_2020_fondef-dga_v1.csv",
#   filename_tem = "data_input/temp_ERA5-BC-ensemble-miembros_N50_1979_2020_fondef-dga_v1.csv",
#   filename_export = "data_input/meteo_monthly_catchments_ChileCentral_ens50.feather"
# )



