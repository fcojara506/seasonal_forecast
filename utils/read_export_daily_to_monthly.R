rm(list = ls())
source("base/Transform_variables_to_monthly.R")

#storage
for (model in c("GR4J","TUW","SAC")) {
for (fo in c("EVDSep","KGE","KGE+logNSE","NSE","SKGE")) {

  #filenames list
  storage_filenames = storage_variables_filename(
    hydrological_model = model,
    objective_function = fo,
    folder_storage_variables = "base/data_input/storage_variables"
  )
  # transform into data frame to use within model
  df= aggregate_hydro(files_list = storage_filenames)
  filename_export = df$fullname_filename_export
  
  # add new variable as the sum of the rest hydro variables
  df= new_hydro_variable(df = df$df,
                        selected_variables = storage_filenames$variables,
                        FUN = sum,
                        var_name = "STORAGE")
  # export
  feather::write_feather(x=df, path = filename_export)

}
}

# function

# # export meteo different versions (raw, pre-processed, ensemble-pre-processed)
# 
#CR2MET
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/1979-2020/precip_CR2MET_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "base/data_input/meteo_variables/1979-2020/temp_CR2MET_1979-2020_fondef-dga_v1.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_CR2MET_1979_2020.feather"
# )

# #ERA5-raw
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/1979-2020/precip_ERA5-raw_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "base/data_input/meteo_variables/1979-2020/temp_ERA5-raw_1979-2020_fondef-dga_v1.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_era5raw_1979_2020.feather"
# )
# 
# #ERA5- QDM
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/1979-2020/precip_ERA5-BC-benchmark_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "base/data_input/meteo_variables/1979-2020/temp_ERA5-BC-benchmark_1979-2020_fondef-dga_v1.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_era5QDM_1979_2020.feather"
# )
# 
# #ERA5- ensemble promedio N=20
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/1979-2020/precip_ERA5-BC-ensemble-prom_N20_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "base/data_input/meteo_variables/1979-2020/temp_ERA5-BC-ensemble-prom_N20_1979-2020_fondef-dga_v1.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens20avg_1979_2020.feather"
# )
# 
# #ERA5- ensemble-pre-processed 20 ensemble
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/1979-2020/precip_ERA5-BC-ensemble-miembros_N20_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "base/data_input/meteo_variables/1979-2020/temp_ERA5-BC-ensemble-miembros_N20_1979_2020_fondef-dga_v1.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens20_1979_2020.feather"
# )
# 
# #ERA5- ensemble-pre-processed 30 ensembles
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/1979-2020/precip_ERA5-BC-ensemble-miembros_N30_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "base/data_input/meteo_variables/1979-2020/temp_ERA5-BC-ensemble-miembros_N30_1979_2020_fondef-dga_v1.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30_1979_2020.feather"
# )
# #ERA5- ensemble promedio N=30
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/1979-2020/precip_ERA5-BC-ensemble-prom_N30_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "base/data_input/meteo_variables/1979-2020/temp_ERA5-BC-ensemble-prom_N30_1979-2020_fondef-dga_v1.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30avg_1979_2020.feather"
# )

#ERA5- ensemble-pre-processed 50 ensembles
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/1979-2020/precip_ERA5-BC-ensemble-miembros_N50_1979_2020_fondef-dga_v1.csv",
#   filename_tem = "base/data_input/meteo_variables/1979-2020/temp_ERA5-BC-ensemble-miembros_N50_1979_2020_fondef-dga_v1.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens50_1979_2020.feather"
# )


# ## 2020-2022
# # ERA5-raw
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/2020-2022/precip_ERA5-raw_2020-2022_fondef-dga.csv",
#   filename_tem = "base/data_input/meteo_variables/2020-2022/temp_ERA5-raw_2020-2022_fondef-dga.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_era5raw_2020_2022.feather"
# )
# # ERA5- ensemble promedio N=30
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/2020-2022/precip_ERA5-BC-ensemble-prom_N30_2020-2022_fondef-dga.csv",
#   filename_tem = "base/data_input/meteo_variables/2020-2022/temp_ERA5-BC-ensemble-prom_N30_2020-2022_fondef-dga.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30avg_2020_2022.feather"
# )
# # ERA5- ensemble-pre-processed 30 ensembles
# export_meteo(
#   filename_pr = "base/data_input/meteo_variables/2020-2022/precip_ERA5-BC-ensemble-miembros_N30_2020-2022_fondef-dga.csv",
#   filename_tem = "base/data_input/meteo_variables/2020-2022/temp_ERA5-BC-ensemble-miembros_N30_2020-2022_fondef-dga.csv",
#   filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30_2020_2022.feather"
# )

## 2022-present
# ERA5-raw
export_meteo(
  filename_pr = "base/data_input/meteo_variables/2022-presente/precip_ERA5-raw_2022-presente_fondef-dga.csv",
  filename_tem = "base/data_input/meteo_variables/2022-presente/temp_ERA5-raw_2022-presente_fondef-dga.csv",
  filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_era5raw_2022_present.feather"
)
# ERA5- ensemble promedio N=30
export_meteo(
  filename_pr = "base/data_input/meteo_variables/2022-presente/precip_ERA5-BC-ensemble-prom_N30_2022-presente_fondef-dga.csv",
  filename_tem = "base/data_input/meteo_variables/2022-presente/temp_ERA5-BC-ensemble-prom_N30_2022-presente_fondef-dga.csv",
  filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30avg_2022_present.feather"
)
# ERA5- ensemble-pre-processed 30 ensembles
export_meteo(
  filename_pr = "base/data_input/meteo_variables/2022-presente/precip_ERA5-BC-ensemble-miembros_N30_2022-presente_fondef-dga.csv",
  filename_tem = "base/data_input/meteo_variables/2022-presente/temp_ERA5-BC-ensemble-miembros_N30_2022-presente_fondef-dga.csv",
  filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30_2022_present.feather"
)


#join files

# # ERA5-raw
list(
  feather::read_feather("base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_era5raw_1979_2020.feather"),
  feather::read_feather("base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_era5raw_2020_2022.feather")
) %>%
  join_files(filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_era5raw_1979_2022.feather")



# # ERA5- ensemble promedio N=30
list(
  feather::read_feather("base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30avg_1979_2020.feather"),
  feather::read_feather("base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30avg_2020_2022.feather"),
  feather::read_feather("base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30avg_2022_present.feather")
) %>%
  join_files(filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30avg_1979_2022.feather")


# # ERA5- ensemble-pre-processed 30 ensembles
list(
  feather::read_feather("base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30_1979_2020.feather"),
  feather::read_feather("base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30_2020_2022.feather"),
  feather::read_feather("base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30_2022_present.feather")
  ) %>%
  join_files(filename_export = "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30_1979_2022.feather")

