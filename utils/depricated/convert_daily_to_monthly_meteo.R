rm(list = ls())
source("base/Transform_variables_to_monthly.R")
a=export_meteo(
  filename_pr = "data_input/meteo_variables/1979-2020/precip_ERA5-BC-ensemble-prom_N30_1979-2020_fondef-dga_v1.csv",
  filename_tem = "data_input/meteo_variables/1979-2020/temp_ERA5-BC-ensemble-prom_N30_1979-2020_fondef-dga_v1.csv"
)
# export meteo different versions (raw, pre-processed, ensemble-pre-processed)
#ERA5- ensemble promedio N=30
list(
  # 1979-2020
export_meteo(
    filename_pr = "data_input/meteo_variables/1979-2020/precip_ERA5-BC-ensemble-prom_N30_1979-2020_fondef-dga_v1.csv",
    filename_tem = "data_input/meteo_variables/1979-2020/temp_ERA5-BC-ensemble-prom_N30_1979-2020_fondef-dga_v1.csv"
  )
,
# 2020-2022
export_meteo(
  filename_pr = "data_input/meteo_variables/2020-2022/precip_ERA5-BC-ensemble-prom_N30_2020-2022_fondef-dga.csv",
  filename_tem = "data_input/meteo_variables/2020-2022/temp_ERA5-BC-ensemble-prom_N30_2020-2022_fondef-dga.csv"
)
,
# 2020-present
export_meteo(
  filename_pr = "data_input/meteo_variables/2022-presente/precip_ERA5-BC-ensemble-prom_N30_2022-presente_fondef-dga.csv",
  filename_tem = "data_input/meteo_variables/2022-presente/temp_ERA5-BC-ensemble-prom_N30_2022-presente_fondef-dga.csv"
)
) %>% 
join_files(filename_export = "data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_ens30avg_1979_present.csv") -> a
