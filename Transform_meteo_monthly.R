rm(list = ls())
library(dplyr)
library(data.table)

#directory = "/Users/fco/CAPTA/Pronostico_estacional/"
#setwd(directory)

wy_simple         <- function(x){
  fifelse(
    lubridate::month(x)>3,
    lubridate::year(x),
    lubridate::year(x) - 1)
  }

month_to_wym <- function(month_as_char)  {
  wym = as.numeric(as.character(month_as_char))
  wym = ifelse(wym>3,wym-3,wym+12-3)
  wym = sprintf("%02d", wym)
  return(wym)
}


aggregate_meteo_variable<- function(filename,var_name="pr",fx=sum) {
  library(data.table)
  
  variable_monthly = 
    data.table::fread(
      filename,
      check.names = F,
      stringsAsFactors=FALSE)
  
  if (!"i_ens" %in% colnames(variable_monthly)) variable_monthly$i_ens = 1
  
  variable_monthly = 
    variable_monthly %>% 
    melt.data.table(
      id.vars = c("date","i_ens"),
      variable.name = "cod_cuenca",
      value.name = "var") %>% 
    mutate(cod_cuenca = as.character(cod_cuenca)) %>% 
    mutate(wym = month_to_wym(lubridate::month(date))) %>% 
    mutate(wy_simple = wy_simple(date)) %>%
    select(-date) %>% 
    .[, .(var=round(fx(var),3)),
      by = list(cod_cuenca,wym,wy_simple,i_ens)]
  
  names(variable_monthly)[names(variable_monthly) == 'i_ens'] <- "ens"  
  names(variable_monthly)[names(variable_monthly) == 'var'] <- var_name
  
  
  return(variable_monthly)
}

export_meteo <- function(filename_pr,filename_tem,filename_export) {
  pr_monthly = aggregate_meteo_variable(filename_pr,"pr",sum)
  tem_monthly = aggregate_meteo_variable(filename_tem,"tem",mean)
  # merge variables and export
  meteo_input = merge(pr_monthly,tem_monthly)
  feather::write_feather(meteo_input,filename_export)
  message("Monthly data successfully exported ")
  return(filename_export)
}


# # export different versions (raw, pre-processed, ensemble-pre-processed)
# 
# #CR2MET
# export_meteo(
#   filename_pr = "data_input/precip_CR2MET_1979-2020_fondef-dga_v1.csv",
#   filename_tem = "data_input/temp_CR2MET_1979-2020_fondef-dga_v1.csv",
#   filename_export = "data_input/meteo_monthly_catchments_ChileCentral_CR2MET.feather"
# )
# 
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
export_meteo(
  filename_pr = "data_input/precip_ERA5-BC-ensemble-miembros_N50_1979_2020_fondef-dga_v1.csv",
  filename_tem = "data_input/temp_ERA5-BC-ensemble-miembros_N50_1979_2020_fondef-dga_v1.csv",
  filename_export = "data_input/meteo_monthly_catchments_ChileCentral_ens50.feather"
)

