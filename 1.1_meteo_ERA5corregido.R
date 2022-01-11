source("1.0_MAIN.R") # load configuration
source("1.x_WYtime_functions.R") # load time related functions

era_var           <- readRDS("data_input/forzERA5BC_hist1979-ACTUAL_qdm-ss_n40.RData") %>%
  reshape2::dcast(cuenca+date~variable,value.var = 'value') %>%
  setnames(old="tm",new="tem") %>%
  transform(yr    = year(date)) %>%
  data.table() %>%
  .[,wym:=wym_simple(date)]%>%
  .[,wy_simple:=wy_simple(date)]

era_var_monthly<- era_var %>%
  group_by(wy_simple,wym,cuenca) %>%
  summarise(tem = mean(tem),
            pr  = sum(pr))

feather::write_feather(era_var_monthly,paste0("data_input/forzantes_mensuales_1979_2021_era5corregido.feather"))

message(paste0("Última fecha disponible ERA5BC (Y-M-D):",max(era_var$date)) )
