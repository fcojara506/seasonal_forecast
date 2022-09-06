rm(list = ls())
library(feather)
library(data.table)

q_maule = read_feather("base/data_input/flows/flows_mm_monthly_catchments_RegionMaule.feather")
q_central = read_feather("base/data_input/flows/flows_mm_monthly_49catchments_ChileCentral.feather")
#c = read_feather("base/data_input/attributes/attributes_49catchments_ChileCentral.feather")

name_code=
alist(
Melado    = 7317005,
Longavi   = 7350003,
Maule     = 7321002,
Achibueno = 7354002,
Ancoa     = 7355002
)

df_maule = lapply(names(name_code), 
  function(cuenca)
  q_maule %>%
  subset(wy_simple >= 1981) %>% 
  subset(cod_cuenca %in% cuenca) %>%
  mutate(cod_cuenca = name_code[cuenca])
) %>%
  rbindlist() %>%
  data.table(key = c("wy_simple","wym"))

cod_cuencas = df_maule[["cod_cuenca"]] %>% 
  unique()

df_central = q_central %>%
  select(-month) %>% 
  subset(!(cod_cuenca %in% cod_cuencas)) %>%
  rbind(df_maule) %>%
  mutate(wym = as.numeric(wym)) %>% 
  data.table(key = c("wy_simple","wym")) %>% 
  tidyr::drop_na()

feather::write_feather(
  x = df_central, 
  path = "base/data_input/flows/flows_mm_monthly_49catchments_ChileCentral.feather"
)
