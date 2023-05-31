rm(list = ls())
source("base/Preprocess_data.R")
source("base/Regression_model.R")
library(forcats)
#all available catchments, no data 6008005, 7317005, 7355002, 8106001
catchments_attributes_filename = "data_input/attributes/attributes_45catchments_ChileCentral.csv"
attributes_catchments = read.csv(catchments_attributes_filename)
cod_cuencas = attributes_catchments$cod_cuenca

# Divide catchments into blocks
cod_cuencas = data.frame(cod_cuencas) %>% 
  mutate(rank = min_rank(cod_cuencas),  # rank the catchment codes
         block = as_factor(ntile(rank, 3))) # divide the ranks into 3 equal-sized groups


data_hindcast <- function(cod_cuencas) {
  months_es <- c("ene", "feb", "mar","abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic")
  months_wy <- c("abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic", "ene", "feb", "mar")
  

  forecast_mode = "cv"
  plots = list()
  df_train_list = list()
  y_ens_list = list()
for (catchment_code in cod_cuencas) {

data_best_models = readRDS(file = paste0(
  "data_input/mejores_predictores_cuenca_mes/",
  catchment_code,
  "_may-mar.RDS"
))
best_combination = data_best_models$best_combination


a = lapply(c(5,7,9), function(month_initialisation) {
best_combination = best_combination[best_combination$month_initialisation == month_initialisation, ]


datetime_emission = lubridate::make_date(2022, month_initialisation)
if (month_initialisation < 4) {
  datetime_emission = datetime_emission %m+% months(12)
}

## best combination of predictors
data_input_best <- preprocess_data(
  datetime_initialisation = datetime_emission,
  forecast_mode = forecast_mode,
  catchment_code = catchment_code,
  predictor_list = unlist(best_combination$predictors),
  remove_wys = c(2020,2021),
  save_raw = T
)

# ensemble volume forecast
data_fore_best = forecast_vol_ensemble(data_input = data_input_best,
                                       forecast_mode = forecast_mode)

y_ens = data_fore_best$y_ens_cv
y_ens = y_ens[ , order(colnames(y_ens))] %>% data.table()

y_ens = melt.data.table(y_ens,
                        variable.name = "wy_simple",
                        value.name = "volume",
                        measure.vars = colnames(y_ens),
                        variable.factor = F)

y_ens$catchment_code = catchment_code
y_ens$wy_simple = as.factor(y_ens$wy_simple)
y_ens$month_initialisation = month_initialisation
df_train = data_input_best$y_train %>% rownames_to_column(var = "wy_simple")
list(y_ens = y_ens,df_train = df_train, data_input = data_input_best)
}
) %>% purrr::transpose()



y_ens = a$y_ens %>% rbindlist()
y_ens$date_label = factor(paste0("1˚",months_es[y_ens$month_initialisation]),levels = paste0("1˚",months_wy))

df_train = a$df_train %>% rbindlist() %>% distinct()
df_train$catchment_code = as.integer(catchment_code)

df_train_list[[catchment_code]] = df_train
y_ens_list[[catchment_code]] = y_ens
}
  

y_ens = rbindlist(y_ens_list)
df_train = rbindlist(df_train_list)
return(list(y_ens = y_ens, df_train = df_train))
}


plot_hindsight <- function(df,attributes_catchments = attributes_catchments) {
 

p=
ggplot() +
  geom_boxplot(
    data = df$y_ens,
    aes(x = wy_simple,
        y = volume,
        fill = date_label),
    lwd = 0.1,
    outlier.size = 0.01
  )+
  geom_point(
    data = df$df_train,
    aes(x = wy_simple,
        y = volume_original,
        col = " "),
    shape = "_",
    size  = 5
  )+
  #geom_hline(aes(yintercept = mean(df$df_train$volume_original)))+
  facet_wrap(~gauge_name,ncol = 1,scales = "free_y")+
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  )) +
  
  scale_color_manual(values = c(" " = "red"),
                     name = "Caudal estación Fluviométrica")+
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0, 'cm')) +
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_brewer()+
labs(y =  glue("Volumen (GL = mill. m3)"),
     x = "Año hidrológico (orden cronológico)",
     fill = "Fecha de emisión",
     title =  glue("Pronóstico retrospectivo de volumen sep-mar 1981-2019 para emisiones desde el 1 de mayo al 1 de septiembre"),
)   +guides(
  fill = guide_legend(
    label.position = "bottom",
    nrow = 1,
    title = "Emisión",
    title.vjust = 0.8
  ),
  color=guide_legend(
    title = "Medido ",
    title.vjust = 0.9,
    nrow=2
  )
)

return(p)
}
plots = list()
# Process each block
for (block_i in as.integer(unique(cod_cuencas$block))) {
  df = data_hindcast( filter(cod_cuencas,block == block_i)$cod_cuenca)
  
  df$y_ens  = merge.data.frame(df$y_ens ,
                               attributes_catchments,
                               by.x = "catchment_code",
                               by.y = "cod_cuenca")
  
  df$y_ens$gauge_name = factor(df$y_ens$gauge_name, levels = unique(df$y_ens$gauge_name))
  
  df$df_train  = merge.data.frame(df$df_train ,
                                  attributes_catchments,
                                  by.x = "catchment_code",
                                  by.y = "cod_cuenca")
  
  df$df_train$gauge_name = factor(df$df_train$gauge_name, levels = unique(df$df_train$gauge_name))
  
  
  
  p = plot_hindsight(df)
  ggsave(plot = p,
         filename = glue("data_output/figuras/informe_final/hindcast_volumen/vol_hindcast_1may_1sep_block{block_i}.png"),
         width = 10, height = 15)
}



