rm(list = ls())
source("base/Preprocess_data.R")
source("base/Regression_model.R")

#all available catchments, no data 6008005, 7317005, 7355002, 8106001
catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 
attributes_catchments = read.csv(catchments_attributes_filename)[-c(32,40,45,49),]
cod_cuencas = attributes_catchments$cod_cuenca


forecast_mode = "cv"
plots = list()

for (catchment_code in cod_cuencas) {

data_best_models = readRDS(file = paste0(
  "data_output/mejores_modelos_cuenca_mes/",
  catchment_code,
  "_may-mar.RDS"
))
best_combination = data_best_models$best_combination


a = lapply(5:9, function(month_initialisation) {
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


months_es <- c("ene", "feb", "mar","abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic")
months_wy <- c("abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic", "ene", "feb", "mar")

y_ens = a$y_ens %>% rbindlist()
y_ens$date_label = factor(paste0("1˚",months_es[y_ens$month_initialisation]),levels = paste0("1˚",months_wy))

df_train = a$df_train %>% rbindlist() %>% distinct()
data_input = a$data_input[[1]]

p1 = ggplot() +
  geom_boxplot(
    data = y_ens,
    aes(x = wy_simple,
        y = volume,
        fill = date_label),
        lwd = 0.1,
        outlier.size = 0.01
      )+
  geom_hline(aes(yintercept = mean(df_train$volume_original)))+
  geom_point(
    data = df_train,
    aes(x = wy_simple,
        y = volume_original,
        col = " "),
    shape = "_",
    size  = 10
  )+
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
  scale_fill_brewer() 
  #   # # change labels
  # labs(y =  glue("Volumen ({data_input$info$water_units$y})"),
  #      x = "Año hidrológico (orden cronológico)",
  #      fill = "Fecha de emisión",
  #      title =  glue("Pronóstico retrospectivo de volumen {data_input$time_horizon$volume_span_text} 1981-2019 para emisiones desde el 1 de mayo al 1 de septiembre"),
  #      subtitle = glue("{data_input$raw_data$attributes_catchment$gauge_name} ({catchment_code})")
  # )

plots[[catchment_code]] = p1
}


combined_plot = gridExtra::grid.arrange(grobs = plots, ncol = 1)
ggsave(plot = combined_plot,
       filename = paste0("data_output/figuras/hindcast_volumen/per_catchment/vol_hindcast_1may_1sep.png"),
       width = 14, height = 50)