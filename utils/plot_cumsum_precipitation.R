rm(list = ls())

source("base/Preprocess_data.R")
short_river_name <-
  function(var) {
    stringr::word(var, start = 1, end = 3)
  }

attributes_catchments =
  "base/data_input/attributes/attributes_49catchments_ChileCentral.feather" %>%
  feather::read_feather()

dataset_data_input <- function(
    cod_cuencas,
    dataset_meteo = "era5raw"#c(,"ens30avg") 
    ) {
  
  month_initialisation = "sep"
  dataset_region = "ChileCentral"
  #dataset_meteo  = dataset_meteo
  dataset_hydro  = "GR4J_EVDSep"
  horizon_strategy = "dynamic"
  horizon_month_start = "oct"
  horizon_month_end = "mar"
  predictor_list = c("pr_sum_-1months")
  wy_holdout = 2022
  remove_wys = NA
  
  predictors = list()
  
  for (catchment_code in cod_cuencas) {
    # catchment data (raw forcings, flows)
    catchment_data_ = catchment_data(
      catchment_code = catchment_code,
      dataset_region = dataset_region,
      dataset_meteo = dataset_meteo,
      dataset_hydro = dataset_hydro,
      remove_wys = remove_wys
    )
    
    # set target period to forecast
    forecast_horizon_ = forecast_horizon(
      month_initialisation = month_initialisation,
      horizon_strategy = horizon_strategy,
      horizon_month_start = horizon_month_start,
      horizon_month_end =  horizon_month_end
    )
    
    # create the predictors variables
    predictor_ = predictors_generator(
      predictor_list = predictor_list,
      forecast_horizon_ = forecast_horizon_,
      catchment_data = catchment_data_
    ) %>%
      select(-ens) %>%
      melt.data.table(id.vars = c("wy_simple"))
    
    predictors[[catchment_code]] = predictor_
    
  }
  
  data_input = predictors %>%
    rbindlist(idcol = "catchment_code") %>%
    merge(attributes_catchments,
          by.x = "catchment_code",
          by.y = "cod_cuenca") %>%
    mutate(short_gauge_name = short_river_name(gauge_name))
  
  return(data_input)
}

for (dataset_meteo in c("ens30avg")) { #"era5raw"
  

data_input =
  dataset_data_input(cod_cuencas = attributes_catchments$cod_cuenca,
                     dataset_meteo = dataset_meteo ) %>%
  merge(
    aggregate(
      formula = value ~ catchment_code ,
      data = subset(.,wy_simple != 2022),
      FUN = median
    ) %>%
      rename(median_value = value)
  )


gauge_names = unique(data_input$gauge_name)
names(gauge_names) = unique(data_input$catchment_code)

wy_year = unique(data_input$wy_simple)

library(ggplot2)
p =
  ggplot(data = data_input,
         mapping = aes(
           x = as.factor(wy_simple),
           y = value,
           fill = ifelse(value > median_value, "> mediana", "<= mediana")
         )) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = median_value)) +
  ylim(0,NA)+
  geom_text(aes(label = sprintf("%0.0f", value)),vjust = 0,size=2)+
  scale_x_discrete(expand = c(0.02, 0.02), breaks = seq(1950, 2050, 10)) +
  scale_fill_manual(values = c("red", "black")) +
  theme(legend.position = "bottom") +
  labs(x = "Año hidrológico (1 de abril a la fecha)",
       y = "Precipitación (mm)",
       title = "Precipitación acumulada por cuenca",
       fill = "",
       caption = "Actualizado con datos derivados de ERA5T+CR2MET (promedio 30 miembros) hasta el 16 de agosto de 2022"
       ) +
  facet_wrap(
    ~ catchment_code,
    scale = "free",
    labeller = labeller(catchment_code = gauge_names),
    ncol = 1
  )


ggsave(
  glue(
  "utils/data_output/figuras/cumsum_precipitacion_49catchment_{dataset_meteo}.png"
  ),
  dpi = 500,
  width = 7,
  height = 80,
  limitsize = F,
  plot = p
)
}