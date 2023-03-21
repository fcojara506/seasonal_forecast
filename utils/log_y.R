# Clear environment
rm(list = ls())

# Load required packages
library(lubridate)
library(dplyr)
library(caret)

# Source required scripts
source("base/Preprocess_data.R")
source("base/DatesWaterYear.R")

#catchment_code = '4311001'
#month_initialisation = 9

# Function to get the results based on catchment_code and month_initialisation parameters
get_results <- function(catchment_code, month_initialisation) {
  # Define predictors
  data_input <- preprocess_data(
    datetime_initialisation = lubridate::make_date(2022, month_initialisation),
    forecast_mode = "cv",
    catchment_code = catchment_code,
    predictor_list = c("pr_sum_-1months"),
    y_transform = list(log_transform = T, plot_transform_predictant = T)
  )
 
  return(list(p_value = data_input$info$y_transform$p_value,
              p_value_transformed = data_input$info$y_transform$p_value_transformed,
              catchment_code = catchment_code,
              month_initialisation = month_initialisation))
}



#all available catchments, no data 6008005, 7317005, 7355002, 8106001
catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 
cod_cuencas = read.csv(catchments_attributes_filename)$cod_cuenca [-c(32,40,45,49)]


results = list()
i = 1
for (month_initialisation in c(9:12,1:3)) {
for (catchment_code in cod_cuencas) {
  print(paste(catchment_code,month_initialisation))
  # Get the results and train the regression model
  results[[i]] <- get_results(catchment_code, month_initialisation)
  i = i+1
  }
}

#months_year <- c("ene", "feb", "mar","abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic")
months_wy <- c("abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic", "ene", "feb", "mar")

p_values_preproceso = results %>%
  rbindlist() %>% 
  mutate(normal_step1 = p_value >= 0.05) %>% 
  mutate(normal_step2 = p_value_transformed >= 0.05) %>%
  mutate(step_status = case_when(
    normal_step1 == TRUE & normal_step2 == TRUE ~ "Normal en ambos pasos",
    normal_step1 == TRUE & normal_step2 == FALSE ~ "Normal originalmente",
    normal_step1 == FALSE & normal_step2 == TRUE ~ "Normal al transformar",
    normal_step1 == FALSE & normal_step2 == FALSE ~ "Nunca normal"
  )) %>%
  mutate(month_wy = as.factor(wateryearmonth(month_initialisation)))


levels(p_values_preproceso$month_wy) = paste0("1˚ ", months_wy[as.numeric(levels(p_values_preproceso$month_wy) )])


ggplot(data = p_values_preproceso,
       mapping = aes(x = as.character(catchment_code),
                     y = month_wy,
                     fill = step_status))+
  geom_tile(color = "black")+
  scale_fill_manual(values = c("Normal en ambos pasos" = "#78B7C5",
                               "Normal originalmente" = "#3B9AB2",
                               "Normal al transformar" = "#E1AF00",
                               "Nunca normal" = "#F21A00")) +
  theme_minimal() +
  labs(x = "Código DGA Cuenca",
       y = "Fecha de emisión",
       fill = "Normalidad",
       title = "Normalidad del volumen según test de Shapiro-Wilk") +
  coord_flip() 

ggsave(filename = "data_output/figuras/normalidad_Shapiro-Wilk_cuencas_sep_dic.png",
       width = 8, height = 8,dpi = 400)


