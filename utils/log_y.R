# Clear environment
rm(list = ls())

# Set working directory
setwd("~/Documents/GitHub/seasonal_forecast")

# Load required packages
library(lubridate)
library(dplyr)
library(caret)

# Source required scripts
source("base/Preprocess_data.R")

# Function to check the normality of the predictand using the Shapiro-Wilk test
check_normality <- function(y) {
  shapiro_test <- shapiro.test(y)
  #cat("Shapiro-Wilk test p-value:", shapiro_test$p.value, "\n")
  return(shapiro_test$p.value)
}

# Function to visualize the distribution with a histogram and a Q-Q plot
plot_distribution <- function(y, title) {
  hist(y, main = title, xlab = "Volume")
  qqnorm(y, main = paste("Q-Q plot of", title))
  qqline(y)
}

# Function to apply a log transformation if the data is not normally distributed
apply_log_transform <- function(y, p_value) {
  if (p_value < 0.05) {
    #cat("The predictand is not normally distributed. Applying a log transformation.\n")
    y_log <- log(y)
    return(y_log)
  } else {
    #cat("The predictand is normally distributed.\n")
    return(y)
  }
}

# Function to generate a combined plot with histograms and Q-Q plots
plot_combined_distribution <- function(y, y_transformed, title) {
  par(mfrow = c(2, 2), mar = c(4, 4, 1, 1), oma = c(0, 0, 2, 0))
  
  hist(y, main = paste("Histogram of", title), xlab = "Volume")
  qqnorm(y, main = paste("Q-Q plot of", title))
  qqline(y)
  
  hist(y_transformed, main = paste("Histogram of the (transformed)", title), xlab = "Volume")
  qqnorm(y_transformed, main = paste("Q-Q plot of the (transformed)", title))
  qqline(y_transformed)
  
  title(main = "Combined Distribution Plot", outer = TRUE)
}

# Function to get the results based on catchment_code and month_initialisation parameters
get_results <- function(catchment_code, month_initialisation,charts = T) {
  # Define predictors
  data_input <- preprocess_data(
    datetime_initialisation = lubridate::make_date(2022, month_initialisation, 1),
    forecast_mode = "cv",
    catchment_code = catchment_code,
    predictor_list = c("STORAGE_last_1months")
  )
  
  # Check the normality of the predictand
  p_value <- check_normality(data_input$y_train$volume)
  
  # Apply a log transformation if the data is not normally distributed
  data_input$y_train$volume_transformed <- apply_log_transform(data_input$y_train$volume, p_value)
  
  # Recheck the normality after the log transformation (if applied)
  p_value_transformed <- check_normality(data_input$y_train$volume_transformed)
  
  if (charts) {
  # Plot the combined distribution chart
  plot_combined_distribution(data_input$y_train$volume, data_input$y_train$volume_transformed, "Predictand")
  }
  
  return(list(p_value = p_value,
              p_value_transformed = p_value_transformed,
              catchment_code = catchment_code,
              month_initialisation = month_initialisation))
}

# Set catchment code and months of initialization


r = get_results('4311001', 9,charts = T)
#all available catchments, no data 6008005, 7317005, 7355002, 8106001
catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.csv" 
cod_cuencas = read.csv(catchments_attributes_filename)$cod_cuenca [-c(32,40,45,49)]
cod_cuencas

results = list()
i = 1
for (month_initialisation in 9:12) {
  
for (catchment_code in cod_cuencas) {
  # Get the results and train the regression model
  results[[i]] <- get_results(catchment_code, month_initialisation,charts = F)
  i = i+1
  }
}

a = results %>%
  rbindlist() %>% 
  mutate(normal_step1 = p_value >= 0.05) %>% 
  mutate(normal_step2 = p_value_transformed >= 0.05) %>%
  mutate(step_status = case_when(
    normal_step1 == TRUE & normal_step2 == TRUE ~ "Normal in both steps",
    normal_step1 == TRUE & normal_step2 == FALSE ~ "Normal in step 1 only",
    normal_step1 == FALSE & normal_step2 == TRUE ~ "Normal in step 2 only",
    normal_step1 == FALSE & normal_step2 == FALSE ~ "Not normal in both steps"
  ))
# Replace geom_col() with geom_tile()
ggplot(data = a,
       mapping = aes(x = (month_initialisation),
                     y = as.character(catchment_code),
                     fill = normal_step2)) +
  geom_tile(color = "black") + # Add color parameter to separate the tiles
  scale_fill_manual(values = c("FALSE" = "#EBCC2A", "TRUE" = "#F21A00")) + # Set custom colors for TRUE and FALSE
  theme_minimal() +
  labs(x = "Month of Initialization",
       y = "Catchment Code",
       fill = "Normal?",
       title = "Log Transformation Status by Catchment and Month")


ggplot(data = a,
       mapping = aes(x = as.character(catchment_code),
                     y = (month_initialisation),
                     fill = step_status)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = c("Normal in both steps" = "#78B7C5",
                               "Normal in step 1 only" = "#3B9AB2",
                               "Normal in step 2 only" = "#E1AF00",
                               "Not normal in both steps" = "#F21A00")) +
  theme_minimal() +
  labs(x = "Catchment Code",
       y = "Month of Initialization",
       fill = "Normality Status",
       title = "Normality Status by Catchment and Month") +
  coord_flip() 



