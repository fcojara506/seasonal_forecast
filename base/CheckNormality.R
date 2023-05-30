# ----------------------------------------------------------------------------
# Nombre del Proyecto: Pronóstico híbridos del volumen estacional en Chile Central 
# Autor(es): Francisco Jara
# Fecha de Finalización: 2023/04/31
# Contacto: pronostico.caudales@gmail.com
# GitHub: https://github.com/fcojara506
# ----------------------------------------------------------------------------



# Function to check the normality of the predictand using the Shapiro-Wilk test
check_normality <- function(y) {
  shapiro_test <- shapiro.test(y)
  #cat("Shapiro-Wilk test p-value:", shapiro_test$p.value, "\n")
  return(shapiro_test$p.value)
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

pass_test <- function(p_value,threshold = 0.05) {
  if (p_value>=threshold) {
    p_value_test = "(pasa)"
  }else{p_value_test = "(no pasa)"}
  
  return(p_value_test)
}
# Function to generate a combined plot with histograms and Q-Q plots
plot_transform_predictant_distribution <- function(y, y_transformed,p_value, p_value_transform, title_text, chart_name) {
  

  png(filename = chart_name,width = 1500,height = 1500,res=200)
  
  par(mfrow = c(2, 2), mar = c(4, 4, 1, 1), oma = c(0, 0, 2, 0))
  
  hist(y, main = paste("Histograma original"), xlab = "Volumen",ylab = "Frecuencia")
  qqnorm(y, main = paste("Q-Q plot original"), xlab = "Cuantiles Teóricos",ylab = "Cuantiles de la muestra")
  qqline(y)
  text(x = 0.2,
       y = 1*min(y),
       paste("P-value Shapiro-Wilk test:", 
             format(p_value, scientific = T, big.mark = ",",digits = 3), pass_test(p_value) ))
  
  
  hist(y_transformed, main = paste("Histograma transformado"), xlab = "log Volumen",ylab = "Frecuencia")
  qqnorm(y_transformed, main = paste("Q-Q plot transformado"), xlab = "Cuantiles Teóricos",ylab = "Cuantiles de la muestra")
  qqline(y_transformed)
  text(x = 0.2,
       y = 1*min(y_transformed),
       paste("P-value Shapiro-Wilk test:",
             format(p_value_transform, scientific = T, big.mark = ",",digits = 3), pass_test(p_value_transform)))
  
  title(main = paste0("Transformación del volumen ", title_text),adj=0, outer = TRUE)
  dev.off()
  }
