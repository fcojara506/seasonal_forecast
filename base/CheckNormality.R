


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