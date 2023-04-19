library(reshape2)
library(tidyverse)
library(pracma)
library(plotly)


create_3d_plot <- function(data, x_var, y_var, z_var, mesh_size = 0.2, margin = 0) {
  

  
  model = lm(formula = volume ~ STORAGE_mean_1months + SOI_mean_1months,data = data)
  x_min <- min(X[,x_var]) - margin
  x_max <- max(X[,x_var]) - margin
  y_min <- min(X[,y_var]) - margin
  y_max <- max(X[,y_var]) - margin
  xrange <- seq(x_min, x_max, mesh_size)
  yrange <- seq(y_min, y_max, mesh_size)
  xy <- meshgrid(x = xrange, y = yrange)
  xx <- xy$X
  yy <- xy$Y
  dim_val <- dim(xx)
  xx1 <- matrix(xx, length(xx), 1)
  yy1 <- matrix(yy, length(yy), 1)
  final <- cbind(xx1, yy1) %>% data.frame()
  names(final) = names(data %>% select(x_var, y_var))
  pred <-  predict(model,newdata = final,type = "response")
  
  #pred <- pred$.pred
  pred <- matrix(pred, dim_val[1], dim_val[2])
  
  fig <- plot_ly(data, x = as.formula(paste("~", x_var)), y = as.formula(paste("~", y_var)), z = as.formula(paste("~", z_var))) %>%
    add_markers(size = 5) %>%
    add_surface(x = xrange, y = yrange, z = pred, alpha = 0.65, type = 'mesh3d', name = 'pred_surface')
  
  
  fig
  return(fig)
}
# Format X_train and y_train
X_train_df <- rownames_to_column(data_input$X_train, var = "wy_simple")
y_train_df <- rownames_to_column(data_input$y_train, var = "wy_simple")[, c("wy_simple", "volume")]

# Merge X_train and y_train by water year
data <- merge(X_train_df, y_train_df, by = "wy_simple", all = TRUE) %>% 
  mutate(wy_simple = as.numeric(wy_simple))
# Usage example
x_var = "STORAGE_mean_1months"
y_var =  "SOI_mean_1months"
z_var =  "volume"


plot <- create_3d_plot(data, x_var,y_var,z_var) %>% 
  layout(scene = list(xaxis = list(title = "C.H.I (mm)"),
                      yaxis = list(title = "SOI"),
                      zaxis = list(title = "volumen (GL)")))
plot

