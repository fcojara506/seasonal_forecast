library(dplyr)
library(data.table)
library(ComplexHeatmap)

df <- readRDS("data_output/scores/RDS/correlations_climates_indices_vol.RDS")
setDT(df)

# Split predictor_name into var, fun, and horizon_months
df <- df[, c("var", "fun", "horizon_months") := tstrsplit(predictor_name, "_", fixed = TRUE, keep = 1:3)][]
df <- df[, horizon_months := substr(horizon_months,1,1)][]
# Create catchment_month and predictor columns
df <- df[, catchment_month := paste(catchment_code, month_initialisation, sep = "_")][]
df <- df[, predictor := paste(var, horizon_months, sep = "_")][]

# Use dcast to reshape the data into a correlation matrix
cor_matrix <- dcast(df, catchment_month ~ predictor, value.var = "correlation", fill = NA)

# Annotation for rows
annotation_row <- data.table(catchment_month = cor_matrix[, catchment_month])
annotation_row <- annotation_row[, c("catchment_code", "month_initialisation") := tstrsplit(catchment_month, "_", fixed = TRUE, keep = 1:2)]
#annotation_row <- annotation_row[, month_initialisation := fct_reorder(month_initialisation, desc(month_initialisation))]
annotation_row <- annotation_row[, catchment_code := as.numeric(catchment_code)]
annotation_row <- annotation_row[, month_initialisation := factor(month_initialisation, levels = rev(levels(df$month_initialisation)))]
annotation_row <- tibble::column_to_rownames(annotation_row, var = "catchment_month")
rows_split <-  annotation_row$month_initialisation
annotation_row$month_initialisation <- NULL

# Annotation for columns
cor_matrix$catchment_month <- NULL
cor_matrix <- as.matrix(cor_matrix)
annotation_col <- data.table(predictor = colnames(cor_matrix))
annotation_col <- annotation_col[, c("var", "horizon_months") := tstrsplit(predictor, "_", fixed = TRUE, keep = 1:2)]
annotation_col <- annotation_col[, horizon_months:= factor(horizon_months)]
annotation_col <- annotation_col[, var:= factor(var)]
annotation_col <- tibble::column_to_rownames(annotation_col, var = "predictor")
cols_split <-  annotation_col$var
annotation_col$var <-  NULL

#### colors
length_horizon = length(levels(annotation_col$horizon_months))
colours_blue <- colorRampPalette(c("#190E53", "#3C3176", "#887FBC"))(length_horizon)

  ann_colors = list(
      horizon_months = setNames(colours_blue, levels(annotation_col$horizon_months)),
      catchment_code = c("firebrick",'green')
    )
  

  #Plot correlation matrix and absolute correlation matrix using pheatmap
  pheatmap((cor_matrix),
           annotation_row = annotation_row,
           annotation_col = annotation_col,
           row_split = rows_split,
           column_split = cols_split,
           annotation_legend = T,
           annotation_colors = ann_colors,
           cluster_row = F,
           cluster_cols = F,
           name = "corr (Spearman)"
           )
  

  
 

