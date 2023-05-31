library(dplyr)
library(data.table)
library(ComplexHeatmap)
rm(list = ls())

filename_input  <- "data_output/scores/RDS/correlations_climates_indices_vol.RDS"
metric_variable <- "correlation"
filename_export <- "correlation_vol_climate_indices_pheatmap"
legend_title <- "correlation (Spearman)"

plot_pheatmap_EDA <- function(
    filename_input,
    metric_variable,
    legend_title,
    filename_export,
  ) {
    df <- readRDS(filename_input)
    setDT(df)
    
    # Split predictor_name into var, fun, and horizon_months
    df <-
      df[, c("var", "fun", "horizon_months") := tstrsplit(predictor_name, "_", fixed = TRUE, keep = 1:3)][]
    df <- df[, horizon_months := substr(horizon_months, 1, 1)][]
    # Create catchment_month and predictor columns
    df <-
      df[, catchment_month := paste(catchment_code, month_initialisation, sep = "_")][]
    df <- df[, predictor := paste(var, horizon_months, sep = "_")][]
    
    # Use dcast to reshape the data into a correlation matrix
    cor_matrix <-
      dcast(df,
            catchment_month ~ predictor,
            value.var = metric_variable,
            fill = NA)
    
    # Annotation for rows
    annotation_row <-
      data.table(catchment_month = cor_matrix[, catchment_month])
    annotation_row <-
      annotation_row[, c("catchment_code", "month_initialisation") := tstrsplit(catchment_month, "_", fixed = TRUE, keep = 1:2)]
    #annotation_row <- annotation_row[, month_initialisation := fct_reorder(month_initialisation, desc(month_initialisation))]
    annotation_row <-
      annotation_row[, catchment_code := as.numeric(catchment_code)]
    annotation_row <-
      annotation_row[, month_initialisation := factor(month_initialisation, levels = rev(levels(df$month_initialisation)))]
    annotation_row <-
      tibble::column_to_rownames(annotation_row, var = "catchment_month")
    rows_split <-  annotation_row$month_initialisation
    annotation_row$month_initialisation <- NULL
    
    # Annotation for columns
    cor_matrix$catchment_month <- NULL
    cor_matrix <- as.matrix(cor_matrix)
    annotation_col <- data.table(predictor = colnames(cor_matrix))
    annotation_col <-
      annotation_col[, c("var", "horizon_months") := tstrsplit(predictor, "_", fixed = TRUE, keep = 1:2)]
    annotation_col <-
      annotation_col[, horizon_months := factor(horizon_months)]
    annotation_col <- annotation_col[, var := factor(var)]
    
    cols_split <-  annotation_col$var
    annotation_col$var <-  NULL # remove var from legend
    annotation_col$predictor <- NULL # remove predictor from legend
    colnames(cor_matrix) <- NULL #remove row labels
    
    #### colors
    length_horizon = length(levels(annotation_col$horizon_months))
    colours_blue <-
      colorRampPalette(c("#190E53", "#3C3176", "#887FBC"))(length_horizon)
    
    ann_colors = list(
      catchment_code = c("firebrick", 'green'),
      horizon_months = setNames(colours_blue, levels(annotation_col$horizon_months))
    )
    
    
    
    #Plot correlation matrix and absolute correlation matrix using pheatmap
    p <-
      pheatmap((cor_matrix),
               cluster_row = F,
               cluster_cols = F,
               annotation_row = annotation_row,
               annotation_col = annotation_col,
               row_split = rows_split,
               column_split = cols_split,
               #annotation_legend = T,
               annotation_names_col = F,
               annotation_names_row = F,
               annotation_colors = ann_colors,
               heatmap_legend_param = list(
                 at = c(-1, 0, 1),
                 title = legend_title,
                 legend_height = unit(3, "cm"),
                 title_position = "topleft"
               ),
               cell_fun = function(j, i, x, y, width, height, fill) {
                 if (cor_matrix[i, j] > -0.1 &
                     cor_matrix[i, j] < 0.1)
                   grid.text('x', x, y, gp = gpar(fontsize = 4))
               }
      )
    
    new_matrix = abs(cor_matrix)
    p_abs <-
      pheatmap(
        new_matrix,
        cluster_row = F,
        cluster_cols = F,
        annotation_row = annotation_row,
        annotation_col = annotation_col,
        row_split = rows_split,
        column_split = cols_split,
        #annotation_legend = T,
        annotation_names_col = F,
        annotation_names_row = F,
        annotation_colors = ann_colors,
        heatmap_legend_param = list(
          at = c(0, 0.25, 0.5, 0.75, 1.0),
          title = legend_title,
          legend_height = unit(3, "cm"),
          title_position = "topleft"
        ),
        cell_fun = function(j, i, x, y, width, height, fill) {
          if (new_matrix[i, j] < 0.1)
            grid.text('x', x, y, gp = gpar(fontsize = 4))
        }
      )
    
    #export in png format
    png(
      paste0(
        "data_output/scores/figures/",
        filename_export,
        today(),
        ".png"
      ),
      width = 10,
      height = 10,
      units = "in",
      res = 800
    )
    draw(p, merge_legend = TRUE)
    dev.off()
    #export absolute(matrix)
    png(
      paste0(
        "data_output/scores/figures/",
        filename_export,
        "_abs",
        today(),
        ".png"
      ),
      width = 10,
      height = 10,
      units = "in",
      res = 800
    )
    draw(p_abs, merge_legend = TRUE)
    dev.off()
  }
