library(ComplexHeatmap)
test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")

pheatmap(test)

annotation_col = data.frame(
  CellType = factor(rep(c("CT1", "CT2"), 5)), 
  Time = 1:5
)

rownames(annotation_col) = paste("Test", 1:10, sep = "")

annotation_row = data.frame(
  GeneClass = factor(rep(c("Path1", "Path2", "Path3"), c(10, 4, 6)))
)
rownames(annotation_row) = paste("Gene", 1:20, sep = "")

ann_colors = list(
  Time = c("white", "firebrick"),
  CellType = c(CT1 = "#1B9E77", CT2 = "#D95F02"),
  GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
)


pheatmap(test, annotation_col = annotation_col,
         annotation_row = annotation_row, 
         annotation_colors = ann_colors)

pheatmap(test, annotation_col = annotation_col,
         annotation_row = annotation_row, 
         annotation_colors = ann_colors, 
         row_split = annotation_row$GeneClass,
         column_split = annotation_col$CellType)
