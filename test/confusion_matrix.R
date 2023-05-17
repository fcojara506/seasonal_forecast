library(caret)
obs=
c("húmedo",
  "seco",
  "seco",
  "normal",
  "húmedo",
  "normal",
  "seco",
  "húmedo"
  ) %>% 
  factor()


pred =
c(
"normal",
"seco",
"normal",
"húmedo",
"normal",
"normal",
"seco",
"húmedo"
)%>% 
  factor()

caret::confusionMatrix(data = pred,reference = obs)
