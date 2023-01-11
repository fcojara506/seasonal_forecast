rm(list = ls())
source("base/Preprocess_data.R")
data4 <- test_preprocess()$data4
monthly_flows <- data4$raw_data$monthly_flows