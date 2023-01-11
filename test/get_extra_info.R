rm(list = ls())
source("base/Preprocess_data.R")
data4 = test_preprocess()
monthly_flows = data4$raw_data$monthly_flows