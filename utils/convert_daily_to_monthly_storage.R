rm(list = ls())
source("base/Transform_variables_to_monthly.R")

#storage
for (model in c("ERA5Ens")) {
  for (fo in c("KGE+logNSE","SKGE","SKGE+logSNSE")) {
  
  #filenames list
  storage_filenames = storage_variables_filename(
    hydrological_model = model,
    objective_function = fo,
    folder_storage_variables = "data_input/storage_variables"
  )
  # transform into data frame to use within model
  df= aggregate_hydro(files_list = storage_filenames)
  filename_export = df$fullname_filename_export

  # add new variable as the sum of the rest hydro variables
  df= new_hydro_variable(df = df$df,
                        selected_variables = storage_filenames$variables,
                        FUN = sum,
                        var_name = "STORAGE")
  df = select(df, c('wy_simple','wym',everything())) 
  
  
  # export
  #feather::write_feather(x=df, path = filename_export)
  write.csv(x = df,file = filename_export,row.names = F)
}
}
