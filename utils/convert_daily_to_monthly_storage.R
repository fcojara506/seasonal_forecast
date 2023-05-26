rm(list = ls())
source("base/Transform_variables_to_monthly.R")

# # PERIODO HISTORICO
# #storage
# model="ERA5Ens"
# fo="historico"
#
#   #filenames list
#   storage_filenames = storage_variables_filename(
#     hydrological_model = model,
#     objective_function = fo,
#     folder_storage_variables = "data_input/storage_variables"
#   )
#   # transform into data frame to use within model
#   df= aggregate_hydro(files_list = storage_filenames)
#   filename_export = df$fullname_filename_export
#
#   # add new variable as the sum of the rest hydro variables
#   df= new_hydro_variable(df = df$df,
#                         selected_variables = storage_filenames$variables,
#                         FUN = sum,
#                         var_name = "STORAGE")
#   df = select(df, c('wy_simple','wym',everything()))
#
#   # export
#   write.csv(x = df,file = filename_export,row.names = F)
#
#
convertir_almacenamientos_diarios_a_mensuales <- function(model = "ERA5Ens",
                                                          fo = "operacional") {
  

#storage

# PERIODO NUEVO

#filenames list
storage_filenames = storage_variables_filename(
  hydrological_model = model,
  objective_function = fo,
  folder_storage_variables = "data_input/storage_variables"
)
# transform into data frame to use within model
df = aggregate_hydro(files_list = storage_filenames)
filename_export = df$fullname_filename_export

# add new variable as the sum of the rest hydro variables
df = new_hydro_variable(
  df = df$df,
  selected_variables = storage_filenames$variables,
  FUN = sum,
  var_name = "STORAGE"
)
df = select(df, c('wy_simple', 'wym', everything()))

# export operational
write.csv(x = df,file = filename_export,row.names = F)
}
convertir_almacenamientos_diarios_a_mensuales()
