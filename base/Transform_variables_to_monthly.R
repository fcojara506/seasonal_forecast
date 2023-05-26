rm(list = ls())
source("base/DatesWaterYear.R")
library(dplyr)
library(data.table)



get_var_name<- function(filename) {
  toupper(tools::file_path_sans_ext(basename(filename)))
}

aggregate_variable <- function(filename,
                               var_name = "pr",
                               fx = sum) {
  library(data.table)
  
  df =
    data.table::fread(
      filename,
      check.names = F,
      stringsAsFactors = FALSE,
      header = TRUE
    )
  colnames(df)[1] = "date"
  
  # Ensure all columns except 'date' are of type double
  cols_to_convert <- setdiff(names(df), "date")
  df[ , (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]
  
  #if (!"i_ens" %in% colnames(df)) df$i_ens = 1
  
  df =
    df %>%
    melt.data.table(
      id.vars = "date",
      variable.name = "cod_cuenca",
      value.name = "var"
    ) %>% 
    mutate(cod_cuenca = as.character(cod_cuenca)) %>%
    mutate(wym = wateryearmonth(lubridate::month(date))) %>%
    mutate(wy_simple = wateryear(date)) %>%
    select(-date) %>%
    .[, .(var = round(fx(var), 3)),
      by = list(cod_cuenca, wym, wy_simple)]
  
 
  names(df)[names(df) == 'var'] <- var_name
  
  
  return(df)
}

export_meteo <- function(filename_pr,
                         filename_tem,
                         filename_export=NULL) {
  
  pr_monthly = aggregate_variable(filename_pr, "pr", sum)
  tem_monthly = aggregate_variable(filename_tem, "tem", mean)
  # merge variables and export
  meteo_input = merge(pr_monthly, tem_monthly)
  if (!is.null(filename_export)) {
    #feather::write_feather(meteo_input, filename_export)
    write.csv(x = meteo_input,file = filename_export,row.names = F)
    message("Monthly data successfully exported ")
  }
  
  return(meteo_input)
}


#### storage
storage_variables_filename <- function(
    hydrological_model = NULL,
    objective_function = NULL,
    folder_storage_variables = "data_input/storage_variables") {
  
  target_folder = glue::glue("{folder_storage_variables}",
                             if (!is.null(hydrological_model)) {glue::glue("/{hydrological_model}")} else {""},
                             if (!is.null(objective_function)) {glue::glue("/{objective_function}")} else {""})
  
  
  files_list =
    list.files(path = target_folder,
               pattern = ".csv",
               full.names = TRUE)
  
  return(list(
    filenames = files_list,
    hydrological_model = hydrological_model,
    objective_function = objective_function,
    folder = folder_storage_variables,
    variables = lapply(files_list, get_var_name) %>% unlist()
  ))
}

aggregate_hydro <- function(files_list,
                            filename_export = glue::glue('hydro_variables_monthly_catchments_ChileCentral_{files_list$hydrological_model}_{files_list$objective_function}.csv')
                            ) {
  
  df  =
  files_list$filenames %>% 
  lapply(function(filename)
    
  aggregate_variable(
    filename = filename,
    var_name = get_var_name(filename),
    fx = last
  )
  
  )
  
  df = Reduce(function(x, y) merge(x, y, all=TRUE), df)
  
  fullname_filename_export = glue::glue("{files_list$folder}/{filename_export}")
  
  return(
    list(
      df = df,
      fullname_filename_export = fullname_filename_export
    )
  )
  
}

new_hydro_variable <- function(df,selected_variables,FUN,var_name) {
  
  df2 = df %>% 
    select(all_of(selected_variables)) %>% 
    apply(1, FUN) %>% 
    data.frame()
  
  colnames(df2) = var_name
  
  df = cbind(df,df2)
  
  return(df)
}

# join files
join_files <- function(df_list,filename_export) {
  library(data.table)
  var = rbindlist(df_list)#Reduce(function(x, y) merge(x, y,all=T), df_list)
  var = as.data.table(var)[order(wy_simple)]
  var = select(var, c('wy_simple','wym',everything()))  
  #feather::write_feather(x=var,path = filename_export )
  write.csv(x = var,file = filename_export,row.names = F)
  return(var)
}
