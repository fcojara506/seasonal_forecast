rm(list = ls())
library(dplyr)
library(data.table)

wy         <- function(x) {
  fifelse(lubridate::month(x) > 3,
          lubridate::year(x),
          lubridate::year(x) - 1)
}

month_to_wym <- function(month_as_char)  {
  wym = as.numeric(as.character(month_as_char))
  wym = ifelse(wym > 3, wym - 3, wym + 12 - 3)
  #wym = sprintf("%02d", wym)
  return(wym)
}

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
  
  if (!"i_ens" %in% colnames(df)) df$i_ens = 1
  
  df =
    df %>%
    melt.data.table(
      id.vars = c("date", "i_ens"),
      variable.name = "cod_cuenca",
      value.name = "var"
    ) %>% 
    mutate(cod_cuenca = as.character(cod_cuenca)) %>%
    mutate(wym = month_to_wym(lubridate::month(date))) %>%
    mutate(wy_simple = wy(date)) %>%
    select(-date) %>%
    .[, .(var = round(fx(var), 3)),
      by = list(cod_cuenca, wym, wy_simple, i_ens)]
  
  names(df)[names(df) == 'i_ens'] <- "ens"
  names(df)[names(df) == 'var'] <- var_name
  
  
  return(df)
}

export_meteo <- function(filename_pr,
                         filename_tem,
                         filename_export) {
  
  pr_monthly = aggregate_variable(filename_pr, "pr", sum)
  tem_monthly = aggregate_variable(filename_tem, "tem", mean)
  # merge variables and export
  meteo_input = merge(pr_monthly, tem_monthly)
  feather::write_feather(meteo_input, filename_export)
  
  message("Monthly data successfully exported ")
  
  return(filename_export)
}

#### storage
storage_variables_filename <- function(
    hydrological_model = "TUW",
    objective_function = "EVDSep",
    folder_storage_variables = "data_input/storage_variables") {
  
  target_folder =
    glue::glue("{folder_storage_variables}/",
               "{hydrological_model}/",
               "{objective_function}")
  
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
                            filename_export = glue::glue('hydro_variables_monthly_catchments_ChileCentral_{files_list$hydrological_model}_{files_list$objective_function}.feather')
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
  
  var = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
  var = as.data.table(var)[order(wy_simple)]
    
  feather::write_feather(x=var,path = filename_export )
  return(var)
}
