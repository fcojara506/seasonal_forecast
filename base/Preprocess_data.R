#rm(list = ls())

library(data.table)
library(dplyr)
library(lubridate)
library(glue)
library(tibble)

# import local libraries
source("base/Convert_units.R")

months_wy = c('abr',
              'may',
              'jun',
              'jul',
              'ago',
              'sep',
              'oct',
              'nov',
              'dic',
              'ene',
              'feb',
              'mar')

get_dataset_filenames <- function(dataset_meteo,dataset_hydro) {
  
    catchments_attributes_filename = "data_input/attributes/attributes_49catchments_ChileCentral.feather"
    
    flows_filename                 = "data_input/flows/flows_mm_monthly_49catchments_ChileCentral.feather"
    
    hydro_filename                 = glue::glue(
      "data_input/storage_variables/",
      "hydro_variables_monthly_catchments_ChileCentral_{dataset_hydro}.feather"
      )
    
    meteo_filename                 = glue::glue(
      "data_input/meteo_variables/",
      "meteo_monthly_catchments_ChileCentral_{dataset_meteo}_1979_2022.feather"
    )
    
    climateindex_filename = "data_input/climate_index_variables/indices_mensuales_1988_2020.feather"
    
    
    return(
      list(
        catchments_attributes_filename = catchments_attributes_filename,
        flows_filename = flows_filename,
        meteo_filename = meteo_filename,
        hydro_filename = hydro_filename,
        climateindex_filename = climateindex_filename
      )
    )
    
  }

read_and_subset_variable <- function(filename, catchment_code) {
  
  df_variable = data.table(
    cod_cuenca = factor(),
    wy_simple = numeric(),
    wym = numeric()
  )
  
  if (file.exists(filename)) {
    df_variable = feather::read_feather(filename) %>%
      mutate(wym = as.numeric(wym)) %>%
      data.table(key = c("wy_simple", "wym")) %>%
      subset(cod_cuenca == catchment_code, select = -cod_cuenca)
  }
  
  return(df_variable)
}

get_catchment_data <- function(catchment_code,
                           dataset_meteo,
                           dataset_hydro,
                           remove_wys,
                           info_list) {
  
  data_filenames_ = 
    get_dataset_filenames(
      dataset_meteo,
      dataset_hydro)
  
  attributes_catchment = data_filenames_$catchments_attributes_filename %>%
    feather::read_feather() %>%
    subset(cod_cuenca == catchment_code)
  
  monthly_flows        = read_and_subset_variable(
    filename = data_filenames_$flows_filename,
    catchment_code = catchment_code) %>%
    remove_years(remove_wys)
  
  monthly_meteo       <- read_and_subset_variable(
    filename = data_filenames_$meteo_filename,
    catchment_code = catchment_code) %>%
    remove_years(remove_wys)
  
  monthly_hydro       <- read_and_subset_variable(
    filename = data_filenames_$hydro_filename,
    catchment_code = catchment_code) %>%
    remove_years(remove_wys)
  
  monthly_climateindex <- data_filenames_$climateindex_filename %>% 
    feather::read_feather() %>%
    mutate(wym = as.numeric(wym)) %>% 
    mutate(ens = 1)
  
  monthly_flows = monthly_flows %>%
    data.table() %>%
    mutate(date = lubridate::dmy(paste0("01",
                                        sprintf("%02d", wy_month_to_month(wy_month = wym)),
                                        wy_to_year(wy = wy_simple,wym = wym)))) %>% 
    mutate(days_months = lubridate::days_in_month(date)) %>%
    select(-date) %>% 
    mutate(Q_converted = hydromad::convertFlow(
      Q_mm,
      from = "mm/month",
      to = info_list$units_q,
      area.km2 = attributes_catchment$area_km2)) %>%
    select(-days_months)
  
  ################# modify this matrix accordingly
 
  raw_data_df =  Reduce(function(x, y) merge(x, y, all=TRUE,by = c("wy_simple", "wym", "ens")),
                        list(monthly_meteo,monthly_hydro,monthly_climateindex) )

  
  return(
    list(
      monthly_flows = monthly_flows ,
      monthly_meteo = monthly_meteo ,
      monthly_hydro = monthly_hydro ,
      monthly_climateindex = monthly_climateindex,
      raw_data_df = raw_data_df,
      attributes_catchment = attributes_catchment
    )
  )
}

### FORECASTING MONTHS HORIZON
get_forecast_horizon <- function(
    month_initialisation,
    horizon_strategy,
    horizon_month_start = "oct",
    horizon_month_end =  "mar"
    ) {
  # indices within months_wy vector
  month_initialisation_index = match(month_initialisation, months_wy)
  init_target_index = match(horizon_month_start, months_wy)
  end_target_index = match(horizon_month_end, months_wy)
  
  # start month
  if (horizon_strategy == "dynamic") {
    if (month_initialisation_index < init_target_index) {
      init_forecast_index = init_target_index
    } else{
      init_forecast_index = month_initialisation_index
    }
  } else if (horizon_strategy == 'static') {
    init_forecast_index = init_target_index
  }
  
  # end month
  if (horizon_strategy == "dynamic") {
    end_forecast_index = end_target_index
  } else if (horizon_strategy == 'static') {
    end_forecast_index = end_target_index
  }
  
  # PREVIOUS MONTHS OF FORECAST
  months_before_initialisation = months_wy[1:month_initialisation_index - 1]
  
  # FINAL PERIOD OF FORECAST
  months_forecast_period = months_wy[init_forecast_index:end_forecast_index]
  
  
  return(
    list(
      init_forecast_index = init_forecast_index,
      end_forecast_index = end_forecast_index,
      months_forecast_period = months_forecast_period,
      months_before_initialisation = months_before_initialisation,
      month_initialisation_index = month_initialisation_index,
      month_initialisation =  month_initialisation,
      months_wy = months_wy
    )
  )
}

split_predictor_name <- function(predictor_name,month_initialisation_index) {
  # var_name, consists of 3 attributes: variable, aggregation function, aggregation period
  
  predictor_list = strsplit(predictor_name, "_")[[1]]
  
  predictor_variable = predictor_list[1]
  predictor_function = predictor_list[2]
  predictor_horizon = as.numeric(strsplit(predictor_list[3], "months")[[1]])
  
  #condition to consider the whole period of the current wy
  if (predictor_horizon < 0) {
    predictor_horizon = month_initialisation_index - 1
  }
  # new name
  predictor_newname = glue::glue('{predictor_variable}_{predictor_function}_{predictor_horizon}months')
  
  return(list(
    predictor_oldname = predictor_name,
    predictor_newname = predictor_newname,
    predictor_variable = predictor_variable,
    predictor_function = predictor_function,
    predictor_horizon = predictor_horizon
  ))
}

# one predictor column generator
one_column_predictor <- function(predictor_name,month_initialisation_index,catchment_data) {
    
    predictor_attributes = split_predictor_name(
      predictor_name = predictor_name,
      month_initialisation_index = month_initialisation_index) 
      
    input_data = catchment_data$raw_data_df
    num_ensembles = unique(input_data$ens)
    
    if (is.null(num_ensembles)) {input_data$ens = 1}
    
    tryCatch({
      
      var = input_data %>%
        select("wy_simple", all_of(predictor_attributes$predictor_variable), "ens",wym) %>% 
        subset(wym < month_initialisation_index) %>%
        subset(wym > month_initialisation_index - predictor_attributes$predictor_horizon -1) %>%
        select("wy_simple", all_of(predictor_attributes$predictor_variable), "ens")
      
      num_records_per_wy <- var %>% 
        count(wy_simple) %>% 
        dplyr::rename(num = n)
      
      kickout_wy <- num_records_per_wy %>% 
        filter(num != predictor_attributes$predictor_horizon) %>% 
        pull(wy_simple)
      
      if (length(kickout_wy)>0) {
        message("Warning. Removing INCOMPLETE PREDICTORS, WATER-YEARS: ",
                unique(interval_wys(kickout_wy)))
      }
      
      var = var %>%
        subset(!(wy_simple %in% kickout_wy)) %>%
        aggregate(
          FUN = predictor_attributes$predictor_function,
          by = list(wy_simple = .$"wy_simple", ens = .$"ens"),
          drop = T
        ) %>%
        select(c("wy_simple",
                 "ens",
                 all_of(predictor_attributes$predictor_variable)))
      
      names(var)[names(var) == predictor_attributes$predictor_variable] <- predictor_attributes$predictor_newname
      
      return(var)
    }
    ,
    error = function(e) {NULL})
  }
## predictor dataframe
predictors_generator <- function(predictor_list,
                                 forecast_horizon_,
                                 catchment_data) {
  
  predictors = lapply(predictor_list,
                      function(predictor_name)
                        
                        one_column_predictor(
                          predictor_name = predictor_name,
                          month_initialisation_index =  forecast_horizon_$month_initialisation_index,
                          catchment_data = catchment_data
                        )
                      )
  
  predictors[sapply(predictors, is.null)] <- NULL
  
  
  predictors = Reduce(merge, predictors) %>%
    data.table(key = "wy_simple,ens") %>%
    tidyr::drop_na()
  
  return(predictors)
}

# target variable : volume and streamflow
predictant_generator <- function(
    forecast_horizon_,
    catchment_data,
    info_list,
    extra_info) {
  
  library(stats)
  library(dplyr)
  cond1 = catchment_data$monthly_flows$wym >= forecast_horizon_$init_forecast_index
  cond2 = catchment_data$monthly_flows$wym <= forecast_horizon_$end_forecast_index
  
  q_period_wy = catchment_data$monthly_flows %>%
    subset(cond1 & cond2) %>%
    select(c("wy_simple", "Q_mm", "wym")) %>%
    mutate(wy_simple = as.integer(wy_simple))
  
  #volume in mm
  y = stats::aggregate(
      x = Q_mm ~ wy_simple ,
      FUN = 'sum',
      data = q_period_wy
      ) %>% dplyr::rename(volume = Q_mm)
    
  # streamflow in mm
  q = q_period_wy %>%
    dcast(wy_simple ~ wym ,
          value.var = "Q_mm")
  
  
  names(q)[names(q) != "wy_simple"] = forecast_horizon_$months_forecast_period
  
  ## convert units
  q_converted = 
    convert_flow(q = select(q,-wy_simple),
                 from = "mm/month",
                 to = info_list$units_q,
                 area_km2 = catchment_data$attributes_catchment$area_km2,
                 days_per_month = extra_info$days_per_month_horizon
                 )
  
  y_converted = 
    convert_vol(
      v = select(y,-wy_simple),
      from = "mm",
      to = info_list$units_y,
      area_km2 = catchment_data$attributes_catchment$area_km2
  )
  
  #add water year column
  q_converted$wy_simple = q$wy_simple
  y_converted$wy_simple = y$wy_simple
  
  return(list(y = y_converted, q = q_converted))
}

remove_years <- function(variable, wys) {
  variable_train = variable[!(variable$wy_simple %in% wys), ]
  return(variable_train)
}

subset_years <- function(variable, wy_train) {
  variable_train = variable[variable$wy_simple %in% wy_train, ] %>%
    tibble::as_tibble() %>%
    tibble::column_to_rownames(var = "wy_simple")
  
  return(variable_train)
}

interval_wys <- function(wy_train) {
  lapply(split(wy_train, c(0, cumsum(
    diff(wy_train) != 1
  ))),
  function(y)
    if (length(y) > 0)
      c(head(y, 1), tail(y, 1))
  else
    y)
}


dataframe_to_list <- function(df) {
  ensemble_names = unique(df$ens)
  df_list = lapply(ensemble_names,
                   function(ens_i)
                     subset(df, ens == ens_i, select = -ens))
  names(df_list) = ensemble_names
  return(df_list)
}


training_data <- function(predictor, predictant, wy_holdout) {
  wy_train = intersect(predictor$wy_simple, predictant$q$wy_simple)
  wy_train = wy_train[wy_train != wy_holdout]
  
  ### training period
  X_train = predictor %>%
    dataframe_to_list %>%
    lapply(function(x)
      subset_years(x, wy_train))
  
  #X_train = predictor %>% subset_years(wy_train)
  y_train = predictant$y %>% subset_years(wy_train)
  q_train = predictant$q %>% subset_years(wy_train)
  return(
    list(
      X_train = X_train,
      y_train = y_train,
      q_train = q_train,
      wy_train = wy_train,
      wy_holdout = wy_holdout
    )
  )
}

testing_data <- function(predictor, predictant, wy_holdout) {
  
  
  ### testing period
  if (wy_holdout %in% predictor$wy_simple) {
    
    X_test = predictor %>%
      dataframe_to_list %>%
      lapply(function(x)
        subset_years(x, wy_holdout))
    
  } else{
    X_test = NULL
    message("PREDICTOR INTERVAL WYS: ", interval_wys(predictor$wy_simple))
    stop("YOU NEED VALID PREDICTORS FOR THE TARGET WY (SEE DATA FOR WY_HOLDOUT)")
  }
  
  if (wy_holdout %in% predictant$q$wy_simple) {
    y_test = predictant$y %>% subset_years(wy_holdout)
    q_test = predictant$q %>% subset_years(wy_holdout)
  } else{
    y_test = NULL
    q_test = NULL
  }
  return(list(
    X_test = X_test,
    y_test = y_test,
    q_test = q_test
  ))
}

wy_to_year <- function(wy, wym){
  #Take a wateryear(wy) to gregorian year
  wy <- as.integer(wy)
  wym <- as.integer(wym)
  
  gregorian_year <- if(wym < 11) wy else (wy + 1)
  return(gregorian_year)
}

wy_month_to_month <- function(wy_month) {
  month <- (wy_month + 3) %% 12 + 1
  return(month)
}

wy_month_to_month <- function(wy_month) {
  month = wy_month + 3
  
  month =
    ifelse(month > 12,
           month - 12,
           month)
  return(month)
}

get_extra_info <- function(info_list, forecast_horizon_) {
  
  # useful text for charts
  year_init = wy_to_year(wy = info_list[['wy_holdout']],
                         wym = forecast_horizon_$init_forecast_index)
  
  year_end =  wy_to_year(wy = info_list[['wy_holdout']],
                         wym = forecast_horizon_$end_forecast_index)
  
  year_initialisation = wy_to_year(wy = info_list[['wy_holdout']],
                                   wym = forecast_horizon_$month_initialisation_index + 1)
  
  month_initialisation = wy_month_to_month(wy_month = forecast_horizon_[['month_initialisation_index']])
  
  date_init_wy =   as.Date(glue("{wy_to_year(info_list[['wy_holdout']],1)}/04/01"))
  date_end_wy =   as.Date(glue("{wy_to_year(info_list[['wy_holdout']],12)}/03/01"))
  
  date_init_forecast            =
    as.Date(
      glue(
        "{year_init}/",
        "{wy_month_to_month(forecast_horizon_[['init_forecast_index']])}/",
        "{01}"
      )
    )
  
  date_end_forecast             =
    as.Date(
      glue(
        "{year_end}/",
        "{wy_month_to_month(forecast_horizon_[['end_forecast_index']])}/",
        "{01}"
      )
    )
  
  
  forecast_horizon_months = seq.Date(from = date_init_forecast,
                                     to = date_end_forecast,
                                     by = "1 months")
  
  days_per_month_horizon  = lubridate::days_in_month(forecast_horizon_months)
  
  wy_holdout_months       = seq.Date(from = date_init_wy,
                                     to = date_end_wy,
                                     by = "1 months")
  
  
  date_initialisation     = glue("1 {info_list[['month_initialisation']]} {year_initialisation}")
  datetime_initialisation = as.Date(glue("{year_initialisation}/{month_initialisation}/01"))
  volume_span_text_v2     = glue(
    "{head(forecast_horizon_$months_forecast_period,1)} {year_init} - {tail(forecast_horizon_$months_forecast_period,1)} {year_end}"
  )
  volume_span_text        = glue(
    "[{head(forecast_horizon_$months_forecast_period,1)},{tail(forecast_horizon_$months_forecast_period,1)}]"
  )
  
  predictor_list_join = paste(info_list[['predictor_list']][[1]] , collapse = "_AND_") ### check
  
  
  return(
    list(
      date_initialisation = date_initialisation,
      datetime_initialisation = datetime_initialisation,
      volume_span_text    = volume_span_text,
      volume_span_text_v2 = volume_span_text_v2,
      predictor_list_join = predictor_list_join,
      forecast_horizon_months = forecast_horizon_months,
      days_per_month_horizon = days_per_month_horizon,
      wy_holdout_months = wy_holdout_months
    )
  )
}

preprocess_data <- function(
    catchment_code = '5410002',
    dataset_meteo  = "ens30avg",
    dataset_hydro  = "ERA5Ens_SKGE",
    month_initialisation = "jun",
    horizon_strategy = "dynamic",
    horizon_month_start = "oct",
    horizon_month_end = "mar",
    predictor_list = c("pr_sum_-1months"),
    wy_holdout = 2016,
    remove_wys = NA,
    units_q = "mm/month",
    units_y = "mm",
    test_subset = T
                            ) {
  
  # save function arguments
  info_list = as.list(environment())
  
  # catchment data (raw forcings, flows)
  catchment_data = 
    get_catchment_data(
    catchment_code = catchment_code,
    dataset_meteo = dataset_meteo,
    dataset_hydro = dataset_hydro,
    remove_wys = remove_wys,
    info_list = info_list
  )
  
  # set target period to forecast
  forecast_horizon_ = 
    get_forecast_horizon(
    month_initialisation = month_initialisation,
    horizon_strategy = horizon_strategy,
    horizon_month_start = horizon_month_start,
    horizon_month_end =  horizon_month_end
  )
  # save relevant info
  extra_info = 
    get_extra_info(
      info_list = info_list,
      forecast_horizon_ = forecast_horizon_
    )

  # create the predictors variables
  predictor = 
    predictors_generator(
    predictor_list = predictor_list,
    forecast_horizon_ = forecast_horizon_,
    catchment_data = catchment_data
  )
  
  info_list$predictor_list_new = predictor %>%
    select(-1, -2) %>%
    colnames
  # create the target variable
  predictant = 
    predictant_generator(
    forecast_horizon_ = forecast_horizon_,
    catchment_data = catchment_data,
    info_list = info_list,
    extra_info = extra_info
    )
  
  # separate into training and testing period
  train_data = 
    training_data(
    predictor = predictor,
    predictant = predictant,
    wy_holdout = wy_holdout
    )
  
  if (test_subset) {
    
  test_data  = 
    testing_data(
    predictor = predictor,
    predictant = predictant,
    wy_holdout = wy_holdout
    )
  return(
    c(
      train_data,
      test_data,
      raw_data = list(catchment_data),
      time_horizon = list(forecast_horizon_),
      extra_info = list(extra_info),
      info = list(info_list)
    )
  )
  }else{
    return(
      c(
        train_data,
        raw_data = list(catchment_data),
        time_horizon = list(forecast_horizon_),
        extra_info = list(extra_info),
        info = list(info_list)
      )
    )
  }
  
    
    
  
}

test_preprocess <- function(){
  
  catchment_code = '5410002'
  dataset_meteo  = "ens30avg"
  dataset_hydro  = "ERA5Ens_SKGE"
  month_initialisation = "jun"
  horizon_strategy = "dynamic"
  horizon_month_start = "oct"
  horizon_month_end = "mar"
  predictor_list = c("pr_sum_-1months","tem_mean_1months")
  wy_holdout = 2022
  remove_wys = c(1980,2013)
  units_q = "m^3/s"
  units_y = "GL"
  test_subset = T
  
  
data1 = preprocess_data(month_initialisation = "oct",
                        wy_holdout = 2022)

data1_1 = preprocess_data(month_initialisation = "oct",
                        wy_holdout = 2022,
                        units_q = "m^3/s",
                        units_y = "GL")

data2 = preprocess_data(
  catchment_code = '7321002',
  month_initialisation = "sep",
  dataset_meteo  = "ens30avg",
  horizon_strategy = "dynamic",
  horizon_month_start = "oct",
  horizon_month_end = "mar",
  predictor_list = c("pr_sum_-1months","tem_mean_3months"),
  wy_holdout = 2022,
  remove_wys = NA
)

data3 = 
  preprocess_data(
  catchment_code = '5410002',
  dataset_meteo = "ens30avg",
  dataset_hydro = "ERA5Ens_SKGE",
  month_initialisation = "sep",
  horizon_strategy = "dynamic",
  predictor_list = c("pr_sum_-1months",
                     "tem_mean_-1months",
                     "SP_last_1months",
                     ##GR4J
                     "PROD_last_1months",
                     "ROUT_last_1months",
                     ##TUW
                     "SM_last_1months",
                     "SUZ_last_1months",
                     "SLZ_last_1months",
                     ##SACRAMENTO
                     "UZT_last_1months",
                     "UZF_last_1months",
                     "LZT_last_1months",
                     "LZS_last_1months",
                     "LZP_last_1months"),
                     wy_holdout = 2000,
                     remove_wys = NULL
  )

data4 = preprocess_data(
  catchment_code = catchment_code,
  dataset_meteo  = dataset_meteo,
  dataset_hydro  = dataset_hydro,
  month_initialisation = month_initialisation,
  horizon_strategy = horizon_strategy,
  horizon_month_start = horizon_month_start,
  horizon_month_end = horizon_month_end,
  predictor_list = predictor_list,
  wy_holdout = wy_holdout,
  remove_wys = remove_wys,
  units_q = units_q,
  units_y = units_y,
  test_subset = T
)


return(data4)
}

#x=test_preprocess()
