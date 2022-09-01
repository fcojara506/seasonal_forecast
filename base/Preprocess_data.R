#rm(list = ls())

library(data.table)
library(dplyr)
library(lubridate)
library(glue)
library(tidyr)
library(tibble)

# MONTHLY DATA FOR A SELECTED CATCHMENT
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

dataset_filenames <-
  function(dataset_region,
           dataset_meteo,
           dataset_hydro) {
    catchments_attributes_filename = "base/data_input/attributes/attributes_49catchments_ChileCentral.feather"
    flows_filename                 = "base/data_input/flows/flows_mm_monthly_49catchments_ChileCentral.feather"
    hydro_filename                 = glue::glue(
      "base/data_input/storage_variables/hydro_variables_monthly_catchments_ChileCentral_{dataset_hydro}.feather"
    )
    meteo_filename                 = glue::glue(
      "base/data_input/meteo_variables/meteo_monthly_catchments_ChileCentral_{dataset_meteo}_1979_2022.feather"
    )
    
    return(
      list(
        catchments_attributes_filename = catchments_attributes_filename,
        flows_filename = flows_filename,
        meteo_filename = meteo_filename,
        hydro_filename = hydro_filename
      )
    )
    
  }

read_and_subset_variable <- function(filename, catchment_code) {
  
  df_variable = data.table(
    cod_cuenca = factor(),
    wy_simple = numeric(),
    wym = numeric(),
    wym_str = character()
  )
  
  if (file.exists(filename)) {
    df_variable = feather::read_feather(filename) %>%
      mutate(wym = as.numeric(wym)) %>%
      mutate(wym_str = months_wy[wym]) %>%
      data.table(key = c("wy_simple", "wym")) %>%
      subset(cod_cuenca == catchment_code)
  }
  
  return(df_variable)
}

catchment_data <- function(catchment_code,
                           dataset_region,
                           dataset_meteo,
                           dataset_hydro,
                           remove_wys) {
  data_filenames_ = dataset_filenames(dataset_region,
                                      dataset_meteo,
                                      dataset_hydro)
  
  attributes_catchment = data_filenames_$catchments_attributes_filename %>%
    feather::read_feather() %>%
    subset(cod_cuenca == catchment_code)
  
  monthly_flows        = read_and_subset_variable(filename = data_filenames_$flows_filename,
                                                  catchment_code = catchment_code) %>%
    remove_years(remove_wys)
  
  monthly_meteo       <- read_and_subset_variable(filename = data_filenames_$meteo_filename,
                                                  catchment_code = catchment_code) %>%
    remove_years(remove_wys)
  
  monthly_hydro       <- read_and_subset_variable(filename = data_filenames_$hydro_filename,
                                                  catchment_code = catchment_code) %>%
    remove_years(remove_wys)
  
  ################# modify this matrix accordingly
  raw_data_df = merge(
    monthly_meteo,
    monthly_hydro,
    all = TRUE,
    by = c("cod_cuenca", "wy_simple", "wym", "wym_str", "ens")
  )
  
  #unite("ens",c(ens.x,ens.y),remove=T,sep=".")
  #mutate(ens = as.numeric(ens))
  
  return(
    list(
      monthly_flows = monthly_flows,
      monthly_meteo = monthly_meteo,
      monthly_hydro = monthly_hydro,
      raw_data_df = raw_data_df,
      attributes_catchment = attributes_catchment
    )
  )
}

### FORECASTING MONTHS HORIZON
forecast_horizon <- function(month_initialisation,
                             horizon_strategy,
                             horizon_month_start = "oct",
                             horizon_month_end =  "mar") {
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
  } else if (horizon_strategy == 'fixed') {
    init_forecast_index = init_target_index
  }
  
  
  
  # end month
  if (horizon_strategy == "dynamic") {
    end_forecast_index = end_target_index
  } else if (horizon_strategy == 'fixed') {
    end_forecast_index = end_target_index
  }
  
  # PREVIOUS MONTHS OF FORECAST
  months_before_initialisation = months_wy[1:month_initialisation_index -
                                             1]
  
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
# one predictor column generator

predictor_generator <-
  function(var_name,
           forecast_horizon_,
           catchment_data) {
    word = strsplit(var_name, "_")[[1]]
    
    variable = word[1]
    operator = word[2]
    period_before = as.numeric(strsplit(word[3], "months")[[1]])
    
    #condition to consider the whole period of the current wy
    if (period_before < 0) {
      period_before = forecast_horizon_$month_initialisation_index - 1
    }
    # new name
    var_name = glue::glue('{variable}_{operator}_{period_before}months')
    
    input_data = catchment_data$raw_data_df
    num_ensembles = unique(input_data$ens)
    
    if (is.null(num_ensembles)) {input_data$ens = 1}
    
    tryCatch({
      
      var = input_data %>%
        subset(wym < forecast_horizon_$month_initialisation_index) %>%
        subset(wym > forecast_horizon_$month_initialisation_index - period_before -1) %>%
        select(all_of(c("wy_simple", all_of(variable), "ens")))
      
      num_records_per_wy = var %>%
        group_by(wy_simple) %>%
        summarise(num = n()) %>%
        data.frame()
      
      kickout_wy = num_records_per_wy[num_records_per_wy$num != period_before, "wy_simple"]
      # message(
      #   glue::glue(
      #     "REMOVED PREDICTOR: Incomplete {variable} records for wateryear(s): {kickout_wy}",
      #     )
      # )
      
      var = var %>%
        subset(!(wy_simple %in% kickout_wy)) %>%
        aggregate(
          FUN = operator,
          by = list(wy_simple = .$"wy_simple", ens = .$"ens"),
          drop = T
        ) %>%
        select(c("wy_simple", "ens", all_of(variable)))
      
      names(var)[names(var) == variable] <- var_name
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
                      function(var_name)
                        
                        predictor_generator(
                          var_name = var_name,
                          forecast_horizon_ = forecast_horizon_,
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
predictant_generator <- function(forecast_horizon_, catchment_data) {
  cond1 = catchment_data$monthly_flows$wym >= forecast_horizon_$init_forecast_index
  cond2 = catchment_data$monthly_flows$wym <= forecast_horizon_$end_forecast_index
  
  q_period_wy = subset(catchment_data$monthly_flows, cond1 &
                         cond2) %>%
    select(c("wy_simple", "Q_mm", "wym", 'wym_str')) %>%
    mutate(wy_simple = as.integer(wy_simple))
  
  
  #volume in mm
  y = q_period_wy %>%
    select(c("wy_simple", "Q_mm")) %>%
    aggregate(
      FUN = 'sum',
      by = list(wy_simple = .$"wy_simple"),
      drop = T
    ) %>%
    select(c("wy_simple", "Q_mm"))
  
  names(y)[names(y) == "Q_mm"] <- 'volume_mm'
  
  # streamflow in mm
  q = q_period_wy %>%
    dcast(wy_simple ~ wym ,
          value.var = "Q_mm")
  names(q)[names(q) != "wy_simple"] = forecast_horizon_$months_forecast_period
  
  return(list(y = y, q = q))
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
    stop("YOU NEED VALID PREDICTORS FOR THE TARGET PERIOD (SEE DATA FOR WY_HOLDOUT)")
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

wy_to_year <- function(wy, wym) {
  #Take a wateryear(wy) to gregorian year
  wy = as.integer(wy)
  wym = as.integer(wym)
  
  gregorian_year = as.integer(ifelse(wym < 10, wy, wy + 1))
  return(gregorian_year)
}

wy_month_to_month <- function(wy_month) {
  month = wy_month + 3
  
  month =
    ifelse(month > 12,
           month - 12,
           month) %>%
    sprintf("%02d", .)
  return(month)
}

set_label_text <- function(info_list, forecast_horizon_) {
  # useful text for charts
  year_init = wy_to_year(info_list[['wy_holdout']], forecast_horizon_$init_forecast_index)
  year_end =  wy_to_year(info_list[['wy_holdout']], forecast_horizon_$end_forecast_index)
  year_initialisation = wy_to_year(info_list[['wy_holdout']],
                                   forecast_horizon_$month_initialisation_index + 1)
  
  month_initialisation = wy_month_to_month(forecast_horizon_[['month_initialisation_index']])
  
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
  
  
  forecast_horizon_months = seq.Date(from = date_init_forecast, to = date_end_forecast, by = "1 months")
  wy_holdout_months       = seq.Date(from = date_init_wy, to = date_end_wy,  by = "1 months")
  
  
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
      wy_holdout_months = wy_holdout_months
    )
  )
}

preprocess_data <- function(catchment_code = '5410002',#  
                            dataset_region = "ChileCentral",
                            dataset_meteo  = "ens30avg",
                            dataset_hydro  = "GR4J_EVDSep",
                            month_initialisation = "jun",
                            horizon_strategy = "dynamic",
                            horizon_month_start = "oct",
                            horizon_month_end = "mar",
                            predictor_list = c("pr_sum_-1months"),
                            wy_holdout = 2016,
                            remove_wys = NA) {
  
  # catchment data (raw forcings, flows)
  catchment_data = catchment_data(
    catchment_code = catchment_code,
    dataset_region = dataset_region,
    dataset_meteo = dataset_meteo,
    dataset_hydro = dataset_hydro,
    remove_wys = remove_wys
  )
  
  # set target period to forecast
  forecast_horizon_ = forecast_horizon(
    month_initialisation = month_initialisation,
    horizon_strategy = horizon_strategy,
    horizon_month_start = horizon_month_start,
    horizon_month_end =  horizon_month_end
  )
  
  # create the predictors variables
  predictor = predictors_generator(
    predictor_list = predictor_list,
    forecast_horizon_ = forecast_horizon_,
    catchment_data = catchment_data
  )
  
  # create the target variable
  predictant = predictant_generator(
    forecast_horizon_ = forecast_horizon_,
    catchment_data = catchment_data
    )
  
  # separate into training and testing period
  train_data = training_data(
    predictor = predictor,
    predictant = predictant,
    wy_holdout = wy_holdout
    )
  
  test_data  = testing_data(
    predictor = predictor,
    predictant = predictant,
    wy_holdout = wy_holdout
    )
  
  
  info_list = data.table(
    catchment_code = catchment_code,
    dataset_region = dataset_region,
    dataset_meteo = dataset_meteo,
    dataset_hydro = dataset_hydro,
    month_initialisation = month_initialisation,
    horizon_strategy = horizon_strategy,
    predictor_list = list(predictor_list),
    wy_holdout = wy_holdout,
    remove_wys = list(remove_wys)
  )
  
  plot_text = set_label_text(
    info_list = info_list,
    forecast_horizon_ = forecast_horizon_
    )
  
  return(
    c(
      train_data,
      test_data,
      raw_data = list(catchment_data),
      time_horizon = list(forecast_horizon_),
      plot_text = list(plot_text),
      info = list(info_list)
    )
  )
}

# x = preprocess_data(
#   catchment_code = '5410002',
#   month_initialisation = "sep",
#   dataset_region = "ChileCentral",
#   dataset_meteo  = "ens30avg",
#   horizon_strategy = "dynamic",
#   horizon_month_start = "oct",
#   horizon_month_end = "mar",
#   predictor_list = c("pr_sum_-1months"),
#   wy_holdout = 2022,
#   remove_wys = NA
# )

# data3 = preprocess_data(
#   catchment_code = '5410002',
#   dataset_region = "ChileCentral",
#   dataset_meteo = "ens30avg",
#   dataset_hydro = "GR4J_KGE",
#   month_initialisation = "sep",
#   horizon_strategy = "dynamic",
#   predictor_list = c("pr_sum_-1months",
#                      "tem_mean_-1months",
#                      "SP_last_1months",
#                      ##GR4J
#                      "PROD_last_1months",
#                      "ROUT_last_1months",
#                      ##TUW
#                      "SM_last_1months",
#                      "SUZ_last_1months",
#                      "SLZ_last_1months",
#                      ##SACRAMENTO
#                      "UZT_last_1months",
#                      "UZF_last_1months",
#                      "LZT_last_1months",
#                      "LZS_last_1months",
#                      "LZP_last_1months"),
#                      wy_holdout = 2000,
#                      remove_wys = NULL
#   )
