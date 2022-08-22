rm(list = ls())
#directory = "/Users/fco/CAPTA/Pronostico_estacional/"
#setwd(directory)

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

data_filenames <- function(region = "ChileCentral") {
  
  
  if (region=="Maule") {
  catchments_attributes_filename = "data_input/attributes_catchments_RegionMaule.feather"
  flows_filename                 = "data_input/flows_mm_monthly_catchments_RegionMaule.feather"
  meteo_filename                 = "data_input/meteo_monthly_catchments_RegionMaule.feather"
  hydro_filename                 = "data_input/hydro_variables_monthly_catchments_RegionMaule.feather"
  }
  
  if (region == "ChileCentral") {
    catchments_attributes_filename = "data_input/attributes_catchments_ChileCentral.feather"
    flows_filename                 = "data_input/flows_mm_monthly_catchments_ChileCentral.feather"
    meteo_filename                 = "data_input/meteo_monthly_catchments_ChileCentral.feather"
    hydro_filename                 = "data_input/hydro_variables_monthly_catchments_ChileCentral.feather"
  }
 
   if (region == "ChileCentral_CR2MET") {
    catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
    flows_filename                 = "data_input/flows_mm_monthly_49catchments_ChileCentral.feather"
    meteo_filename                 = "data_input/meteo_monthly_catchments_ChileCentral_era5raw.feather"
    hydro_filename                 = "data_input/hydro_variables_monthly_catchments_ChileCentral.feather"
  }
  
  if (region == "ChileCentral_era5raw") {
    catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
    flows_filename                 = "data_input/flows_mm_monthly_49catchments_ChileCentral.feather"
    meteo_filename                 = "data_input/meteo_monthly_catchments_ChileCentral_era5raw.feather"
    hydro_filename                 = "data_input/hydro_variables_monthly_catchments_ChileCentral.feather"
  }
  
  if (region == "ChileCentral_era5QDM") {
    catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
    flows_filename                 = "data_input/flows_mm_monthly_49catchments_ChileCentral.feather"
    meteo_filename                 = "data_input/meteo_monthly_catchments_ChileCentral_era5QDM.feather"
    hydro_filename                 = "data_input/hydro_variables_monthly_catchments_ChileCentral.feather"
  }
  
  
  if (region == "ChileCentral_ens30avg") {
    catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
    flows_filename                 = "data_input/flows_mm_monthly_49catchments_ChileCentral.feather"
    meteo_filename                 = "data_input/meteo_monthly_catchments_ChileCentral_ens30avg.feather"
    hydro_filename                 = "data_input/hydro_variables_monthly_catchments_ChileCentral.feather"
  }
  
  if (region == "ChileCentral_ens20") {
    catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
    flows_filename                 = "data_input/flows_mm_monthly_49catchments_ChileCentral.feather"
    meteo_filename                 = "data_input/meteo_monthly_catchments_ChileCentral_ens20.feather"
    hydro_filename                 = "data_input/hydro_variables_monthly_catchments_ChileCentral.feather"
  }
  
  if (region == "ChileCentral_ens30") {
    catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
    flows_filename                 = "data_input/flows_mm_monthly_49catchments_ChileCentral.feather"
    meteo_filename                 = "data_input/meteo_monthly_catchments_ChileCentral_ens30.feather"
    hydro_filename                 = "data_input/hydro_variables_monthly_catchments_ChileCentral.feather"
  }
  
  if (region == "ChileCentral_ens50") {
    catchments_attributes_filename = "data_input/attributes_49catchments_ChileCentral.feather"
    flows_filename                 = "data_input/flows_mm_monthly_49catchments_ChileCentral.feather"
    meteo_filename                 = "data_input/meteo_monthly_catchments_ChileCentral_ens50.feather"
    hydro_filename                 = "data_input/hydro_variables_monthly_catchments_ChileCentral.feather"
  }

    return(
      list(
        catchments_attributes_filename = catchments_attributes_filename,
        flows_filename = flows_filename,
        meteo_filename = meteo_filename,
        hydro_filename = hydro_filename
      )
    )

  

  
}

read_and_subset_variable <- function(filename,catchment_code) {
  
  df_variable = data.table(
    cod_cuenca=factor(),
    wy_simple=numeric(),
    wym=numeric(),
    wym_str=character())
  
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
                           region,
                           remove_wys
                           ) {
  
  data_filenames_ = data_filenames(region=region)
  
  attributes_catchment = data_filenames_$catchments_attributes_filename %>% 
    feather::read_feather() %>%
    subset(cod_cuenca == catchment_code)
  
  monthly_flows        = read_and_subset_variable(
    filename = data_filenames_$flows_filename,
    catchment_code = catchment_code
    ) %>%
    remove_years(remove_wys)
  
  monthly_meteo       <- read_and_subset_variable(
    filename = data_filenames_$meteo_filename,
    catchment_code = catchment_code
  ) %>% 
    remove_years(remove_wys)
  
  monthly_hydro       <- read_and_subset_variable(
    filename = data_filenames_$hydro_filename,
    catchment_code = catchment_code
  ) %>%
  remove_years(remove_wys)
  
  ################# modify this matrix accordingly
  raw_data_df = merge(monthly_meteo,
                      monthly_hydro,
                      all.x=TRUE,
                      by= c("cod_cuenca","wy_simple","wym","wym_str")) 
  
  return(
    list(
      monthly_flows = monthly_flows,
      monthly_meteo = monthly_meteo,
      raw_data_df = raw_data_df,
      attributes_catchment = attributes_catchment
    )
  )
}

### FORECASTING MONTHS HORIZON
forecast_horizon <- function(
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
  months_before_initialisation = months_wy[1:month_initialisation_index-1]
  
  # FINAL PERIOD OF FORECAST
  months_forecast_period = months_wy[init_forecast_index:end_forecast_index]
  
  
  return(
    list(
      init_forecast_index = init_forecast_index,
      end_forecast_index = end_forecast_index,
      months_forecast_period = months_forecast_period,
      months_before_initialisation =months_before_initialisation,
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
    if (period_before < 0) {
      period_before = forecast_horizon_$month_initialisation_index-1
    }
    
    var_name = glue::glue('{variable}_{operator}_{period_before}months')
    

    input_data = catchment_data$raw_data_df
    num_ensembles = unique(input_data$ens)
    
    if (is.null(num_ensembles)) {
      input_data$ens = 1
    }
    
    var = input_data %>%
      subset(wym < forecast_horizon_$month_initialisation_index) %>%
      subset(wym > forecast_horizon_$month_initialisation_index - period_before-1) %>% 
      select(c("wy_simple", all_of(variable), ens)) %>% 
      aggregate(
        FUN = operator,
        by = list(wy_simple = .$"wy_simple",ens = .$"ens"),
        drop = T
      ) %>%
      select(c("wy_simple","ens", all_of(variable))) %>%
      drop_na()
    
    names(var)[names(var) == variable] <- var_name
    
    
    return(var)
  }
## predictor dataframe
predictors_generator <- function(predictor_list,
                                 forecast_horizon_,
                                 catchment_data
                                 ) {
  
  predictors = lapply(predictor_list,
                      function(var_name)
                        
                      
                        var = predictor_generator(
                          var_name = var_name,
                          forecast_horizon_ = forecast_horizon_,
                          catchment_data = catchment_data
                        ))
  
  predictors = data.table(Reduce(merge, predictors),key="wy_simple,ens")
  
  return(predictors)
}

# target variable : volume and streamflow
predictant_generator <- function(forecast_horizon_,catchment_data) {
  

  
  cond1 = catchment_data$monthly_flows$wym >= forecast_horizon_$init_forecast_index
  cond2 = catchment_data$monthly_flows$wym <= forecast_horizon_$end_forecast_index
  
  q_period_wy = subset(catchment_data$monthly_flows,cond1 & cond2) %>% 
    select(c("wy_simple","Q_mm","wym",'wym_str')) %>% 
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
  
  return(list(y = y,q = q))
}

remove_years <- function(variable,wys) {
  variable_train = variable[!(variable$wy_simple %in% wys),]
  return(variable_train)
}

subset_years <- function(variable,wy_train) {
  variable_train = variable[variable$wy_simple %in% wy_train,] %>%
    tibble::as_tibble() %>% 
    tibble::column_to_rownames(var="wy_simple")
  
  return(variable_train)
}

dataframe_to_list <- function(df) {
  
ensemble_names = unique(df$ens)
df_list = lapply(ensemble_names, function(ens_i) subset(df,ens == ens_i,select = - ens))
names(df_list) = ensemble_names
return(df_list)
}



training_data <- function(predictor, predictant,wy_holdout) {
  wy_train = intersect(predictor$wy_simple,predictant$q$wy_simple)
  wy_train = wy_train[wy_train != wy_holdout]
  
  ### training period
  X_train = predictor %>%
    dataframe_to_list %>%
    lapply(function(x) subset_years(x,wy_train))
  
  #X_train = predictor %>% subset_years(wy_train)
  y_train = predictant$y %>% subset_years(wy_train)
  q_train = predictant$q %>% subset_years(wy_train)
  return(list(
    X_train = X_train,
    y_train = y_train,
    q_train = q_train,
    wy_train = wy_train,
    wy_holdout = wy_holdout
    ))
}

testing_data <- function(predictor, predictant,wy_holdout) {
  
  ### testing period
  if (wy_holdout %in% predictor$wy_simple) {
    X_test = predictor %>%
      dataframe_to_list %>%
      lapply(function(x) subset_years(x,wy_holdout))
    
  }else{
    X_test = NULL
    stop("You need predictors for the testing period (see wy_holdout).")
  }
  
  if (wy_holdout %in% predictant$q$wy_simple) {
    y_test = predictant$y %>% subset_years(wy_holdout)
    q_test = predictant$q %>% subset_years(wy_holdout)
  }else{
    y_test = NULL
    q_test = NULL
  }
  return(list(
    X_test = X_test,
    y_test = y_test,
    q_test = q_test
    ))
}

wy_to_year <- function(wy,wym) {
  #Take a wateryear(wy) to gregorian year
  wy = as.integer(wy)
  wym = as.integer(wym)
  
  gregorian_year = as.integer(ifelse(wym<10,wy,wy+1))
  return(gregorian_year)
}

wy_month_to_month <- function(wy_month) {
  month = wy_month+3
  
  month =
    ifelse(month>12,
           month-12,
           month) %>% 
    sprintf("%02d", .)
return(month)
}

set_label_text <- function(info_list,forecast_horizon_) {
  
  # useful text for charts
  year_init = wy_to_year(info_list[['wy_holdout']],forecast_horizon_$init_forecast_index)
  year_end =  wy_to_year(info_list[['wy_holdout']],forecast_horizon_$end_forecast_index)
  year_initialisation = wy_to_year(info_list[['wy_holdout']], forecast_horizon_$month_initialisation_index+1)
  
  month_initialisation = wy_month_to_month(forecast_horizon_[['month_initialisation_index']])
  
  date_init_wy =   as.Date(glue("{wy_to_year(info_list[['wy_holdout']],1)}/04/01"))
  date_end_wy =   as.Date(glue("{wy_to_year(info_list[['wy_holdout']],12)}/03/01"))
  
  date_init_forecast            = 
    as.Date(
    glue(
      "{year_init}/",
      "{wy_month_to_month(forecast_horizon_[['init_forecast_index']])}/",
      "{01}"
    ))
  
  date_end_forecast             = 
    as.Date(
    glue(
      "{year_end}/",
      "{wy_month_to_month(forecast_horizon_[['end_forecast_index']])}/",
      "{01}"
    ))
  
  
  forecast_horizon_months = seq.Date(from = date_init_forecast,to=date_end_forecast, by = "1 months")
  wy_holdout_months       = seq.Date(from = date_init_wy,to = date_end_wy,  by = "1 months")
    
  
  
  
  date_initialisation     = glue("1 {info_list[['month_initialisation']]} {year_initialisation}")
  datetime_initialisation = as.Date(glue("{year_initialisation}/{month_initialisation}/01"))
  volume_span_text_v2     = glue("{head(forecast_horizon_$months_forecast_period,1)} {year_init} - {tail(forecast_horizon_$months_forecast_period,1)} {year_end}")
  volume_span_text        = glue("[{head(forecast_horizon_$months_forecast_period,1)},{tail(forecast_horizon_$months_forecast_period,1)}]")
  
  
  predictor_list_join = paste( info_list[['predictor_list']][[1]] ,collapse = "_AND_") ### check
  
  
  
  return(list(
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



preprocess_data <- function(catchment_code = '5410002',
                            region = "ChileCentral_ens30avg",
                            month_initialisation = "may",
                            horizon_strategy = "dynamic",
                            horizon_month_start = "oct",
                            horizon_month_end = "mar",
                            predictor_list = c("pr_sum_-1months"),
                            wy_holdout = 2016,
                            remove_wys = c(2020,2021)
                            ) {

  # catchment data (raw forcings, flows)
  catchment_data = catchment_data(
    catchment_code = catchment_code,
    region = region,
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
    forecast_horizon_=forecast_horizon_,
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
    wy_holdout = wy_holdout)
  
  
  info_list = data.table(
    catchment_code = catchment_code,
    region = region,
    month_initialisation = month_initialisation,
    horizon_strategy = horizon_strategy,
    predictor_list = list(predictor_list),
    wy_holdout = wy_holdout,
    remove_wys = list(remove_wys)
  )
  
  
  plot_text = 
    set_label_text(
    info_list = info_list,
    forecast_horizon_ = forecast_horizon_)
  
  return(c(train_data,
           test_data,
           raw_data = list(catchment_data),
           time_horizon = list(forecast_horizon_),
           #args = args,
           plot_text=list(plot_text),
           info = list(info_list)
         )
  )
}

##################################
############## MAIN ##############
##################################

# data = preprocess_data(
# catchment_code = '5410002',
# region = "ChileCentral_ens30avg",
# month_initialisation = "ene",
# horizon_strategy = "dynamic",
# predictor_list = c("pr_sum_-1months",
#                    "tem_mean_-1months"),
# wy_holdout = 2016
# )

# # print(data$info)
# data2 = preprocess_data(
#   catchment_code = "Longavi",
#   region = "Maule",
#   month_initialisation = "sep",
#   horizon_strategy = "dynamic",
#   predictor_list = c("pr_sum_-1months",
#                      "tem_mean_2months",
#                      "SP_last_1months",
#                      "PROD_last_1months",
#                      "ROUT_last_1months",
#                      "EXP_last_1months"),
#   wy_holdout = 2021,
#   remove_wys = seq(1979,1981)
# )
# 
# data3 = preprocess_data(
#   catchment_code = '5410002',
#   region = "ChileCentral_ens",
#   month_initialisation = "nov",
#   horizon_strategy = "dynamic",
#   predictor_list = c("pr_sum_-1months",
#                      "tem_mean_-1months"),
#   wy_holdout = 2016
# )