get_extra_info <- function(wy_holdout, forecast_horizon_) {
  
  year_init = wy_to_year(
    wy = wy_holdout,
    wym = forecast_horizon_$init_forecast_index)
  
  year_end =  wy_to_year(
    wy = wy_holdout,
    wym = forecast_horizon_$end_forecast_index)
  
  year_initialisation = wy_to_year(
    wy = wy_holdout,
    wym = forecast_horizon_$month_initialisation_index + 1)
  
  month_initialisation = wy_month_to_month(
    wy_month = forecast_horizon_$month_initialisation_index)
  
  date_init_wy =   as.Date(glue("{wy_to_year(wy_holdout,1)}/04/01"))
  date_end_wy =   as.Date(glue("{wy_to_year(wy_holdout,12)}/03/01"))
  
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


get_extra_info <- function(water_year_holdout, forecast_horizon) {
  
  year_init = water_year_to_year(water_year = water_year_holdout,
                                 month_index = forecast_horizon$init_forecast_index)
  
  year_end =  water_year_to_year(water_year = water_year_holdout,
                                 month_index = forecast_horizon$end_forecast_index)
  
  year_initialisation = water_year_to_year(water_year = water_year_holdout,
                                           month_index = forecast_horizon$month_initialisation_index + 1)
  
  month_initialisation = water_year_month_to_month(month_index = forecast_horizon$month_initialisation_index)
  
  date_init_wy = as.Date(strftime(as.Date("2000/01/01"), "%Y/04/01", tz = "UTC"))
  date_end_wy = as.Date(strftime(as.Date("2000/12/31"), "%Y/03/01", tz = "UTC"))
  
  date_init_forecast = as.Date(strftime(as.Date(paste0(year_init,"/01/01")), format = "%Y/%m/%d", tz = "UTC"))
  date_end_forecast = as.Date(strftime(as.Date(paste0(year_end,"/01/01")), format = "%Y/%m/%d", tz = "UTC"))
  
  forecast_horizon_months = seq.Date(from = date_init_forecast,
                                     to = date_end_forecast,
                                     by = "1 months")
  
  days_per_month_horizon  = lubridate::days_in_month(forecast_horizon_months)
  
  water_year_holdout_months = seq.Date(from = date_init_wy,
                                       to = date_end_wy,
                                       by = "1 months")
  
  date_initialisation = strftime(as.Date(paste0(year_initialisation,"/01/01")), format = "%d %B %Y", tz = "UTC")
  datetime_initialisation = as.Date(strftime(as.Date(paste0(year_initialisation,"/01/01")), format = "%Y/%m/%d", tz = "UTC"))
  
  volume_span_text_v2     = paste0(month.name[head(forecast_horizon$months_forecast_period,1)], " ", year_init, " - ", month.name[tail(forecast_horizon$months_forecast_period,1)], " ", year_end)
  volume_span_text        = paste0("[]" ,month.name[head(forecast_horizon$months_forecast_period,1)], ", ", month.name[tail(forecast_horizon$months_forecast_period,1)], "]")
  
  predictor_list_join = paste(info_list[['predictor_list']][[1]] , collapse = "AND")
  
  return(
    list(
      date_initialisation = date_initialisation,
      datetime_initialisation = datetime_initialisation,
      volume_span_text = volume_span_text,
      volume_span_text_v2 = volume_span_text_v2,
      predictor_list_join = predictor_list_join,
      forecast_horizon_months = forecast_horizon_months,
      days_per_month_horizon = days_per_month_horizon,
      water_year_holdout_months = water_year_holdout_months
    )
  )
}
}


source("base/Preprocess_data.R")
data4 = test_preprocess()
info_list = data4$info
forecast_horizon_ = data4$time_horizon
