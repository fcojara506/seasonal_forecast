library(data.table)
library(dplyr)
library(lubridate)
library(glue)
library(tibble)

# import local libraries
source("base/Convert_units.R")
source("base/DatesWaterYear.R")
source("base/CheckNormality.R")
source("base/Pexc.R")


get_default_datasets_path <- function(meteo=NULL, hydro=NULL) {
  
  # catchment attributes
  catchments_attributes_filename <-"data_input/attributes/attributes_45catchments_ChileCentral.csv"
  # flow in average monthly mm
  flows_filename <- "data_input/flows/flows_mm_monthly_49catchments_ChileCentral.csv"
  # variable from the hydrology model
  hydro_filename <- NULL
  hydro_filename <- if(!is.null(hydro)) glue::glue("data_input/storage_variables/","hydro_variables_monthly_catchments_ChileCentral_{hydro}.csv")
  # meteorological data from meteo
  meteo_filename <- NULL
  meteo_filename <- if(!is.null(meteo)) glue::glue("data_input/meteo_variables/","meteo_monthly_catchments_ChileCentral_{meteo}_1979_present.csv")
  # monthly standard climate indices
  climateindex_filename <- "data_input/climate_index_variables/indices_mensuales_1979_present.csv"
  
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
  # read files from the catchment data and 
  # subset given the catchment code
  cod_cuenca <- factor()
  wym <- numeric()
  df_variable <- NULL
  
  if (!is.null(filename) && file.exists(filename)) {
    # read file and subset by catchment code
    df_variable <- fread(filename) %>%
      mutate(wym = as.numeric(wym)) %>%
      data.table(key = c("wy_simple", "wym")) %>%
      subset(cod_cuenca == catchment_code, select = -cod_cuenca)
  }
  return(df_variable)
}

read_catchment_data <- function(catchment_code,
                                remove_wys,
                                water_units,
                                data_location_paths
) {
  #' @title Read and Process Catchment Data
  #' @description This script reads and processes data for a specific catchment. 
  #' for the catchment attributes, flows, meteorological data, and hydrological 
  #' variables.
  #' @param catchment_code The code for the specific catchment
  #' @param remove_wys
  #' @param water_units
  library(dplyr)
  #initialise NULL attributes
  attributes_catchment <- 
    monthly_flows <- 
    monthly_meteo <-
    monthly_hydro <- 
    monthly_climateindex <- NULL
  
  # read all the catchments attributes from the study area
  attributes_catchments <- fread(data_location_paths$catchments_attributes_filename)
  #check if the catchment code exists
  if(!(catchment_code %in% attributes_catchments$cod_cuenca)) {
    stop(paste0(catchment_code, " not found in catchments"))
  }
  
  # subset the attributes for the specific catchment
  attributes_catchment <- attributes_catchments %>% 
    subset(cod_cuenca == catchment_code)
  
  # read and subset the flows, meteorological data, and hydrological data
  monthly_flows        <- read_and_subset_variable(
    filename = data_location_paths$flows_filename,
    catchment_code = catchment_code) 
  
  # convert the flow data to the desired units
  monthly_flows <- monthly_flows %>%
    data.table() %>%
    mutate(date = lubridate::dmy(paste0("01",
                                        sprintf("%02d", wateryearmonth2month(wy_month = wym)),
                                        wateryear2year(wy = wy_simple, wym = wym)
    ))) %>% 
    mutate(days_months = lubridate::days_in_month(date)) %>% 
    select(-date) %>% 
    mutate(Q_converted =
             hydromad::convertFlow(
               Q_mm,
               from = "mm/month",
               to = water_units$q,
               area.km2 = attributes_catchment$area_km2)) %>% 
    select(-days_months) %>% 
    remove_years(remove_wys)
  # meteorological data
  monthly_meteo       <- read_and_subset_variable(
    filename = data_location_paths$meteo_filename,
    catchment_code = catchment_code) 
  # data from the hydrological model
  monthly_hydro       <- read_and_subset_variable(
    filename = data_location_paths$hydro_filename,
    catchment_code = catchment_code) %>% 
    select("wy_simple", "wym", "STORAGE")
  # read and process the climate index data
  monthly_climateindex <- data_location_paths$climateindex_filename %>% 
    fread() %>%
    mutate(wym = as.numeric(wym))
  
  # merge the meteorological, hydrological, and climate index data into a single dataframe
  raw_data_df <-  Reduce(
    f = function(x, y) {
      merge(x,
            y,
            all = TRUE,
            by = c("wy_simple", "wym"),
      )
    },
    x = purrr::compact(list(
      monthly_meteo, monthly_hydro, monthly_climateindex
    ))
  )
  
  #return a list of the processed data and the catchment attributes
  return(
    list(
      monthly_flows = monthly_flows,
      monthly_meteo = monthly_meteo,
      monthly_hydro = monthly_hydro,
      monthly_climateindex = monthly_climateindex,
      raw_data_df = raw_data_df,
      attributes_catchment = attributes_catchment
    )
  )
}

### FORECASTING MONTHS HORIZON
get_forecast_horizon <- function(date_init,horizon) {
  
  months_year <- c("ene", "feb", "mar","abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic")
  months_wy <- c("abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic", "ene", "feb", "mar")
  
  
  
  # indices within months_wy vector
  month_initialisation_index <- date_init$init_water_year_month
  init_target_index <-  wateryearmonth(horizon$month_start)
  end_target_index <- wateryearmonth(horizon$month_end)
  
  # check horizon$window_method
  horizon$window_method <- match.arg(horizon$window_method,c("dynamic","static"))
  # start month
  if (horizon$window_method == "dynamic") {
    if (month_initialisation_index < init_target_index) {
      init_forecast_index <- init_target_index
    } else {
      init_forecast_index <- month_initialisation_index
    }
  } else if (horizon$window_method == "static") {
    init_forecast_index <- init_target_index
  }
  
  # end month
  if (horizon$window_method == "dynamic") {
    end_forecast_index <- end_target_index
  } else if (horizon$window_method == "static") {
    end_forecast_index <- end_target_index
  }
  
  ############################################################################
  ######## DERIVED DATA ###############
  # PREVIOUS MONTHS OF FORECAST
  months_before_initialisation <- months_wy[1:month_initialisation_index - 1]
  
  # FINAL PERIOD OF FORECAST
  months_forecast_period <- months_wy[init_forecast_index:end_forecast_index]
  library(glue)
  ### initialisation dates
  
  #format date: %d %b %Y
  date_initialisation <- format(date_init$ymd_datetime,'%d %m %Y')
  #format date: %Y-%m-%d
  datetime_initialisation <- date_init$datetime_initialisation
  ### forecast horizon dates
  ###
  # (1) forecast horizon start date FHSD
  
  #year of FHSD
  year_init <- wateryear2year(wy = date_init$init_water_year, wym = init_forecast_index)
  #month of FHSD
  month_init <- wateryearmonth2month(init_forecast_index)
  #date of FHSD %Y-%m-%d
  date_init_forecast <- make_date(year_init,month_init,1)
  # (2) forecast horizon end date FHED
  
  
  
  #year of FHED
  year_end <-  wateryear2year(wy = date_init$init_water_year, wym = end_forecast_index)
  #month of FHED
  month_end <- wateryearmonth2month(end_forecast_index)
  #date of FHED %Y-%m-%d
  date_end_forecast <- make_date(year_end,month_end,1)
  
  
  # months in the forecast horizon
  forecast_horizon_months <- seq.Date(from = date_init_forecast,
                                      to = date_end_forecast, by = "1 months")
  
  
  
  # start and end of forecast horizon (%b %Y and %b)
  # change date format to Spanish (for convenience)
  system_locale <- Sys.getlocale(category = "LC_TIME")
  spanish_locale <- Sys.setlocale("LC_TIME", "es_ES.UTF-8")
  
  # number of day in each month of the forecast horizon
  days_per_month_horizon  <- lubridate::days_in_month(forecast_horizon_months)
  
  volume_span_text_v2     <-
    format(forecast_horizon_months,"%b %Y") %>%
    .[c(1, length(.))] %>%
    paste(collapse = " - ")
  
  volume_span_text        <-
    format(forecast_horizon_months,"%b") %>%
    .[c(1, length(.))] %>%
    paste(collapse = "-")
    #paste0("[", ., "]")
  
  #return to system locale
  system_locale <- Sys.setlocale(category = "LC_TIME", locale = system_locale)
  
  ### target water year
  # start date
  date_init_wy <- make_date(wateryear2year(date_init$init_water_year,1),4,1)
  # end date
  date_end_wy <-  make_date(wateryear2year(date_init$init_water_year,12),3,31)
  # months in target water year
  wy_holdout_months   <- seq.Date(from = date_init_wy,
                                  to = date_end_wy,
                                  by = "1 months")
  #results
  return(
    list(
      months_wy = months_wy,
      months_forecast_period = months_forecast_period,
      dates_forecast_period = forecast_horizon_months,
      days_per_month_horizon = days_per_month_horizon,
      months_before_initialisation = months_before_initialisation,
      init_forecast_index = init_forecast_index,
      end_forecast_index = end_forecast_index,
      month_initialisation_index = month_initialisation_index,
      date_initialisation = date_initialisation,
      datetime_initialisation = datetime_initialisation,
      volume_span_text    = volume_span_text,
      volume_span_text_v2 = volume_span_text_v2,
      months_target_water_year = wy_holdout_months,
      wy_holdout = date_init$init_water_year
    )
  )
}

split_predictor_name <- function(predictor_name, month_initialisation_index) {
  # var_name, consists of 3 attributes:
  # variable, aggregation function, aggregation period
  predictor_name_list <- strsplit(predictor_name, "_")[[1]]
  # check if predictor's name is correct
  stopifnot(length(predictor_name_list)==3)
  # separate into variable function and horizon
  predictor_variable <- predictor_name_list[1]
  predictor_function <- predictor_name_list[2]
  predictor_horizon <- as.numeric(strsplit(predictor_name_list[3], "months")[[1]])
  #condition to consider the whole period of the current wy
  if (predictor_horizon < 0) {
    predictor_horizon <- month_initialisation_index - 1
  }
  # check if values of predictor_horizon are positive
  if (predictor_horizon==0) {
    stop(paste0("ERROR: predictor horizon is zero for your variable ", predictor_variable))
  }
  # subset data in forecast horizon interval
  diff_wym_a = (month_initialisation_index - predictor_horizon)%% 12 
  diff_wym_b = (month_initialisation_index - 1)%% 12
  wym_a = diff_wym_a + ifelse(diff_wym_a == 0, 12, 0)
  wym_b = diff_wym_b + ifelse(diff_wym_b == 0, 12, 0)
  
  wym_selected =  if (wym_a <= wym_b) {wym_a:wym_b} else {c(wym_a:12, 1:(wym_b))}
  
  # new name
  predictor_newname <- glue::glue("{predictor_variable}_{predictor_function}_{predictor_horizon}months")
  
  return(list(
    predictor_oldname = predictor_name,
    predictor_newname = predictor_newname,
    predictor_variable = predictor_variable,
    predictor_function = predictor_function,
    predictor_horizon = predictor_horizon,
    wym_selected = wym_selected
  ))
}

find_initial_months <- function(date, month_i){
  date <- as.Date(date)
  year <- as.numeric(format(date, "%Y")) + (month_i < as.numeric(format(date, "%m")))
  as.Date(paste0(year, "-", month_i, "-01"))
}

# one predictor column generator
one_column_predictor <- function(predictor_name,month_initialisation_index,catchment_data) {
  
  predictor_attributes <- split_predictor_name(
    predictor_name = predictor_name,
    month_initialisation_index = month_initialisation_index)
  
  predictor_var <- predictor_attributes$predictor_variable
  input_data <- catchment_data$raw_data_df
  
  # check if values of variable belong to catchment data
  if(!(predictor_var %in% names(catchment_data$raw_data_df))) {
    stop(paste0("predictor: ", predictor_var, " not found in database"))
  }
  
  
  # subset the predictor and months
  var <- input_data %>%
    select("wy_simple", all_of(predictor_var),"wym") %>% 
    subset(wym %in% predictor_attributes$wym_selected) %>%
    mutate(date_initial = find_initial_months(ymd(paste(wy_simple, wym, 1, sep = "-")),
                                              month_initialisation_index)) %>% 
    select(date_initial,all_of(predictor_var))
  
  ### find and remove incomplete year
  num_records_per_wy <- var %>% 
    count(date_initial) %>% 
    dplyr::rename(num = n)
  
  kickout_wy <- num_records_per_wy %>% 
    filter(num != predictor_attributes$predictor_horizon) %>% 
    pull(date_initial)
  
  if (length(kickout_wy)>0) {
    message("Warning. Removing INCOMPLETE PREDICTORS, WATER-YEARS: ",
            unique(interval_wys(lubridate::year(kickout_wy))))
  }
  
  #print(predictor_name)
  ## group by date_initial
  var <- var[!var$date_initial %in% kickout_wy,]
  var <- aggregate(select(var,all_of(predictor_var)),
                   by=list(date_initial=var$date_initial),
                   FUN=predictor_attributes$predictor_function) %>%
    select(c("date_initial",all_of(predictor_var))) %>% 
    mutate(wy_simple = year(date_initial)) %>% 
    select(c("wy_simple",all_of(predictor_var)))
  
  names(var)[names(var) == predictor_var] <- predictor_attributes$predictor_newname
  
  return(var)
}
## predictor dataframe
predictors_generator <- function(predictor_list,
                                 month_initialisation_index,
                                 catchment_data,
                                 remove_wys) {
  
  predictors <- lapply(predictor_list,
                       function(predictor_name)
                         one_column_predictor(
                           predictor_name = predictor_name,
                           month_initialisation_index = month_initialisation_index,
                           catchment_data = catchment_data
                         )
  )
  
  predictors[sapply(predictors, is.null)] <- NULL
  
  
  predictors <- Reduce(merge, predictors) %>%
    data.table(key = "wy_simple") %>%
    tidyr::drop_na() %>% 
    remove_years(remove_wys)
  
  return(predictors)
}

# target variable : volume and streamflow
predictant_generator <- function(
    forecast_horizon,
    catchment_data,
    water_units,
    log_transform,
    plot_transform_predictant) {
  
  library(stats)
  library(dplyr)
  # filter in the horizon period
  cond1 <- catchment_data$monthly_flows$wym >= forecast_horizon$init_forecast_index
  cond2 <- catchment_data$monthly_flows$wym <= forecast_horizon$end_forecast_index
  
  q_period_wy <- catchment_data$monthly_flows %>%
    subset(cond1 & cond2) %>%
    select(c("wy_simple", "Q_mm", "wym")) %>%
    mutate(wy_simple = as.integer(wy_simple))
  
  #volume in mm
  y <- stats::aggregate(
    x = Q_mm ~ wy_simple ,
    FUN = "sum",
    data = q_period_wy
  ) %>% dplyr::rename(volume = Q_mm)
  
  # streamflow in mm
  q <- q_period_wy %>%
    dcast(wy_simple ~ wym ,
          value.var = "Q_mm")
  
  #remove year when cummulative volume is zero
  remove_wy = which(y$volume == 0)
  if (length(remove_wy)>0) {
    y = y[-remove_wy,]
    q = q[-remove_wy,]
  }

  
  names(q)[names(q) != "wy_simple"] <- forecast_horizon$months_forecast_period
  
  ## convert units
  q_converted <-
    convert_flow(q = select(q,-wy_simple),
                 from = "mm/month",
                 to = water_units$q,
                 area_km2 = catchment_data$attributes_catchment$area_km2,
                 days_per_month = forecast_horizon$days_per_month_horizon
    )
  
  y_converted <- 
    convert_vol(
      v = select(y,-wy_simple),
      from = "mm",
      to = water_units$y,
      area_km2 = catchment_data$attributes_catchment$area_km2
    )
  
  #add water year column
  q_converted$wy_simple <- q$wy_simple 
  y_converted$wy_simple <- y$wy_simple
  
  
  ### normality
  p_value <- check_normality(y_converted$volume)
  p_value_transformed <- NULL
  function_transformed <- NULL
  function_y <- NULL
  
  if (log_transform) {
    #Shapiro-Wilk test
    
    volume_transformed <- apply_log_transform(y_converted$volume, p_value)
    p_value_transformed <- check_normality(volume_transformed)
    
    y_converted$volume_original = y_converted$volume
    y_converted$volume = volume_transformed
    
    if (!identical(y_converted$volume,y_converted$volume_original)) {
      function_y = "log"
    } 
    
    if (plot_transform_predictant) {
      plot_transform_predictant_distribution(y_converted$volume_original,
                                             volume_transformed,
                                             p_value,
                                             p_value_transformed,
                                             paste0(forecast_horizon$volume_span_text,". ",catchment_data$attributes_catchment$gauge_name," (",catchment_data$attributes_catchment$cod_cuenca, ")"),
                                             chart_name = paste0("data_output/figuras/histogramas_log_volumen/hist_volumen_transformado_",catchment_data$attributes_catchment$cod_cuenca,forecast_horizon$volume_span_text,".png")
                                            )
    }
  
    
  } 
  

  return(list(y = y_converted, q = q_converted, function_y = function_y, p_value = p_value, p_value_transformed = p_value_transformed))
  
}

remove_years <- function(variable, wys) {
  variable_train <- variable[!(variable$wy_simple %in% wys), ]
  return(variable_train)
}

subset_years <- function(variable, wy_train) {
  variable_train <- variable[variable$wy_simple %in% wy_train, ] %>%
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

training_set <- function(predictors, predictant, water_year_target) {
  if (is.null(predictors) | is.null(predictant)) { 
    stop("error: PREDICTORS OR PREDICTANT CANNOT BE NULL")
  }
  
  wy_train <- intersect(predictors$wy_simple, predictant$q$wy_simple)
  wy_train <- wy_train[wy_train != water_year_target]
  
  X_train <- predictors %>% subset_years(wy_train)
  y_train <- predictant$y %>% subset_years(wy_train)
  q_train <- predictant$q %>% subset_years(wy_train)
  
  
  return(
    list(
      X_train = X_train,
      y_train = y_train,
      q_train = q_train,
      wy_train = wy_train,
      y_train_pexc = pexc(y_train,"volume_original") %>% seco_normal_humedo_years()
      #water_year_target = water_year_target
    )
  )
}

testing_set <- function(predictors, predictant, water_year_target) {
  ### testing period
  
  # water_year_target must be in predictors
  X_test <- NULL
  if (water_year_target %in% predictors$wy_simple) {
    X_test <- predictors %>% subset_years(water_year_target)
  } else{
    message("PREDICTOR INTERVAL WYS: ", interval_wys(predictors$wy_simple))
    stop("YOU NEED VALID PREDICTORS FOR THE TARGET WY (SEE DATA FOR water_year_target)")
  }
  
  # water_year_target must be in predictant
  y_test <- NULL
  q_test <- NULL
  if (water_year_target %in% predictant$q$wy_simple) {
    y_test <- predictant$y %>% subset_years(water_year_target)
    q_test <- predictant$q %>% subset_years(water_year_target)
  } 
  
  return(list(
    X_test = X_test,
    y_test = y_test,
    q_test = q_test,
    wy_test = water_year_target
  ))
}


convert_items_to_lists <- function(lst) {
  lst <- lapply(lst, function(x) if (length(x) > 1) list(x) else x)
  return(lst)
}

initialisation_dates <- function(datetime_initialisation) {
  library(lubridate)
  #parse as datetime
  ymd_datetime = as_datetime(datetime_initialisation)
  #get gregorian year,month
  init_year = year(ymd_datetime)
  init_month = month(ymd_datetime)
  
  #get water-year year,month
  init_water_year = wateryear(ymd_datetime)
  init_water_year_month = wateryearmonth(init_month)
  
  return(as.list(environment())) 
}

grid_pred  <- function(  variable,
                         agg_months,
                         agg_func) {
  # explore variables (averaged)
  grid = expand.grid(variable, agg_months)
  predictors = paste(grid[,1], agg_func, paste0(grid[,2],"months"), sep = "_")
  return(predictors)
}

horizon_mode <- function(window_method,month_start,month_end) {
  return(as.list(environment()))
}

waterunits <- function(q, y) {return(list(q=q,y=y))}


preprocess_data <- function(
    catchment_code ,
    datetime_initialisation , 
    predictor_list, 
    horizon = horizon_mode(window_method = "dynamic", month_start = 9, month_end = 3),
    data_location_paths = get_default_datasets_path(meteo = NULL,hydro = "ERA5Ens_operacional"),
    water_units = waterunits(q = "m^3/s", y = "GL"),
    forecast_mode = "prediction",
    remove_wys = NULL,
    save_raw = F,
    y_transform = list(log_transform = T, plot_transform_predictant = F)
) {
  
  # save arguments
  info_list <- as.list(environment())
  
  # catchment data (raw forcings, flows)
  catchment_data <-
    read_catchment_data(
      catchment_code = catchment_code,
      water_units = water_units,
      remove_wys = remove_wys,
      data_location_paths = data_location_paths
    )
  #dates related to the initialisation
  date_init = initialisation_dates(datetime_initialisation)
  
  # set target period to forecast
  forecast_horizon <- get_forecast_horizon(
    date_init = date_init,
    horizon = horizon)
  
  month_initialisation_index = forecast_horizon$month_initialisation_index
  # create the predictors variables
  predictors <-
    predictors_generator(
      predictor_list = predictor_list,
      month_initialisation_index = month_initialisation_index,
      catchment_data = catchment_data,
      remove_wys = remove_wys
    )
  
  # create the target variable (VOLUME)
  predictant <-
    predictant_generator(
      forecast_horizon = forecast_horizon,
      catchment_data = catchment_data,
      water_units = water_units,
      log_transform = y_transform$log_transform,
      plot_transform_predictant = y_transform$plot_transform_predictant
    )
  
  info_list$y_transform = append(info_list$y_transform,within(predictant, rm(q,y)))
  
  # separate into training set based on water_year_target
  train_set <- 
    training_set(
      predictor = predictors,
      predictant = predictant,
      water_year_target = date_init$init_water_year
    )
  
  
  
  # append corrected predictor list
  info_list$predictor_list_corrected <- 
    predictors %>%
    select(- "wy_simple") %>%
    colnames
  
  # save results in structure
  results <- c(
    train_set,
    time_horizon = list(convert_items_to_lists(forecast_horizon)),
    info = list((info_list))
  )
  
  if (save_raw) {
    results <- c(results,raw_data = list(catchment_data))
  }
  
  if (forecast_mode == "prediction" | forecast_mode == "both"){
    # testing set 
    test_set  <- 
      testing_set(
        predictors = predictors,
        predictant = predictant,
        water_year_target = date_init$init_water_year)
    # append to results
    results <- c(results,test_set)
  }
  
  return(results)
  
  
}






######################### Testing

example_preprocess <- function(){
  
  catchment_code <- "7321002"
  datetime_initialisation = lubridate::make_date(2022,5)
  horizon = horizon_mode(window_method = "dynamic", month_start = 9, month_end = 3)
  predictor_list <- c("STORAGE_last_1months")
  remove_wys <- NULL#c(1990,1940,2013)
  water_units = waterunits(q = "m^3/s", y = "GL")
  forecast_mode <- "cv"
  data_location_paths = get_default_datasets_path(meteo = NULL, hydro = "ERA5Ens_operacional")
  save_raw = F
  y_transform = list(log_transform = T, plot_transform_predictant = F)
  
  data1 <- preprocess_data(
    catchment_code = catchment_code,
    datetime_initialisation = datetime_initialisation,
    horizon = horizon,
    predictor_list = predictor_list,
    remove_wys = remove_wys,
    water_units = water_units,
    forecast_mode = forecast_mode,
    save_raw = F,
    y_transform = y_transform
  )
  
  
  return(data1)
  
  
}
