library(data.table)
library(dplyr)
library(lubridate)
library(glue)
library(tibble)

# import local libraries
source("base/Convert_units.R")


get_dataset_filenames <- function(dataset_meteo, dataset_hydro) {

  # catchment attributes
    catchments_attributes_filename <-"data_input/attributes/attributes_49catchments_ChileCentral.feather"
    # flow in average monthly mm
    flows_filename <- "data_input/flows/flows_mm_monthly_49catchments_ChileCentral.feather"
    # variable from the hydrology model
    hydro_filename <- NULL
    hydro_filename <- if(!is.null(dataset_hydro)) glue::glue("data_input/storage_variables/","hydro_variables_monthly_catchments_ChileCentral_{dataset_hydro}.feather")
    # meteorological data from dataset_meteo
    meteo_filename <- NULL
    meteo_filename <- if(!is.null(dataset_meteo)) glue::glue("data_input/meteo_variables/","meteo_monthly_catchments_ChileCentral_{dataset_meteo}_1979_2022.feather")
    # monthly standard climate indices
    climateindex_filename <- "data_input/climate_index_variables/indices_mensuales_1988_2020.feather"
    
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
  # read feather files from the catchment data and 
  # subset given the catchment code
  cod_cuenca <- factor()
  wym <- numeric()
  df_variable <- NULL
  
  if (!is.null(filename) && file.exists(filename)) {
    # read the feather file and subset by catchment code
    df_variable <- feather::read_feather(filename) %>%
      mutate(wym = as.numeric(wym)) %>%
      data.table(key = c("wy_simple", "wym")) %>%
      subset(cod_cuenca == catchment_code, select = -cod_cuenca)
  }
  
  return(df_variable)
}

read_and_process_catchment_data <- function(catchment_code,
                           dataset_meteo,
                           dataset_hydro,
                           remove_wys,
                           units_q) {
  #' @title Read and Process Catchment Data
  #'
  #' @description This script reads and processes data for a specific catchment. 
  #' It imports the feather library and then defines the filenames
  #' for the catchment attributes, flows, meteorological data, and hydrological 
  #' variables using the get_dataset_filenames() function. 
  #'
  #' @param dataset_meteo The dataset for the meteorological data
  #' @param dataset_hydro The dataset for the hydrological data
  #' @param catchment_code The code for the specific catchment
  #' @param remove_wys A list of years to be removed from the data
  #' @param units_q The units for the flow data
  #'
  #' @return A list of processed data and the catchment attributes
  #'
  #' @examples
  #' catchment_data <- read_and_process_catchment_data(dataset_meteo = dataset_meteo,
  #'                                                   dataset_hydro = dataset_hydro,
  #'                                                   catchment_code = "4311001",
  #'                                                   remove_wys = c(1990, 1995),
  #'                                                   units_q = "m^3/s")
  
  library(feather)
  if(missing(catchment_code) || missing(dataset_meteo) || missing(dataset_hydro) || missing(remove_wys) || missing(units_q)) {
    stop("Missing parameter. All parameters are required (catchment_code, dataset_meteo, dataset_hydro, remove_wys, units_q)")
  }
  
  #initialise NULL attributes
  attributes_catchment <- NULL
  monthly_flows <- NULL
  monthly_meteo <- NULL
  monthly_hydro <- NULL
  monthly_climateindex <- NULL
  
  # path filenames to catchment attributes
  data_filenames <- get_dataset_filenames(
    dataset_meteo = dataset_meteo,
    dataset_hydro = dataset_hydro)
  
  # read all the catchments attributes from the study area
  attributes_catchments <- read_feather(data_filenames$catchments_attributes_filename)
  
  #check if the catchment code exists
  if(!(catchment_code %in% attributes_catchments$cod_cuenca)) {
    stop(paste0(catchment_code, " not found in catchments"))
  }
  
  # subset the attributes for the specific catchment
  attributes_catchment <- attributes_catchments %>% 
    subset(cod_cuenca == catchment_code)
  
  # read and subset the flows, meteorological data, and hydrological data
  monthly_flows        <- read_and_subset_variable(
    filename = data_filenames$flows_filename,
    catchment_code = catchment_code) %>%
    remove_years(remove_wys)
  
  # convert the flow data to the desired units
  monthly_flows <- monthly_flows %>%
    data.table() %>%
    mutate(date = lubridate::dmy(paste0("01",
    sprintf("%02d", wy_month_to_month(wy_month = wym)),
    wy_to_year(wy = wy_simple, wym = wym)
    ))) %>% 
    mutate(days_months = lubridate::days_in_month(date)) %>%
    select(-date) %>% 
    mutate(Q_converted =
             hydromad::convertFlow(
      Q_mm,
      from = "mm/month",
      to = units_q,
      area.km2 = attributes_catchment$area_km2)) %>%
    select(-days_months)
  # meteorological data
  monthly_meteo       <- read_and_subset_variable(
    filename = data_filenames$meteo_filename,
    catchment_code = catchment_code) %>%
    remove_years(remove_wys)
  # data from the hydrological model
  monthly_hydro       <- read_and_subset_variable(
    filename = data_filenames$hydro_filename,
    catchment_code = catchment_code) %>%
    remove_years(remove_wys)
  
  # read and process the climate index data
  monthly_climateindex <- data_filenames$climateindex_filename %>% 
    feather::read_feather() %>%
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
get_forecast_horizon <- function(
    month_initialisation,
    horizon_strategy,
    horizon_month_start = "oct",
    horizon_month_end =  "mar",
    wy_holdout
    ) {
  
  months_wy <- c("abr", "may", "jun", "jul", "ago", "sep",
    "oct", "nov", "dic", "ene", "feb", "mar")
  
  # indices within months_wy vector
  month_initialisation_index <- match(month_initialisation, months_wy)
  init_target_index <- match(horizon_month_start, months_wy)
  end_target_index <- match(horizon_month_end, months_wy)
  # check horizon_strategy
  horizon_strategy <- match.arg(horizon_strategy,c("dynamic","static"))
  # start month
  if (horizon_strategy == "dynamic") {
    if (month_initialisation_index < init_target_index) {
      init_forecast_index <- init_target_index
    } else {
      init_forecast_index <- month_initialisation_index
    }
  } else if (horizon_strategy == "static") {
    init_forecast_index <- init_target_index
  }
  
  # end month
  if (horizon_strategy == "dynamic") {
    end_forecast_index <- end_target_index
  } else if (horizon_strategy == "static") {
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
  #gregorian year
  year_initialisation <- wy_to_year(
    wy = wy_holdout,
    wym = month_initialisation_index + 1)
  #gregorian month number (january = 1)
  month_initialisation_num <- wy_month_to_month(
    wy_month = month_initialisation_index)
  #format date: %d %b %Y
  date_initialisation <- glue("1 {month_initialisation_num} {year_initialisation}")
  #format date: %Y-%m-%d
  datetime_initialisation <- as.Date(glue("{year_initialisation}/{month_initialisation_num}/01"))
  ### forecast horizon dates
  ###
  # (1) forecast horizon start date FHSD
  #year of FHSD
  year_init <- wy_to_year(
    wy = wy_holdout,
    wym = init_forecast_index)
  #month of FHSD
  month_init <- wy_month_to_month(init_forecast_index)
  #date of FHSD %Y-%m-%d
  date_init_forecast <- as.Date(glue("{year_init}/", "{month_init}/", "{01}"))
  # (2) forecast horizon end date FHED
  #year of FHED
  year_end <-  wy_to_year(wy = wy_holdout, wym = end_forecast_index)
  #month of FHED
  month_end <- wy_month_to_month(end_forecast_index)
  #date of FHED %Y-%m-%d
  date_end_forecast <- as.Date(glue("{year_end}/", "{month_end}/", "{01}"))
  # months in the forecast horizon
  forecast_horizon_months <- seq.Date(
    from = date_init_forecast,
    to = date_end_forecast,
    by = "1 months")
  # number of day in each month of the forecast horizon
  days_per_month_horizon  <- lubridate::days_in_month(forecast_horizon_months)
  # start and end of forecast horizon (%b %Y and %b)
  # change date format to Spanish (for convenience)
  system_locale <- Sys.getlocale(category = "LC_TIME")
  spanish_locale <- Sys.setlocale("LC_TIME", "es_ES.UTF-8")
  
  volume_span_text_v2     <- 
    forecast_horizon_months %>%
    format("%b %Y") %>%
    .[c(1, length(.))] %>%
    paste(collapse = " - ")
  
  volume_span_text        <-
    forecast_horizon_months %>%
    format("%b") %>%
    .[c(1, length(.))] %>%
    paste(collapse = ",") %>%
    paste0("[", ., "]")
  #return to system locale
  system_locale <- Sys.setlocale(category = "LC_TIME", locale = system_locale)
  
  ### target water year
  # start date
  date_init_wy <-   as.Date(glue("{wy_to_year(wy_holdout,1)}/04/01"))
  # end date
  date_end_wy <-   as.Date(glue("{wy_to_year(wy_holdout,12)}/03/31"))
  # months in target water year
  wy_holdout_months   <- seq.Date(from = date_init_wy,
                                     to = date_end_wy,
                                     by = "1 months")
  #results
  return(
    list(
      init_forecast_index = init_forecast_index,
      end_forecast_index = end_forecast_index,
      months_forecast_period = months_forecast_period,
      months_before_initialisation = months_before_initialisation,
      month_initialisation_index = month_initialisation_index,
      month_initialisation =  month_initialisation,
      months_wy = months_wy,
      date_initialisation = date_initialisation,
      datetime_initialisation = datetime_initialisation,
      volume_span_text    = volume_span_text,
      volume_span_text_v2 = volume_span_text_v2,
      forecast_horizon_months = forecast_horizon_months,
      days_per_month_horizon = days_per_month_horizon,
      wy_holdout_months = wy_holdout_months
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
  # new name
  predictor_newname <- glue::glue("{predictor_variable}_{predictor_function}_{predictor_horizon}months")
  
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
    
    predictor_attributes <- split_predictor_name(
      predictor_name = predictor_name,
      month_initialisation_index = month_initialisation_index)
    
    input_data <- catchment_data$raw_data_df
    
    # check if values of variable belong to catchment data
    if(!(predictor_attributes$predictor_variable %in% names(catchment_data$raw_data_df))) {
      stop(paste0("predictor: ",predictor_attributes$predictor_variable, " not found in database"))
    }

    tryCatch({
    # subset data in forecast horizon interval
      var <- input_data %>%
        select("wy_simple", all_of(predictor_attributes$predictor_variable),"wym") %>% 
        subset(wym < month_initialisation_index) %>%
        subset(wym > month_initialisation_index - predictor_attributes$predictor_horizon -1) %>%
        select("wy_simple", all_of(predictor_attributes$predictor_variable))
      
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
      
      var <- var %>%
        subset(!(wy_simple %in% kickout_wy)) %>%
        aggregate(
          FUN = predictor_attributes$predictor_function,
          by = list(wy_simple = .$"wy_simple"),
          drop = T
        ) %>%
        select(c("wy_simple",
                 all_of(predictor_attributes$predictor_variable)))
      
      names(var)[names(var) == predictor_attributes$predictor_variable] <- predictor_attributes$predictor_newname
      
      return(var)
    }
    ,
    error = function(e) {NULL})
  }
## predictor dataframe
predictors_generator <- function(predictor_list,
                                 forecast_horizon,
                                 catchment_data) {
  
  predictors <- lapply(predictor_list,
                      function(predictor_name)
                        
                        one_column_predictor(
                          predictor_name = predictor_name,
                          month_initialisation_index =  forecast_horizon$month_initialisation_index,
                          catchment_data = catchment_data
                        )
                      )
  
  predictors[sapply(predictors, is.null)] <- NULL
  
  
  predictors <- Reduce(merge, predictors) %>%
    data.table(key = "wy_simple") %>%
    tidyr::drop_na()
  
  return(predictors)
}

# target variable : volume and streamflow
predictant_generator <- function(
    forecast_horizon,
    catchment_data,
    units_q,
    units_y) {
  
  library(stats)
  library(dplyr)
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
  
  
  names(q)[names(q) != "wy_simple"] <- forecast_horizon$months_forecast_period
  
  ## convert units
  q_converted <-
    convert_flow(q = select(q,-wy_simple),
                 from = "mm/month",
                 to = units_q,
                 area_km2 = catchment_data$attributes_catchment$area_km2,
                 days_per_month = forecast_horizon$days_per_month_horizon
                 )
  
  y_converted <- 
    convert_vol(
      v = select(y,-wy_simple),
      from = "mm",
      to = units_y,
      area_km2 = catchment_data$attributes_catchment$area_km2
  )
  
  #add water year column
  q_converted$wy_simple <- q$wy_simple
  y_converted$wy_simple <- y$wy_simple
  
  return(list(y = y_converted, q = q_converted))
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


# dataframe_to_list <- function(df) {
#   ensemble_names <- unique(df$ens)
#   df_list <- lapply(ensemble_names,
#                    function(ens_i)
#                      subset(df, ens == ens_i, select = -ens))
#   names(df_list) <- ensemble_names
#   return(df_list)
# }


training_set <- function(predictors, predictant, wy_holdout) {
  if (is.null(predictors) | is.null(predictant)) { 
    stop("error: PREDICTORS OR PREDICTANT CANNOT BE NULL")
  }
  
  wy_train <- intersect(predictors$wy_simple, predictant$q$wy_simple)
  wy_train <- wy_train[wy_train != wy_holdout]
  
  ### training period for 
  # X_train <- predictors %>%
  #   dataframe_to_list %>%
  #   lapply(function(x) subset_years(x, wy_train))
  
  X_train <- predictors %>% subset_years(wy_train)
  y_train <- predictant$y %>% subset_years(wy_train)
  q_train <- predictant$q %>% subset_years(wy_train)
  return(
    list(
      X_train = X_train,
      y_train = y_train,
      q_train = q_train,
      wy_train = wy_train
      #wy_holdout = wy_holdout
    )
  )
}

testing_set <- function(predictors, predictant, wy_holdout) {
  ### testing period
  
  # wy_holdout must be in predictors
  if (wy_holdout %in% predictors$wy_simple) {
    # X_test <- predictors %>%
    #   dataframe_to_list %>%
    #   lapply(function(x)
    #     subset_years(x, wy_holdout))
    X_test <- predictors %>% subset_years(wy_holdout)
  } else{
    X_test <- NULL
    message("PREDICTOR INTERVAL WYS: ", interval_wys(predictors$wy_simple))
    stop("YOU NEED VALID PREDICTORS FOR THE TARGET WY (SEE DATA FOR WY_HOLDOUT)")
  }
  
  # wy_holdout must be in predictant
  if (wy_holdout %in% predictant$q$wy_simple) {
    y_test <- predictant$y %>% subset_years(wy_holdout)
    q_test <- predictant$q %>% subset_years(wy_holdout)
  } else{
    y_test <- NULL
    q_test <- NULL
  }
  
  return(list(
    X_test = X_test,
    y_test = y_test,
    q_test = q_test
  ))
}

wy_to_year <- function(wy, wym) {
  #Take a wateryear(wy) to gregorian year
  wy <- as.integer(wy)
  wym <- as.integer(wym)
  
  gregorian_year <- as.integer(ifelse(wym < 11, wy, wy + 1))
  return(gregorian_year)
}

wy_month_to_month <- function(wy_month) {
  month <- wy_month + 3
  
  month <-
    ifelse(month > 12,
           month - 12,
           month)
  return(month)
}

preprocess_data <- function(
    catchment_code = "5410002",
    dataset_meteo  = "ens30avg",
    dataset_hydro  = "ERA5Ens_SKGE",
    month_initialisation = "jun",
    wy_holdout = 2016,
    remove_wys = NA,
    horizon_strategy = "dynamic",
    horizon_month_start = "sep",
    horizon_month_end = "mar",
    predictor_list = c("pr_sum_-1months","tem_mean_2months"),
    units_q = "mm/month",
    units_y = "mm",
    test_subset = T
    ) {
  
  # catchment data (raw forcings, flows)
  catchment_data <-
    read_and_process_catchment_data(
    catchment_code = catchment_code,
    dataset_meteo = dataset_meteo,
    dataset_hydro = dataset_hydro,
    remove_wys = remove_wys,
    units_q = units_q
  )
  
  # set target period to forecast
  forecast_horizon <-
    get_forecast_horizon(
    month_initialisation = month_initialisation,
    horizon_strategy = horizon_strategy,
    horizon_month_start = horizon_month_start,
    horizon_month_end =  horizon_month_end,
    wy_holdout = wy_holdout
  )

  # create the predictors variables
  predictors <-
    predictors_generator(
    predictor_list = predictor_list,
    forecast_horizon = forecast_horizon,
    catchment_data = catchment_data
  )

  # create the target variable (VOLUME)
  predictant <-
    predictant_generator(
    forecast_horizon = forecast_horizon,
    catchment_data = catchment_data,
    units_q = units_q,
    units_y = units_y
    )
  
  # separate into training set based on wy_holdout
  train_set <- 
    training_set(
    predictor = predictors,
    predictant = predictant,
    wy_holdout = wy_holdout
    )
  
  # save function arguments
  info_list <- as.list(environment())
  # append corrected predictor list
  info_list$predictor_list_corrected <- 
             predictors %>%
             select(-1, -2) %>%
             colnames
  
  # save results in structure
  results <- c(
    train_set,
    raw_data = list(catchment_data),
    time_horizon = list(forecast_horizon),
    info = list(info_list)
  )
  
  if (test_subset){
    # testing set 
    test_set  <- 
      testing_set(
        predictors = predictors,
        predictant = predictant,
        wy_holdout = wy_holdout)
    # append to results
    results <- c(results,test_set)
  }
  
  return(results)
  
  
}

test_preprocess <- function(){
  
  catchment_code <- "5410002"
  dataset_meteo  <- "ens30avg"
  dataset_hydro  <- "ERA5Ens_SKGE"
  month_initialisation <- "jun"
  horizon_strategy <- "dynamic"
  horizon_month_start <- "sep"
  horizon_month_end <- "mar"
  predictor_list <- c("pr_sum_-1months","tem_mean_1months")
  wy_holdout <- 2010
  remove_wys <- c(1990,1980,2013)
  units_q <- "m^3/s"
  units_y <- "GL"
  test_subset <- T
  
  
data1 <- preprocess_data(
  month_initialisation = "oct",
  wy_holdout = 2022)

data1_1 <- preprocess_data(
  month_initialisation = "oct",
  wy_holdout = 2022,
  units_q = "m^3/s",
  units_y = "GL")

data2 <- preprocess_data(
  catchment_code = "7321002",
  month_initialisation = "sep",
  dataset_meteo  = "ens30avg",
  horizon_strategy = "dynamic",
  horizon_month_start = "oct",
  horizon_month_end = "mar",
  predictor_list = c("pr_sum_-1months","tem_mean_3months"),
  wy_holdout = 2022,
  remove_wys = NA
)

data3 <- preprocess_data(
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
  test_subset = test_subset
)


return(

  list(
  data1 = data1,
  data2 = data2,
  data3 = data3)

)
}