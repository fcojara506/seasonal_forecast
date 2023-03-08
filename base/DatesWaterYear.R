
wateryear         <- function(datetime) {
  fifelse(lubridate::month(datetime) > 3,
          lubridate::year(datetime),
          lubridate::year(datetime) - 1)
}

wateryearmonth <- function(month)  {
  wym = as.numeric(as.character(month))
  wym = ifelse(wym > 3, wym - 3, wym + 12 - 3)
  return(wym)
}

wateryear2year <- function(wy, wym) {
  #Take a wateryear(wy) to gregorian year
  wy <- as.integer(wy)
  wym <- as.integer(wym)
  
  gregorian_year <- as.integer(ifelse(wym < 11, wy, wy + 1))
  return(gregorian_year)
}

wateryearmonth2month <- function(wy_month) {
  month <- wy_month + 3
  
  month <-
    ifelse(month > 12,
           month - 12,
           month)
  return(month)
}