

 
create_period_id <- function(dates, initial_month, period) {
  library(lubridate)
  
  initial_months = find_initial_months(date = dates,month_i = initial_month)

}


  
dates = c("1978-11-01","1978-12-01","1979-01-01","1979-10-01","1979-12-01","1980-01-01","1980-09-01")
initial_month = 3
period = 7
find_initial_months(dates,month_initialisation_index)
