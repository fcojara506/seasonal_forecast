
create_period_id <- function(dates, initial_month, period) {
  
  period_id <- rep(NA, length(dates))
  for (i in 1:length(dates)) {
    date <- as.Date(dates[i])
    if (month(date) >= initial_month - period && month(date) <= initial_month-1) {
      period_id[i] <- year(date)
    }
  }
  period_id
}


create_period_id <- function(dates, initial_month, period) {
  
  period_id <- rep(NA, length(dates))
  for (i in 1:length(dates)) {
    date <- as.Date(dates[i])
    
    diff_wym_a = (initial_month - period)%% 12 
    diff_wym_b = (initial_month - 1)%% 12
    wym_a = diff_wym_a + ifelse(diff_wym_a == 0, 12, 0)
    wym_b = diff_wym_b + ifelse(diff_wym_b == 0, 12, 0)
    
    year_1 =  if (wym_a <= wym_b) {wym_a:wym_b} else {c(wym_a:12, 1:(wym_b))}
    }
  }
  period_id
}
  
dates = c("1978-11-01","1978-12-01","1979-01-01","1979-10-01","1979-12-01","1980-01-01","1980-09-01")
period_id <- create_period_id(dates, 2, 6)
