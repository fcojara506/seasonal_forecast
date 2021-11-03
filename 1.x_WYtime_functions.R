library(dplyr)
library(data.table)
library(lubridate)
#library(aweek)

get_week          <- function(x){date2week(x,week_start = 1,numeric = T)}

wy_firstday       <- function(year){
  sec=seq.Date(as.Date(paste0(year,"-03-26")),by="day",length.out = 15)
  dia=sec[weekdays(sec)=="Monday"][1]
  return(dia)}

wy_firstday_list=lapply(1900:2100, wy_firstday) %>%
  reshape2::melt() %>%
  rename(first.day=value) %>%
  mutate(yr=year(first.day),L1=NULL) %>%
  data.table()%>%
  setkey("yr")

wy                <- function(x,primer_dia){result=fifelse(x >= primer_dia,year(primer_dia),year(primer_dia)-1)}
wy_woy            <- function(x,primer_dia_wy){trunc(difftime(x,primer_dia_wy,units="days")/7)+1}

# based on start of the year at YYYY-04-01
wym_simple        <- function(x){fifelse(month(x)>3, month(x)-3,month(x)+9)}
wy_simple         <- function(x){fifelse(month(x)>3, year(x),year(x) - 1)}
