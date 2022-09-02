rm(list = ls())
library(lubridate)
library(hydromad)
# partially inspired by
# https://github.com/josephguillaume/hydromad/blob/master/R/convertFlow.R

area_km2 = 2113.423
days_per_month_horizon = readRDS(file = "days_horizon.RDS")
q_mm = readRDS(file = "q.RDS")
#v_mm = readRDS(file = "vol.RDS")

convert_flow <- function(q,
                         from = "mm/month",
                         to="m^3/s",
                         area_km2 = area_km2,
                         days_per_month = 365.25/12
                         ) {
  
  # to monthly cubic meters 
q_m3 = 
  hydromad::convertFlow(
    x = q, 
    from =from,
    to = "m3/month",
    area.km2 = area_km2)## q_mm*area_km2*1000

q_final = sweep(x = q_m3,
              MARGIN =  2,
              STATS =  1/days_per_month/86400,
              FUN= "*") #https://stackoverflow.com/questions/51110216/how-to-multiply-each-column-by-each-scalar-in-r

return(q_final)
}

convert_flow2 <- function(q,
                         from = "mm/month",
                         to = "m^3/s",
                         area_km2 = area_km2,
                         days_per_month = 365.25/12,
                         timestep.default = "months"
) {
  
  ## extract timestep from each of 'from' and 'to'
  from.step = to.step = timestep.default
  if (any(grep("/", from))) {from.step = sub("^.*/ *", "", from)}
  if (any(grep("/", to))) {to.step = sub("^.*/ *", "", to)}
  ## extract multiplier from each timestep
  from.step = gsub("[0-9\\. ]", "", from.step)
  to.step   = gsub("[0-9\\. ]", "", to.step)

  ## number of seconds for each possible time step
  timefactors <- alist(
    seconds = , second = , sec = , s = days_per_month,
    months = , month = , mon = 365.25/12
    )
  
  from.days <- do.call(switch, c(from.step, timefactors))
  to.days <- do.call(switch, c(to.step, timefactors))
  
  #print(from.days)
  #print(to.days)

  # to monthly cubic meters 
  q_temporal = 
    hydromad::convertFlow(
      x = q, 
      from = from,
      to = to,
      area.km2 = area_km2)
  
  f = days_per_month_horizon/(365.25/12)
  f_to = to.days/(365.25/12)
  f_from = from.days/(365.25/12)
  
  print(f_to)
  print(f_from)
  
  q_final = sweep(x = q_temporal,
                  MARGIN =  2,
                  STATS =  f_from/f_to,
                  FUN= "*") #https://stackoverflow.com/questions/51110216/how-to-multiply-each-column-by-each-scalar-in-r

  return(q_final)
}

q_m3s_v1 = convert_flow(
  q = q_mm,
  from = "mm/month",
  to = "m^3/s",
  area_km2 = area_km2,
  days_per_month = days_per_month_horizon
)

q_m3s_v2 = convert_flow(
  q = q_mm,
  from = "mm/month",
  to = "m^3/s",
  area_km2 = area_km2,
  days_per_month = days_per_month_horizon
)

q_mm_v2 = convert_flow2(
  q = q_m3s_v1,
  from = "m^3/s",
  to = "mm/month",
  area_km2 = area_km2,
  days_per_month = days_per_month_horizon
)






