#rm(list = ls())
#library(lubridate)
#library(hydromad)

# script transform from
# https://github.com/josephguillaume/hydromad/blob/master/R/convertFlow.R

convert_flow <- function(q,
                         from = "mm/month",
                         to = "m^3/s",
                         area_km2,
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

  # to monthly cubic meters 
  q_temporal = 
    hydromad::convertFlow(
      x = q, 
      from = from,
      to = to,
      area.km2 = area_km2)
  
  f_to = to.days/(365.25/12)
  f_from = from.days/(365.25/12)
  
  #https://stackoverflow.com/questions/51110216/how-to-multiply-each-column-by-each-scalar-in-r
  q_final = sweep(x = q_temporal,
                  MARGIN =  2,
                  STATS =  f_from/f_to,
                  FUN= "*") 

  return(q_final)
}


convert_vol <- function(
    v,
    from = "mm",
    to = "GL",
    area_km2
    ) {
  
  v =
  hydromad::convertFlow(
    x = v,
    from = from,
    to = to,
    area.km2 = area_km2
  )
  return(v)
}

# convert_data_input <- function(data_input) {
#   
# }
# 
# convert_data_fore <- function(data_fore,data_input) {
#   
# }
# 
# convert_q_fore  <- function(q_fore,data_input = data_input) {
#   q_fore_m3s = q_fore %>% 
#   convert_flow(from = "mm/month",
#                to = "m^3/s",
#                area_km2 = data_input$raw_data$attributes_catchment$area_km2,
#                days_per_month = data_input$plot_text$days_per_month_horizon,
#                timestep.default = "months"
#                )
# }


# ######### test
# library(dplyr)
# ### flow
# area_km2 = 2113.423
# days_per_month_horizon = readRDS(file = "base/data_input/tests/days_horizon.RDS")
# q_mm = readRDS(file = "base/data_input/tests/q.RDS") %>% data.frame()
# 
# q_m3s = convert_flow(
#   q = q_mm,
#   from = "mm/month",
#   to = "m^3/s",
#   area_km2 = area_km2,
#   days_per_month = days_per_month_horizon
# )
# 
# q_mm_test = convert_flow(
#   q = q_m3s,
#   from = "m^3/s",
#   to = "mm/month",
#   area_km2 = area_km2,
#   days_per_month = days_per_month_horizon
# )
# message(all.equal(q_mm,q_mm_test))
# # 
# # ####### volume
# v_mm = readRDS(file = "base/data_input/tests/vol.RDS")
#  
# v_GL = convert_vol(
#   v = v_mm,
#   from = "mm",
#   to = "GL",
#   area_km2 = area_km2
# )
# 
# v_mm_test = convert_vol(
#   v = v_GL,
#   to = "mm/yr",
#   from = "GL/yr",
#   area_km2 = area_km2
# )
# 
# message(all.equal(v_mm,v_mm_test))
# # 
