# ----------------------------------------------------------------------------
# Nombre del Proyecto: Pronóstico híbridos del volumen estacional en Chile Central 
# Autor(es): Francisco Jara
# Fecha de Finalización: 2023/04/31
# Contacto: pronostico.caudales@gmail.com
# GitHub: https://github.com/fcojara506
# ----------------------------------------------------------------------------

#library(hydromad)

# script modified from hydromad
# https://github.com/josephguillaume/hydromad/blob/master/R/convertFlow.R

convertunit <-
  function(x, from = "mm", to = "mm", area.km2 = -1,
           timestep.default = "days") {
    if (from == "cumecs") from <- "m^3/sec"
    if (to == "cumecs") to <- "m^3/sec"
    
    ## extract timestep from each of 'from' and 'to'
    from.step <- to.step <- timestep.default
    if (any(grep("/", from))) {
      from.step <- sub("^.*/ *", "", from)
      from <- sub(" */.*$", "", from)
    }
    if (any(grep("/", to))) {
      to.step <- sub("^.*/ *", "", to)
      to <- sub(" */.*$", "", to)
    }
    ## extract multiplier from each timestep
    from.mult <- gsub("[^0-9\\.]", "", from.step)
    from.step <- gsub("[0-9\\. ]", "", from.step)
    to.mult <- gsub("[^0-9\\.]", "", to.step)
    to.step <- gsub("[0-9\\. ]", "", to.step)
    ## number of seconds for each possible time step
    timefactors <- alist(
      millisecond = , ms = 0.001,
      seconds = , second = , sec = , s = 1,
      minutes = , minute = , min = 60,
      hours = , hour = , hr = , h = 60 * 60,
      days = , day = , d = 24 * 60 * 60,
      weeks = , week = 7 * 24 * 60 * 60,
      months = , month = , mon = 30.4375 * 24 * 60 * 60,
      annum = , anna = , a = , years = , year = , yr = , y = 365.25 * 24 * 60 * 60
    )
    from.secs <- do.call(switch, c(from.step, timefactors))
    to.secs <- do.call(switch, c(to.step, timefactors))
    if (is.null(from.secs) || is.null(to.secs)) {
      stop("unrecognised time unit")
    }
    if (nchar(from.mult) > 0) from.secs <- from.secs * as.numeric(from.mult)
    if (nchar(to.mult) > 0) to.secs <- to.secs * as.numeric(to.mult)
    ## handle volumes
    depthUnits <- c("mm", "cm", "metres", "km", "inches", "feet", "ft")
    volUnits <- c(
      "mL", "cL", "dL", "L", "daL", "hL", "kL", "ML", "GL", "TL",
      "cm3", "dm3", "m3", "km3", "ft3",
      "cm^3", "dm^3", "m^3", "km^3", "ft^3"
    )
    allUnits <- c(depthUnits, volUnits)
    from <- match.arg(from, allUnits)
    to <- match.arg(to, allUnits)
    if ((from %in% depthUnits) != (to %in% depthUnits)) {
      if (missing(area.km2)) stop("need to give 'area.km2'")
    }
    
    ## factors to convert to mm (*) or from mm (/) per timestep
    Litres <- (1 / area.km2) / 1e6
    vfactors <- alist(
      mm = 1,
      cm = 10,
      metres = , metre = , m = 1000,
      km = 1000 * 1000,
      inches = , inch = , `in` = 25.4,
      feet = , ft = 304.8,
      mL = , cm3 = , `cm^3` = 1e-3 * Litres,
      cL = 0.01 * Litres,
      dL = 0.1 * Litres,
      L = , dm3 = , `dm^3` = Litres,
      daL = 10 * Litres,
      hL = 100 * Litres,
      kL = , m3 = , `m^3` = 1000 * Litres,
      ML = 1e6 * Litres,
      GL = 1e9 * Litres,
      TL = , km3 = , `km^3` = 1e12 * Litres,
      ft3 = , `ft^3` = 1 / 0.0353146667 * Litres,
      stop("unrecognised volume unit")
    )
    ## first convert to mm
    x <- x * do.call(switch, c(from, vfactors))
    ## now convert to required unit 'to'
    x <- x / do.call(switch, c(to, vfactors))
    ## now convert timesteps
    x <- x * (to.secs / from.secs)
    x
  }

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
    convertunit(
    x = v,
    from = from,
    to = to,
    area.km2 = area_km2
  )
  return(v)
}

test_conversion <- function(s = c(4,2,3,4)) {
  
  convertunit(x = s,from = "mm",
              to = "m3/s",
              area.km2 = 1,
              timestep.default = "months")
  
  convert_flow(q = t(s),
               from = "mm/month",
               to = "m3/s",
               area_km2 = 1,
              # days_per_month = c(31,31,31,31),
               timestep.default = "months"
               )
  
  convert_vol(v = s,from = "mm",to = "GL",area_km2 = 1)
  
}


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
