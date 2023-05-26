rm(list = ls())
####

library(data.table)
library(dplyr)
library(reshape2)
library(bestNormalize)
library(magrittr)

# (*) Segun analisis exploratorios de ajuste de precipitacion:
# N mayor (40-50) es mejor en estacion humeda y cuencas humedas,
# N menor (<10 o <20) es mejor en estacion seca y cuencas secas,
# se escoge N=30 como consenso para todo el dominio (explorar para cuencas especificas!)

N_miembros_meteorologia = 30

####

## Sim Schaake usa dias "similares" basados en dias anteriores de reanalisis
## Para esto se debe definir una metrica de similitud multivariada (similar a RMSE)
## (multivariable: inter-variables, inter-locaciones)
## Como caso paeticular se usan dos variables (precip y temp),
## y una sola locacion (la misma cuenca)

## Verificacion tipo leave-3-out
## Bloques de 3 agnos periodo 1979-2020 (n = 42)
## E.g.,
## Periodo calibracion: 1979-2017 (n = 39)
## Periodo validacion: 2018-2020 (n = 3)

## Todos los analisis de estratifican por mes

####

## Funcion que busca dias similares/analogos
## se podria simplificar, este es el caso general!
## (caso general: varias locaciones y varias variables)

## "cal" es periodo de calibracion y "proy" es periodo de proyeccion/validacion
## sim.cal, sim.proy: son dataframes de cuatro columnas, despues de usar melt()
## columnas: locacion, date (Date), variable, value (num)
## N: numero de dias similares retenidos
## Inputs tienen que ser estratificados por mes

DiaSim <- function(sim.cal, sim.proy, N) {
  
  cal.loc <- as.character(sim.cal$loc)
  cal.date <- as.character(sim.cal$date)
  cal.var <- as.character(sim.cal$variable)
  cal.val <- sim.cal$value
  
  proy.loc <- as.character(sim.proy$loc)
  proy.date <- as.character(sim.proy$date)
  proy.var <- as.character(sim.proy$variable)
  proy.val <- sim.proy$value
  
  Dias <- unique(proy.date)
  Vars <- unique(proy.var)
  Locs <- unique(proy.loc)
  Anas <- unique(cal.date)
  i = 0
  Sim <-
    lapply(Dias, function(Dia) {
      sapply(Anas, function(Ana) {
        lapply(Vars, function(Var) {
          lapply(Locs, function(Loc) {
            i <<- i+1
            (proy.val[proy.loc %in% Loc &
              proy.date %in% Dia & proy.var %in% Var] -
              cal.val[cal.loc %in% Loc &
                cal.date %in% Ana & cal.var %in% Var])^2
          })
        }) %>%
          unlist(recursive = T) %>%
          mean() %>%
          sqrt()
      }, USE.NAMES = F) %>%
        data.table(date = Anas, simil = .) %>%
        arrange(simil) %>%
        .[1:N, ]
    }) %>% setNames(Dias)
  print(i)
  return(Sim)
}


# version mas rapida ocupando data.table
DiaSim2 <- function(sim.cal, sim.proy, N) {
  
  # Renombrando columnas para 'sim.cal' y convirtiendo a data.table
  dt_sim_cal = data.table(sim.cal) %>%
    rename(date_ana = date, value_sim = value)
  
  # Renombrando columnas para 'sim.proy' y convirtiendo a data.table
  dt_sim_proy = data.table(sim.proy) %>%
    rename(date_dia = date, value_proy = value)
  
  # Uniendo 'dt_sim_cal' y 'dt_sim_proy' por las columnas 'loc' y 'variable'
  # Se permite el producto cartesiano y se calcula el cuadrado de la diferencia entre 'value_proy' y 'value_sim'
  dt <- merge(dt_sim_cal, dt_sim_proy, by = c("loc", "variable"), allow.cartesian = TRUE)[, dif := (value_proy - value_sim) ^2]
  
  # Calculando la raíz cuadrada de la media de 'dif' y agrupando por 'date_ana' y 'date_dia'
  dt2 = dt[, .(simil = sqrt(mean(dif))), by = .(date_ana, date_dia)]
  
  # Obteniendo días únicos simulados y ordenándolos
  dias_simulados = sort(unique(dt2$date_dia))
  
  # Aplicando función lapply a 'dias_simulados'
  dt3 <- lapply(dias_simulados, function(x) {
    # Filtrando 'dt2' por 'date_dia' y ordenando por 'simil'
    dt_filtered = dt2[date_dia == x][order(simil)]
    # Eliminando la columna 'date_dia'
    dt_filtered$date_dia <- NULL
    # Cambiando la columna 'date_ana' a string
    dt_filtered[, date := as.character(date_ana)]
    # Renombrando 'date_ana' a 'date'
    setnames(dt_filtered, "date_ana", "date")
    # Retornando los primeros 'N' elementos de 'dt_filtered'
    return(dt_filtered[1:N])
  })
  
  # Nombrando elementos de 'dt3' con 'dias_simulados'
  names(dt3) = dias_simulados
  
  # Retornando 'dt3'
  return(dt3)
}





####
dias_similares_operativo  <- function(N = 30) {

## Bloques Leave-3-Out

AgnosLO <- list(
  1979:1981, 1982:1984, 1985:1987, 1988:1990, 1991:1993, 1994:1996, 1997:1999,
  2000:2002, 2003:2005, 2006:2008, 2009:2011, 2012:2014, 2015:2017, 2018:2020
)
Agnos <- 1979:2020

## Carga series escala-cuenca, estratificadas por meses
## CR2MET es la referencia, ERA5 son las series a corregir
## Periodo historico coincidente 1979-01-01/2020-04-30 y periodo a corregir 2023-02-01/presente

load("data_input/preproceso_meteo/input_preproceso_meteo/series-escala-cuenca_fondef_1979-2020.RData")
load("data_input/preproceso_meteo/output_preproceso_meteo/ti-tf.RData")
load("data_input/preproceso_meteo/output_preproceso_meteo/series-escala-cuenca_fondef_2023-presente.RData")

era5pr_val <- lapply(era5pr_val, function(df) filter(df, date >= t_i & date <= t_f)) %>% setNames(names(era5pr_val))
era5tm_val <- lapply(era5tm_val, function(df) filter(df, date >= t_i & date <= t_f)) %>% setNames(names(era5tm_val))
rm(t_i, t_f)

cuencas_att <- read.table("data_input/preproceso_meteo/input_preproceso_meteo/Atributos_Cuencas_Fondef.csv", sep = ",", header = T)
Cuencas <- cuencas_att$gauge_id %>% as.character()

## Primero estandariza variables (transformada Yeo-Johnson)
## Luego busca dias similares

for (Cuenca in Cuencas) {
  Locs <- Cuenca
  for (Mes in Meses) {
    print(paste("buscando dias similares para", Cuenca, "del mes", Mes))
    for (AgnoLO in AgnosLO[1]) { # L3O (ahora es redundante)
      prcal.sc <- vector("list", length(Locs)) %>% setNames(Locs)
      prval.sc <- vector("list", length(Locs)) %>% setNames(Locs)
      tmcal.sc <- vector("list", length(Locs)) %>% setNames(Locs)
      tmval.sc <- vector("list", length(Locs)) %>% setNames(Locs)
      for (Loc in Locs) {
        ## TRANSFORMA PRECIP
        ref <- cr2pr[[Mes]] %>%
          melt(id = "date") %>%
          filter(variable %in% Loc & format(date, "%Y") %in% as.character(Agnos[!Agnos %in% AgnoLO])) %$% value %>%
          sqrt()
        cal <- era5pr[[Mes]] %>%
          melt(id = "date") %>%
          filter(variable %in% Loc & format(date, "%Y") %in% as.character(Agnos[!Agnos %in% AgnoLO])) %$% value %>%
          sqrt()
        val <- era5pr_val[[Mes]] %>%
          melt(id = "date") %>%
          filter(variable %in% Loc) %$% value %>%
          sqrt()
        ref.yj <- yeojohnson(ref[ref != 0], standardize = T)
        prcal.sc[[Loc]] <- predict(ref.yj, cal)
        prval.sc[[Loc]] <- predict(ref.yj, val)
        rm(ref, ref.yj, cal, val)
        ## TRANSFORMA TEMP
        ref <- cr2tm[[Mes]] %>%
          melt(id = "date") %>%
          filter(variable %in% Loc & format(date, "%Y") %in% as.character(Agnos[!Agnos %in% AgnoLO])) %$% value
        cal <- era5tm[[Mes]] %>%
          melt(id = "date") %>%
          filter(variable %in% Loc & format(date, "%Y") %in% as.character(Agnos[!Agnos %in% AgnoLO])) %$% value
        val <- era5tm_val[[Mes]] %>%
          melt(id = "date") %>%
          filter(variable %in% Loc) %$% value
        ref.yj <- yeojohnson(ref, standardize = T)
        tmcal.sc[[Loc]] <- predict(ref.yj, cal)
        tmval.sc[[Loc]] <- predict(ref.yj, val)
        rm(ref, ref.yj, cal, val)
      }
      
      ## EXTRAE DIAS
      t.cal <- cr2pr[[Mes]] %>%
        dplyr::select(date) %>%
        filter(format(date, "%Y") %in% as.character(Agnos[!Agnos %in% AgnoLO])) %$% date
      t.val <- era5pr_val[[Mes]] %>% dplyr::select(date) %$% date
      ## BUSQUEDA DE DIAS ANALOGOS
      df_cal <- rbind(
        lapply(Locs, function(Loc) {
          data.table(loc = Loc, variable = "pr", date = t.cal, value = prcal.sc[[Loc]])
        }) %>% do.call(rbind, .),
        lapply(Locs, function(Loc) {
          data.table(loc = Loc, variable = "tm", date = t.cal, value = tmcal.sc[[Loc]])
        }) %>% do.call(rbind, .)
      )
      df_val <- rbind(
        lapply(Locs, function(Loc) {
          data.table(loc = Loc, variable = "pr", date = t.val, value = prval.sc[[Loc]])
        }) %>% do.call(rbind, .),
        lapply(Locs, function(Loc) {
          data.table(loc = Loc, variable = "tm", date = t.val, value = tmval.sc[[Loc]])
        }) %>% do.call(rbind, .)
      )
      dias_similares <- DiaSim2(df_cal, df_val, N = N_miembros_meteorologia) # %>%  # N=30 por analisis exploratorios (*)
      
      saveRDS(dias_similares, paste0("data_input/preproceso_meteo/output_preproceso_meteo/dias-similares/m", Mes, "_2023-presente_", Cuenca, ".RData"))
      rm(df_cal, df_val, t.cal, t.val)
    }
  }
}
rm(Cuenca, Mes, AgnoLO, Locs, Loc)
}

dias_similares_operativo(N = N_miembros_meteorologia)

# 
# sim.cal = df_cal
# sim.proy = df_val
# N = 30
# 
# dias_similares1 <- DiaSim(df_cal,df_val, N = N_miembros_meteorologia)
# dias_similares2 <- DiaSim2(df_cal, df_val, N = N_miembros_meteorologia)
# all.equal(dias_similares1,dias_similares2,tolerance = 1e-100)

