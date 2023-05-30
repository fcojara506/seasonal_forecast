# ----------------------------------------------------------------------------
# Nombre del Proyecto: Pronóstico híbridos del volumen estacional en Chile Central 
# Autor(es): Diego Hernandez
# Fecha de Finalización: 2023/04/31
# Contacto: pronostico.caudales@gmail.com
# GitHub: https://github.com/hndiego
# ----------------------------------------------------------------------------

library(data.table)
library(dplyr)
library(reshape2)
library(ECBC) # version 1.1 (Schaake Shuffle)
library(MBC) # version 0.10-5 (Quantile Delta Mapping)
library(magrittr)


####

## Parametros preproceso

N    <- 30   # MIEMBROS DE ENSEMBLE: POR ANALISIS EXPLORATORIOS N=30 (*) (N < N_analogos)
umbr <- 0.1  # umbral ceros precipitacion (mm)

# (*) Segun analisis exploratorios de ajuste de precipitacion:
# N mayor (40-50) es mejor en estacion humeda y cuencas humedas,
# N menor (<10 o <20) es mejor en estacion seca y cuencas secas,
# se escoge N=30 como consenso para todo el dominio (explorar para cuencas especificas!)

####


## Crea funcion truncar a cero (util para precip)
tr_0 <- function(x, min){
  x[x < min] <- 0
  return(x)
}

## Crea funcion QDM para usar en precipitacion
## Por restriccion de Schaake Shuffle, todas las series son de igual largo
## (modificacion para atajar errores: agrega ruido/jitter)
pr_BC <- function(obscal, modcal, modval, umbral){
  obscal <- tr_0(jitter(obscal, 0.1 * umbral), 0)
  modcal <- tr_0(jitter(modcal, 0.1 * umbral), 0)
  modval <- tr_0(jitter(modval, 0.1 * umbral), 0)
  boolpr <- modval > 0
  if (sum(boolpr) < 2){  # con 0-1 dias con precip no se puede usar QDM...
    res           <- tr_0(modval, umbral)
    res[! boolpr] <- 0
    return(res)
  } else{
    res.bc        <- QDM(obscal[boolpr], modcal[boolpr], modval[boolpr], ratio = T, trace = umbral)$mhat.p
    res           <- modval
    res[! boolpr] <- 0
    res[boolpr]   <- res.bc
    return(res)
  }
}

correccion_sesgo_meteorologico <- function(N = 30,umbr = 0.1) {
  
## Cuencas

cuencas_att <- read.table("data_input/preproceso_meteo/input_preproceso_meteo/Atributos_Cuencas_Fondef.csv", sep = ",", header = T)
Cuencas     <- cuencas_att$gauge_id %>% as.character

## Bloques Leave-3-Out

AgnosLO <- list(1979:1981, 1982:1984, 1985:1987, 1988:1990, 1991:1993, 1994:1996, 1997:1999,
                2000:2002, 2003:2005, 2006:2008, 2009:2011, 2012:2014, 2015:2017, 2018:2020)
Agnos   <- 1979:2020

## Carga series escala-cuenca
## CR2MET es la referencia, ERA5 son las series a corregir
## Periodo historico coincidente 1979-01-01/2020-04-30 y periodo a corregir 2023-02-01/presente 

load("data_input/preproceso_meteo/input_preproceso_meteo/series-escala-cuenca_fondef_1979-2020.RData")
load("data_input/preproceso_meteo/output_preproceso_meteo/ti-tf.RData")
load("data_input/preproceso_meteo/output_preproceso_meteo/series-escala-cuenca_fondef_2023-presente.RData")

era5pr_val <- lapply(era5pr_val, function(df) filter(df, date >= t_i & date <= t_f)) %>% setNames(names(era5pr_val))
era5tm_val <- lapply(era5tm_val, function(df) filter(df, date >= t_i & date <= t_f)) %>% setNames(names(era5tm_val))
rm(t_i, t_f)

## Metodo "SimSchaake" (by Roman Schefzik) modificado

## Correccion/ajuste de sesgo
## Estratificada por mes
## Itera para cada periodo de validacion

## genera series diarias de largo del periodo de verificacion
## se generan N series (ensemble)
## la primera iteracion son los dias mas similares
## la segunda iteracion son los segundos dias mas similares
## ... hasta N

## Precipitacion: modelo quantile delta mapping (QDM) + schaake shuffle (SS)
## Temperatura: modelo quantile delta mapping (QDM) + schaake shuffle (SS)
## reg logistica para precip se descarto...
## tambien se descarto SS en aplicacion inter-diaria... se aplica intra-diaria

for (Cuenca in Cuencas){
  Locs <- Cuenca
    for (Mes in Meses){
      print(paste("corrigiendo sesgo meteorologico para", Cuenca, "del mes", Mes))
      
      for(AgnoLO in AgnosLO[1]){  # L3O (ahora es redundante)
        # DIAS A CORREGIR
        dias_similares = paste0("data_input/preproceso_meteo/output_preproceso_meteo/dias-similares/m", Mes, "_2023-presente_", Cuenca, ".RData") %>% readRDS
        DiasVal <- dias_similares %>% names
        
        DiasSim <- lapply(1:N, function(i_ens) 
          sapply(DiasVal, function(DiaVal){ # dias similares
         dias_similares[[DiaVal]]$date[i_ens]}) %>%
            as.Date(origin = "1970-01-01") %>% 
            setNames(DiasVal)) %>%
          setNames(1:N)
        
        DiasVal <- as.Date(DiasVal)
        ## QDM PRECIP
        pr_mod_cal <- lapply(1:N, function(i_ens) lapply(Locs, function(Loc)
          era5pr[[Mes]] %>% melt(id = "date") %>% filter(variable %in% Loc) %>% dplyr::select(! variable) %>%
            setNames(c("time", "value")) %>% right_join(data.table(time = DiasSim[[i_ens]]), by = "time") %>%
            arrange(order(DiasSim[[i_ens]])) %$% value) %>% setNames(Locs)) %>% setNames(1:N)
        
        pr_obs_cal <- lapply(1:N, function(i_ens) lapply(Locs, function(Loc)
          cr2pr[[Mes]] %>% melt(id = "date") %>% filter(variable %in% Loc) %>% dplyr::select(! variable) %>%
            setNames(c("time", "value")) %>% right_join(data.table(time = DiasSim[[i_ens]]), by = "time") %>%
            arrange(order(DiasSim[[i_ens]])) %$% value) %>% setNames(Locs)) %>% setNames(1:N)
        
        pr_mod_val <- lapply(Locs, function(Loc)
          era5pr_val[[Mes]] %>% melt(id = "date") %>% filter(variable %in% Loc) %>% dplyr::select(! variable) %>%
            setNames(c("time", "value")) %>% filter(time %in% DiasVal) %$% value) %>% setNames(Locs)
        
        pr_QDM     <- lapply(1:N, function(i_ens) lapply(Locs, function(Loc)
          pr_BC(pr_obs_cal[[i_ens]][[Loc]], pr_mod_cal[[i_ens]][[Loc]], pr_mod_val[[Loc]], umbr)) %>%
            setNames(Locs)) %>% setNames(1:N)
        rm(pr_mod_cal, pr_mod_val)
        ## SCHAAKE SHUFFLE PRECIP
        lapply(Locs, function(Loc) lapply(seq_along(DiasVal), function(iDia)
          sapply(1:N, function(i_ens) pr_obs_cal[[i_ens]][[Loc]][iDia]) %>%
            Schaake.Shuffle(sapply(1:N, function(i_ens) pr_QDM[[i_ens]][[Loc]][iDia])) %>%
            data.table(date = as.character(DiasVal[iDia]), value = ., i_ens = 1:N)) %>% do.call(rbind, .) %>%
            write.table(paste0("data_input/preproceso_meteo/output_preproceso_meteo/bias-adj-ensemble/pr/pr_m", Mes, "_2023-presente ", Loc, ".csv"), sep = ",", row.names = F))
        rm(pr_obs_cal, pr_QDM)
        ## QDM TEMP
        tm_mod_cal <- lapply(1:N, function(i_ens) lapply(Locs, function(Loc)
          era5tm[[Mes]] %>% melt(id = "date") %>% filter(variable %in% Loc) %>% dplyr::select(! variable) %>%
            setNames(c("time", "value")) %>% right_join(data.table(time = DiasSim[[i_ens]]), by = "time") %>%
            arrange(order(DiasSim[[i_ens]])) %$% value) %>% setNames(Locs)) %>% setNames(1:N)
        tm_obs_cal <- lapply(1:N, function(i_ens) lapply(Locs, function(Loc)
          cr2tm[[Mes]] %>% melt(id = "date") %>% filter(variable %in% Loc) %>% dplyr::select(! variable) %>%
            setNames(c("time", "value")) %>% right_join(data.table(time = DiasSim[[i_ens]]), by = "time") %>%
            arrange(order(DiasSim[[i_ens]])) %$% value) %>% setNames(Locs)) %>% setNames(1:N)
        tm_mod_val <- lapply(Locs, function(Loc)
          era5tm_val[[Mes]] %>% melt(id = "date") %>% filter(variable %in% Loc) %>% dplyr::select(! variable) %>%
            setNames(c("time", "value")) %>% filter(time %in% DiasVal) %$% value) %>% setNames(Locs)
        tm_QDM     <- lapply(1:N, function(i_ens) lapply(Locs, function(Loc)
          QDM(tm_obs_cal[[i_ens]][[Loc]], tm_mod_cal[[i_ens]][[Loc]], tm_mod_val[[Loc]])$mhat.p) %>%
            setNames(Locs)) %>% setNames(1:N)
        rm(tm_mod_cal, tm_mod_val)
        ## SCHAAKE SHUFFLE TEMP
        lapply(Locs, function(Loc) lapply(seq_along(DiasVal), function(iDia)
          sapply(1:N, function(i_ens) tm_obs_cal[[i_ens]][[Loc]][iDia]) %>%
            Schaake.Shuffle(sapply(1:N, function(i_ens) tm_QDM[[i_ens]][[Loc]][iDia])) %>%
            data.table(date = as.character(DiasVal[iDia]), value = ., i_ens = 1:N)) %>% do.call(rbind, .) %>%
            write.table(paste0("data_input/preproceso_meteo/output_preproceso_meteo/bias-adj-ensemble/tm/tm_m", Mes, "_2023-presente ", Loc, ".csv"), sep = ",", row.names = F))
        rm(tm_obs_cal, tm_QDM)
      }
    } 
}
rm(Cuenca, Mes, AgnoLO, Locs, DiasVal, DiasSim)

## GUARDA SERIES ERA5-BC ENSEMBLE PROMEDIO

## pr
pr_prom  <- lapply(Meses, function(Mes)
  lapply(Cuencas, function(Loc) paste0("data_input/preproceso_meteo/output_preproceso_meteo/bias-adj-ensemble/pr/pr_m", Mes, "_2023-presente ", Loc, ".csv") %>% read.table(sep = ",", header = T) %>%
           filter(i_ens %in% seq_len(N)) %>% cbind(., variable = Loc)) %>% do.call(rbind, .)) %>% do.call(rbind, .) %>%
  group_by(variable, date) %>% summarise(value = mean(value)) %>% dcast(date ~ variable) %>% mutate(date = as.Date(date)) %>%
  group_by(date) %>% arrange(.by_group = T) %>% mutate(date = as.character(date)) %>% ungroup
paste0("data_input/preproceso_meteo/output_preproceso_meteo/precip_ERA5-BC-ensemble-prom_N", N, "_2023-presente.csv") %>% write.table(pr_prom, file = ., row.names = F, sep = ",")

## tm
tm_prom  <- lapply(Meses, function(Mes)
  lapply(Cuencas, function(Loc) paste0("data_input/preproceso_meteo/output_preproceso_meteo/bias-adj-ensemble/tm/tm_m", Mes, "_2023-presente ", Loc, ".csv") %>% read.table(sep = ",", header = T) %>%
           filter(i_ens %in% seq_len(N)) %>% cbind(., variable = Loc)) %>% do.call(rbind, .)) %>% do.call(rbind, .) %>%
  group_by(variable, date) %>% summarise(value = mean(value)) %>% dcast(date ~ variable) %>% mutate(date = as.Date(date)) %>%
  group_by(date) %>% arrange(.by_group = T) %>% mutate(date = as.character(date)) %>% ungroup
paste0("data_input/preproceso_meteo/output_preproceso_meteo/temp_ERA5-BC-ensemble-prom_N", N, "_2023-presente.csv") %>% write.table(tm_prom, file = ., row.names = F, sep = ",")

return(message("Sesgo meteorologico completado"))
}

