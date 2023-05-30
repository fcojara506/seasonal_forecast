# ----------------------------------------------------------------------------
# Nombre del Proyecto: Pronóstico híbridos del volumen estacional en Chile Central 
# Autor(es): Francisco Jara
# Fecha de Finalización: 2023/04/31
# Contacto: pronostico.caudales@gmail.com
# GitHub: https://github.com/fcojara506
# ----------------------------------------------------------------------------

rm(list = ls())

library(data.table)
library(dplyr)

index_folder = "data_input/climate_index_variables/"

wym_simple        <- function(month){fifelse(month>3, month-3,month+9)}
wy_simple         <- function(month,year){fifelse(month>3, year,year - 1)}


read_indices_files <- function(download_index_files = T, year_start= 1979) {
 
  if (download_index_files) {
    
    urls =
      list(
        "https://www.cpc.ncep.noaa.gov/data/indices/ersst5.nino.mth.91-20.ascii",
        #"https://psl.noaa.gov/enso/mei/data/meiv2.data",
        #"https://psl.noaa.gov/pdo/data/pdo.timeseries.ersstv5.csv",
        "https://psl.noaa.gov/data/correlation/soi.data"
        #"https://psl.noaa.gov/data/correlation/censo.data",
        #"https://psl.noaa.gov/data/correlation/oni.data",
        #"https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/aao/monthly.aao.index.b79.current.ascii",
        #"http://eagle1.umd.edu/GPCP_ICDR/Data/ESPI.txt",
        #"https://www.cpc.ncep.noaa.gov/data/indices/olr"
      )
    
    names = c("ssts",
              "soi"
              )
    
    sapply(seq_along(urls), function(i) 
      download.file(url = urls[[i]],
                    destfile = paste0(index_folder,names[i]),
                    method = 'curl')
      
    )
  }
  
  
  colnames = c("year", seq(1:12))
  
  ### temperatura NIÑOS
  ssts = read.csv(paste0(index_folder,"ssts"), sep = "") %>%
    rename(month = MON, year = YR) %>%
    mutate(
      NINO1.2 = ANOM,
      NINO3 = ANOM.1,
      NINO4 = ANOM.2,
      NINO3.4 = ANOM.3
    ) %>%
    mutate(
      ANOM = NULL,
      ANOM.1 = NULL,
      ANOM.2 = NULL,
      ANOM.3 = NULL
    ) %>%
    mutate_all(as.numeric) %>%
    data.table(key = c("year", "month")) %>% 
    select(year,month,NINO1.2)

  # Southern Oscillation index
  soi = read.csv(paste0(index_folder,"soi"), skip = 4, sep = "") %>%
    head(-3) %>%
    as.matrix.data.frame() %>%
    `colnames<-`(colnames) %>%
    data.table %>%
    reshape2::melt(id.vars = "year") %>%
    rename(month = variable, SOI = value) %>%
    mutate_all(as.numeric) %>%
    data.table(key = c("year", "month")) %>% 
    select(year,month,SOI)
  
  # merge data and clean na
  result = merge.data.frame(ssts,soi) %>%
    subset(year>year_start) %>% 
    dplyr::mutate_all(~dplyr::na_if(., -99.99)) %>% 
    dplyr::mutate_all(~dplyr::na_if(., -9.9)) %>% 
    dplyr::mutate_all(~dplyr::na_if(., -99.9))  %>%
    dplyr::mutate_all(~dplyr::na_if(., -9.99))  %>%
    dplyr::mutate_all(~dplyr::na_if(., -999)) %>%
    dplyr::mutate_all(~dplyr::na_if(., 99.99)) %>% 
    tidyr::drop_na() %>% 
    mutate(
      wy_simple = wy_simple(month = month, year = year),
      wym = wym_simple(month = month)
    ) %>% 
    select(-month,-year)
  result = select(result, c('wy_simple','wym',everything())) 
  
  write.csv(x = result, file = paste0(index_folder, "indices_mensuales_1979_present.csv"),  row.names = F)
  
  return(message("Indices climaticos descargados con exito"))
}

### run download
read_indices_files(download_index_files = T)





