rm(list = ls())


index_folder = "data_input/climate_index_variables/"
setwd(index_folder)

wym_simple        <- function(month){fifelse(month>3, month-3,month+9)}
wy_simple         <- function(month,year){fifelse(month>3, year,year - 1)}



read_indices_files <- function(download_index_files = T) {
  library(data.table)
  library(dplyr)
  
  
  
  if (download_index_files) {
    
    urls =
      list(
        "https://www.cpc.ncep.noaa.gov/data/indices/ersst5.nino.mth.91-20.ascii",
        "https://psl.noaa.gov/enso/mei/data/meiv2.data",
        #"https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",
        "https://psl.noaa.gov/pdo/data/pdo.timeseries.ersstv5.csv",
        "https://psl.noaa.gov/data/correlation/soi.data",
        "https://psl.noaa.gov/data/correlation/censo.data",
        "https://psl.noaa.gov/data/correlation/oni.data",
        "https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/aao/monthly.aao.index.b79.current.ascii",
        "http://eagle1.umd.edu/GPCP_ICDR/Data/ESPI.txt",
        "https://www.cpc.ncep.noaa.gov/data/indices/olr"
      )
    
    names = c("ssts",
              "meiv2",
              "pdo",
              "soi",
              "censo",
              "oni",
              "aao",
              "espi",
              "olr")
    
    sapply(seq_along(urls), function(i) 
      download.file(url = urls[[i]],
                    destfile =  names[i],
                    method = 'curl')
      
    )
  }
  
  
  colnames = c("year", seq(1:12))
  
  
  a = read.table("aao",col.names = c("year","month","AAO")) %>%
    data.table(key = c("year", "month"))
  
  b = read.csv("censo", skip = 1, sep = "",header = T) %>%
    head(-2) %>% 
    as.matrix.data.frame() %>%
    `colnames<-`(colnames) %>%
    data.table %>%
    reshape2::melt(id.vars = "year",variable.name = "month",value.name = "BIENSO") %>% 
    mutate_all(as.numeric) %>%
    data.table(key = c("year", "month"))
  
  c = read.csv("espi", sep = "") %>% 
    rename(year = YYYY, month = MM) %>%
    mutate(EI = NULL, LI = NULL) %>%
    data.table(key = c("year", "month"))
  
  d = read.csv("ssts", sep = "") %>%
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
    data.table(key = c("year", "month"))
  
  e = read.csv(file = "olr",skip=111,sep = "",na.strings = "-999.9") %>% 
    as.matrix.data.frame() %>% 
    data.table %>% 
    `colnames<-`(colnames) %>%
    reshape2::melt(id.vars = "year") %>%
    rename(month = variable, OLR = value) %>%
    mutate_all(as.numeric) %>%
    data.table(key = c("year", "month"))
  
  
  f = read.csv("oni", skip = 1, sep = "") %>% head(-8) %>%
    as.matrix.data.frame() %>%
    `colnames<-`(colnames) %>%
    data.table %>%
    reshape2::melt(id.vars = "year") %>%
    rename(month = variable, ONI = value) %>%
    mutate_all(as.numeric) %>%
    data.table(key = c("year", "month"))
  
  g = read.csv("soi", skip = 4, sep = "") %>% head(-3) %>%
    as.matrix.data.frame() %>%
    `colnames<-`(colnames) %>%
    data.table %>%
    reshape2::melt(id.vars = "year") %>%
    rename(month = variable, SOI = value) %>%
    mutate_all(as.numeric) %>%
    data.table(key = c("year", "month"))
  
  h = read.csv("pdo",header = T,skip = 1, sep = ',') %>%
    `colnames<-`(c("date","PDO")) %>%
    data.table %>% 
    mutate(year = year(date)) %>% 
    mutate(month = month(date)) %>% 
    select(-date) %>% 
    mutate_all(as.numeric) %>%
    data.table(key = c("year", "month"))
  
  i = read.csv("meiv2", skip = 1, sep = "") %>% head(-5) %>%
    as.matrix.data.frame() %>%
    `colnames<-`(colnames) %>%
    data.table %>%
    reshape2::melt(id.vars = "year") %>%
    rename(month = variable, MEIv2 = value) %>%
    mutate_all(as.numeric) %>%
    data.table(key = c("year", "month"))
  
  result = a[b] %>%
    c[.] %>%
    d[.] %>%
    e[.] %>%
    f[.] %>%
    g[.] %>%
    h[.] %>%
    i[.] %>% 
    subset(year>1979) %>% 
    dplyr::na_if(-9.9)   %>%
    dplyr::na_if(-99.99) %>%
    dplyr::na_if(-99.9)  %>%
    dplyr::na_if(-9.99)  %>%
    dplyr::na_if(-999) %>%
    dplyr::na_if(99.99) %>%
    tidyr::drop_na() %>% 
  mutate(
      wy_simple = wy_simple(month = month, year = year),
      wym = wym_simple(month = month),
      year = NULL,
      month = NULL
    )
    
  return(result)
}

monthly_indices = read_indices_files(download_index_files = F)

feather::write_feather(monthly_indices,"indices_mensuales_1988_2020.feather")


