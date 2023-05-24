

feather2csv <- function(filename) {
  library(feather)
  library(stringr)
  library(utils)
  
  new_file = feather::read_feather(filename)
  new_file = select(new_file, c('wy_simple','wym',everything())) 
  
  new_filename = stringr::str_replace(filename, pattern = '.feather', '.csv')
  
  write.csv(x = new_file,
            file = new_filename ,
            row.names = F)
  
  return(TRUE)
}


'data_input/flows/flows_mm_monthly_49catchments_ChileCentral.feather' %>% feather2csv


filename = 'data_input/attributes/attributes_49catchments_ChileCentral.feather'
new_file = feather::read_feather(filename) [c('cod_cuenca','gauge_name','gauge_lat','gauge_lon','area_km2')]
new_filename = stringr::str_replace(filename,pattern = '.feather','.csv')
write.csv(x = new_file, file = new_filename ,row.names = F)
