# ----------------------------------------------------------------------------
# Nombre del Proyecto: Pronóstico híbridos del volumen estacional en Chile Central 
# Autor(es): Francisco Jara
# Fecha de Finalización: 2023/04/31
# Contacto: pronostico.caudales@gmail.com
# GitHub: https://github.com/fcojara506
# ----------------------------------------------------------------------------

install_from_zip <- function(package_name, path_to_zip) {
  packagecheck <- match(package_name, utils::installed.packages()[, 1])
  packagestoinstall <- package_name[is.na(packagecheck)]
  
  if (length(packagestoinstall) > 0L) {
    
    devtools::install_local(path_to_zip)
    
  }else{
    message(paste(package_name, "package already installed"))
  }
  
}

cargar_librerias <- function(packages = "default",repo = "https://cran.dcc.uchile.cl") {
  
  if (length(packages) == 1L && packages == "default") {
    packages <- c("devtools", "data.table", "dplyr", "ggplot2",
     "lubridate", "glue", "tibble",
     "ecmwfr", "magrittr", "ncdf4",
      "sf", "terra", "exactextractr",
      "TUWmodel","airGR",
      "reshape2","bestNormalize","MBC",
      "hydromad","rlist","icesTAF","caret","verification",
      "stringr","gridExtra"#,"zoo", "latticeExtra", "polynom", "car", "Hmisc","reshape"
     )
  }

  packagecheck <- match(packages, utils::installed.packages()[, 1])

  packagestoinstall <- packages[is.na(packagecheck)]

  if (length(packagestoinstall) > 0L) {
    
    utils::install.packages(packagestoinstall, repos = repo)
    
  } else {
    message("All requested packages already installed")
  }
  
}

cargar_librerias(packages = "default")

install_from_zip(package_name = "ECBC",
                 path_to_zip = "data_input/preproceso_meteo/input_preproceso_meteo/ECBC.zip")


#cargar_librerias(package_name = "hydromad",repo = "http://hydromad.catchment.org")

