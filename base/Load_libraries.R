install_from_zip <- function(package_name, path_to_zip) {
  packagecheck <- match(package_name, utils::installed.packages()[, 1])
  packagestoinstall <- packages[is.na(packagecheck)]
  
  if (length(packagestoinstall) > 0L) {
    
    devtools::install_local(path_to_zip)
    
  }else{
    message(paste(package_name, "package already installed"))
  }
  
}

cargar_librerias <- function(packages = "default") {
  
  if (length(packages) == 1L && packages == "default") {
    packages <- c("devtools", "data.table", "dplyr", "ggplot2",
     "lubridate", "glue", "tibble",
     "ecmwfr", "magrittr", "ncdf4",
      "sf", "terra", "exactextractr",
      "TUWmodel","airGR",
      "reshape2","bestNormalize","MBC",
      "hydromad","rlist","icesTAF","caret","verification",
      "stringr","gridExtra"
     )
  }

  packagecheck <- match(packages, utils::installed.packages()[, 1])

  packagestoinstall <- packages[is.na(packagecheck)]

  if (length(packagestoinstall) > 0L) {
    
    utils::install.packages(packagestoinstall, repos = "https://cran.dcc.uchile.cl")
    
  } else {
    message("All requested packages already installed")
  }
  
  
  # for (package in packages) {
  #   suppressPackageStartupMessages(
  #     library(package, character.only = TRUE, quietly = TRUE)
  #   )
  # }
}

cargar_librerias(packages = "default")

install_from_zip(package_name = "ECBC",
                 path_to_zip = "data_input/preproceso_meteo/input_preproceso_meteo/ECBC.zip")



