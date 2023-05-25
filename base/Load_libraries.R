cargar_librerias <- function(packages = "default") {
  if (length(packages) == 1L && packages == "default") {
    packages <- c("data.table", "dplyr", "ggplot2",
     "lubridate", "glue", "tibble",
     "ecmwfr", "magrittr", "ncdf4",
      "sf", "terra", "exactextractr",
      "TUWmodel","airGR",
      "reshape2","bestNormalize","ECBC","MBC",
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

  for (package in packages) {
    suppressPackageStartupMessages(
      library(package, character.only = TRUE, quietly = TRUE)
    )
  }
}

cargar_librerias(packages = "default")
