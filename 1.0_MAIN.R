# clear variables and environment
rm(list = ls())
gc()

# set current directory
directory = "~/Desktop/Pronostico_estacional-20220105T171402Z-001/Pronostico_estacional/"
setwd(directory)

#Load libraries
load_silently <- function(lib_name) {suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))}
sapply(c("feather","ggplot2","dplyr","data.table","hydroGOF","pbapply", 'lubridate'),load_silently)

# constants
meses_wy          <- c("abr","may","jun","jul","ago","sep","oct","nov","dic","ene","feb","mar")

### set variables

set.seed(2) # SET SEED TO REPLICABILITY
cuencas_target    <- c("Achibueno","Ancoa","Melado","Maule","Lontue","Longavi") # CATCHMENTS
starting_wy       <- 1988 # START DATA
ending_wy         <- 2021 # END DATA
## MODELLING
wy_target         <- 2021 # SET FORECAST YEAR
mes_inicio_str    <- 'ene'
tipos_predictando <- "predictando_dinamico" # dinamico: de sep a mar el vol es mes-marzo # estatico: vol es fijo sep-mar
predictores_cols  <- list(c("SWE","PROD","ROUT"), c("pr_acum","tem_mean_3mons")) 
## KNN
n_kNN             <- 6
# AUXILIAR VARIABLES
wyms_inicio       <- which(mes_inicio_str == meses_wy)-1 # SET INITIAL MONTH


