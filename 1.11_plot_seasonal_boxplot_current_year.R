# FREE ENVIRONMENT FILES
rm(list = ls())
gc()

# LOAD LIBRARIES 
load_silently <- function(lib_name) {suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))}
sapply(c("feather","ggplot2","dplyr","data.table"),load_silently)

########## SET CURRENT DIRECTORY ###########################
setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")
##########FUNCTIONS#####################
#wy_target=2019
########## LOAD PREDICTED VOLUME ##################
wym_inverse <- function(wym) {ifelse(wym<10,wym+3,wym-9)}
wy_inverse <- function(wy,wym) {ifelse(wym<10,wy,wy+1)}

vol_obs                 <- read_feather("data_input/MODELO_VOLUMENES_ESTACIONALES.feather") %>% 
  subset(GRP=="[6-12]") %>% 
  rename(vol_obs=volumen)

modelo_v2               <-
  read_feather("data_output/pronostico_vol_estacional_v2.feather") %>% 
  subset(regression_type=="lineal") %>% 
  #subset(version %in% "pr_sum + SP + PROD + ROUT") %>%
  transform(wym_label= as.Date(paste0(wy_inverse(2019,wym+1),"-",wym_inverse(wym+1),"-01")))
  


plot_vol <- function(wy_target) {
  
  a=modelo_v2 %>% subset(wy == wy_target)
  b=vol_obs %>% subset(wy == wy_target)
  data=merge(a, b)
  if (wy_target==2021) {
    data=subset(data,wym<6)
  }
  
ggplot(data)+
  geom_boxplot(aes(x=wym_label,y=volumen,group=c(wym_label)),
               draw_quantiles = c(0.25, 0.5, 0.75),
               outlier.shape = NA)+
  geom_hline(aes(yintercept=vol_obs),col="red")+
  facet_wrap(cuenca~.,scales="free")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%d\n%b")+
  #scale_x_discrete(labels=list("1 may","1 jun","1 jul","1 ago","1 sep"))+
  labs(x="Fecha de emisión del pronóstico",
       y= "Volumen estacional (mill. m3)",
       title= paste0("Evolución del pronóstico sep-mar a lo largo del año ",wy_target))+
  ylim(0,NA)

ggsave(paste0("Figuras/evolucion_volumen_",wy_target,".png"),width = 10,height = 5,units = "in",dpi=400)
}

for (wy_target in c(2017,2021)) {
  plot_vol(wy_target)
}
