# FREE ENVIRONMENT FILES
rm(list = ls())
gc()

# LOAD LIBRARIES 
load_silently <- function(lib_name) {suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))}
sapply(c("feather","ggplot2","dplyr","data.table","plotly"),load_silently)

wym_inverse <- function(wym) {ifelse(wym<10,wym+3,wym-9)}
wy_inverse <- function(wy,wym) {ifelse(wym<10,wy,wy+1)}
########## SET CURRENT DIRECTORY ###########################
setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")
##########FUNCTIONS#####################
wy_months_labels            <- c("abr","may","jun","jul","ago","sep","oct","nov","dic","ene","feb","mar")

input_data=feather::read_feather("data_output/list_scores_seasonal_volume.feather") %>% 
  transform(wym_label=as.Date(paste0(wy_inverse(2019,wym+1),"-",wym_inverse(wym+1),"-01"))) %>%  
  subset(regression_type=="lineal")

p1=ggplot(data=input_data, aes(x=wym_label,y=crpss,col=version))+
  geom_point() +
  geom_line() +
  geom_abline(slope=0,intercept = 0)+
  facet_wrap(cuenca~.)+
  labs(title = "CRPSS para distintas fechas de emisión del pronóstico",
       subtitle = "(respecto a pronóstico con volumen promedio 1988-2020)",
       y="",
       #y="Continuous Ranked Probability Score-CRPSS",
       x="Fecha de emisión del pronóstico",
       col="Predictores")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%d\n%b")+
  scale_y_continuous(breaks = round(seq(-0.6, 0.8, by = 0.2),1),limits = c(-0.6,0.8))+
  scale_color_manual(values = c("springgreen3", "red","grey30", "grey"))+
  #scale_color_brewer()+
  theme(legend.position = "bottom")
  

plot(p1) 
#p2=ggplotly(p1)
#stop()
ggsave("Figuras/Evolucion_CRPSS.png",width = 8,height = 5,units = "in",dpi=400,plot = p1)
#htmlwidgets::saveWidget(p2, "Figuras/test_lineal.html", selfcontained=FALSE)



ggplot(data=input_data,aes(x=wym_label,y=R2,col=version))+
  geom_point() +
  geom_line() +
  geom_abline(slope=0,intercept = 0)+
  facet_wrap(cuenca~.)+
  labs(title = "Coeficiente de determinación (R2) del pronóstico determinístico",
       subtitle="(respecto al volumen observado 1988-2020)",
       #y="R2",
       y="",
       x="Fecha de emisión del pronóstico",
       col="")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%d\n%b")+
  #scale_x_date(date_breaks = "1 month", date_labels =  "%d\n%b")+
  scale_y_continuous(breaks = round(seq(-0.6, 1, by = 0.2),1),limits = c(-0.6,1))+
  scale_color_manual(values = c("springgreen3", "red","grey30", "grey"))+
  theme(legend.position = "bottom")

ggsave("Figuras/Evolucion_R2.png",width = 8,height = 5,units = "in",dpi=400)


# ggplot(data=input_data,aes(x=wym_label,y=rmse,shape=regression_type,col=version))+
#   geom_point() +
#   geom_line() +
#   geom_abline(slope=0,intercept = 0)+
#   facet_wrap(cuenca~.,scales="free_y")+
#   labs(title = "Evolución del RMSE con respecto a Volumen promedio",
#        y="RMSE",
#        x="Fecha de emisión del pronóstico",
#        col="")+
#   scale_x_date(date_breaks = "1 month", date_labels =  "%d\n%b")+
#   #scale_y_continuous(breaks = round(seq(-0.6, 1, by = 0.2),1),limits = c(-0.6,1))+
#   scale_color_manual(values = c("springgreen3", "red","grey30", "grey"))+
#   theme(legend.position = "bottom")
# 
# ggsave("Figuras/Evolucion_RMSE.png",width = 8,height = 5,units = "in",dpi=400)

