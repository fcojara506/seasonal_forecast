# FREE ENVIRONMENT FILES
rm(list = ls())
gc()

# LOAD LIBRARIES 
load_silently <- function(lib_name) {suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))}
sapply(c("feather","ggplot2","dplyr","data.table"),load_silently)

########## SET CURRENT DIRECTORY ###########################
setwd("~/GoogleDrive/CORFO_Maule_FJ/Pronostico_estacional")
##########FUNCTIONS#####################


data                     <- read_feather("data_input/MODELO_VOLUMENES_ESTACIONALES.feather")%>% 
  subset(GRP    == "[6-12]" ) %>% 
  merge.data.frame(read_feather(paste0("data_output/predictores_v2.feather")),
                   by.x=c("wy","cuenca"),
                   by.y = c("wy","cuenca")) %>% 
  reshape2::melt(id.vars=c("wy","wym","cuenca","GRP","volumen"))



  
data1=data %>%
  subset(wym==5) 

data2=data1 %>%
  subset(wy==2021) %>% 
  transform(tipo="Año H. 2021") %>%
  select(cuenca,variable,value,tipo) %>%
  as.matrix()

data3=subset(data1,wy!=2021) %>%
  group_by(wym,cuenca,variable) %>%
  summarize(value=mean(value)) %>%
  transform(tipo="Promedio")%>%
  select(cuenca,variable,value,tipo)%>%
  as.matrix()

data4=rbind(data2,data3)%>%
  as.data.frame()

ggplot()+
  geom_point(data= subset(data1,wy!=2021),aes(x=value,y=volumen))+
  geom_vline(data = data4,aes(xintercept=as.numeric(value),col=tipo))+
  facet_wrap(cuenca ~ variable, scales="free",labeller = label_wrap_gen(multi_line=FALSE))+
  labs(title="Predictores vs Volumen Estacional",
       col="",
       #subtitle=cuenca_target,
       x="",
       y= "volumen sep-mar (mill m3)")+
  theme(legend.position = "bottom")

ggsave(
  paste0("Figuras/scatter_predictores_vs_volumen.png"),
  width = 9,height = 9,units = "in",dpi=400)



