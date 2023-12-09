# mapas_PP

####################################################

# MAPAS DE PRECIPITACION

####################################################

graphics.off() # Elimina configuracion de graficos previos.

setwd("/home/ezequiel.amor/Caso_100mm/Caso3") # Cambiar segun donde esten los archivos.

# Librerias a usar en este programa.

library(metR)
library(ggplot2)
library(maps)
library(mapdata)
library(ggrepel)
library(sf)
library(scales)

####################################################

# Leo los datos para cada dia de la semana en estudio en la region de Chaco.
# Si se queire usar otra region, modificar lat y lon.

pp1 <- ReadNetCDF("2022095.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp2 <- ReadNetCDF("2022096.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp3 <- ReadNetCDF("2022097.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp4 <- ReadNetCDF("2022098.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp5 <- ReadNetCDF("2022099.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp6 <- ReadNetCDF("2022100.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 
pp7 <- ReadNetCDF("2022101.nc", vars = "precipitation", 
                  subset = list(lat = c(-28.05,-25.95),lon = c(-59.55,-57.45))) 

# Calculo el acumulado de precipitacion durante esa semana para despues poder
# graficarlo. Luego lo guardo en un dataframe con sus correpondientes coordenadas.

suma_pp <- c()

for(i in 1:length(pp1$precipitation)) {
  suma_pp[i] <- pp1$precipitation[i] + pp2$precipitation[i] + pp3$precipitation[i] +
    pp4$precipitation[i] + pp5$precipitation[i] + pp6$precipitation[i] + 
    pp7$precipitation[i]
}

pp_total <- data.frame("lon"=pp1$lon,"lat"=pp1$lat,"precipitation"=suma_pp)

# Elementos para el mapa.
# Para los limites se debe cambiar el setwd segun donde esten los archivos con
# dichos datos. Una vez cargados, no hace falta volver a correr las lineas.
# Luego se guardan las coordenadas de las diferentes estaciones para luego 
# localizarlas en el grafico.

mapa_pais          <- read_sf("paises/linea_de_limite_FA004.shp")
mapa_provincias    <- read_sf("provincias/linea_de_limite_070111.shp")
mapa_departamentos <- read_sf("departamentos/linea_de_limite_070110.shp")

coord_Resistencia <- data.frame(longR=c(-59.04583),latR=c(-27.43861),
                                stringsAsFactors = F)
coord_Corrientes  <- data.frame(longC=c(-58.75974),latC=c(-27.44979),
                                stringsAsFactors = F)
coord_Formosa     <- data.frame(longF=c(-58.22929),latF=c(-26.21242),
                                stringsAsFactors = F)
coord_TresHROLON  <- data.frame(longT=c(-58.5863892276972),latT=c(-26.9696770400602),
                                stringsAsFactors = F)
coord_Moncholo    <- data.frame(longM=c(-58.682443),latM=c(-27.043854),
                                stringsAsFactors = F)
coord_GralVedia   <- data.frame(longG=c(-58.661198137723),latG=c(-26.930967580464),
                                stringsAsFactors = F)
coord_Lote16      <- data.frame(longL=c(-58.8067559429548),latL=c(-26.8659190692775),
                                stringsAsFactors = F)
coord_PtoBjo90    <- data.frame(longP=c(-58.5838380967161),latP=c(-26.8785437446458),
                               stringsAsFactors = F)

# Agrego el valor de precipitacion para poner luego en los circulos donde estan
# posicionadas las estaciones meteorologicas en el grafico.

coord_Resistencia$acumulado <- PP_acum$Acum_S2_Resistencia[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_Corrientes$acumulado  <- PP_acum$Acum_S2_Corrientes[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_Formosa$acumulado     <- PP_acum$Acum_S2_Formosa[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_TresHROLON$acumulado  <- PP_acum$Acum_S2_TresHoquetas2[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_Moncholo$acumulado    <- PP_acum$Acum_S2_Moncholo[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_GralVedia$acumulado   <- PP_acum$Acum_S2_GralVedia[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_Lote16$acumulado      <- PP_acum$Acum_S2_Lote16[PP_acum$S2_Desde==as.character(pp1$time[1])]
coord_PtoBjo90$acumulado    <- PP_acum$Acum_S2_PtoBermejokm90[PP_acum$S2_Desde==as.character(pp1$time[1])]

# Guardo las fechas del dia inicial y final de cada semana para tener un 
# formato del tipo DD/MM/AAAA. 

dia_inicio <- paste(substr(as.character(pp1$time[1]),9,10),"/",substr(as.character(pp1$time[1]),6,7),"/",
                    substr(as.character(pp1$time[1]),1,4),sep="")
dia_final  <- paste(substr(as.character(pp7$time[1]),9,10),"/",substr(as.character(pp7$time[1]),6,7),"/",
                    substr(as.character(pp7$time[1]),1,4),sep="")

# Realizo el gráfico.
# No haria falta modificar nada, salvo que se quiera cambiar la region de la 
# figura. En ese caso cambiar los limites en "coord_sf".

if(max(pp_total$precipitation)<=30) {
  ggplot(pp_total,aes(lon,lat)) +
    geom_contour_fill(aes(z=precipitation)) +
    geom_contour(aes(z=precipitation),color="grey90",size=0.15) + 
    geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
    geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
    geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
    coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
    geom_point(data=coord_Resistencia,aes(longR,latR,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Resistencia,aes(longR,latR,label=paste(acumulado,"mm")),
              color="black",size=2.28) + 
    geom_text(data=coord_Resistencia,aes(longR,latR,label="Est. 1"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.001,fontface="bold") +
    geom_point(data=coord_TresHROLON,aes(longT,latT,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_TresHROLON,aes(longT,latT,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_TresHROLON,aes(longT,latT,label="Est. 2"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.002,fontface="bold") +
    geom_point(data=coord_Moncholo,aes(longM,latM,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Moncholo,aes(longM,latM,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Moncholo,aes(longM,latM,label="Est. 3"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.004,fontface="bold") +
    geom_point(data=coord_GralVedia,aes(longG,latG,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_GralVedia,aes(longG,latG,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_GralVedia,aes(longG,latG,label="Est. 4"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.009,fontface="bold") +
    geom_point(data=coord_Lote16,aes(longL,latL,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Lote16,aes(longL,latL,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Lote16,aes(longL,latL,label="Est. 5"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.002,fontface="bold") +
    geom_point(data=coord_PtoBjo90,aes(longP,latP,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_PtoBjo90,aes(longP,latP,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_PtoBjo90,aes(longP,latP,label="Est. 6"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.002,fontface="bold") +
    geom_point(data=coord_Corrientes,aes(longC,latC,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Corrientes,aes(longC,latC,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Corrientes,aes(longC,latC,label="Est. 7"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.001,fontface="bold") +
    geom_point(data=coord_Formosa,aes(longF,latF,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Formosa,aes(longF,latF,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Formosa,aes(longF,latF,label="Est. 8"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.003,fontface="bold") +
    scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,30),
                           breaks=seq(0,30,3),oob=squish,guide=NULL) +
    scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,30),
                         breaks=seq(0,30,3),oob=squish) +
    labs(fill="Acumulado (mm)",x="Longitud",y="Latitud",
         title=paste("Precipitación acumulada entre el",dia_inicio,"y el",dia_final),
         subtitle=paste("Probabilidad de acumulado: 1 mm =",
                        Probabilidad$Prob_1mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "20 mm =",Probabilidad$Prob_20mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "50 mm =",Probabilidad$Prob_50mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "100 mm =",Probabilidad$Prob_100mm[Probabilidad$S2_Desde==as.character(pp1$time[1])])) +
    theme(legend.key.size=unit(1.5,'cm')) +
    geom_label(aes(x=-57.671,y=-27.855,label="Estaciones (Est.):
1 - Resistencia Aero
2 - Tres Horquetas ROLON
3 - Moncholo
4 - General Vedia
5 - Lote 16
6 - Puerto Bermejo Km 90
7 - Corrientes Aero
8 - Formosa Aero"),stat="unique",size=3,color="black",fill="white") 
} else {
  ggplot(pp_total,aes(lon,lat)) +
    geom_contour_fill(aes(z=precipitation)) +
    geom_contour(aes(z=precipitation),color="grey90",size=0.15) + 
    geom_sf(data=mapa_pais,inherit.aes=FALSE,fill=NA,color="black") +
    geom_sf(data=mapa_provincias,inherit.aes=FALSE,color="black") +
    geom_sf(data=mapa_departamentos,inherit.aes=FALSE,color="grey40") +
    coord_sf(ylim=c(-27.95,-26.05),xlim=c(-59.45,-57.55)) +
    geom_point(data=coord_Resistencia,aes(longR,latR,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Resistencia,aes(longR,latR,label=paste(acumulado,"mm")),
              color="black",size=2.28) + 
    geom_text(data=coord_Resistencia,aes(longR,latR,label="Est. 1"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.001,fontface="bold") +
    geom_point(data=coord_TresHROLON,aes(longT,latT,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_TresHROLON,aes(longT,latT,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_TresHROLON,aes(longT,latT,label="Est. 2"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.002,fontface="bold") +
    geom_point(data=coord_Moncholo,aes(longM,latM,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Moncholo,aes(longM,latM,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Moncholo,aes(longM,latM,label="Est. 3"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.004,fontface="bold") +
    geom_point(data=coord_GralVedia,aes(longG,latG,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_GralVedia,aes(longG,latG,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_GralVedia,aes(longG,latG,label="Est. 4"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.009,fontface="bold") +
    geom_point(data=coord_Lote16,aes(longL,latL,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Lote16,aes(longL,latL,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Lote16,aes(longL,latL,label="Est. 5"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.002,fontface="bold") +
    geom_point(data=coord_PtoBjo90,aes(longP,latP,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_PtoBjo90,aes(longP,latP,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_PtoBjo90,aes(longP,latP,label="Est. 6"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.002,fontface="bold") +
    geom_point(data=coord_Corrientes,aes(longC,latC,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Corrientes,aes(longC,latC,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Corrientes,aes(longC,latC,label="Est. 7"),
              color="black",size=2.8,nudge_y=-0.0535,nudge_x=0.001,fontface="bold") +
    geom_point(data=coord_Formosa,aes(longF,latF,fill=acumulado),color="black",
               pch=21,size=12,alpha=0.75,stroke=0.3) +
    geom_text(data=coord_Formosa,aes(longF,latF,label=paste(acumulado,"mm")),
              color="black",size=2.28) +
    geom_text(data=coord_Formosa,aes(longF,latF,label="Est. 8"),
              color="black",size=2.8,nudge_y=0.0535,nudge_x=-0.003,fontface="bold") +
    scale_colour_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                           breaks=seq(0,100,10),oob=squish,guide=NULL) +
    scale_fill_gradientn(colours=c('#F2FBFC','#A2B9D9','#712476'),limits=c(0,100),
                         breaks=seq(0,100,10),oob=squish) +
    labs(fill="Acumulado (mm)",x="Longitud",y="Latitud",
         title=paste("Precipitación acumulada entre el",dia_inicio,"y el",dia_final),
         subtitle=paste("Probabilidad de acumulado: 1 mm =",
                        Probabilidad$Prob_1mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "20 mm =",Probabilidad$Prob_20mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "50 mm =",Probabilidad$Prob_50mm[Probabilidad$S2_Desde==as.character(pp1$time[1])],"//",
                        "100 mm =",Probabilidad$Prob_100mm[Probabilidad$S2_Desde==as.character(pp1$time[1])])) +
    theme(legend.key.size=unit(1.5,'cm')) +
    geom_label(aes(x=-57.671,y=-27.855,label="Estaciones (Est.):
1 - Resistencia Aero
2 - Tres Horquetas ROLON
3 - Moncholo
4 - General Vedia
5 - Lote 16
6 - Puerto Bermejo Km 90
7 - Corrientes Aero
8 - Formosa Aero"),stat="unique",size=3,color="black",fill="white")
}
