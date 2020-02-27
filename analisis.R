##### ANALISIS DESCRIPTIVO 

#hist(CALIDAD$P.36.SI)
#boxplot(CALIDAD$P.69.1,CALIDAD$P.69.2)

#cantidad de mujeres encuestadas por comuna
#par(mar=c(11,4.1,0.5,2))
#barplot(summary(DAT_MUJ$encuesta_calidad.comuna),
   #     las=2)

#par(mar=c(11,4.1,0.5,2))
#barplot(sort(summary(DAT_MUJ$encuesta_calidad.comuna),decreasing=TRUE),
    #    las=2)

#resum_21<-summary(DAT_MUJ$encuesta_calidad.barrio,maxsum=10000)
#par(mar=c(11,4.1,0.5,2))
#barplot(sort(resum_21[resum_21>1300],decreasing=TRUE),
 #       las=2)

#par(mar=c(11,4.1,0.5,2))
#barplot(sort(summary(as.factor(DAT_MUJ$encuesta_calidad.p_20)),
 #            decreasing = TRUE),las=2)

require(readr)
require(tidyr)
require(ggplot2)

#### agrupaciones

dat_scal <- scale(CALIDAD[,3:38])
dat_dist=dist(dat_scal)

dat_clust=hclust(dat_dist,method="single")


library(FactoClass)
library(ggplot2)
library(factoextra)

fviz_nbclust(x = na.omit(dat_scal),FUNcluster = kmeans, method = "silhouette") ## metodo del codo
fviz_nbclust(x = na.omit(dat_scal),FUNcluster = kmeans, method = "wss")
fviz_nbclust(x = na.omit(dat_scal),FUNcluster = kmeans, method = "gap_stat")

library(NbClust)

NbClust(data = na.omit(dat_scal), diss = NULL, distance = "euclidean",
        min.nc = 4, max.nc = 10, method = "kmeans")





## despues de tener el k optimo que fue k=6, calculo kmeans 

set.seed(2016)
km.res <- kmeans(na.omit(dat_scal), centers=6, iter.max = 10, nstart = 1)

## agregar una variable nueva con los grupos 
datosC <- data.frame(cbind(na.omit(CALIDAD), cluster = km.res$cluster))
head(datosC)



grupo1 <- datosC[datosC$cluster==1,]
grupo2 <- datosC[datosC$cluster==2,]
grupo3 <- datosC[datosC$cluster==3,]
grupo4 <- datosC[datosC$cluster==4,]
grupo5 <- datosC[datosC$cluster==5,]
grupo6 <- datosC[datosC$cluster==6,]

for(i in 1: nrow(datosC)){
  if(datosC$cluster[i]==1){datosC$COLOR[i] <- "yellow"}
  if(datosC$cluster[i]==2){datosC$COLOR[i] <- "red"}
  if(datosC$cluster[i]==3){datosC$COLOR[i] <- "blue"}
  if(datosC$cluster[i]==4){datosC$COLOR[i] <- "#D53CE8"}
  if(datosC$cluster[i]==5){datosC$COLOR[i] <- "green"}
  if(datosC$cluster[i]==6){datosC$COLOR[i] <- "#FF753E"}
}



DATOS <- data.frame(datosC[,c(1,39,40)])
names(DATOS)[1]="NOMBRE"
DATOS[1:3,]

DATOS$NOMBRE <- chartr('ÁÉÍÓÚ','AEIOU', DATOS$NOMBRE)
DATOS[1:3,]


### MAPA

library(ggplot2)
library(rgdal)
library(sp)
library(leaflet)
setwd("C:/Users/USUARIO/Downloads/medellin")
unzip("C:/Users/USUARIO/Downloads/medellin/Barrio_Vereda.zip")
barrios_med=readOGR(dsn = "C:/Users/USUARIO/Downloads/medellin", layer = "Barrio_Vereda" )

print(barrios_med@data$NOMBRE)
nombres_barrios=iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")

library(stringr)

barrios_med@data$NOMBRE  <- str_to_upper(nombres_barrios, locale = "es")
barrios_med@data$NOMBRE <- chartr('ÁÉÍÓÚ','AEIOU', barrios_med@data$NOMBRE)

#ordenar en orden del abecedario 

library(dplyr)

#barrios_med@data$NOMBRE <- arrange(barrios_med@data, NOMBRE)
DATOS <- arrange(DATOS, NOMBRE)


#cambiando los nombres para que coincidan debe dar dim 297
#PENDIENTE  PROGRESO N°.2




DATOS$NOMBRE[DATOS$NOMBRE=="ALTAVISTA CENTRAL"]<- "ALTAVISTA SECTOR CENTRAL"
DATOS$NOMBRE[DATOS$NOMBRE=="AREA DE EXPANCION SAN CRISTOBAL"]<- "AREA DE EXPANSION SAN CRISTOBAL"
DATOS$NOMBRE[DATOS$NOMBRE=="AURES Nº 1"]<- "AURES NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="AURES Nº 2"]<- "AURES NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="BARRIO CAYCEDO"]<- "BARRIO CAICEDO"
DATOS$NOMBRE[DATOS$NOMBRE=="BASILIA"]<- "BRASILIA"
DATOS$NOMBRE[DATOS$NOMBRE=="BOMBONA Nº 1"]<- "BOMBONA NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="CALAZANS PARTE ALTA"]<- "CALASANZ PARTE ALTA"
DATOS$NOMBRE[DATOS$NOMBRE=="CAMPO VALDES Nº 1"]<- "CAMPO VALDES NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="CARLOS E RESTREPO"]<- "CARLOS E. RESTREPO"
DATOS$NOMBRE[DATOS$NOMBRE=="LA ASOMADERA Nº 1"]<- "ASOMADERA NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="MANRIQUE CENTRAL Nº 1"]<- "MANRIQUE CENTRAL NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="PROGRESO"]<- "EL PROGRESO"
DATOS$NOMBRE[DATOS$NOMBRE=="SAN JAVIER Nº 1"]<- "SAN JAVIER NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="SAN JAVIER Nº 2"]<- "SAN JAVIER NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="JUAN XXIII - LA QUIEBRA"]<- "JUAN XXIII LA QUIEBRA"
DATOS$NOMBRE[DATOS$NOMBRE=="LA ESPERANZA Nº 2"]<- "LA ESPERANZA NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="MARIA CANO - CARAMBOLAS"]<- "MARIA CANO-CARAMBOLAS"
DATOS$NOMBRE[DATOS$NOMBRE=="CAMPO VALDES Nº 2"]<- "CAMPO VALDES NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="DOCE DE OCTUBRE Nº 1"]<- "DOCE DE OCTUBRE NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="DOCE DE OCTUBRE Nº 2"]<- "DOCE DE OCTUBRE NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="EL DIAMANTE Nº 2"]<- "EL DIAMANTE NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="EL NOGAL - LOS ALMENDROS"]<- "EL NOGAL-LOS ALMENDROS"
DATOS$NOMBRE[DATOS$NOMBRE=="EL PLAYON DE LOS COMUNEROS"]<- "PLAYON DE LOS COMUNEROS"
DATOS$NOMBRE[DATOS$NOMBRE=="EL SOCORRO / LA GABRIELA"]<- "EL SOCORRO"
DATOS$NOMBRE[DATOS$NOMBRE=="LA ASOMADERA Nº 2"]<- "ASOMADERA NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="LA ASOMADERA Nº 3"]<- "ASOMADERA NO.3"
DATOS$NOMBRE[DATOS$NOMBRE=="LAS LOMAS Nº 1"]<- "LAS LOMAS NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="LAS LOMAS Nº 2"]<- "LAS LOMAS NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="LOS BALSOS Nº 1"]<- "LOS BALSOS NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="LOS BALSOS Nº 2"]<- "LOS BALSOS NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="MANRIQUE CENTRAL Nº 2"]<- "MANRIQUE CENTRAL NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="MOSCU Nº 1"]<- "MOSCU NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="MOSCU Nº 2"]<- "MOSCU NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="SAN JOSELA CIMA Nº 1"]<- "SAN JOSE LA CIMA NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="SAN JOSE LA CIMA Nº 2"]<- "SAN JOSE LA CIMA NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="SANTO DOMINGO SABIO Nº 1"]<- "SANTO DOMINGO SAVIO NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="SANTO DOMINGO SABIO Nº 2"]<- "SANTO DOMINGO SAVIO NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="VERSALLES Nº 1"]<- "VERSALLES NO.1"
DATOS$NOMBRE[DATOS$NOMBRE=="VERSALLES Nº 2"]<- "VERSALLES NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="BARRIO FACULTAD DE MINAS"]<- "FACULTAD DE MINAS"
DATOS$NOMBRE[DATOS$NOMBRE=="BERMEJAL- LOS ALAMOS"]<- "BERMEJAL-LOS ALAMOS"
DATOS$NOMBRE[DATOS$NOMBRE=="BOMBONA Nº 2"]<- "BOMBONA NO.2"
DATOS$NOMBRE[DATOS$NOMBRE=="CABECERA SAN ANT DE PR."]<- "SAN ANTONIO DE PRADO"
DATOS$NOMBRE[DATOS$NOMBRE=="CERRO EL VOLADOR"]<- "B. CERRO EL VOLADOR"
DATOS$NOMBRE[DATOS$NOMBRE=="CORREGIMIENTO PALMITAS"]<- "PALMITAS SECTOR CENTRAL"
DATOS$NOMBRE[DATOS$NOMBRE=="NUEVA VILLA DE ABURRA"]<- "NUEVA VILLA DEL ABURRA"
DATOS$NOMBRE[DATOS$NOMBRE=="EL YOLOMBO"]<- "YOLOMBO"
DATOS$NOMBRE[DATOS$NOMBRE=="VILLA TINA"]<- "VILLATINA"
DATOS$NOMBRE[DATOS$NOMBRE=="VILLA LILIAM"]<- "VILLA LILLIAM"
DATOS$NOMBRE[DATOS$NOMBRE=="CABECERA SAN CRISTOBAL"]<- "CABECERA URBANA CORREGIMIENTO SAN CRISTOBAL"
DATOS$NOMBRE[DATOS$NOMBRE=="MIRA FLORES"]<- "MIRAFLORES"




dim(merge(x = DATOS, y = barrios_med@data , by = "NOMBRE"))


#-------  PONER UNA COLUMNA DE COLORES EN LA METADATA------------------


barrios_med@data$COLOR<-rep("white",332)



for (i in 1:332) {
  for (j in 1:297) {
    if(barrios_med@data$NOMBRE[i]==DATOS$NOMBRE[j]){
      barrios_med@data$COLOR[i] <- DATOS$COLOR[j]
    }
    
  }
  
}






#------------------------- CREACION DEL MAPA ---------------------
library(rgdal)
library(leaflet)


m=leaflet(barrios_med)
m=addTiles(m)
m


#colores=sample(x=c("orange","green","yellow"),size=length(nombres_barrios),replace=TRUE)

m=addPolygons(m,popup=nombres_barrios,color=barrios_med@data$COLOR)
m

