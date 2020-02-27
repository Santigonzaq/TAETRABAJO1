library(readr)

CAL_VID_2019 <- read_csv2("Universidad/Materias/Técnicas de Aprendizaje Estadistico/Primer trabajo/CAL_VID_2019.csv")
View(CAL_VID_2019)


save(CAL_VID_2019, file = "CAL_VID_2019.RData")

load("CAL_VID_2019.RData")


vars<-c("encuesta_calidad.barrio",
        "encuesta_calidad.comuna",
        "encuesta_calidad.p_36",
        "encuesta_calidad.p_38",
        "encuesta_calidad.p_45",
        "encuesta_calidad.p_49")

DAT_EDU<-CAL_VID_2019[,vars]
names(DAT_EDU)
dim(DAT_EDU)
head(DAT_EDU)

DAT_EDU$encuesta_calidad.p_36 <- as.factor(DAT_EDU$encuesta_calidad.p_36)
DAT_EDU$encuesta_calidad.p_38 <- as.factor(DAT_EDU$encuesta_calidad.p_38)
DAT_EDU$encuesta_calidad.p_45 <- as.factor(DAT_EDU$encuesta_calidad.p_45)
DAT_EDU$encuesta_calidad.p_49 <- as.factor(DAT_EDU$encuesta_calidad.p_49)
DAT_EDU$encuesta_calidad.barrio <- as.factor(DAT_EDU$encuesta_calidad.barrio) 
DAT_EDU$encuesta_calidad.comuna <- as.factor(DAT_EDU$encuesta_calidad.comuna)
str(DAT_EDU)

require(editrules)
Cond1<-editset(c("encuesta_calidad.p_38>0",
                 "encuesta_calidad.p_45>0",
                 "encuesta_calidad.p_49>0",
                 "encuesta_calidad.p_36 %in% c('1','2')",
                 "encuesta_calidad.p_38 %in% c('1','2','3','4','5','6','7','8','9','10',
                 '11','12','13','14','15','16','17','18','19','20','21','22')",
                 "encuesta_calidad.p_45 %in% c('1','2','3','4','5','6','7','8','9','10')",
                 "encuesta_calidad.p_49 %in% c('1','2')"))

Errores1<-violatedEdits(c(Cond1), DAT_EDU)

plot(Errores1)


Locali1<-localizeErrors(Cond1,DAT_EDU)$adapt
apply(X=Locali1, MARGIN = 2, FUN=function(x)
  which(x==TRUE))

#Identificando problemas de cada variable 


Problem1<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_36

Problem2<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_38

Problem3<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_45
Problem4<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_49


# Todos los problemas:

Problems<-Reduce(union,list(Problem1,Problem2,Problem3,
                            Problem4))

# RESUMEN

require(dplyr)

# PREGUNTA 36:
0/nrow(DAT_EDU) # % PROBLEMA CON 36
DAT_EDU_36<-DAT_EDU[,c("encuesta_calidad.barrio",
                       "encuesta_calidad.p_36")]
TABLA_36<-round(100*as.matrix(table(DAT_EDU_36))/rowSums(as.matrix(table(DAT_EDU_36))),2) 
TABLA_36

# PREGUNTA 38:
length(Problem2)/nrow(DAT_EDU) # % PROBLEMA CON 38

DAT_EDU_38<-DAT_EDU[-Problem2,
                    c("encuesta_calidad.barrio",
                      "encuesta_calidad.p_38")]
TABLA_38<-round(100*as.matrix(table(DAT_EDU_38))/rowSums(as.matrix(table(DAT_EDU_38))),2)
TABLA_38


# PREGUNTA 45:
length(Problem3)/nrow(DAT_EDU) # % PROBLEMA CON 45
DAT_EDU_45<-DAT_EDU[-Problem3,
                    c("encuesta_calidad.barrio",
                      "encuesta_calidad.p_45")]
TABLA_45<-round(100*as.matrix(table(DAT_EDU_45))/rowSums(as.matrix(table(DAT_EDU_45))),2)
TABLA_45


# PREGUNTA 49:
length(Problem4)/nrow(DAT_EDU) # % PROBLEMA CON 49
DAT_EDU_49<-DAT_EDU[-Problem4,
                    c("encuesta_calidad.barrio",
                      "encuesta_calidad.p_49")]
TABLA_49<-round(100*as.matrix(table(DAT_EDU_49))/rowSums(as.matrix(table(DAT_EDU_49))),2)
TABLA_49


#Construyendo el DATAFRAME-----------------

BARRIOS_EDU<-row.names(as.matrix(summary(DAT_EDU$encuesta_calidad.barrio,maxsum=100000)))


TOTAL_ENC<-as.vector(summary(DAT_EDU$encuesta_calidad.barrio,
                             maxsum=100000))


EDUCACION<-data.frame(BARRIOS_EDU,TOTAL_ENC, P.36.1=TABLA_36[,1], P.36.2=TABLA_36[,2],
                      P.38.3=TABLA_38[,3], P.38.4=TABLA_38[,4], P.38.5=TABLA_38[,5],
                      P.38.6=TABLA_38[,6], P.38.7=TABLA_38[,7], P.38.8=TABLA_38[,8], P.38.9=TABLA_38[,9],
                      P.38.10=TABLA_38[,10], P.38.11=TABLA_38[,11], P.38.12=TABLA_38[,12], P.38.13=TABLA_38[,13],
                      P.38.14=TABLA_38[,14], P.38.15=TABLA_38[,15], P.38.16=TABLA_38[,16], P.38.17=TABLA_38[,17],
                      P.38.18=TABLA_38[,18], P.38.19=TABLA_38[,19], P.38.20=TABLA_38[,20], P.38.21=TABLA_38[,21],
                      P.38.22=TABLA_38[,22], P.45.4=TABLA_45[,4], P.45.5=TABLA_45[,5],
                      P.45.6=TABLA_45[,6], P.45.7=TABLA_45[,7],
                      P.45.8=TABLA_45[,8], P.45.9=TABLA_45[,9], P.45.10=TABLA_45[,10]
                      )


##### ANALISIS DESCRIPTIVO ---------------------


#### AGRUPACIONES -----------------------                     

library(FactoClass)
library(ggplot2)
library(factoextra)
library(NbClust)


dat_scal <- scale(EDUCACION[,3:31])
head(dat_scal)
dat_dist=dist(dat_scal)

fviz_nbclust(x = na.omit(dat_scal),FUNcluster = kmeans, method = "silhouette") ## metodo del codo
fviz_nbclust(x = na.omit(dat_scal),FUNcluster = kmeans, method = "wss")
fviz_nbclust(x = na.omit(dat_scal),FUNcluster = kmeans, method = "gap_stat")

library(NbClust)

NbClust(data = na.omit(dat_scal), diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 10, method = "kmeans")


dat_clust=hclust(dat_dist,method="single")
dat_clust

set.seed(2016)
km.res <- kmeans(na.omit(dat_scal), centers=3, iter.max = 10, nstart = 1)

## agregar una variable nueva con los grupos 
datosC <- data.frame(cbind(na.omit(EDUCACION), cluster = km.res$cluster))
head(datosC)


grupo1 <- datosC[datosC$cluster==1,]
grupo2 <- datosC[datosC$cluster==2,]
grupo3 <- datosC[datosC$cluster==3,]

for(i in 1: nrow(datosC)){
  if(datosC$cluster[i]==1){datosC$COLOR[i] <- "yellow"}
  if(datosC$cluster[i]==2){datosC$COLOR[i] <- "red"}
  if(datosC$cluster[i]==3){datosC$COLOR[i] <- "blue"}
}

DATOS <- data.frame(datosC[,c(1,32,33)])
names(DATOS)[1]="NOMBRE"
DATOS[1:3,]

DATOS$NOMBRE <- chartr('ÁÉÍÓÚ','AEIOU', DATOS$NOMBRE)
DATOS[1:3,]

### MAPA

library(ggplot2)
library(rgdal)
library(sp)
library(leaflet)
setwd("C:/Users/Yerandin/Documents/Universidad/Materias/Técnicas de Aprendizaje Estadistico/Primer trabajo/medellin")
unzip("C:/Users/Yerandin/Documents/Universidad/Materias/Técnicas de Aprendizaje Estadistico/Primer trabajo/medellin/Barrio_Vereda.zip")
barrios_med=readOGR(dsn = "C:/Users/Yerandin/Documents/Universidad/Materias/Técnicas de Aprendizaje Estadistico/Primer trabajo/medellin", layer = "Barrio_Vereda" )

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
  for (j in 1:276) {
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


#PARA LOS QUE ESTUDIAN-------------------------------------
#Grupo 1-----------

###Usted estudia-  p_36 1
###Causa por la que no estudia p_38 2
### Ultimo nivel de estudio p_45 3,4,5,6,7,8,9,10



GRUPO1E <- data.frame(P.36.1=grupo1$P.36.1, P.36.2=(grupo1$P.36.2)*(-1), 
                      P.38.3=(grupo1$P.38.3)*(-1), P.38.4=(grupo1$P.38.4)*(-1),
                      P.38.5=(grupo1$P.38.5)*(-1), P.38.6=(grupo1$P.38.6)*(-1),
                      P.38.7=(grupo1$P.38.7)*(-1), P.38.8=(grupo1$P.38.8)*(-1),
                      P.38.9=(grupo1$P.38.9)*(-1), P.38.10=(grupo1$P.38.10)*(-1),
                      P.38.11=(grupo1$P.38.11)*(-1), P.38.12=(grupo1$P.38.12)*(-1),
                      P.38.13=(grupo1$P.38.13)*(-1), P.38.14=(grupo1$P.38.14)*(-1),
                      P.38.15=(grupo1$P.38.15)*(-1), P.38.16=(grupo1$P.38.16)*(-1),
                      P.38.17=(grupo1$P.38.17)*(-1), P.38.18=(grupo1$P.38.18)*(-1),
                      P.38.19=(grupo1$P.38.19)*(-1), P.38.20=(grupo1$P.38.20)*(-1),
                      P.38.21=(grupo1$P.38.21)*(-1), P.38.22=(grupo1$P.38.22)*(-1),
                      P.45.4=grupo1$P.45.4, P.45.5=grupo1$P.45.5, P.45.6=grupo1$P.45.6,
                      P.45.7=grupo1$P.45.7, P.45.8=grupo1$P.45.8, P.45.9=grupo1$P.45.9,
                      P.45.10=grupo1$P.45.10)

apply(GRUPO1E,2,mean)
sum(apply(GRUPO1E,2,mean))/29


#Grupo 2-----------

###Usted estudia-  p_36 1
###Causa por la que no estudia p_38 2
### Ultimo nivel de estudio p_45 3,4,5,6,7,8,9,10



GRUPO2E <- data.frame(P.36.1=grupo2$P.36.1, P.36.2=(grupo2$P.36.2)*(-1), 
                      P.38.3=(grupo2$P.38.3)*(-1), P.38.4=(grupo2$P.38.4)*(-1),
                      P.38.5=(grupo2$P.38.5)*(-1), P.38.6=(grupo2$P.38.6)*(-1),
                      P.38.7=(grupo2$P.38.7)*(-1), P.38.8=(grupo2$P.38.8)*(-1),
                      P.38.9=(grupo2$P.38.9)*(-1), P.38.10=(grupo2$P.38.10)*(-1),
                      P.38.11=(grupo2$P.38.11)*(-1), P.38.12=(grupo2$P.38.12)*(-1),
                      P.38.13=(grupo2$P.38.13)*(-1), P.38.14=(grupo2$P.38.14)*(-1),
                      P.38.15=(grupo2$P.38.15)*(-1), P.38.16=(grupo2$P.38.16)*(-1),
                      P.38.17=(grupo2$P.38.17)*(-1), P.38.18=(grupo2$P.38.18)*(-1),
                      P.38.19=(grupo2$P.38.19)*(-1), P.38.20=(grupo2$P.38.20)*(-1),
                      P.38.21=(grupo2$P.38.21)*(-1), P.38.22=(grupo2$P.38.22)*(-1),
                      P.45.4=grupo2$P.45.4, P.45.5=grupo2$P.45.5, P.45.6=grupo2$P.45.6,
                      P.45.7=grupo2$P.45.7, P.45.8=grupo2$P.45.8, P.45.9=grupo2$P.45.9,
                      P.45.10=grupo2$P.45.10)

apply(GRUPO2E,2,mean)
sum(apply(GRUPO2E,2,mean))/29

#Grupo 3-----------

###Usted estudia-  p_36 1
###Causa por la que no estudia p_38 2
### Ultimo nivel de estudio p_45 3,4,5,6,7,8,9,10

GRUPO3E <- data.frame(P.36.1=grupo3$P.36.1, P.36.2=(grupo3$P.36.2)*(-1), 
                      P.38.3=(grupo3$P.38.3)*(-1), P.38.4=(grupo3$P.38.4)*(-1),
                      P.38.5=(grupo3$P.38.5)*(-1), P.38.6=(grupo3$P.38.6)*(-1),
                      P.38.7=(grupo3$P.38.7)*(-1), P.38.8=(grupo3$P.38.8)*(-1),
                      P.38.9=(grupo3$P.38.9)*(-1), P.38.10=(grupo3$P.38.10)*(-1),
                      P.38.11=(grupo3$P.38.11)*(-1), P.38.12=(grupo3$P.38.12)*(-1),
                      P.38.13=(grupo3$P.38.13)*(-1), P.38.14=(grupo3$P.38.14)*(-1),
                      P.38.15=(grupo3$P.38.15)*(-1), P.38.16=(grupo3$P.38.16)*(-1),
                      P.38.17=(grupo3$P.38.17)*(-1), P.38.18=(grupo3$P.38.18)*(-1),
                      P.38.19=(grupo3$P.38.19)*(-1), P.38.20=(grupo3$P.38.20)*(-1),
                      P.38.21=(grupo3$P.38.21)*(-1), P.38.22=(grupo3$P.38.22)*(-1),
                      P.45.4=grupo3$P.45.4, P.45.5=grupo3$P.45.5, P.45.6=grupo3$P.45.6,
                      P.45.7=grupo3$P.45.7, P.45.8=grupo3$P.45.8, P.45.9=grupo3$P.45.9,
                      P.45.10=grupo3$P.45.10)

apply(GRUPO3E,2,mean)
sum(apply(GRUPO3E,2,mean))/29



#PARA LOS QUE NO ESTUDIAN-------------------------------------

#Grupo 1-----------

###Usted estudia-  p_36 1
###Causa por la que no estudia p_38 2
### Ultimo nivel de estudio p_45 3,4,5,6,7,8,9,10



GRUPO1NE <- data.frame(P.36.1=(grupo1$P.36.1)*(-1), P.36.2=grupo1$P.36.2, 
                       P.38.3=grupo1$P.38.3, P.38.4=grupo1$P.38.4,
                       P.38.5=grupo1$P.38.5, P.38.6=grupo1$P.38.6,
                       P.38.7=grupo1$P.38.7, P.38.8=grupo1$P.38.8,
                       P.38.9=grupo1$P.38.9, P.38.10=grupo1$P.38.10,
                       P.38.11=grupo1$P.38.11, P.38.12=grupo1$P.38.12,
                       P.38.13=grupo1$P.38.13, P.38.14=grupo1$P.38.14,
                       P.38.15=grupo1$P.38.15, P.38.16=grupo1$P.38.16,
                       P.38.17=grupo1$P.38.17, P.38.18=grupo1$P.38.18,
                       P.38.19=grupo1$P.38.19, P.38.20=grupo1$P.38.20,
                       P.38.21=grupo1$P.38.21, P.38.22=grupo1$P.38.22,
                       P.45.4=(grupo1$P.45.4)*(-1), P.45.5=(grupo1$P.45.5)*(-1),
                       P.45.6=(grupo1$P.45.6)*(-1), P.45.7=(grupo1$P.45.7)*(-1), 
                       P.45.8=(grupo1$P.45.8)*(-1), P.45.9=(grupo1$P.45.9)*(-1),
                       P.45.10=(grupo1$P.45.10)*(-1))

apply(GRUPO1NE,2,mean)
sum(apply(GRUPO1NE,2,mean))/29


#Grupo 2-----------

###Usted estudia-  p_36 1
###Causa por la que no estudia p_38 2
### Ultimo nivel de estudio p_45 3,4,5,6,7,8,9,10



GRUPO2NE <- data.frame(P.36.1=(grupo2$P.36.1)*(-1), P.36.2=grupo2$P.36.2, 
                       P.38.3=grupo2$P.38.3, P.38.4=grupo2$P.38.4,
                       P.38.5=grupo2$P.38.5, P.38.6=grupo2$P.38.6,
                       P.38.7=grupo2$P.38.7, P.38.8=grupo2$P.38.8,
                       P.38.9=grupo2$P.38.9, P.38.10=grupo2$P.38.10,
                       P.38.11=grupo2$P.38.11, P.38.12=grupo2$P.38.12,
                       P.38.13=grupo2$P.38.13, P.38.14=grupo2$P.38.14,
                       P.38.15=grupo2$P.38.15, P.38.16=grupo2$P.38.16,
                       P.38.17=grupo2$P.38.17, P.38.18=grupo2$P.38.18,
                       P.38.19=grupo2$P.38.19, P.38.20=grupo2$P.38.20,
                       P.38.21=grupo2$P.38.21, P.38.22=grupo2$P.38.22,
                       P.45.4=(grupo2$P.45.4)*(-1), P.45.5=(grupo2$P.45.5)*(-1),
                       P.45.6=(grupo2$P.45.6)*(-1), P.45.7=(grupo2$P.45.7)*(-1), 
                       P.45.8=(grupo2$P.45.8)*(-1), P.45.9=(grupo2$P.45.9)*(-1),
                       P.45.10=(grupo2$P.45.10)*(-1))

apply(GRUPO2NE,2,mean)
sum(apply(GRUPO2NE,2,mean))/29

#Grupo 3-----------

###Usted estudia-  p_36 1
###Causa por la que no estudia p_38 2
### Ultimo nivel de estudio p_45 3,4,5,6,7,8,9,10

GRUPO3NE <- data.frame(P.36.1=(grupo3$P.36.1)*(-1), P.36.2=grupo3$P.36.2, 
                       P.38.3=grupo3$P.38.3, P.38.4=grupo3$P.38.4,
                       P.38.5=grupo3$P.38.5, P.38.6=grupo3$P.38.6,
                       P.38.7=grupo3$P.38.7, P.38.8=grupo3$P.38.8,
                       P.38.9=grupo3$P.38.9, P.38.10=grupo3$P.38.10,
                       P.38.11=grupo3$P.38.11, P.38.12=grupo3$P.38.12,
                       P.38.13=grupo3$P.38.13, P.38.14=grupo3$P.38.14,
                       P.38.15=grupo3$P.38.15, P.38.16=grupo3$P.38.16,
                       P.38.17=grupo3$P.38.17, P.38.18=grupo3$P.38.18,
                       P.38.19=grupo3$P.38.19, P.38.20=grupo3$P.38.20,
                       P.38.21=grupo3$P.38.21, P.38.22=grupo3$P.38.22,
                       P.45.4=(grupo3$P.45.4)*(-1), P.45.5=(grupo3$P.45.5)*(-1),
                       P.45.6=(grupo3$P.45.6)*(-1), P.45.7=(grupo3$P.45.7)*(-1), 
                       P.45.8=(grupo3$P.45.8)*(-1), P.45.9=(grupo3$P.45.9)*(-1),
                       P.45.10=(grupo3$P.45.10)*(-1))

apply(GRUPO3NE,2,mean)

sum(apply(GRUPO3NE,2,mean))/29


  summary(grupo3)
