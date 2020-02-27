


load("E:/ASUS R/Desktop/TAE_trabajo_1/CAL_VID_2019.RData")

#aca se sacan las variables del dataset
#vars es un array de una dimension donde guardamos las variables de las preguntas
vars<-c("encuesta_calidad.barrio",
        "encuesta_calidad.p_212",
        "encuesta_calidad.p_213",
        "encuesta_calidad.p_214",
        "encuesta_calidad.p_215",
        "encuesta_calidad.p_216",
        "encuesta_calidad.p_217",
        "encuesta_calidad.p_218",
        "encuesta_calidad.p_252",
        "encuesta_calidad.p_312",
        "encuesta_calidad.p_318",
        "encuesta_calidad.p_320",
        "encuesta_calidad.p_321",
        "encuesta_calidad.p_322")

#saco mi dataset llamado datos, y le tiro mis vars
Datos<- CAL_VID_2019[vars]
#visualizo nombres de mi datos
names(Datos)
#visualizo las dimensiones
dim(Datos)
#visualizo los primeros elementos
head(Datos)

#DAT_MUJ$encuesta_calidad.p_307


#Aca editamos condiciones de los valores de las variables
require(editrules)
#el numero de autos p212>=0
#el numero de motos p213>=0
#el numero de ciclas p214>=0
#el numero de botes p215>=0
#numero de autos modelo 1año o menos 216>=0
#numero de autos modelo 2 a 5 años 217>=0
#numero de autos modelo mas 6 años>=0
#cantidad impuestovehicular anual 252>=0------0=no aplica
#contaminacion del aire 1-5 312
#estado vial 1-5 318
#cumplimiento normas transito 1-5 320
#cobertura tansporte publico 1-5 321
#calidad transporte publico 1-5 322
Cond1<-editset(c("encuesta_calidad.p_212>=0",
               "encuesta_calidad.p_213>=0",
               "encuesta_calidad.p_214>=0",
               "encuesta_calidad.p_215>=0",
               "encuesta_calidad.p_216>=0",
               "encuesta_calidad.p_217>=0",
               "encuesta_calidad.p_218>=0",
               "encuesta_calidad.p_252>0",
               "encuesta_calidad.p_312 %in% c('1','2','3','4','5')",
               "encuesta_calidad.p_318 %in% c('1','2','3','4','5')",
               "encuesta_calidad.p_320 %in% c('1','2','3','4','5')",
               "encuesta_calidad.p_321 %in% c('1','2','3','4','5')",
               "encuesta_calidad.p_322 %in% c('1','2','3','4','5')")
               )

#Chequea los registros que violan las reglas recien mencionadas
Errores1<-violatedEdits(c(Cond1), Datos)

#Se localizan errores en cada una de las variables de nuestro dataset
#se crea una matriz de errores, donde aparecen true o false segun se violen o no las condiciones
Locali1<-localizeErrors(Cond1,Datos)$adapt
apply(X=Locali1, MARGIN = 2, FUN=function(x)
  which(x==TRUE))

###########  IDENTIFICANDO LOS PROBLEMAS INDIVIDUALMENTE ############

#Identificamos los problemas que se van encontrando en locali1, contandolos y añadiendolos
# en los problems

Problem1<-apply(X=Locali1, MARGIN = 2,
                  FUN=function(x)
                    which(x==TRUE))$encuesta_calidad.p_212

Problem2<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_213

Problem3<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_214

Problem4<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_215

Problem5<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_216

Problem6<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_217

Problem7<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_218

Problem8<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_252

Problem9<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_312

Problem10<- apply(X = Locali1, MARGIN = 2,
                  FUN = function(x) 
                   which(x==TRUE))$encuesta_calidad.p318

Problem11<- apply(X = Locali1, MARGIN = 2,
                  FUN = function(x) 
                    which(x==TRUE))$encuesta_calidad.p320

Problem12<- apply(X = Locali1, MARGIN = 2,
                  FUN = function(x) 
                    which(x==TRUE))$encuesta_calidad.p321

Problem13<- apply(X = Locali1, MARGIN = 2,
                  FUN = function(x) 
                    which(x==TRUE))$encuesta_calidad.p322

# Todos los problemas:
Problems<-Reduce(union,list(Problem1,Problem2,Problem3,
            Problem4,Problem5,Problem6,
            Problem7,Problem8,Problem9,Problem10,
            Problem11, Problem12, Problem13))

################  RESUMEN ###########################################

require(dplyr)

#SOLO SE TOMAN LAS VARIBALES QUE TENGAN ERRORES, LAS QUE TIENEN PROBLEM# = 0, NO SE TENDRA EN CUENTA

# PREGUNTA 212:
#sacamos el procentaje de el numero de errores que hay en la 216
length(Problem1)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 0%
Datos_212<-Datos[c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_212")]
RESUM_212<-Datos_212 %>% group_by(encuesta_calidad.barrio) %>% summarise(Median_Carros=median(encuesta_calidad.p_212))
#class(RESUM_212)


# PREGUNTA 213:
#sacamos el procentaje de el numero de errores que hay en la 216
length(Problem2)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 3%
Datos_213<-Datos[c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_213")]
RESUM_213<-Datos_213 %>% group_by(encuesta_calidad.barrio) %>% summarise(Median_Motos=median(encuesta_calidad.p_213))



# PREGUNTA 214:
#sacamos el procentaje de el numero de errores que hay en la 214
length(Problem3)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 0%
Datos_214<-Datos[c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_214")]
RESUM_214<-Datos_214 %>% group_by(encuesta_calidad.barrio) %>% summarise(Median_Bicicletas=median(encuesta_calidad.p_214))



# PREGUNTA 216:
#sacamos el procentaje de el numero de errores que hay en la 216
length(Problem5)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 59%, NO SE TENDRÁ EN CUENTA EN EL DATAFRAME
#DADO QUE NO ES RELEVANTE PARA LA DIMENSION
Datos_216<-Datos[-Problem5,
                     c("encuesta_calidad.barrio",
                       "encuesta_calidad.p_216")]
TABLA_216<-round(100*as.matrix(table(Datos_216))/rowSums(as.matrix(table(Datos_216))),2) 

  
# PREGUNTA 217:
#sacamos el procentaje de el numero de errores que hay en la 217
length(Problem6)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 59%, NO SE TENDRÁ EN CUENTA EN EL DATAFRAME
#DADO QUE NO ES RELEVANTE PARA LA DIMENSION
Datos_217<-Datos[-Problem6,
                 c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_217")]
TABLA_217<-round(100*as.matrix(table(Datos_217))/rowSums(as.matrix(table(Datos_217))),2) 


# PREGUNTA 218:
#sacamos el procentaje de el numero de errores que hay en la 218
length(Problem7)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 59%, NO SE TENDRÁ EN CUENTA EN EL DATAFRAME
#DADO QUE NO ES RELEVANTE PARA LA DIMENSION
Datos_218<-Datos[-Problem7,
                 c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_218")]
TABLA_218<-round(100*as.matrix(table(Datos_218))/rowSums(as.matrix(table(Datos_218))),2) 


# PREGUNTA 252:
#sacamos el procentaje de el numero de errores que hay en la 252
length(Problem8)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 80%, NO SE TENDRA EN CUENTA EN EL DATAFRAME
Datos_252<-Datos[-Problem8,
                 c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_252")]
TABLA_252<-round(100*as.matrix(table(Datos_252))/rowSums(as.matrix(table(Datos_252))),2) 


# PREGUNTA 312:
#sacamos el procentaje de el numero de errores que hay en la 312
length(Problem9)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 2%
Datos_312<-Datos[c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_312")]
TABLA_312<-round(100*as.matrix(table(Datos_312))/rowSums(as.matrix(table(Datos_312))),2) 


# PREGUNTA 318:
#sacamos el procentaje de el numero de errores que hay en la 318
length(Problem10)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 0%
Datos_318<-Datos[c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_318")]
TABLA_318<-round(100*as.matrix(table(Datos_318))/rowSums(as.matrix(table(Datos_318))),2) 



# PREGUNTA 320:
#sacamos el procentaje de el numero de errores que hay en la 320
length(Problem11)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 0%
Datos_320<-Datos[c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_320")]
TABLA_320<-round(100*as.matrix(table(Datos_320))/rowSums(as.matrix(table(Datos_320))),2) 


# PREGUNTA 321:
#sacamos el procentaje de el numero de errores que hay en la 321
length(Problem12)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 0%
Datos_321<-Datos[c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_321")]
TABLA_321<-round(100*as.matrix(table(Datos_321))/rowSums(as.matrix(table(Datos_321))),2) 


# PREGUNTA 322:
#sacamos el procentaje de el numero de errores que hay en la 322
length(Problem13)/nrow(Datos)
#ESTA VARIABLE TIENE UN PORCENTAJE DE ERROR DE 0%
Datos_322<-Datos[c("encuesta_calidad.barrio",
                   "encuesta_calidad.p_322")]
TABLA_322<-round(100*as.matrix(table(Datos_322))/rowSums(as.matrix(table(Datos_322))),2) 


#ya teniendo todas las varibales en tablas independientes y con los problemas
#corregidos, procedemos a construir el dataframe con las vars que nos 
#aportan una buena cantidad de datos

##############   CONSTRUYENDO EL DATA FRAME  #######


BARRIOS<-row.names(as.matrix(summary(Datos$encuesta_calidad.barrio,
                  maxsum=100000)))

#Este es el total encuestados 
TOTAL_ENC<-as.vector(summary(Datos$encuesta_calidad.barrio,
                  maxsum=100000))

#agregamos las variables de las tablas y las medianas al dataframe
TRANSPORTE<-data.frame(BARRIOS,TOTAL_ENC,
                    #P.212.median = RESUM_212$Median_Carros,
                    #P.213.median = RESUM_213$Median_Motos,
                    #P.214.median = RESUM_214$Median_Bicicletas,
                    #las 5 opciones de p312
                    P.312.1 = TABLA_312[,1],
                    P.312.2 = TABLA_312[,2],
                    P.312.3 = TABLA_312[,3],
                    P.312.4 = TABLA_312[,4],
                    P.312.5 = TABLA_312[,5],
                    #las 5 opciones de p318
                    P.318.1 = TABLA_318[,1],
                    P.318.2 = TABLA_318[,2],
                    P.318.3 = TABLA_318[,3],
                    P.318.4 = TABLA_318[,4],
                    P.318.5 = TABLA_318[,5],
                    #las 5 opciones de p320
                    P.320.1 = TABLA_320[,1],
                    P.320.2 = TABLA_320[,2],
                    P.320.3 = TABLA_320[,3],
                    P.320.4 = TABLA_320[,4],
                    P.320.5 = TABLA_320[,5],
                    #las 5 opciones de p321
                    P.321.1 = TABLA_321[,1],
                    P.321.2 = TABLA_321[,2],
                    P.321.3 = TABLA_321[,3],
                    P.321.4 = TABLA_321[,4],
                    P.321.5 = TABLA_321[,5],
                    #las 5 de p322
                    P.322.1 = TABLA_322[,1],
                    P.322.2 = TABLA_322[,2],
                    P.322.3 = TABLA_322[,3],
                    P.322.4 = TABLA_322[,4],
                    P.322.5 = TABLA_322[,5])


################## AGRUPAMIENTO ##########################



# Analizando el metodo se decide hacer por agrupamiento no jerarquico
# k-means, dado que por jerarquico 

# Averiguaremos el 

#Lo primero que se hace es escalar los datos 
TRANSPORTE_SCAL <- scale(TRANSPORTE[,3:30])
# definimos la fn de distancias
TRANSPORTE_DIST = dist(TRANSPORTE_SCAL)




require(ggplot2)

library(FactoClass)
library(ggplot2)
library(factoextra)


fviz_nbclust(x = na.omit(TRANSPORTE_SCAL), FUNcluster =kmeans, method = "wss") 
# por el criterio del codo, se dan como numero de clusters entre 5 y 6


#luego de saber el numero de clusters k=6, hacemos la clusterizacion por kmeans


set.seed(2020)

clusterizacion_km <- kmeans(TRANSPORTE_SCAL, centers = 5, nstart = 50)
head(clusterizacion_km)



#Ya teniendo los datos clusterizados en 6 creamos un nuevo dataframe 
#con los nuevos datos



TRANSPORTE_CLUST <- data.frame(cbind(TRANSPORTE), cluster = clusterizacion_km$cluster)
head(TRANSPORTE_CLUST)


#otra visualización -----> LA MEJOR
fviz_cluster(clusterizacion_km, data = TRANSPORTE_SCAL  ,stand = T, geom = "point",
             pointsize = 1)
#visualizamos de manera geometrica, los 5 clusteres



# veamos el grafico de dispersion de las primeras 3 variables
p3_centrados <- scale(TRANSPORTE_CLUST[,3:5])
pairs(p3_centrados)
cov(p3_centrados)
#vemos por la matriz de covarianza que las variables tienen una relacion negativa asi que
#tienen una relacion inversa.



# Se crearán variables que contengan los barrios pertenecientes a cada cluster

cluster1 <- TRANSPORTE_CLUST[TRANSPORTE_CLUST$cluster == 1,]
summary(cluster1)

cluster2 <- TRANSPORTE_CLUST[TRANSPORTE_CLUST$cluster == 2,]
summary(cluster2)

cluster3 <- TRANSPORTE_CLUST[TRANSPORTE_CLUST$cluster == 3,]
summary(cluster3)

cluster4 <- TRANSPORTE_CLUST[TRANSPORTE_CLUST$cluster == 4,]
summary(cluster4)

cluster5 <- TRANSPORTE_CLUST[TRANSPORTE_CLUST$cluster == 5,]
summary(cluster5)


# Creamos una nueva varible con los colores que se usaran dependiendo de cada cluster
for(i in 1: nrow(TRANSPORTE_CLUST)){
  if(TRANSPORTE_CLUST$cluster[i]==1){TRANSPORTE_CLUST$COLOR[i] <- "yellow"}
  if(TRANSPORTE_CLUST$cluster[i]==2){TRANSPORTE_CLUST$COLOR[i] <- "red"}
  if(TRANSPORTE_CLUST$cluster[i]==3){TRANSPORTE_CLUST$COLOR[i] <- "blue"}
  if(TRANSPORTE_CLUST$cluster[i]==4){TRANSPORTE_CLUST$COLOR[i] <- "gray"}
  if(TRANSPORTE_CLUST$cluster[i]==5){TRANSPORTE_CLUST$COLOR[i] <- "green"}
  
}



DATOS <- data.frame(TRANSPORTE_CLUST[,c(1,28,29)])
names(DATOS)[1]="NOMBRE"
DATOS[1:3,]

DATOS$NOMBRE <- chartr('ÁÉÍÓÚ','AEIOU', DATOS$NOMBRE)
DATOS[1:3,]


### MAPA DE MEDELLIN PARA EL ANALISIS DE DATOS

library(ggplot2)
library(rgdal)
library(sp)
library(leaflet)

#leemos el archivo .shp
barrios_med=readOGR("E:/ASUS R/Desktop/Barrio_Vereda.shp",layer="Barrio_Vereda")
# el archivo contiene un dataframe llamado data, accedemos a el con @
# contiene id, codigo barrio, nombre barrio, shapearea, shapelen


# Mostramos los nombres de los barrios del dataframe del .shp
print(barrios_med@data$NOMBRE)
# Arreglamos el problema de codificacion en los nombres de los barrios
nombres_barrios=iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")


library(stringr)
# Se ponen los nombres en el mismo formato que tenemos nuestros nobres en DATOS
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

#Se ponen todos los colores en blanco para luego cambiar
barrios_med@data$COLOR<-rep("white",332)


#se asignan los colores a cada barrio
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











