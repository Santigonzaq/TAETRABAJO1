

load("C:/Users/Gonza/Desktop/Noveno semestre/TAE/trabajo1/CAL_VID_2019.RData")




##Saco mis variables de interes
vars<-c("encuesta_calidad.barrio","encuesta_calidad.p_258","encuesta_calidad.p_272","encuesta_calidad.p_280",
        "encuesta_calidad.p_281","encuesta_calidad.p_282","encuesta_calidad.p_283","encuesta_calidad.p_284",
        "encuesta_calidad.p_285","encuesta_calidad.p_286","encuesta_calidad.p_287","encuesta_calidad.p_288",
        "encuesta_calidad.p_289","encuesta_calidad.p_290","encuesta_calidad.p_229","encuesta_calidad.p_66")


length(vars)
#filtro para separar mi vector preguntas de todo el data set
DATA<-CAL_VID_2019[vars]

#Muestra los nombres de las variables
names(DATA)
##Dimensión después de filtrar
dim(DATA)
#Mostrar un pedazo
head(DATA)



# Cree usted que los Ãºltimos 12 meses las viviendas nuevas ofrecidas en venta, en su sector o barrio ha:
unique(DATA$encuesta_calidad.p_258)
##Tiene 5 categorías, la -98 y -99 son sospechos, qué significan?
##-98 y -99 es no sabe, no responde, por lo que si son errores que deben sacarse.

# CÃ³mo calificarÃa usted en una escala desde 1 hasta 5, la su nivel de satisfacciÃ³n con su municipio:
unique(DATA$encuesta_calidad.p_272)
##Tiene 6 categorías, la  -99 es sospechosa, qué significa?
##-98 y -99 es no responde, por lo que si son errores que deben sacarse.

# CÃ³mo calificarÃa usted en una escala desde 1 hasta 5, su grado de confianza en las Instituciones PÃºblicas del gobierno:
unique(DATA$encuesta_calidad.p_280)
##Tiene 6 categorías, la  -99 es sospechosa, qué significa?
##-99 No está idenfticada en la metada, es error.

# Â¿CÃ³mo calificarÃa en una escala de 1 hasta 5, la posibilidad de moverse libremente de un sitio a otro que tiene en su barrio o vereda:
unique(DATA$encuesta_calidad.p_281)
##Tiene 6 categorías, la  -99 es sospechosa, qué significa?
##-99 No está idenfticada en la metada, es error.

#CÃ³mo calificarÃusted en una escala desde 1 hasta 5, la libertad de expresar los pensamientos u opiniones polÃticas, que se tiene en su barrio o vereda
unique(DATA$encuesta_calidad.p_282)
##Tiene 6 categorías, la  -99 es sospechosa, qué significa?
##-99 No está idenfticada en la metada, es error.

#Como se siente en el barrio o vereda donde vive?
unique(DATA$encuesta_calidad.p_283)
##Tiene 4 categorías, la  -98 es sospechosa, qué significa?
##-99 No está idenfticada en la metada, es error.

#CuÃ¡les son los dos problemas mÃ¡s graves en orden de importancia para usted en relaciÃ³n con la seguridad que se presenta en su barrio , corregimiento o vereda? Primera opciÃ³n
unique(DATA$encuesta_calidad.p_284)
##Tiene 17 categorías, la  -98,-99, -77 es sospechosa, qué significa?
##-99 y -98 No está idenfticada en la metada, es error. -77 se refiere a otro

#CuÃ¡les son los dos problemas mÃ¡s graves en orden de importancia para usted en relaciÃ³n con la seguridad que se presenta en su barrio , corregimiento o vereda? Primera opciÃ³n
unique(DATA$encuesta_calidad.p_285)
##Tiene 17 categorías, la  -98,-88, -77 es sospechosa, qué significa?
##Los negativos son errores o no áplica.


#Durante los Ãºltimos doce meses, usted o algÃºn miembro de su hogar ha sido victima de algÃºn hecho contra su vida, patrimonio, seguridad personal, etc. Primera opciÃ³n
unique(DATA$encuesta_calidad.p_286)
##Tiene 21 categorías, la  -88 y -99 es sospechosa, qué significa?
##-88 No está idenfticada en la metada, la -99 es no áplica, es error.

#Durante los Ãºltimos doce meses, usted o algÃºn miembro de su hogar ha sido victima de algÃºn hecho contra su vida, patrimonio, seguridad personal, etc. Primera opciÃ³n
unique(DATA$encuesta_calidad.p_287)
##Tiene 21 categorías, la  -88 y -99 es sospechosa, qué significa?
##-88 No está idenfticada en la metada, la -99 es no áplica, es error.

#Denunciaron el hecho ante algun organismo o autoridad competente
unique(DATA$encuesta_calidad.p_288)
##Tiene 5 categorías, la  -88, -98 y -99 es sospechosa, qué significa?
##Corresponden a no sabe, no responde y no áplica, se borran.

#Durante los Ãºltimos doce meses, en su barrio o vereda se han presentado problemas de convivencia? Primera opciÃ³n
unique(DATA$encuesta_calidad.p_289)
##Tiene 15 categorías, la  -88, -98 y -99 es sospechosa, qué significa?
##-98 y -99 Corresponden a no responde y no áplica, -88 no está en la metadata, se borran.

#Durante los Ãºltimos doce meses, en su barrio o vereda se han presentado problemas de convivencia? Segunda opciÃ³n
unique(DATA$encuesta_calidad.p_290)
##Tiene 15 categorías, la  -88, -98 y -99 es sospechosa, qué significa?
##-98 y -99 Corresponden a no responde y no áplica, -88 no está en la metadata, se borran.

#Â¿Cuantas personas que eran miembros de este hogar, han muerto en los Ãºltimos 12 meses?
unique(DATA$encuesta_calidad.p_229)
##Tiene 7 categorías, ningun ae ve sospechosa
##Nada raro, son cantidades de personas mmuertas.

#Â¿QuÃ© tipo de afiliaciÃ³n al sistema de seguridad social en salud tienen los miembros de su familia?
unique(DATA$encuesta_calidad.p_66)
##Tiene 9 categorías,  -98 y -99 se ven sospechosas
##Corresponden a no sabe y no responde, por lo que se eliminan.

#bARRIOS
unique(DATA$encuesta_calidad.barrio)
##Hay 299 barrios

##Con base en lo anterior, toca modificar muchos registros, según lo encontrado

##Yo no tengo nulos
require(editrules)


##CORRECIÓN DE ERRORES EN EL DATASET
##229 no

Cond1<-editset(c("encuesta_calidad.p_258>0","encuesta_calidad.p_272>0",
                 "encuesta_calidad.p_280>0","encuesta_calidad.p_281>0",
                 "encuesta_calidad.p_282>0","encuesta_calidad.p_283>0","encuesta_calidad.p_284>0",
                 "encuesta_calidad.p_285>0","encuesta_calidad.p_286>0","encuesta_calidad.p_287>0",
                 "encuesta_calidad.p_288>0","encuesta_calidad.p_289>0",
                 "encuesta_calidad.p_290>0","encuesta_calidad.p_66>0"))
 

 # "encuesta_calidad.p_307>0",
  #              "encuesta_calidad.p_69>0",
  #              "encuesta_calidad.p_308>0",
  #              "encuesta_calidad.p_69>0",
  #              "encuesta_calidad.p_20>0",
  #              "encuesta_calidad.p_58>0",
  #              "encuesta_calidad.p_66>0",
  #              "encuesta_calidad.p_69>0",
  #              "encuesta_calidad.p_329 %in% c('1','2','3','4','NULL')")
               


Errores1<-violatedEdits(c(Cond1), DATA)



##plot(Errores1)
##Matriz de localización de errores en cada registro. Es una matriz de true o false, según si 
#El registro en alguna columna violó las condiciones establecidas en cond1.

Locali1<-localizeErrors(Cond1,DATA)$adapt
apply(X=Locali1, MARGIN = 2, FUN=function(x)
  which(x==TRUE))

###########  IDENTIFICANDO LOS PROBLEMAS INDIVIDUALMENTE ############
#DATA$encuesta_calidad.p_258

Problem1<-apply(X=Locali1, MARGIN = 2,
                  FUN=function(x)
                    which(x==TRUE))$encuesta_calidad.p_258


Problem2<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_272

Problem3<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_280

Problem4<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_281

Problem5<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_282

Problem6<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_283
Problem7<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_284

Problem8<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_285

Problem9<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_286


Problem10<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_287


Problem11<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_288


Problem12<-apply(X=Locali1, MARGIN = 2,
                 FUN=function(x)
                   which(x==TRUE))$encuesta_calidad.p_289


Problem13<-apply(X=Locali1, MARGIN = 2,
                 FUN=function(x)
                   which(x==TRUE))$encuesta_calidad.p_290


Problem14<-apply(X=Locali1, MARGIN = 2,
                 FUN=function(x)
                   which(x==TRUE))$encuesta_calidad.p_66




# Todos los problemas:
Problems<-Reduce(union,list(Problem1,Problem2,Problem3,
            Problem4,Problem5,Problem6,
            Problem7,Problem8,Problem9,Problem10,Problem11,Problem12,Problem13,Problem14))

################  RESUMEN 

require(dplyr)

##Busquemos porcentaje de error por cada variable, para saber si es mejor desechar alguna:
length(Problem1)/nrow(DATA)
length(Problem2)/nrow(DATA)
length(Problem3)/nrow(DATA)
length(Problem4)/nrow(DATA)
length(Problem5)/nrow(DATA)
length(Problem6)/nrow(DATA)
length(Problem7)/nrow(DATA)
length(Problem8)/nrow(DATA)
length(Problem9)/nrow(DATA)
length(Problem10)/nrow(DATA)##90%, no la voy a usar encuesta_calidad.p_287
length(Problem11)/nrow(DATA)## 91% no la voy a usar encuesta_calidad.p_288
length(Problem12)/nrow(DATA)
length(Problem13)/nrow(DATA)## 77%, no la voy a usar encuesta_calidad.p_290
length(Problem14)/nrow(DATA)

#Pregunta 258
DATA_258<-DATA[-Problem1,
                     c("encuesta_calidad.barrio",
                       "encuesta_calidad.p_258")]
TABLA_258<-round(100*as.matrix(table(DATA_258))/rowSums(as.matrix(table(DATA_258))),2) 

#Pregunta 272
DATA_272<-DATA[-Problem2,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_272")]
TABLA_272<-round(100*as.matrix(table(DATA_272))/rowSums(as.matrix(table(DATA_272))),2) 


#Pregunta 280
DATA_280<-DATA[-Problem3,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_280")]
TABLA_280<-round(100*as.matrix(table(DATA_280))/rowSums(as.matrix(table(DATA_280))),2) 

#Pregunta 281
DATA_281<-DATA[-Problem4,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_281")]
TABLA_281<-round(100*as.matrix(table(DATA_281))/rowSums(as.matrix(table(DATA_281))),2) 

#Pregunta 282
DATA_282<-DATA[-Problem5,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_282")]
TABLA_282<-round(100*as.matrix(table(DATA_282))/rowSums(as.matrix(table(DATA_282))),2)

#Pregunta 283
DATA_283<-DATA[-Problem6,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_283")]
TABLA_283<-round(100*as.matrix(table(DATA_283))/rowSums(as.matrix(table(DATA_283))),2) 


#Pregunta 284
DATA_284<-DATA[-Problem7,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_284")]
TABLA_284<-round(100*as.matrix(table(DATA_284))/rowSums(as.matrix(table(DATA_284))),2) 

#Pregunta 285
DATA_285<-DATA[-Problem8,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_285")]
TABLA_285<-round(100*as.matrix(table(DATA_285))/rowSums(as.matrix(table(DATA_285))),2) 


#Pregunta 286
DATA_286<-DATA[-Problem9,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_286")]
TABLA_286<-round(100*as.matrix(table(DATA_286))/rowSums(as.matrix(table(DATA_286))),2) 




#Pregunta 289
DATA_289<-DATA[-Problem12,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_289")]
TABLA_289<-round(100*as.matrix(table(DATA_289))/rowSums(as.matrix(table(DATA_289))),2) 


#Pregunta 66
DATA_66<-DATA[-Problem14,
               c("encuesta_calidad.barrio",
                 "encuesta_calidad.p_66")]
TABLA_66<-round(100*as.matrix(table(DATA_66))/rowSums(as.matrix(table(DATA_66))),2) 








BARRIOS<-row.names(as.matrix(summary(DATA$encuesta_calidad.barrio,
                  maxsum=1000000)))
TOTAL_ENC<-as.vector(summary(DATA$encuesta_calidad.barrio,
                  maxsum=100000))


TABLA_280
TABLA_258

##Dataframe final
CALIDAD<-data.frame(BARRIOS,TOTAL_ENC,
                    p.258.aumentado=TABLA_258[,1],
                    p.258.igual=TABLA_258[,2],
                    p.258.disminuido=TABLA_258[,3],
                    p.272.muyinsatifecho=TABLA_272[,1],
                    p.272.insatisfecho=TABLA_272[,2],
                    p.272.aceptable=TABLA_272[,3],
                    p.272.satisfecho=TABLA_272[,4],
                    p.272.muysatisfecho=TABLA_272[,5],
                    p.281.muypocalibertad=TABLA_281[,1],
                    p.281.pocalibertad=TABLA_281[,2],
                    p.281.aceptable=TABLA_281[,3],
                    p.281.libertad=TABLA_281[,4],
                    p.281.muchalibertad=TABLA_281[,5],
                    p.282.muypocalibertad=TABLA_282[,1],
                    p.282.pocalibertad=TABLA_282[,2],
                    p.282.aceptable=TABLA_282[,3],
                    p.282.libertad=TABLA_282[,4],
                    p.282.muchalibertad=TABLA_282[,5],
                    p.283.muyseguro=TABLA_283[,1],
                    p.283.seguro=TABLA_283[,2],
                    p.283.inseguro=TABLA_283[,3],
                    p.284.bandasocombos=TABLA_284[,1],
                    p.284.atracoscallejeros=TABLA_284[,2],
                    p.284.roboscarros=TABLA_284[,3],
                    p.284.asaltancasas=TABLA_284[,4],
                    p.284.atracostiendas=TABLA_284[,5],
                    p.284.homicidios=TABLA_284[,6],
                    p.284.traficodedrogas=TABLA_284[,7],
                    p.284.violaciones=TABLA_284[,8],
                    p.284.vandalismoedificaciones=TABLA_284[,9],
                    p.284.milicias=TABLA_284[,10],
                    p.284.extorsiones=TABLA_284[,11],
                    p.284.desaparaciones=TABLA_284[,12],
                    p.284.violenciaintrafamiliar=TABLA_284[,13],
                    p.284.ninguno=TABLA_284[,14],
                    p.285.bandasocombos=TABLA_285[,1],
                    p.285.atracoscallejeros=TABLA_285[,2],
                    p.285.roboscarros=TABLA_285[,3],
                    p.285.asaltancasas=TABLA_285[,4],
                    p.285.atracostiendas=TABLA_285[,5],
                    p.285.homicidios=TABLA_285[,6],
                    p.285.traficodedrogas=TABLA_285[,7],
                    p.285.violaciones=TABLA_285[,8],
                    p.285.vandalismoedificaciones=TABLA_285[,9],
                    p.285.milicias=TABLA_285[,10],
                    p.285.extorsiones=TABLA_285[,11],
                    p.285.desaparaciones=TABLA_285[,12],
                    p.285.violenciaintrafamiliar=TABLA_285[,13],
                    p.285.ninguno=TABLA_285[,14],
                    p.286.carterismo=TABLA_286[,1],
                    p.286.hurtopartesvehiculo=TABLA_286[,2],
                    p.286.atraco=TABLA_286[,3],
                    p.286.amenazas=TABLA_286[,4],
                    p.286.hurtoresidencias=TABLA_286[,5],
                    p.286.hurtovehiculos=TABLA_286[,6],
                    p.286.hurtovehiculosrepartidores=TABLA_286[,7],
                    p.286.hurtocomercio=TABLA_286[,8],
                    p.286.hurtoempresas=TABLA_286[,9],
                    p.286.cobrovacunas=TABLA_286[,10],
                    p.286.homicidios=TABLA_286[,11],
                    p.286.accidentetransito=TABLA_286[,12],
                    p.286.secuestro=TABLA_286[,13],
                    p.286.paseomillonario=TABLA_286[,14],
                    p.286.robocajeroelectronico=TABLA_286[,15],
                    p.286.violenciamaltratofamiliar=TABLA_286[,16],
                    p.286.violaciones=TABLA_286[,17],
                    p.286.estafafraude=TABLA_286[,18],
                    p.286.ninguno=TABLA_286[,19],
                    p.289.problemasruidosbasuras=TABLA_289[,1],
                    p.289.problemaslinderos=TABLA_289[,2],
                    p.289.espaciopublico=TABLA_289[,3],
                    p.289.rinascallejeras=TABLA_289[,4],
                    p.289.rinasfamiliares=TABLA_289[,5],
                    p.289rinascentroeducativo=TABLA_289[,6],
                    p.289.rinastrabajo=TABLA_289[,7],
                    p.289.tiroteos=TABLA_289[,8],
                    p.289.destruccionbienraiz=TABLA_289[,9],
                    p.289.drogas=TABLA_289[,10],
                    p.289.ninguno=TABLA_289[,11],
                    p.289.discriminacionoriengenetn=TABLA_289[,12]
                    )

                    
                    # P.307.SI=TABLA_307[,1],
                    # P.69.1=TABLA_69[,1],P.69.2=TABLA_69[,2],
                    # P.69.3=TABLA_69[,3],P.69.4=TABLA_69[,4],
                    # P.69.5=TABLA_69[,5],P.69.6=TABLA_69[,6],
                    # P.69.7=TABLA_69[,7],P.69.8=TABLA_69[,8],
                    # P.329.1=TABLA_329[,4],P.329.2=TABLA_329[,5],
                    # P.329.3=TABLA_329[,6],P.329.4=TABLA_329[,7],
                    # P.329.NULL=TABLA_329[,8],P.18.MEDIAN=RESUM_18$Median_Edad)

# Tener cuidado con La Ilusion en la 150
#CALIDAD[150,]



########################################DATAFRAME LISTO########################################
##TENGO 94 Variables, hay que depurar:
dim(CALIDAD)
losbarrios<-CALIDAD$BARRIOS
Datafinal <- CALIDAD[-(1:2)]

dim(Datafinal)

##Tengo 92 variables. Se sacó barrios que es el index y total por barrio que no es necesario tenerlo.

#Es necesario realizar un analisis descriptivo de las variables, para entender mejor el comportamiento de los datos, así como
#para observar patrones o datos irrelevantes dentro del conjunto, ya que se tienen 92 features.


##Busquedda de NA:
sum(is.na(Datafinal)) #tengi 14 NA

##Quitar los NA, por un método más directo, usando la librería imputeTS, que ofrece un rapido reemplazo de los NA
#En este caso por la media. Sugerencia tomada de stackoverflow.
#install.packages("imputeTS")
library(imputeTS)
Datafinal<-na_mean(Datafinal) ##https://stackoverflow.com/questions/25835643/replace-missing-values-with-column-mean

sum(is.na(Datafinal)) ##Ahora son cero


##Por medio de exploración visual, se encontró que hay varias columnas donde la mayoría de sus entradas son ceros, lo
#cual no representa ningún tipo de información, en un caso donde representaria la proporción sobre
#un barrio en especifico. Explorar el porcentaje de ceros:

##Codigo para borrar variables con alta cantidad de ceros. Autoría propia 

for (i in names(Datafinal)) {
  #ceros=length(which(Datafinal$i==0))/length(Datafinal$i)
  columna<-Datafinal[[i]]
  ceros=length(which(columna==0))/length(columna)
  if(ceros>0.83){
    Datafinal<-select(Datafinal,-c(i))
  }
  
  }
  

###ANÁLISIS DESCRIPTIVO:
#p.258.aumentado
#Se puede ver como en el rango de proporciones de 0 a 100, solo hay un barrio por cada valor extremo. Hay un barrio donde nadie cree
#que el aumento de viviendas nuevas ha aumentado. Por el contrario solo hay un barrio donde todas las personas creen que la venta de viviendas
#Nuevas han aumentado. La mayoría de valores se concentran en el rango del 30% y el 70%. Es decir, en la mayoría de los barrios,
#entre el 30 y el 70% de las personas creen que la venta de viviendas nuevas ha aumentado.
table(Datafinal$p.258.aumentado)
#Con esta información de outlier se confirma lo que se pensó anteriormente.
boxplot.stats(Datafinal$p.258.aumentado)

#p.258.igual
#Se puede observar que contrario a la pregunta anterior, no existe un barrio donde el 100% de las personas considere que la venta de viviendas
#Nuevas esté igual en los últimos 12 meses. Sigue existiendo un barrio en el cual nadie considera esto. Llama la atención el salto que hay
#entre el 80 y el 90%, dejando ver que no hay un barrio que en ese rango las personas crean que sigue igual la venta de vivienda
#nueva.

table(Datafinal$p.258.igual)
#Con esta información de outlier se confirma lo que se pensó anteriormente.
boxplot.stats(Datafinal$p.258.igual)

#p.258.disminuido
#Panorama diferente se observa en esta variable, donde hay 22 barrios en los que nadie cree que la venta de viviendas nuevas disminuyó.
#Los datos muestran que en la gran mayoría de barrios muy poca gente cree que la venta de vivienda nueva disminuyó en los últimos 12 meses,
#Pues se ve como entre 30% y 100% hay datos casi inexistentes.
table(Datafinal$p.258.disminuido)
#Con esta información de outlier se confirma lo que se pensó anteriormente.
boxplot.stats(Datafinal$p.258.disminuido)


#p.272.muyinsatifecho Nivel de sarisfaccion con el municipio.
##Se deciden eliminar las relacionada a esta pregunta, ya que está por municipio y el estudio finalmente se desea hacer es a nivel
#de barrio.
#En general, la gran mayoría de barrios no se sienten insatisfechos con su municipio

Datafinal<-select(Datafinal,-c("p.272.muyinsatifecho","p.272.insatisfecho","p.272.aceptable","p.272.satisfecho","p.272.muysatisfecho"))

##PREGUNTA 280, METERLA?????


##p.281 libertad para moverse libremente de un lado a otro en su barrio:

# muy Poca libertad:
table(Datafinal$p.281.muypocalibertad)
#Se puede notar en la tabla como a nivel general la perceción de tener muy poca libertad para moverse en los barrios, es baja.
#Los valores suben máximo a 25% en un único barrio (podría ser el que tiene más problematicas sociales). Pero en general los valores
#están bajos, incluso hay 46 barrios con el 0%.
#Con esta información de outlier se confirma lo que se pensó anteriormente.
boxplot.stats(Datafinal$p.281.muypocalibertad)

#  Poca libertad:
table(Datafinal$p.281.pocalibertad)
#En esta categoría de poca libertad, los datos siguen estando generalmente bajos. Hay 21 barrios donde la proporción es del 0%, sube
#máximo hasta el 50%, en 2 casos. Generalmente los datos están entre el 4 y el 19%. En resumidas cuentas, en casi todos los barrios, la 
#genete no tiene la percecpción de que  hay poca libertad para moverse. 
boxplot.stats(Datafinal$p.281.pocalibertad)

#  Acetable libertad:
table(Datafinal$p.281.aceptable)
#En esta categoria los datos se empiezan a elevar, pues es una sensación más agradable para el ciudadano. Es el punto intermedio
#entre sentirse seguro e inseguro, que es donde normalmente deberían de clasificarse más lugares. En este punto la cantidad de barrios
#en donde nadie se siente con libertad aceptable, baja considerablemente. El barrio que más siente este es solo uno, en el cual el 60.75% de la
#gente se siente aceptable.
boxplot.stats(Datafinal$p.281.aceptable)


#  Hay libertad:
table(Datafinal$p.281.libertad)
#En esta categoría que expresa que los habitantes se sienten cómodos moviendose dentro de su barrio, los valores aumentaron notoriamente.
#Se puede ver como solo en 2 barrios nadie se siente con buena libertad para moverse. Aparece por primeravez en un barrio donde el 100% de su gente
# se siente con libertad para moverse. Hay valoes outlier en 18%, un solo barrio. Generalmente los valores están por encima del 40%.
boxplot.stats(Datafinal$p.281.libertad)



#  mucha libertad:
table(Datafinal$p.281.muchalibertad)
#Este categoría representa el punto máximo de libertad y como era de esperarse los datos cambian respecto a la anteriro categoría.
#En ningún barrio toda la gente tiene esta sensación. De hecho, el máximo valor que alcanza un único barrio es el 55.7% de la proporción de habitantes
#Con este pensamiento. La gran mayoría de barrios se concentran por debajo del 30%.
boxplot.stats(Datafinal$p.281.muchalibertad)


##Pregunta 282 tienen que ver con sentires politicos, se decide no tener en cuenta, ya que no está tan relacionada respecto al tema de la seguridad.
Datafinal<-select(Datafinal,-c("p.282.muypocalibertad","p.282.pocalibertad","p.282.aceptable","p.282.libertad","p.282.muchalibertad"))


##Pregunta 283, es de las más importantes en este análisis de seguridad: Como se siente en el barrio o vereda donde vive?

#Muy seguro
table(Datafinal$p.283.muyseguro)

#Esta categoría deja ver mucha parte del pensamiento de las personas, en un aspecto muy importante para este análisis de seguridad.
#En el barrio donde la gente más siente esta sensación de mucha seguridd, la siente solo el 60% de ella. En la gran mayoría de barrios los valores son
#Muy pqueños, por debajo del 25%. Incluso hay 9 barrios donde nadie se siente muy seguro. En general, muy pocos se sienten muy seguros.
boxplot.stats(Datafinal$p.283.muyseguro)

# seguro
table(Datafinal$p.283.seguro)

#Los datos en esta categoria se presentan de una forma más positiva, en donde la gran mayoría de gente tiene la percepción de
#que su barrio es seguro. Hay unos valores atipicos al rededor del 25%, pero pasan rapidamente al 40%, en donde sigue creciendo con
#mucha más proporción hasta el 100%. Inlcuso hay 4 barrios en donde el 100% de las personas se sienten seguras. Y hay muchos valores
#distribuidos por arriba del 80%.
boxplot.stats(Datafinal$p.283.seguro)


# inseguro
table(Datafinal$p.283.inseguro)

#Los valores en esta categoría se mantienen bajos, pero llama la atencion 2 barrios en especifico en los cuales los valores estan por el
#70%, es decir que allí el 70% de su gente se siente insegura. En general la percepción es buena, hay 11 barrios donde nadie se siente in
#seguro. 
boxplot.stats(Datafinal$p.283.inseguro)


##Pregunta 284
#Muy importante también para este análisis:
#CuÃ¡les son los dos problemas mÃ¡s graves en orden de importancia para usted en relaciÃ³n con
#la seguridad que se presenta en su barrio , corregimiento o vereda? Primera opciÃ³n

#Asalto de casas
table(Datafinal$p.284.asaltancasas)

#Es de las problematicas menos sentidad en todos los barrios, incluso en 67 barrios es del 0%, donde más se siente es solo del 14%
boxplot.stats(Datafinal$p.284.asaltancasas)


#Bandas y combos callejeros
table(Datafinal$p.284.bandasocombos)

#Esta percepción de problema sí es en general más grande, donde se ven incluso valores de alrededor del 60%. Sin embargo no hay un solo
#barrio donde todos los ciudadanos estén de acuerdo que este es el principal problema.
boxplot.stats(Datafinal$p.284.bandasocombos)

#Atracos callejeros
table(Datafinal$p.284.atracoscallejeros)
#Llama la atención que hay muchos barrios en los cuales nadie cree que este sea el principal problem (20 barrios). El barrio donde más sienten esto
#Es solo 1 con el 75$, que es un valor muy puntual y alejado de los demás. La mayoria de valores se encuentran por debajo del 50%
boxplot.stats(Datafinal$p.284.atracoscallejeros)

#Homicidios: 
table(Datafinal$p.284.homicidios)
#Los homicidios no son el sentir más grande en toda la población en general. Hay 126 barrios en los cuales todas las personas dicen que este no es el
#problema más grande y el máximo valor que alcanza es 6.5%. Muy baja esta percepción en general.
boxplot.stats(Datafinal$p.284.homicidios)

#Trafico de drgoas: 
table(Datafinal$p.284.traficodedrogas)
#Los datos en todos los barrios están por debajo del 40%, hay un valor átipico en 56%, pero en general esta percepción es baja. La gente no cree que sea lo más grave
boxplot.stats(Datafinal$p.284.traficodedrogas)

#Violaciones:
table(Datafinal$p.284.violaciones)
#Deifntivamnte la gente no lo considra lo más grave. Hay 255 barrios donde todos piensan que no es lo más grave.
boxplot.stats(Datafinal$p.284.violaciones)

###################PREGUNTA 286, IMPORTANTE###########################

#Durante los Ãºltimos doce meses, usted o algÃºn miembro de su hogar ha sido victima de algÃºn hecho contra su vida, 
#patrimonio, seguridad personal, etc. Primera opciÃ³n

##Se muestra en general el hecho que más y menos siente la población en su primera opción:
##atraco:
table(Datafinal$p.286.atraco)
#Es lo que en general más ha sentido la gente de los barrios. Tampoco hay un opinión compartita totalmente en donde en algún barrio
#más del 30 % de la gente opine esto
boxplot.stats(Datafinal$p.286.atraco)


#hurtocomercio:

table(Datafinal$p.286.hurtocomercio)
#En 236 barrios el 100% de las personas piensa que este no es el problema que más han vivido durante el último año.
boxplot.stats(Datafinal$p.286.hurtocomercio)



#violaciones
table(Datafinal$p.286.violaciones)
#En 233 barrios el 100% de las personas piensa que este no es el problema que más han vivido durante el último año. El máximo valor que toma
#es el 0,97% en un solo barrio.
boxplot.stats(Datafinal$p.286.violaciones)



Datafinalescalada<-as.data.frame(scale(Datafinal))

library(FactoClass)
library(ggplot2)
library(factoextra)

set.seed(5651)
# function to compute total within-cluster sum of squares
fviz_nbclust(Datafinalescalada, kmeans, method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")
##TOMADO DE https://www.r-bloggers.com/10-tips-for-choosing-the-optimal-number-of-clusters/
##Se puede observar que el número óptimo de clusters está entre 4 y 5, acá es donde el cambio empieza a bajar lentamente
#Se toma con k=5

##Semilla para cluster:
set.seed(239) ##323
kmeansfirs<-kmeans(Datafinalescalada, centers=5)
kmeansfirs$cluster


##Nuevo dataFrame con el respectivo cluster añadido
clusterizacion<-data.frame(cbind(Datafinal), cluster=kmeansfirs$cluster)


fviz_cluster(kmeansfirs, data = Datafinalescalada) + theme_minimal() + ggtitle("k = 5")
##Se logra observar que hay un cluster que queda bastante diferencias de los demás. Así mismo, se observa que
##Los otros 4 grupos están un poco más cercanos.

##Obtengamos los clusters:
cluster1<-clusterizacion[clusterizacion$cluster==1,]
length(cluster1)
cluster2<-clusterizacion[clusterizacion$cluster==2,]
length(cluster2)
cluster3<-clusterizacion[clusterizacion$cluster==3,]
length(cluster3)
cluster4<-clusterizacion[clusterizacion$cluster==4,]

cluster5<-clusterizacion[clusterizacion$cluster==5,]


install.packages("leaflet")
library(rgdal)
library(leaflet)


for(i in 1: nrow(clusterizacion)){
  if(clusterizacion$cluster[i]==1){clusterizacion$COLOR[i] <- "yellow"}
  if(clusterizacion$cluster[i]==2){clusterizacion$COLOR[i] <- "red"}
  if(clusterizacion$cluster[i]==3){clusterizacion$COLOR[i] <- "blue"}
  if(clusterizacion$cluster[i]==4){clusterizacion$COLOR[i] <- "black"}
  if(clusterizacion$cluster[i]==5){clusterizacion$COLOR[i] <- "green"}
  
}

clusterizacion$BARRIOS=losbarrios
clusterizacion$COLOR


DATOS <- data.frame(clusterizacion[,c(1:67)])
names(DATOS)[67]="NOMBRE"


DATOS$NOMBRE <- chartr('ÁÉÍÓÚ','AEIOU', DATOS$NOMBRE)
DATOS[1:3,]


library(ggplot2)
library(rgdal)
library(sp)
library(leaflet)

barrios_med=readOGR("C:/Users/Gonza/Desktop/Noveno semestre/TAE/trabajo1/Barrios de Medellín/Barrio_vereda.shp")
nombres_barrios=iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")

library(stringr)

barrios_med@data$NOMBRE  <- str_to_upper(nombres_barrios, locale = "es")
barrios_med@data$NOMBRE <- chartr('ÁÉÍÓÚ','AEIOU', barrios_med@data$NOMBRE)
library(dplyr)

#barrios_med@data$NOMBRE <- arrange(barrios_med@data, NOMBRE)
DATOS <- arrange(DATOS, NOMBRE)

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
barrios_med@data$COLOR<-rep("white",332)

for (i in 1:332) {
  for (j in 1:297) {
    if(barrios_med@data$NOMBRE[i]==DATOS$NOMBRE[j]){
      barrios_med@data$COLOR[i] <- DATOS$COLOR[j]
    }
    
  }
  
}


m=leaflet(barrios_med)
m=addTiles(m)
m


#colores=sample(x=c("orange","green","yellow"),size=length(nombres_barrios),replace=TRUE)

m=addPolygons(m,popup=nombres_barrios,color=barrios_med@data$COLOR)
m  


cluster1.df<-sapply(cluster1,FUN=mean)
cluster1.df<-as.data.frame(cluster1.df)



##El rojo es el cluster 2

cluster2.df<-sapply(cluster2,FUN=mean)
cluster2.df<-as.data.frame(cluster2.df)

length(cluster4)


cluster4.df<-sapply(cluster4,FUN=mean)
cluster4.df<-as.data.frame(cluster4.df)

cluster3.df<-sapply(cluster3,FUN=mean)
cluster3.df<-as.data.frame(cluster3.df)


cluster5.df<-sapply(cluster5,FUN=mean)
cluster5.df<-as.data.frame(cluster5.df)


barrios_med_dff<-as.data.frame(barrios_med)

summary(cluster1)
##Tengo los barrios (299) y nombre_barrios (336), hay que parear.

# 
# barrios_med_datafreame=as.data.frame(barrios_med)
# barriosformato=as.data.frame(nombres_barrios)
# 
# ##Debo reemplazar la columna de nombres del dataframe, con barriosformato
# 
# 
# ##Listo, ya tengo el dataframe con los nombres bien codificados.
# barrios_med_datafreame$NOMBRE=barriosformato$nombres_barrios
# ##Listo, ya tengo el dataframe con los nombres bien codificados.
# 
# ##nombres de la BD mía. Todo está en mayuscula, toca pasar todo a minuscula.
# barriospropio=as.data.frame(BARRIOS)
# barriospropio$BARRIOS=tolower(barriospropio$BARRIOS)
# barrios_med_datafreame$NOMBRE=tolower(barrios_med_datafreame$NOMBRE)
# barriospropio$BARRIOS=sub("nº ","no.",barriospropio$BARRIOS)
# 
# 
# 
# 
# require(sqldf)
# 
# ##Renombrar para hacer un join
# barriospropio<-barriospropio %>% 
#   rename(
#     "NOMBRE"  =BARRIOS,
#     
#   )
# 
# 
# ##Añadamosle a barriospropio la columna color, en el cual va a ir el cluster
# barriospropio$color=clusterizacion$cluster
# 
# ##Hagamos un left JOIN, poniedo a la izquierda la base grande
# 
# dfmerge<-merge(x=barrios_med_datafreame,y=barriospropio,by="NOMBRE",all.x=TRUE)
# 
# 
# sum(is.na(dfmerge$color)) ##79 sin match
# ###Al final yo quiero es graficar respecto a la BD del profesor, que es la que conoce las dimensiones y eso.
# ###utilizo los barrios que yo tengo y darles color según cluster.
# 
# ##Arreglar problema de diferencía de nombres. Con ingeniería de Strings.
# ##Hay que hacer coincidir los nombres.
# ##Hay que cuadrar los colores.
# 
# #Manos a la obra.
# 
# 
# ##ACA TENGO LOS NOMBRES, QUE VIENEN DE LA BASE DE DATOS DEL PROFESOR Y NO HUBO MATCH
# ##CON BARRIOSPROPIO. BUSQUEDA CARÁCTER POR CARÁCTER PARA TRARAR DE HALLAR MÁS POSIBILIDADES
# nohubomatch=as.matrix(subset(dfmerge$NOMBRE, is.na(dfmerge[,7] )))
# 
# for(barriosinmatch in nohubomatch){
#   
#   for (barrioabuscar in barriospropio$NOMBRE){
#     for (letrabuscar in strsplit(barriosinmatch, "")[[1]]){
#       print(letrabuscar)
#       
#     }
#   }
#   
# }
# 
# #################################ARREGLAR ESTA CUESTIÓN EN PYTHON, SIMPLE. No voy a pelear más con
# ##esto.
# 
# 
# 
# m=leaflet(barrios_med)
# m=addTiles(m)
# m
# 
# m=addPolygons(m,popup=nombres_barrios)
# m=addTiles(m)
# m
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# colores=sample(x=c("orange","green","yellow"),size=length(nombres_barrios),replace=TRUE)
# ## Warning in sample.int(length(x), size, replace, prob): '.Random.seed' is
# ## not an integer vector but of type 'NULL', so ignored
# #Ahora veamos el mapa con estos colores:
# m=addPolygons(m,popup=nombres_barrios,color=colores)
# m
# # BARRIOS=as.data.frame(BARRIOS)
# # nombres_barrios=as.data.frame(nombres_barrios)
# # BARRIOS$BARRIOS=tolower(BARRIOS$BARRIOS)
# # BARRIOS$BARRIOS=sub(" ","",BARRIOS$BARRIOS)
# 
# 
# 
# ###Sisas, información de coordenadas. Tengo los nombres. Debo gráficar con respecto a la BD del profe. Seleccionar los barrios que
# # yo tengo.
# 
# ###Los nombres de las bases de datos difieren mucho.
# 
# 
# 
# ##Listo, ya tengo todos los barrios en minuscula y sin espacios.
# nombres_barrios$nombres_barrios=tolower(nombres_barrios$nombres_barrios)
# nombres_barrios$nombres_barrios=sub(" ","",nombres_barrios$nombres_barrios)
# #BARRIOS$BARRIOS="hola"
# 
# install.packages("sqldf")
# require(sqldf)
# 
# ##Renombrar para hacer un join
# nombres_barrios<-nombres_barrios %>% 
#   rename(
#     "BARRIOS"  =nombres_barrios,
#     
#   )
# 
# 
# 
# 
# semi_join(BARRIOS,nombres_barrios)
