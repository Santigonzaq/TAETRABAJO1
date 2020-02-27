load("C:/Users/Usuario/Documents/UNIVERSIDAD/2019-2/TAE/Trabajo TAE/CAL_VID_2019.RData")

vars<-c("encuesta_calidad.barrio","encuesta_calidad.comuna",
        "encuesta_calidad.p_18",
        "encuesta_calidad.p_10",
        "encuesta_calidad.p_15",
        "encuesta_calidad.p_20",
        "encuesta_calidad.p_36",
        "encuesta_calidad.p_58",
        "encuesta_calidad.p_66",
        "encuesta_calidad.p_69",
        "encuesta_calidad.p_307",
        "encuesta_calidad.p_308",
        "encuesta_calidad.p_329")

DAT_MUJ<-CAL_VID_2019[CAL_VID_2019$encuesta_calidad.p_15==2,vars]

names(DAT_MUJ)
dim(DAT_MUJ)
head(DAT_MUJ)



require(editrules)
Cond1<-editset(c("encuesta_calidad.p_307>0",
                 "encuesta_calidad.p_69>0",
                 "encuesta_calidad.p_308>0",
                 "encuesta_calidad.p_20>0",
                 "encuesta_calidad.p_58>0",
                 "encuesta_calidad.p_66>0",
                 "encuesta_calidad.p_329 %in% c('1','2','3','4','NULL')")
)

Errores1<-violatedEdits(c(Cond1), DAT_MUJ)

plot(Errores1)

Locali1<-localizeErrors(Cond1,DAT_MUJ)$adapt
apply(X=Locali1, MARGIN = 2, FUN=function(x)
  which(x==TRUE))

###########  IDENTIFICANDO LOS PROBLEMAS INDIVIDUALMENTE ############


Problem1<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_307

Problem2<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_69

Problem3<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_308

Problem4<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_20

Problem5<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_58

Problem6<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_66


Problem7<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_329

Problem8<-apply(X=Locali1, MARGIN = 2,
                FUN=function(x)
                  which(x==TRUE))$encuesta_calidad.p_10


# Todos los problemas:
Problems<-Reduce(union,list(Problem1,Problem2,Problem3,
                            Problem4,Problem5,Problem6,
                            Problem7, Problem8))

################  RESUMEN 

require(dplyr)

# PREGUNTA 307:
length(Problem1)/nrow(DAT_MUJ) # % PROBLEMA CON 307
DAT_MUJ_307<-DAT_MUJ[-Problem1,
                     c("encuesta_calidad.barrio",
                       "encuesta_calidad.p_307")]
TABLA_307<-round(100*as.matrix(table(DAT_MUJ_307))/rowSums(as.matrix(table(DAT_MUJ_307))),2) 

# PREGUNTA 10:

length(Problem8)/nrow(DAT_MUJ) # % PROBLEMA CON 10
DAT_MUJ_10<-DAT_MUJ[c("encuesta_calidad.barrio",
                       "encuesta_calidad.p_10")]
TABLA_10<-round(100*as.matrix(table(DAT_MUJ_10))/rowSums(as.matrix(table(DAT_MUJ_10))),2) 


## PREGUNTA 308
length(Problem3)/nrow(DAT_MUJ)
DAT_MUJ_308<-DAT_MUJ[-Problem3,
                     c("encuesta_calidad.barrio",
                       "encuesta_calidad.p_308")]
TABLA_308<-round(100*as.matrix(table(DAT_MUJ_308))/rowSums(as.matrix(table(DAT_MUJ_308))),2) 



# PREGUNTA 69:
length(Problem2)/nrow(DAT_MUJ) # % PROBLEMA CON 69
DAT_MUJ_69<-DAT_MUJ[-Problem2,
                    c("encuesta_calidad.barrio",
                      "encuesta_calidad.p_69")]
TABLA_69<-round(100*as.matrix(table(DAT_MUJ_69))/rowSums(as.matrix(table(DAT_MUJ_69))),2) 


# PREGUNTA 329:
length(Problem7)/nrow(DAT_MUJ) # % PROBLEMA CON 329
#DAT_MUJ_329<-droplevels(DAT_MUJ[-Problem9,
#                    c("encuesta_calidad.barrio",
#                      "encuesta_calidad.p_329")])
DAT_MUJ_329<-DAT_MUJ[-Problem7,
                     c("encuesta_calidad.barrio",
                       "encuesta_calidad.p_329")]

TABLA_329<-round(100*as.matrix(table(DAT_MUJ_329))/rowSums(as.matrix(table(DAT_MUJ_329))),2) 


#PREGUNTA 20:
length(Problem4)/nrow(DAT_MUJ) # % PROBLEMA CON 20
DAT_MUJ_20<-DAT_MUJ[-Problem4,
                    c("encuesta_calidad.barrio",
                      "encuesta_calidad.p_20")]
TABLA_20<-round(100*as.matrix(table(DAT_MUJ_20))/rowSums(as.matrix(table(DAT_MUJ_20)),2)) 

# PROBLEMA 58:
length(Problem5)/nrow(DAT_MUJ) # % PROBLEMA CON 20
DAT_MUJ_58<-DAT_MUJ[-Problem5,
                    c("encuesta_calidad.barrio",
                      "encuesta_calidad.p_58")]

RESUM_58<-DAT_MUJ_58 %>% group_by(encuesta_calidad.barrio) %>% summarise(Median_Hijos=round(median(encuesta_calidad.p_58)),2)
class(RESUM_58)


##PREGUNTA 66:
length(Problem6)/nrow(DAT_MUJ) # % PROBLEMA CON 69
DAT_MUJ_66<-DAT_MUJ[-Problem6,
                    c("encuesta_calidad.barrio",
                      "encuesta_calidad.p_66")]
TABLA_66<-round(100*as.matrix(table(DAT_MUJ_66))/rowSums(as.matrix(table(DAT_MUJ_66))),2) 

## PREGUNTA 36 :
0/nrow(DAT_MUJ) # % PROBLEMA CON 36
DAT_MUJ_36<-DAT_MUJ[c("encuesta_calidad.barrio",
                       "encuesta_calidad.p_36")]
TABLA_36<-round(100*as.matrix(table(DAT_MUJ_36))/rowSums(as.matrix(table(DAT_MUJ_36))),2)



##############   CONSTRUYENDO EL DATA FRAME  #######


BARRIOS<-row.names(as.matrix(summary(DAT_MUJ$encuesta_calidad.barrio,
                                     maxsum=100000)))

TOTAL_ENC<-as.vector(summary(DAT_MUJ$encuesta_calidad.barrio,
                             maxsum=100000))

CALIDAD<-data.frame(BARRIOS,TOTAL_ENC,P.307.SI=TABLA_307[,1], P.308.SI=TABLA_308[,1],
                    P.10.1=TABLA_10[,1],P.10.2=TABLA_10[,2],P.10.3=TABLA_10[,3],
                    P.10.4=TABLA_10[,4],P.10.5=TABLA_10[,5],P.10.6=TABLA_10[,6],
                    P.69.1=TABLA_69[,1],P.69.2=TABLA_69[,2],
                    P.69.3=TABLA_69[,3],P.69.4=TABLA_69[,4],
                    P.69.5=TABLA_69[,5],P.69.6=TABLA_69[,6],
                    P.69.7=TABLA_69[,7],P.69.8=TABLA_69[,8],
                    P.329.1=TABLA_329[,4],P.329.2=TABLA_329[,5],
                    P.329.3=TABLA_329[,6],P.329.4=TABLA_329[,7],
                    P.329.NULL=TABLA_329[,8],P.20.1=TABLA_20[,1],
                    P.20.2=TABLA_20[,2],P.20.3=TABLA_20[,3],
                    P.20.4=TABLA_20[,4],P.20.5=TABLA_20[,5],P.20.6=TABLA_20[,6],
                    P.58.MEDIAN=RESUM_58$Median_Hijos, P.66.1=TABLA_66[,1], 
                    P.66.2=TABLA_66[,2],P.66.3=TABLA_66[,3],P.66.4=TABLA_66[,4],
                    P.66.5=TABLA_66[,5],P.66.6=TABLA_66[,6],P.66.7=TABLA_66[,7],
                    P.36.SI=TABLA_36[,1])

# Tener cuidado con La Ilusion en la 150
#CALIDAD[150,]


CALIDAD[1:3,]


