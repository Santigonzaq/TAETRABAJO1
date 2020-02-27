datosC <- data.frame(cbind(na.omit(CALIDAD), cluster = km.res$cluster))
head(datosC)



grupo1 <- datosC[datosC$cluster==1,]
grupo2 <- datosC[datosC$cluster==2,]
grupo3 <- datosC[datosC$cluster==3,]
grupo4 <- datosC[datosC$cluster==4,]
grupo5 <- datosC[datosC$cluster==5,]
grupo6 <- datosC[datosC$cluster==6,]

###estrato + estratos mayores p_10 4,5,6
### estado civil +  2,5,6 p_20
### estudia p_36 + 
## hijos p_58 mayores que la mediana - menos +
## p_66 tipo de afiliacion + 1-5 6 y 7  -
### actividad p_69 + 1,3,4,5,6,7,8
### p_307 + 
### p_308 +
### p_329 +para 3- 4

###GRUPO 1


GRUPO_1 <- data.frame(P.307.SI=grupo1$P.307.SI, P.308.SI=grupo1$P.308.SI,P.10.1=(grupo1$P.10.1)*(-1),
                       P.10.2=(grupo1$P.10.2)*(-1),P.10.3=(grupo1$P.10.3)*(-1),P.10.4=grupo1$P.10.4,
                       P.10.5=grupo1$P.10.5,P.10.6=grupo1$P.10.6,P.69.1=grupo1$P.69.1,P.69.2=(grupo1$P.69.2)*(-1),
                       P.69.3=grupo1$P.69.3,P.69.4=grupo1$P.69.4,P.69.5=grupo1$P.69.5,P.69.6=grupo1$P.69.6,
                       P.69.7=grupo1$P.69.7,P.69.8=grupo1$P.69.8,P.329.1=(grupo1$P.329.1)*(-1),P.329.2=(grupo1$P.329.2)*(-1),
                       P.329.3=grupo1$P.329.3,P.329.4=grupo1$P.329.4,P.329.NULL=(grupo1$P.329.NULL)*(-1),P.20.1=(grupo1$P.20.1)*(-1),
                       P.20.2=grupo1$P.20.2,P.20.3=(grupo1$P.20.3)*(-1),P.20.4=(grupo1$P.20.4)*(-1),P.20.5=grupo1$P.20.5,
                       P.20.6=grupo1$P.20.6,P.66.1=grupo1$P.66.1,P.66.2=grupo1$P.66.2,P.66.3=grupo1$P.66.3,
                       P.66.4=grupo1$P.66.4,P.66.5=grupo1$P.66.5,P.66.6=(grupo1$P.66.6)*(-1),P.66.7=(grupo1$P.66.7)*(-1),
                       P.36.SI=grupo1$P.36.SI)

apply(GRUPO_1,2,mean)
sum(apply(GRUPO_1,2,mean))/35

GRUPO_2 <- data.frame(P.307.SI=grupo2$P.307.SI, P.308.SI=grupo2$P.308.SI,P.10.1=(grupo2$P.10.1)*(-1),
                      P.10.2=(grupo2$P.10.2)*(-1),P.10.3=(grupo2$P.10.3)*(-1),P.10.4=grupo2$P.10.4,
                      P.10.5=grupo2$P.10.5,P.10.6=grupo2$P.10.6,P.69.1=grupo2$P.69.1,P.69.2=(grupo2$P.69.2)*(-1),
                      P.69.3=grupo2$P.69.3,P.69.4=grupo2$P.69.4,P.69.5=grupo2$P.69.5,P.69.6=grupo2$P.69.6,
                      P.69.7=grupo2$P.69.7,P.69.8=grupo2$P.69.8,P.329.1=(grupo2$P.329.1)*(-1),P.329.2=(grupo2$P.329.2)*(-1),
                      P.329.3=grupo2$P.329.3,P.329.4=grupo2$P.329.4,P.329.NULL=(grupo2$P.329.NULL)*(-1),P.20.1=(grupo2$P.20.1)*(-1),
                      P.20.2=grupo2$P.20.2,P.20.3=(grupo2$P.20.3)*(-1),P.20.4=(grupo2$P.20.4)*(-1),P.20.5=grupo2$P.20.5,
                      P.20.6=grupo2$P.20.6,P.66.1=grupo2$P.66.1,P.66.2=grupo2$P.66.2,P.66.3=grupo2$P.66.3,
                      P.66.4=grupo2$P.66.4,P.66.5=grupo2$P.66.5,P.66.6=(grupo2$P.66.6)*(-1),P.66.7=(grupo2$P.66.7)*(-1),
                      P.36.SI=grupo2$P.36.SI)  
  
apply(GRUPO_2,2,mean)
sum(apply(GRUPO_2,2,mean))/35  
  
GRUPO_3 <- data.frame(P.307.SI=grupo3$P.307.SI, P.308.SI=grupo3$P.308.SI,P.10.1=(grupo3$P.10.1)*(-1),
                      P.10.2=(grupo3$P.10.2)*(-1),P.10.3=(grupo3$P.10.3)*(-1),P.10.4=grupo3$P.10.4,
                      P.10.5=grupo3$P.10.5,P.10.6=grupo3$P.10.6,P.69.1=grupo3$P.69.1,P.69.2=(grupo3$P.69.2)*(-1),
                      P.69.3=grupo3$P.69.3,P.69.4=grupo3$P.69.4,P.69.5=grupo3$P.69.5,P.69.6=grupo3$P.69.6,
                      P.69.7=grupo3$P.69.7,P.69.8=grupo3$P.69.8,P.329.1=(grupo3$P.329.1)*(-1),P.329.2=(grupo3$P.329.2)*(-1),
                      P.329.3=grupo3$P.329.3,P.329.4=grupo3$P.329.4,P.329.NULL=(grupo3$P.329.NULL)*(-1),P.20.1=(grupo3$P.20.1)*(-1),
                      P.20.2=grupo3$P.20.2,P.20.3=(grupo3$P.20.3)*(-1),P.20.4=(grupo3$P.20.4)*(-1),P.20.5=grupo3$P.20.5,
                      P.20.6=grupo3$P.20.6,P.66.1=grupo3$P.66.1,P.66.2=grupo3$P.66.2,P.66.3=grupo3$P.66.3,
                      P.66.4=grupo3$P.66.4,P.66.5=grupo3$P.66.5,P.66.6=(grupo3$P.66.6)*(-1),P.66.7=(grupo3$P.66.7)*(-1),
                      P.36.SI=grupo3$P.36.SI)  

apply(GRUPO_3,2,mean)
sum(apply(GRUPO_3,2,mean))/35  

GRUPO_4 <- data.frame(P.307.SI=grupo4$P.307.SI, P.308.SI=grupo4$P.308.SI,P.10.1=(grupo4$P.10.1)*(-1),
                      P.10.2=(grupo4$P.10.2)*(-1),P.10.3=(grupo4$P.10.3)*(-1),P.10.4=grupo4$P.10.4,
                      P.10.5=grupo4$P.10.5,P.10.6=grupo4$P.10.6,P.69.1=grupo4$P.69.1,P.69.2=(grupo4$P.69.2)*(-1),
                      P.69.3=grupo4$P.69.3,P.69.4=grupo4$P.69.4,P.69.5=grupo4$P.69.5,P.69.6=grupo4$P.69.6,
                      P.69.7=grupo4$P.69.7,P.69.8=grupo4$P.69.8,P.329.1=(grupo4$P.329.1)*(-1),P.329.2=(grupo4$P.329.2)*(-1),
                      P.329.3=grupo4$P.329.3,P.329.4=grupo4$P.329.4,P.329.NULL=(grupo4$P.329.NULL)*(-1),P.20.1=(grupo4$P.20.1)*(-1),
                      P.20.2=grupo4$P.20.2,P.20.3=(grupo4$P.20.3)*(-1),P.20.4=(grupo4$P.20.4)*(-1),P.20.5=grupo4$P.20.5,
                      P.20.6=grupo4$P.20.6,P.66.1=grupo4$P.66.1,P.66.2=grupo4$P.66.2,P.66.3=grupo4$P.66.3,
                      P.66.4=grupo4$P.66.4,P.66.5=grupo4$P.66.5,P.66.6=(grupo4$P.66.6)*(-1),P.66.7=(grupo4$P.66.7)*(-1),
                      P.36.SI=grupo4$P.36.SI)  

apply(GRUPO_4,2,mean)
sum(apply(GRUPO_4,2,mean))/35  


GRUPO_5 <- data.frame(P.307.SI=grupo5$P.307.SI, P.308.SI=grupo5$P.308.SI,P.10.1=(grupo5$P.10.1)*(-1),
                      P.10.2=(grupo5$P.10.2)*(-1),P.10.3=(grupo5$P.10.3)*(-1),P.10.4=grupo5$P.10.4,
                      P.10.5=grupo5$P.10.5,P.10.6=grupo5$P.10.6,P.69.1=grupo5$P.69.1,P.69.2=(grupo5$P.69.2)*(-1),
                      P.69.3=grupo5$P.69.3,P.69.4=grupo5$P.69.4,P.69.5=grupo5$P.69.5,P.69.6=grupo5$P.69.6,
                      P.69.7=grupo5$P.69.7,P.69.8=grupo5$P.69.8,P.329.1=(grupo5$P.329.1)*(-1),P.329.2=(grupo5$P.329.2)*(-1),
                      P.329.3=grupo5$P.329.3,P.329.4=grupo5$P.329.4,P.329.NULL=(grupo5$P.329.NULL)*(-1),P.20.1=(grupo5$P.20.1)*(-1),
                      P.20.2=grupo5$P.20.2,P.20.3=(grupo5$P.20.3)*(-1),P.20.4=(grupo5$P.20.4)*(-1),P.20.5=grupo5$P.20.5,
                      P.20.6=grupo5$P.20.6,P.66.1=grupo5$P.66.1,P.66.2=grupo5$P.66.2,P.66.3=grupo5$P.66.3,
                      P.66.4=grupo5$P.66.4,P.66.5=grupo5$P.66.5,P.66.6=(grupo5$P.66.6)*(-1),P.66.7=(grupo5$P.66.7)*(-1),
                      P.36.SI=grupo5$P.36.SI)  

apply(GRUPO_5,2,mean)
sum(apply(GRUPO_5,2,mean))/35  

GRUPO_6 <- data.frame(P.307.SI=grupo6$P.307.SI, P.308.SI=grupo6$P.308.SI,P.10.1=(grupo6$P.10.1)*(-1),
                      P.10.2=(grupo6$P.10.2)*(-1),P.10.3=(grupo6$P.10.3)*(-1),P.10.4=grupo6$P.10.4,
                      P.10.5=grupo6$P.10.5,P.10.6=grupo6$P.10.6,P.69.1=grupo6$P.69.1,P.69.2=(grupo6$P.69.2)*(-1),
                      P.69.3=grupo6$P.69.3,P.69.4=grupo6$P.69.4,P.69.5=grupo6$P.69.5,P.69.6=grupo6$P.69.6,
                      P.69.7=grupo6$P.69.7,P.69.8=grupo6$P.69.8,P.329.1=(grupo6$P.329.1)*(-1),P.329.2=(grupo6$P.329.2)*(-1),
                      P.329.3=grupo6$P.329.3,P.329.4=grupo6$P.329.4,P.329.NULL=(grupo6$P.329.NULL)*(-1),P.20.1=(grupo6$P.20.1)*(-1),
                      P.20.2=grupo6$P.20.2,P.20.3=(grupo6$P.20.3)*(-1),P.20.4=(grupo6$P.20.4)*(-1),P.20.5=grupo6$P.20.5,
                      P.20.6=grupo6$P.20.6,P.66.1=grupo6$P.66.1,P.66.2=grupo6$P.66.2,P.66.3=grupo6$P.66.3,
                      P.66.4=grupo6$P.66.4,P.66.5=grupo6$P.66.5,P.66.6=(grupo6$P.66.6)*(-1),P.66.7=(grupo6$P.66.7)*(-1),
                      P.36.SI=grupo6$P.36.SI)  

apply(GRUPO_6,2,mean)
sum(apply(GRUPO_6,2,mean))/35                       


summary(grupo1[,3:38])
