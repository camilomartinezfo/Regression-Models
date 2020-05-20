#Lectura de datos de una parcela permanente de 1/40 hectareas

datos <- read.csv2("Tabla1.csv",dec=".",fileEncoding = "latin1")
str(datos)
View(datos)
names(datos) <- c("ID","DAP","ALTURA","VOLUMEN")

#Se propondra un modelo de regresion lineal simple, relacionando volumen con una variable
#explicatoria, Area basal*Altura, pues es logico pensar en una estimacion del volumen de esa
#forma.

datos$V.Ind <- (((datos$DAP/100)^2)*pi/4)*datos$ALTURA

#Posterior se hace una vision general de los datos a relacionar
plot(datos$V.Ind,datos$VOLUMEN,xlab="Nueva variable Ab*h",ylab="Volumen")
#Parecen presentarsen outliers, lo que va a generar mayor error en un modelo explicatorio
#Sin embargo se confirmaran con el generado de boxplot

str(boxplot(datos$VOLUMEN))
#Efectivamente hay datos at?picos
Outliers <- boxplot(datos$VOLUMEN)$out
#Muestra dos outliers
datos[datos$VOLUMEN==(Outliers[1])|datos$VOLUMEN==(Outliers[2]),]


#Prueba de deteccion de atipicos de TUKEY 
cuantiles <- as.list(summary(datos$VOLUMEN))
#se puede notar que la media esta al doble de la mediana, muy alejadas, significa que
#pasa algo raro. Entre Q1 y Q3 se encuentran el 50% de los datos
#Rango intercuantilico IQR
IQR <- cuantiles$`3rd Qu.`-cuantiles$`1st Qu.`
Q1 <- cuantiles$`1st Qu.`
Q2 <- cuantiles$`3rd Qu.`
#hay 0.4795 pasos
#valor atipico leve que dista 1.5 veces *IQR debajo de
#Q1 o encima de Q3
#Valor atipico extremo dista 3 veces
#Estos serian los intervalos con valores atipicos leves
Q1-1.5*IQR #negativo, minimo seria cero
maximo <- Q2+1.5*IQR #maximo posible

#Visualizacion de los datos que son considerados atipicos leves
datos[(which(datos$VOLUMEN>maximo)),]

#visualizacion de los datos que son atipicos extremos
maximo2 <- Q2+3*IQR
datos[(which(datos$VOLUMEN>maximo2)),]


#Generando el modelo lineal, tambien es posible encontrar las medidas atipicas
modelo1 <- lm(datos$VOLUMEN~datos$V.Ind)
summary(modelo1)
influence.measures(modelo1)

#data Wrangling 
datos2 <- datos[-(which(datos$VOLUMEN>maximo)),]

#Visualizacion de datos ggplot
library(ggplot2)
ggplot(datos2, aes(x=V.Ind, y=VOLUMEN)) + 
  geom_point() + theme_light()
#Ahora si hay una tendencia mas clara en los datos


#Ajuste modelo
modelo1 <- lm(VOLUMEN~V.Ind,datos2)
modelo1
summary(modelo1)

#A?adiendo recta de regresion 
ggplot(datos2, aes(x=V.Ind, y=VOLUMEN)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  theme_light() +
  ggtitle("Volumen en funcion de la variable Ab*h")+
  labs(x="Ab*h", y="Volumen")

#Predicciones
datos2$predicciones <- predict(modelo1)
#Graficacion predichos 
ggplot(datos2, aes(x=V.Ind, y=VOLUMEN)) +
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +
  geom_segment(aes(xend=V.Ind, yend=predicciones), col='red', lty='dashed') +
  geom_point() +
  geom_point(aes(y=predicciones), col='red') +
  theme_light()
