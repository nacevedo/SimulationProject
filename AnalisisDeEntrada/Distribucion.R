# Simulaci?n de Eventos Discretos
# equiposimulacion@uniandes.edu.co
#################################################

##Instalaci?n de los paquetes utilizados
install.packages("MASS")
install.packages("survival")
install.packages("fitdistrplus")
install.packages("rriskDistributions")

##Cargar los paquetes
library(MASS)
library(survival)
library(fitdistrplus)
library(readxl)

############################################################################################
## Cargar serie de datos:
datos <- read_excel("DATA_FINAL.xlsx", sheet = "DATA")


############################################################################################
## Histograma de la serie de datos.
hist(datos$E1, main = "Histograma de la serie de datos", xlab="Hora entre Arribos", las=1, pro = FALSE)

############################################################################################
### Prueba de homogeneidad de varianzas 

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles<-datos[datos$Franja %in% c("1", "2"), ]

#Ver tabla
#View(dosniveles)

#Prueba de varianzas iguales
res.ftest<-var.test(dosniveles$E1~dosniveles$Franja,data=dosniveles)

#Resultados test F 
res.ftest

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles<-datos[datos$Franja %in% c("2", "3"), ]

#Ver tabla
#View(dosniveles)

#Prueba de varianzas iguales
res.ftest<-var.test(dosniveles$E1~dosniveles$Franja,data=dosniveles)

#Resultados test F 
res.ftest

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles<-datos[datos$Franja %in% c("3", "1"), ]

#Ver tabla
#View(dosniveles)

#Prueba de varianzas iguales
res.ftest<-var.test(dosniveles$E1~dosniveles$Franja,data=dosniveles)

#Resultados test F 
res.ftest
############################################################################################
### Prueba de diferencia de medias

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles <- datos[datos$Franja %in% c("3", "1"), ]

# Prueba de diferencia de medias
res.ttest <- t.test(dosniveles$E1~dosniveles$Franja,data=dosniveles)

# Resultados prueba diferencia de medias
res.ttest

############################################################################################
##  Bondad de ajuste para la franja horaria 1
franjahoraria<-datos[datos$Franja %in% c("1"), ]

## Histograma de la serie de datos.
hist(franjahoraria$E1, main = "Histograma de la serie de datos", las=1, prob=FALSE)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste <- fitdist(franjahoraria$E1, "exp")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados <- gofstat(ajuste)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados$chisqpvalue
############################################################################################
##  Bondad de ajuste para la franja horaria 1
franjahoraria<-datos[datos$Franja %in% c("2"), ]


## Histograma de la serie de datos.
hist(franjahoraria$E1, main = "Histograma de la serie de datos", las=1, prob=FALSE)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste <- fitdist(franjahoraria$E1, "exp")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados <- gofstat(ajuste)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados$chisqpvalue
############################################################################################
##  Bondad de ajuste para la franja horaria 1
franjahoraria<-datos[datos$Franja %in% c("3"), ]

## Histograma de la serie de datos.
hist(franjahoraria$E1, main = "Histograma de la serie de datos", las=1, prob=FALSE)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste <- fitdist(franjahoraria$E1, "weibull")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados <- gofstat(ajuste)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados$chisqpvalue
