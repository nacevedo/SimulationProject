# Simulaci?n de Eventos Discretos
# equiposimulacion@uniandes.edu.co
#################################################

##Instalaci?n de los paquetes utilizados
install.packages("MASS")
install.packages("survival")
install.packages("fitdistrplus")
install.packages("rriskDistributions")
install.packages("actuar")

##Cargar los paquetes
library(MASS)
library(survival)
library(fitdistrplus)
library(readxl)
library(rriskDistributions)
library(actuar)

############################################################################################
## Cargar serie de datos:
datos <- read.csv("data.csv", sep = ";")

r2 <- datos$Recepcion2
r2 <- r2[!is.na(r2)]

r3 <- datos$Recepcion3
r3 <- r3[!is.na(r3)]

saludO <- datos$SaludOcupacional
saludO <- saludO[!is.na(saludO)]

audio <- datos$Audiometria
audio <- audio[!is.na(audio)]

espir <- datos$Espirometria
espir <- espir[!is.na(espir)]

opto <- datos$Optometria
opto <- opto[!is.na(opto)]

tm <- datos$TomaMuestras
tm <- tm[!is.na(tm)]

entrega <- datos$EntregaLab
entrega <- entrega[!is.na(entrega)]

############################################################################################
## Recepción 2

## Histograma de la serie de datos
hist(r2, main = "Histograma de la serie de datos", xlab="Hora entre Arribos", las=1, pro = FALSE)

##  Bondad de ajuste para la franja horaria 1

res <- fit.cont(r2)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste <- fitdist(r2, "lnorm")

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
## Recepción 3

## Histograma de la serie de datos
hist(r3, main = "Histograma de la serie de datos Recepción 3", xlab="Hora entre Arribos", las=1, pro = FALSE)

##  Bondad de ajuste para la franja horaria 1

res2 <- fit.cont(r3)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste2 <- fitdist(r3, "lnorm")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste2$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste2)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados2 <- gofstat(ajuste)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados2$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados2$chisqpvalue

############################################################################################
## Salud Ocupacional

## Histograma de la serie de datos
hist(saludO, main = "Histograma de la serie de datos Salud Ocupacional", xlab="Hora entre Arribos", las=1, pro = FALSE)

##  Bondad de ajuste para la franja horaria 1

res3 <- fit.cont(saludO)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste3 <- fitdist(saludO, "gamma")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste3$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste3)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados3 <- gofstat(ajuste3)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados3$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados3$chisqpvalue

############################################################################################
## Audiometría ********* cuadra más con otras distribuciones

## Histograma de la serie de datos
hist(audio, main = "Histograma de la serie de datos", xlab="Hora entre Arribos", las=1, pro = FALSE)

##  Bondad de ajuste para la franja horaria 1

res4 <- fit.cont(audio)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste4 <- fitdist(audio, "llogis")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste4$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste4)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados4 <- gofstat(ajuste4)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados4$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados4$chisqpvalue

############################################################################################
## Espirometría ********* cuadra más con otras distribuciones

## Histograma de la serie de datos
hist(espir, main = "Histograma de Espirometría", xlab="Tasa de servicio", las=1, pro = FALSE)

##  Bondad de ajuste para la franja horaria 1

res5 <- fit.cont(espir)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste5 <- fitdist(espir, "gamma")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste5$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste5)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados5 <- gofstat(ajuste5)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados5$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados5$chisqpvalue

############################################################################################
## Optometría ********* cuadra más con otras distribuciones

## Histograma de la serie de datos
hist(opto, main = "Histograma de Optometría", xlab="Tasa de servicio", las=1, pro = FALSE)

##  Bondad de ajuste para la franja horaria 1

res6 <- fit.cont(opto)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste6 <- fitdist(opto, "gamma")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste6$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste6)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados6 <- gofstat(ajuste6)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados6$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados6$chisqpvalue

############################################################################################
## Toma de muestras ********* no cuadra jaja

## Histograma de la serie de datos
hist(tm, main = "Histograma de toma de muestras", xlab="Tasa de servicio", las=1, pro = FALSE)

##  Bondad de ajuste para la franja horaria 1

res7 <- fit.cont(tm)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste7 <- fitdist(tm, "gamma")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste7$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste7)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados7 <- gofstat(ajuste7)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados7$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados7$chisqpvalue

############################################################################################
## Entrega laboratorio

## Histograma de la serie de datos
hist(entrega, main = "Histograma de Optometría", xlab="Tasa de servicio", las=1, pro = FALSE)

##  Bondad de ajuste para la franja horaria 1

res8 <- fit.cont(entrega)

## Almacenar la estimaci?n por m?xima verosimilitud de la serie de datos
## a una distribuci?n de probabilidad ingresada por par?metro.
ajuste8 <- fitdist(entrega, "llogis")

## Mostrar los par?metros del ajuste a la distribuci?n dada.
ajuste8$estimate

## Mostrar las gr?ficas de inter?s de las distribuciones emp?rica y te?rica.
plot(ajuste8)

## Realizar y guardar prueba de bondad de ajuste a la serie de datos con respecto a
## la distribuci?n te?rica escogida.
resultados8 <- gofstat(ajuste8)

## Rechazo de la prueba de Kolmogorov-Smirnov
resultados8$kstest

## P-Value de la prueba de Chi-Cuadrado
resultados8$chisqpvalue

