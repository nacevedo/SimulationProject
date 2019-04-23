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
library(rriskDistributions)

############################################################################################
## Cargar serie de datos:
datos <- read_excel("DATA_FINAL.xlsx", sheet = "DATA")

r2 <- datos$Recepción2
r2 <- r2[!is.na(r2)]

r3 <- datos$Recepción3
r3 <- r3[!is.na(r3)]

saludO <- datos$SaludOcupacional
saludO <- saludO[!is.na(saludO)]

audio <- datos$Audiometría
audio <- audio[!is.na(audio)]

espir <- datos$Espirometría
espir <- espir[!is.na(espir)]

opto <- datos$Optometría
opto <- opto[!is.na(opto)]

tm <- datos$TomaMuestras
tm <- tm[!is.na(tm)]

entrega <- datos$EntregaLab
entrega <- entrega[!is.na(entrega)]

datos <- read.csv(file="Datos.csv", header=TRUE, sep=",")




### Prueba de homogeneidad de varianzas 

############################################################################################################################
concat <- c("1", "2") #Comparar franja 1 con 2 
#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles<-datos[datos$FranjaHoraria %in% concat, ]

#Ver tabla
View(dosniveles)

#Prueba de varianzas iguales
res.ftest<-var.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

#Resultados test F 
res.ftest

############################################################################################
### Prueba de diferencia de medias

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles <- datos[datos$FranjaHoraria %in% concat, ]

# Prueba de diferencia de medias
res.ttest <- t.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

# Resultados prueba diferencia de medias
res.ttest

############################################################################################################################
concat <- c("1", "3") #Comparar franja 1 con 3

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles<-datos[datos$FranjaHoraria %in% concat, ]

#Ver tabla
View(dosniveles)

#Prueba de varianzas iguales
res.ftest<-var.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

#Resultados test F 
res.ftest

############################################################################################
### Prueba de diferencia de medias

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles <- datos[datos$FranjaHoraria %in% concat, ]

# Prueba de diferencia de medias
res.ttest <- t.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

# Resultados prueba diferencia de medias
res.ttest

############################################################################################################################
concat <- c("1", "4") #Comparar franja 1 con 3

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles<-datos[datos$FranjaHoraria %in% concat, ]

#Ver tabla
View(dosniveles)

#Prueba de varianzas iguales
res.ftest<-var.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

#Resultados test F 
res.ftest

############################################################################################
### Prueba de diferencia de medias

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles <- datos[datos$FranjaHoraria %in% concat, ]

# Prueba de diferencia de medias
res.ttest <- t.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

# Resultados prueba diferencia de medias
res.ttest

############################################################################################################################
concat <- c("1", "4") #Comparar franja 1 con 3

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles<-datos[datos$FranjaHoraria %in% concat, ]

#Ver tabla
View(dosniveles)

#Prueba de varianzas iguales
res.ftest<-var.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

#Resultados test F 
res.ftest

############################################################################################
### Prueba de diferencia de medias

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles <- datos[datos$FranjaHoraria %in% concat, ]

# Prueba de diferencia de medias
res.ttest <- t.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

# Resultados prueba diferencia de medias
res.ttest

############################################################################################################################
concat <- c("1", "5") #Comparar franja 1 con 3

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles<-datos[datos$FranjaHoraria %in% concat, ]

#Ver tabla
View(dosniveles)

#Prueba de varianzas iguales
res.ftest<-var.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

#Resultados test F 
res.ftest

############################################################################################
### Prueba de diferencia de medias

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles <- datos[datos$FranjaHoraria %in% concat, ]

# Prueba de diferencia de medias
res.ttest <- t.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

# Resultados prueba diferencia de medias
res.ttest

############################################################################################################################
concat <- c("2", "4") #Comparar franja 1 con 3

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles<-datos[datos$FranjaHoraria %in% concat, ]

#Ver tabla
View(dosniveles)

#Prueba de varianzas iguales
res.ftest<-var.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

#Resultados test F 
res.ftest

############################################################################################
### Prueba de diferencia de medias

#Filtrar la tabla por las 2 franjas horarias que voy a comparar
dosniveles <- datos[datos$FranjaHoraria %in% concat, ]

# Prueba de diferencia de medias
res.ttest <- t.test(dosniveles$TiempoEntreArribo~dosniveles$FranjaHoraria,data=dosniveles)

# Resultados prueba diferencia de medias
res.ttest




