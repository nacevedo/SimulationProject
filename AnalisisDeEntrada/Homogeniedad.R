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

