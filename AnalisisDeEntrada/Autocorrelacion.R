library(readxl)

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

############################################################################################
# Recepción 2
acf(x=r2)

# Recepción 3
acf(x=r3)

# Salud ocupasional
acf(x=saludO)

# Audiometría
acf(x=audio)

# Espirometría
acf(x=espir)

# Optometria
acf(x=opto)

# Entrega de resultados
acf(x=entrega)


