##Instalaci?n de los paquetes utilizados
install.packages("MASS")
install.packages("survival")
install.packages("fitdistrplus")
install.packages("rriskDistributions")
install.packages("PEIP")

##Cargar los paquetes
library(MASS)
library(survival)
library(fitdistrplus)
library(readxl)
library(rriskDistributions)
library(PEIP)

############################################################################################
## Cargar serie de datos:
datos2 <- read_excel("Validación.xlsx", sheet = "VF")

p1mediaR <- datos2$P1[1]
p1desvR <- datos2$P1[2]

p2mediaR <- datos2$P2[1]
p2desvR <- datos2$P2[2]

p3mediaR <- datos2$P3[1]
p3desvR <- datos2$P3[2]

p4mediaR <- datos2$P4[1]
p4desvR <- datos2$P4[2]

p1media <- 48.3353
p1desv <- (13.0826*sqrt(30))/tinv(0.975,29)

p1icInferiorR <- p1mediaR - tinv(0.975,2) * (p1desvR/sqrt(3))
p1icSuperiorR <- p1mediaR + tinv(0.975,2) * (p1desvR/sqrt(3))

icInferior <- p1media - tinv(0.975,29) * (p1desv/sqrt(30))
icSuperior <- p1media + tinv(0.975,29) * (p1desv/sqrt(30))

p2media <- 46.4444
p2desv <- (8.6092*sqrt(30))/tinv(0.975,29)

p3media <- 82.1889
p3desv <- (10.3582*sqrt(30))/tinv(0.975,29)

p4media <- 88.8348
p4desv <- (12.4082*sqrt(30))/tinv(0.975,29)

