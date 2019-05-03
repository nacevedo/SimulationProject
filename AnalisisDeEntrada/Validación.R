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

############################################################################################
## Profesiograma 1

p1media <- 48.3353
p1desv <- (13.0826*sqrt(30))/tinv(0.975,29)

p1icInferiorR <- p1mediaR - tinv(0.975,14) * (p1desvR/sqrt(15))
p1icSuperiorR <- p1mediaR + tinv(0.975,14) * (p1desvR/sqrt(15))

icInferior1 <- p1media - tinv(0.975,29) * (p1desv/sqrt(30))
icSuperior1 <- p1media + tinv(0.975,29) * (p1desv/sqrt(30))

ic1R = paste("[",p1icInferiorR, ", ", p1icSuperiorR, "]")
ic1S = paste("[",icInferior1, ", ", icSuperior1, "]")
ic1R
ic1S

# Después de agregar réplicas

p1media2 <- 44.2533
p1desv2 <- (2.8627*sqrt(251))/tinv(0.975,250)

icInferior12 <- p1media2 - tinv(0.975,250) * (p1desv2/sqrt(251))
icSuperior12 <- p1media2 + tinv(0.975,250) * (p1desv2/sqrt(251))

ic1S2 = paste("[",icInferior12, ", ", icSuperior12, "]")
ic1R
ic1S2

############################################################################################
## Profesiograma 2

p2media <- 46.4444
p2desv <- (8.6092*sqrt(30))/tinv(0.975,29)

p2icInferiorR <- p2mediaR - tinv(0.975,14) * (p2desvR/sqrt(15))
p2icSuperiorR <- p2mediaR + tinv(0.975,14) * (p2desvR/sqrt(15))

icInferior2 <- p2media - tinv(0.975,29) * (p2desv/sqrt(30))
icSuperior2 <- p2media + tinv(0.975,29) * (p2desv/sqrt(30))

ic2R = paste("[",p2icInferiorR, ", ", p2icSuperiorR, "]")
ic2S = paste("[",icInferior2, ", ", icSuperior2, "]")
ic2R
ic2S

# Después de agregar réplicas

p2media2 <- 46.8198
p2desv2 <- (2.4926*sqrt(251))/tinv(0.975,250)

icInferior22 <- p2media2 - tinv(0.975,250) * (p2desv2/sqrt(251))
icSuperior22 <- p2media2 + tinv(0.975,250) * (p2desv2/sqrt(251))

ic2R = paste("[",p2icInferiorR, ", ", p2icSuperiorR, "]")
ic2S2 = paste("[",icInferior22, ", ", icSuperior22, "]")
ic2R
ic2S2

############################################################################################
## Profesiograma 3

p3media <- 82.1889
p3desv <- (10.3582*sqrt(30))/tinv(0.975,29)

p3icInferiorR <- p3mediaR - tinv(0.975,14) * (p3desvR/sqrt(15))
p3icSuperiorR <- p3mediaR + tinv(0.975,14) * (p3desvR/sqrt(15))

icInferior3 <- p3media - tinv(0.975,29) * (p3desv2/sqrt(30))
icSuperior3 <- p3media + tinv(0.975,29) * (p3desv2/sqrt(30))

ic3R = paste("[",p3icInferiorR, ", ", p3icSuperiorR, "]")
ic3S = paste("[",icInferior3, ", ", icSuperior3, "]")
ic3R
ic3S

# Después de agregar réplicas

p3media2 <- 82.8239
p3desv2 <- (3.0419*sqrt(251))/tinv(0.975,250)

icInferior32 <- p3media2 - tinv(0.975,250) * (p3desv2/sqrt(251))
icSuperior32 <- p3media2 + tinv(0.975,250) * (p3desv2/sqrt(251))

ic3R = paste("[",p3icInferiorR, ", ", p3icSuperiorR, "]")
ic3S2 = paste("[",icInferior32, ", ", icSuperior32, "]")
ic3R
ic3S2

############################################################################################
## Profesiograma 4

p4media <- 88.8348
p4desv <- (12.4082*sqrt(30))/tinv(0.975,29)

p4icInferiorR <- p4mediaR - tinv(0.975,14) * (p4desvR/sqrt(15))
p4icSuperiorR <- p4mediaR + tinv(0.975,14) * (p4desvR/sqrt(15))

icInferior4 <- p4media - tinv(0.975,29) * (p4desv/sqrt(30))
icSuperior4 <- p4media + tinv(0.975,29) * (p4desv/sqrt(30))

ic4R = paste("[",p4icInferiorR, ", ", p4icSuperiorR, "]")
ic4S = paste("[",icInferior4, ", ", icSuperior4, "]")
ic4R
ic4S

# Después de agregar réplicas

p4media2 <- 86.6374
p4desv2 <- (3.1715*sqrt(251))/tinv(0.975,250)

icInferior42 <- p4media2 - tinv(0.975,250) * (p4desv2/sqrt(251))
icSuperior42 <- p4media2 + tinv(0.975,250) * (p4desv2/sqrt(251))

ic4R = paste("[",p4icInferiorR, ", ", p4icSuperiorR, "]")
ic4S2 = paste("[",icInferior42, ", ", icSuperior42, "]")
ic4R
ic4S2
