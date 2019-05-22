############################################################################################
## Cargar serie de datos:
datos2 <- read_excel("Validación.xlsx", sheet = "VF")

p1mediaR <- datos2$P1[1]
p1desvR <- datos2$P1[2]

p2mediaR <- datos2$P2[1]
p2desvR <- datos2$P2[2]

p3mediaR <- datos2$P3[1]
p3desvR <- datos2$P3[2]

############################################################################################
## Original vs Alternativa 1

# Intervalo de confianza σ1^2/σ2^2. H0: σ1^2=σ2^2. Si el intervalo contiene el 1 no hay d