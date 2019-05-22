library(MASS)
library(survival)
library(fitdistrplus)
library(rriskDistributions)

# Optometría
datosA = rgamma(n = 39, shape = 124.28, scale = 1/9.36 )
hist(datosA, main = "Histograma A", xlab="Tiempo (mins)", las=1, pro = FALSE)
fit.cont(datosA)


# Espirometria
datosB = rgamma(n = 57, shape = 2.35, scale = 2.83 )
hist(datosB, main = "Histograma B", xlab="Tiempo (mins)", las=1, pro = FALSE)
fit.cont(datosB)

# Audiometría
datosC = rlnorm(n = 48, meanlog = 2, sdlog = 0.37)
hist(datosC, main = "Histograma C", xlab="Tiempo (mins)", las=1, pro = FALSE)
fit.cont(datosC)

lNormMeanC = 2
lNormStdC = 0.37

normMeanC = log( lNormMeanC^2 / sqrt( lNormStdC^2 + lNormMeanC^2 ) )
normStdC = sqrt ( log(1 + lNormStdC^2/lNormMeanC^2) )

# Salud ocupasional
datosD = rweibull(n = 43, shape = 15.29, scale = 15.17)
hist(datosD, main = "Histograma D", xlab="Tiempo (mins)", las=1, pro = FALSE)
fit.cont(datosD)

# Recepción 2
lNormMeanE = 0.9
lNormStdE = 0.12
datosE = rlnorm(n = 74, meanlog = lNormMeanE, sdlog = lNormStdE)
hist(datosE, main = "Histograma E", xlab="Tiempo (mins)", las=1, pro = FALSE)
fit.cont(datosE)

normMeanE = log( lNormMeanE^2 / sqrt( lNormStdE^2 + lNormMeanE^2 ) )
normStdE = sqrt ( log(1 + lNormStdE^2/lNormMeanE^2) )


# Recepción 3
datosF = rlnorm(n = 74, meanlog = 0.683, sdlog = 0.19)
hist(datosF, main = "Histograma E", xlab="Tiempo (mins)", las=1, pro = FALSE)
fit.cont(datosE)

lNormMeanF = 0.683
lNormStdF = 0.19

normMeanF = log( lNormMeanF^2 / sqrt( lNormStdF^2 + lNormMeanF^2 ) )
normStdF = sqrt ( log(1 + lNormStdF^2/lNormMeanF^2) )


datos = cbind(datosA, datosB, datosC)

write.csv(datosA, "a.csv")
write.csv(datosB, "b.csv")
write.csv(datosC, "c.csv")
write.csv(datosD, "d.csv")
write.csv(datosE, "e.csv")

###############################################################################
# Datos Validación

# Profesiograma 1
datosP1 = rnorm(n = 30, mean = 32, sd = 19)
mean(datosP1)
sd(datosP1)
hist(datosP1, main = "Histograma P1", xlab="Tiempo (mins)", las=1, pro = FALSE)

# Profesiograma 2
datosP2= rnorm(n = 30, mean = 69, sd = 21)
mean(datosP2)
sd(datosP2)
hist(datosP2, main = "Histograma P2", xlab="Tiempo (mins)", las=1, pro = FALSE)

# Profesiograma 3
datosP3 = rnorm(n = 30, mean = 83, sd = 25)
mean(datosP3)
sd(datosP3)
hist(datosP3, main = "Histograma P3", xlab="Tiempo (mins)", las=1, pro = FALSE)

# Profesiograma 4
datosP4 = rnorm(n = 30, mean = 90, sd = 25)
mean(datosP4)
sd(datosP4)
hist(datosP4, main = "Histograma P4", xlab="Tiempo (mins)", las=1, pro = FALSE)

dValidacion = data.frame(datosP1, datosP2, datosP3, datosP4)
write.csv(dValidacion, "DatosValidacion.csv")