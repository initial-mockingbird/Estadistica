library(purrr)
library(readr)
library(MASS)
source("src/extra_functools.R")
notas <- read.delim("datasets/notas3.txt",sep=" ")
attach(notas)

# 2) Realice un intervalo de confianza del 97 % para la media de cada variable
# en estudio. Analice lo obtenido.

# Debido a que estamos analizando notas de un examen estandarizado, asumiremos
# que cada variable posee distribucion normal.

tipos <- names(notas)
tipos.cuantitativos <- tipos[-c(9,10)]
datos.cuantitativos <- notas[tipos.cuantitativos]

int_confianza <- datos.cuantitativos %<$>%
                       ((function (x) x$conf.int) %<$>%
                       (function (x)  t.test(x,conf.level = .97) ) )


# 3) Pruebe, a un nıvel de 0.05, que el promedio de la prueba estandarizada
# es mayor a 60 puntos.

# Sea:
# H0: el promedio de la muestra es mayor a 60 puntos
# Ha: el promedio de la muestra es menor a 60 puntos
# mu0: 60
# n = length(Y) = 118
# mu: mean(Y) = 60.2
# Region de rechazo: Z< -z_{a} = Z < -z_{0.05}

nivel <- .05
mu0 <- 61.5
#s <- sd(Y)
#n <- length(Y)
#mu <- mean(Y)
#rr <- qnorm(nivel)
#z = (mu-mu0)/(s/(n)^(.5))

#esRechazada <- z<rr
# alternativamente:
coso <- t.test(Y,alternative = "less",mu=mu0,conf.level = 1-nivel)
esRechazado <- coso$p.value > nivel


# 4) Estudie si las notas promedio entre matematicas 1 y
# sociales 1 son iguales.


var.MA1 <- var(MA1)
var.CS1 <- var(CS1)
# Ya que trabajamos con muestras grandes los resultados de dividir las varianzas
# y usar var.test deben coincidir.
err1 <- var.MA1/var.CS1
err2 <- var.test(MA1,CS1)

mu <- mean(MA1)- mean(CS1)
#diff <- t.test(MA1,CS1,var.equal = T,conf.level = 0.97)$conf.int
diff <- t.test(MA1,CS1,var.equal = F,conf.level = 0.97,mu=0,alternative = "two.sided")

# 5) Realizar una prueba de bondad de ajuste para determinar si la
# variable “Y” tiene distribucion normal.

# Se pierden 2 grados de libertad para estimar la media y la varianza.
r <- 2
qqnorm(Y)
qqline(Y)

fi <- hist(Y,breaks = 30, plot = F,right = F)

qqnorm(fi$counts)
qqline(fi$counts)
k <- length(fi$counts)
n <- sum(fi$counts)
xbarra <- sum(fi$counts*fi$mids)/n
x_barra <- rep(xbarra, k)
S_cuadrado <- sum( fi$counts*(fi$mids-x_barra)^2 )/(n-1)
S <- sqrt(S_cuadrado)
lim.inf <- c(0,fi$breaks[-(length(fi$breaks))])
pi <- pnorm(fi$breaks, xbarra, S) - pnorm(lim.inf, xbarra, S)
chi2_obs <- sum((fi$counts-n*pi)^2/(n*pi))
(p_valor <- 1 - pchisq(chi2_obs, k - 1 - r))

# 6) Con un nıvel de significancia de 0.02, pruebe si las proporciones
# de estudiantes por carrera son iguales.

(cuentas <- summary(factor(Car)))
(n <- sum(cuentas))
(proporciones <- prop.test(cuentas,rep(n,length(cuentas))))
