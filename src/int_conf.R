library(purrr)
library(readr)
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