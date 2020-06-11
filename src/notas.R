library(purrr)
library(readr)
source("src/extra_functools.R")
notas <- read.delim("datasets/notas3.txt",sep=" ")
attach(notas)


tipos <- names(notas)
tipos_cualitativos <- tipos[c(9,10)]
tipos_cuantitativos <- tipos[-c(9,10)]

######################################
#                                    #
# Analisis Cualitativo de los datos  #
#                                    #
######################################

datos_cualitativos <- notas[tipos_cualitativos]
analisis_cualitativo <- datos_cualitativos %<$>% (summary %<$>% factor)


## Definimos ciertas variables para hacer el plot lindo

# Los titulos de las graficas
mains <- c("Genero del estudiante","Carrera del Estudiante")

# Los colores a usar:
col = c("brown","red","grey","yellow")

## Definimos ciertas funciones auxiliares
plotting <- function (x,y) plot(unlist(x),main=y,col=col)
plot.pairs <- partial(uncurry,plotting)

## Plotting de la data.
# Para hacer el plot de las variables cualitativas lado a lado
par(mfrow=c(1,2))
pairs <- zip(datos_cualitativos,mains)
pairs %<$>% plot.pairs


######################################
#                                    #
# Analisis Cuantitativo de los datos #
#                                    #
######################################


datos_cuantitativos <- notas[tipos_cuantitativos]
analisis_cuantitativo <- datos_cuantitativos %<$>% summary

## Definimos ciertas variables para hacer el plot lindo

## Titulo de los histogramas
tipos_cuantitativos[length(tipos_cuantitativos)] <- "Examen Estandarizado"
mainh <- unlist (tipos_cuantitativos %<$>% partial(paste,"Notas en"))

## Titulo de los boxplots

mainb <- unlist(tipos_cuantitativos %<$>% partial(paste,"Boxplot de"))

# Funciones auxiliares
boxplotting <- function (x,y) boxplot(unlist(x),horizontal=F,
                                      xlab="Calificaciones",main=y,col="yellow")

histplotting <- function (x,y) hist(unlist(x),xlab="Calificaciones",
                               ylab="Frecuencia",main=y,col=col)

# Creando una clausura para poder trabajar sin depender del zip multiple
# junto al uncurry
hacky.plot <- function () {
     i <- 0
     function (x) {
          i <<- i+1
          histplotting(x,mainh[i])
          boxplotting(x,mainb[i])
     }

}

extra.hacky <- hacky.plot()

## Plotting de la data.
# Para hacer el plot de las variables cuantitativas lado a lado
par(mfrow=c(1,2))
res <- datos_cuantitativos %<$>% extra.hacky


