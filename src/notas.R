library(purrr)
library(readr)
source("src/extra_functools.R")
notas <- read.delim("datasets/notas3.txt",sep=" ")
attach(notas)


# Punto 1: Realizar un análisis descriptivo de los datos.

tipos <- names(notas)
tipos.cualitativos <- tipos[c(9,10)]
tipos.cuantitativos <- tipos[-c(9,10)]

######################################
#                                    #
# Analisis Cualitativo de los datos  #
#                                    #
######################################

datos.cualitativos <- notas[tipos.cualitativos]
analisis.cualitativo <- datos.cualitativos %<$>% (summary %<$>% factor)


## Definimos ciertas variables para hacer el plot lindo

# Los titulos de las graficas
mains <- c("Genero del estudiante","Carrera del Estudiante")

# Los colores a usar:
col = c("brown","red","grey","yellow","green","pink","purple","blue")

## Definimos ciertas funciones auxiliares
plotting <- function (x,y) plot(unlist(x),main=y,col=col,ylab="Personas")
plot.pairs <- partial(uncurry,plotting)

## Plotting de la data.
# Para hacer el plot de las variables cualitativas lado a lado
par(mfrow=c(1,2))
pairs <- zip(datos.cualitativos,mains)
pairs %<$>% plot.pairs


######################################
#                                    #
# Analisis Cuantitativo de los datos #
#                                    #
######################################


datos.cuantitativos <- notas[tipos.cuantitativos]
analisis.cuantitativo <- datos.cuantitativos %<$>% summary
analisis.cuantitativo %<$>% sd
analisis.cuantitativo %<$>% IQR

## Definimos ciertas variables para hacer el plot lindo

## Titulo de los histogramas
tipos.cuantitativos[length(tipos.cuantitativos)] <- "Examen Estandarizado"
mainh <- unlist (tipos.cuantitativos %<$>% partial(paste,"Notas en"))

## Titulo de los boxplots

mainb <- unlist(tipos.cuantitativos %<$>% partial(paste,"Boxplot de"))

# Funciones auxiliares
boxplotting <- function (x,y) boxplot(unlist(x),horizontal=F, ylab="Notas",
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
res <- datos.cuantitativos %<$>% extra.hacky

par(mfrow=c(1,1))
boxplot(datos.cuantitativos,main="Boxplot de Datos Conjuntos",
        ylab="Notas",col=col)
