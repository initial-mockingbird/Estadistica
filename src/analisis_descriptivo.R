library(purrr)
library(readr)
library(ggplot2)
library(gridExtra)
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

datos.cualitativos <- data.frame(notas[tipos.cualitativos] %<$>% factor)
# Analisis descriptivo usando summary.
analisis.cualitativo <- datos.cualitativos %<$>% summary
# Colorcitos cheveres
fillcol <- c("#F9766E","#00BFC4","#50BE97","#CE9ADA","#CEA665",
             "#b0ce65","#6a65ce","#ad65ce")

## Definimos ciertas variables para hacer el plot lindo

# Los titulos de las graficas
mains <- c("Genero del estudiante","Carrera del Estudiante")

cual_gen <- ggplot(datos.cualitativos,aes(x=Gen,fill=Gen)) +
     geom_histogram(stat="count") +
     labs(title=mains[1],y="Frecuencias",x="") +
     theme(plot.title = element_text(hjust = 0.5,face="bold"),legend.position="none") +
     scale_fill_manual(values=fillcol[1:2])

cual_carr <- ggplot(datos.cualitativos,aes(x=Car,fill=Car)) +
     geom_histogram(stat="count") +
     labs(title=mains[2],y="Frecuencias",x="") +
     theme(plot.title = element_text(hjust = 0.5,face="bold"),legend.position="none") +
     scale_fill_manual(values=fillcol[1:5])

grid.arrange(cual_gen,cual_carr,nrow=1)


######################################
#                                    #
# Analisis Cuantitativo de los datos #
#                                    #
######################################


datos.cuantitativos <- notas[tipos.cuantitativos]
# Analisis descriptivo usando summary.
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

gen_plotting <- function (x,y,z){
     p1 <- ggplot(df,aes(x=x)) +
          geom_histogram(aes(y=..density..),colour="black",fill="white") +
          geom_density(alpha=.5,fill="#4605ff") +
          labs(title=y,x="Notas",y="Frecuencias") +
          theme(plot.title = element_text(hjust = 0.5,face="bold"))

     p2 <- ggplot(df, aes(x=0,y=x)) +
          geom_boxplot(alpha=.5,fill="#5E5086")+
          labs(title=z,x="", y="Notas") +
          theme(legend.position="none",
                plot.title = element_text(hjust = 0.5,face="bold"))

     grid.arrange(p1,p2,nrow=1)

}

# Creando una clausura para poder trabajar sin depender del zip multiple
# junto al uncurry
hacky.plot <- function () {
     i <- 0
     function (x) {
          i <<- i+1
          gen_plotting(x,mainh[i],mainb[i])
     }

}

extra.hacky <- hacky.plot()

## Plotting de la data.
df <- data.frame((datos.cuantitativos %<$>% data.frame) )
df %<$>% extra.hacky


ggplot(stack(datos.cuantitativos),aes(x=ind,y=values,fill=ind)) +
        geom_boxplot() +
        labs(title="Boxplot de Datos Conjuntos",x="Materias",y="Notas") +
        theme(plot.title = element_text(hjust = 0.5,face="bold")) +
        scale_fill_manual(values=fillcol[1:8])

# Finalmente, realizamos un analisis cruzado de las carreras vs notas
# para comprobar la hipotesis:

# Clausura para nombres de carrera
main <- map(names(datos.cuantitativos),
            function (x)  paste("Boxplot de Carreras vs ",x) )
carrera.nota.aux <- function () {
     i <- 0
     function (x) {
          i <<- i+1
          ggplot(notas,aes(x=Car,y=x,fill=Car)) +
          geom_boxplot() +
          labs(title=main[i],x="Carreras",y="Notas") +
          theme(plot.title = element_text(hjust = 0.5,face="bold")) +
          scale_fill_manual(values=fillcol[1:5])
     }
}

# Diagramas mixtos de las carreras vs notas
carrera.nota <- carrera.nota.aux()
datos.cuantitativos  %<$>% carrera.nota