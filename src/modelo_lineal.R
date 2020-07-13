library(purrr)
library(readr)
library(ggplot2)
library(gridExtra)
library("GGally")
library(stargazer)
source("src/extra_functools.R")
notas <- read.delim("datasets/notas3.txt",sep=" ")
notas_predict <- read.delim("datasets/notas_predict.txt",sep=" ")
attach(notas)

# 7) Realizar un gráfico de dispersión y una matriz de correlación de las
# variables.
tipos <- names(notas)
tipos.cuantitativos <- tipos[-c(9,10)]
datos.cuantitativos <- notas[tipos.cuantitativos]
#datos.cuantitativos <- (datos.cuantitativos - lapply(datos.cuantitativos, mean))/
#     lapply(datos.cuantitativos, sd)
#attach(datos.cuantitativos)
ggpairs(datos.cuantitativos,
        lower = list(continous = wrap("smooth",alpha =.3, size =.1)))
#pairs(datos.cuantitativos)
corr <- cor(datos.cuantitativos)

# 8) Halle un modelo lineal que explique mejor la variable “Y”. Incluya
# todas las pruebas necesarias para llegar a este modelo, asi
# como un analisis de residuos del modelo final.

variables <- names(datos.cuantitativos)[-c(8)]
formula <- function(variables) {
     return(
          as.formula(
               paste("Y",
                     paste(variables, collapse = " + "),
                     sep = " ~ "))
     )
}


ln.Y.completo <- lm(Y ~ MA1 + MA2 + MA3 + CS1 + CS2 + LL1 + LL2)

no.significativos <- c(7)

# ln.Y.2 <- lm(formula(variables[-no.significativos]))
ln.Y.2 <- lm(Y ~ MA1 + MA2 + MA3 + CS1 + CS2 + LL1)

no.significativos <- c(no.significativos,4)
# ln.Y.3 <- lm(formula(variables[-no.significativos]))
ln.Y.3 <- lm(Y ~ MA1 + MA2 + MA3 + CS2 + LL1)

no.significativos <- c(no.significativos,5)
#ln.Y.4 <- lm(formula(variables[-no.significativos]))
ln.Y.4 <- lm(Y ~ MA1 + MA2 + MA3 + LL1)

no.significativos <- c(no.significativos,6)
#ln.Y.5 <- lm(formula(variables[-no.significativos]))
ln.Y.5 <- lm(Y ~ MA1 + MA2 + MA3)

modelos <- list (
     ln.Y.completo,
     ln.Y.2,
     ln.Y.3,
     ln.Y.4,
     ln.Y.5 # <- mejor
)

# Grafiquitasssssss
par(mfrow = c(2,2))
lapply(modelos,plot)
# Descomentar para obtener codigo en latex
  stargazer(modelos[1:3],title="Modelos",align=T)
  stargazer(modelos[4:5],title="Resumen de los Modelos 2",align=T)
par(mfrow = c(1,1))
# Summaryyyy
lapply(modelos,summary)

# 8)
detach(notas)
attach(notas_predict)
predict.int <- data.frame(predict(ln.Y.5,notas_predict,interval="prediction"))
predict.int$fit <- round(predict.int$fit)
conf.int <- data.frame(predict(ln.Y.5,notas_predict,interval="confidence"))

y.res <- data.frame(notas_predict$Y-predict.int$fit)
names(y.res) <- c("Residuos_de_prediccion")

y.res.hist <- ggplot(y.res,aes(x=Residuos_de_prediccion,fill=..density..)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha=.5,fill="#50BE97") +
  labs(title="Histograma de Residuos",y="Densidad",x="Valor observado - Prediccion") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"),legend.position="none") +
  scale_fill_gradient(low="#245745",high="#50BE97")

y.real.bp <- ggplot(y.res,aes(x=0,y=Residuos_de_prediccion)) +
  geom_boxplot(fill="#50BE97") +
  labs(title="Boxplot de Residuos",x="",y="Valor observado - Prediccion") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))
grid.arrange(y.res.hist,y.real.bp,nrow=1)