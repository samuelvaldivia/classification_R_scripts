##
# Autor: Samuel Valdivia Mantas
# Proyecto: Trabajo de Fin de Master de Biotecnologia Avanzada de la Universidad de Malaga: "Desarrollo y comparación de rutinas de análisis y visualización de datos relacionados con la calidad y clasificación de semillas"
# Fecha: 18/09/2017
# Descripcion: Cálculo, representación y predicción de  C5.0 a partir de datos morfológicos de un lote de semillas


### load required packages
library(C50)
library(gmodels)

#Importar datos
source("Scripts/S01_Extraccion_datos.R")

### Desordenarlos
set.seed(42)
g <- runif(nrow(raw_data))
raw_data_desordenada <- raw_data[order(g),]
str(raw_data_desordenada)

#Primer modelo tomando las 90%  muestras sin label
ind <- sample(2,nrow(raw_data_desordenada),replace=TRUE,prob=c(0.9,0.1))
trainData <- raw_data_desordenada[ind==1,]
testData  <- raw_data_desordenada[ind==2,]

m1 <- C5.0(trainData[,-1], trainData[,1])
m1
summary(m1)

#predecir usando el modelo anterior
p1 <- predict(m1, testData[,])
p1
table(testData[,1], Predicted = p1 )

#representar el arbol de decisiones
plot(m1)