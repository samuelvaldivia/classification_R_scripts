##
# Autor: Samuel Valdivia Mantas
# Proyecto: Trabajo de Fin de Master de Biotecnologia Avanzada de la Universidad de Malaga: "Desarrollo y comparación de rutinas de análisis y visualización de datos relacionados con la calidad y clasificación de semillas"
# Fecha: 18/09/2017
# Descripcion: Cálculo, representación y predicción de Rpart a partir de datos morfológicos de un lote de semillas.


### load required packages
require(rpart)
require(rpart.plot)

#Importar datos
source("Scripts/S01_Extraccion_datos.R")

### Desordenarlos
set.seed(42)
g <- runif(nrow(raw_data))
raw_data_desordenada <- raw_data[order(g),]

str(raw_data_desordenada)

#Primer modelo tomando las 90%  muestras sin label
ind <- sample(2,nrow(raw_data_desordenada),replace=TRUE,prob=c(0.9,0.1))
trainData2 <- raw_data_desordenada[ind==1,]
testData2  <- raw_data_desordenada[ind==2,]

## RPART
require(rpart)
require(rpart.plot)

# Seleccion
m3 <- rpart(Label ~ ., data = trainData2, method = "class") #metodo de clasificacion
m3
rpart.plot(m3, type =3, extra = 101, fallen.leaves = T)


#Prediction
p3 <- predict(m3, testData2, type = "class")

table(testData2[,1], rpart_Predicted = p3)