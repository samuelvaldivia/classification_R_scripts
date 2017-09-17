##
# Autor: Samuel Valdivia Mantas
# Proyecto: Trabajo de Fin de Master de Biotecnologia Avanzada de la Universidad de Malaga: "Desarrollo y comparación de rutinas de análisis y visualización de datos relacionados con la calidad y clasificación de semillas"
# Fecha: 18/09/2017
# Descripcion: Cálculo y predicción de k-NN a partir de datos morfológicos de un lote de semillas

#cargar bibliotecas
library(class)

#Importar datos
source("Scripts/S01_Extraccion_datos.R")
head(raw_data)

### Desordenarlos
set.seed(42)
g <- runif(nrow(raw_data))
raw_data_desordenada <- as.data.frame(raw_data[order(g),])

head(raw_data_desordenada)
str(raw_data_desordenada)
summary(raw_data_desordenada[,c(2,3,4,5,6,7,8,9,10,11,12)])

#como knn usa distancias necesitamos escalar los datos. (normalizar o escalar)
normalizar <- function(x) {
  + return((x -min(x)) / (max(x)) ) }

data_knn <- as.data.frame(lapply(raw_data_desordenada[,-1], normalizar))

head(data_knn)
str(data_knn)
summary(data_knn)

#Dividir set de datos en train y test
ind <- sample(2,nrow(raw_data_desordenada),replace=TRUE,prob=c(0.9,0.1))
knn_train <- data_knn[ind==1, ]
knn_test  <- data_knn[ind==2, ]

knn_train_target <-  raw_data_desordenada[ind==1,1]
knn_test_target <-  raw_data_desordenada[ind==2,1]

model1 <- knn(train = knn_train, test = knn_test, cl= knn_train_target)
model1

table(knn_test_target, model1)

