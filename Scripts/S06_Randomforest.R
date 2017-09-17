##
# Autor: Samuel Valdivia Mantas
# Proyecto: Trabajo de Fin de Master de Biotecnologia Avanzada de la Universidad de Malaga: "Desarrollo y comparación de rutinas de análisis y visualización de datos relacionados con la calidad y clasificación de semillas"
# Fecha: 18/09/2017
# Descripcion: Cálculo y predicción de RandomForest a partir de datos morfológicos de un lote de semillas. Representaciones con Randomforestexplainer

#Paquetes
library(readr)
library(randomForest)
set.seed(42) 

#Importar datos
source("~/Scripts/S01_Extraccion_datos.R")
#
## Desordenarlos
g <- runif(nrow(raw_data))
raw_data_desordenada <- as.data.frame(raw_data[order(g),])

#Dividir set de datos en train y test
ind <- sample(2,nrow(raw_data_desordenada),replace=TRUE,prob=c(0.9,0.1))
trainData <- raw_data_desordenada[ind==1,]
testData  <- raw_data_desordenada[ind==2,]

#Generar arbol aprendizaje Random Forest learning 
sem_rf <- randomForest(Label~., data = trainData , mtry= 2, ntree=500 ,proximity=TRUE)
table(predict(sem_rf),trainData$Label)

#Print Random Forest model e importancia variables
print(sem_rf)
plot(sem_rf)
importance(sem_rf)
varImpPlot(sem_rf)

#Construir Rf para test 
semPred<-predict(sem_rf,newdata=testData)
table(semPred, testData$Label)

#Ver el margen, positivo or negativo, si es positivo se trata de una clasificacion correcta
library(RColorBrewer)
plot(margin(sem_rf,testData$Label))

#Try to tune Random Forest
tune.rf <- tuneRF(raw_data_desordenada[,-1],raw_data_desordenada[,1], doBest = T )
print(tune.rf)

(59    + 75  + 128 +  124)/nrow(testData)


#random forest explainer
#crear random forest con localImp = T

library("randomForestExplainer")
set.seed(42)

#crear bosque
forest <- randomForest(Label~., data = trainData , mtry= 2, ntree=500 , localImp = TRUE)

# Correlacion variables
forest_stats <- measure_importance(forest,mean_sample = "relevant_trees")
plot_importance_ggpairs(forest_stats)

#Distribucion media de profundidad minima
forest_frame <-min_depth_distribution(forest)
plot_min_depth_distribution(forest_frame)

# 30 principales interacciones
forest_interactions <- min_depth_interactions(forest)
plot_min_depth_interactions(forest_interactions)