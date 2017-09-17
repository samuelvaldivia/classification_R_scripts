##
# Autor: Samuel Valdivia Mantas
# Proyecto: Trabajo de Fin de Master de Biotecnologia Avanzada de la Universidad de Malaga: "Desarrollo y comparación de rutinas de análisis y visualización de datos relacionados con la calidad y clasificación de semillas"
# Fecha: 18/09/2017
# Descripcion: calculo y representacion de t-SNE
# Reference:   

# Paquetes 
require(caret)  
require(Rtsne)
library(readr)
source("Scripts/S01_Extraccion_datos.R")

# Rtsne
set.seed(42)  

  # Crear los modelos de cluster para distintos niveles de perplejidad
t1 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 5  , theta=0.1, dims=2)
t2 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 10 , theta=0.1, dims=2)
t3 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 15 , theta=0.1, dims=2)
t4 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 20 , theta=0.1, dims=2)
t5 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 25 , theta=0.1, dims=2)
t6 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 30 , theta=0.1, dims=2)
t7 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 35 , theta=0.1, dims=2)
t8 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 40 , theta=0.1, dims=2)
t9 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 45 , theta=0.1, dims=2)
t10 =Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 50 , theta=0.1, dims=2)
t11 =Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 55 , theta=0.1, dims=2)

t0 = Rtsne(as.matrix(my_data[,-1]), check_duplicates=FALSE, pca=TRUE, perplexity= 1  , theta=0 , dims=2)

#Representar modelos cluster

ggplot(as.data.frame(t1$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()

ggplot(as.data.frame(t2$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


ggplot(as.data.frame(t3$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


ggplot(as.data.frame(t4$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


ggplot(as.data.frame(t5$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


ggplot(as.data.frame(t6$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


ggplot(as.data.frame(t7$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


ggplot(as.data.frame(t8$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


ggplot(as.data.frame(t9$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


ggplot(as.data.frame(t10$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


ggplot(as.data.frame(t11$Y), aes(x=V1, y=V2,color=raw_data$Label)) +  
  geom_point(size=1) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("t-SNE") + theme_minimal()


  