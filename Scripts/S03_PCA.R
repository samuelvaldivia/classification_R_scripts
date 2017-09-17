##
# Autor: Samuel Valdivia Mantas
# Proyecto: Trabajo de Fin de Master de Biotecnologia Avanzada de la Universidad de Malaga: "Desarrollo y comparación de rutinas de análisis y visualización de datos relacionados con la calidad y clasificación de semillas"
# Fecha: 18/09/2017
# Descripcion: Análisis de componentes principales y representación.

# Paquetes

library("FactoMineR")
library("factoextra")
library("ggplot2")

#1 cargar datos y escalar

set.seed(42)

library(readr)
source("Scripts/S01_Extraccion_datos.R")

#Data
#raw_data <- data con LABEL
my_data_pca <- my_data

#PCA
res.pca <- PCA(my_data_pca,  graph = FALSE)
get_eig(res.pca) # Extract eigenvalues/variances

# Visualizar eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 70))

##Con variables
var <- get_pca_var(res.pca)   # Extract the results for variables
var

head(var$coord)# coordenadas de variables
head(var$contrib)# contribuciones de variables   

# Gráfica variables  
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE )# Avoid text overlapping

# Contribuciones de variables a PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 11)
# Contribuciones de variables a PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 11)

##Con individuos
# Extract the results for individuals
ind <- get_pca_ind(res.pca)
ind

# Coordinates of individuals
head(ind$coord)	

#Colorear individuos por grupo

fviz_pca_ind(res.pca,  label="none", habillage=raw_data$Label, pointsize = 0.5)

#con elipses coloreadas
fviz_pca_biplot(res.pca, 
                habillage = raw_data$Label, addEllipses = TRUE,
                col.var = "red", alpha.var ="cos2",geom = "polygon",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()

