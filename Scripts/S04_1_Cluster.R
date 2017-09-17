##
# Autor: Samuel Valdivia Mantas
# Proyecto: Trabajo de Fin de Master de Biotecnologia Avanzada de la Universidad de Malaga: "Desarrollo y comparación de rutinas de análisis y visualización de datos relacionados con la calidad y clasificación de semillas"
# Fecha: 18/09/2017
# Descripcion: Análisis de cluster jerárquico(hclust, AGNES y DIANA) y no jerárquico(kmeans, PAM y CLARA). Hibrido( pca + kmeans) y pvclust.

#1 cargar datos y escalar
set.seed(42)

#paquetes
library("ggplot2")
library("FactoMineR")
library("factoextra")
library("cluster")
library("dendextend")
library("ggdendro")
library("readr")

#cargar datos
source("Scripts/S01_Extraccion_datos.R")

# 1 CLUSTER JERARQUICO

# Compute dendograma distancias correlacion spearman
res.hc <-    eclust(my_data , stand = T, k = 4,  "hclust" , hc_metric = "spearman", hc_method = "ward.D2") # compute hclust
res.agnes <- eclust(my_data , stand = T, k = 4,  "agnes"  , hc_metric = "spearman", hc_method = "ward.D2") # compute agnes
res.diana <- eclust(my_data , stand = T, k = 4,  "diana"  , hc_metric = "spearman", hc_method = "ward.D2") # compute diana..

#plots
fviz_dend(res.hc, rect = TRUE, show_labels = FALSE,
          main = "hclust Cluster Dendrogram method spearman", xlab = "Semillas", ylab = "Altura") 

fviz_dend(res.agnes, rect = TRUE, show_labels = FALSE,
          main = "AGNES Cluster Dendrogram method spearman", xlab = "Semillas", ylab = "Altura") 

fviz_dend(res.diana, rect = TRUE, show_labels = FALSE,
          main = "DIANA Cluster Dendrogram method spearman", xlab = "Semillas", ylab = "Altura") 


####cuttree
grp.hc    <- cutree(res.hc, k = 4)
grp.agnes <- cutree(as.hclust(res.agnes), k = 4)
grp.diana <- cutree(as.hclust(res.diana), k = 4)


#Tablas de confusion  real/predicho spearman
table(raw_data$Label,grp.hc)
table(raw_data$Label,grp.agnes)
table(raw_data$Label,grp.diana)


#Distancia correlacion pearson
res.hc2 <-    eclust(my_data , stand = T, k = 4,  "hclust" , hc_metric = "pearson", hc_method = "ward.D2") # compute hclust
res.agnes2 <- eclust(my_data , stand = T, k = 4,  "agnes"  , hc_metric = "pearson", hc_method = "ward.D2") # compute agnes
res.diana2 <- eclust(my_data , stand = T, k = 4,  "diana"  , hc_metric = "pearson", hc_method = "ward.D2") # compute diana

#plots
fviz_dend(res.hc2, rect = TRUE, show_labels = FALSE,
          main = "hclust Cluster Dendrogram method pearson", xlab = "Semillas", ylab = "Altura") 

fviz_dend(res.agnes2, rect = TRUE, show_labels = FALSE,
          main = "AGNES Cluster Dendrogram method pearson", xlab = "Semillas", ylab = "Altura") 

fviz_dend(res.diana2, rect = TRUE, show_labels = FALSE,
          main = "DIANA Cluster Dendrogram method pearson", xlab = "Semillas", ylab = "Altura") 


####cuttree
grp.hc2    <- cutree(res.hc2, k = 4)
grp.agnes2 <- cutree(as.hclust(res.agnes2), k = 4)
grp.diana2 <- cutree(as.hclust(res.diana2), k = 4)


#Tablas de confusion  real/predicho pearson
table(raw_data$Label,grp.hc2)
table(raw_data$Label,grp.agnes2)
table(raw_data$Label,grp.diana2)


# 2 CLUSTER NO JERARQUICO

#Calculo de cluster

res.km <-    eclust(my_data, stand = T, k = 4,  "kmean", graph = F ) #Kmeans
res.pam <-   eclust(my_data, stand = T, k = 4, "pam"  , graph = F )  #PAM
res.clara <- eclust(my_data, stand = T, k = 4, "clara" , graph = F ) #CLARA

#Representaciones
fviz_cluster(res.km,    data = my_data , pointsize = 0.3, labelsize = 0, main = "K-Mean Cluster")
fviz_cluster(res.pam,   data = my_data , pointsize = 0.3, labelsize = 0, main = "PAM Cluster")
fviz_cluster(res.clara, data = my_data , pointsize = 0.3, labelsize = 0, main = "CLARA Cluster")

#silueta
fviz_silhouette(res.km)
fviz_silhouette(res.pam)
fviz_silhouette(res.clara)

#tablas de confusion no jerarquico
table(raw_data$Label,res.km$cluster)
table(raw_data$Label,res.pam$cluster)
table(raw_data$Label,res.clara$cluster)

## otros

#Cluster Jerarquico de componentes principales: PCA -> Cluster

pca <- PCA(my_data_pca, graph=FALSE, scale.unit = TRUE)
hcpc <- HCPC(pca, min = 3, max=10, iter.max=10, graph=FALSE)
plot(hcpc, labels= 0)

#pvclust 

library(pvclust)
fit <- pvclust(scale(na.omit(my_data)), method.hclust="ward.D2", method.dist = "correlation", nboot=10000) #probar con ward.D2
plot(fit,print.pv = TRUE, print.num=FALSE, col.pv=c("purple","green","white"))

