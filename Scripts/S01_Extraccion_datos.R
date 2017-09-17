##
# Autor: Samuel Valdivia Mantas
# Proyecto: Trabajo de Fin de Master de Biotecnologia Avanzada de la Universidad de Malaga: "Desarrollo y comparación de rutinas de análisis y visualización de datos relacionados con la calidad y clasificación de semillas"
# Fecha: 18/09/2017
# Descripcion: scrip para la extraccion de datos de una muestra de semillas y transformarla a un data frame para su posterior tratamiento.


# Extraccion datos
library(readr)
semillas_entrenamiento <- read_delim("Data/EntrenamientoForma1.txt",  "\t", escape_double = FALSE, trim_ws = TRUE)

# Renombrar
raw_data <- as.data.frame(semillas_entrenamiento)

# Convertir en factor label
raw_data$Label <- factor(raw_data$Label)

# Seleccion de datos numéricos
my_data <- raw_data[,-1]
head(my_data)

# Subset limpio sin FeretX FeretY FeretAngle
clean_data <- within(raw_data, rm( FeretX , FeretY , FeretAngle ))
