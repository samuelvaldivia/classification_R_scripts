##
# Autor: Samuel Valdivia Mantas
# Proyecto: Trabajo de Fin de Master de Biotecnologia Avanzada de la Universidad de Malaga: "Desarrollo y comparación de rutinas de análisis y visualización de datos relacionados con la calidad y clasificación de semillas"
# Fecha: 18/09/2017
# Descripcion: Análisis exploratorio para lote de semillas. Datos estadísticos y graficas ggplot, ggjoy, stripchart, scatterplot.


#Importar datos
library(readr)
source("Scripts/S01_Extraccion_datos.R")

#######################################################################################################

head(raw_data)
summary(raw_data)
str(raw_data)

######################################################################################################
#lapply y sapply

lapply(raw_data[,-1], mean)
#medias de las areas de cada Label
sapply(unique(raw_data$Label), function(tipo) mean(raw_data$Area[raw_data$Label == tipo]))
#medias de las perimetro de cada Label
sapply(unique(raw_data$Label), function(tipo) mean(raw_data$Perim[raw_data$Label == tipo]))
#...
######################################################################################################
#histogramas

library(ggplot2)
ggplot(raw_data, aes(Area, fill = Label)) +
  geom_histogram(binwidth = 1, mapping = )

shapedata <- reshape2::melt(raw_data)

ggplot(shapedata, aes(value, fill = Label)) + facet_wrap(~variable, scales = 'free_x' , ncol = 3) +  geom_histogram( )

######################################################################################################
#boxplot 

ggplot(raw_data, aes(x=Label, y=Area, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=Perim, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=Circ, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=MinFeret, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=AR, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=Round, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=Solidity, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=Feret, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=FeretX, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=FeretY, group=Label)) +   geom_boxplot(aes(fill=Label))
ggplot(raw_data, aes(x=Label, y=MinFeret, group=Label)) +   geom_boxplot(aes(fill=Label))

######################################################################################################

#Stripchart  
library("ggpubr")

# Stripchart colored by groups: Label
ggstripchart(raw_data, x = "Label", y = "Area", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "Perim", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "Circ", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "Feret", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "FeretX", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "FeretY", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "FeretAngle", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "MinFeret", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "AR", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "Round", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro
ggstripchart(raw_data, x = "Label", y = "Solidity", color = "Label", add = "mean_sd", size = 0.3) # ponerla en negro



stripcharts <- ggstripchart(raw_data, x = "Label",
                            y = c("Area","Perim","Circ",  "Feret", "FeretX", "FeretY", "FeretAngle", "MinFeret", "AR", "Round", "Solidity"),
                            combine = TRUE, ncol = 2, font.label = 4 ,
                            color = "Label", 
                            size = 0.1, jitter = 0.4, pointsize = 0.2, scales = "free",
                            ylab = "Expresión", xlab = F, legend = 0, 
                            add = "median_iqr",
                            title =   "Análisis exploratorio" ,
                            add.params = list(color = "black")) 

stripcharts 


######################################################################################################
#Análisis discriminatorio

library(MASS)
entrenamiento.lda <- lda(Label ~ ., data=raw_data)
#For convenience, the value for each discriminant function  are scaled so that their mean value is zero and its variance is one.
entrenamiento.lda
# scatterplot of the best two discriminant functions, with the data points labelled by LABEL, by typing:
# make a stacked histogram of the first discriminant function’s values

entrenamiento.lda.values <- predict(entrenamiento.lda)
ldahist(data = entrenamiento.lda.values$x[,1], g=raw_data$Label)
ldahist(data = entrenamiento.lda.values$x[,2], g=raw_data$Label)
ldahist(data = entrenamiento.lda.values$x[,3], g=raw_data$Label)

#scaterplot variables 1 y 2
plot(entrenamiento.lda.values$x[,1],entrenamiento.lda.values$x[,2]) # make a scatterplot
text(entrenamiento.lda.values$x[,1],entrenamiento.lda.values$x[,2],raw_data$Label,cex=0.7,pos=4,col="red") # add labels


#######################################################################################################

#MANOVA

library("dplyr")
set.seed(42)

#Label aleatoria
dplyr::sample_n(raw_data, 10)

Area <- raw_data$Area
Perim <- raw_data$Perim
# MANOVA test
res.man <- manova(cbind(Area,Perim) ~ Label, data = raw_data)
summary(res.man)

# Look to see which differ
summary.aov(res.man)

#######################################################################################################

#Scatter plot with marginal density plots

library("ggpubr")

# Scatter plot colored by groups ("Label")
sp <- ggscatter(raw_data, x = "Area", y = "Perim",
                color = "Label",   
                size = 0.5)+
  border()                                         
# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggdensity(raw_data, "Area", fill = "Label" )
yplot <- ggdensity(raw_data, "Perim", fill = "Label")+
  rotate()
# Cleaning the plots
yplot <- yplot + clean_theme() 
xplot <- xplot + clean_theme()
# Arranging the plot
ggarrange(xplot, NULL, sp, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)



#######################################################################################

#ggjoy

library(ggplot2)
library(ggjoy)

 
ggjoy1 <-ggplot(raw_data, aes(x = Area, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none") 

ggjoy2 <-ggplot(raw_data, aes(x = Perim, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none")
ggjoy3 <-ggplot(raw_data, aes(x = Circ, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none")
ggjoy4 <-ggplot(raw_data, aes(x = Feret, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none")
ggjoy5 <-ggplot(raw_data, aes(x = FeretX, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none")

ggjoy6 <-ggplot(raw_data, aes(x = FeretY, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none")

ggjoy7 <-ggplot(raw_data, aes(x = FeretAngle, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none")
ggjoy8 <-ggplot(raw_data, aes(x = MinFeret, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none")
ggjoy9 <-ggplot(raw_data, aes(x = AR, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none")
ggjoy10 <-ggplot(raw_data, aes(x = Round, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none")
ggjoy11 <-ggplot(raw_data, aes(x = Solidity, y = Label, fill = Label, height = ..density..)) +
  geom_joy(scale = 4) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy() + theme(legend.position = "none") 


grid.arrange(ggjoy1, ggjoy2, ggjoy3, ggjoy4,ggjoy5,ggjoy6, ggjoy7, ggjoy8, ggjoy9,ggjoy10,ggjoy11, ncol=2)

