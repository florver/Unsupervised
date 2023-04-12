
######## Exploracion variables

library(readxl)
#Career_Mode_player_datasets_FIFA_15_21 <- read_excel("C:/Users/nkraselnik/Desktop/Maestria/Aprendizaje No Supervisado/TP/BaseDeDatos/Career Mode player datasets - FIFA 15-21.xlsx", 
#                                                         sheet = "FIFA 21")

Career_Mode_player_datasets_FIFA_15_21 <- read_excel("C:/Users/flore/OneDrive/Escritorio/FLOR/Facultad UDESA/Maestría Cs. de Datos/MCD08 - Aprendizaje No Supervisado/Trabajo Final/Career Mode player datasets - FIFA 15-21.xlsx", 
                                                     sheet = "FIFA 21")

#setwd(choose.dir())
#getwd()
#Career_Mode_player_datasets_FIFA_15_21=read.csv("C:/Users/Asus/Documents/Maestria Ciencia de Datos - UDESA/Regresión Avanzada/players_21.csv")

library(dplyr)
bd = Career_Mode_player_datasets_FIFA_15_21 %>% 
  filter(league_name == 'Spain Primera Division')

bd = bd %>% select (short_name, player_positions,overall,attacking_crossing,attacking_finishing,
                     ###age,height_cm, weight_kg,
                    attacking_heading_accuracy,attacking_short_passing,attacking_volleys,skill_dribbling,
                    skill_curve,skill_fk_accuracy,skill_long_passing,skill_ball_control,movement_acceleration,
                    movement_sprint_speed,movement_agility,movement_reactions,movement_balance,power_shot_power,
                    power_jumping,power_stamina,power_strength,power_long_shots,mentality_aggression,
                    mentality_interceptions,mentality_positioning,mentality_vision,mentality_penalties,
                    mentality_composure,defending_standing_tackle,defending_sliding_tackle,
                    goalkeeping_diving,goalkeeping_handling,goalkeeping_kicking,goalkeeping_positioning,
                    goalkeeping_reflexes)

summary(bd)

stacked_bd <- stack(bd[,-1:-2])

library(ggplot2)
ggplot(data = stacked_bd, aes(y = as.numeric(values),x=as.factor(ind))) + 
  geom_boxplot(aes(fill = ind), width=0.2)+
  theme(axis.text.x = element_text(angle = 90))+
  xlab('Features')+
  ylab('')+
  ggtitle('Features')+
  theme(legend.position = 'none')+
  coord_flip()


####################################################
################## Componentes Principales##########
####################################################

library(FactoMineR)
library(factoextra)
library(xtable)
View(bd)
bd_2 = bd[,-1:-2]
res.pca <- PCA(bd_2, graph = FALSE)

# Grafico autovalores
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100), main="", xlab="Dimensiones", ylab="Porcetaje de var.", barfill = "#005391", title ='PCA')
      #Comentario: sacando edad, pero y altura el componente principal explica mejor

# Contribución de la variable a la componente
res.pca$var$contrib
    

# Correlacion entre variables y componentes

library("corrplot")
corrplot(res.pca$var$cor, is.corr=FALSE)
  #Cometario: La mayoria de las variables se explican con el primer componente

#Gráfico de las proyecciones de los datos a las dos primeras componentes
fviz_pca_ind(res.pca, title="", geom="point")

# Biplot
fviz_pca_biplot(res.pca) ### --> buscar como graficar una linea y extraer los arqueros (puntos de la izquierda)
#Se ve en el grafico y en el de correlacion como las variables de goalkeapper son correlacionadas opuestas al resto

fviz_pca_biplot(res.pca, label ="var")


# Gráfico contribución de las variables a la primer componente
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10, title="Contribución a la primer componente", fill = "#9B33F9")

# Gráfico contribución de las variables a la segunda componente
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10, title="Contribución a la segunda componente",fill ="#9B33F9")

#### ----->>>>> se identifica rapidamente el cluster de los arqueros
#### por lo que excluimos los arqueros

# arqueros = Dim1<-7.5
res.pca$ind$coord
bd$Dim1 = res.pca$ind$coord[,1]
bd$Dim2 = res.pca$ind$coord[,2]
library(dplyr)
bd_3 = bd %>% filter(Dim1 >=-7.5)

# volvemos a correr PCA

res.pca.2 <- PCA(bd_3[,-1:-2], graph = FALSE)

fviz_eig(res.pca.2, addlabels = TRUE, ylim = c(0, 100), main="", xlab="Dimensiones", ylab="Porcetaje de var.")
    #vemos que cae el poder de explicar de la primer componente quitando arqueros
corrplot(res.pca.2$var$cor, is.corr=FALSE)

fviz_pca_ind(res.pca.2, title="", geom="point")
    #Detectamos una posible separacion arriba a la derecha. Pero leve

fviz_pca_biplot(res.pca.2) 
####################################################
################## k-means##########################
####################################################

# Definimos el k 

par(mfrow=c(1,3))
fviz_nbclust(bd_2, kmeans, method = "wss") +
  labs(subtitle = "FIFA players")+
  xlab('Número de clusters K')+ 
  ylab('Suma de los cuadrados (w)') +
  labs(title ='Número de clusters óptimo') 

fviz_nbclust(bd_2, kmeans, method = "silhouette") +#2
labs(subtitle = "FIFA players")+
  xlab('Número de clusters K')+ 
  ylab('Avg silhouette (w)') +
  labs(title ='Número de clusters óptimo') 

fviz_nbclust(bd_2, kmeans, method = "gap_stat") +
  labs(subtitle = "FIFA players")+ #4
 xlab('Número de clusters K')+ 
  labs(title ='Número de clusters óptimo')

# corremos k means

k_means_3 = kmeans(bd_2, 3)
bd_k_3 = cbind(bd[,1:2], k_means_3$cluster)

k_means_5 = kmeans(bd_2, 5)
bd_k_5 = cbind(bd[,1:2], k_means_5$cluster)

write.excel(bd_k_4)

k_means_4 = kmeans(bd_2, 4) 
bd_k_4 = cbind(bd[,1:2], k_means_4$cluster) ### Lo volvi a correr y puede ser una opcion. Ver


fviz_cluster(k_means_4, data=bd_2, geom = c('point')
             , main = 'K-Means', show.clust.cent = TRUE,
             ggtheme = theme_minimal())



# Plot de cluster con k=4
k_plot1= cbind(bd$Dim1, bd$Dim2,k_means_4$cluster,bd$short_name)
k_plot1 = as.data.frame(k_plot1)
colnames(k_plot1) = c("Dim1","Dim2", "cluster", "short_name")




plot(x=k_plot1$Dim1, y = k_plot1$Dim2)
p<-ggplot(k_plot1,aes(x=Dim1,y=Dim2,color=cluster ))
p<-p+geom_point()
p



k_plot1.1= cbind(bd$skill_ball_control, bd$skill_dribbling,k_means_4$cluster,bd$short_name)
k_plot1.1 = as.data.frame(k_plot1.1)
colnames(k_plot1.1) = c("skill_ball_control","skill_dribbling", "cluster", "short_name")

ggplot() +
  geom_point(data = k_plot1.1, 
             mapping = aes(x = skill_ball_control, 
                           y = skill_dribbling, 
                           colour = cluster)) 

ggplot(k_plot1.1, aes(x= skill_ball_control, y= skill_dribbling, colour = cluster, 
                      label=short_name))+
  geom_point() +geom_text(aes(label=short_name),hjust=0, vjust=0)



####################################################
################## Jerarquicos##########################
####################################################
fviz_nbclust(bd_2, hcut, method = "wss", hc_method="complete") +
  labs(subtitle = "FIFA players")  # entre 3, 4 o 5

fviz_nbclust(bd_2, hcut, method = "silhouette", hc_method="complete") +
  labs(subtitle = "FIFA players") #2

fviz_nbclust(bd_2, hcut, method = "gap_stat", hc_method="complete") +
  labs(subtitle = "FIFA players") #6

# Dendograma AGLOMERATIVO
d = dist(bd_2)
## Método Single
 #Calculamos la matriz de distancias
hclust1 = hclust(d, method="single")
fviz_dend(hclust1, cex=1.5)  #Rompe todo

# Cortamos el árbol

hclust1_cut = cutree(hclust1, k=4)
dendograma_1 = data.frame(bd[,1:2], clus=hclust1_cut) #solamente identifica bien a los arqueros

## Método complete
hclust2 = hclust(d, method="complete")
fviz_dend(hclust2, cex=1.5)  #Se ven unos cuatro grupos bien

# Cortamos el árbol

hclust2_cut = cutree(hclust2, k=4)
dendograma_2 = data.frame(bd[,1:2], clus=hclust2_cut) #Clasifica muy bien !!! --> TOMARIA ESTE

write.excel(dendograma_2)


fviz_dend(hclust2, cex=1.5, k = 4,  rect = TRUE,  
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          rect_border = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"), 
          rect_fill = TRUE, 
          show_labels = FALSE, main = 'Dendograma - K = 4')

## Método average
hclust3 = hclust(d, method="average")
fviz_dend(hclust3, cex=1.5)  # Se puedne ver 4 o 6 cluster claros

# Cortamos el árbol

hclust3_cut = cutree(hclust3, k=4)
dendograma_3 = data.frame(bd[,1:2], clus=hclust3_cut) 

write.excel(dendograma_3) #Uno de los grupos rompe dejando un solo nombre, ver de subir k

## Método centroid
hclust4 = hclust(d, method="centroid")
fviz_dend(hclust4, cex=1.5)  # Se puedne ver 4 o 6 cluster claros

# Cortamos el árbol

hclust4_cut = cutree(hclust4, k=4)
dendograma_4 = data.frame(bd[,1:2], clus=hclust4_cut) 

write.excel(dendograma_4) #Uno de los grupos rompe dejando un solo nombre


# Dendograma DIVISIVO

hclust5 = hclust(d, method="ward.D",)
fviz_dend(hclust5, scale="none")  #Ver este como le pongo cuantos cluster quiero

hclust5$


### papelon para BORRAR!!!!
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


###DBSCAN#####

install.packages("fpc")
install.packages("dbscan")
library(fpc)
library(dbscan)

kNNdist(bd_2, k = 4) # calcula las distancias al 4to vecino mas cercano sin ordenar
kNNdistplot(bd_2, k =  4) # hace el plot con las distancias ordenadas de menos a mayor 
abline(h=50, lty=2)#definimos eps donde cambia la pendiente

scan_fpc = dbscan(bd_2, eps = 50, MinPts = 10)


plot(bd_2, col=scan_fpc$cluster)

scan_fpc
bd_dbscan = cbind(bd[,1:2], scan_fpc$cluster)
bd_dbscan


##DBSCAN sin arqueros###


kNNdist(bd_3[,-1:-2], k = 3) # calcula las distancias al 4to vecino mas cercano sin ordenar
kNNdistplot(bd_3[,-1:-2], k =  3) # hace el plot con las distancias ordenadas de menos a mayor 
abline(h=48, lty=2)#definimos eps donde cambia la pendiente

scan_fpc_singk = dbscan(bd_3[,-1:-2], eps = 50, MinPts = 3)

bd_3
plot(bd_3, col=scan_fpc$cluster)

scan_fpc_singk
bd_dbscan = cbind(bd_3[,1:2], scan_fpc_singk$cluster)
bd_dbscan_0=bd_dbscan %>% filter (scan_fpc_singk$cluster==0)
bd_dbscan_0


bd_4=cbind(bd_3,scan_fpc_singk$cluster)
bd_4
bd_4_0=bd_4 %>% filter (scan_fpc_singk$cluster==0)

ggplot(bd_4_0, aes(x= skill_ball_control, y= skill_dribbling, 
                      label=short_name))+
  geom_point() +geom_text(aes(label=short_name),hjust=0, vjust=0)



#########################LIGA ARGENTINA#########################################
################################################################################


library(readxl)
#Career_Mode_player_datasets_FIFA_15_21 <- read_excel("C:/Users/nkraselnik/Desktop/Maestria/Aprendizaje No Supervisado/TP/BaseDeDatos/Career Mode player datasets - FIFA 15-21.xlsx", 
#                                                         sheet = "FIFA 21")

#Career_Mode_player_datasets_FIFA_15_21 <- read_excel("C:/Users/flore/OneDrive/Escritorio/FLOR/Facultad UDESA/Maestría Cs. de Datos/MCD08 - Aprendizaje No Supervisado/Trabajo Final/Career Mode player datasets - FIFA 15-21.xlsx", 
#                                                     sheet = "FIFA 21")

setwd(choose.dir())
getwd()
#Career_Mode_player_datasets_FIFA_15_21=read_excel("C:/Users/flore/OneDrive/Escritorio/FLOR/Facultad UDESA/Maestría Cs. de Datos/MCD08 - Aprendizaje No Supervisado/Trabajo Final/Career Mode player datasets - FIFA 15-21.xlsx", 
#                                                                                           sheet = "FIFA 21")

library(dplyr)
bd_arg = Career_Mode_player_datasets_FIFA_15_21 %>% 
  filter(league_name == 'Argentina Primera División')

bd_arg = bd_arg %>% select (short_name, player_positions,overall,attacking_crossing,attacking_finishing,
                    attacking_heading_accuracy,attacking_short_passing,attacking_volleys,skill_dribbling,
                    skill_curve,skill_fk_accuracy,skill_long_passing,skill_ball_control,movement_acceleration,
                    movement_sprint_speed,movement_agility,movement_reactions,movement_balance,power_shot_power,
                    power_jumping,power_stamina,power_strength,power_long_shots,mentality_aggression,
                    mentality_interceptions,mentality_positioning,mentality_vision,mentality_penalties,
                    mentality_composure,defending_standing_tackle,defending_sliding_tackle,
                    goalkeeping_diving,goalkeeping_handling,goalkeeping_kicking,goalkeeping_positioning,
                    goalkeeping_reflexes)

summary(bd_arg)

stacked_bd_arg <- stack(bd_arg[,-1:-2])

ggplot(data = stacked_bd_arg, aes(y = as.numeric(values),x=as.factor(ind))) + 
  geom_boxplot(aes(fill = ind), width=0.2)+
  theme(axis.text.x = element_text(angle = 90))+
  xlab('Features - Copa Argentina')+
  ylab('')+
  ggtitle('Features')+
  theme(legend.position = 'none')+
  coord_flip()


####################################################
################## Componentes Principales##########
####################################################

library(FactoMineR)
library(factoextra)
library(xtable)
View(bd_arg)
bd_2_arg = bd_arg[,-1:-2]
res.pca.arg <- PCA(bd_2_arg, graph = FALSE)

# Grafico autovalores
fviz_eig(res.pca.arg, addlabels = TRUE, ylim = c(0, 100), main="", xlab="Dimensiones", ylab="Porcetaje de var.")
#Comentario: sacando edad, pero y altura el componente principal explica mejor

# Contribución de la variable a la componente
res.pca.arg$var$contrib


# Correlacion entre variables y componentes

library("corrplot")
corrplot(res.pca.arg$var$cor, is.corr=FALSE)
#Cometario: La mayoria de las variables se explican con el primer componente

#Gráfico de las proyecciones de los datos a las dos primeras componentes
fviz_pca_ind(res.pca.arg, title="", geom="point")

# Biplot
fviz_pca_biplot(res.pca.arg) ### --> buscar como graficar una linea y extraer los arqueros (puntos de la izquierda)
#Se ve en el grafico y en el de correlacion como las variables de goalkeapper son correlacionadas opuestas al resto


# Gráfico contribución de las variables a la primer componente
fviz_contrib(res.pca.arg, choice = "var", axes = 1, top = 10, title="Contribución a la primer componente")

# Gráfico contribución de las variables a la segunda componente
fviz_contrib(res.pca.arg, choice = "var", axes = 2, top = 10, title="Contribución a la segunda componente")

#### ----->>>>> se identifica rapidamente el cluster de los arqueros
#### por lo que excluimos los arqueros

# arqueros = Dim1<-7.5
res.pca$ind$coord
bd_arg$Dim1 = res.pca.arg$ind$coord[,1]
bd_arg$Dim2 = res.pca.arg$ind$coord[,2]
library(dplyr)
bd_3_arg = bd_arg %>% filter(Dim1 >=-7.5)

# volvemos a correr PCA

res.pca.2.arg <- PCA(bd_3_arg[,-1:-2], graph = FALSE)

fviz_eig(res.pca.2.arg, addlabels = TRUE, ylim = c(0, 100), main="", xlab="Dimensiones", ylab="Porcetaje de var.")
#vemos que cae el poder de explicar de la primer componente quitando arqueros
corrplot(res.pca.2.arg$var$cor, is.corr=FALSE)

fviz_pca_ind(res.pca.2.arg, title="", geom="point")
#Detectamos una posible separacion arriba a la derecha. Pero leve

fviz_pca_biplot(res.pca.2.arg) 
####################################################
################## k-means##########################
####################################################

# Definimos el k 


fviz_nbclust(bd_2_arg, kmeans, method = "wss") +
  labs(subtitle = "FIFA players") #4

fviz_nbclust(bd_2_arg, kmeans, method = "silhouette") +
  labs(subtitle = "FIFA players")  #2

fviz_nbclust(bd_2_arg, kmeans, method = "gap_stat") +
  labs(subtitle = "FIFA players") #7

# corremos k means

k_means_4_arg = kmeans(bd_2_arg, 4)
bd_k_4_arg = cbind(bd_arg[,1:2], k_means_4_arg$cluster)

k_means_2_arg = kmeans(bd_2_arg, 2)
bd_k_2_arg = cbind(bd_arg[,1:2], k_means_2$cluster)

write.excel(bd_k_4)

k_means_7_arg = kmeans(bd_2_arg, 7) 
bd_k_7_arg = cbind(bd_arg[,1:2], k_means_7$cluster) ### Lo volvi a correr y puede ser una opcion. Ver



# Plot de cluster con k=4
k_plot1_arg= cbind(bd_arg$Dim1, bd_arg$Dim2,k_means_4_arg$cluster,bd_arg$short_name)
k_plot1_arg = as.data.frame(k_plot1_arg)
colnames(k_plot1_arg) = c("Dim1","Dim2", "cluster", "short_name")


plot(x=k_plot1$Dim1, y = k_plot1$Dim2)
p<-ggplot(k_plot1,aes(x=Dim1,y=Dim2,color=cluster ))
p<-p+geom_point()
p


fviz_cluster(k_means_4_arg, data=bd_2_arg, geom = c('point')
             , main = 'K-Means - Copa Argentina', show.clust.cent = TRUE,
             ggtheme = theme_minimal())

write.excel(bd_k_4_arg)



k_plot1.1.arg= cbind(bd_arg$skill_ball_control, bd_arg$skill_dribbling,k_means_7_arg$cluster,bd_arg$short_name)
k_plot1.1.arg = as.data.frame(k_plot1.1.arg)
colnames(k_plot1.1.arg) = c("skill_ball_control","skill_dribbling", "cluster", "short_name")

ggplot() +
  geom_point(data = k_plot1.1.arg, 
             mapping = aes(x = skill_ball_control, 
                           y = skill_dribbling, 
                           colour = cluster)) 

ggplot(k_plot1.1.arg, aes(x= skill_ball_control, y= skill_dribbling, colour = cluster, 
                      label=short_name))+
  geom_point() +geom_text(aes(label=short_name),hjust=0, vjust=0)





# Miremos la validacion interna
df_kmeans_silhouette_arg = silhouette(k_means_4_arg$cluster, dist(bd_2_arg))
plot(df_kmeans_silhouette_arg, main='Silhouette')


# Miremos la estabilidad
df_kmeans_stability = clusterboot(bd_3_sin_sup, clustermethod=kmeansCBI,k=4)
df_kmeans_stability #todos los JACCARDS dan cercano a 1, lo que indica similaridad




####Qué pasa en la liga española sacando los mediocampistas######

bd_sin_medio = bd %>% select (short_name, player_positions,team_position,overall,attacking_crossing,attacking_finishing,
                    attacking_heading_accuracy,attacking_short_passing,attacking_volleys,skill_dribbling,
                    skill_curve,skill_fk_accuracy,skill_long_passing,skill_ball_control,movement_acceleration,
                    movement_sprint_speed,movement_agility,movement_reactions,movement_balance,power_shot_power,
                    power_jumping,power_stamina,power_strength,power_long_shots,mentality_aggression,
                    mentality_interceptions,mentality_positioning,mentality_vision,mentality_penalties,
                    mentality_composure,defending_standing_tackle,defending_sliding_tackle,
                    goalkeeping_diving,goalkeeping_handling,goalkeeping_kicking,goalkeeping_positioning,
                    goalkeeping_reflexes) %>% filter(!team_position %in% c('CDM', 'LCM', 'RCM', 'LM', 'RM', 'RDM', 'LDM','LW','RW','CAM','RES','SUB'))


distinct(bd_sin_medio,team_position)


stacked_bd_sin_medio <- stack(bd_sin_medio[,-1:-2])

library(ggplot2)
ggplot(data = stacked_bd_sin_medio, aes(y = as.numeric(values),x=as.factor(ind))) + 
  geom_boxplot(aes(fill = ind), width=0.2)+
  theme(axis.text.x = element_text(angle = 90))+
  xlab('Features')+
  ylab('')+
  ggtitle('Features')+
  theme(legend.position = 'none')+
  coord_flip()


####################################################
################## Componentes Principales##########
####################################################

library(FactoMineR)
library(factoextra)
library(xtable)
View(bd_sin_medio)
bd_2_sin_medio = bd_sin_medio[,-1:-3]
res.pca.sin.medio <- PCA(bd_2_sin_medio, graph = FALSE)

# Grafico autovalores
fviz_eig(res.pca.sin.medio, addlabels = TRUE, ylim = c(0, 100), main="", xlab="Dimensiones", ylab="Porcetaje de var.")
#Comentario: sacando edad, pero y altura el componente principal explica mejor

# Contribución de la variable a la componente
res.pca.sin.medio$var$contrib


# Correlacion entre variables y componentes

library("corrplot")
corrplot(res.pca.sin.medio$var$cor, is.corr=FALSE)
#Cometario: La mayoria de las variables se explican con el primer componente

#Gráfico de las proyecciones de los datos a las dos primeras componentes
fviz_pca_ind(res.pca.sin.medio, title="", geom="point")

# Biplot
fviz_pca_biplot(res.pca.sin.medio) ### --> buscar como graficar una linea y extraer los arqueros (puntos de la izquierda)
#Se ve en el grafico y en el de correlacion como las variables de goalkeapper son correlacionadas opuestas al resto


# Gráfico contribución de las variables a la primer componente
fviz_contrib(res.pca.sin.medio, choice = "var", axes = 1, top = 10, title="Contribución a la primer componente")

# Gráfico contribución de las variables a la segunda componente
fviz_contrib(res.pca.sin.medio, choice = "var", axes = 2, top = 10, title="Contribución a la segunda componente")

#### ----->>>>> se identifica rapidamente el cluster de los arqueros
#### por lo que excluimos los arqueros


bd_sin_medio$Dim1 = res.pca.sin.medio$ind$coord[,1]
bd_sin_medio$Dim2 = res.pca.sin.medio$ind$coord[,2]


####################################################
################## k-means##########################
####################################################

# Definimos el k 


fviz_nbclust(bd_2_sin_medio, kmeans, method = "wss") +
  labs(subtitle = "FIFA players")#4 

fviz_nbclust(bd_2_sin_medio, kmeans, method = "silhouette") +
  labs(subtitle = "FIFA players")  #2

fviz_nbclust(bd_2_sin_medio, kmeans, method = "gap_stat") +
  labs(subtitle = "FIFA players") #2

# corremos k means

k_means_4_sin_medio = kmeans(bd_2_sin_medio, 4)
bd_k_4_sin_medio = cbind(bd_sin_medio[,1:3], k_means_4_sin_medio$cluster)


k_means_3_sin_medio = kmeans(bd_2_sin_medio, 3)
bd_k_3_sin_medio = cbind(bd_sin_medio[,1:3], k_means_3_sin_medio$cluster)

k_means_2_sin_medio = kmeans(bd_2_sin_medio, 2)
bd_k_2_sin_medio = cbind(bd_sin_medio[,1:3], k_means_2_sin_medio$cluster)

write.excel(bd_k_3_sin_medio)


fviz_cluster(k_means_4_sin_medio, data=bd_2_sin_medio, geom = c('point')
             , main = 'K-Means - Sin Medio', show.clust.cent = TRUE,
             ggtheme = theme_minimal())

fviz_cluster(k_means_3_sin_medio, data=bd_2_sin_medio, geom = c('point')
             , main = 'K-Means - Sin Medio', show.clust.cent = TRUE,
             ggtheme = theme_minimal())


# Plot de cluster con k=4
k_plot1_sin_medio= cbind(bd_sin_medio$Dim1, bd_sin_medio$Dim2,k_means_3_sin_medio$cluster,bd_sin_medio$short_name)
k_plot1_sin_medio = as.data.frame(k_plot1_sin_medio)
colnames(k_plot1_sin_medio) = c("Dim1","Dim2", "cluster", "short_name")


plot(x=k_plot1_sin_medio$Dim1, y = k_plot1_sin_medio$Dim2)
p<-ggplot(k_plot1_sin_medio,aes(x=Dim1,y=Dim2,color=cluster))
p<-p+geom_point()
p



k_plot1.1.sin.medio= cbind(bd_sin_medio$skill_ball_control, bd_sin_medio$skill_dribbling,k_means_3_sin_medio$cluster,bd_sin_medio$short_name)
k_plot1.1.sin.medio = as.data.frame(k_plot1.1.sin.medio)
colnames(k_plot1.1.sin.medio) = c("skill_ball_control","skill_dribbling", "cluster", "short_name")

ggplot() +
  geom_point(data = k_plot1.1.sin.medio, 
             mapping = aes(x = skill_ball_control, 
                           y = skill_dribbling, 
                           colour = cluster)) 

ggplot(k_plot1.1.sin.medio, aes(x= skill_ball_control, y= skill_dribbling, colour = cluster, 
                      label=short_name))+
  geom_point() +geom_text(aes(label=short_name),hjust=0, vjust=0)



#####BD SIN SUPLENTES

bd_sin_sup = bd %>% select (short_name, player_positions,team_position,overall,attacking_crossing,attacking_finishing,
                    ###age,height_cm, weight_kg,
                    attacking_heading_accuracy,attacking_short_passing,attacking_volleys,skill_dribbling,
                    skill_curve,skill_fk_accuracy,skill_long_passing,skill_ball_control,movement_acceleration,
                    movement_sprint_speed,movement_agility,movement_reactions,movement_balance,power_shot_power,
                    power_jumping,power_stamina,power_strength,power_long_shots,mentality_aggression,
                    mentality_interceptions,mentality_positioning,mentality_vision,mentality_penalties,
                    mentality_composure,defending_standing_tackle,defending_sliding_tackle,
                    goalkeeping_diving,goalkeeping_handling,goalkeeping_kicking,goalkeeping_positioning,
                    goalkeeping_reflexes)%>% filter(!team_position %in% c('RES','SUB'))


bd_2_sin_sup = bd_sin_sup[,-1:-3]


####################################################
################## k-means##########################
####################################################

# Definimos el k 


fviz_nbclust(bd_2_sin_sup, kmeans, method = "wss") +
  labs(subtitle = "FIFA players")#4 

fviz_nbclust(bd_2_sin_sup, kmeans, method = "silhouette") +
  labs(subtitle = "FIFA players")  #2

fviz_nbclust(bd_2_sin_sup, kmeans, method = "gap_stat") +
  labs(subtitle = "FIFA players") #2

# PCA

#bd_2_sin_sup = bd_sin_sup[,-1:-3]
res.pca.sin.sup <- PCA(bd_2_sin_sup, graph = FALSE)


bd_2_sin_sup$Dim1 = res.pca.sin.sup$ind$coord[,1]
bd_2_sin_sup$Dim2 = res.pca.sin.sup$ind$coord[,2]

# corremos k means

k_means_4_sin_sup = kmeans(bd_2_sin_sup, 4)
bd_k_4_sin_sup = cbind(bd_sin_sup[,1:3], k_means_4_sin_sup$cluster)


#k_means_3_sin_medio = kmeans(bd_2_sin_medio, 3)
#bd_k_3_sin_medio = cbind(bd_sin_medio[,1:3], k_means_3_sin_medio$cluster)

#k_means_2_sin_medio = kmeans(bd_2_sin_medio, 2)
#bd_k_2_sin_medio = cbind(bd_sin_medio[,1:3], k_means_2_sin_medio$cluster)

write.excel(bd_k_4_sin_sup)



fviz_cluster(k_means_4_sin_sup, data=bd_2_sin_sup, geom = c('point')
             , main = 'K-Means - Sin suplentes', show.clust.cent = TRUE,
             ggtheme = theme_minimal())


# Plot de cluster con k=4
k_plot1_sin_sup= cbind(bd_2_sin_sup$skill_ball_control, bd_2_sin_sup$skill_dribbling,k_means_4_sin_sup$cluster,bd_sin_sup$short_name)
k_plot1_sin_sup = as.data.frame(k_plot1_sin_sup)
colnames(k_plot1_sin_sup) = c("Dim1","Dim2", "cluster", "short_name")


k_plot2_sin_sup= cbind(bd_2_sin_sup$skill_ball_control, bd_2_sin_sup$skill_dribbling,k_means_4_sin_sup$cluster,bd_sin_sup$short_name)
k_plot2_sin_sup = as.data.frame(k_plot2_sin_sup)
colnames(k_plot2_sin_sup) = c("skill_ball_control","skill_dribbling", "cluster", "short_name")


ggplot(k_plot2_sin_sup, aes(x= skill_ball_control, y= skill_dribbling, colour = cluster, 
                            label=short_name))+
  geom_point() +geom_text(aes(label=short_name),hjust=0, vjust=0)


#plot(x=k_plot1_sin_medio$Dim1, y = k_plot1_sin_medio$Dim2)
#p<-ggplot(k_plot1_sin_medio,aes(x=Dim1,y=Dim2,color=cluster))
#p<-p+geom_point()
#p



#k_plot1.1.sin.sup= cbind(bd_sin_sup$skill_ball_control, bd_sin_sup$skill_dribbling,k_means_4_sin_sup$cluster,bd_sin_sup$short_name)
#k_plot1.1.sin.sup = as.data.frame(k_plot1.1.sin.sup)
#colnames(k_plot1.1.sin.sup) = c("skill_ball_control","skill_dribbling", "cluster", "short_name")

#ggplot() +
#  geom_point(data = k_plot1.1.sin.sup, 
#             mapping = aes(x = skill_ball_control, 
#                           y = skill_dribbling, 
#                           colour = cluster)) 

ggplot(k_plot1_sin_sup, aes(x= skill_ball_control, y= skill_dribbling, colour = cluster, 
                               label=short_name))+
 geom_point() +geom_text(aes(label=short_name),hjust=0, vjust=0)

####PCA

# Grafico autovalores
fviz_eig(res.pca.sin.sup, addlabels = TRUE, ylim = c(0, 100), main="", xlab="Dimensiones", ylab="Porcetaje de var.", barfill = "#005391", title ='PCA - Sin suplentes')
#Comentario: sacando edad, pero y altura el componente principal explica mejor

# Contribución de la variable a la componente
res.pca.sin.sup$var$contrib


# Correlacion entre variables y componentes

library("corrplot")
corrplot(res.pca.sin.sup$var$cor, is.corr=FALSE)
#Cometario: La mayoria de las variables se explican con el primer componente

#Gráfico de las proyecciones de los datos a las dos primeras componentes
fviz_pca_ind(res.pca.sin.sup, title="", geom="point")

# Biplot
fviz_pca_biplot(res.pca.sin.sup) ### --> buscar como graficar una linea y extraer los arqueros (puntos de la izquierda)
#Se ve en el grafico y en el de correlacion como las variables de goalkeapper son correlacionadas opuestas al resto

fviz_pca_biplot(res.pca.sin.sup, label ="var", title = 'PCA - Biplot sin Suplentes')


# Gráfico contribución de las variables a la primer componente
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10, title="Contribución a la primer componente", fill = "#9B33F9")

# Gráfico contribución de las variables a la segunda componente
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10, title="Contribución a la segunda componente",fill ="#9B33F9")


###DBSCAN#####

#install.packages("fpc")
#install.packages("dbscan")
library(fpc)
library(dbscan)

kNNdist(bd_2_sin_sup, k = 4) # calcula las distancias al 4to vecino mas cercano sin ordenar
kNNdistplot(bd_2_sin_sup, k =  4) # hace el plot con las distancias ordenadas de menos a mayor 
abline(h=53, lty=2, col='red')#definimos eps donde cambia la pendiente

scan_fpc = dbscan(bd_2_sin_sup, eps = 53, MinPts = 4)
scan_fpc


bd_dbscan = cbind(bd_sin_sup, scan_fpc$cluster)
bd_dbscan

bd_3_sin_sup=bd_dbscan%>% 
               filter(scan_fpc$cluster != 0)

bd_3_sin_sup=bd_3_sin_sup[,-35:-37]

k_means_4_sin_sup_ruido = kmeans(bd_3_sin_sup, 4)
bd_k_4_sin_sup_ruido = cbind(bd_sin_sup[,1:3], k_means_4_sin_sup$cluster)

fviz_cluster(k_means_4_sin_sup_ruido, data=bd_3_sin_sup, geom = c('point', 'text')
             , main = 'K-Means - Sin suplentes & sin ruido', show.clust.cent = TRUE,
             ggtheme = theme_minimal())


########## Jerarquicos

## Método complete
hclust2_sin_sup = hclust(dist(bd_3_sin_sup), method="complete")
fviz_dend(hclust2_sin_sup, cex=1.5)  #Se ven unos cuatro grupos bien

# Cortamos el árbol

hclust2_cut_sin_sup = cutree(hclust2_sin_sup, k=4)
dendograma_2_sin_sup = data.frame(bd_3_sin_sup[,1:2], clus=hclust2_cut_sin_sup) #Clasifica muy bien !!! --> TOMARIA ESTE

write.excel(dendograma_2_sin_sup)


fviz_dend(hclust2_sin_sup, cex=1.5, k = 4,  rect = TRUE,  
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          rect_border = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"), 
          rect_fill = TRUE, 
          show_labels = FALSE, main = 'Dendograma - K = 4 - Sin Suplentes')

hclust2_cut_sin_sup

df_kmeans_silhouette_dendo = silhouette(hclust2_cut_sin_sup, dist(bd_3_sin_sup))
plot(df_kmeans_silhouette_dendo, main='Silhouette - Complete Linkage - Sin suplentes & sin ruido')


################## Validacion Interna##########################


library(fpc)
library(mclust)
library(NbClust)
library(cluster)

# Miremos la validacion interna
df_kmeans_silhouette = silhouette(k_means_4_sin_sup_ruido$cluster, dist(bd_3_sin_sup))
min(df_kmeans_silhouette)
plot(df_kmeans_silhouette, main='Silhouette')


# Miremos la estabilidad
df_kmeans_stability = clusterboot(bd_3_sin_sup, clustermethod=kmeansCBI,k=4)
df_kmeans_stability #todos los JACCARDS dan cercano a 1, lo que indica similaridad


## Miremos la matriz de disimilaridad para ver si podemos notar algun patron
# El heatmap trata de juntar lo que esta cerca para que queden cuadrados de mismos colores que implican que esten cerca
fviz_dist(dist(bd_2), show_labels = FALSE) + labs(title = "FIFA Players")
dist(bd_2)

