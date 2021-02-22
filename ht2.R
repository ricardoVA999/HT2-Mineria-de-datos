library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering

setwd("C:/Users/Zephyrus/Documents/U/7mo Semestre/Mineria de Datos/HT2")
movies<-read.csv("movies.csv")

#Seleccion de los datos con los que se realizaran los clusters, desidimos agregar todos aquellos de caracter cuantitativo.
cuanti_data_movies<-cbind(movies[,3:5], movies$runtime, movies[,17:21])

#Cantidad de clusters recomendados
wss <- (nrow(cuanti_data_movies-1)*sum(apply(cuanti_data_movies,2,var)))
for (i in 2:10) 
  wss[i] <- sum(kmeans(cuanti_data_movies, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")


#Kmedias
cuanti_data_movies<-cbind(movies[,3:5], movies$runtime, movies[,17:21])
km<-kmeans(cuanti_data_movies,4,iter.max =100)
movies$grupo<-km$cluster

plotcluster(cuanti_data_movies,km$cluster)

#Jerarquico
hc<-hclust(dist(cuanti_data_movies))
plot(hc)
rect.hclust(hc,k=4)
groups<-cutree(hc,k=4)
movies$gruposHC<-groups

#Fuzzy C-Means
fcm<-cmeans(cuanti_data_movies,3)
movies$FCGrupos<-fcm$cluster
movies<-cbind(movies,fcm$membership)

#Mixture of gaussians
mc<-Mclust(cuanti_data_movies,3)
plot(mc, what = "classification", main="MClust Classification")
movies$mxGau<-mc$classification

