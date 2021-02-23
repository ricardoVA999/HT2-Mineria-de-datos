library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering

setwd("C:/Users/Zephyrus/Documents/U/7mo Semestre/Mineria de Datos/HT2")
movies <- read.csv("movies.csv", stringsAsFactors = TRUE)

#Seleccion de los datos con los que se realizaran los clusters, desidimos agregar todos aquellos de caracter cuantitativo.
cuanti_data_movies<-cbind(movies[,3:5], movies$runtime, movies[,17:19])

#Cantidad de clusters recomendados, haciendo uso del Metodo de Ward para determinar el numero correcto de clusteres con k-medias
wss <- (nrow(cuanti_data_movies-1)*sum(apply(cuanti_data_movies,2,var)))
for (i in 2:10) 
  wss[i] <- sum(kmeans(cuanti_data_movies, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Kmedias
km<-kmeans(cuanti_data_movies,4,iter.max =100)
movies$grupokm<-km$cluster
plotcluster(cuanti_data_movies,km$cluster)
fviz_cluster(km, data = cuanti_data_movies,geom = "point", ellipse.type = "norm")

#Clustering jerarquico
hc<-hclust(dist(cuanti_data_movies))
plot(hc)
rect.hclust(hc,k=4)
groups<-cutree(hc,k=4)
movies$gruposHC<-groups

hc.cut<-hcut(cuanti_data_movies, k=4, hc_method = "complete")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
fviz_cluster(hc.cut, ellipse.type = "convex")

#Fuzzy C-Means
fcm<-cmeans(cuanti_data_movies,4)
movies$FCGrupos<-fcm$cluster
movies<-cbind(movies,fcm$membership)
plotcluster(cuanti_data_movies,fcm$cluster)
fviz_cluster(fcm, data = cuanti_data_movies,geom = "point", ellipse.type = "norm")



#Calidad de agrupamientos, por metodo de la silueta.
#K-means
silkm<-silhouette(km$cluster,dist(cuanti_data_movies))
mean(silkm[,3])
#Jerarquico
silch<-silhouette(groups,dist(cuanti_data_movies))
mean(silch[,3])
#Fuzzy c-means
silfcm<-silhouette(fcm$cluster,dist(cuanti_data_movies))
mean(silfcm[,3])

g1HC<-movies[movies$gruposHC==1,]
g2HC<-movies[movies$gruposHC==2,]
g3HC<-movies[movies$gruposHC==3,]
g4HC<-movies[movies$gruposHC==4,]

summary(g1HC)
summary(g2HC)
nrow(g2HC)
summary(g3HC)
summary(g4HC)
