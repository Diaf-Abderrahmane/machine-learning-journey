# Charger le package FactoMineR
library(FactoMineR)
library(mlbench)
library(factoextra)
library(FSelector)
library(cluster)


# Charger le dataset Glass
data(Glass)
data=Glass
data=data[,1:9]


#Visualiser le dataset
head(data)
dim(data)
summary(data)


# Determine the optimal number of clusters using the elbow method
elbow <- fviz_nbclust(data, kmeans, method = "wss") + ggtitle("Elbow method")
print(elbow)
# k optimal = 5 

#------------------------------ PARTIE 1 ------------------------------------#


#Perform a classic kmeans
t7 <- Sys.time()
glass_kmeans <- kmeans(Glass, centers = 5)
glass_kmeans
t8 <- Sys.time()
t9 <- t8-t7
t9
#temps d'execution =  0.02380802 secs
#(between_SS / total_SS =  72.4 %) donne de tres bonnes performances


#Visualiser les clusters
clusplot(data, glass_kmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)



#------------------------------ PARTIE 2 ------------------------------------#

# Réaliser kmeans apres une ACP

#ACP
pca <- PCA(data, scale=TRUE, graph =T)

#Visualiser les valeurs propres et l'inertie cumulée
pca$eig
# on prend les 5 premieres composantes, cum=89% > 80%


#projection sur les 5 composantes principales retenues
pca$ind
data1 = pca$ind$coord[,1:5]
data1

set.seed(123)

# Determine the optimal number of clusters using the elbow method
elbow <- fviz_nbclust(data1, kmeans, method = "wss") + ggtitle("Elbow method")
print(elbow)
# k optimal = 5 


#calcul temps d'exec en utilisant le dataset réduit
t1 <- Sys.time()
glass_kmeans <- kmeans(data1, centers = 5)
glass_kmeans
t2 <- Sys.time()
t3 <- t2-t1
t3
# - Execution Time =  0.01528502 secs
# - Le temps d'éxecution a diminué, ce qui est logique vu qu'on a réduit les 
#   attributs
# - (between_SS / total_SS =  57.6 %)
# - La performance de kmeans a diminué : ce qui est aussi logique, parce 
#   que apres réduction la quantité d'information fournit a diminué (89%)

#Visualiser les clusters
clusplot(data, glass_kmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


#------------------------------ PARTIE 3 ------------------------------------#

#Kmeans avec feature selection

#Feature selection
result <- cfs(Type ~ ., Glass)
result
# - "Mg" "Al" "K"  "Ca" "Ba"

# - Projection sur les colonnes désirées ( les features les plus pertinents !)
Glass_selected <- Glass[, result]
Glass_selected

# elbow 
elbow <- fviz_nbclust(Glass_selected, kmeans, method = "wss") + ggtitle("Elbow method")
print(elbow)

t4 <- Sys.time()
glass_kmeans <- kmeans(Glass_selected, centers = 5)
glass_kmeans
t5 <- Sys.time()
t6 <- t5-t4
t6
#Execution time = 0.01715493 secs
#(between_SS / total_SS =  65.4 %)
# - Temps d'execution avec feature selection ( 0.017 ) > temps d'exec avec acp (0.015)
# - Performance de kmeans par contre dans ce cas > à celle de acp
# - On peut constater par exp que la methode features selection réduit les données
# de façon réduite par rapport a l'acp, et donc donne plus de temps d'execution et 
# plus de performance

#Visualiser les clusters
clusplot(data, glass_kmeans$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
