## Partie 1


library(datasets)
iris.3means <- kmeans(iris[,1:4],3)
iris.3means$cluster
iris.3means$centers
iris.3means$withinss
iris.3means$size

iris.3means <- kmeans(iris[,-5],3,nstart=30)
#1. Faites en sorte que chaque donnée soit colorée en fonction du groupe dans lequel kmeans l'a placé.

plot(iris$Petal.Length,iris$Petal.Width,col=iris.3means$cluster)

#2. Faites en sorte que chaque donnée soit colorée en fonction de la valeur du cinquième attribut de IRIS (l'espèce).

plot(iris$Petal.Length,iris$Petal.Width,col=iris$Species)

#4. Faites une table de contingence entre classe des iris et classe trouvée par kmeans() (matrice de confusion).

table(iris.3means$cluster,iris$Species)

#Qu'en pensez-vous ?
#il a un taux d"erreur de 16/150

###Trouver le nombre optimal de groupes


iris.kmeans <- list()
for (k in 2:10){
    iris.kmeans[[k]] <- kmeans(iris[,-5],k,nstart = 30)
}

#1. Calculer un vecteur dont la composante k est l'inertie intraclasse de la segmentation correspondante.

inertie.expl <- rep(0,times=10)
inertie.intra <- rep(0,times=10)

 for (k in 2:10){
 clus <- kmeans(iris[,-5],centers=k,nstart=5)
 inertie.expl[k] <- clus$betweenss/clus$totss
 inertie.intra[k] <- sum (clus$withinss)
 
 #betweenss = la somme des distances carrées
 #totss = N
 }

#2. Faites un graphe de l'évolution de l'inertie intraclasse en fonction de k.

 plot(1:10,inertie.expl,type="b",xlab="Nombre de clusters",ylab="% inertie expliquée")
 plot(inertie.intra)
 
 #j'ai calculé se qui me ressemble tnertie totale 
 
 #semble la troisieme vu que à partir de 3 clusters 
 #l'neritie n'augmente pas concederablement


##Partie 2
###Exercices
###Exercice 1 : IRIS suite

jeu1 <- read.table("TP Clustering/Data/jeu1.txt",header = T)
dim (jeu1)
plot(jeu1)

##Exercice iris suite
#1. Faites une segmentation avec le meilleur k des iris en 3 groupes en utilisant uniquement les attributs longueur et largeur des pétales. Le résultat est-il le même qu'en utilisant les 4 attributs ?
#avec 2 attributs

for (k in 2:10){
 clus <- kmeans(iris[,3:4],centers=k,nstart=5)
 inertie.expl[k] <- clus$betweenss/clus$totss
 inertie.intra[k] <- sum (clus$withinss)
 }

 plot(1:10,inertie.expl,type="b",xlab="Nombre de clusters",ylab="% inertie expliquée")
 plot(inertie.intra)
 

 #avec 4 attributs
 for (k in 2:10){
 clus <- kmeans(iris[,-5],centers=k,nstart=5)
 inertie.expl[k] <- clus$betweenss/clus$totss
 inertie.intra[k] <- sum (clus$withinss)
 }

 plot(1:10,inertie.expl,type="b",xlab="Nombre de clusters",ylab="% inertie expliquée")
 plot(inertie.intra)
 
 #2. On utilise à nouveau les 4 attributs. Comparez les segmentations à 3 et à 4 groupes obtenues par les k-moyennes : visuellement en utilisant un graphe comme plus haut, et via leur matrice de confusion.
 
iris.3means <- kmeans(iris[,-5],4,nstart=30)

plot(iris$Petal.Length,iris$Petal.Width,col=iris.3means$cluster)

plot(iris$Petal.Length,iris$Petal.Width,col=iris$Species)

table(iris.3means$cluster,iris$Species)



###Exercice 2 : variations sur un jeu de données

#1. Déterminez le nombre de groupes optimal et les groupes dans le jeu de données jeu1.txt. Visualisez la segmentation optimale avec des couleurs.

jeu1 <- read.table("TP Clustering/Data/jeu1.txt",header = T)
dim (jeu1)
plot(jeu1)

inertie <- rep (NA, times = 15)
for (i in 2:15) {
  km <- kmeans (jeu1, nstart = 30, iter.max = 50, centers = i)
  inertie [i] <- sum (km$withinss)
}
plot (inertie)

#il y a 5 clusters

testQ1 <- kmeans(jeu1,nstart = 30, iter.max = 50, centers = 5)
plot(jeu1,col=testQ1$cluster)

#2. Mêmes questions avec jeu2.txt. Comparez ce jeu de données avec le précédent. Visualisez la projection sur les deux premiers attributs de la segmentation optimale avec des couleurs.

jeu2 <- read.table("TP Clustering/Data/jeu2.txt",header = T)
dim (jeu2)
plot(jeu2)

inertie <- rep (NA, times = 15)
for (i in 2:15) {
  km <- kmeans (jeu2, nstart = 30, iter.max = 50, centers = i)
  inertie [i] <- sum (km$withinss)
}
plot (inertie)

#il y a 5 clusters

testQ1 <- kmeans(jeu2,nstart = 30, iter.max = 50, centers = 5)
plot(jeu2,col=testQ1$cluster)

#semble plus claire avec a et b
plot(jeu2[,1:2],col=testQ1$cluster)

#3. Mêmes questions avec jeu3.txt. Comparez ce jeu de données avec le précédent. Visualisez la projection sur les deux premiers attributs de la segmentation optimale avec des couleurs.

jeu3 <- read.table("TP Clustering/Data/jeu3.txt",header = T)
dim (jeu3)
plot(jeu3)

inertie <- rep (NA, times = 15)
inertie.expl <- rep (NA, times = k)
for (i in 2:15) {
  km <- kmeans (jeu3, nstart = 30, iter.max = 50, centers = i)
  inertie [i] <- sum (km$withinss)
  inertie.expl[i] <- km$betweenss/km$totss
}
plot (inertie)

plot(1:15,inertie.expl,type="b",xlab="Nombre de clusters",ylab="% inertie expliquée")

testQ1 <- kmeans(jeu3,nstart = 30, iter.max = 50, centers = 5)
plot(jeu3,col=testQ1$cluster)

#voyant s'il est plus claire avec a et b
plot(jeu3[,1:2],col=testQ1$cluster)


#?????
#4. Mêmes questions avec jeu11.txt. Comparez ce jeu de données avec le précédent. Visualisez la projection sur les deux premiers attributs de la segmentation optimale avec des couleurs.

jeu11 <- read.table("TP Clustering/Data/jeu11.txt",header = T)
dim (jeu11)
plot(jeu11)

inertie <- rep (NA, times = 15)
inertie.expl <- rep (NA, times = k)
for (i in 2:15) {
  km <- kmeans (jeu11, nstart = 30, iter.max = 50, centers = i)
  inertie [i] <- sum (km$withinss)
  inertie.expl[i] <- km$betweenss/km$totss
}
plot (inertie)

plot(1:15,inertie.expl,type="b",xlab="Nombre de clusters",ylab="% inertie expliquée")

testQ1 <- kmeans(jeu11,nstart = 30, iter.max = 50, centers = 3)
plot(jeu11,col=testQ1$cluster)

#voyant s'il est plus claire avec c et d
plot(jeu11[,2:3],col=testQ1$cluster)

#5. Quelles sont vos conclusions pour cet exercice : expliquez et comprendre les différences de résultats obtenus pour ces différentes variations autour d'un même jeu de données. ?


###Exercice 3 : les serpentins
#On considère le jeu de données jeu4.txt. Faîtes une segmentation en 3 groupes. Visualisez le jeu de
#données (par un plot() tout simple sur les deux premiers attributs). Visualisez le jeu de données en
#colorant chaque point par une couleur différente en fonction du groupe auquel kmeans() l a associé.
#Qu en pensez-vous ? chercher le k optimal. Faites une représentation graphique de cette segmentation
#« optimale » ; qu'en pensez-vous ?


jeu4 <- read.table("TP Clustering/Data/jeu4.txt",header = T)
dim (jeu4)
plot(jeu4)

inertie <- rep (NA, times = 15)
inertie.expl <- rep (NA, times = k)
for (i in 2:15) {
  km <- kmeans (jeu4, nstart = 30, iter.max = 50, centers = i)
  inertie [i] <- sum (km$withinss)
  inertie.expl[i] <- km$betweenss/km$totss
}
plot (inertie)

plot(1:15,inertie.expl,type="b",xlab="Nombre de clusters",ylab="% inertie expliquée")

testQ1 <- kmeans(jeu4,nstart = 30, iter.max = 50, centers = 3)
plot(jeu11,col=testQ1$cluster)

#pas tres claire


###Exercice 4 : les cercles concentriques
#On considère le jeu de données jeu5.txt. Répondre aux mêmes questions que précédemment. Pensezvous
#qu une transformation des données permettrait à kmeans() de trouver les groupes attendus ?



jeu5 <- read.table("TP Clustering/Data/jeu5.txt",header = T)
dim (jeu5)
plot(jeu5)

inertie <- rep (NA, times = 15)
inertie.expl <- rep (NA, times = k)
for (i in 2:15) {
  km <- kmeans (jeu5, nstart = 30, iter.max = 50, centers = i)
  inertie [i] <- sum (km$withinss)
  inertie.expl[i] <- km$betweenss/km$totss
}
plot (inertie)

plot(1:15,inertie.expl,type="b",xlab="Nombre de clusters",ylab="% inertie expliquée")

testQ1 <- kmeans(jeu5,nstart = 30, iter.max = 50, centers = 3)
plot(jeu11,col=testQ1$cluster)

#pas tres claire


