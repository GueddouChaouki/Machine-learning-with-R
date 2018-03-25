##Partie 1

#1. Charger le jeu de données d’apprentissage dans R (soit avec la commande read.table, soit avec l’outil d’importation de Rstudio : Tools > Import Dataset). Afficher les données d’apprentissage.
train <- read.table(file="data/synth_train.txt", header=TRUE)

#2. Afficher la dimension de l’ensemble d’apprentissage.
dim(train)

#3. Afficher les 6 premiers enregistrements.
head(train)

#4. Représenter graphiquement les observations à l’aide de la fonction plot. On pourra colorier les points en fonction de leur classe à l’aide du paramètre col et modifier le symbole avec le paramètre pch (=point character) et rajouter une légende à l’aide de la fonction legend.
X <- train[,-1]
Y <- train$y
plot(X, pch=Y, col=Y)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2)

#5. Appliquer la fonction knn du package class avec k = 15 voisins pour prédire les points de coordonnées (0,0) et (-2,2).
library(class)
Xtest <- matrix(c(0,0,-2,2), nrow=2, byrow=TRUE)
pred <- knn(X,Xtest, Y,15)
pred

#6. Prédire les données de l’ensemble d’apprentissage avec ce classifieur et comparer avec les vraies classes. Calculer le taux d’erreur empirique (nombre de fausses prédictions sur l’échantillon d’apprentissage divisé par taille de cet échantillon). Recommencer avec k=10, k= 5, k=3 et k = 1 voisins respectivement.
# avec k=15 voisins
pred_train <- knn(X, X,Y,15)
pred_train != Y
#erreur empirique
sum(pred_train!=Y)/length(Y)

# avec k=10 voisins
pred_train <- knn(X, X,Y,10)
pred_train != Y
#erreur empirique
sum(pred_train!=Y)/length(Y)

# avec k=5 voisins
pred_train <- knn(X, X,Y,5)
pred_train != Y
#erreur empirique
sum(pred_train!=Y)/length(Y)

# avec k=3 voisins
pred_train <- knn(X, X,Y,3)
pred_train != Y
#erreur empirique
sum(pred_train!=Y)/length(Y)

# avec k=1 voisins
pred_train <- knn(X, X,Y,1)
pred_train != Y
#erreur empirique
sum(pred_train!=Y)/length(Y)

#7. Représenter graphiquement la frontière de décision pour k = 15 voisins : commencer par construire une grille de points, prédire ces points, puis ajouter ces points sur le graphique en les coloriant en fonction de leur prédiction. Recommencer avec k = 1 voisin.

a <- seq(from=min(train$x1), to=max(train$x1), length.out=100)
b <- seq(from=min(train$x2), to=max(train$x2), length.out=100)
grille <- NULL
for (i in a){
grille <- rbind(grille, cbind(i,b))
}
#avec k=15 voisins
pred_grille <- knn(X, grille, Y,15)
plot(X, pch=Y, col=Y,main="Frontière de décision pour k=15 voisins")
points(grille, pch=20, col=pred_grille, cex=0.5)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2, bg="white")

#avec k=1 voisins
pred_grille <- knn(X, grille, Y,1)
plot(X, pch=Y, col=Y,main="Frontière de décision pour k=1 voisins")
points(grille, pch=20, col=pred_grille, cex=0.5)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2, bg="white")

#8. Charger le jeu de données de test dans R. Afficher les données test. Prédire les données de l’ensemble test avec k = 15 voisins puis avec k = 1 voisin. Calculer le taux d’erreur empirique dans les deux cas. Comparer avec les taux d’erreur des prédictions de l’ensemble d’apprentissage.
test <- read.table(file="data/synth_test.txt", header=TRUE)
Xtest <- test[,-1]
Ytest <- test$y
# avec k=15 voisins
pred_test <- knn(X,Xtest,Y,15)
sum(pred_test!=Ytest)/length(Ytest)

# avec k=1 voisins
pred_test <- knn(X,Xtest,Y,1)
sum(pred_test!=Ytest)/length(Ytest)

#9. On choisit à partir de maintenant le classifieur knn avec k = 15 voisins. Calculer le taux de vrai positifs (TVP) et le taux de vrai négatif (TVN) de ce classifieur sur l’échantillon test ... ici 1=positif et 2=négatif.

# prédictions knn avec 15 voisins
pred <- knn(X,Xtest,Y,15)

# matrice de confusion
table(Ytest,pred)

# TVP ou sensibilité
47/(47+15)

# TVN ou spécificité
138/138

#10. On souhaite maintenant associer un coût 3 fois plus important aux faux négatifs. On utilise donc la matrice de coût 0 3 1 0 dans la règle de classification de Bayes. Les probabilités à posteriori P(Y = 1|X = x) d’une entrée x sont estimées par la fréquences de voisins de x appartenant à la classe 1.
#a. Programmer une fonction prob_knn qui estime les probabilités P(Y = 1|X = x) d’un ensemble de données d’entrées.

prob_knn <- function(X,Y,Xtest,k)
{
K <- length(unique(Y))
prob <- rep(NA,nrow(Xtest))
for (i in 1:nrow(test))
{
x0 <- Xtest[i,]
d <- apply(X,1,function(x){sum((x-x0)^2)})
Nx0 <- sort(d,index.return=TRUE)$ix[1:k]
prob[i]<- sum(Y[Nx0]==1)/k
}
return(prob)
}

#b. Appliquer cette fonction aux données de l’ensemble test. En déduire leur prédiction avec la règle de Bayes incluant les coûts. Vous pouvez utiliser la fonction I pour construire le code R.

#estimation des probas d'être dans la classe 1
prob <- prob_knn(X,Y,Xtest,15)
head(prob)

#verification avec les probas de la fonction knn
#attention : il s'agit des probas de la classe effectivement predite !
res <- knn(X,Xtest,Y,15,prob=TRUE)
head(attr(res,"prob"))

#prediction avec la règle de Bayes et la matrice de coûts
pred_cout <- 1*I(3*prob>(1-prob))+2*I(3*prob<(1-prob))

#c. Calculer le taux de vrai positifs (TVP) et le taux de vrai négatif (TVN).
#matrice de confusion
table(Ytest,pred_cout)

# TVP ou sensibilité
54/(54+8)

# TVN ou spécificité
138/(134+8)

# TBC
(54+134)/200

#d. Faire un petit bilan méthodologique de cet exercice.



##Partie II (Données Réelles) :

#1. Charger le jeu de données dans R avec la commande load.
load("data/real_data.rda")

#2. Afficher la dimension de l’ensemble d’apprentissage.
dim(data)

#3. Afficher les 6 premiers enregistrements.
head(data)

#4. On s’intéresse d’abord à la méthodologie du choix de k 
#a. Créer un jeu de données de données d’apprentissage de taille 945 (75% des données) et un jeu de données test de taille 315 (25% des données) avec le code suivant.
set.seed(30)
tr <- sample(1:nrow(data),945)
train <- data[tr,]
test <- data[-tr,]

#b. Calculer les taux d’erreur sur les données test pour k variant de 1 à 100. Avec la fonction plot, représenter ce taux d’erreur test en fonction de k (contrôler que l’abscisse du graphique part de 0). Avec la fonction which.min, trouver le nombre de voisins qui donne la plus petite erreur test.
library(class)
kmax=100
err_test <- rep(NA,kmax)
for (k in 1:kmax)
{
pred <- knn(train[,-1],test[,-1],train$DIFF,k)
err_test[k] <- sum(pred!=test$DIFF)/length(test$DIFF)
}
lim <- c(0,max(err_test))
plot(err_test,type="l",ylim=lim,col=2,xlab="nombre de voisins",
ylab="taux d'erreur")
which.min(err_test)

#c. Recommencer avec un autre découpage aléatoire apprentissage/test et représenter la courbe d’évolution du taux d’erreur test sur le même graphique qu’à la question précédente.
#Nouveau decoupage apprentissage/test
set.seed(10)
tr <- sample(1:nrow(data),900)
train <- data[tr,]
test <- data[-tr,]
for (k in 1:kmax)
{
pred <- knn(train[,-1],test[,-1],train$DIFF,k)
err_test[k] <- sum(pred!=test$DIFF)/length(test$DIFF)
}
points(err_test,type="l",col=4)
legend("bottomright", legend=c("decoupage 1", "decoupage 2"),
lty=1, col=c(2,4))
which.min(err_test)

#d. Exécuter le code suivant et faire un choix pour k.
B<- 20
kmax <- 100
err_test <- matrix(NA,kmax,B)
for (b in 1:B)
{
tr <- sample(1:nrow(data),900)
train <- data[tr,]
test <- data[-tr,]
for (k in 1:kmax)
{
pred <- knn(train[,-1],test[,-1],train$DIFF,k)
err_test[k,b] <- sum(pred!=test$DIFF)/length(test$DIFF)
}
}
mean_err_test <- apply(err_test,1,mean)
lim <-c(0,max(err_test))
matplot(err_test,type="l",lty=2,col=2,ylim=lim, xlab="nombre de
voisins",ylab="taux d’erreur")
matpoints(mean_err_test,type="l",col=2,lwd=4)
legend("bottomright", legend=c("Erreur moyenne", "Erreurs conditionnelles"),
lty=c(1,3),lwd=c(4,2),col=c(2,2))
which.min(mean_err_test)

#e. Choisir maintenant le nombre k de voisin en utilisant par validation croisée (cross validation) leave-one-out (LOO) avec la fonction knn.cv.
err_test <- rep(NA,kmax)
for (k in 1:kmax)
{
pred <- knn.cv(data[,-1],data$DIFF,k)
err_test[k] <- sum(pred!=data$DIFF)/length(data$DIFF)
}
lim <-c(0,max(err_test))
plot(err_test,type="l",col=2,ylim=lim,xlab="nombre de voisins",
ylab="taux d'erreur")
points(mean_err_test,type="l",col=4,lwd=1)
legend("bottomright", legend=c("Erreur loo", "Erreur moyenne"),
col=c(2,4),lty=1)
which.min(err_test)

#f. Faire un petit bilan méthodologique concernant le choix du paramètre k.

#5. On veut maintenant non seulement choisir k mais également avoir une idée de l’erreur de prédiction de ce classifieur. Pour cela, il faut utiliser des données n’ayant jamais été utilisées. Les données doivent donc être découpées en trois parties : apprentissage/validation/test
#a. Couper aléatoirement les données deux parties : un ensemble "apprentissage-validation" de taille 945 (75 % des données) et un ensemble test de taille 315 (25% des données).

set.seed(30)
tr <- sample(1:nrow(data),945)
trainval <- data[tr,]
test <- data[-tr,]

#(b) Utiliser la première approche pour choisir k sur l’ensemble "apprentissage-validation" :
#i. Choisir k en découpant les 945 données de l’ensemble "apprentissage-validation" en deux parties : une partie "apprentissage" de taille 630 (50% des données) et une partie "validation" de taille 315 (25 % des données). Choisir k qui minimise le taux d’erreur moyen sur les ensembles de validations de B = 25 découpages.
B <- 25
kmax <- 50
err_valid <- matrix(NA,kmax,B)
for (b in 1:B)
{
tr <- sample(1:nrow(trainval),630)
train <- trainval[tr,]
valid <- trainval[-tr,]
for (k in 1:kmax)
{
pred <- knn(train[,-1],valid[,-1],train$DIFF,k)
err_valid[k,b] <- sum(pred!=valid$DIFF)/length(valid$DIFF)
}
}
mean_err_valid <- apply(err_valid,1,mean)
plot(mean_err_valid,type="l")
which.min(mean_err_valid)

#ii. Construire le classifieur avec ce nombre de voisins sur l’ensemble "apprentissage-validation" et calculer le taux d’erreur des données test.
pred <- knn(trainval[,-1],test[,-1],trainval$DIFF,k=which.min(mean_err_valid))
sum(pred!=test$DIFF)/length(test$DIFF)

#(c) Utiliser la seconde approche pour choisir k par validation croisée LOO sur l’ensemble "apprentissagevalidation".Calculer ensuite le taux d’erreur des données test.
err_valid <- rep(NA,kmax)
for (k in 1:kmax)
{
pred <- knn.cv(trainval[,-1],trainval$DIFF,k)
err_valid[k] <- sum(pred!=trainval$DIFF)/length(trainval$DIFF)
}
which.min(err_valid)
pred <- knn(trainval[,-1],test[,-1],trainval$DIFF,k=which.min(err_valid))
sum(pred!=test$DIFF)/length(test$DIFF)


