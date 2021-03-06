##Partie 1

#1. Charger le jeu de donn�es d�apprentissage dans R (soit avec la commande read.table, soit avec l�outil d�importation de Rstudio : Tools > Import Dataset). Afficher les donn�es d�apprentissage.
train <- read.table(file="data/synth_train.txt", header=TRUE)

#2. Afficher la dimension de l�ensemble d�apprentissage.
dim(train)

#3. Afficher les 6 premiers enregistrements.
head(train)

#4. Repr�senter graphiquement les observations � l�aide de la fonction plot. On pourra colorier les points en fonction de leur classe � l�aide du param�tre col et modifier le symbole avec le param�tre pch (=point character) et rajouter une l�gende � l�aide de la fonction legend.
X <- train[,-1]
Y <- train$y
plot(X, pch=Y, col=Y)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2)

#5. Appliquer la fonction knn du package class avec k = 15 voisins pour pr�dire les points de coordonn�es (0,0) et (-2,2).
library(class)
Xtest <- matrix(c(0,0,-2,2), nrow=2, byrow=TRUE)
pred <- knn(X,Xtest, Y,15)
pred

#6. Pr�dire les donn�es de l�ensemble d�apprentissage avec ce classifieur et comparer avec les vraies classes. Calculer le taux d�erreur empirique (nombre de fausses pr�dictions sur l��chantillon d�apprentissage divis� par taille de cet �chantillon). Recommencer avec k=10, k= 5, k=3 et k = 1 voisins respectivement.
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

#7. Repr�senter graphiquement la fronti�re de d�cision pour k = 15 voisins : commencer par construire une grille de points, pr�dire ces points, puis ajouter ces points sur le graphique en les coloriant en fonction de leur pr�diction. Recommencer avec k = 1 voisin.

a <- seq(from=min(train$x1), to=max(train$x1), length.out=100)
b <- seq(from=min(train$x2), to=max(train$x2), length.out=100)
grille <- NULL
for (i in a){
grille <- rbind(grille, cbind(i,b))
}
#avec k=15 voisins
pred_grille <- knn(X, grille, Y,15)
plot(X, pch=Y, col=Y,main="Fronti�re de d�cision pour k=15 voisins")
points(grille, pch=20, col=pred_grille, cex=0.5)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2, bg="white")

#avec k=1 voisins
pred_grille <- knn(X, grille, Y,1)
plot(X, pch=Y, col=Y,main="Fronti�re de d�cision pour k=1 voisins")
points(grille, pch=20, col=pred_grille, cex=0.5)
legend("topright", legend=c("classe 1", "classe 2"), pch=1:2, col=1:2, bg="white")

#8. Charger le jeu de donn�es de test dans R. Afficher les donn�es test. Pr�dire les donn�es de l�ensemble test avec k = 15 voisins puis avec k = 1 voisin. Calculer le taux d�erreur empirique dans les deux cas. Comparer avec les taux d�erreur des pr�dictions de l�ensemble d�apprentissage.
test <- read.table(file="data/synth_test.txt", header=TRUE)
Xtest <- test[,-1]
Ytest <- test$y
# avec k=15 voisins
pred_test <- knn(X,Xtest,Y,15)
sum(pred_test!=Ytest)/length(Ytest)

# avec k=1 voisins
pred_test <- knn(X,Xtest,Y,1)
sum(pred_test!=Ytest)/length(Ytest)

#9. On choisit � partir de maintenant le classifieur knn avec k = 15 voisins. Calculer le taux de vrai positifs (TVP) et le taux de vrai n�gatif (TVN) de ce classifieur sur l��chantillon test ... ici 1=positif et 2=n�gatif.

# pr�dictions knn avec 15 voisins
pred <- knn(X,Xtest,Y,15)

# matrice de confusion
table(Ytest,pred)

# TVP ou sensibilit�
47/(47+15)

# TVN ou sp�cificit�
138/138

#10. On souhaite maintenant associer un co�t 3 fois plus important aux faux n�gatifs. On utilise donc la matrice de co�t 0 3 1 0 dans la r�gle de classification de Bayes. Les probabilit�s � posteriori P(Y = 1|X = x) d�une entr�e x sont estim�es par la fr�quences de voisins de x appartenant � la classe 1.
#a. Programmer une fonction prob_knn qui estime les probabilit�s P(Y = 1|X = x) d�un ensemble de donn�es d�entr�es.

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

#b. Appliquer cette fonction aux donn�es de l�ensemble test. En d�duire leur pr�diction avec la r�gle de Bayes incluant les co�ts. Vous pouvez utiliser la fonction I pour construire le code R.

#estimation des probas d'�tre dans la classe 1
prob <- prob_knn(X,Y,Xtest,15)
head(prob)

#verification avec les probas de la fonction knn
#attention : il s'agit des probas de la classe effectivement predite !
res <- knn(X,Xtest,Y,15,prob=TRUE)
head(attr(res,"prob"))

#prediction avec la r�gle de Bayes et la matrice de co�ts
pred_cout <- 1*I(3*prob>(1-prob))+2*I(3*prob<(1-prob))

#c. Calculer le taux de vrai positifs (TVP) et le taux de vrai n�gatif (TVN).
#matrice de confusion
table(Ytest,pred_cout)

# TVP ou sensibilit�
54/(54+8)

# TVN ou sp�cificit�
138/(134+8)

# TBC
(54+134)/200

#d. Faire un petit bilan m�thodologique de cet exercice.



##Partie II (Donn�es R�elles) :

#1. Charger le jeu de donn�es dans R avec la commande load.
load("data/real_data.rda")

#2. Afficher la dimension de l�ensemble d�apprentissage.
dim(data)

#3. Afficher les 6 premiers enregistrements.
head(data)

#4. On s�int�resse d�abord � la m�thodologie du choix de k 
#a. Cr�er un jeu de donn�es de donn�es d�apprentissage de taille 945 (75% des donn�es) et un jeu de donn�es test de taille 315 (25% des donn�es) avec le code suivant.
set.seed(30)
tr <- sample(1:nrow(data),945)
train <- data[tr,]
test <- data[-tr,]

#b. Calculer les taux d�erreur sur les donn�es test pour k variant de 1 � 100. Avec la fonction plot, repr�senter ce taux d�erreur test en fonction de k (contr�ler que l�abscisse du graphique part de 0). Avec la fonction which.min, trouver le nombre de voisins qui donne la plus petite erreur test.
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

#c. Recommencer avec un autre d�coupage al�atoire apprentissage/test et repr�senter la courbe d��volution du taux d�erreur test sur le m�me graphique qu�� la question pr�c�dente.
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

#d. Ex�cuter le code suivant et faire un choix pour k.
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
voisins",ylab="taux d�erreur")
matpoints(mean_err_test,type="l",col=2,lwd=4)
legend("bottomright", legend=c("Erreur moyenne", "Erreurs conditionnelles"),
lty=c(1,3),lwd=c(4,2),col=c(2,2))
which.min(mean_err_test)

#e. Choisir maintenant le nombre k de voisin en utilisant par validation crois�e (cross validation) leave-one-out (LOO) avec la fonction knn.cv.
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

#f. Faire un petit bilan m�thodologique concernant le choix du param�tre k.

#5. On veut maintenant non seulement choisir k mais �galement avoir une id�e de l�erreur de pr�diction de ce classifieur. Pour cela, il faut utiliser des donn�es n�ayant jamais �t� utilis�es. Les donn�es doivent donc �tre d�coup�es en trois parties : apprentissage/validation/test
#a. Couper al�atoirement les donn�es deux parties : un ensemble "apprentissage-validation" de taille 945 (75 % des donn�es) et un ensemble test de taille 315 (25% des donn�es).

set.seed(30)
tr <- sample(1:nrow(data),945)
trainval <- data[tr,]
test <- data[-tr,]

#(b) Utiliser la premi�re approche pour choisir k sur l�ensemble "apprentissage-validation" :
#i. Choisir k en d�coupant les 945 donn�es de l�ensemble "apprentissage-validation" en deux parties : une partie "apprentissage" de taille 630 (50% des donn�es) et une partie "validation" de taille 315 (25 % des donn�es). Choisir k qui minimise le taux d�erreur moyen sur les ensembles de validations de B = 25 d�coupages.
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

#ii. Construire le classifieur avec ce nombre de voisins sur l�ensemble "apprentissage-validation" et calculer le taux d�erreur des donn�es test.
pred <- knn(trainval[,-1],test[,-1],trainval$DIFF,k=which.min(mean_err_valid))
sum(pred!=test$DIFF)/length(test$DIFF)

#(c) Utiliser la seconde approche pour choisir k par validation crois�e LOO sur l�ensemble "apprentissagevalidation".Calculer ensuite le taux d�erreur des donn�es test.
err_valid <- rep(NA,kmax)
for (k in 1:kmax)
{
pred <- knn.cv(trainval[,-1],trainval$DIFF,k)
err_valid[k] <- sum(pred!=trainval$DIFF)/length(trainval$DIFF)
}
which.min(err_valid)
pred <- knn(trainval[,-1],test[,-1],trainval$DIFF,k=which.min(err_valid))
sum(pred!=test$DIFF)/length(test$DIFF)


