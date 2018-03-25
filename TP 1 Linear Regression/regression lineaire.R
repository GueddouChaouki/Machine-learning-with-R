
## TP Regression lineaire

###Exercice 1
{r cars}
x <- c(2,5,3,7)
y = 2*x+5
lm(y ~ x)



dframe <- data.frame(x=c(2,5,3,7) , y=c(9, 15, 11, 19))
reglin <- lm(y ~x, dframe)



plot(y ~ x, dframe)
abline(lm(y ~ x, dframe))


###Exercice 2


xi <- c(1:12)
xi


yi <- c(40, 42, 44, 45, 48, 50, 52, 55, 58, 63, 68, 70)
yi


plot(xi, yi)




lm(yi ~ xi)



plot(xi, yi)
abline(35.076,2.745,col='red')



2.745 * 13 + 35.076



data.frame(xi, yi)
summary(xi)
summary(yi)

###Exercice 3

data(iris)
head(iris)
structure(iris)
require(ggplot2)
plot(iris$Petal.Length,iris$Petal.Width)
qplot(Petal.Length,Petal.Width,data=iris)
qplot(Petal.Length,Petal.Width,data=iris,colour=Species)
qplot(Petal.Length,Petal.Width,data=iris,colour=Species,size=Sepal.Length+Sepal.Width)
qplot(Petal.Length,Petal.Width,data=iris,colour=Species,size=Sepal.Length+Sepal.Width)+geom_smooth(method = "lm")
qplot(Species,Petal.Width,data = iris,geom = c("boxplot","jitter"))


###Exercice 4
1- Charger les données dans une variable nommée tab.

tab <- read.table(file.choose(), header = T)
structure(tab)



2- Charger le vecteur age dans une variable nommée x.

x<-tab[,2]


3- Charger le vecteur height dans une variable nommée y.

y<-tab[,3]


4- Donner la distribution de y en fonction de x (utiliser les fonctions plot et qplot).

m <- length(y)
plot(x,y,xlab="Age", ylab="Poid")
require(ggplot2)
qplot(x,y,xlab="Age", ylab="Poid")+geom_smooth(method="lm")


5- On regardant le graphique. Peut-on dire qu'il y a une corrélation entre les deux
variables. Justifiez.

cor(x,y)

6- Prouvez d'une manière statistique (rigoureuse) que x et y sont soit corrélées soit non ?

#reste 

7- Quelles sont les valeurs des paramètres (??0 et ??1).

m1<-lm(height ~ age, data=tab)
m1


8- Quelle est la valeur de l'erreur résiduelle.

summary(m1)


9- Donnez l'équation de la droite de régression.

**Y=0.06388X+0.75016**

10- Dessiner la droite de régression.

#abline (m1, col="red")

11- Selon le modèle trouvé prédire les poids de 4 enfants ayant respectivement 3, 7, 9 et 12 ans.

predict(m1, data.frame("age"=3))
predict(m1, data.frame("age"=7))
predict(m1, data.frame("age"=9))
predict(m1, data.frame("age"=12))

12- Prédire la taille d'un enfant de 10 ans avec un intervalle de confiance de 95%.

predict(m1, data.frame("age"=10), interval="prediction",level = 0.95)



###Exercice 05

ozone <- read.table(file.choose(), header = T)
plot(maxO3~T12,data=ozone)

Une régression linéaire simple semble-t-elle justifiée graphiquement ?

reponse : NON

reg<-lm(maxO3~T12,data=ozone)
resume<-summary(reg)
resume

Que représente les coefficients de la matrice coefficients ?

reponse : represente la concentration en ozone par rapport à la température à midi


plot(maxO3~T12,data=ozone)
#T12=seq(min(ozone[,"T12"]),max(ozone[,"T12"]),length=100)
#grille<-data.frame(T12)
#ICdte<-predict(reg,new=grille,interval="confidence",level=0.95)
#matlines(grille$T12,cbind(ICdte),lty=c(1,2,2),col=1)

#res<-rstudent(reg)
#plot(res,pch=15,ylab=Résidus,ylim=c(-3,3))
#abline(h=c(-2,0,2),lty=c(2,1,2)

Que remarquez-vous ?

reponse : 


#plot(maxO3~T12,data=ozone)
#T12=seq(min(ozone[,"T12"]),max(ozone[,"T12"]),length=100)
#grille<-data.frame(T12)
#ICprev<-predict(reg,new=grille,interval="pred",level=0.95)
#matlines(grille$T12,cbind(ICprev),lty=c(1,2,2),col=1)





seuil<-qt(0.975,df=reg$df.res)
beta0min<-coef(resume)[1,1]-seuil*coef(resume)[1,2]
beta0max<-coef(resume)[1,1]+seuil*coef(resume)[1,2]
beta1min<-coef(resume)[2,1]-seuil*coef(resume)[2,2]
beta1max<-coef(resume)[2,1]+seuil*coef(resume)[2,2]

Que remarquez-vous sur l'intervalle de confiance de ??0 ? Comment l'expliquez-vous ?

reponse : 



#library(ellipse)
#plot(ellipse(reg,level=0.95),type="l",xlab="beta0",ylab="beta1")
#points(coef(reg)[1],coef(reg)[2],pch=3)
#lines(c(beta0min,beta0min,beta0max,beta0max,beta0min),c(beta1min,bet
#a1max,beta1max,beta1min,beta1min),lty=2)
#plot(ellipse(reg,level=0.95),type="l",xlab="beta0",ylab="beta1")
#points(coef(reg)[1],coef(reg)[2],pch=3)
#lines(c(beta0min,beta0min,beta0max,beta0max,beta0min),c(beta1min,bet
#a1max,beta1max,beta1min,beta1min),lty=2)



