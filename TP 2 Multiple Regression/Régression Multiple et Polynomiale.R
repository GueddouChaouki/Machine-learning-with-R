## Partie 1



LungCapData <- read.table(file.choose(), header = T, sep="\t")
#attach the data
attach(LungCapData)

# verifier les noms des variables
names(LungCapData)

#verifier les types des variables
class(Age)
class(Smoke)
#Domaine??
levels(Smoke)

#cr�e un modele en utilisant Age et Height
model1 <- lm(LungCap ~ Age + Height)

#un resum� de modele
summary(model1)

#Calculer Pearson correlation entre Age et Height
cor(Age, Height, method="pearson")

#faite une m a j de modele avec un interval de confiance de 0.95
confint(model1, coef.level=0.95)

#cr�e un model avec toutes les variables
model2 <- lm(LungCap ~ Age + Height + Smoke + Gender +
Caesarean)

#resum� de modele 2
summary(model2)

#plot
plot(model2)


## Partie 1



# charger le package MASS
library(MASS)

#1. Charger la table Boston.
data(Boston)

#2. Afficher les informations.
names(Boston)

#3. Afficher le type de chaque donn�e.
class(Boston$crim)
class(Boston$zn)
class(Boston$indus)
class(Boston$chas)
class(Boston$nox)
class(Boston$rm)
class(Boston$age)
class(Boston$dis)
class(Boston$rad)
class(Boston$tax)
class(Boston$ptratio)
class(Boston$black)
class(Boston$lstat)
class(Boston$medv)

#4. Que pouvez-vous dire de ces donn�es.
#des donn�es numeriques

#5. Diviser les donn�es en utilisant les 400 premi�res observations que les donn�es
#d'entra�nement et le reste en tant que donn�es de test.
train = 1:400
test = -train
training_data = Boston[train,]
testing_data = Boston[test,]

#6. V�rifier s'il existe une relation lin�aire entre medv et age.
cor(training_data$age, training_data$medv)

#7. Dessiner le nuage de points de ces deux variables.
plot(training_data$age, 
     training_data$medv, 
     xlab = "Age de la maison", 
     ylab = "valeur m�diane des habitants-propri�taires en 1000$ par unit�.")

#8. Faite la m�me chose avec medv et lstat.
plot(training_data$lstat, 
     training_data$medv, 
     xlab = "statut minimal de la population (pourcentage).", 
     ylab = "valeur m�diane des habitants-propri�taires en 1000$ par unit�.")

#9. Dessiner la droite de r�gression.
#10. V�rifier statiquement et graphiquement si la variable medv peut �tre expliqu� par un mod�le lin�aire simple par lstat.
#11. Que pouvez-vous d�duire ?



modelX = lm(training_data$medv~training_data$lstat)

plot(training_data$lstat, training_data$medv, main ="Scatterplot", xlab="Lstat", ylab="Median Value")
abline(modelX, col="red", lwd=6)

#12. Ex�cutez et commenter l�instruction pairs(Boston).
pairs(Boston)

#13. R�p�ter l�instruction mais uniquement avec la premi�re, la troisi�me et la septi�me variable.
pairs(Boston[,c(1,3,7)])

#14. utiliser l'ensemble de donn�es de formation pour former le mod�le lin�aire multiple avec comme variable expliqu�e medv et comme variables explicatives la variable lstat et la variable age.
#15. Que peut-on conclure ? Expliquez

model1 = lm(medv~ lstat + age, data = training_data)
summary(model1)

#16. utiliser l'ensemble de donn�es de formation pour former le mod�le lin�aire multiple avec comme variable expliqu�e medv et comme variables explicatives le logarithme de la variable lstat et la variable age. 
#17. Que peut-on conclure ? Expliquez
model2 = lm(medv~ log(lstat) + age, data = training_data)
summary(model2)

#18. V�rifier la relation lin�aire medv entre et toutes les autres variables.
#19. Que peut-on conclure ? Expliquez

model3 = lm(medv~., data = training_data)
summary(model3)

#20. Reconstruire le mod�le lin�aire avec toutes les variables sauf celles non significatives.
#21. Que peut-on conclure ? Expliquez
model4 = lm(medv~.-age-indus-black, data = training_data)
summary(model4)


#23. V�rifier si le mod�le est non lin�aire de degr� 2, 3, 4, 5, 6 et 7.
#24. Que peut-on conclure.
model6 = lm(medv~poly(lstat,2), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,3), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,4), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,5), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,6), data = training_data)
summary(model6)

model6 = lm(medv~poly(lstat,7), data = training_data)
summary(model6)

