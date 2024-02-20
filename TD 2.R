
#EXERCICE 1
#ourvrir le répertoire courant par défaut
2)
getwd()

3)
setwd("L://BUT/SD/Promo 2023/tchabanel/R/dataset")
getwd

4)
bodies_kart = read.csv(file ="bodies_karts.csv" , header =TRUE, sep = ";", dec = ",")
drivers = read.csv(file ="drivers.csv", header=TRUE, sep = ";", dec = ",")
tires = read.csv(file = "tires.csv", header=TRUE, sep="\t", dec=",")
gliders = read.csv(file="gliders.csv", header=TRUE, sep="|", dec=".")

5)
dim(bodies_kart)
dim(drivers)
dim(tires)
dim(gliders)

#EXERICICE 2
1)
# effectuer un résumé des données
summary(bodies_kart)
summary(drivers)
summary(tires)
summary(gliders)

2)
plot(x= drivers$Weight,
     y= drivers$Acceleration,
     main = "lien Drivers : Weight / Acceleration")
# on observe une correlation négative
#Il y a autant de points mais ils sont superposés car certains drivers ont les mêmes statistiques

3)
#coefficient de corrélation
 cor(x=drivers$Weight,
    y=drivers$Acceleration)
 4)
covXY = cov(x=drivers$Weight,
            y=drivers$Acceleration)
sdX= sd(drivers$Weight)
sdY = sd(drivers$Acceleration)
print(covXY/(sdX*sdY))

5)
#coefficient de détermination
coefCorr = cor(x=drivers$Weight,
               y=drivers$Acceleration)
coefDeter = coefCorr^2
print(coefDeter)

6)
matriceCor = cor(drivers[,-1]) #le -1 enleve la premiere colonne car elle n'est pas numerique
matriceCor = round(matriceCor,2)
View(matriceCor)

7)
# installe le packages corrplot
install.packages("corrplot")

8)
library(corrplot)
corrplot(matriceCor,method="circle")


9)

matriceCor = round(cor(tires[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE)



matriceCor = round(cor(bodies_karts[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE)


matriceCor = round(cor(gliders[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE)

#EXERCICE 3
1)
resultat = drivers[ , c("Driver" , "Weight")]
View(resultat)

2)
resultat = drivers[ 1:10, c("Driver" , "Acceleration")]
View(resultat)

3)
resultat = drivers[,c(-5,-7,-9)]
View(resultat)

#ou
resultat = drivers[ , -c(5,7,9)]
View(resultat)

4)














