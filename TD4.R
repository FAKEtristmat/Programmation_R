getwd()
#1.1
df <- read.csv("L:/BUT/SD/Promo 2023/acordier/Progra statistiques/datasets/velov.csv", header = TRUE, sep = ";", dec = ",")
#1.2
summary(df)
class(df$status)
class(df$CodePostal)
#1.3
df$status <- as.factor(df$status)
df$CodePostal <- as.factor(df$CodePostal)







summary(df)
#1.4
df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO" , "OK")
tables(df$bornes)
View(df)
#2.1
hist(df$capacity, main="Distribution des capacités", xlab = "capacity", col = "red")
#2.2
hist(df$capacity, main="Distribution des capacités", xlab = "capacity", col = "red", breaks = 6)
#2.5
hist(df$capacity, main="Distribution des capacités", xlab = "capacity", col = "red", breaks = 6)
abline(a=100, b=0, col="blue", lty=1)
#2.6
hist(x = df$capacity, main = "Distribution de \n la capacité des stations",col = "red", probability = TRUE, xlab = "Capacity")
#2.7
lines(x= density(df$capacity), lwd = 3, col="blue")
#2.8
hist(x = df$capacity, main = "Distribution de \n la capacité des stations",col = "red", probability = TRUE, xlab = "Capacity", ylim = c(0,0.08))
lines(x= density(df$capacity), lwd = 3, col="blue")
#3.1
boxplot(df$capacity,main="distrib des capacités")
#3.2
boxplot(df$capacity,main="distrib des capacités",outline=FALSE,horizontal = FALSE)
#3.4
points(df$capacity,col="red",pch=11)
#3.5
par(mfrow=c(1,2))
df7 = subset(df, CodePostal=="69007")
boxplot(df7$bikes,main= "distrib nb velos 7e arrondissement" ,ylim=c(0,40))
df8 = subset(df, CodePostal=="69008")
boxplot(df8$bikes,main= "distrib nb velos 8e arrondissement" ,ylim=c(0,40))

#3.6
par(mfrow=c(1,1)) #fenêtre sur 1 ligne et 1 colonne
# Tracer le graphique boxplot
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")



#3.7
moy = tapply(X=df$bikes,INDEX = df$bonus, FUN =function(X) mean(X))
print(moy)

points(moy, col = "red",pch=19)

#4.1
effectif = table(df$bonus)
barplot(effectif, main="repartition nombre de station bonus")

#4.2
barplot(effectif, main="repartition nombre de station bonus",horiz = TRUE )

#4.3
proba = prop.table(effectif)
barplot(proba, main="repartition en % du station bonus",horiz = TRUE )

#4.4

effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?")
#On remarque qu'on ne sait pas distinguer les deux modalités car il n'y a pas de légende.


#4.5






