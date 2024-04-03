#EXERCICE 1

  #1
par(mfrow = c(1,1))
plot(NA,xlim= c(-5,5), ylim= c (0,1), xlab = "X", ylab = "Densité proba", main = "graph loi normale")

  #2
moyenne = c(0,0,0,-2)
ecart = c(0.45,1,2.25,0.7)
colors = c("red","blue","green","orange")
legend_labels = c()
for (i in 1:length(moyenne)) {
  serie = rnorm(n = 1000,
                mean = moyenne[i],
                sd = ecart[i])
  lines(density(serie), col = colors[i])
  legend_labels = c(legend_labels, paste ("m =", moyenne[i], "," , "s =", ecart[i]))
}

legend("topright", legend = legend_labels , col = colors , lwd = 2 , cex=0.8)
  
  #3
serie = rnorm(n =10 000, mean = 0, sd = 1)

  #4
hist(serie, main = "loi normal centrée-réduite", probability = TRUE)
lines(density(serie))

  #5

median(serie)

  #6

quantile(serie)

  #7
 quantile(serie, probs = seq(0,1,0.01))

quantile(serie , probs = 0.95)

  #8

qnorm(p =0.95, mean= 0 , sd = 1)
pnorm(q=1.644854, mean = 0, sd=1)


  #9
qnorm(p=0.975, mean = 0, sd= 1)


  #10

pnorm (q=1.96, mean = 0, sd=1)

#EXERCICE 2

  #1
indices_ligne = seq(from = 0 , to=3.9 , by=0.1)
all_probas = c()

for (i in indices_ligne) {
  proba = pnorm(q = i, mean = 0, sd=1)
  all_probas = c(all_probas, proba)
  all_probas = round(all_probas, 4)
} 

  #2

indices_colones = seq(from = 0.00, to = 0.09, by = 0.01)
indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#On crée un objet résultat vide.
resultat = NULL
#On parcourt les indices colonnes
for (j in indices_colones) {
  #on crée un vecteur vide pour ajouter les probas au fur et à mesure
  all_probas = c()
  #On parcourt les indices lignes
  for (i in indices_lignes){
    quantile = i + j
    proba = pnorm(q = quantile, mean = 0, sd = 1)
    #on ajoute la nouvelle proba au vecteur existant
    all_probas = c(all_probas,proba)
    all_probas = round(all_probas,digits = 4)
  }
  #On ajoute une colonne au resultat
  resultat = cbind(resultat,all_probas)
}

  #3
class(resultat)
table = data.frame(resultat)
colnames(table) = indices_colones
rownames(table) = indices_lignes
View(table)

#EXERCICE 3

  #1
population = rnorm(n=1e7, mean=171, sd=9)
View(population)

#2

mean(population)
sd(population)

#3

hist(population)
nb_ta = 0
for (i in 1:length(population)) {
  if (population[i] < 190){
    nb_ta = nb_ta + 1
  }
}
print(nb_ta)

#4

hist(population)
nb_ta = 0
for (i in 1:length(population)) {
  if (population[i] > 200){
    nb_ta = nb_ta + 1
  }
}
print(nb_ta)

#EXERCICE 4







