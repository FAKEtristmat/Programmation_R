

#EXERCICE 1
  #a)
df = read.csv("C://Users/chaba/Desktop/COURS/R/fao.csv", header = TRUE, sep =";", dec = ",")
str(df)

  #b)
nb_pays=nrow(df)     #lire le nombre de ligne
print(nb_pays)

  #c)
resumé = summary(df)
print(resumé)

#EXERCICE 2
  #a)
disponibilité_moyenne_mondial = mean(df$Dispo_alim)
print(disponibilité_moyenne_mondial)

  #b)
nb_habitant = sum(df$Population, na.rm = TRUE )
print(nb_habitant)

  #c)
ecart_type_impV = sd(df$Import_viande, na.rm = FALSE)
ecart_type_expV = sd(df$Export_viande, na.rm = TRUE)
print(ecart_type_impV)
print(ecart_type_expV)

  #d)
mediane_volProd = median(df$Prod_viande , na.rm = TRUE)
print(mediane_volProd)

  #e)
quartile_dispoA = quantile(df$Dispo_alim , probs = c(0.25,0.5,0.75), na.rm = TRUE)
print(quartile_dispoA)

  #f)
centile_VolImp = quantile(df$Import_viande , probs = seq(0,1,0.01), na.rm = TRUE)
print(centile_VolImp)

#EXERCICE 3
  #a)
df_trie <- df[order(df$Population), ]


cinq_premiers_pays <- head(df_trie, 5)
print(cinq_premiers_pays)
View(cinq_premiers_pays)

  #b)

# Trier le dataframe par la colonne "Population" de manière descendante
df_trie <- df[order(df$Population, decreasing = TRUE), ]

# Afficher les cinq premières lignes du dataframe trié
cinq_premiers_pays <- head(df_trie, 5)
print(cinq_premiers_pays)
View(cinq_premiers_pays)

  #c)

df_trie <- df[order(df$Prod_viande, decreasing = TRUE), ]

cinq_premiers_prod <- head(df_trie, 5)
print(cinq_premiers_prod)
View(cinq_premiers_prod)

  #d)
df_trie <- df[order(df$Import_viande, decreasing = TRUE), ]

cinq_premiers_import <- head(df_trie, 5)
print(cinq_premiers_import)
View(cinq_premiers_import)

  #e)
pays_dispo_superieure_ou_egale_2300 = df[df$Dispo_alim>=2300,]
print(pays_dispo_superieure_ou_egale_2300)
View(pays_dispo_superieure_ou_egale_2300)
l = nrow(pays_dispo_superieure_ou_egale_2300)
print(l)
  #f)
ff = df[df$Dispo_alim>3500 & df$Import_viande>=1000000,]
print(nrow(ff))
print(ff)
View(ff)

  #g)
Fr_Be=subset(df,df$Nom %in% c("France","Belgique"),)
print(Fr_Be)
View(Fr_Be)

#EXERCICE 4

  #a)

df$part_export = df$Export_viande / df$Prod_viande * 100








