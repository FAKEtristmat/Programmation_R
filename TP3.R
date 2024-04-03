install.packages("readxl")
library(readxl)
pokemon <- readxl::read_excel(path = "L:/BUT/SD/Promo 2023/acordier/Progra statistiques/pokemon.xlsx", sheet = "pokemon")
#lignes et colonnes du fichier
dim(pokemon)
ncol(pokemon)
nrow(pokemon)
summary(pokemon)
pokemon$generation <- as.factor(pokemon$generation)
pokemon$is_legendary <- as.factor(pokemon$is_legendary)
pokemon$type <- as.factor(pokemon$type)
summary(pokemon)
pokemon$attack_group <- ifelse(test = pokemon$attack > median(pokemon$attack), yes = "attack+", no = "attack-")
pokemon$attack_group <- as.factor(pokemon$attack_group)
summary(pokemon)
pokemon$water_fire <- ifelse(test = pokemon$type %in% c("water","fire"),yes = "yes",no = "no")
pokemon$water_fire <- as.factor(pokemon$water_fire)
summary(pokemon)
q3_attack <- quantile(pokemon$attack,probs = c(0.75) )
q3_defense <- quantile(pokemon$defense, probs = 0.75)
q3_speed <- quantile(pokemon$speed, probs = 0.75)
pokemon$best <- ifelse(test = pokemon$attack > q3_attack & pokemon$defense > q3_defense & pokemon$speed > q3_speed, yes = "yes", no = "no" )
pokemon$best <- as.factor(pokemon$best)
summary(pokemon)
requete <- subset(pokemon, is.na(weight_kg))
View(requete)
requete <- subset(pokemon, !is.na(weight_kg))
View(requete)
med_weight <- median(pokemon$weight_kg, na.rm = TRUE)
pokemon$weight_kgNa <- ifelse(test = is.na(pokemon$weight_kg), yes = med_weight, no = pokemon$weight_kg)
View(pokemon$weight_kgNa)
med_height <- median(pokemon$height_m, na.rm = TRUE)
pokemon$height_mNA <- ifelse(test = is.na(pokemon$height_m), yes = med_height, no = pokemon$height_m)
pokemon$weight_group <- cut(pokemon$weight_kg, breaks = 3, labels = c("léger","moyen","lourd"))
pokemon$height_m_group <- cut(pokemon$height_m, breaks = 4, labels = c("]0,1]","]1,2]","]2,3]","]3,max]"))#essai
#correction
pokemon$height_m_group = cut(pokemon$height_m,
                             breaks = c(0,1,2,3,
                                        max(pokemon$height_m,
                                            na.rm = TRUE)))
pokemon$defense_group = cut(pokemon$defense,
                            breaks = quantile(pokemon$defense,
                                              na.rm = TRUE),
                            include.lowest = TRUE)
summary(pokemon$defense_group)
moy_attack = aggregate(x = attack~ type, data = pokemon, FUN = function(x) mean(x))                          
aggregate(x = attack ~ generation + type, data = pokemon, FUN = function(x) median(x))  
aggregate(x = pokedex_number ~ type, data = pokemon, FUN = function(x) length(x))
aggregate(x = speed ~ generation + type, data = pokemon, FUN = function(x) c(moy = mean(x), med = median(x), eff = length(x))

          