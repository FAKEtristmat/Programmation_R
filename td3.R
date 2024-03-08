#EXERCICE 1
 #1/

 install.packages("readxl")
 library(readxl)
 pokemon =read_excel(path ="L:/BUT/SD/Promo 2023/tchabanel/R/dataset/pokemon.xlsx",sheet = "pokemon")

  #2/
 dim(pokemon)
 
  #3/
 
 summary(pokemon)

  #4/
 
 pokemon$is_legendary = as.factor(pokemon$is_legendary)
 pokemon$generation = as.factor(pokemon$generation)
 pokemon$type = as.factor(pokemon$type)
 
  #5/
 
 summary(pokemon)

 #EXERCICE 2
 
  #1/
  med = median(pokemon$attack)
  pokemon$attack_group= ifelse(pokemon$attack >= med ,yes= "attack+",no="attack-")
  pokemon$attack_group = as.factor(pokemon$attack_group)
  summary(pokemon$attack_group)
  
  #2/
  
  pokemon$water_fire = ifelse(pokemon$type %in% c("water","fire"),"yes","no")
  pokemon$water_fire = as.factor(pokemon$water_fire)
  summary(pokemon$water_fire)
  
  #3/
  quartattack = quantile(pokemon$attack, probs=0.75)
  quartdefense = quantile(pokemon$defense, probs = 0.75)
  quartspeed = quantile(pokemon$speed, probs=0.75)
  
  pokemon$best = ifelse(pokemon$attack > quartattack &
                        pokemon$defense > quartdefense &
                        pokemon$speed > quartspeed, "yes","no")
  pokemon$best = as.factor(pokemon$best)
  summary(pokemon$best)
  
  #1/
  
  requete = subset(pokemon, is.na(weight_kg))
  View(requete)
  
  #2/
  
  
  requete = subset(pokemon, !is.na(weight_kg))
  View(requete)
  
  #3/
  
  medna_weight = median(pokemon$weight_kg , na.rm = TRUE)
  pokemon$weight_kgNa = ifelse(is.na(pokemon$weight_kg),
                               medna_weight,
                               pokemon$weight_kg)
  medna_height = median(pokemon$height_m, na.rm = TRUE)
  pokemon$height_mNA = ifelse(is.na(pokemon$height_m),
                              medna_height,
                              pokemon$height_m)
  
  #1/
  
  pokemon$weight_group = cut(pokemon$weight_kg,
                             breaks = 3,
                             labels = c("léger","moyen","lourd"))
  
  
  #2/
  
  pokemon$height_m_group = cut(pokemon$height_m,
                               breaks = c(0,1,2,3,
                                          max(pokemon$height_m,
                                              na.rm = TRUE)))
  #3/
  
  pokemon$defense_group = cut(pokemon$defense,
                              breaks = quantile(pokemon$defense,
                                                na.rm = TRUE),
                              include.lowest = TRUE)
  summary(pokemon$defense_group)
  
  
  
#EXERCICE 3
  
  #1/
  aggregate(x = attack ~ type, data = pokemon, FUN = function(x) mean(x))
  
  #2/
  aggregate(x = attack ~ type, data = pokemon, FUN = function(x) median(x))
  
  #3/
  
  aggregate(x = pokedex_number ~ type, data = pokemon, FUN = function(x) length(x))
  
  #4/
  
  aggregate(speed ~ generation + type,
            data = pokemon, 
            FUN = function(x) c(moy = mean(x),
                                med = median(x),
                                eff = length(x) ) )
  
  
  
  

  
  
  
  
  
 
 
 
 
 
 
 