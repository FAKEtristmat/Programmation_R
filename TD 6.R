#EXERCICE 1

  df <- read.csv(file = "C:/Users/tchabanel/Downloads/NBA2014_2015.csv", sep = ",",
                 header = TRUE, dec = ".")
  # changé le chemin d'acces le header TRUE, le sep et le dec
  nrow(df)
  # l'excel est definie par la variable df
  ncol(df)
  # l'excel est definie par la variable df
  colnames(df)
  # colnames avec un S
  srt(df)
  
  df$Period <- as.factor(df$PERIOD)
  # mis des majuscule
  df$PTSTYPE = as.factor(df$PTS_TYPE)
  # changer la direction de la fleche et le nom de l'attribut que l'on veut modifier
  
  df$SHOOTER = as.factor(df$SHOOTER)
  #changer le nom de l'attribut en majuscule
  
#EXERCICE 2
  
   length(df$Period)
   length(df$PTSTYPE)
   length(df$SHOOTER)
   #modifier le nom de la commande length et les nom des attributs demandé
   
   summary(df)
   # pas summary(ddf) mais df car c'est le nom de notre table excel
   
   sd(df$SHOT_DIST)
   sd(df$SHOT_CLOCK,na.rm = TRUE)
   #na.rm car dans le summary on le voit   
       
   #combien de tirs manqués/réussis
   table(df$SHOT_RESULT)
   # changer la sintaxe de la phrase
   
   
   #les quartiles
   quantile(df$SHOT_CLOCK, probs = seq(from =0.25, to =1, by =0.25), na.rm = TRUE)
   #il faut creer une sequence de 0 a 1 avec un pas de 0.25    
   
   #les déciles
   quantile(df$CLOSE_DEF_DIST, probs = seq(from =0, to =1, by=0.1),na.rm = TRUE)
   # enlever le s de quantiles et une sequence de 0 a 1 avec un pas de 0.1
   
   #nombre de matches différents
   liste_game <- unique(df$GAME_ID)
   length(liste_game)
   #supprimer une parenthese et modifier le nom de la liste_game dans le length
   
   
  #nombre de joueurs différents
  df$SHOOTER <- as.factor(df$SHOOTER)
  # le point au as.factor
  
  nlevels(df$SHOOTER)
  # oublier le s au nlevels et la )
  
  #conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
  df$SHOT_DIST_METRE = df$SHOT_DIST * 0.30
  #suppression des double egalité     
         
         
  #nombre de points qu'a rapporté la tentative (0,2 ou 3)  
  df$PTS_MARQUES <- ifelse(df$SHOT_RESULT == "made", yes = df$PTS_TYPE, 0)
  # ==        
  
  #On supprime la variable GAME_RESULT car elle n'est pas utile
  df$GAME_RESULT <- NULL
  # null avec 2 l

         
  #création d'un objet sans la première colonne GAME_ID
  df2 <- [ df,-1  ]

  
  
  
#EXERCICE 3
  
  #Les 100 tirs réussis ou manqués les plus loin
  rang <- order(df$SHOT_DIST, decreasing = TRUE)
  df3 <- df[rang,]
  df3 <- df3[ 1 : 100 ,]
  View(df3)  
  #Les 100 tirs réussis les plus loin
  rang <- order(df$SHOT_DIST, decreasing = TRUE)
  df4 = subset(df3, SHOT_RESULT == "made")
  df4 <- df4[ 1 : 100 ,]
  View(df4)
  
  #Combien de tirs à 3 points a réussi Kobe Bryant ?
  df_kobe = subset(df,SHOT_RESULT == "made" &
                     PTS_TYPE == 3 & 
                     SHOOTER == "kobe bryant")
  print(df_kobe)
  nrow(df_kobe)
  
  #Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
  df_total <- aggregate(PTS_TYPE ~ SHOOTER, data = df, FUN = sum)
  df_total_tri <- order(df_total$PTS_TYPE, decreasing = FALSE)
  View(df_total_tri)
  df_top5 <-  df_total_tri[  5  ,  ]
  
  