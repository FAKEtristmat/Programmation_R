#EXERCICE 1
  #1
  getwd()
  setwd("L:/BUT/SD/Promo 2023/tchabanel/R/nba")
  getwd()
  
  #2
  fichiers = list.files(path =getwd(),pattern = ".csv$",full.names= TRUE)
  
  #3
  library(tools)
  print(fichiers[1])
  nom_fichier = basename(path = fichiers[1])
  nom_fichier_sans_extension = file_path_sans_ext(x=nom_fichier)
  print(nom_fichier_sans_extension)
  
  #4
  # Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
  assign(x = nom_fichier_sans_extension, 
         value = read.csv(fichier[1],
                          sep = ",",
                          dec = "."))
  #un dataframe vient d'être créé avec comme nom d'objet le nom du fichier sans extension.
         
         
  #5

  # Boucle pour lire chaque fichier CSV
  for (fichier in fichiers) {
    # Extraire le nom du fichier sans extension
    nom_objet <- file_path_sans_ext(basename(fichier))
    
    # Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
    start_time <- Sys.time()
    assign(nom_objet, read.csv(fichier, 
                               sep = ",",
                               dec = "."))
    end_time <- Sys.time()
    # Calcul du temps écoulé
    execution_time <- end_time - start_time
    cat("Importation : ",nom_objet, "=" , execution_time , "\n")
  } 
    
#EXERCICE 2
  
  #6
  df_x = subset(team, city == "Los Angeles", select = c("id", "city"))
  df_y = subset(game, select = c("game_id", "team_id_home"))
  dfJoin = merge(x = df_x, y = df_y, 
                 by.x = "id", 
                 by.y = "team_id_home", 
                 all.x = TRUE)
  nrow(dfJoin)
  View(dfJoin)
  
  #7
  df_x = dfJoin
  df_y = subset(game_info, select = c("game_id", "attendance"))
  dfJoin = merge(x = df_x, y = df_y, 
                 by = "game_id",
                 all.x = TRUE)
  mean(dfJoin$attendance, na.rm = TRUE)
  View(dfJoin)
  
  #8
  df_x = subset(game_summary, season == 2020,
                select = c("game_id", "season"))
  dfJoin = merge(x = df_x, y = officials, 
                 by = "game_id",
                 all.x = TRUE)
  length(unique(dfJoin$official_id))
  View(dfJoin)
  
  
  
  