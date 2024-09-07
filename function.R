# Fichier d'appelation des fonctions ----
# Ce fichier R répertorie l'ensemble des fonctions utilisables avec les données IFN

# Pour des questions d'optimistion
# CHARGER UNIQUMENT CE FICHIER AVEC LE BOUTON :
#----------SOURCE---------



# Fonction importation zone d'étude particulière ----
get_import_zone <- function(){
  shp_path <- file.choose()  # Ouvrir les fichiers locaux du PC
  
  shp_etude <<- st_read(shp_path)  # Importer le shapefile sélectionné ds l'env
  # Permet à l'utilisateur de dessiner la zone d'étude
  #drawn_zone <- editMap()
  
  # Convertir l'objet en sf si nécessaire
  #shp_etude <<- st_as_sf(drawn_zone$finished)  # "finished" contient la géométrie dessinée
  
  return (shp_etude)  # Visualiser les géométries
}

# Fonction obtention buffer et placette à l'intérieur ----
get_buffer_zone <- function(buffer = 1500){
  # Création de la zone tampon autour du shapefile
  zone_tampon <- st_buffer(shp_etude, dist = buffer)
  placette_tampon <- st_intersection(placette, zone_tampon)
  
  # Obtenir les limites combinées de toutes les couches pour ajuster le zoom
  all_bounds <- st_bbox(st_union(st_geometry(shp_etude), st_geometry(zone_tampon), st_geometry(placette_tampon)))
  
  # Afficher la carte avec tmap et ajuster le zoom sur toutes les couches
  plot_zone <- tm_shape(shp_etude, bbox = all_bounds) +  # Zone d'étude originale avec ajustement des limites
    tm_fill(col = "lightgreen", alpha = 0.5) +  # Remplissage vert avec transparence
    tm_borders(col = "black") +  # Contour noir
    tm_shape(zone_tampon) +  # Zone tampon
    tm_borders(col = "blue", lty = "dashed") +  # Ligne bleue en pointillé
    tm_shape(placette_tampon) +  # Placettes dans la zone tampon
    tm_symbols(col = "red") +  # Placettes en rouge
    tm_layout(main.title = "Placettes dans la zone tampon",
              frame = FALSE)  # Ajouter un titre, sans cadre
  
  # Extraire les IDP des placettes dans la zone tampon
  idp_placette_tampon <- placette_tampon$IDP
  
  # Filtre ARBRE
  arbre_zone_etude <<- arbre[arbre$IDP %in% idp_placette_tampon, ]
  
  return(plot_zone)
}





# Fonction du calcul d'accroissement ----

get_acc_G <- function(buffer = 1500){
  get_import_zone()
  get_buffer_zone(buffer)
  arbre_zone_etude$Essence <- NA
  # Fabrication d'un numéro unique
  arbre_zone_etude$num_unique <- paste(arbre_zone_etude$IDP,
                                       arbre_zone_etude$A,
                                       sep = ".")
  # Créer une nouvelle colonne qui stocke toutes les années de campagne pour chaque IDP
  arbre_zone_etude <- arbre_zone_etude %>%
    group_by(num_unique) %>%
    mutate(
      annee_mesure = paste(unique(CAMPAGNE), collapse = ", "),# Créer une chaîne de caractères avec les années uniques
      circ_mesure = paste(na.omit(C13), collapse = ", ")  # Créer une liste avec les mesures de circonférecnes
    ) %>%
    ungroup()
  
  # Les sections vides dans ESPAR sont remplacé par NA
  arbre_zone_etude$ESPAR[arbre_zone_etude$ESPAR == "" | arbre_zone_etude$ESPAR == " "] <- NA
  
  # Remplir les valeurs manquantes de code ESPAR pour chaque arbre
  arbre_zone_etude_cor <- arbre_zone_etude %>%
    group_by(num_unique) %>%          # Grouper par le numéro unique
    fill(ESPAR, .direction = "downup") %>%  # Remplir les valeurs manquantes par les valeurs non manquantes
    ungroup()  # Désactiver le regroupement
  
  
  # Boucle pour parcourir chaque ligne du tableau `arbres`
  for (i in 1:nrow(arbre_zone_etude_cor)) {
    # Récupérer le code ESPAR pour l'arbre à la ligne `i`
    code_espar <- arbre_zone_etude_cor$ESPAR[i]
    
    # Trouver l'essence correspondante dans la table `metadonnees`
    essence_correspondante <- code_essence$libelle[code_essence$code == code_espar]
    
    # Vérifier si une essence a été trouvée
    if (length(essence_correspondante) > 0) {
      # Ajouter l'essence à la colonne "Essence" de `arbres`
      arbre_zone_etude_cor$Essence[i] <- essence_correspondante
    } else {
      # Si aucun code ESPAR correspondant n'est trouvé, laisser la valeur par défaut (NA)
      arbre_zone_etude_cor$Essence[i] <- NA
    }
  }
  
  # On sélectionne uniquement quelques colonnes utile
  arbre_zone_etude_cor <- arbre_zone_etude_cor %>%
    select(IDP,CAMPAGNE, num_unique, Essence, C13,C0, HTOT, HDEC, V, W, IR5, IR1, annee_mesure, circ_mesure) %>%
    
    pivot_wider(names_from = CAMPAGNE,   # Créer des colonnes pour chaque année
                values_from = C13) %>%  # Les valeurs à placer dans ces colonnes sont les circonférences mesurées
    
    group_by(num_unique) %>%
    summarise(across(everything(), ~ first(na.omit(.)), .names = "{col}"), .groups = "drop")
  
  # On cherche la circonférence max dans la liste
  arbre_zone_etude_cor <- arbre_zone_etude_cor %>%
    group_by(num_unique) %>%
    # Calculer l'accroissement annuel
    mutate(
      # Séparer la chaîne de caractères en vecteur d'années pour chaque ligne
      annee_vector = list(as.numeric(strsplit(annee_mesure, ",\\s*")[[1]])),
      circ_vector = list(as.numeric(strsplit(circ_mesure, ",\\s*")[[1]])),
      
      # Extraire l'année maximale et minimale
      annee_max = max(annee_vector[[1]], na.rm = TRUE),  # L'année de la circonférence max
      annee_min = min(annee_vector[[1]], na.rm = TRUE),  # L'année de la circonférence min
      
      # Extraire la circonférence maximale et minimale
      circonference_max = max(circ_vector[[1]], na.rm = TRUE),  # La circonférence max
      circonference_min = min(circ_vector[[1]], na.rm = TRUE))%>%  # La circonférence min
    
    mutate(cat_diam = case_when(
      circonference_max >= 0.235 & circonference_max < 0.705 ~ "PB",   # Petite Bois (PB)
      circonference_max >= 0.705 & circonference_max < 1.175 ~ "BM",  # Bois Moyen (BM)
      circonference_max >= 1.175 ~ "GB",  # Gros Bois (GB))
      TRUE ~ NA_character_ ))%>%
    
    mutate(w = case_when(
      cat_diam == "PB" ~ 88.4,
      cat_diam == "BM" ~ 39.3,
      cat_diam == "GB" ~ 14.1,
      TRUE ~ NA_real_)) %>%
    
    mutate(
      # On calcul le diamètre de l'arbre
      diam = round((circonference_max / pi)/0.01,1),
      # On calcul sa classe de diam
      clas_diam = round(diam / 5) * 5
    ) %>%
    
    mutate(
      # Calcul accroissement en G/ha/an
      g_min = ((circonference_min^2) / (4 * pi)),
      g_max = ((circonference_max^2) / (4 * pi)),
      acc_g_ha = if_else(
        circonference_max != circonference_min & annee_max != annee_min,
        ((g_max - g_min) * w / (annee_max - annee_min)),
        NA_real_  # Sinon NA
    ))
  
  
  # Calculer les moyennes d'accroissement par essence, catégorie de diamètre et placette
  table_recap_placette <- arbre_zone_etude_cor %>%
    filter(is.finite(acc_g_ha)) %>%  # Exclure les valeurs infinies et petites
    group_by(IDP, Essence, cat_diam) %>%  # Groupement par Placette, Essence et catégorie de diamètre
    summarise(
      moyenne_accroissement = sum(acc_g_ha, na.rm = TRUE),  # Moyenne d'accroissement pour chaque placette
      .groups = 'drop'
    )
  
  # Calculer la moyenne sur toutes les placettes par essence et catégorie de diamètre
  table_recap_global <- table_recap_placette %>%
    group_by(Essence, cat_diam) %>%  # Groupement par Essence et catégorie de diamètre seulement
    summarise(
      moyenne_accroissement_placettes = mean(moyenne_accroissement, na.rm = TRUE),  # Moyenne globale sur toutes les placettes
      .groups = 'drop'
    ) %>%
    mutate(moyenne_accroissement_placettes = round(moyenne_accroissement_placettes, 3)) %>%  # Arrondir à 0.001 près
    pivot_wider(
      names_from = cat_diam,  # Colonnes pour chaque catégorie de diamètre
      values_from = moyenne_accroissement_placettes,
      values_fill = list(moyenne_accroissement_placettes = NA)  # Remplir les valeurs manquantes avec NA
    ) %>%
    arrange(Essence)  # Trier par essence
  
  # Calculer la moyenne globale par essence sans distinction de catégorie de diamètre
  table_recap_global_sans_diam <- arbre_zone_etude_cor %>%
    filter(is.finite(acc_g_ha)) %>%
    group_by(IDP, Essence) %>%  # Groupement par Placette et Essence uniquement
    summarise(
      moyenne_accroissement_sans_diam = sum(acc_g_ha, na.rm = TRUE),  # Moyenne d'accroissement par placette sans cat_diam
      .groups = 'drop'
    ) %>%
    group_by(Essence) %>%  # Groupement par essence pour faire la moyenne globale de toutes les placettes
    summarise(
      moyenne_accroissement_sans_diam = mean(moyenne_accroissement_sans_diam, na.rm = TRUE),  # Moyenne globale sur toutes les placettes
      .groups = 'drop'
    ) %>%
    mutate(moyenne_accroissement_sans_diam = round(moyenne_accroissement_sans_diam, 3))  # Arrondir à 0.001 près
  
  # Fusionner les résultats avec ou sans catégorie de diamètre
  table_recap_final_G <<- table_recap_global %>%
    left_join(table_recap_global_sans_diam, by = "Essence")  # Ajouter la moyenne sans catégorie de diamètre
  
  return(table_recap_final_G)
}



# Fonction calcul de volume----

get_acc_V <- function(buffer = 1500){
  get_import_zone()
  get_buffer_zone(buffer)
  arbre_zone_etude$Essence <- NA
  # Fabrication d'un numéro unique
  arbre_zone_etude$num_unique <- paste(arbre_zone_etude$IDP,
                                       arbre_zone_etude$A,
                                       sep = ".")
  # Créer une nouvelle colonne qui stocke toutes les années de campagne pour chaque IDP
  arbre_zone_etude <- arbre_zone_etude %>%
    group_by(num_unique) %>%
    mutate(
      annee_mesure = paste(unique(CAMPAGNE), collapse = ", "),# Créer une chaîne de caractères avec les années uniques
      circ_mesure = paste(na.omit(C13), collapse = ", ")  # Créer une liste avec les mesures de circonférecnes
    ) %>%
    ungroup()
  
  # Les sections vides dans ESPAR sont remplacé par NA
  arbre_zone_etude$ESPAR[arbre_zone_etude$ESPAR == "" | arbre_zone_etude$ESPAR == " "] <- NA
  
  # Remplir les valeurs manquantes de code ESPAR pour chaque arbre
  arbre_zone_etude_cor <- arbre_zone_etude %>%
    group_by(num_unique) %>%          # Grouper par le numéro unique
    fill(ESPAR, .direction = "downup") %>%  # Remplir les valeurs manquantes par les valeurs non manquantes
    ungroup()  # Désactiver le regroupement
  
  
  # Boucle pour parcourir chaque ligne du tableau `arbres`
  for (i in 1:nrow(arbre_zone_etude_cor)) {
    # Récupérer le code ESPAR pour l'arbre à la ligne `i`
    code_espar <- arbre_zone_etude_cor$ESPAR[i]
    
    # Trouver l'essence correspondante dans la table `metadonnees`
    essence_correspondante <- code_essence$libelle[code_essence$code == code_espar]
    
    # Vérifier si une essence a été trouvée
    if (length(essence_correspondante) > 0) {
      # Ajouter l'essence à la colonne "Essence" de `arbres`
      arbre_zone_etude_cor$Essence[i] <- essence_correspondante
    } else {
      # Si aucun code ESPAR correspondant n'est trouvé, laisser la valeur par défaut (NA)
      arbre_zone_etude_cor$Essence[i] <- NA
    }
  }
  
  # On sélectionne uniquement quelques colonnes utile
  arbre_zone_etude_cor <- arbre_zone_etude_cor %>%
    select(IDP,CAMPAGNE, num_unique, Essence, C13,C0, HTOT, HDEC, V, W, IR5, IR1, annee_mesure, circ_mesure) %>%
    
    pivot_wider(names_from = CAMPAGNE,   # Créer des colonnes pour chaque année
                values_from = C13) %>%  # Les valeurs à placer dans ces colonnes sont les circonférences mesurées
    
    group_by(num_unique) %>%
    summarise(across(everything(), ~ first(na.omit(.)), .names = "{col}"), .groups = "drop")
  
  # On cherche la circonférence max dans la liste
  arbre_zone_etude_cor <- arbre_zone_etude_cor %>%
    group_by(num_unique) %>%
    # Calculer l'accroissement annuel
    mutate(
      HTOT = as.numeric(HTOT),
      # Séparer la chaîne de caractères en vecteur d'années pour chaque ligne
      annee_vector = list(as.numeric(strsplit(annee_mesure, ",\\s*")[[1]])),
      circ_vector = list(as.numeric(strsplit(circ_mesure, ",\\s*")[[1]])),
      
      # Extraire l'année maximale et minimale
      annee_max = max(annee_vector[[1]], na.rm = TRUE),  # L'année de la circonférence max
      annee_min = min(annee_vector[[1]], na.rm = TRUE),  # L'année de la circonférence min
      
      # Extraire la circonférence maximale et minimale
      circonference_max = max(circ_vector[[1]], na.rm = TRUE),  # La circonférence max
      circonference_min = min(circ_vector[[1]], na.rm = TRUE))%>%  # La circonférence min
    
    mutate(cat_diam = case_when(
      circonference_max >= 0.235 & circonference_max < 0.705 ~ "PB",   # Petite Bois (PB)
      circonference_max >= 0.705 & circonference_max < 1.175 ~ "BM",  # Bois Moyen (BM)
      circonference_max >= 1.175 ~ "GB",  # Gros Bois (GB))
      TRUE ~ NA_character_ ))%>%
    
    mutate(w = case_when(
      cat_diam == "PB" ~ 88.4,
      cat_diam == "BM" ~ 39.3,
      cat_diam == "GB" ~ 14.1,
      TRUE ~ NA_real_)) %>%
    
    mutate(
      # On calcul le diamètre de l'arbre
      diam = round((circonference_max / pi)/0.01,1),
      # On calcul sa classe de diam
      clas_diam = round(diam / 5) * 5
    ) %>%
    
    mutate(
      # Calcul accroissement en V/ha/an
      g_min = ((circonference_min^2) / (4 * pi)),
      g_max = ((circonference_max^2) / (4 * pi)),
      acc_V_ha = if_else(
        circonference_max != circonference_min & annee_max != annee_min,
        ((0.8 * g_max * HTOT)*w) - ((0.8 * g_min * HTOT*w)) / (annee_max - annee_min),
        NA_real_  # Sinon NA
      ))
  
  
  # Calculer les moyennes d'accroissement par essence, catégorie de diamètre et placette
  table_recap_placette <- arbre_zone_etude_cor %>%
    filter(is.finite(acc_V_ha)) %>%  # Exclure les valeurs infinies et petites
    group_by(IDP, Essence, cat_diam) %>%  # Groupement par Placette, Essence et catégorie de diamètre
    summarise(
      moyenne_accroissement = sum(acc_V_ha, na.rm = TRUE),  # Moyenne d'accroissement pour chaque placette
      .groups = 'drop'
    )
  
  # Calculer la moyenne sur toutes les placettes par essence et catégorie de diamètre
  table_recap_global <- table_recap_placette %>%
    group_by(Essence, cat_diam) %>%  # Groupement par Essence et catégorie de diamètre seulement
    summarise(
      moyenne_accroissement_placettes = mean(moyenne_accroissement, na.rm = TRUE),  # Moyenne globale sur toutes les placettes
      .groups = 'drop'
    ) %>%
    mutate(moyenne_accroissement_placettes = round(moyenne_accroissement_placettes, 3)) %>%  # Arrondir à 0.001 près
    pivot_wider(
      names_from = cat_diam,  # Colonnes pour chaque catégorie de diamètre
      values_from = moyenne_accroissement_placettes,
      values_fill = list(moyenne_accroissement_placettes = NA)  # Remplir les valeurs manquantes avec NA
    ) %>%
    arrange(Essence)  # Trier par essence
  
  # Calculer la moyenne globale par essence sans distinction de catégorie de diamètre
  table_recap_global_sans_diam <- arbre_zone_etude_cor %>%
    filter(is.finite(acc_V_ha)) %>%
    group_by(IDP, Essence) %>%  # Groupement par Placette et Essence uniquement
    summarise(
      moyenne_accroissement_sans_diam = sum(acc_V_ha, na.rm = TRUE),  # Moyenne d'accroissement par placette sans cat_diam
      .groups = 'drop'
    ) %>%
    group_by(Essence) %>%  # Groupement par essence pour faire la moyenne globale de toutes les placettes
    summarise(
      moyenne_accroissement_sans_diam = mean(moyenne_accroissement_sans_diam, na.rm = TRUE),  # Moyenne globale sur toutes les placettes
      .groups = 'drop'
    ) %>%
    mutate(moy_acc_V_m3_ha = round(moyenne_accroissement_sans_diam, 3))  # Arrondir à 0.001 près
  
  # Fusionner les résultats avec ou sans catégorie de diamètre
  table_recap_final_V <<- table_recap_global %>%
    left_join(table_recap_global_sans_diam, by = "Essence")  # Ajouter la moyenne sans catégorie de diamètre
  
  return(table_recap_final_V)
}





# Obtenir les placettes d'une sylvoecoregion ----
get_placette_sylvo_eco <- function(sylvoecoregion) {
  # Charger les données ser et des placettes
  data("ser")           # Charger l'objet 'ser'
  
  # Filtrer le polygone correspondant au nom donné
  polygone <- ser %>% filter(ser[[2]] == sylvoecoregion)
  
  if (nrow(polygone) == 0) {
    stop("Le nom du polygone n'existe pas dans la table.")
  }
  
  # S'assurer que les systèmes de coordonnées sont identiques
  if (st_crs(placette) != st_crs(polygone)) {
    placette <- st_transform(placette, st_crs(polygone))
  }
  
  # Utiliser une approche vectorisée pour la vérification
  within_list <- st_within(placette, polygone)
  
  # Filtrer les placettes qui sont dans le polygone
  placettes_sylvo_eco <<- placette[lengths(within_list) > 0, ]
  
  return(placettes_sylvo_eco)
}





