# Fichier d'appelation des fonctions ----
# Ce fichier R répertorie l'ensemble des fonctions utilisables avec les données IFN

# Pour des questions d'optimistion
# CHARGER UNIQUMENT CE FICHIER AVEC LE BOUTON :
#----------SOURCE---------



# Fonction d'importation de la zone d'étude ----
get_import_zone <- function(){
  # Code utilisé pour importer un shape----
  #shp_path <- file.choose()  # Ouvrir les fichiers locaux du PC
  #shp_etude <<- st_read(shp_path)  # Importer le shapefile sélectionné ds l'env
  # Convertir l'objet en sf si nécessaire
  # "finished" contient la géométrie dessinée
  #shp_etude <<- st_as_sf(drawn_zone$finished)
  
  
  # Code pour dessiner une features avec mapedit----
  # Créer une carte leaflet centrée sur la France
  # Longitude, Latitude approximative de la France
  france_center <- c(2.2137, 46.2276)  
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = france_center[1], lat = france_center[2], zoom = 6)
  
  # Utiliser cette carte avec drawFeatures pour dessiner la zone d'étude
  drawn_zone <- drawFeatures(map = map)
  
  # Vérifier si quelque chose a été dessiné
  if (is.null(drawn_zone)) {
    stop("Aucune zone n'a été dessinée. Veuillez dessiner une zone avant de continuer.")
  }
  
  # Convertir l'objet dessiné en sf si nécessaire
  shp_etude <- st_as_sf(drawn_zone)
  
  # Vérifier si la conversion a bien fonctionné
  if (is.null(shp_etude)) {
    stop("Erreur lors de la conversion en objet sf.")
  }
  # Assurez-vous que les coordonnées sont correctement transformées
  # Les données placette IFN étant en lambert 93 (2154)
  shp_etude <- st_transform(shp_etude, 2154)
  
  # Sauvegarder la zone d'étude dans l'environnement
  shp_etude <<- shp_etude
  
  
  return (shp_etude)  # return les géométrie chargé
}

# Fonction obtention buffer et placette à l'intérieur ----
get_buffer_zone <- function(buffer = 0){
  
  # Création de la zone tampon autour du shapefile
  zone_tampon <<- st_buffer(shp_etude, dist = buffer)
  
  # Selection des placettes uniquement dans la zone tampon
  placette_tampon <<- st_intersection(placette, zone_tampon)
  
  # Obtenir les limites combinées de toutes les couches pour ajuster le zoom
  all_bounds <<- st_bbox(st_union(st_geometry(shp_etude),
                                 st_geometry(zone_tampon),
                                 st_geometry(placette_tampon)))
  
  # Extraire les IDP des placettes dans la zone tampon
  idp_placette_tampon <- placette_tampon$IDP
  
  # Filtre les arbres ayant le même IDP que les placettes de la zone
  arbre_zone_etude <<- arbre[arbre$IDP %in% idp_placette_tampon, ]
  
  
  # Convertir l'objet dessiné en sf si nécessaire
  shp_etude <- st_as_sf(shp_etude)
  
  # Vérifier si la conversion a bien fonctionné
  if (is.null(shp_etude)) {
    stop("Erreur lors de la conversion en objet sf.")
  }
  # Assurez-vous que les coordonnées sont correctement transformées
  # Les données placette IFN étant en lambert 93 (2154)
  shp_etude <<- st_transform(shp_etude, 2154)
  
  return(shp_etude)
}

# Fonction d'affichage des cartes avec placettes----
get_read_map <- function(){
  # Passer en mode interactif avec tmap
  tmap_mode("view")
  
  # Choisir le CRS de la zone d'étude
  common_crs <- st_crs(shp_etude)  
  zone_tampon <- st_transform(zone_tampon, common_crs)
  placette_tampon <- st_transform(placette_tampon, common_crs)
  
  
  # Calculer le nombre de placettes et d'arbres dans la zone tampon
  nombre_placettes <- nrow(placette_tampon)  # Nombre de placettes dans la zone tampon
  nombre_arbres <- nrow(arbre_zone_etude)  # Nombre d'arbres dans la zone tampon
  
  titre <- paste("Placettes dans la zone",
                 "\nNombre de placettes :", nombre_placettes,
                 "\nNombre d'arbres :", nombre_arbres)
  
  # Afficher la carte avec tmap et ajuster le zoom sur toutes les couches
  plot_zone <<- tm_shape(shp_etude, bbox = all_bounds) +  # Zone d'étude originale avec ajustement des limites
    tm_fill(col = "lightgreen", alpha = 0.3) +  # Remplissage vert avec transparence
    tm_borders(col = "black") +  # Contour noir
    tm_shape(zone_tampon) +  # Zone tampon
    tm_borders(col = "blue", lty = "dashed") +  # Ligne bleue en pointillé
    tm_shape(placette_tampon) +  # Placettes dans la zone tampon
    tm_symbols(col = "red", size = 0.005) +  # Placettes en rouge
    tm_layout(main.title = titre,
              main.title.size = 1,
              frame = TRUE)  # Ajouter un titre, sans cadre
  return(plot_zone)

}



# Fonction d'arrangement des données par placette ----
get_arrange_data <- function(){
  
  # Fabrication d'un numéro unique pour chaque arbre
  # Nomencalture num unique "num placette.num arbre"
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
  arbre_zone_etude_cor <<- arbre_zone_etude %>%
    group_by(num_unique) %>%          # Grouper par le numéro unique
    fill(ESPAR, .direction = "downup") %>%  # Remplir les valeurs manquantes par les valeurs non manquantes
    ungroup()  # Désactiver le regroupement
  
  return (arbre_zone_etude_cor)
}

# Fonction de remplissage des libellés d'essence dans la table de données----
get_species <- function(){
  # Ajout de la colonne Essence
  arbre_zone_etude$Essence <- NA
  
  # Boucle de remplissage des libellés pour chaque code Essence
  for (i in 1:nrow(arbre_zone_etude_cor)) {
    # Récupérer le code ESPAR pour l'arbre à la ligne `i`
    code_espar <- arbre_zone_etude_cor$ESPAR[i]
    code_veget5 <- arbre_zone_etude_cor$VEGET[i]
    
    # Si le code ESPAR est NA, remplacer par le libelle de code_veget
    if (is.na(code_espar)) {
      # Ajouter l'essence correspondante depuis code_veget
      veget_correspondante <- code_veget$libelle[code_veget$code == code_veget5]
      arbre_zone_etude_cor$Essence[i] <- veget_correspondante
    } else {
      # Trouver l'essence correspondante dans la table `metadonnees`
      essence_correspondante <- code_essence$libelle[code_essence$code == code_espar]
      
      # Vérifier si une essence a été trouvée
      if (length(essence_correspondante) > 0) {
        # Ajouter l'essence à la colonne "Essence" de `arbre_zone_etude_cor`
        arbre_zone_etude_cor$Essence[i] <- essence_correspondante
      } else {
        # Si aucun code ESPAR correspondant n'est trouvé, laisser la valeur par défaut (NA)
        arbre_zone_etude_cor$Essence[i] <- "NA"
      }
    }
  }
  arbre_zone_etude_cor <<- arbre_zone_etude_cor
  return(arbre_zone_etude_cor)
}


# Fonction option des principales variables dendrométrique des placettes----
get_data_dendro <- function(){
  
  # On sélectionne uniquement quelques colonnes utile
  arbre_zone_etude_cor <- arbre_zone_etude_cor %>%
    select(IDP,CAMPAGNE, num_unique, Essence, C13,C0, VEGET, HTOT, HDEC, V, W, IR5, IR1, annee_mesure, circ_mesure) %>%
    
    pivot_wider(names_from = CAMPAGNE,   # Créer des colonnes pour chaque année
                values_from = C13) %>%  # Les valeurs à placer dans ces colonnes sont les circonférences mesurées
    
    group_by(num_unique) %>%
    summarise(across(everything(), ~ first(na.omit(.)), .names = "{col}"), .groups = "drop")
  
  # On cherche la circonférence max dans la liste
  arbre_zone_etude_cor <<- arbre_zone_etude_cor %>%
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
    )
  return(arbre_zone_etude_cor)
  
  
}


# Fonction calcul accroissement en G/ha/an ----
get_calc_G <- function(){
  arbre_zone_etude_cor <<- arbre_zone_etude_cor %>%
    group_by(num_unique) %>%
    # Calculer l'accroissement annuel en G
    mutate(
      acc_g_ha = if_else(
        circonference_max != circonference_min & annee_max != annee_min,
        ((g_max - g_min) * w / (annee_max - annee_min)),
        NA_real_  # Sinon NA
      ))
  
}

# Fonction calcul accroissement en V/m3/an----
get_calc_V <- function(){
  arbre_zone_etude_cor <<- arbre_zone_etude_cor %>%
    group_by(num_unique) %>%
    # Calculer l'accroissement annuel en V
    mutate(
      HTOT = as.numeric(HTOT),
      acc_V_ha = if_else(
        circonference_max != circonference_min & annee_max != annee_min,
        (((0.65 * g_max * HTOT) - (0.65 * g_min * HTOT))*w )/ (annee_max - annee_min),
        NA_real_  # Sinon NA
      ))
  
}

# Fonction de lecture (affichage tableau) des valeurs d'accroissements ----
get_read_acc_G <- function(){
  
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
    mutate(accroisselent_G_ha_an = round(moyenne_accroissement_sans_diam, 3))  # Arrondir à 0.001 près
  
  # Fusionner les résultats avec ou sans catégorie de diamètre
  table_recap_final_G <<- table_recap_global %>%
    left_join(table_recap_global_sans_diam, by = "Essence")  # Ajouter la moyenne sans catégorie de diamètre
  
  View(table_recap_final_G)
  return(table_recap_final_G)
  
}
get_read_acc_V <- function(){
  
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
      moy_acc_V_m3_ha = mean(moyenne_accroissement_sans_diam, na.rm = TRUE),  # Moyenne globale sur toutes les placettes
      .groups = 'drop'
    ) %>%
    mutate(moy_acc_V_m3_ha = round(moy_acc_V_m3_ha, 3))  # Arrondir à 0.001 près
  
  # Fusionner les résultats avec ou sans catégorie de diamètre
  table_recap_final_V <<- table_recap_global %>%
    left_join(table_recap_global_sans_diam, by = "Essence")  # Ajouter la moyenne sans catégorie de diamètre
  
  View(table_recap_final_V)
  
  return(table_recap_final_V)
  
}

# Fonction intégral calcul de l'accroissement en volume sur les placettes----
get_acc_V <- function(buffer = 0){
  get_import_zone()
  get_buffer_zone(buffer)
  get_arrange_data()
  get_species()
  get_data_dendro()
  get_calc_V()
  get_read_acc_V()
  
  return(table_recap_final_V)
}

# Fonction intégral calcul de l'accroissement en G/ha/an sur les placettes
get_acc_G <- function(buffer = 0){
  get_import_zone()
  get_buffer_zone(buffer)
  get_arrange_data()
  get_species()
  get_data_dendro()
  get_calc_G()
  get_read_acc_G()
    
  return(table_recap_final_G)
}


# Obtenir les placettes et arbre mesurer d'une sylvoecoregion ----
get_sylvo_eco <- function(sylvoecoregion) {
  # Charger les données ser
  data("ser")           # Charger l'objet 'ser'
  
  # Filtrer le polygone correspondant au nom donné
  shp_etude <<- ser %>% filter(ser$NomSER == sylvoecoregion)
  
  get_buffer_zone()
  get_read_map()
  
  return(plot_zone)
}

# Obtenir les placettes et arbre mesurer d'une région forestière ----
get_reg_foret <- function(reg_foret) {
  # Charger les données ser et des placettes
  data("rfn")           # Charger l'objet 'rfn'
  
  # Filtrer le polygone correspondant au nom donné
  shp_etude <<- rfn %>% filter(rfn$REGIONN == reg_foret)
  get_buffer_zone()
  get_read_map()
  
  return(plot_zone)
}

# Obtenir l'accroissement en G/m²/ha/an d'une sylvoecoregion----
get_acc_G_sylvo_eco <- function(sylvoecoregion){
  get_sylvo_eco(sylvoecoregion)
  get_arrange_data()
  get_species()
  get_data_dendro()
  get_calc_G()
  get_read_acc_G()
  View(table_recap_final_G)
  
  return(plot_zone)
  
}

# Obtenir l'accroissement en G/m²/ha/an d'une Région forestière----
get_acc_G_reg_foret <- function(reg_foret){
  get_reg_foret(reg_foret)
  get_arrange_data()
  get_species()
  get_data_dendro()
  get_calc_G()
  get_read_acc_G()
  View(table_recap_final_G)
  
  return(plot_zone)
  
}

# Obtenir l'accroissement en V/m3/ha/an d'une sylvoecoregion----
get_acc_V_sylvo_eco <- function(sylvoecoregion){
  get_sylvo_eco(sylvoecoregion)
  get_arrange_data()
  get_species()
  get_data_dendro()
  get_calc_V()
  get_read_acc_V()
  View(table_recap_final_V)
  
  return(plot_zone)
  
}

# Obtenir l'accroissement en V/m3/ha/an d'une région forestière----
get_acc_V_reg_foret <- function(reg_foret){
  get_reg_foret(reg_foret)
  get_arrange_data()
  get_species()
  get_data_dendro()
  get_calc_V()
  get_read_acc_V()
  View(table_recap_final_V)
  
  return(plot_zone)
  
}


