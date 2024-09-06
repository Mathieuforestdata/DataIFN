# Descriptif du projet R ----
# Projet extraction données IFN
# Nom du projet : DataIFN
# Auteurs : PENET Mathieu / PERIER Benoit / DELAGES Jean-Eudes / GARDES Roman
# Etudiant AgroParisTech

# Instalations des packages ----

install.packages("remotes")  # Permets d'importer les données Github nécessaire
install.packages("devtools")
install.packages("data.table")
install.packages("sf")  # Package SIG
install.packages("ggplot2")
install.packages("dplyr")
install.packages("happifn")
install.packages("tidyr")
install.packages("tmap")

# Installation du dossier de travail ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir <- getwd()


# Installation des extensions Github ----
devtools::install_github("paul-carteron/happifn")

# Installation des librairies ----
library(happifn)
library(FrenchNFIfindeR)
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tmap);ttm

# Chargement des fonction de récupération des données IFN ----
#get_ifn_all()
get_dataset_names()  # Permet d'obtenir les noms des fichiers fournis par IFN
arbre <- get_ifn("arbre")
placette <- get_ifn("placette")
metadata <-get_ifn_metadata()  # Chargement des metadonnee

# On charge indépendament toutes les listes de metadata
code <- metadata[[1]]
units <- metadata[[2]]
units_value_set <- metadata[[3]]

# On identifie les codes essences
code_essence <- units_value_set %>% 
  filter(units == "ESPAR")



# Importation de la zone buffer d'étude ----

shp_path <- file.choose()  # Ouvrir les fichiers locaux du PC

shp_etude <- st_read(shp_path)  # Importer le shapefile sélectionné

#shp_etude <- mapedit::drawFeatures()  # Permet de dessiner une features manuellement
plot(st_geometry(shp_etude))  # Visualiser les géométries



# Selection des placettes IFN appartenant à la zone d'étude ----
st_crs(placette)  # Test de la projection de la couche placette_sf
st_crs(shp_etude)  # Test projetction zone d'étude

# Croisement des emplacements de placette avec le shapefile importé
#placette_ifn_zone_etude <- st_intersection(placette, shp_etude)

# Parfois la zone tampon est trop petite
# Alors on ajoute un buffer au shp importé

# Définition de la zone tampon
largeur_tampon <- 50000  # Ajustez cette valeur selon vos besoins

# Création de la zone tampon autour du shapefile
zone_tampon <- st_buffer(shp_etude, dist = largeur_tampon)

placette_tampon <- st_intersection(placette, zone_tampon)

# Affichage de la zone, avec le tampon et  les placettes de l'IFN
ggplot() +
  geom_sf(data = shp_etude,  # Zone d'étude original 
          fill = "lightgreen",  # Remplissage vert 
          color = "black") +  # Contour noir
  geom_sf(data = zone_tampon, 
          fill = "transparent",  # Remplissage transparent 
          color = "blue",  # Ligne en bleu
          linetype = "dashed") +  # Pointillé
  geom_sf(data = placette_tampon, 
          color = "red") +  # Placettes en rouge
  theme_minimal() +
  labs(title = "Placettes dans la zone tampon autour de la zone d'étude")

# Extraction de toutes les données IFN sur la zone d'étude ----
# Extraire les IDP des placettes dans la zone tampon
idp_placette_tampon <- placette_tampon$IDP

# Filtre ARBRE
# Construction numéro unique arbre

arbre_zone_etude <- arbre[arbre$IDP %in% idp_placette_tampon, ]
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
  select(CAMPAGNE, num_unique, Essence, C13,C0, HTOT, HDEC, V, W, IR5, IR1, annee_mesure, circ_mesure) %>%
  
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
    acc_g_ha = round((((circonference_max^2) / (4 * pi)) * w ) / (annee_max - annee_min),2)
  )


# Calculer les moyennes d'accroissement par essence et catégorie de diamètre
# Puis les affciher dans un tableau récapitulatif
table_recap <- arbre_zone_etude_cor %>%
  filter(is.finite(acc_g_ha)) %>%  # Exclure les valeurs infinies
  group_by(Essence, cat_diam) %>%
  summarise(moyenne_accroissement = mean(acc_g_ha, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = cat_diam, values_from = moyenne_accroissement, values_fill = list(moyenne_accroissement = NA)) %>%
  arrange(Essence)

# Afficher le tableau récapitulatif
print(table_recap)












# Modif roman 06/09, fonction sylvoécorégion ----
  check_happifndata()
  library(happifndata)  
  placette <- get_ifn("placette")  # Charger l'objet 'placette' à partir d'un fichier ou d'une source
  data("ser")
  
  get_placette <- function(sylvoecoregion) {
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
    placettes_dans_polygone <- placette[lengths(within_list) > 0, ]
    
    return(placettes_dans_polygone)
  }
  
  test <- get_placette(sylvoecoregion = "Ardenne primaire")
  View(test)
  #fontion pour avoir tout les arbres d'une sylvoecoregion
  
  get_arbre_parcelle<- function(sylvoecoregion){
    placette_etude <- get_placette(sylvoecoregion)
    for (i in 1:nrow(placette_etude)){
      for (j in 1:nrow(arbre)){
        if (arbre[j, "IDP"] == placette_etude[i, "IDP"]) {
          arbre_etudie <- add_row(arbre_etudie, arbre[j, ])
        }
        
      }
    }
    return(arbre_etudie)
  }
  
  
  test <- get_arbre_parcelle(sylvoecoregion = "Ardenne primaire")
  
  print(test)
  
  get_placette("Ardenne primaire")
  
qtm(test)

# Descriptif de la donnée brut ARBRE ----

# Campagne = Date de mesure
# IDP = Identifiant de la placette de mesure
# A = Numéro individuel de l'arbre sur la placette
# ESPAR = L'essence
# C13 = Circonférence à 1m30
# HTOT = Hauteur total de l'arbre
# V = Volume de l'arbre (découpe fin bout 7cm)
# W = Poids statistique de l'arbre à l'hectare
# IR5 = Accroissement radial sur 5ans
# IR1 = Accroissement radial sur 1ans


# catégorie de grosseur des bois
# PB [7,5 - 22,5[
# BM [22,5 - 47,5[
# GB [47,5 - 67,5[
# TGB [67,5 [
