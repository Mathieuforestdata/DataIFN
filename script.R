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

# Installation du dossier de travail ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir <- getwd()


# Installation des extensions Github ----

#remotes::install_github("Jeremy-borderieux/FrenchNFIfindeR")  # Nes pas installer
devtools::install_github("paul-carteron/happifn")

# Installation des librairies ----
library(happifn)
library(FrenchNFIfindeR)
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)

# Chargement de la fonction principal de récupération des données IFN ----
#get_ifn_all()
get_dataset_names()
arbre <- get_ifn("arbre")
placette <- get_ifn("placette")
metadata <-get_ifn_metadata()  # Chargement des metadonnee

# On charge indépendament toutes les listes de metadata
code <- metadata[[1]]
units <- metadata[[2]]
units_value_set <- metadata[[3]]


# Géolocalisation des placettes avec les données de latitudes et longitudes
#placette_sf <- st_as_sf(placette_ifn_total, coords = c("XL", "YL"), crs = 2154)


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
placette_ifn_zone_etude <- st_intersection(placette, shp_etude)

# Affichage des placettes et de la zone d'étude strict
ggplot() +
  geom_sf(data = shp_etude, 
          fill = "lightblue",  # Remplissage bleu
          color = "black") +  # Contour noir
  geom_sf(data = placette_ifn_zone_etude, 
          color = "red") +  # Placettes en rouge
  theme_minimal() +
  labs(title = "Placettes dans la zone d'étude")

# Parfois la zone tampon est trop petite
# Alors on ajoute un buffer au shp importé

# Définition de la zone tampon
largeur_tampon <- 1000  # Ajustez cette valeur selon vos besoins

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
arbre$num_unique <- paste(arbre$IDP, arbre$A, sep = ".")

arbre$Essence <- NA
arbre_zone_etude <- arbre[arbre$IDP %in% idp_placette_tampon, ]


arbre_zone_etude$ESPAR[arbre_zone_etude$ESPAR == "" | arbre_zone_etude$ESPAR == " "] <- NA

# Remplir les valeurs manquantes pour chaque groupe de numéros uniques
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
  select(CAMPAGNE, num_unique, Essence, C13,C0, HTOT, HDEC, V, W, IR5, IR1) %>%
  
  pivot_wider(names_from = CAMPAGNE,   # Créer des colonnes pour chaque année
              values_from = C13) %>%  # Les valeurs à placer dans ces colonnes sont les circonférences mesurées
  
  group_by(num_unique) %>%
  summarise(across(everything(), ~ first(na.omit(.)), .names = "{col}"), .groups = "drop")
  
# On cherche la circonférence max dans la liste
arbre_zone_etude_cor <- arbre_zone_etude_cor %>%
  # On vérifie que toutes les colonnes de date de mesure soit numérique
  mutate(across(starts_with("20"), as.numeric)) %>%
  rowwise() %>%
  # Dans toutes les colonnes date, on cherche la valeur max de circonférence
  mutate(circonference_max = max(c_across(starts_with("20")), na.rm = TRUE),
         circonference_min = min(c_across(starts_with("20")), na.rm = TRUE),
         acc_moy = circonference_max - circonference_min) %>%
  ungroup() %>%
  
  
  
  
  
  # On calcul le diamètre de la circonférence max
  mutate(diametre = circonference_max / pi) %>%  # Diamètre exacte
  mutate(diametre = round(diametre / 0.001) * 0.001*100) %>%
  mutate(class_diametre = round(diametre /5) * 5) %>% # On arrondi
  
  # On définis les catégories de diamètre
  mutate(cat_diam = case_when(
    diametre >= 7.5 & diametre < 22.5 ~ "PB",   # Petite Bois (PB)
    diametre >= 22.5 & diametre < 47.5 ~ "BM",  # Bois Moyen (BM)
    diametre >= 47.5 & diametre < 67.5 ~ "GB",  # Gros Bois (GB)
    diametre >= 67.5 ~ "TGB",                  # Très Gros Bois (TGB)
    TRUE ~ NA_character_                       # Gérer les cas manquants ou en dehors des intervalles
  ))









# Filtre BOIS MORT
bois_mort_zone_etude <- bois_mort_ifn[bois_mort_ifn$IDP %in% idp_placette_tampon, ]



check_happifndata() # PAckage stockage data
library(happifndata)


data(rfn)
tmap::qtm(rfn)


View(ser)

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
