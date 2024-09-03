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

# Installation du dossier de travail ----
# Attention, dossier de travail individuel à chaque personne !!!

setwd("C:/Users/mathi/OneDrive/Documents/AgroParisTech/Cours 3A/Rstudio/DataIFN")


# Installation des extensions Github ----

remotes::install_github("Jeremy-borderieux/FrenchNFIfindeR")
devtools::install_github("paul-carteron/happifn")

# Installation des librairies ----
library(happifn)
library(FrenchNFIfindeR)
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)

# Chargement de la fonction principal de récupération des données IFN ----
get_NFI()

# Importation de la zone buffer d'étude ----

shp_path <- file.choose()  # Ouvrir les fichiers locaux du PC

shp_etude <- st_read(shapefile_path)  # Importer le shapefile sélectionné

plot(st_geometry(shp_etude))  # Visualiser les géométries


# Chargement des fichiers CSV utilisés ----
placette_ifn <- read.csv("./NFI_data/Raw_data/PLACETTE.csv",
                         header = TRUE,
                         sep = ';')

arbre_ifn <- read.csv("./NFI_data/Raw_data/ARBRE.csv",
                      header = TRUE,
                      sep = ';')
metadonnee_ifn <- read.csv("./NFI_data/meta_data_nfi.csv",
                      header = TRUE,
                      sep = ';')

# Lecture du code essence des arbres IFN ----
# Chaque arbre détiens un code essence ("ESPAR")
essence_ifn <- metadonnee_ifn %>%  # Filtrer les lignes où la colonne `rawnales` est égale à "ESPAR"
  filter(row.names == "ESPAR")

# Corps du script ----

# Affichage des premières lignes des fichiers
head(placette_ifn)
head(arbre_ifn)


# Exportation des placettes IFN suivant le département ----
depart_selec <- 54  # Choix du numéro de département

# Sélection des placettes, avec application du filtre département
placette_filtre <- subset(placette_ifn, DEP == depart_selec)

# Géolocalisation des placettes avec les données de latitudes et longitudes
placette_sf <- st_as_sf(placette_filtre, coords = c("XL", "YL"), crs = 2154)

# Affichage des placettes de l'IFN sur le département selectionné
ggplot(data = placette_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Localisation des placettes")



# Selection des placettes IFN appartenant à la zone d'étude ----
st_crs(placette_sf)  # Test de la projection de la couche placette_sf
st_crs(shp_etude)

# Croisement des emplacements de placette avec le shapefile importé
placette_ifn_zone_etude <- st_intersection(placette_sf, shp_etude)

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
largeur_tampon <- 500  # Ajustez cette valeur selon vos besoins

# Création de la zone tampon autour du shapefile
zone_tampon <- st_buffer(shp_etude, dist = largeur_tampon)

placette_tampon <- st_intersection(placette_sf, zone_tampon)

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

# Extraire les IDP des placettes dans la zone tampon
idp_placette_tampon <- placette_tampon$IDP

# Filtrer les arbres pour ne garder que ceux des placettes dans le tampon
arbre_tampon <- arbre_ifn[arbre_ifn$IDP %in% idp_placette_tampon, ]

# Afficher les premières lignes des arbres filtrés
head(arbre_tampon)
















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

