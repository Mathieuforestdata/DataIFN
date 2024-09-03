# Descriptif du projet R ----
# Projet extraction données IFN
# Auteurs : PENET Mathieu / PERIER Benoit / DELAGES Jean-Eudes / GARDES Roman
# Etudiant AgroParisTech

# Instalations des packages ----

install.packages("remotes")  # Permets d'importer les données Github nécessaire
install.packages("devtools")
install.packages("data.table")
install.packages("sf")  # Package SIG
install.packages("ggplot2")

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

# Chargement de la fonction principal de récupération des données IFN ----
get_NFI()

# Importation de la zone buffer d'étude ----

shp_path <- file.choose()  # Ouvrir les fichiers locaux du PC

shp_etude <- st_read(shapefile_path)  # Importer le shapefile sélectionné

plot(st_geometry(shp_etude))  # Visualiser les géométries


# Corps du script ----
read.csv("./NFI_data/meta_data_nfi.csv")

# On lit le CSV de l'inventaire de toutes les placettes (position, dep, etc..)
placette_ifn <- read.csv("./NFI_data/Raw_data/PLACETTE.csv",
                         header = TRUE,
                         sep = ';')

# On charge le fichier csv des arbres inventoriés par l'IFN
arbre_ifn <- read.csv("./NFI_data/Raw_data/ARBRE.csv",
                         header = TRUE,
                         sep = ';')



# Affichage des premières lignes du fichier
head(placette_ifn)
head(arbre_ifn)




# Choix du département concernant le buffer 
depart_selec <- 54
placette_filtre <- subset(placette_ifn, DEP == depart_selec)

placette_sf <- st_as_sf(placette_filtre, coords = c("XL", "YL"), crs = 2154)

ggplot(data = placette_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Localisation des placettes")

# Selection des placettes IFN appartenant à la zone d'étude ----
st_crs(placette_sf)
st_crs(shp_etude)
placette_ifn_zone_etude <- st_intersection(placette_sf, shp_etude)

ggplot() +
  geom_sf(data = shp_etude, fill = "lightblue", color = "black") +  # Carte de la zone d'étude
  geom_sf(data = placette_ifn_zone_etude, color = "red") +  # Points des placettes
  theme_minimal() +
  labs(title = "Placettes dans la zone d'étude")
