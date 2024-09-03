# Descriptif du projet R ----
# Projet extraction données IFN
# Auteurs : PENET Mathieu / PERIER Benoit / DELAGES Jean-Eudes / GARDES Roman
# Etudiant AgroParisTech

# Instalations des packages ----

install.packages("remotes")  # Permets d'importer les données Github nécessaire
install.packages("devtools")
install.packages("data.table")
install.packages("sf")  # Package SIG

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

# Chargement de la fonction principal de récupération des données IFN ----
get_NFI()

# Importation de la zone buffer d'étude ----

shapefile_path <- file.choose()  # Ouvrir les fichiers locaux du PC

shapefile_data <- st_read(shapefile_path)  # Importer le shapefile sélectionné

plot(st_geometry(shapefile_data))  # Visualiser les géométries


# Corps du script ----
read.csv("./NFI_data/meta_data_nfi.csv")

placette_ifn <- read.csv("./NFI_data/Raw_data/PLACETTE.csv", header = TRUE)

depart_selec <- 52
placette_filtre <- subset(placette_ifn, DEP == depart_selec)

summary(NFI_dendro)


