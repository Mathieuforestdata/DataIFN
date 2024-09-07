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
install.packages("kableExtra")
install.packages("mapview")
install.packages("mapedit")

# Installation du dossier de travail ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir <- getwd()
check_happifndata()


# Installation des extensions Github ----
devtools::install_github("paul-carteron/happifn")

# Installation des librairies ----
library(happifn)
library(happifndata)
library(FrenchNFIfindeR)
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tmap);ttm
library(kableExtra)
library(mapview)
library(mapedit)

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



# Calcul de l'accroissement en G (m/ha/an)
get_acc_G(1000)


# Modif roman 06/09, fonction sylvoécorégion ----

get_placette_sylvo_eco("Ardenne primaire")

qtm(placettes_sylvo_eco)
  

  
get_arbre_sylvo_eco<- function(sylvoecoregion){
  placette_etude <- get_placette_sylvo_eco(sylvoecoregion)
  for (i in 1:nrow(placette_etude)){
    for (j in 1:nrow(arbre)){
      if (arbre[j, "IDP"] == placette_etude[i, "IDP"]) {
        arbre_etudie <- add_row(arbre_etudie, arbre[j, ])
      }
        
    }
  }
  return(arbre_etudie)
}

get_arbre_sylvo_eco("Ardenne primaire") 
  
qtm(test)
