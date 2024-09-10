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
install.packages("leaflet")

# Installation du dossier de travail ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir <- getwd()



# Installation des extensions Github ----
devtools::install_github("paul-carteron/happifn")

# Installation des librairies ----
library(happifn)
library(happifndata)
check_happifndata()
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
library(leaflet)


# Chargement des fonction de récupération des données IFN ----
#get_ifn_all()
get_dataset_names()  # Permet d'obtenir les noms des fichiers fournis par IFN
arbre <- get_ifn("arbre")
placette <- get_ifn("placette")
habitat <- get_ifn("habitat")
#ecologie <- get_ifn("ecologie")
metadata <-get_ifn_metadata()  # Chargement des metadonnee

# On charge indépendament toutes les listes de metadata
code <- metadata[[1]]
units <- metadata[[2]]
units_value_set <- metadata[[3]]

# On identifie les codes essences
code_essence <- units_value_set %>% 
  filter(units == "ESPAR")
code_veget <- units_value_set %>% 
  filter(units == "VEGET5")

#code_ecologie <- units_value_set %>%
  filter(units %in% c("TOPO", "OBSTOPO", "HUMUS", "OLT", "TSOL", "TEXT1", "TEXT2", "ROCHED0"))


get_pplmt() 
  
  
# Activation des fonctions----

get_acc_G()

get_acc_V()

get_acc_D()

mel <- arbre_zone_etude_cor[arbre_zone_etude_cor$Essence=="Mélèze d'Europe",]

get_acc_G_reg_foret("PLAINE CORSE ORIENTALE")

get_acc_G_sylvo_eco("Ardenne primaire")

get_calc_taux_acc_G()



# Script jean eudes

View(arbre_zone_etude_cor)
arbre_zone_etude_cor <- arbre_zone_etude_cor[!is.na(arbre_zone_etude_cor$diam) & !is.na(arbre_zone_etude_cor$V), ]

# Assurez-vous que les colonnes 'diam' et 'V' sont numériques
arbre_zone_etude_cor$diam <- as.numeric(arbre_zone_etude_cor$diam)
arbre_zone_etude_cor$V <- as.numeric(arbre_zone_etude_cor$V)

# Ajustement d'un modèle polynomiale (ordre 2) sur les données
modele_poly <- lm(V ~ poly(diam, 2), data = arbre_zone_etude_cor)

# Lecture du fichier Excel contenant les tarifs Shaeffer et Algan
tarif_lent <- read.csv("./tarif_shaeffer_lent.csv",
                       header = TRUE,
                       sep = ';')
tarif_rapide <- read.csv("./tarif_shaeffer_lent.csv",
                         header = TRUE,
                         sep = ';')

# Assurez-vous que les colonnes 'Diametre' et les volumes dans chaque feuille sont numériques
tarif_lent$Diametre <- as.numeric(tarif_lent$Diametre)
tarif_rapide$Diametre <- as.numeric(tarif_rapide$Diametre)

tarif_lent[ , 2:ncol(tarif_lent)] <- lapply(tarif_lent[ , 2:ncol(tarif_lent)], as.numeric)
tarif_rapide[ , 2:ncol(tarif_rapide)] <- lapply(tarif_rapide[ , 2:ncol(tarif_rapide)], as.numeric)

# Fonction pour calculer l'erreur quadratique moyenne (MSE)
calculate_mse <- function(vol_predicted, vol_actual) {
  if (any(is.na(vol_predicted)) || any(is.na(vol_actual))) {
    return(NA)  # Retourner NA si des valeurs manquent
  }
  mean((vol_predicted - vol_actual)^2)
}

# Fonction pour comparer le modèle polynomiale avec un tarif donné
compare_with_tarif <- function(tarif_data, tarif_type) {
  diameters <- tarif_data$Diametre
  mse_values <- c()
  
  for (i in 2:ncol(tarif_data)) {  # Les colonnes 2 à n sont les volumes
    volumes_tarif <- tarif_data[[i]]
    
    # Interpolation pour correspondre aux diamètres de l'étude
    volumes_interp <- approx(diameters, volumes_tarif, arbre_zone_etude_cor$diam, rule = 2)$y
    
    # Vérifier si les volumes interpolés contiennent des NA
    if (all(is.na(volumes_interp))) {
      next  # Si tous les volumes interpolés sont NA, ignorer cette courbe
    }
    
    # Calcul de la MSE entre le modèle polynomiale et le tarif
    mse <- calculate_mse(arbre_zone_etude_cor$V, volumes_interp)
    mse_values <- c(mse_values, mse)
  }
  
  # Retourner la MSE la plus faible, l'index du tarif correspondant, et le type de tarif
  if (length(mse_values) == 0 || all(is.na(mse_values))) {
    return(list(min_mse = Inf, best_tarif = NA, type = tarif_type))
  }
  
  min_mse <- min(mse_values, na.rm = TRUE)
  best_tarif <- which.min(mse_values) + 1  # +1 à cause de la colonne 'Diametre'
  return(list(min_mse = min_mse, best_tarif = best_tarif, type = tarif_type))
}

# Comparer avec les tarifs Shaeffer lent, rapide et Algan
best_tarif_lent <- compare_with_tarif(tarif_lent, "lent")
best_tarif_rapide <- compare_with_tarif(tarif_rapide, "rapide")

# Comparer les trois MSE pour déterminer le meilleur tarif
best_tarif <- best_tarif_lent

# Vérifier les valeurs manquantes avant de comparer
if (!is.na(best_tarif_rapide$min_mse) && best_tarif_rapide$min_mse < best_tarif$min_mse) {
  best_tarif <- best_tarif_rapide
}

print(paste("Le meilleur tarif est :", best_tarif$best_tarif, 
            "de type", best_tarif$type, 
            "avec une MSE de", best_tarif$min_mse))

# Visualisation de la courbe
plot(arbre_zone_etude_cor$diam, arbre_zone_etude_cor$V, 
     xlab = "Diamètre (cm)", 
     ylab = "Volume (m³)", 
     main = "Volume en fonction du diamètre", 
     pch = 16, col = "blue")

# Ajout de la courbe polynomiale
lines(sort(arbre_zone_etude_cor$diam), 
      predict(modele_poly, newdata = data.frame(diam = sort(arbre_zone_etude_cor$diam))), 
      col = "green", lwd = 2)

# Ajouter la courbe du tarif Shaeffer choisi
tarif_data <- if (best_tarif$type == "lent") tarif_lent else tarif_rapide
volumes_tarif <- tarif_data[[best_tarif$best_tarif]]
volumes_interp <- approx(tarif_data$Diametre, volumes_tarif, arbre_zone_etude_cor$diam, rule = 2)$y

lines(sort(arbre_zone_etude_cor$diam), 
      volumes_interp[order(arbre_zone_etude_cor$diam)], 
      col = "red", lwd = 2, lty = 2)
}



