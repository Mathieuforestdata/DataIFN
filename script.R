# Descriptif du projet R ----
# Projet extraction données IFN
# Auteurs : PENET Mathieu / PERIER Benoit / DELAGES Jean-Eudes / GARDES Roman
# Etudiant AgroParisTech

# Instalations des packages ----

install.packages("remotes")  # Permets d'importer les données Github nécessaire
install.packages("devtools")


# Installation des extensions Github ----

remotes::install_github("Jeremy-borderieux/FrenchNFIfindeR")
devtools::install_github("paul-carteron/happifn")

# Installation des librairies ----
library(happifn)
