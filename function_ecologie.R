# Fichier des fonctions lié à l'écologie des placettes ----

# Fonction obtention des placettes ecologie dans la zone ----
get_ecologie_zone <- function(buffer = 1500){
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
  ecologie_zone_etude <<- ecologie[ecologie$IDP %in% idp_placette_tampon, ]
  
  return(plot_zone)
}
