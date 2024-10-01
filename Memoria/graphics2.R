library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(chilemapas)


# Cargar los datos de Chile
world <- ne_countries(scale = "medium", returnclass = "sf")
chile <- world[world$name == "Chile", ]
comunas_sf <- chilemapas::mapa_comunas


chile_vrs <- function(hospitales_df) {
  # Cargar el mapa de Chile
  chile_map <- map_data("world", region = "Chile")
  
  # Crear el mapa de Chile
  mapa_chile <- ggplot(data = chile) +
    geom_sf() +
    geom_point(data = hospitales_df, aes(x = longitud, y = latitud, color = vrs, size = vrs), alpha = 0.7) +
    scale_color_gradient(low = "red", high = "green") +  # Colores según valor
    labs(title = "Eficiencia técnica hospitales públicos Chilenos (VRS)", color = "Valor", size = "Valor") +
    theme_minimal() +
    labs(title = "Mapa de Chile")
  
  # Mostrar el mapa con ggplot2
  print(mapa_chile)
  
  
}

region_vrs <- function(hospitales_df, region) {
  
  # Filtro de hospitales para la región seleccionada
  hospitales_df_rm <- hospitales_df %>%
    filter(region_id == region) %>%
    filter(IdEstablecimiento != 112107)
  
  nombre_region <- hospitales_df_rm$Region[[1]]
  
  if (region == 13){
    nombre_region = "Región Metropolitana"
   }
  
  # Verificar si la región tiene hospitales después del filtro
  if (nrow(hospitales_df_rm) == 0) {
    stop("No hay hospitales en la región seleccionada.")
  }
  
  codigo <- ifelse(region < 10, paste0("0", as.character(region)), as.character(region))
  
  # Filtrar las comunas de la región seleccionada
  rm_comunas <- comunas_sf[comunas_sf$codigo_region == codigo, ]
  
  # Verificar si hay comunas en la región seleccionada
  if (nrow(rm_comunas) == 0) {
    stop("No hay comunas disponibles para la región seleccionada.")
  }
  
  
  # Crear el mapa de la región seleccionada
  mapa_rm <- ggplot(data = rm_comunas) +
    geom_sf(aes(geometry = geometry)) + 
    geom_point(data = hospitales_df_rm, 
               aes(x = longitud, y = latitud, 
                   color = vrs, 
                   size = vrs, 
                   text = paste("Hospital:", Nombre, "<br>VRS:", vrs)),
               alpha = 0.7) +
    scale_color_gradient(low = "red", high = "green") +  # Colores según valor
    labs(title = paste("Eficiencia técnica hospitales públicos en", nombre_region, "(VRS)"),
         color = "Valor", 
         size = "Valor") +
    theme_minimal()
  
  
  return(mapa_rm)
}




