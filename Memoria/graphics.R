library(ggplot2)
library(maps)

#chile_map <- map_data("world", region = "Chile")
#chile_map <- ne_states(country = "Chile", returnclass = "sf")

graficar_hospitales_vrs <- function(hospitales_df) {
  # Cargar el mapa de Chile
  chile_map <- map_data("world", region = "Chile")
  
  # Crear el gráfico
  ggplot() +
    geom_polygon(data = chile_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = hospitales_df, aes(x = longitud, y = latitud, color = vrs, size = vrs), alpha = 0.7) +
    scale_color_gradient(low = "red", high = "green") +  # Colores según valor
    labs(title = "Eficiencia técnica hospitales públicos Chilenos (VRS)", color = "Valor", size = "Valor") +
    coord_fixed(1.3) +  # Mantiene la proporción correcta del mapa
    theme_minimal()
}




graficar_hospitales_crs <- function(hospitales_df) {
  # Cargar el mapa de Chile
  chile_map <- map_data("world", region = "Chile")
  
  # Crear el gráfico
  ggplot() +
    geom_polygon(data = chile_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = hospitales_df, aes(x = longitud, y = latitud, color = crs, size = crs), alpha = 0.7) +
    scale_color_gradient(low = "red", high = "green") +  # Colores según valor
    labs(title = "Eficiencia técnica hospitales públicos Chilenos (VRS)", color = "Valor", size = "Valor") +
    coord_fixed(1.3) +  # Mantiene la proporción correcta del mapa
    theme_minimal()
}


graficar_hospitales_vrs_rm <- function(hospitales_df) {
  
  hospitales_df_rm <- hospitales_df %>%
    filter(region == 13)  %>%
    filter(IdEstablecimiento != 112107)
  # Cargar el mapa de Chile filtrando por la Región Metropolitana
  chile_map_rm <- chile_map %>%
    filter(long > -71.5 & long < -70.3 & lat > -34.2 & lat < -33.2)  # Coordenadas aproximadas
  
  # Crear el gráfico
  ggplot() +
    geom_polygon(data = chile_map_rm, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = hospitales_df_rm, aes(x = longitud, y = latitud, color = vrs, size = vrs), alpha = 0.7) +
    scale_color_gradient(low = "red", high = "green") +  # Colores según valor
    labs(title = "Mapa de la Región Metropolitana con Hospitales", color = "Valor", size = "Valor") +
    coord_fixed(1.3) +  # Mantiene la proporción correcta del mapa
    theme_minimal()
}




# Llamar a la función para graficar
#graficar_hospitales(hospitales)
