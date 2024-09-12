install.packages("ggplot2")
install.packages("maps")

library(ggplot2)
library(maps)

chile_map <- map_data("world", region = "Chile")

hospitales <- data.frame(
  nombre = c("Hospital 1", "Hospital 2", "Hospital 3"),
  latitud = c(-18.4827411739751, -32.417, -39.828),
  longitud = c( -70.31266928987736, -71.135, -73.245),
  valor = c(100, 200, 150)  # Valores que deseas representar
)


graficar_hospitales <- function(hospitales_df) {
  # Cargar el mapa de Chile
  chile_map <- map_data("world", region = "Chile")
  
  # Crear el gráfico
  ggplot() +
    geom_polygon(data = chile_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    geom_point(data = hospitales_df, aes(x = longitud, y = latitud, color = valor, size = valor), alpha = 0.7) +
    scale_color_gradient(low = "red", high = "green") +  # Colores según valor
    labs(title = "Mapa de Chile con Hospitales", color = "Valor", size = "Valor") +
    coord_fixed(1.3) +  # Mantiene la proporción correcta del mapa
    theme_minimal()
}


# Llamar a la función para graficar
#graficar_hospitales(hospitales)
