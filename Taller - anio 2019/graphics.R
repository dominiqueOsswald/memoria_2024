#install.packages("ggplot2")
#install.packages("maps")

library(ggplot2)
library(maps)

chile_map <- map_data("world", region = "Chile")

#hospitales <- data.frame(
#  nombre = c("Hospital 1", "Hospital 2", "Hospital 3"),
#  latitud = c(-18.4827411739751, -20.21392734846593,-23.659761892406777),
#  longitud = c( -70.31266928987736, -70.13837254566423,-70.39590564743622),
#  valor = c(0.5, 1, 0.75)  # Valores que deseas representar
#)


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
