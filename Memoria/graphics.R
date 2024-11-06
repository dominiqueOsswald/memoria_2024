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


chile_vrs <- function(hospitales_df, anio, tipo) {
  # Cargar el mapa de Chile
  chile_map <- map_data("world", region = "Chile")
  
  mapa_chile <- ggplot(data = chile) +
    geom_sf() +
    geom_point(data = hospitales_df, aes(x = longitud, y = latitud, color = vrs, size = (1/vrs) * 5,  text = paste("Hospital:", Nombre, "<br>VRS:", vrs, "<br>Region:", region_id)), alpha = 0.7) +
    scale_color_gradient(low = "red", high = "green", limits = c(0, 1)) +  # Rango de valores para los colores
    labs(title = paste(tipo," - Año ", anio), color = "Valor", size = "Valor") +
    # labs(title = paste("Eficiencia técnica hospitales públicos Chilenos (VRS) - Año ", anio), color = "Valor", size = "Valor") +
    theme_minimal()
  #labs(title = paste("Eficiencia técnica hospitales públicos en", nombre_region, "(VRS) - Año ", anio),
  # Mostrar el mapa con ggplot2
  #print(mapa_chile)
  return(mapa_chile)
  
}


chile_crs <- function(hospitales_df, anio, tipo) {
  # Cargar el mapa de Chile
  chile_map <- map_data("world", region = "Chile")
  
  mapa_chile <- ggplot(data = chile) +
    geom_sf() +
    geom_point(data = hospitales_df, aes(x = longitud, y = latitud, color = crs, size = (1/crs) * 5,  text = paste("Hospital:", Nombre, "<br>CRS:", crs, "<br>Region:", region_id)), alpha = 0.7) +
    scale_color_gradient(low = "red", high = "green", limits = c(0, 1)) +  # Rango de valores para los colores
    labs(title = paste(tipo," - Año ", anio), color = "Valor", size = "Valor") +
    # labs(title = paste("Eficiencia técnica hospitales públicos Chilenos (VRS) - Año ", anio), color = "Valor", size = "Valor") +
    theme_minimal()

  return(mapa_chile)
  
}

region_vrs <- function(hospitales_df, region, anio, tipo) {
  
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
    geom_point(data = hospitales_df_rm, aes(x = longitud, y = latitud, color = vrs, size = (1/vrs) * 5,  text = paste("Hospital:", Nombre, "<br>VRS:", vrs, "<br>Region:", region_id)), alpha = 0.6)  +
    
    scale_color_gradient(low = "red", high = "green", limits = c(0, 1)) +  # Rango de valores para los colores
    labs(title = paste("Eficiencia técnica hospitales públicos en", nombre_region, "(",tipo,") - Año ", anio),
         color = "Valor", 
         size = "Valor") +
    theme_minimal()
  
  return(mapa_rm)
}

# Función para generar gráficos para cada tipo de resultado de un año específico
generar_graficos_por_anio <- function(anio, tipo) {
  grafico1 <- chile_vrs(resultados_in[[anio]]$data, anio, tipo)
  grafico2 <- chile_vrs(resultados_in_2_vrs[[anio]]$data, anio, tipo)
  grafico3 <- chile_vrs(resultados_in_3_vrs[[anio]]$data, anio, tipo)
  
  # Devolver una lista con los tres gráficos
  list(grafico1, grafico2, grafico3)
}


# Función para generar gráficos para cada tipo de resultado de un año específico
generar_graficos_por_anio_crs <- function(anio, tipo) {
  grafico1 <- chile_crs(resultados_in[[anio]]$data, anio, tipo)
  grafico2 <- chile_crs(resultados_in_2_vrs[[anio]]$data, anio, tipo)
  grafico3 <- chile_crs(resultados_in_3_vrs[[anio]]$data, anio, tipo)
  
  # Devolver una lista con los tres gráficos
  list(grafico1, grafico2, grafico3)
}


# Función general para graficar
chile_map_plot <- function(hospitales_df, anio, tipo, tipo_columna) {
  ggplot(data = chile) +
    geom_sf() +
    geom_point(
      data = hospitales_df,
      aes_string(
        x = "longitud",
        y = "latitud",
        color = tipo_columna,
        size = paste("(1/", tipo_columna, ") * 5"),
        text = paste0("paste('Hospital:', Nombre, '<br>", tipo, ":', ", tipo_columna, ", '<br>Region:', region_id)")
      ),
      alpha = 0.7
    ) +
    scale_color_gradient(low = "red", high = "green", limits = c(0, 1)) +
    labs(
      title = paste(tipo, "- Año", anio),
      color = "Valor",
      size = "Valor"
    ) +
    theme_minimal()
}


# Función para generar gráficos por iteración
generar_graficos_iteracion <- function(resultados, tipo, tipo_columna) {
  lapply(names(resultados), function(anio) {
    chile_map_plot(resultados[[anio]]$data, anio, tipo, tipo_columna)
  })
}






