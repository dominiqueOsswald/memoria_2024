library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(chilemapas)
library(gridExtra)



# Cargar los datos de Chile
world <- ne_countries(scale = "medium", returnclass = "sf")
chile <- world[world$name == "Chile", ]
comunas_sf <- chilemapas::mapa_comunas



# -------------------------------------- #
# Graficar region de Chile según el criterio de orientacion y variacion
# -------------------------------------- #
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



# -------------------------------------- #
# Graficar Chile según el criterio de orientacion y variacion
# -------------------------------------- #
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



# -------------------------------------- #
# Colorear regiones según porcentaje
# -------------------------------------- #
colorear_region <- function(resumen){
  chile_comunas <- chilemapas::mapa_comunas
  
  # Convertir el mapa a nivel de regiones, agrupando las comunas por región
  chile_regiones <- chile_comunas %>%
    group_by(codigo_region) %>%
    summarize(geometry = st_union(geometry), .groups = "drop") %>%
    st_as_sf()
  
  # Asegurarse de que la columna IDRegion en los datos de porcentaje sea de tipo character
  resumen$Porcentajes <- resumen$Porcentajes %>%
    mutate(region_id = as.character(region_id))
  
  # Unir el dataframe de porcentajes con el mapa de Chile usando el IDRegion
  chile_porcentaje <- chile_regiones %>%
    left_join(resumen$Porcentajes, by = c("codigo_region" = "region_id"))
  
  chile_porcentaje <- chile_porcentaje %>%
    #filter(!is.na(Region)) %>%
    mutate(across(starts_with("20"), as.numeric))  # Cambia "20" si tus columnas de año tienen otro prefijo
  
  
  # Generar un gráfico para cada año
  for (year in names(chile_porcentaje)[4:ncol(chile_porcentaje)]) { # Ajusta el índice según las columnas de año en tu data
    
    g <- ggplot(data = chile_porcentaje) +
      geom_sf(aes(fill = !!sym(year)), color = "black") +
      scale_fill_gradient(low = "#b2d8b2", high = "green", na.value = "white") +
      labs(title = paste("Mapa de Chile - Año", year), fill = "Porcentaje") +
      theme_minimal() 
    #+  ggsave(paste0("mapa_chile_", year, ".png"))
    print(g)
  }
  
}



# -------------------------------------- #
# Colorear regiones según porcentaje
# -------------------------------------- #
calcular_y_graficar_correlaciones <- function(lista_resultados_combinados_in, anios, orientacion) {
  # Instalar y cargar las librerías necesarias
  if (!require(corrplot)) install.packages("corrplot")
  library(corrplot)
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(reshape2)) install.packages("reshape2")
  library(ggplot2)
  library(reshape2)
  
  # Calcular las matrices de correlación para cada dataframe en la lista
  correlaciones_lista <- lapply(lista_resultados_combinados_in, function(df) {
    df_num <- df %>%
      select(-IdEstablecimiento) %>%
      mutate(across(starts_with("vrs_iteracion_"), ~ as.numeric(replace(., . == "NO APLICA", NA)))) %>%
      mutate(across(starts_with("crs_iteracion_"), ~ as.numeric(replace(., . == "NO APLICA", NA))))
    
    cor(df_num[, sapply(df_num, is.numeric)], use = "complete.obs")
  })
  
  # Nombrar la lista con los años para identificación
  names(correlaciones_lista) <- names(lista_resultados_combinados_in)
  
  # Definir la cuadrícula de gráficos para las matrices de correlación
  num_graficos <- length(correlaciones_lista)
  filas <- ceiling(sqrt(num_graficos))
  columnas <- ceiling(num_graficos / filas)
  
  
  
  
  # Ajustar la ventana gráfica y definir una configuración para múltiples gráficos
  colores_personalizados <- colorRampPalette(c("red", "yellow", "green"))(200)  # De rojo (mínimo) a verde (máximo)
  par(mfrow = c(filas, columnas), mar = c(2, 2, 2, 2), oma = c(4, 4, 4, 4))  # Ajustar márgenes (oma es el margen externo)
  
  # Crear las gráficas
  for (anio in names(correlaciones_lista)) {
    corrplot(
      correlaciones_lista[[anio]], 
      col = colores_personalizados, 
      method = "color", 
      title = paste("Matriz de Correlación - Año", anio),
      mar = c(0, 0, 2, 0)  # Márgenes más pequeños para cada gráfico
    )
  }
  
  if (orientacion == "io"){
    texto <- "Matrices de correlación de métodos orientado a entradas por año"
    texto2 <- "Correlación entre matrices de correlación entre años \n Orientado a entradas"
  }else if (orientacion == "oo"){
    texto <- "Matrices de correlación de métodos orientado a salidas por año"
    texto2 <- "Correlación entre matrices de correlación entre años \n Orientado a salidas"
  }else{
    texto <- "Matrices de correlación de métodos por año"
    texto2 <- "Correlación entre matrices de correlación entre años"
  }
  # Agregar un título general
  mtext(
    texto, 
    outer = TRUE, 
    cex = 1.5,  # Tamaño del texto
    font = 2    # Estilo en negrita
  )
  
  # Restablecer la configuración gráfica por defecto
  par(mfrow = c(1, 1))
  
  
  
  
  
  
  # Calcular la correlación entre todas las combinaciones de años y almacenar en una matriz 6x6
  correlacion_matriz <- matrix(NA, nrow = length(anios), ncol = length(anios), dimnames = list(anios, anios))
  
  for (i in 1:length(anios)) {
    for (j in 1:length(anios)) {
      matriz_i <- as.vector(correlaciones_lista[[anios[i]]])
      matriz_j <- as.vector(correlaciones_lista[[anios[j]]])
      
      correlacion_matriz[i, j] <- cor(matriz_i, matriz_j, use = "complete.obs")
    }
  }
  
  print(correlacion_matriz)
  
  # Convertir la matriz a formato largo para ggplot2
  correlacion_df <- melt(correlacion_matriz, varnames = c("Año1", "Año2"), value.name = "Correlacion")
  
  print(correlacion_df)
  grafico <- ggplot(correlacion_df, aes(x = Año1, y = Año2, fill = Correlacion)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0) +
    geom_text(aes(label = round(Correlacion, 2)), color = "black", size = 3) + # Añadir valores redondeados en los recuadros
    labs(
      title = texto2,
      x = "Año",
      y = "Año"
    ) +
    scale_x_continuous(breaks = unique(correlacion_df$Año1)) + # Mostrar todos los años en el eje X
    scale_y_continuous(breaks = unique(correlacion_df$Año2)) + # Mostrar todos los años en el eje Y
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5) # Cambiar tamaño y estilo del título
    )
  
  print(grafico)
  
  
  # Retornar resultados de correlación entre matrices de distintos años
  return(list(correlaciones_lista = correlaciones_lista))
}


