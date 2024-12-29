library(rnaturalearthdata)
library(rnaturalearth)
library(chilemapas)
library(gridExtra)
library(corrplot)
library(reshape2)
library(ggplot2)
library(plotly)
library(sf)

procesar_index <- function(index) {
  # Asegurarse de que las columnas sean numéricas
  index[, -1] <- lapply(index[, -1], as.numeric)
  
  # Renombrar las columnas de acuerdo a los años consecutivos
  columnas <- colnames(index)[-1]
  nuevos_nombres <- paste(columnas[-length(columnas)], columnas[-1], sep = "_")
  colnames(index)[-1] <- c("2014_2015", nuevos_nombres)
  
  # Calcular la tasa promedio pre-pandemia
  index$Tasa_Promedio_Pre_Pandemia <- rowMeans(index[, 2:6], na.rm = TRUE)
  index$Tasa_Promedio_Pandemia <- rowMeans(index[, 7:10], na.rm = TRUE)
  
  return(index)
}



procesar_y_graficar <- function(malmquist_indices) {
  graficas <- list()
  for (key in names(malmquist_indices)) {
    index <- malmquist_indices[[key]][["index"]]
    print(paste("Generando gráfico para:", key))
    
    if (key == "out_vrs"){
      key_name = "modelo orientado a salidas VRS"
    }
    if (key == "out_crs"){
      key_name = "modelo orientado a salidas CRS"
    }
    if (key == "in_vrs"){
      key_name = "modelo orientado a entradas VRS"
    }
    if (key == "in_crs"){
      key_name = "modelo orientado a entradas CRS"
    }
    # Seleccionar columnas excepto la primera
    columnas <- colnames(index)[-c(1, 7, 8, 9, 10, 11, 12)]
    
    # Crear un dataframe combinado para todas las columnas
    datos_comb <- data.frame()
    for (col in columnas) {
      datos_temp <- na.omit(index[[col]])  # Omitir NAs
      datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
    }
    
    #limite <- max(abs(datos_comb$Valores), na.rm = TRUE)  # Máximo absoluto
    rango_x <- c(-2, 5)  
    
    # Crear el gráfico combinado
    grafico_pre_pandemia <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(paste("Índice Malmquist para", key_name),  subtitle = "Periodo pre pandemia por COVID-19") +
      xlab("Valores") +
      ylab("Densidad") +
      theme_minimal() +
      ylim(0, 6) + 
      theme(legend.position = "right",
            legend.box = "vertical",                # Orden vertical fijo
            #legend.key.height = unit(1, "cm"),      # Altura fija para las leyendas
            #legend.key.width = unit(5, "cm"),
            plot.margin = unit(c(3, 3, 3, 3), "cm")) + 
      
      scale_x_continuous(
        breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
        limits = rango_x  # Limitar los valores al rango simétrico
      ) +
      
      # Leyenda para colores
      scale_color_brewer(
        palette = "Set1",                           # Paleta de colores
        labels = c("2014 - 2015", "2015 - 2016", 
                   "2016 - 2017", "2017 - 2018", 
                   "2018 - 2019")    # Etiquetas personalizadas
      ) +
      scale_fill_brewer(
        palette = "Set1",                           # Usar la misma paleta para rellenos
        labels = c("2014 - 2015", "2015 - 2016", 
                   "2016 - 2017", "2017 - 2018", 
                   "2018 - 2019")
      ) +

      labs(
        color = "Años",    # Cambia el nombre para colores
        fill = "Años"      # Cambia el nombre para rellenos
      )
    
    
    print(grafico_pre_pandemia)
    
    
    # Seleccionar columnas excepto la primera
    columnas <- colnames(index)[-c(1, 2, 3, 4, 5, 6, 11, 12)]
    
    # Crear un dataframe combinado para todas las columnas
    datos_comb <- data.frame()
    for (col in columnas) {
      datos_temp <- na.omit(index[[col]])  # Omitir NAs
      datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
    }
    
    #limite <- max(abs(datos_comb$Valores), na.rm = TRUE)  # Máximo absoluto
    rango_x <- c(-2, 5)  
    
    # Crear el gráfico combinado
    grafico_pandemia <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(paste("Índice Malmquist para", key_name), subtitle = "Periodo de pandemia por COVID-19") +
      xlab("Valores") +
      ylab("Densidad") +
      theme_minimal() +
      ylim(0, 6) + 
      theme(legend.position = "right",
            legend.box = "vertical",                # Orden vertical fijo
            #legend.key.height = unit(1, "cm"),      # Altura fija para las leyendas
            #legend.key.width = unit(5, "cm"),
            plot.margin = unit(c(3, 3, 3, 3), "cm")) + 
      
      scale_x_continuous(
        breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
        limits = rango_x  # Limitar los valores al rango simétrico
      ) +
      
      # Leyenda para colores
      scale_color_brewer(
        palette = "Set1",                           # Paleta de colores
        labels = c("2019 - 2020", 
                   "2020 - 2021", 
                   "2021 - 2022", "2022 - 2023")    # Etiquetas personalizadas
      ) +
      scale_fill_brewer(
        palette = "Set1",                           # Usar la misma paleta para rellenos
        labels = c("2019 - 2020", 
                   "2020 - 2021",
                   "2021 - 2022", "2022 - 2023")
      ) +
      
      labs(
        color = "Años",    # Cambia el nombre para colores
        fill = "Años"      # Cambia el nombre para rellenos
      )
    
    
    print(grafico_pandemia)
    
    
    
    
    
    
    
    # Seleccionar columnas excepto la primera
    columnas_2 <- colnames(index)[11:12]
    
    # Crear un dataframe combinado para todas las columnas
    datos_comb <- data.frame()
    for (col in columnas_2) {
      datos_temp <- na.omit(index[[col]])  # Omitir NAs
      datos_comb <- rbind(datos_comb, data.frame(Valores = datos_temp, Columna = col))
    }
    
    grafico_tasas <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(paste("Índice Malmquist para", key_name), subtitle = "Promedio por periodo") +
      xlab("Valores") +
      ylab("Densidad") +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.margin = unit(c(3, 3, 3, 3), "cm") # Márgenes
      ) +
      scale_x_continuous(
        breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
        limits = rango_x  # Limitar los valores al rango simétrico
      ) +
      # Leyenda para colores
      scale_color_manual(
        values = c("red", "blue"),                    # Colores personalizados
        labels = c("Pandemia", "Pre pandemia")       # Etiquetas personalizadas
      ) +
      # Leyenda para rellenos
      scale_fill_manual(
        values = c("pink", "lightblue"),              # Colores de relleno
        labels = c("Pandemia", "Pre pandemia")       # Etiquetas personalizadas
      ) +
      # Cambiar nombres de las leyendas
      labs(
        color = "Periodo",    # Cambia el nombre para colores
        fill = "Periodo"      # Cambia el nombre para rellenos
      )
    

    print(grafico_tasas)
    
    graficas[[key]] <- list(
      "Pre-Pandemia" = grafico_pre_pandemia,
      "Pandemia" =  grafico_pandemia,
      "Tasas" = grafico_tasas
    )
  }
  
  return (graficas)
}


# Función para graficar el top 10 de frecuencias
graficar_top_10 <- function(data, titulo) {
  # Seleccionar el top 10 según la frecuencia
  top_10 <- data[order(-data$Frecuencia), ][1:10, ]
  
  # Crear el gráfico
  grafico <- ggplot(top_10, aes(x = reorder(Variable, -Frecuencia), y = Frecuencia)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +  # Para que sea horizontal
    theme_minimal() +
    labs(
      title = paste0(titulo, " 2014 - 2023"),
      x = "Variables",
      y = "Frecuencia"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Mostrar gráfico
  print(grafico)
  
  
  # Crear el gráfico
  grafico_pre <- ggplot(top_10, aes(x = reorder(Variable, -Frecuencia_Pre_Pandemia), y = Frecuencia_Pre_Pandemia)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +  # Para que sea horizontal
    theme_minimal() +
    labs(
      title = paste0(titulo, " Pre Pandemia"),
      x = "Variables",
      y = "Frecuencia"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Mostrar gráfico
  print(grafico_pre)
  
  
  # Crear el gráfico
  grafico_pandemia <- ggplot(top_10, aes(x = reorder(Variable, -Frecuencia_Pandemia), y = Frecuencia_Pandemia)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +  # Para que sea horizontal
    theme_minimal() +
    labs(
      title = paste0(titulo, " Pandemia") ,
      x = "Variables",
      y = "Frecuencia"
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Mostrar gráfico
  print(grafico_pandemia)
}


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
chile_map_plot <- function(hospitales_df, anio, tipo) {

    ggplot(data = chile) +
      geom_sf() +
      geom_point(
        data = hospitales_df,
        aes_string(
          x = "longitud",
          y = "latitud",
          color = tipo,
          text = paste0("paste('Hospital:', Nombre, '<br>", tipo, ":', ", tipo, ", '<br>Region:', region_id)")
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
graficar_correlaciones <- function(correlaciones_lista, orientacion, etiquetas = c(), subtitulo = "") {
  # Definir colores personalizados
  colores_personalizados <- colorRampPalette(c("red", "yellow", "green"))(200)  # De rojo (mínimo) a verde (máximo)
  
  # Determinar la cantidad de gráficos
  num_graficos <- length(correlaciones_lista)
  graficos_por_pagina <- 4  # Máximo 4 gráficos por página (2x2)
  paginas <- ceiling(num_graficos / graficos_por_pagina)
  
  # Crear lista para almacenar los gráficos
  lista_graficos <- list()
  
  # Iterar sobre cada página
  for (pagina in 1:paginas) {
    # Definir el rango de años a mostrar en la página actual
    inicio <- (pagina - 1) * graficos_por_pagina + 1
    fin <- min(pagina * graficos_por_pagina, num_graficos)
    años_actuales <- names(correlaciones_lista)[inicio:fin]
    
    # Ajustar la ventana gráfica para 2x2
    par(mfrow = c(2, 2), mar = c(2, 2, 2, 2), oma = c(4, 4, 4, 4))
    
    # Crear las gráficas para los años actuales
    for (anio in años_actuales) {
      corr_matrix <- correlaciones_lista[[anio]]
      
      if (length(etiquetas) != 0) {
        rownames(corr_matrix) <- etiquetas
        colnames(corr_matrix) <- etiquetas
      }
      
      corrplot(
        corr_matrix, 
        col = colores_personalizados, 
        method = "color", 
        title = paste("Año", anio),
        mar = c(0, 0, 2, 0),  # Márgenes más pequeños para cada gráfico
        addCoef.col = "black",
        cl.ratio = 0.4,        # Ajustar tamaño de la barra de leyenda
        cl.align = "r" 
      )
    }
    
    # Determinar título según orientación
    if (orientacion == "io") {
      texto <- "Matrices de correlación de métodos orientado a entradas por año"
    } else if (orientacion == "oo") {
      texto <- "Matrices de correlación de métodos orientado a salidas por año"
    } else {
      texto <- "Matrices de correlación de métodos por año"
    }
    
    # Título principal centrado
    mtext(
      texto, 
      outer = TRUE, 
      cex = 1.5,  # Tamaño del texto
      font = 2,   # Estilo en negrita
      line = 1.5  # Ajusta la posición vertical
    )
    
    # Subtítulo centrado y más cerca del título
    if (subtitulo != "") {
      mtext(
        subtitulo,
        outer = TRUE, 
        cex = 1.2,  # Tamaño del texto
        font = 3,   # Cursiva
        line = 0.1  # Justo debajo del título principal
      )
    }
    
    # Ajustar márgenes exteriores para centrar contenido
    par(oma = c(5, 4, 5, 4))  # Márgenes: abajo, izquierda, arriba, derecha
    
    
    # Capturar gráfico como objeto si es necesario
    grafico <- recordPlot()
    lista_graficos[[pagina]] <- grafico
    
    # Restablecer configuración gráfica
    par(mfrow = c(1, 1))
  }
  
  # Retornar lista de gráficos
  return(lista_graficos)
}




