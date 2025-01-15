library(rnaturalearthdata)
library(rnaturalearth)
library(chilemapas)
library(gridExtra)
library(corrplot)
library(reshape2)
library(ggplot2)
library(plotly)
library(sf)
library(RColorBrewer)

# Cargar los datos de Chile
world <- ne_countries(scale = "medium", returnclass = "sf")
chile <- world[world$name == "Chile", ]
comunas_sf <- chilemapas::mapa_comunas



grafica_eficiencias <- function(resultados) {
  # Parámetros para iterar
  tipos <- c("io", "oo")
  periodos <- list(
    "todos" = "2014 - 2023",
    "pre" = "2014 - 2019",
    "post" = "2020 - 2023"
  )
  
  # Generar gráficos para cada tipo y período
  for (tipo in tipos) {
    for (periodo in names(periodos)) {
      # Crear dataframe
      df <- crear_dataframe(resultados, tipo, periodo)
      
      titulo1 <- paste("Distribución de Eficiencia Técnica Orientación", ifelse(tipo == "io", "Entradas", "Salidas"), "(VRS)")
      subtitulo1 <- paste("Período", periodos[[periodo]])
      titulo2 <- paste("Distribución de Eficiencia Técnica Orientación", ifelse(tipo == "io", "Entradas", "Salidas"), "(CRS)")
      subtitulo2 <- paste("Período", periodos[[periodo]])
      
      # Graficar VRS
      grafica1 <- graficar_boxplots(
        df,
        "vrs",
        titulo1,
        subtitulo1
      )
      
      # Graficar CRS
      grafica2 <-graficar_boxplots(
        df,
        "crs",
        titulo2,
        subtitulo2
      )
      
      ggsave(paste0(titulo1,"_",subtitulo1,".jpg"), plot = grafica1, width = 8, height = 6, dpi = 300)
      ggsave(paste0(titulo2,"_",subtitulo2,".jpg"), plot = grafica2, width = 8, height = 6, dpi = 300)
      
      print(grafica1)
      print(grafica2)
    }
  }
  
}




# Función genérica para graficar boxplots
graficar_boxplots <- function(df, eficiencia, titulo, subtitulo) {
  ggplot(df, aes_string(x = "year", y = eficiencia, fill = "year")) +
    geom_violin(outlier.colour = "red", outlier.size = 2, alpha = 0.6) + 
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Año",
      y = paste("Eficiencia", toupper(eficiencia))  # VRS o CRS
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3") + 
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(2, 2, 2, 2), "cm")
    ) + 
    labs(
      color = "Años",    # Cambia el nombre para colores
      fill = "Años"      # Cambia el nombre para rellenos
    )
  
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
    
    titulo_pre <- paste("Índice Malmquist para", key_name)
    subtitulo_pre <- "Periodo pre pandemia por COVID-19"
    
    # Crear el gráfico combinado
    grafico_pre_pandemia <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(titulo_pre,  subtitle = subtitulo_pre) +
      
      xlab("Índice Malmquist") +
      ylab("Densidad") +
      theme_minimal() +
      ylim(0, 6) + 
      theme(legend.position = "right",
            legend.box = "vertical",                # Orden vertical fijo
            #legend.key.height = unit(1, "cm"),      # Altura fija para las leyendas
            #legend.key.width = unit(5, "cm"),
            plot.margin = unit(c(3, 3, 3, 3), "cm"),
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 14)) + 
      
      scale_x_continuous(
        breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
        limits = rango_x  # Limitar los valores al rango simétrico
      ) +
      
      # Leyenda para colores
      scale_color_brewer(
        palette = "Spectral",                           # Paleta de colores
        labels = c("2014 - 2015", "2015 - 2016", 
                   "2016 - 2017", "2017 - 2018", 
                   "2018 - 2019")    # Etiquetas personalizadas
      ) +
      scale_fill_brewer(
        palette = "Spectral",                           # Usar la misma paleta para rellenos
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
    
    titulo_post <- paste("Índice Malmquist para", key_name)
    subtitulo_post <- "Periodo de pandemia por COVID-19"
    # Crear el gráfico combinado
    grafico_pandemia <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(titulo_post, subtitle = subtitulo_post) +
      xlab("Índice Malmquist") +
      ylab("Densidad") +
      theme_minimal() +
      ylim(0, 6) + 
      theme(legend.position = "right",
            legend.box = "vertical",                # Orden vertical fijo
            #legend.key.height = unit(1, "cm"),      # Altura fija para las leyendas
            #legend.key.width = unit(5, "cm"),
            plot.margin = unit(c(3, 3, 3, 3), "cm"),
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 14)) + 
      
      scale_x_continuous(
        breaks = seq(floor(-2), ceiling(5), by = 1),  # Incrementos de 1 en 1
        limits = rango_x  # Limitar los valores al rango simétrico
      ) +
      
      # Leyenda para colores
      scale_color_brewer(
        palette = "Spectral",                           # Paleta de colores
        labels = c("2019 - 2020", 
                   "2020 - 2021", 
                   "2021 - 2022", "2022 - 2023")    # Etiquetas personalizadas
      ) +
      scale_fill_brewer(
        palette = "Spectral",                           # Usar la misma paleta para rellenos
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
    
    titulo_tasas <- paste("Índice Malmquist para", key_name)
    subtitulo_tasas <- "Promedio por periodo"
    
    grafico_tasas <- ggplot(datos_comb, aes(x = Valores, fill = Columna, color = Columna)) +
      geom_density(alpha = 0.3) +  # Añadir transparencia
      ggtitle(titulo_tasas, subtitle = subtitulo_tasas) +
      xlab("Índice Malmquist") +
      ylab("Densidad") +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.margin = unit(c(3, 3, 3, 3), "cm"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14)# Márgenes
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
  
  ggsave(paste0(titulo_pre,"_",subtitulo_pre,".jpg"), plot = grafica1, width = 8, height = 6, dpi = 300)
  ggsave(paste0(titulo_post,"_",subtitulo_post,".jpg"), plot = grafica2, width = 8, height = 6, dpi = 300)
  ggsave(paste0(titulo_tasas,"_",subtitulo_tasas,".jpg"), plot = grafica2, width = 8, height = 6, dpi = 300)
  
  
  return (graficas)
}


# Función para graficar el top 10 de frecuencias
graficar_top_10 <- function(data, titulo, subtitulo) {
  head(data)
  # Seleccionar el top 10 según la frecuencia
  top_10 <- data[order(-data$Frecuencia), ][1:10, ]
  
  # Crear el gráfico
  grafico <- ggplot(top_10, aes(x = reorder(Variable, -Frecuencia), y = Frecuencia, fill = Frecuencia)) +
    geom_bar(stat = "identity") +
    coord_flip() +  # Para que sea horizontal
    theme_minimal() +
    ggtitle(titulo,  subtitle = paste0(subtitulo, " Entre años 2014 y 2023")) +
    labs(
      x = "Variables",
      y = "Frecuencia"
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
    scale_fill_gradientn(
      colors = brewer.pal(11, "Spectral"), # Usar la paleta "Spectral" con 11 colores
      limits = c(0, 10) 
      ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(2, 2, 2, 2), "cm"),
      legend.position = "none"
    )

  
  # Mostrar gráfico
  print(grafico)
  
  
  # Crear el gráfico
  grafico_pre <- ggplot(top_10, aes(x = reorder(Variable, -Frecuencia_Pre_Pandemia), y = Frecuencia_Pre_Pandemia,  fill = Frecuencia_Pre_Pandemia)) +
    geom_bar(stat = "identity") +
    coord_flip() +  # Para que sea horizontal
    theme_minimal() +
    ggtitle(titulo,  subtitle = paste0(subtitulo, " Periodo pre pandemia (2014 - 2019)")) +
    labs(
      #title = paste0(titulo, " Pre Pandemia"),
      x = "Variables",
      y = "Frecuencia"
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
    scale_fill_gradientn(
      colors = brewer.pal(11, "Spectral"), # Usar la paleta "Spectral" con 11 colores
      limits = c(0, 10) 
      ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(2, 2, 2, 2), "cm"),
      legend.position = "none"
    )
  
  # Mostrar gráfico
  print(grafico_pre)
  
  
  # Crear el gráfico
  grafico_pandemia <- ggplot(top_10, aes(x = reorder(Variable, -Frecuencia_Pandemia), y = Frecuencia_Pandemia,  fill = Frecuencia_Pandemia)) +
    geom_bar(stat = "identity") +
    coord_flip() +  # Para que sea horizontal
    theme_minimal() +
    ggtitle(titulo,  subtitle = paste0(subtitulo, " Periodo pandemia (2020 - 2023)")) +
    labs(
      #title = paste0(titulo, " Pandemia") ,
      x = "Variables",
      y = "Frecuencia"
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
    scale_fill_gradientn(
      colors = brewer.pal(11, "Spectral"), # Usar la paleta "Spectral" con 11 colores
      limits = c(0, 10) 
      ) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = unit(c(2, 2, 2, 2), "cm"),
      legend.position = "none"
    )
  
  # Mostrar gráfico
  print(grafico_pandemia)
}



# -------------------------------------- #
# Graficar Chile según el criterio de orientacion y variacion
# -------------------------------------- #
chile_map_plot <- function(hospitales_df, anio, tipo, titulo, subtitulo) {

    ggplot(data = chile) +
      geom_sf() +
      ggtitle(titulo,  subtitle = paste0(subtitulo, " Año ",anio)) +
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
        x = "Longitud", y= "Latitud",
        #title = paste(tipo, "- Año", anio),
        color = "Valor",
        size = "Valor"
      ) +
      theme_minimal()  +
      theme(legend.position = "right",
                             legend.box = "vertical",                
                             plot.margin = unit(c(2, 2, 2, 2), "cm"))

}


# -------------------------------------- #
# Colorear regiones según porcentaje
# -------------------------------------- #
graficar_correlaciones <- function(correlaciones_lista, orientacion, etiquetas = c(), subtitulo = "") {
  # Definir colores personalizados
  #colores_personalizados <- colorRampPalette(c("red", "yellow", "green"))(200)  # De rojo (mínimo) a verde (máximo)
  #colores_personalizados <- colorRampPalette(brewer.pal(11, "YIGn"))(200)
  #colores_personalizados <- colorRampPalette(brewer.pal(9, "YlGn"))(200)
  colores_personalizados <- colorRampPalette(brewer.pal(9, "RdYlBu"))(200)
  
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
        #mar = c(0, 0, 2, 0),  # Márgenes más pequeños para cada gráfico
        mar = c(0.3, 0.3, 4, 0.3),
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




