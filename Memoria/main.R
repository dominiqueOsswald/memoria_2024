setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2023
anios_pre_pandemia <- c("2014", "2015", "2016", "2017", "2018", "2019")
anios_pandemia <- c("2020", "2021", "2022", "2023")

datos_iniciales <- lapply(anios, consolidar_datos_por_anio)
names(datos_iniciales) <- as.character(anios)


# Encontrar las DMUs comunes en todos los años y filtrar los datos para incluir solo esas DMUs
dmus_comunes <- Reduce(intersect, lapply(datos_iniciales, `[[`, "IdEstablecimiento"))
datos <- lapply(datos_iniciales, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])



# ==============================================
#  CÁLCULO DEA
# ==============================================

#  SENSIBILIDAD - ELIMINACION EFICIENTES

resultados <- list(
  io = resultados_iteracion(datos, "io"),
  oo = resultados_iteracion(datos, "oo")
)


#  SENSIBILIDAD - ELIMINACIÓN DE DATOS ATÍPICOS

resultados_cortados <- list(
  io = resultados_corte(resultados$io, "io"),
  oo = resultados_corte(resultados$oo, "oo")
)





graficas_sensibilidad <- list(
  # GRAFICA DE SENSIBILIDAD POR EFICIENCIA
  eficiencia_io = graficar_correlaciones(resultados[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes"),
  eficiencia_oo = graficar_correlaciones(resultados[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"),  "Sensibilidad por eliminación de DMU eficientes"),
  
  # GRAFICA DE SENSIBILIDAD POR ELIMINACION DE DATOS ATIPICOS
  atipicos_io = graficar_correlaciones(resultados_cortados[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"),  "Sensibilidad por eliminación de datos atípicos"),
  atipicos_oo = graficar_correlaciones(resultados_cortados[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de datos atípicos")

)

#print(graficas_sensibilidad$eficiencia_io)

# CORRELACION DE VALORES ORIGINALES PARA TODAS LAS COMBINACIONES EN TODOS LOS AÑOS
input_output_original <- combinar_resultados_in_out(resultados$io[["original"]], resultados$oo[["original"]])

correlacion_todos_metodos <- calcular_correlaciones_all(input_output_original)

grafica_correlacion_metodos <- graficar_correlaciones(correlacion_todos_metodos[["correlaciones_lista"]], "ambos", c("vrs_io", "vrs_oo", "crs_io", "crs_oo"))



# ==============================================
#  MALMQUIST 
# ==============================================

malmquist_indices <- list(
  in_vrs = malmquist("vrs", "in"),
  in_crs = malmquist("crs", "in"),
  out_vrs = malmquist("vrs", "out"),
  out_crs = malmquist("crs", "out")
)

malmquist_graficas <- procesar_y_graficar(malmquist_indices)


ggplot(resultados[["io"]][["original"]][["2014"]][["data"]], aes(x = vrs)) +
  geom_density(fill = "blue", alpha = 0.4) +  # Gráfico suavizado
  labs(title = "Gráfico de Densidad", x = "Valores", y = "Densidad") +
  theme_minimal()

ggplot(resultados[["io"]][["original"]][["2015"]][["data"]], aes(x = vrs)) +
  geom_density(fill = "blue", alpha = 0.4) +  # Gráfico suavizado
  labs(title = "Gráfico de Densidad", x = "Valores", y = "Densidad") +
  theme_minimal()

ggplot(resultados[["io"]][["original"]][["2016"]][["data"]], aes(x = vrs)) +
  geom_density(fill = "blue", alpha = 0.4) +  # Gráfico suavizado
  labs(title = "Gráfico de Densidad", x = "Valores", y = "Densidad") +
  theme_minimal()




ggplot(resultados[["io"]][["original"]][["2014"]][["data"]], aes(x = Valores, fill = Columna, color = Columna)) +
  geom_density(alpha = 0.3) +  # Añadir transparencia
  ggtitle(paste("Índice Malmquist para", key_name), subtitle = "Promedio por periodo") +
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

# ==============================================
#  DETERMINANTES
# ==============================================
# -------------------------------------------- #
#  CONFIGURACIÓN Y MODELO RANDOM FOREST
# -------------------------------------------- #

# Aplicar Random Forest para cada año
random_forest <- list(
  io_vrs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados$io, 500, "vrs")}),
  io_crs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados$io, 500, "crs")}),
  oo_vrs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados$oo, 500, "vrs")}),
  oo_crs = lapply(anios, function(anio) {analize_rf(anio, resultados_in = resultados$oo, 500, "crs")})
)

# Asignar nombres a la lista de modelos
names(random_forest$io_vrs) <- paste0(anios)
names(random_forest$io_crs) <- paste0(anios)
names(random_forest$oo_vrs) <- paste0(anios)
names(random_forest$oo_crs) <- paste0(anios)

# -------------------------------------------- #
#  EXTRACCIÓN DE VARIABLES POR AÑO
# -------------------------------------------- #

# Llamar a la función
resultados_importancia <- procesar_importancia(random_forest, anios_pre_pandemia, anios_pandemia)

# Acceder a los resultados
resultados_IncNodePurity <- resultados_importancia$IncNodePurity
resultados_IncMSE <- resultados_importancia$IncMSE



# -------------------------------------------- #
#  VISUALIZACIÓN DE FRECUENCIAS
# -------------------------------------------- #


# Graficar para IncNodePurity
graficar_top_10(resultados_IncNodePurity[["io_vrs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a entradas - VRS -")
graficar_top_10(resultados_IncNodePurity[["io_crs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a entradas - CRS -")
graficar_top_10(resultados_IncNodePurity[["oo_vrs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a salidas - VRS -")
graficar_top_10(resultados_IncNodePurity[["oo_crs"]], "Top 10 Determinantes - IncNodePurity", "Modelo orientado a salidas - CRS -")

# Graficar para %IncMSE
graficar_top_10(resultados_IncMSE[["io_vrs"]], "Top 10 Determinantes - %IncMSE", "Modelo orientado a entradas - VRS -")
graficar_top_10(resultados_IncMSE[["io_crs"]], "Top 10 Determinantes - %IncMSE", "Modelo orientado a entradas - CRS -")
graficar_top_10(resultados_IncMSE[["oo_vrs"]], "Top 10 Determinantes - %IncMSE", "Modelo orientado a salidas - VRS -")
graficar_top_10(resultados_IncMSE[["oo_crs"]], "Top 10 Determinantes - %IncMSE", "Modelo orientado a salidas - CRS -")









# ==============================================
#  RESULTADOS
# ==============================================

# Generación de excel con valores de eficiencias y determinantes

# Procesar OUTPUT
procesar_y_guardar_resultados(
  dataframes = crear_dataframes(resultados, "oo"),
  resultados_IncNodePurity = resultados_IncNodePurity,
  resultados_IncMSE = resultados_IncMSE,
  archivo_salida = "RESULTADOS OUTPUT.xlsx",
  prefijo = "oo"
)

# Procesar INPUT
procesar_y_guardar_resultados(
  dataframes = crear_dataframes(resultados, "io"),
  resultados_IncNodePurity = resultados_IncNodePurity,
  resultados_IncMSE = resultados_IncMSE,
  archivo_salida = "RESULTADOS INPUT.xlsx",
  prefijo = "io"
)

# ==============================================
#    GRAFICAS
# ==============================================
#  TODOS - GRAFICA DEA INPUT VRS

lapply(anios, function(anio) {
  chile_map_plot(resultados$io[["original"]][[as.character(anio)]][["data"]], anio, "vrs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a entradas - VRS - ")
})

lapply(anios, function(anio) {
  chile_map_plot(resultados$io[["original"]][[as.character(anio)]][["data"]], anio, "crs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a entradas - CRS -")
})

lapply(anios, function(anio) {
  chile_map_plot(resultados$oo[["original"]][[as.character(anio)]][["data"]], anio, "vrs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a salidas - VRS -")
})

lapply(anios, function(anio) {
  chile_map_plot(resultados$oo[["original"]][[as.character(anio)]][["data"]], anio, "crs", "Gráfica Chile - Eficiencia técnica ", "Modelo orientado a salidas - CRS -")
})


# MEJORES RESULTADOS 

mejores_25 <- list("in_vrs" =top_eficiencia(resultados$io, "vrs", 25, TRUE),
                   "in_crs" = top_eficiencia(resultados$io, "crs", 25, TRUE),
                   "out_vrs" = top_eficiencia(resultados$oo, "vrs", 25, TRUE),
                   "out_crs" = top_eficiencia(resultados$oo, "crs", 25, TRUE)) 


# VRS INPUT
lapply(anios, function(anio) {
  chile_map_plot(mejores_25[["in_vrs"]][[as.character(anio)]], anio, "vrs")
})


# REGION COLOREADA POR PORCENTAJE DENTRO DE 25 MEJOR
resumen <- resumen_eficiencia(mejores_25$in_vrs)
colorear_region(resumen)


# -------------------------------------------- #

#mapa_interactivo <- ggplotly(region, tooltip = "text")
#print(mapa_interactivo)

# ------------------------------------------- #

# ==============================================