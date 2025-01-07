setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  PRE PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2023
anios_pre_pandemia <- c("2014", "2015", "2016", "2017", "2018", "2019")
anios_pandemia <- c("2020", "2021", "2022", "2023")

datos <- lapply(anios, consolidar_datos_por_anio)
names(datos_iniciales) <- as.character(anios)

# Encontrar las DMUs comunes en todos los años y filtrar los datos para incluir solo esas DMUs
dmus_comunes <- Reduce(intersect, lapply(datos, `[[`, "IdEstablecimiento"))
datos <- lapply(datos, function(data) data[data$IdEstablecimiento %in% dmus_comunes, ])



# ==============================================
#  CÁLCULO DEA
# ==============================================

#  SENSIBILIDAD - ELIMINACION EFICIENTES

resultados <- list(
  io = resultados_iteracion(datos, "io"),
  oo = resultados_iteracion(datos, "oo")
)



#  SENSIBILIDAD - ELIMINACIÓN DE DATOS ATÍPICOS

#resultados_sin_atipicos <- list(
#  io = resultados_corte(resultados$io, "io"),
#  oo = resultados_corte(resultados$oo, "oo")
#)


# Aplicar el filtro a todos los años
datos_filtrados_vrs_io <- lapply(names(datos), function(anio) {
  datos[[anio]] %>% 
    filter(!(IdEstablecimiento %in% resultados[["io"]][["vector_outliers_vrs"]]))
})

datos_filtrados_crs_io <- lapply(names(datos), function(anio) {
  datos[[anio]] %>% 
    filter(!(IdEstablecimiento %in% resultados[["io"]][["vector_outliers_crs"]]))
})

datos_filtrados_vrs_oo <- lapply(names(datos), function(anio) {
  datos[[anio]] %>% 
    filter(!(IdEstablecimiento %in% resultados[["oo"]][["vector_outliers_vrs"]]))
})

datos_filtrados_crs_oo <- lapply(names(datos), function(anio) {
  datos[[anio]] %>% 
    filter(!(IdEstablecimiento %in% resultados[["oo"]][["vector_outliers_crs"]]))
})




# Mantener los nombres originales
names(datos_filtrados_vrs_oo) <- names(datos)
names(datos_filtrados_crs_oo) <- names(datos)
names(datos_filtrados_vrs_io) <- names(datos)
names(datos_filtrados_crs_io) <- names(datos)

anios <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022","2023")
iteracion_1_io_vrs <-  sapply(datos_filtrados_vrs_io, function(data) analisis_dea_general(data, "io"), simplify = FALSE)
iteracion_1_io_crs <-  sapply(datos_filtrados_vrs_io, function(data) analisis_dea_general(data, "io"), simplify = FALSE)
iteracion_1_oo_vrs <-  sapply(datos_filtrados_vrs_io, function(data) analisis_dea_general(data, "oo"), simplify = FALSE)
iteracion_1_oo_crs <-  sapply(datos_filtrados_vrs_io, function(data) analisis_dea_general(data, "oo"), simplify = FALSE)


iteracion_2_io_vrs <-  sapply(datos_filtrados_crs_oo, function(data) analisis_dea_general(data, "io"), simplify = FALSE)
iteracion_2_io_crs <-  sapply(datos_filtrados_crs_oo, function(data) analisis_dea_general(data, "io"), simplify = FALSE)
iteracion_2_oo_vrs <-  sapply(datos_filtrados_crs_oo, function(data) analisis_dea_general(data, "oo"), simplify = FALSE)
iteracion_2_oo_crs <-  sapply(datos_filtrados_crs_oo, function(data) analisis_dea_general(data, "oo"), simplify = FALSE)


iteracion_3_io_vrs <-  sapply(datos_filtrados_vrs_oo, function(data) analisis_dea_general(data, "io"), simplify = FALSE)
iteracion_3_io_crs <-  sapply(datos_filtrados_vrs_oo, function(data) analisis_dea_general(data, "io"), simplify = FALSE)
iteracion_3_oo_vrs <-  sapply(datos_filtrados_vrs_oo, function(data) analisis_dea_general(data, "oo"), simplify = FALSE)
iteracion_3_oo_crs <-  sapply(datos_filtrados_vrs_oo, function(data) analisis_dea_general(data, "oo"), simplify = FALSE)


iteracion_4_io_vrs <-  sapply(datos_filtrados_crs_io, function(data) analisis_dea_general(data, "io"), simplify = FALSE)
iteracion_4_io_crs <-  sapply(datos_filtrados_crs_io, function(data) analisis_dea_general(data, "io"), simplify = FALSE)
iteracion_4_oo_vrs <-  sapply(datos_filtrados_crs_io, function(data) analisis_dea_general(data, "oo"), simplify = FALSE)
iteracion_4_oo_crs <-  sapply(datos_filtrados_crs_io, function(data) analisis_dea_general(data, "oo"), simplify = FALSE)


grafica_eficiencias_2(original_io_vrs)
grafica_eficiencias_2(original_io_crs)
grafica_eficiencias_2(original_oo_vrs)
grafica_eficiencias_2(original_oo_crs)





# GRAFICA DE SENSIBILIDAD POR EFICIENCIA
graficar_correlaciones(resultados[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de DMU eficientes")
graficar_correlaciones(resultados[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"),  "Sensibilidad por eliminación de DMU eficientes")

# GRAFICA DE SENSIBILIDAD POR ELIMINACION DE DATOS ATIPICOS
#graficar_correlaciones(resultados_sin_atipicos[["io"]][["resultados_correlacion"]][["correlaciones_lista"]], "io", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"),  "Sensibilidad por eliminación de datos atípicos")
#graficar_correlaciones(resultados_sin_atipicos[["oo"]][["resultados_correlacion"]][["correlaciones_lista"]], "oo", c("vrs_i1", "vrs_i2", "vrs_i3", "crs_i1", "crs_i2",  "crs_i3"), "Sensibilidad por eliminación de datos atípicos")


# CORRELACION DE VALORES ORIGINALES PARA TODAS LAS COMBINACIONES EN TODOS LOS AÑOS
resultados_combinaciones <- combinar_resultados_in_out(resultados$io[["original"]], resultados$oo[["original"]])



correlacion_todos_metodos <- calcular_correlaciones_all(resultados_combinaciones)

graficar_correlaciones(correlacion_todos_metodos[["correlaciones_lista"]], "ambos", c("vrs_io", "vrs_oo", "crs_io", "crs_oo"))


# GRAFICA DE DISTRIBUCIÓN DE EFICIENCIAS

grafica_eficiencias(resultados)


# ==============================================
#  MALMQUIST 
# ==============================================

malmquist_indices <- list(
  in_vrs = malmquist("vrs", "in"),
  in_crs = malmquist("crs", "in"),
  out_vrs = malmquist("vrs", "out"),
  out_crs = malmquist("crs", "out")
)

procesar_y_graficar(malmquist_indices)

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

#mejores_25 <- list("in_vrs" =top_eficiencia(resultados$io, "vrs", 25, TRUE),
#                   "in_crs" = top_eficiencia(resultados$io, "crs", 25, TRUE),
#                   "out_vrs" = top_eficiencia(resultados$oo, "vrs", 25, TRUE),
#                   "out_crs" = top_eficiencia(resultados$oo, "crs", 25, TRUE)) 


# VRS INPUT
#lapply(anios, function(anio) {
#  chile_map_plot(mejores_25[["in_vrs"]][[as.character(anio)]], anio, "vrs")
#})


# REGION COLOREADA POR PORCENTAJE DENTRO DE 25 MEJOR
#resumen <- resumen_eficiencia(mejores_25$in_vrs)
#colorear_region(resumen)


# -------------------------------------------- #

#mapa_interactivo <- ggplotly(region, tooltip = "text")
#print(mapa_interactivo)

# ------------------------------------------- #

# ==============================================