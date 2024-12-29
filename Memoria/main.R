setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions.R")
source("graphics.R")

# ==============================================
#  PROCESAMIENTO DE DATOS
# ==============================================

#  CONSOLIDADO DE DATOS POR AÑO
anios <- 2014:2023
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


variables_random_forest <- list()

# Iterar sobre cada método/posición
for (metodo in names(random_forest)) {
  variables_random_forest[[metodo]] <- lapply(random_forest[[metodo]], function(modelo) {
    names(modelo$top_IncMSE)  # Extraer los nombres de filas de 'importancia'
  })
}




# -------------------------------------------- #
#  COMBINACIÓN Y ANÁLISIS DE VARIABLES
# -------------------------------------------- #


# Crear listas para almacenar resultados
resultados_IncNodePurity <- list()
resultados_IncMSE <- list()

# Definir períodos
anios_pre_pandemia <- c("2014", "2015", "2016", "2017", "2018", "2019")
anios_pandemia <- c("2020", "2021", "2022", "2023")

# Iterar sobre cada método
for (metodo in names(random_forest)) {
  
  # Crear dataframes vacíos para almacenar resultados
  df_IncNodePurity <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  df_IncMSE <- data.frame(Variable = character(), stringsAsFactors = FALSE)
  
  # Recopilar datos por año
  for (anio in names(random_forest[[metodo]])) {
    # Obtener importancia
    importancia <- random_forest[[metodo]][[anio]]$importancia
    
    # --- IncNodePurity ---
    temp_IncNodePurity <- data.frame(
      Variable = rownames(importancia),
      Valor = importancia[, "IncNodePurity"],  # Seleccionar columna
      Año = anio
    )
    df_IncNodePurity <- rbind(df_IncNodePurity, temp_IncNodePurity)
    
    # --- %IncMSE ---
    temp_IncMSE <- data.frame(
      Variable = rownames(importancia),
      Valor = importancia[, "%IncMSE"],  # Seleccionar columna
      Año = anio
    )
    df_IncMSE <- rbind(df_IncMSE, temp_IncMSE)
  }
  
  # --- Procesar IncNodePurity ---
  pivot_IncNodePurity <- reshape(df_IncNodePurity, 
                                 idvar = "Variable", 
                                 timevar = "Año", 
                                 direction = "wide")
  colnames(pivot_IncNodePurity) <- gsub("Valor\\.", "", colnames(pivot_IncNodePurity))
  
  # Calcular frecuencia y promedios
  pivot_IncNodePurity$Frecuencia <- table(df_IncNodePurity$Variable)[pivot_IncNodePurity$Variable]
  pivot_IncNodePurity$Promedio_Pre_Pandemia <- rowMeans(pivot_IncNodePurity[, anios_pre_pandemia], na.rm = TRUE)
  pivot_IncNodePurity$Promedio_Pandemia <- rowMeans(pivot_IncNodePurity[, anios_pandemia], na.rm = TRUE)
  
  # Calcular frecuencias separadas
  pivot_IncNodePurity$Frecuencia_Pre_Pandemia <- rowSums(!is.na(pivot_IncNodePurity[, anios_pre_pandemia]))
  pivot_IncNodePurity$Frecuencia_Pandemia <- rowSums(!is.na(pivot_IncNodePurity[, anios_pandemia]))
  
  # Ordenar y seleccionar las 50 más importantes
  pivot_IncNodePurity <- pivot_IncNodePurity[order(-pivot_IncNodePurity$Promedio_Pre_Pandemia), ]
  resultados_IncNodePurity[[metodo]] <- head(pivot_IncNodePurity, 50)
  
  # --- Procesar %IncMSE ---
  pivot_IncMSE <- reshape(df_IncMSE, 
                          idvar = "Variable", 
                          timevar = "Año", 
                          direction = "wide")
  colnames(pivot_IncMSE) <- gsub("Valor\\.", "", colnames(pivot_IncMSE))
  
  # Calcular frecuencia y promedios
  pivot_IncMSE$Frecuencia <- table(df_IncMSE$Variable)[pivot_IncMSE$Variable]
  pivot_IncMSE$Promedio_Pre_Pandemia <- rowMeans(pivot_IncMSE[, anios_pre_pandemia], na.rm = TRUE)
  pivot_IncMSE$Promedio_Pandemia <- rowMeans(pivot_IncMSE[, anios_pandemia], na.rm = TRUE)
  
  # Calcular frecuencias separadas
  pivot_IncMSE$Frecuencia_Pre_Pandemia <- rowSums(!is.na(pivot_IncMSE[, anios_pre_pandemia]))
  pivot_IncMSE$Frecuencia_Pandemia <- rowSums(!is.na(pivot_IncMSE[, anios_pandemia]))
  
  # Ordenar y seleccionar las 50 más importantes
  pivot_IncMSE <- pivot_IncMSE[order(-pivot_IncMSE$Promedio_Pre_Pandemia), ]
  resultados_IncMSE[[metodo]] <- head(pivot_IncMSE, 50)
}


# Mostrar resultados
#head(resultados_IncNodePurity[["io_vrs"]])  # Ver resultados para IncNodePurity
#head(resultados_IncMSE[["io_vrs"]])        # Ver resultados para %IncMSE

write.csv(resultados_IncNodePurity[["oo_vrs"]], "top_IncNodePurity_io_vrs.csv")
write.csv(resultados_IncMSE[["oo_vrs"]], "top_IncMSE_io_vrs.csv")




# -------------------------------------------- #
#  VISUALIZACIÓN DE FRECUENCIAS
# -------------------------------------------- #


# Graficar para IncNodePurity
graficar_top_10(resultados_IncNodePurity[["io_vrs"]], "Top 10 - IncNodePurity (io_vrs)")
graficar_top_10(resultados_IncNodePurity[["io_crs"]], "Top 10 - IncNodePurity (io_crs)")
graficar_top_10(resultados_IncNodePurity[["oo_vrs"]], "Top 10 - IncNodePurity (oo_vrs)")
graficar_top_10(resultados_IncNodePurity[["oo_crs"]], "Top 10 - IncNodePurity (oo_crs)")

# Graficar para %IncMSE
graficar_top_10(resultados_IncMSE[["io_vrs"]], "Top 10 - %IncMSE (io_vrs)")
graficar_top_10(resultados_IncMSE[["io_crs"]], "Top 10 - %IncMSE (io_crs)")
graficar_top_10(resultados_IncMSE[["oo_vrs"]], "Top 10 - %IncMSE (oo_vrs)")
graficar_top_10(resultados_IncMSE[["oo_crs"]], "Top 10 - %IncMSE (oo_crs)")



# ==============================================
#  RESULTADOS
# ==============================================


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
  chile_map_plot(resultados$io[["original"]][[as.character(anio)]][["data"]], anio, "vrs")
})


#    MEJORES RESULTADOS 

mejores_25 <- list("in_vrs" =top_eficiencia(resultados$io, "vrs", 25, TRUE),
                   "in_crs" = top_eficiencia(resultados$io, "crs", 25, TRUE),
                   "out_vrs" = top_eficiencia(resultados$oo, "vrs", 25, TRUE),
                   "out_crs" = top_eficiencia(resultados$oo, "crs", 25, TRUE)) 


#    VRS INPUT
lapply(anios, function(anio) {
  chile_map_plot(mejores_25[["in_vrs"]][[as.character(anio)]], anio, "vrs")
})


#    REGION COLOREADA POR PORCENTAJE DENTRO DE 25 MEJOR
resumen <- resumen_eficiencia(mejores_25$in_vrs)
colorear_region(resumen)


# -------------------------------------------- #

#mapa_interactivo <- ggplotly(region, tooltip = "text")
#print(mapa_interactivo)

# ------------------------------------------- #

# ==============================================